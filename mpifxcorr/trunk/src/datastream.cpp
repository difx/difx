/***************************************************************************
 *   Copyright (C) 2006 by Adam Deller                                     *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#include "datastream.h"
#include "core.h"
#include "fxmanager.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <string.h>
#include <strings.h>
#include <limits.h>
#include <math.h>
#include "config.h"
#include "alert.h"

DataStream::DataStream(Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments)
  : databufferfactor(bufferfactor), numdatasegments(numsegments), streamnum(snum), config(conf), mpiid(id), numcores(ncores)
{
  coreids = new int[numcores];
  for(int i=0;i<numcores;i++)
    coreids[i] = cids[i];
  model = config->getModel();
  activescan = 0;
  activesec = 0;
  activens = 0;
}


DataStream::~DataStream()
{
  if(input.is_open())
    input.close();
  vectorFree(databuffer);
  for(int i=0;i<numdatasegments;i++)
  {
    delete [] bufferinfo[i].datarequests;
    delete [] bufferinfo[i].controlrequests;
    for(int j=0;j<maxsendspersegment;j++)
    {
      vectorFree(bufferinfo[i].controlbuffer[j]);
    }
    delete [] bufferinfo[i].controlbuffer;
  }
  delete [] coreids;
  delete [] bufferlock;
  delete [] bufferinfo;
  delete [] filesread;
  delete [] confignumfiles;
}


void DataStream::initialise()
{
  int currentconfigindex, currentoverflowbytes, overflowbytes = 0;
  bufferbytes = databufferfactor*config->getMaxDataBytes(streamnum);
  readbytes = bufferbytes/numdatasegments;
  estimatedbytes = config->getEstimatedBytes();

  //cinfo << startl << "******DATASTREAM " << mpiid << ": Initialise. bufferbytes=" << bufferbytes << "  numdatasegments=" << numdatasegments << "  readbytes=" << readbytes << endl;

  for(int i=0;i<config->getNumConfigs();i++) {
    currentoverflowbytes = int((((long long)config->getDataBytes(i,streamnum))*((long long)(config->getSubintNS(i) + config->getGuardNS(i))))/config->getSubintNS(i));
    if(currentoverflowbytes > overflowbytes)
      overflowbytes = currentoverflowbytes;
  }
  cinfo << startl << "DATASTREAM " << mpiid << " about to allocate " << bufferbytes << " + " << overflowbytes << " bytes in databuffer" << endl;
  databuffer = vectorAlloc_u8(bufferbytes + overflowbytes + 4); // a couple extra for mark5 case
  estimatedbytes += bufferbytes + overflowbytes + 4;
  if(databuffer == NULL) {
    cfatal << startl << "Error - datastream " << mpiid << " could not allocate databuffer (length " << bufferbytes + overflowbytes << ")! Aborting correlation" << endl;
    MPI_Abort(MPI_COMM_WORLD, 1);
  }
  int mindatabytes = config->getDataBytes(0, streamnum);
  for(int i=1;i<config->getNumConfigs();i++)
  {
    if(config->getDataBytes(i, streamnum) < mindatabytes)
      mindatabytes = config->getDataBytes(i, streamnum);
  }
  maxsendspersegment = 3*bufferbytes/(mindatabytes*numdatasegments); //overkill, can get more sends than you would expect with MkV data

  stationname = config->getDStationName(0, streamnum);
  intclockseconds = int(floor(config->getDClockCoeff(0, streamnum, 0)/1000000.0 + 0.5));
  corrstartday = config->getStartMJD();
  corrstartseconds = config->getStartSeconds();

  //threaded initialisation
  bufferlock = new pthread_mutex_t[numdatasegments];
  bufferinfo = new readinfo[numdatasegments];
  readscan = 0;
  currentconfigindex = config->getScanConfigIndex(readscan);
  while(currentconfigindex < 0 && readscan < model->getNumScans())
    currentconfigindex = config->getScanConfigIndex(++readscan);
  if(readscan == model->getNumScans()) {
    cfatal << "Couldn't find a scan for which we had a valid config - aborting!" << endl;
    MPI_Abort(MPI_COMM_WORLD, 1);
  }
  readseconds = 0;
  readfromfile = config->isReadFromFile(currentconfigindex, streamnum);

  for(int i=0;i<numdatasegments;i++)
  {
    pthread_mutex_init(&bufferlock[i], NULL);
    //set up all the parameters in this bufferinfo slot
    updateConfig(i);
    bufferinfo[i].numsent = 0;
    bufferinfo[i].scan = readscan;
    bufferinfo[i].scanseconds = 0;
    bufferinfo[i].scanns = 0;
    bufferinfo[i].validbytes = 0;
    bufferinfo[i].readto = false;
    bufferinfo[i].datarequests = new MPI_Request[maxsendspersegment];
    bufferinfo[i].controlrequests = new MPI_Request[maxsendspersegment];
    bufferinfo[i].controlbuffer = new s32*[maxsendspersegment];
    for(int j=0;j<maxsendspersegment;j++)
      bufferinfo[i].controlbuffer[j] = vectorAlloc_s32(config->getMaxBlocksPerSend()/FLAGS_PER_INT + 4);
  }
  pthread_mutex_init(&outstandingsendlock, NULL);

  filesread = new int[config->getNumConfigs()];
  confignumfiles = new int[config->getNumConfigs()];
  datafilenames = new string*[config->getNumConfigs()];
  for(int i=0;i<config->getNumConfigs();i++)
  {
    filesread[i] = 0;
    confignumfiles[i] = config->getDNumFiles(i, streamnum);
    datafilenames[i] = config->getDDataFileNames(i, streamnum);
  }

  numsent = 0;
  keepreading = true;
}

void DataStream::execute()
{
  cverbose << startl << "DATASTREAM " << mpiid << " has started execution" << endl;
  datastatuses = new MPI_Status[maxsendspersegment];
  controlstatuses = new MPI_Status[maxsendspersegment];
  MPI_Request msgrequest;
  MPI_Status msgstatus;
  int targetcore, status, action, startpos, bufferremaining, perr;
  int receiveinfo[4];
  
  waitsegment = 0;
  atsegment = 0; //this is the section of buffer we will start in
  status = vecNoErr;

  //read in some data from the first file and launch the reading/network thread
  initialiseMemoryBuffer();

  //get the first instruction on where to send data, and how much of it (three ints in a row - core index, seconds offset, ns offset)
  MPI_Irecv(receiveinfo, 4, MPI_INT, fxcorr::MANAGERID, MPI_ANY_TAG, MPI_COMM_WORLD, &msgrequest);
  
  while(status == vecNoErr)
  {
    //wait til the message has been received
    MPI_Wait(&msgrequest, &msgstatus);

    //store what the message tells us to do
    action = msgstatus.MPI_TAG;
    targetcore = receiveinfo[0];
    activescan = receiveinfo[1];
    activesec = receiveinfo[2];
    activens = receiveinfo[3];

    //now get another instruction while we process
    MPI_Irecv(receiveinfo, 4, MPI_INT, fxcorr::MANAGERID, MPI_ANY_TAG, MPI_COMM_WORLD, &msgrequest);

    if(action == DS_PROCESS) //send the appropriate data to the core specified
    {
      //work out the index from which to send data - if this overlsps with an existing send, call readdata
      //(waits until all sends in the zone have been received, then reads, and calculates the control array values)
      startpos = calculateControlParams(activescan, activesec, activens);

      //ensure that if we need to wrap around the end of the array the overflow is copied back
      bufferremaining = bufferbytes - startpos;
      if(bufferremaining < bufferinfo[atsegment].sendbytes)
      {
        status = vectorCopy_u8(databuffer, &databuffer[bufferbytes], bufferinfo[atsegment].sendbytes - bufferremaining);
        if(status != vecNoErr)
          csevere << startl << "Error copying in the DataStream data buffer!!!" << endl;
      }

      if(bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] == Mode::INVALID_SUBINT)
      {
        //bad or no data, don't waste time sending full length of junk
        MPI_Issend(&databuffer[startpos], 1, MPI_UNSIGNED_CHAR, targetcore, CR_PROCESSDATA, MPI_COMM_WORLD, &(bufferinfo[atsegment].datarequests[bufferinfo[atsegment].numsent]));
        MPI_Issend(bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent], bufferinfo[atsegment].controllength, MPI_INT, targetcore, CR_PROCESSCONTROL, MPI_COMM_WORLD, &(bufferinfo[atsegment].controlrequests[bufferinfo[atsegment].numsent]));
      }
      else
      {
        //data is ok
        MPI_Issend(&databuffer[startpos], bufferinfo[atsegment].sendbytes, MPI_UNSIGNED_CHAR, targetcore, CR_PROCESSDATA, MPI_COMM_WORLD, &(bufferinfo[atsegment].datarequests[bufferinfo[atsegment].numsent]));
        MPI_Issend(bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent], bufferinfo[atsegment].controllength, MPI_INT, targetcore, CR_PROCESSCONTROL, MPI_COMM_WORLD, &(bufferinfo[atsegment].controlrequests[bufferinfo[atsegment].numsent]));
      }

      bufferinfo[atsegment].numsent++;
      if(bufferinfo[atsegment].numsent >= maxsendspersegment) //can occur at the start when many come from segment 0
      {
        //wait til everything has sent so we can reset the requests and go again
        MPI_Waitall(maxsendspersegment, bufferinfo[atsegment].datarequests, datastatuses);
        MPI_Waitall(maxsendspersegment, bufferinfo[atsegment].controlrequests,  controlstatuses);
        bufferinfo[atsegment].numsent = 0;
      }
    }
    else //must have been a terminate signal
    {
      cinfo << startl << "DATASTREAM " << mpiid << " will terminate next round!!!" << endl;
      status = -1; //will terminate next round
      keepreading = false;
    }
  }

  //wait on all the sends 
  for(int i=0;i<numdatasegments;i++)
  {
    if(bufferinfo[i].numsent > 0)
    {
      MPI_Waitall(bufferinfo[i].numsent, bufferinfo[i].datarequests, datastatuses);
      MPI_Waitall(bufferinfo[i].numsent, bufferinfo[i].controlrequests,  controlstatuses);
      bufferinfo[i].numsent = 0;
    }
  }
  perr = pthread_cond_signal(&readcond);
  if(perr != 0)
    csevere << startl << "DataStream mainthread " << mpiid << " error trying to signal read thread to wake up!!!" << endl;

  //join the reading thread
  perr = pthread_mutex_unlock(&(bufferlock[atsegment]));
  if(perr != 0)
    csevere << startl << "Error in telescope mainthread unlock of buffer section!!!" << atsegment << endl;
  perr = pthread_mutex_unlock(&(bufferlock[(atsegment+1)%numdatasegments]));
  if(perr != 0)
    csevere << startl << "Error in telescope mainthread unlock of buffer section!!!" << (atsegment+1)%numdatasegments << endl;
  perr = pthread_join(readerthread, NULL);
  if(perr != 0)
    csevere << startl << "Error in closing telescope " << mpiid << " readerthread!!!" << endl;

  delete [] datastatuses;
  delete [] controlstatuses;
  cverbose << startl << "DATASTREAM " << mpiid << " terminating" << endl;
}

//the returned value MUST be between 0 and bufferlength
int DataStream::calculateControlParams(int scan, int offsetsec, int offsetns)
{
  int firstoffsetns, lastoffsetns, bufferindex, perr, blockbytes, segoffbytes, segoffns, srcindex;
  double delayus1, delayus2;
  bool foundok;

  srcindex = 0;
  if(model->getNumPhaseCentres(scan) == 1 && !model->isPointingCentreCorrelated(scan))
    srcindex = 1;
  //cinfo << startl << "Working on scan " << scan << " offsetsec " << offsetsec << ", offsetns " << offsetns << endl;
  //work out the first delay and the last delay and place in control buffer
  bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][0] = scan;
  foundok = model->calculateDelayInterpolator(scan, (double)offsetsec + ((double)offsetns)/1000000000.0, 0.0, 0, config->getDModelFileIndex(bufferinfo[atsegment].configindex, streamnum), srcindex, 0, &delayus1);
  delayus1 -= intclockseconds;
  firstoffsetns = offsetns - static_cast<int>(delayus1*1000);
  foundok = foundok && model->calculateDelayInterpolator(scan, (double)offsetsec + ((double)offsetns + bufferinfo[atsegment].numchannels*bufferinfo[atsegment].blockspersend*2*bufferinfo[atsegment].sampletimens)/1000000000.0, 0.0, 0, config->getDModelFileIndex(bufferinfo[atsegment].configindex, streamnum), srcindex, 0, &delayus2);
  delayus2 -= intclockseconds;
  if(!foundok) {
    cerror << startl << "Could not find a Model interpolator for scan " << scan << " offsetseconds " << offsetsec << " offsetns " << offsetns << " - will torch this subint!" << endl;
    bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
    return 0; //note exit here!!!!
  }
  lastoffsetns = offsetns + int(bufferinfo[atsegment].numchannels*bufferinfo[atsegment].blockspersend*2*bufferinfo[atsegment].sampletimens - 1000*delayus2 + 0.5);
  //cout << mpiid << ": delayus1 is " << delayus1 << ", delayus2 is " << delayus2 << endl;
  if(lastoffsetns < 0)
  {
    offsetsec -= 1;
    offsetns += 1000000000;
    firstoffsetns += 1000000000;
    lastoffsetns += 1000000000;
  }
  if(lastoffsetns < 0)
  {
    cerror << startl << "lastoffsetns less than 0 still! =" << lastoffsetns << endl;
  }

  if(scan < bufferinfo[atsegment].scan)
  {
    bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
    return 0; //note exit here!!!!
  }

  //while we have passed the first of our two locked sections, unlock that and lock the next - have two tests so sample difference can't overflow
  waitForSendComplete();

  if(!bufferinfo[atsegment].readto) //can only occur when *all* datastream files bad, hence no good data, ever
  {
    bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
    return 0; //note exit here!!!!
  }

  if(scan == bufferinfo[atsegment].scan && offsetsec < bufferinfo[atsegment].scanseconds - 1) //coarse test to see if its all bad
  {
    bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
    return 0; //note exit here!!!!
  }

  while((scan > bufferinfo[(atsegment+1)%numdatasegments].scan || (scan == bufferinfo[(atsegment+1)%numdatasegments].scan && (offsetsec > bufferinfo[(atsegment+1)%numdatasegments].scanseconds + 1 || ((offsetsec - bufferinfo[(atsegment+1)%numdatasegments].scanseconds)*1000000000 + (firstoffsetns - bufferinfo[(atsegment+1)%numdatasegments].scanns) >= 0)))) && (keepreading || (atsegment != lastvalidsegment)))
  {
    //test the to see if sends have completed from the wait segment
    waitForSendComplete();

    perr = pthread_mutex_lock(&(bufferlock[(atsegment+2)%numdatasegments]));
    if(perr != 0)
      csevere << startl << "Error in telescope mainthread lock of buffer section!!!" << (atsegment+2)%numdatasegments << endl;
    perr = pthread_mutex_unlock(&(bufferlock[atsegment]));
    if(perr != 0)
      csevere << startl << "Error in telescope mainthread unlock of buffer section!!!" << atsegment << endl;
    atsegment = (atsegment+1)%numdatasegments;
  }

  //in case the atsegment has changed...
  bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][0] = scan;

  //look at the segment we are in now - if we want to look wholly before this or the buffer segment is bad then 
  //can't continue, fill control buffer with -1 and bail out
  if((scan < bufferinfo[atsegment].scan) || (offsetsec < bufferinfo[atsegment].scanseconds - 1) || ((offsetsec - bufferinfo[atsegment].scanseconds < 2) && ((offsetsec - bufferinfo[atsegment].scanseconds)*1000000000 + lastoffsetns - bufferinfo[atsegment].scanns < 0)))
  {
    //if(mpiid == 1)
    //  cout << "Bailing out: was asked for scan " << scan << ", offset " << offsetsec << "/" << lastoffsetns << " and the segment I'm at is scan " << bufferinfo[atsegment].scan << ", offset " << bufferinfo[atsegment].scanseconds << "/" << bufferinfo[atsegment].scanns << endl;
    bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
    return 0; //note exit here!!!!
  }

  // FIXME -- talk to Adam about this next check.  -WFB
  if(offsetsec > bufferinfo[atsegment].scanseconds + 1)
  {
    //if(mpiid == 1)
    //  cout << "Bailing out2: was asked for scan " << scan << ", offset " << offsetsec << "/" << lastoffsetns << " and the segment I'm at is scan " << bufferinfo[atsegment].scan << ", offset " << bufferinfo[atsegment].scanseconds << "/" << bufferinfo[atsegment].scanns << endl;
    bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
    return 0; //note exit here!!!!
  }
  
  //now that we obviously have a lock on all the data we need, fill the control buffer
  blockbytes = (bufferinfo[atsegment].numchannels*2*bufferinfo[atsegment].bytespersamplenum)/bufferinfo[atsegment].bytespersampledenom;

  if(bufferinfo[atsegment].validbytes < readbytes || bufferinfo[(atsegment+1)%numdatasegments].scanseconds - bufferinfo[atsegment].scanseconds > 5) //need to be careful - could be a big gap between this segment and the next that would cause int overflows
  {
    double dbufferindex = atsegment*readbytes + (((double(offsetsec - bufferinfo[atsegment].scanseconds)*1000000000.0 + (firstoffsetns - bufferinfo[atsegment].scanns))/bufferinfo[atsegment].sampletimens)*bufferinfo[atsegment].bytespersamplenum)/bufferinfo[atsegment].bytespersampledenom;
    if(dbufferindex < double(-blockbytes*bufferinfo[atsegment].blockspersend) || dbufferindex > double(bufferbytes)) //its way off, bail out
    {
      //if(mpiid == 1)
      //  cout << "Bailing out3: was asked for scan " << scan << ", offset " << offsetsec << "/" << lastoffsetns << " and the segment I'm at is scan " << bufferinfo[atsegment].scan << ", offset " << bufferinfo[atsegment].scanseconds << "/" << bufferinfo[atsegment].scanns << endl;
      bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
      return 0; //note exit here!!!!
    }
  }

  for(int i=0;i<bufferinfo[atsegment].blockspersend;i++) //zero the valid flags (so all invalid for now)
    bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][3+i/FLAGS_PER_INT] = 0;

  //if we make it here, its safe to make the int calculations below
  //cout << "Datastream " << mpiid << " has a firstoffsetns of " << firstoffsetns << " and an offsetsec of " << offsetsec << endl;
  bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = bufferinfo[atsegment].scanseconds;
  bufferindex = atsegment*readbytes + (int(((offsetsec - bufferinfo[atsegment].scanseconds)*1000000000 + (firstoffsetns - bufferinfo[atsegment].scanns))/bufferinfo[atsegment].sampletimens + 0.5)*bufferinfo[atsegment].bytespersamplenum)/bufferinfo[atsegment].bytespersampledenom;
  //cout << "And so the bufferindex is " << bufferindex << endl;

  //align the index to nearest previous 16 bit boundary
  if(bufferindex % 2 != 0)
    bufferindex--;

  int count=0;
  while(bufferindex < atsegment*readbytes && count < bufferinfo[atsegment].blockspersend)
  {
    bufferindex += blockbytes;
    count++;
  }
  if(bufferindex < atsegment*readbytes)
  {
    bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
    return 0;
  }

  //if we had to skip some, make sure we account for the possibly changed delay
  if(count > 0) {
    cout << "Datastream " << mpiid << " is realigning bufferindex - it was " << bufferindex << endl;
    bufferindex -= (int(count*1000.0*(delayus2 - delayus1)/(bufferinfo[atsegment].sampletimens*bufferinfo[atsegment].blockspersend) + 0.5)*bufferinfo[atsegment].bytespersamplenum)/bufferinfo[atsegment].bytespersampledenom;
    //re-align the index to nearest previous 16 bit boundary
    bufferindex -= bufferindex%2;
    cout << "Datastream " << mpiid << " has realigned bufferindex to " << bufferindex << endl;
  }
  //change the bufferindex backwards til we hit a byte boundary where the ns is an integer value
  segoffbytes = bufferindex - atsegment*readbytes;
  bufferindex -= segoffbytes % bufferinfo[atsegment].bytesbetweenintegerns;
  segoffbytes -= segoffbytes % bufferinfo[atsegment].bytesbetweenintegerns;
  segoffns = int(double(segoffbytes*bufferinfo[atsegment].bytespersampledenom/bufferinfo[atsegment].bytespersamplenum)* bufferinfo[atsegment].sampletimens + 0.5);
  bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] += segoffns/1000000000;
  bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][2] = bufferinfo[atsegment].scanns + segoffns%1000000000;

  //cout << "Count for datastream " << mpiid << " is " << count << ", and at this stage the first controlbuffer value is " << bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][3] << endl;
  if((bufferinfo[atsegment].validbytes - segoffbytes >= bufferinfo[atsegment].sendbytes) || (bufferinfo[atsegment].validbytes == readbytes && ((bufferinfo[(atsegment+1)%numdatasegments].scanseconds - bufferinfo[atsegment].scanseconds)*1000000000 + bufferinfo[(atsegment+1)%numdatasegments].scanns - bufferinfo[atsegment].scanns) == bufferinfo[atsegment].nsinc && (bufferinfo[atsegment].validbytes-segoffbytes+bufferinfo[(atsegment+1)%numdatasegments].validbytes) > bufferinfo[atsegment].sendbytes)) //they're all ok
  {
    for(int i=count;i<bufferinfo[atsegment].blockspersend;i++)
    {
      //all blocks are valid
      bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][3+i/FLAGS_PER_INT] |= 1<<(i%FLAGS_PER_INT);
    }
  }
  else
  {
    double dbufferindex = atsegment*readbytes + (((double(offsetsec - bufferinfo[atsegment].scanseconds)*1000000000.0 + (firstoffsetns - bufferinfo[atsegment].scanns))/bufferinfo[atsegment].sampletimens)*bufferinfo[atsegment].bytespersamplenum)/bufferinfo[atsegment].bytespersampledenom;
    if(dbufferindex < double(-blockbytes*bufferinfo[atsegment].blockspersend) || dbufferindex > double(bufferbytes)) //its way off, bail out
    {
      bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][1] = Mode::INVALID_SUBINT;
      return 0; //note exit here!!!!
    }
    for(int i=count;i<bufferinfo[atsegment].blockspersend;i++)
    {
      if(bufferindex + i*blockbytes - atsegment*readbytes < bufferinfo[atsegment].validbytes)
        bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][3+i/FLAGS_PER_INT] |= 1<<(i%FLAGS_PER_INT);
      else if(bufferinfo[atsegment].validbytes == readbytes && ((bufferinfo[(atsegment+1)%numdatasegments].scanseconds - bufferinfo[atsegment].scanseconds)*1000000000 + bufferinfo[(atsegment+1)%numdatasegments].scanns - bufferinfo[atsegment].scanns) == bufferinfo[atsegment].nsinc && (bufferinfo[atsegment].validbytes-segoffbytes+bufferinfo[(atsegment+1)%numdatasegments].validbytes) > i*blockbytes)
        bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][3+i/FLAGS_PER_INT] |= 1<<(i%FLAGS_PER_INT);
    }
  }

  return bufferindex;
}

void DataStream::initialiseMemoryBuffer()
{
  int perr;
  readthreadstarted = false;
  cverbose << startl << "DATASTREAM " << mpiid << " started initialising memory buffer" << endl;

  perr = pthread_mutex_lock(&bufferlock[0]);
  if(perr != 0)
    csevere << startl << "Error in main thread locking buffer segment " << 0 << endl;
  perr = pthread_mutex_lock(&bufferlock[1]);
  if(perr != 0)
    csevere << startl << "Error in main thread locking buffer segment " << 1 << endl;

  //initialise the condition signals
  pthread_cond_init(&readcond, NULL);
  pthread_cond_init(&initcond, NULL);

  if(readfromfile)
  {
    //launch the file reader thread
    perr = pthread_create(&readerthread, NULL, DataStream::launchNewFileReadThread, (void *)(this));
    if(perr != 0)
      csevere << startl << "Error in launching telescope readerthread!!!" << endl;
  }
  else
  {
    //launch the network reader thread
    perr = pthread_create(&readerthread, NULL, DataStream::launchNewNetworkReadThread, (void *)(this));
    if(perr != 0)
      csevere << startl << "Error in launching telescope networkthread!!!" << endl;
  }
  
  while(!readthreadstarted) //wait to ensure the thread got started ok
  {
    perr = pthread_cond_wait(&initcond, &(bufferlock[1]));
    if (perr != 0)
      csevere << startl << "Error waiting on readthreadstarted condition!!!!" << endl;
  }

  cverbose << startl << "DATASTREAM " << mpiid << " finished initialising memory buffer" << endl;
}

void DataStream::updateConfig(int segmentindex)
{
  //work out what the new config will be
  //cout << "Setting configindex of segment " << segmentindex << " to " << config->getScanConfigIndex(readscan) << " from scan " << readscan << endl;
  bufferinfo[segmentindex].configindex = config->getScanConfigIndex(readscan);
  if(bufferinfo[segmentindex].configindex < 0)
  {
    cerror << startl << "Datastream " << mpiid << " read a file containing data from a source for which we had no configuration - leaving parameters unchanged" << endl;
    return;
  }

  //set everything as directed by the config object
  bufferinfo[segmentindex].sendbytes = int((((long long)config->getDataBytes(bufferinfo[segmentindex].configindex,streamnum))*((long long)(config->getSubintNS(bufferinfo[segmentindex].configindex) + config->getGuardNS(bufferinfo[segmentindex].configindex))))/ config->getSubintNS(bufferinfo[segmentindex].configindex));
  bufferinfo[segmentindex].blockspersend = config->getBlocksPerSend(bufferinfo[segmentindex].configindex);
  bufferinfo[segmentindex].controllength = bufferinfo[segmentindex].blockspersend/FLAGS_PER_INT + 3;
  if(bufferinfo[segmentindex].blockspersend%FLAGS_PER_INT > 0)
    bufferinfo[segmentindex].controllength++;
  bufferinfo[segmentindex].bytespersamplenum = config->getDBytesPerSampleNum(bufferinfo[segmentindex].configindex, streamnum);
  bufferinfo[segmentindex].bytespersampledenom = config->getDBytesPerSampleDenom(bufferinfo[segmentindex].configindex, streamnum);
  bufferinfo[segmentindex].numchannels = config->getFNumChannels(config->getDRecordedFreqIndex(bufferinfo[segmentindex].configindex, streamnum, 0));
  bufferinfo[segmentindex].sampletimens = 500.0/config->getDRecordedBandwidth(bufferinfo[segmentindex].configindex,streamnum,0);
  bufferinfo[segmentindex].bytesbetweenintegerns = 0;
  double nsaccumulate = 0.0;
  do {
    nsaccumulate += bufferinfo[segmentindex].bytespersampledenom*bufferinfo[segmentindex].sampletimens;
    bufferinfo[segmentindex].bytesbetweenintegerns += bufferinfo[segmentindex].bytespersamplenum;
  } while (!(fabs(nsaccumulate - int(nsaccumulate)) < Mode::TINY));
  bufferinfo[segmentindex].nsinc = int((bufferinfo[segmentindex].sampletimens*(bufferbytes/numdatasegments)*bufferinfo[segmentindex].bytespersampledenom)/(bufferinfo[segmentindex].bytespersamplenum) + 0.5);
  portnumber = config->getDPortNumber(bufferinfo[segmentindex].configindex, streamnum);
  tcpwindowsizebytes = config->getDTCPWindowSizeKB(bufferinfo[segmentindex].configindex, streamnum)*1024;
  tcp = 1;
  if (tcpwindowsizebytes<0) {
    tcp = 0;
  }
}

void * DataStream::launchNewFileReadThread(void * thisstream)
{
  DataStream * me = (DataStream *)thisstream;
  me->loopfileread();

  return 0;
}

void * DataStream::launchNewNetworkReadThread(void * thisstream)
{
  DataStream * me = (DataStream *)thisstream;
  me->loopnetworkread();

  return 0;
}

void DataStream::loopfileread()
{
  int perr;
  int numread = 0;

  //lock the outstanding send lock
  perr = pthread_mutex_lock(&outstandingsendlock);
  if(perr != 0)
    csevere << startl << "Error in initial telescope readthread lock of outstandingsendlock!!!" << endl;

  //lock the first section to start reading
  dataremaining = false;
  while(!dataremaining && keepreading) {
    openfile(bufferinfo[0].configindex, filesread[bufferinfo[0].configindex]++);
    if(!dataremaining)
      input.close();
  }
  if(keepreading) {
    diskToMemory(numread++);
    diskToMemory(numread++);
    perr = pthread_mutex_lock(&(bufferlock[numread]));
    if(perr != 0)
      csevere << startl << "Error in initial telescope readthread lock of first buffer section!!!" << endl;
  }
  else
  {
    csevere << startl << "Couldn't find any valid data - will be shutting down gracefully!!!" << endl;
  }
  readthreadstarted = true;
  perr = pthread_cond_signal(&initcond);
  if(perr != 0)
    csevere << startl << "Datastream readthread " << mpiid << " error trying to signal main thread to wake up!!!" << endl;
  if(keepreading)
    diskToMemory(numread++);

  lastvalidsegment = (numread-1)%numdatasegments;
  while(keepreading && (bufferinfo[lastvalidsegment].configindex < 0 || filesread[bufferinfo[lastvalidsegment].configindex] <= confignumfiles[bufferinfo[lastvalidsegment].configindex]))
  {
    while(dataremaining && keepreading)
    {
      lastvalidsegment = (lastvalidsegment + 1)%numdatasegments;

      //lock the next section
      perr = pthread_mutex_lock(&(bufferlock[lastvalidsegment]));
      if(perr != 0)
        csevere << startl << "Error in telescope readthread lock of buffer section!!!" << lastvalidsegment << endl;

      //unlock the previous section
      perr = pthread_mutex_unlock(&(bufferlock[(lastvalidsegment-1+numdatasegments)% numdatasegments]));
      if(perr != 0)
        csevere << startl << "Error in telescope readthread unlock of buffer section!!!" << (lastvalidsegment-1+numdatasegments)%numdatasegments << endl;

      //do the read
      diskToMemory(lastvalidsegment);
      numread++;
    }
    if(keepreading)
    {
      input.close();
      //if we need to, change the config
      int nextconfigindex = config->getScanConfigIndex(readscan);
      while(nextconfigindex < 0 && readscan < model->getNumScans()) {
        readseconds = 0; 
        nextconfigindex = config->getScanConfigIndex(++readscan);
      }
      if(readscan == model->getNumScans())
      {
        bufferinfo[(lastvalidsegment+1)%numdatasegments].scan = model->getNumScans()-1;
        bufferinfo[(lastvalidsegment+1)%numdatasegments].scanseconds = model->getScanDuration(model->getNumScans()-1);
        bufferinfo[(lastvalidsegment+1)%numdatasegments].scanns = 0;
        keepreading = false;
      }
      else
      {
        if(config->getScanConfigIndex(readscan) != bufferinfo[(lastvalidsegment + 1)%numdatasegments].configindex)
          updateConfig((lastvalidsegment + 1)%numdatasegments);
        //if the datastreams for two or more configs are common, they'll all have the same 
        //files.  Therefore work with the lowest one
        int lowestconfigindex = bufferinfo[(lastvalidsegment+1)%numdatasegments].configindex;
        for(int i=config->getNumConfigs()-1;i>=0;i--)
        {
          if(config->getDDataFileNames(i, streamnum) == config->getDDataFileNames(lowestconfigindex, streamnum))
            lowestconfigindex = i;
        }
        openfile(lowestconfigindex, filesread[lowestconfigindex]++);
        bool skipsomefiles = (config->getScanConfigIndex(readscan) < 0)?true:false;
        while(skipsomefiles) {
          int nextscan = peekfile(lowestconfigindex, filesread[lowestconfigindex]);
          if(nextscan == readscan || (nextscan == readscan+1 && config->getScanConfigIndex(nextscan) < 0))
            openfile(lowestconfigindex, filesread[lowestconfigindex]++);
          else
            skipsomefiles = false;
        }
      }
    }
  }
  if(input.is_open())
    input.close();
  if(numread > 0) {
    perr = pthread_mutex_unlock(&(bufferlock[lastvalidsegment]));
    if(perr != 0)
      csevere << startl << "Error in telescope readthread unlock of buffer section!!!" << lastvalidsegment << endl;
  }

  //unlock the outstanding send lock
  perr = pthread_mutex_unlock(&outstandingsendlock);
  if(perr != 0)
    csevere << startl << "Error in telescope readthread unlock of outstandingsendlock!!!" << endl;

  if(lastvalidsegment >= 0)
    cverbose << startl << "DATASTREAM " << mpiid << "'s readthread is exiting!!! Filecount was " << filesread[bufferinfo[lastvalidsegment].configindex] << ", confignumfiles was " << confignumfiles[bufferinfo[lastvalidsegment].configindex] << ", dataremaining was " << dataremaining << ", keepreading was " << keepreading << endl;
  else
    cverbose << startl << "DATASTREAM " << mpiid << "'s readthread is exiting, after not finding any data at all!" << endl;
}

void DataStream::loopnetworkread()
{
  int perr, framebytesremaining, lastvalidsegment;

  //lock the outstanding send lock
  perr = pthread_mutex_lock(&outstandingsendlock);
  if(perr != 0)
    csevere << startl << "Error in initial telescope readthread lock of outstandingsendlock!!!" << endl;

  //open the socket
  openstream(portnumber, tcpwindowsizebytes);

  //start the first frame
  framebytesremaining = openframe();
  lastvalidsegment = 0;
  networkToMemory(lastvalidsegment, framebytesremaining);
  networkToMemory(++lastvalidsegment, framebytesremaining);

  //lock the segment we are at now
  perr = pthread_mutex_lock(&(bufferlock[lastvalidsegment + 1]));
  if(perr != 0)
    csevere << startl << "Error in initial telescope networkreadthread lock of first buffer section!!!" << endl;
  readthreadstarted = true;
  perr = pthread_cond_signal(&initcond);
  if(perr != 0)
    csevere << startl << "Datastream networkreadthread " << mpiid << " error trying to signal main thread to wake up!!!" << endl;
  networkToMemory(++lastvalidsegment, framebytesremaining);

  //loop until we have to stop
  while(keepreading)
  {
    while(framebytesremaining > 0 && keepreading)
    {
      lastvalidsegment = (lastvalidsegment + 1)%numdatasegments;

      //lock the next section
      perr = pthread_mutex_lock(&(bufferlock[lastvalidsegment]));
      if(perr != 0)
        csevere << startl << "Error in telescope readthread lock of buffer section!!!" << lastvalidsegment << endl;

      //unlock the previous section
      perr = pthread_mutex_unlock(&(bufferlock[(lastvalidsegment-1+numdatasegments)% numdatasegments]));
      if(perr != 0)
        csevere << startl << "Error in telescope readthread unlock of buffer section!!!" << (lastvalidsegment-1+numdatasegments)%numdatasegments << endl;

      //do the read
      networkToMemory(lastvalidsegment, framebytesremaining);
    }
    if(keepreading)
    {
      framebytesremaining = openframe();
      if(config->getScanConfigIndex(readscan) != bufferinfo[(lastvalidsegment+1)%numdatasegments].configindex)
        updateConfig((lastvalidsegment + 1)%numdatasegments);
    }
  }
  closestream();
  
  perr = pthread_mutex_unlock(&(bufferlock[lastvalidsegment]));
  if(perr != 0)
    csevere << startl << "Error in telescope readthread unlock of buffer section!!!" << lastvalidsegment << endl;

  //unlock the outstanding send lock
  perr = pthread_mutex_unlock(&outstandingsendlock);
  if(perr != 0)
    csevere << startl << "Error in telescope readthread unlock of outstandingsendlock!!!" << endl;

  cinfo << startl << "DATASTREAM " << mpiid << "'s networkreadthread is exiting!!! Keepreading was " << keepreading << ", framebytesremaining was " << framebytesremaining << endl;
}

void DataStream::openstream(int portnumber, int tcpwindowsizebytes)
{
  //okay - this has no counterpart in the disk case.  Just gets the socket open
  int serversock, status;
  socklen_t client_len;
  struct sockaddr_in server, client;    /* Socket address */

  /* Open a server connection for reading */

  /* Initialise server's address */
  memset((char *)&server,0,sizeof(server));
  server.sin_family = AF_INET;
  //server.sin_addr.s_addr = htonl(INADDR_ANY); /* Anyone can connect */
  server.sin_port = htons((unsigned short)portnumber); /* Which port number to use */

  if (tcp) { // TCP socket
    cinfo << startl << "Datastream " << mpiid << ": Creating a TCP socket on port " << portnumber << endl;
    /* Create a server to listen with */
    serversock = socket(AF_INET,SOCK_STREAM,0); 
    if (serversock==-1) 
      cerror << startl << "Error creating socket" << endl;

    /* Set the TCP window size */

    if (tcpwindowsizebytes>0) {
      cinfo << startl << "Datastream " << mpiid << ": Set TCP window to " << int(tcpwindowsizebytes/1024) << " kB" << endl;
      status = setsockopt(serversock, SOL_SOCKET, SO_RCVBUF,
		 (char *) &tcpwindowsizebytes, sizeof(tcpwindowsizebytes));

      if (status!=0) {
	cerror << startl << "Datastream " << mpiid << ": Error setting socket RCVBUF" << endl;
	close(serversock);
      } 

      int window_size;
      socklen_t winlen = sizeof(window_size);
      status = getsockopt(serversock, SOL_SOCKET, SO_RCVBUF,
			  (char *) &window_size, &winlen);
      if (status!=0) {
	cerror << startl << "Datastream " << mpiid << ": Error getting socket RCVBUF" << endl;
      }
      cinfo << startl << "Datastream " << mpiid << ": TCP window set to " << window_size/1024 << " bytes" << endl;

    }
  } else { // UDP socket  
    cinfo << startl << "Datastream " << mpiid << ": Creating a UDP socket on port " << portnumber << endl;
    serversock = socket(AF_INET,SOCK_DGRAM, IPPROTO_UDP); 
    if (serversock==-1) 
      cerror << startl << "Error creating UDP socket" << endl;
    // Should exit here on error

    int udpbufbytes = 32*1024*1024;
    status = setsockopt(serversock, SOL_SOCKET, SO_RCVBUF,
			(char *) &udpbufbytes, sizeof(udpbufbytes));
    
    if (status!=0) {
      cerror << startl << "Datastream " << mpiid << ": Error setting socket RCVBUF" << endl;
	close(serversock);
    } 
  }

  status = bind(serversock, (struct sockaddr *)&server, sizeof(server));
  if (status!=0) {
    cerror << startl << "Datastream " << mpiid << ": Error binding socket" << endl;
    close(serversock);
  } 
  
  if (tcp) { // TCP

    /* We are willing to receive conections, using the maximum
       back log of 1 */
    status = listen(serversock,1);
    if (status!=0) {
      cerror << startl << "Datastream " << mpiid << ": Error binding socket" << endl;
      close(serversock);
    }

    cinfo << startl << "Datastream " << mpiid << ": Waiting for connection" << endl;

    /* Accept connection */
    client_len = sizeof(client);
    socketnumber = accept(serversock, (struct sockaddr *)&client, &client_len);
    if (socketnumber == -1) {
      cerror << startl << "Datastream " << mpiid << ": Error connecting to client" << endl;
      close(serversock);
    }
    cinfo << startl << "Datastream " << mpiid << " got a connection from " << inet_ntoa(client.sin_addr) << endl;
  } else {  // UDP
    socketnumber = serversock;
    cinfo << startl << "Datastream " << mpiid << ": Ready to receive UDP data" << endl;
  }
}

void DataStream::closestream()
{
  //closes the socket
  int status;

  status = close(socketnumber);
  if (status!=0) 
    cerror << startl << "Error closing socket" << endl;
}

int DataStream::openframe()
{
  char *buf;
  short fnamesize;
  int ntoread, nread, status;
  unsigned long long framesize;

  ntoread = sizeof(long long) + sizeof(short);

  buf = (char*)malloc(LBA_HEADER_LENGTH); // Minimum size of file header

  //cinfo << startl << "About to open frame" << endl;
  status = readnetwork(socketnumber, buf, ntoread, &nread);
  //cinfo << startl << "Read first network successfully" << endl;
  if (status==-1) { // Error reading socket
    cerror << startl << "Error reading socket" << endl;
    keepreading=false;
    return(0);
  } else if (status==0) {  // Socket closed remotely
    keepreading=false;
    return(0);
  } else if (nread!=ntoread) { // This should never happen
    keepreading=false;
    cerror << startl << "Error reading network header" << endl;
    return(0);
  }
	
  // Read totalnumber of expected bytes and filename size
  memcpy(&framesize,  buf, sizeof(long long));
  memcpy(&fnamesize,  buf+sizeof(long long), sizeof(short));
  
  framesize = framesize - LBA_HEADER_LENGTH;

  if (framesize>INT_MAX) {
    keepreading=false;
    cerror << startl << "Network stream trying to send too large frame. Aborting!" << endl;
    return(0);
  }

  // Read filename size then ignore it
  if (fnamesize>LBA_HEADER_LENGTH) {
    buf = (char *)realloc(buf, fnamesize);
  }
  status = readnetwork(socketnumber, buf, fnamesize, &nread);
  if (status==-1) { // Error reading socket
    cerror << startl << "Error reading socket" << endl;
    keepreading=false;
    return(0);
  } else if (status==0) {  // Socket closed remotely
    keepreading=false;
    return(0);
  } else if (nread!=fnamesize) { // This should never happen
    keepreading=false;
    cerror << startl << "Error reading network header" << endl;
    return(0);
  }

  // Check to see if this is an old style header first
  ntoread = sizeof(long long) + sizeof(long long);
  status = readnetwork(socketnumber, buf, ntoread, &nread);
  if (status==-1) { // Error reading socket
    cerror << startl << "Error reading socket" << endl;
    keepreading=false;
    return(0);
  } else if (status==0) {  // Socket closed remotely
    keepreading=false;
    return(0);
  } else if (nread!=ntoread) { // This should never happen
    keepreading=false;
    cerror << startl << "Error file header" << endl;
    return(0);
  }

  if (buf[8] != ':') {
    // Read file header and extract time from it
    status = readnetwork(socketnumber, &(buf[16]), LBA_HEADER_LENGTH - 16, &nread);
    if (status==-1) { // Error reading socket
      cerror << startl << "Error reading socket" << endl;
      keepreading=false;
      return(0);
    } else if (status==0) {  // Socket closed remotely
      keepreading=false;
      return(0);
    } else if (nread!=LBA_HEADER_LENGTH-16) { // This should never happen
      keepreading=false;
      cerror << startl << "Error file header" << endl;
      return(0);
    }
  }

  status = initialiseFrame(buf);
  if (status < 0)
    return 0; //note exit here! Some problem with header

  free(buf);
  return framesize;
}

int DataStream::initialiseFrame(char * frameheader)
{
  int year, month, day, hour, minute, second;
  char * at;
  char * endline;
  string inputline;

  at = frameheader;
  if (at[8] != ':') { //not an old style LBA file
    while (strncmp(at,"TIME",4)!=0)
    {
      endline = index(at, '\n');
      if (endline==NULL || endline-frameheader>=LBA_HEADER_LENGTH-1) {
        cerror << startl << "Could not parse file header" << endl;
        keepreading=false;
        return -1;
      }
      at = endline+1;
    }
    endline = index(at, '\n');
    *endline = 0;
    at += 5;  // Skip over "TIME"
    inputline = at;
  } else {
    inputline = string(at, 15);
  }

  year = atoi((inputline.substr(0,4)).c_str());
  month = atoi((inputline.substr(4,2)).c_str());
  day = atoi((inputline.substr(6,2)).c_str());
  hour = atoi((inputline.substr(9,2)).c_str());
  minute = atoi((inputline.substr(11,2)).c_str());
  second = atoi((inputline.substr(13,2)).c_str());

  config->getMJD(filestartday, filestartseconds, year, month, day, hour, minute, second);

  cinfo << startl << "DATASTREAM " << mpiid << " worked out a filestartday of " << filestartday << " and a filestartseconds of " << filestartseconds << endl;

  //set readseconds, accounting for the intclockseconds
  readseconds = 86400*(filestartday-corrstartday) + (filestartseconds-corrstartseconds) + intclockseconds;
  readnanoseconds = 0;
  while(readscan < (model->getNumScans()-1) && model->getScanEndSec(readscan, corrstartday, corrstartseconds) < readseconds)
    readscan++;
  while(readscan > 0 && model->getScanStartSec(readscan, corrstartday, corrstartseconds) > readseconds)
    readscan--;
  readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);

  return 0;
}

void DataStream::networkToMemory(int buffersegment, int & framebytesremaining)
{
  char *ptr;
  int bytestoread, nread, status, synccatchbytes;

  //do the buffer housekeeping
  waitForBuffer(buffersegment);

  bytestoread = readbytes;
  if (bytestoread>framebytesremaining)
    bytestoread = framebytesremaining;

  // Check we will not read past end of file
  bufferinfo[buffersegment].validbytes = 0;
  ptr = (char*)&databuffer[buffersegment*(bufferbytes/numdatasegments)];

  status = readnetwork(socketnumber, ptr, bytestoread, &nread);
  if (status==-1) { // Error reading socket
    cerror << startl << "Datastream " << mpiid << ": Error reading socket" << endl;
    keepreading=false;
  } else if (status==0) {  // Socket closed remotely
    keepreading=false;
  } else {
    bufferinfo[buffersegment].validbytes = nread;
    framebytesremaining -= nread;
    bufferinfo[buffersegment].readto = true;
  }
  if (framebytesremaining<=0) {
    dataremaining = false;
  }

  synccatchbytes = testForSync(bufferinfo[buffersegment].configindex, buffersegment);
  if(synccatchbytes > 0) {
    status = readnetwork(socketnumber, ptr, synccatchbytes, &nread);
    if (status==-1) { // Error reading socket
      cerror << startl << "Datastream " << mpiid << ": Error reading socket" << endl;
      keepreading=false;
    } else if (status==0) {  // Socket closed remotely
      keepreading=false;
    } else {
      bufferinfo[buffersegment].validbytes -= (synccatchbytes - nread);
      framebytesremaining -= nread;
    }
    if (framebytesremaining<=0) {
      dataremaining = false;
    }
  }

  readnanoseconds += bufferinfo[buffersegment].nsinc;
  readseconds += readnanoseconds/1000000000;
  readnanoseconds %= 1000000000;
  if(readseconds >= model->getScanDuration(readscan)) {
    if(readscan < model->getNumScans()-1) {
      readscan++;
      readseconds -= model->getScanStartSec(readscan, corrstartday, corrstartseconds) - model->getScanStartSec(readscan-1, corrstartday, corrstartseconds);
    }
    else
      keepreading = false;
  }
}

int DataStream::readnetwork(int sock, char* ptr, int bytestoread, int* nread)
{
  int nr;

  *nread = 0;

  //cinfo << startl << "DATASTREAM " << mpiid << ": Reading " << bytestoread << " bytes" << endl;

  while (bytestoread>0)
  {
    nr = recv(sock,ptr,bytestoread,0);

    if (nr==-1) { // Error reading socket
      if (errno == EINTR) continue;
      return(nr);
    } else if (nr==0) {  // Socket closed remotely
      return(nr);
    } else {
      ptr += nr;
      *nread += nr;
      bytestoread -= nr;
    }
  }
  return(1);
}

int DataStream::peekfile(int configindex, int fileindex)
{
  if(fileindex >= confignumfiles[configindex])
    return readscan; //we're at the end - let openfile figure that out

  if(input.fail())
    input.clear(); //get around EOF problems caused by peeking
  input.open(datafilenames[configindex][fileindex].c_str(),ios::in);
  if(!input.is_open() || input.bad())
  {
    cerror << startl << "Error trying to peek at file " << datafilenames[configindex][fileindex] << endl;
    dataremaining = false;
    return readscan;
  }

  string inputline;
  int year, month, day, hour, minute, second, bytes, peekscan, peekseconds;
  char headerbuffer[LBA_HEADER_LENGTH];
  bytes = 0;

  getline(input, inputline);
  if(input.fail()) //get around problems caused by "peeking" for EOF
  {
    input.clear();
    getline(input, inputline);
  }

  if(inputline.length() != 15 || inputline.c_str()[8] != ':') //must be a new style header, find the time keyword
  {
    bytes = inputline.length() + 1;
    while(inputline.substr(0,4) != "TIME")
    {
      getline(input, inputline);
      bytes += inputline.length() + 1;
    }
    inputline = inputline.substr(5,15);
  }

  year = atoi((inputline.substr(0,4)).c_str());
  month = atoi((inputline.substr(4,2)).c_str());
  day = atoi((inputline.substr(6,2)).c_str());
  hour = atoi((inputline.substr(9,2)).c_str());
  minute = atoi((inputline.substr(11,2)).c_str());
  second = atoi((inputline.substr(13,2)).c_str());

  if(bytes != 0) //it was a new style header so we need to get the rest of the header
  {
    input.read(headerbuffer, LBA_HEADER_LENGTH - bytes);
  }

  //convert this date into MJD
  config->getMJD(filestartday, filestartseconds, year, month, day, hour, minute, second);

  //set readseconds, accounting for the intclockseconds
  peekscan = readscan;
  peekseconds = 86400*(filestartday-corrstartday) + (filestartseconds-corrstartseconds) + intclockseconds;
  while(model->getScanEndSec(peekscan, corrstartday, corrstartseconds) < peekseconds)
    peekscan++;
  while(model->getScanStartSec(peekscan, corrstartday, corrstartseconds) > peekseconds)
    peekscan--;
  if(peekscan < 0)
    peekscan = 0;
  if(peekscan >= model->getNumScans())
    peekscan = model->getNumScans() - 1;

  return peekscan;
}

void DataStream::openfile(int configindex, int fileindex)
{
  cverbose << startl << "DATASTREAM " << mpiid << " is about to try and open file index " << fileindex << " of configindex " << configindex << endl;
  if(fileindex >= confignumfiles[configindex]) //run out of files - time to stop reading
  {
    dataremaining = false;
    keepreading = false;
    cinfo << startl << "DATASTREAM " << mpiid << " is exiting because fileindex is " << fileindex << ", while confignumfiles is " << confignumfiles[configindex] << endl;
    return;
  }
  
  dataremaining = true;
  if(input.fail())
    input.clear(); //get around EOF problems caused by peeking
  //cout << "About to open fileindex " << fileindex << " of config " << configindex << endl;
  //cout << "It is called " << datafilenames[configindex][fileindex] << endl;
  input.open(datafilenames[configindex][fileindex].c_str(),ios::in);
  cverbose << startl << "input.bad() is " << input.bad() << ", input.fail() is " << input.fail() << endl;
  if(!input.is_open() || input.bad())
  {
    cerror << startl << "Error trying to open file " << datafilenames[configindex][fileindex] << endl;
    dataremaining = false;
    return;
  }

  cinfo << startl << "DATASTREAM " << mpiid << " has opened file index " << fileindex << ", which was " << datafilenames[configindex][fileindex] << endl;

  //read the header and set the appropriate times etc based on this information
  initialiseFile(configindex, fileindex);
}

void DataStream::initialiseFile(int configindex, int fileindex)
{
  string inputline;
  int year, month, day, hour, minute, second, bytes;
  char headerbuffer[LBA_HEADER_LENGTH];
  bytes = 0;

  getline(input, inputline);
  if(input.fail()) //get around problems caused by "peeking" for EOF
  {
    input.clear();
    getline(input, inputline);
  }

  if(inputline.length() != 15 || inputline.c_str()[8] != ':') //must be a new style header, find the time keyword
  {
    bytes = inputline.length() + 1;
    while(inputline.substr(0,4) != "TIME")
    {
      getline(input, inputline);
      bytes += inputline.length() + 1;
    }
    inputline = inputline.substr(5,15);
  }

  year = atoi((inputline.substr(0,4)).c_str());
  month = atoi((inputline.substr(4,2)).c_str());
  day = atoi((inputline.substr(6,2)).c_str());
  hour = atoi((inputline.substr(9,2)).c_str());
  minute = atoi((inputline.substr(11,2)).c_str());
  second = atoi((inputline.substr(13,2)).c_str());

  if(bytes != 0) //it was a new style header so we need to get the rest of the header
  {
    input.read(headerbuffer, LBA_HEADER_LENGTH - bytes);
    cinfo << startl << "Processed a new style header, all info ignored except date/time" << endl;
  }
  
  cinfo << startl << "DATASTREAM " << mpiid << " got the header " << inputline << " ok" << endl;

  //convert this date into MJD
  config->getMJD(filestartday, filestartseconds, year, month, day, hour, minute, second);

  cinfo << startl << "DATASTREAM " << mpiid << " worked out a filestartday of " << filestartday << " and a filestartseconds of " << filestartseconds << endl;

  //set readseconds, accounting for the intclockseconds
  readseconds = 86400*(filestartday-corrstartday) + (filestartseconds-corrstartseconds) + intclockseconds;
  readnanoseconds = 0;
  while(readscan < (model->getNumScans()-1) && model->getScanEndSec(readscan, corrstartday, corrstartseconds) < readseconds)
    readscan++;
  while(readscan > 0 && model->getScanStartSec(readscan, corrstartday, corrstartseconds) > readseconds)
    readscan--;
  //cout << "About to call model->getScanStartSec for scan " << readscan << endl;
  readseconds = readseconds - model->getScanStartSec(readscan, corrstartday, corrstartseconds);
  //cout << "Datastream " << mpiid << " just set readseconds to be " << readseconds << endl;
}

void DataStream::diskToMemory(int buffersegment)
{
  int synccatchbytes;

  //do the buffer housekeeping
  waitForBuffer(buffersegment);
  
  //read some data
  input.read((char*)&databuffer[buffersegment*(bufferbytes/numdatasegments)], readbytes);
  bufferinfo[buffersegment].validbytes = input.gcount();
  bufferinfo[buffersegment].readto = true;
  synccatchbytes = testForSync(bufferinfo[buffersegment].configindex, buffersegment);
  if(synccatchbytes > 0) {
    input.read((char*)&databuffer[(buffersegment+1)*(bufferbytes/numdatasegments) - synccatchbytes], synccatchbytes);
    bufferinfo[buffersegment].validbytes -= (synccatchbytes - input.gcount());
  }
  readnanoseconds += bufferinfo[buffersegment].nsinc;
  readseconds += readnanoseconds/1000000000;
  readnanoseconds %= 1000000000;
  if(readseconds >= model->getScanDuration(readscan)) {
    if(readscan < (model->getNumScans()-1)) {
      readscan++;
      readseconds -= model->getScanStartSec(readscan, corrstartday, corrstartseconds) - model->getScanStartSec(readscan-1, corrstartday, corrstartseconds);
    }
    else
      keepreading = false;
  }

  if(input.eof() || input.peek() == EOF)
  {
    dataremaining = false;
  }
}
int DataStream::testForSync(int configindex, int buffersegment)
{
  //can't test for sync with LBA files
  return 0;
}

void DataStream::waitForBuffer(int buffersegment)
{
  int perr;
  double bufferfullfraction = double((buffersegment-1-atsegment+numdatasegments)%numdatasegments)/double(numdatasegments);

  bufferinfo[buffersegment].scanns = readnanoseconds;
  bufferinfo[buffersegment].scanseconds = readseconds;
  bufferinfo[buffersegment].scan = readscan;

  //send a message once per pass through the buffer
  if(buffersegment == numdatasegments-1)
    cinfo << startl << "Datastream databuffer is " << int(bufferfullfraction*100 + 0.5) << "% full (max " << int(100.0*double(numdatasegments-1)/double(numdatasegments)) << "%)" << endl;

  //if we need to, change the config
  if(config->getScanConfigIndex(readscan) != bufferinfo[buffersegment].configindex)
    updateConfig(buffersegment);
    
  //ensure all the sends from this index have actually been made
  while(bufferinfo[buffersegment].numsent > 0)
  {
    perr = pthread_cond_wait(&readcond, &outstandingsendlock);
    if (perr != 0)
      csevere << startl << "Error waiting on ok to read condition!!!!" << endl;
    usleep(10);
  }
}

void DataStream::waitForSendComplete()
{
  int perr, dfinished, cfinished;
  bool testonly = (atsegment != (waitsegment - 2 + numdatasegments)%numdatasegments);

  if((atsegment - waitsegment + numdatasegments)%numdatasegments <= 2) //we are very close so don't bother
    return;

  if(bufferinfo[waitsegment].numsent > 0)
  {
    if(testonly) // we only need to test, we're close enough that we can afford to go one segment further ahead
    {
      MPI_Testall(bufferinfo[waitsegment].numsent, bufferinfo[waitsegment].datarequests, &dfinished, datastatuses);
      MPI_Testall(bufferinfo[waitsegment].numsent, bufferinfo[waitsegment].controlrequests, &cfinished, controlstatuses);
    }
    else
    {
      //have to wait for this segment before advancing
      MPI_Waitall(bufferinfo[waitsegment].numsent, bufferinfo[waitsegment].datarequests, datastatuses);
      MPI_Waitall(bufferinfo[waitsegment].numsent, bufferinfo[waitsegment].controlrequests, controlstatuses);
    }
    if(!testonly || (dfinished && cfinished)) // all the sends from this segment are finished
    {
      bufferinfo[waitsegment].numsent = 0;
      waitsegment = (waitsegment + 1)%numdatasegments;
      perr = pthread_cond_signal(&readcond);
      if(perr != 0)
        csevere << startl << "DATASTREAM mainthread " << mpiid << " error trying to signal read thread to wake up!!!" << endl;
    }
  }
  else //already done!  can advance for next time
    waitsegment = (waitsegment + 1)%numdatasegments;
}
