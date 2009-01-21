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

  //cinfo << startl << "******DATASTREAM " << mpiid << ": Initialise. bufferbytes=" << bufferbytes << "  numdatasegments=" << numdatasegments << "  readbytes=" << readbytes << endl;

  for(int i=0;i<config->getNumConfigs();i++) {
    currentoverflowbytes = int((((long long)config->getDataBytes(i,streamnum))*((long long)(config->getBlocksPerSend(i) + config->getGuardBlocks(i))))/config->getBlocksPerSend(i));
    if(currentoverflowbytes > overflowbytes)
      overflowbytes = currentoverflowbytes;
  }
  cinfo << startl << "DATASTREAM " << mpiid << " about to allocate " << bufferbytes << " + " << overflowbytes << " bytes in databuffer" << endl;
  databuffer = vectorAlloc_u8(bufferbytes + overflowbytes + 4); // a couple extra for mark5 case
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
  clockoffset = config->getDClockOffset(0, streamnum);
  intclockseconds = int(floor(clockoffset/1000000.0 + 0.5));
  clockoffset -= intclockseconds*1000000;
  clockrate = config->getDClockRate(0, streamnum);
  corrstartday = config->getStartMJD();
  corrstartseconds = config->getStartSeconds();

  //threaded initialisation
  bufferlock = new pthread_mutex_t[numdatasegments];
  bufferinfo = new readinfo[numdatasegments];
  if(!config->loaduvwinfo(true))
    MPI_Abort(MPI_COMM_WORLD, 1);
  readseconds = 0;
  currentconfigindex = config->getConfigIndex(readseconds);
  while(currentconfigindex < 0)
    currentconfigindex = config->getConfigIndex(++readseconds);
  readfromfile = config->isReadFromFile(currentconfigindex, streamnum);

  for(int i=0;i<numdatasegments;i++)
  {
    pthread_mutex_init(&bufferlock[i], NULL);
    //set up all the parameters in this bufferinfo slot
    updateConfig(i);
    bufferinfo[i].numsent = 0;
    bufferinfo[i].seconds = -9999;
    bufferinfo[i].nanoseconds = 0;
    bufferinfo[i].validbytes = 0;
    bufferinfo[i].datarequests = new MPI_Request[maxsendspersegment];
    bufferinfo[i].controlrequests = new MPI_Request[maxsendspersegment];
    bufferinfo[i].controlbuffer = new f64*[maxsendspersegment];
    for(int j=0;j<maxsendspersegment;j++)
      bufferinfo[i].controlbuffer[j] = vectorAlloc_f64(config->getMaxSendBlocks() + 1);
  }

  filesread = new int[config->getNumConfigs()];
  confignumfiles = new int[config->getNumConfigs()];
  datafilenames = new string*[config->getNumConfigs()];
  for(int i=0;i<config->getNumConfigs();i++)
  {
    filesread[i] = 0;
    confignumfiles[i] = config->getDNumFiles(i, streamnum);
    datafilenames[i] = config->getDDataFileNames(i, streamnum);
  }

  cverbose << startl << "Telescope " << stationname << " is about to process the delay file " << config->getDelayFileName() << endl;
  processDelayFile(config->getDelayFileName());

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
  int targetcore, offsetsec, offsetns, status, action, startpos, bufferremaining, perr;
  int receiveinfo[3];
  
  waitsegment = 0;
  atsegment = 0; //this is the section of buffer we will start in
  status = vecNoErr;

  //read in some data from the first file and launch the reading/network thread
  initialiseMemoryBuffer();

  //get the first instruction on where to send data, and how much of it (three ints in a row - core index, seconds offset, ns offset)
  MPI_Irecv(receiveinfo, 3, MPI_INT, fxcorr::MANAGERID, MPI_ANY_TAG, MPI_COMM_WORLD, &msgrequest);
  
  while(status == vecNoErr)
  {
    //wait til the message has been received
    MPI_Wait(&msgrequest, &msgstatus);

    //store what the message tells us to do
    action = msgstatus.MPI_TAG;
    targetcore = receiveinfo[0];
    offsetsec = receiveinfo[1];
    offsetns = receiveinfo[2];

    //now get another instruction while we process
    MPI_Irecv(receiveinfo, 3, MPI_INT, fxcorr::MANAGERID, MPI_ANY_TAG, MPI_COMM_WORLD, &msgrequest);

    if(action == DS_PROCESS) //send the appropriate data to the core specified
    {
      //work out the index from which to send data - if this overlsps with an existing send, call readdata
      //(waits until all sends in the zone have been received, then reads, and calculates the control array values)
      startpos = calculateControlParams(offsetsec, offsetns);

      //ensure that if we need to wrap around the end of the array the overflow is copied back
      bufferremaining = bufferbytes - startpos;
      if(bufferremaining < bufferinfo[atsegment].sendbytes)
      {
        status = vectorCopy_u8(databuffer, &databuffer[bufferbytes], bufferinfo[atsegment].sendbytes - bufferremaining);
        if(status != vecNoErr)
          csevere << startl << "Error copying in the DataStream data buffer!!!" << endl;
      }

      if(bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][0] == MAX_NEGATIVE_DELAY)
      {
        //bad or no data, don't waste time sending full length of junk
        MPI_Issend(&databuffer[startpos], 1, MPI_UNSIGNED_CHAR, targetcore, CR_PROCESSDATA, MPI_COMM_WORLD, &(bufferinfo[atsegment].datarequests[bufferinfo[atsegment].numsent]));
        MPI_Issend(bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent], bufferinfo[atsegment].controllength, MPI_DOUBLE, targetcore, CR_PROCESSCONTROL, MPI_COMM_WORLD, &(bufferinfo[atsegment].controlrequests[bufferinfo[atsegment].numsent]));
      }
      else
      {
        //data is ok
        MPI_Issend(&databuffer[startpos], bufferinfo[atsegment].sendbytes, MPI_UNSIGNED_CHAR, targetcore, CR_PROCESSDATA, MPI_COMM_WORLD, &(bufferinfo[atsegment].datarequests[bufferinfo[atsegment].numsent]));
        MPI_Issend(bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent], bufferinfo[atsegment].controllength, MPI_DOUBLE, targetcore, CR_PROCESSCONTROL, MPI_COMM_WORLD, &(bufferinfo[atsegment].controlrequests[bufferinfo[atsegment].numsent]));
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
}

//the returned value MUST be between 0 and bufferlength
int DataStream::calculateControlParams(int offsetsec, int offsetns)
{
  int intdelayseconds, firstoffsetns, lastoffsetns, bufferindex, perr, blockbytes;

  //work out the first delay and the last delay and place in control buffer
  intdelayseconds = ((offsetsec + corrstartseconds) - delaystartseconds) + 86400*(corrstartday - delaystartday);
  calculateQuadraticParameters(intdelayseconds, offsetns);
  firstoffsetns = offsetns - static_cast<int>(quadinterpolatedelay(intdelayseconds, offsetns)*1000);
  lastoffsetns = offsetns + int(bufferinfo[atsegment].numchannels*2*bufferinfo[atsegment].sampletimens - 1000*quadinterpolatedelay(intdelayseconds, offsetns+int(bufferinfo[atsegment].numchannels*2*bufferinfo[atsegment].sampletimens+0.5)) + 0.5);
  if(lastoffsetns < 0)
  {
    offsetsec -= 1;
    intdelayseconds -= 1;
    offsetns += 1000000000;
    firstoffsetns += 1000000000;
    lastoffsetns += 1000000000;
  }
  if(lastoffsetns < 0)
  {
    cerror << startl << "lastoffsetns less than 0 still! =" << lastoffsetns << endl;
  }

  //while we have passed the first of our two locked sections, unlock that and lock the next - have two tests so sample difference can't overflow
  waitForSendComplete();
  if(offsetsec < bufferinfo[atsegment].seconds - 1 || bufferinfo[atsegment].seconds < 0) //coarse test to see if its all bad
  {
    for(int i=0;i<bufferinfo[atsegment].controllength;i++)
      bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][i] = MAX_NEGATIVE_DELAY;
    return 0; //note exit here!!!!
  }

  while((offsetsec > bufferinfo[(atsegment+1)%numdatasegments].seconds + 1 || ((offsetsec - bufferinfo[(atsegment+1)%numdatasegments].seconds)*1000000000 + (firstoffsetns - bufferinfo[(atsegment+1)%numdatasegments].nanoseconds) >= 0)) && (keepreading || (atsegment != lastvalidsegment)))
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

  //look at the segment we are in now - if we want to look wholly before this or the buffer segment is bad then 
  //can't continue, fill control buffer with -1 and bail out
  if((offsetsec < bufferinfo[atsegment].seconds - 1) || ((offsetsec - bufferinfo[atsegment].seconds < 2) && ((offsetsec - bufferinfo[atsegment].seconds)*1000000000 + lastoffsetns - bufferinfo[atsegment].nanoseconds < 0)))
  {
    for(int i=0;i<bufferinfo[atsegment].controllength;i++)
      bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][i] = MAX_NEGATIVE_DELAY;
    return 0; //note exit here!!!!
  }

  // FIXME -- talk to Adam about this next check.  -WFB
  if(offsetsec > bufferinfo[atsegment].seconds + 1)
  {
    for(int i=0;i<bufferinfo[atsegment].controllength;i++)
      bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][i] = MAX_NEGATIVE_DELAY;
    return 0; //note exit here!!!!
  }
  
  //now that we obviously have a lock on all the data we need, fill the control buffer
  blockbytes = (bufferinfo[atsegment].numchannels*2*bufferinfo[atsegment].bytespersamplenum)/bufferinfo[atsegment].bytespersampledenom;
  if(bufferinfo[atsegment].validbytes < readbytes || bufferinfo[(atsegment+1)%numdatasegments].seconds - bufferinfo[atsegment].seconds > 5) //need to be careful - could be a big gap between this segment and the next that would cause int overflows
  {
    double dbufferindex = atsegment*readbytes + (((double(offsetsec - bufferinfo[atsegment].seconds)*1000000000.0 + (firstoffsetns - bufferinfo[atsegment].nanoseconds))/bufferinfo[atsegment].sampletimens)*bufferinfo[atsegment].bytespersamplenum)/bufferinfo[atsegment].bytespersampledenom;
    if(dbufferindex < double(-blockbytes*bufferinfo[atsegment].controllength) || dbufferindex > double(bufferbytes)) //its way off, bail out
    {
      for(int i=0;i<bufferinfo[atsegment].controllength;i++)
        bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][i] = MAX_NEGATIVE_DELAY;
      return 0; //note exit here!!!!
    }
  }

  //if we make it here, its safe to make the int calculations below
  bufferindex = atsegment*readbytes + (int(((offsetsec - bufferinfo[atsegment].seconds)*1000000000 + (firstoffsetns - bufferinfo[atsegment].nanoseconds))/bufferinfo[atsegment].sampletimens + 0.5)*bufferinfo[atsegment].bytespersamplenum)/bufferinfo[atsegment].bytespersampledenom;

  //align the index to nearest previous 16 bit boundary
  if(bufferindex%2 != 0)
    bufferindex -= 1;

  int count=0;
  while(bufferindex < 0 && count < bufferinfo[atsegment].controllength - 1)
  {
    bufferindex += blockbytes;
    bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][(count++) + 1] = MAX_NEGATIVE_DELAY;
  }
  if(bufferindex < 0)
  {
    for(int i=0;i<bufferinfo[atsegment].controllength;i++)
      bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][i] = MAX_NEGATIVE_DELAY;
    return 0;
  }

  bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][0] = bufferinfo[atsegment].seconds + (double(bufferinfo[atsegment].nanoseconds) + double(((bufferindex-(atsegment*readbytes))*bufferinfo[atsegment].bytespersampledenom)/ bufferinfo[atsegment].bytespersamplenum)*bufferinfo[atsegment].sampletimens)/1000000000.0;
  
  if(bufferinfo[atsegment].validbytes == readbytes && ((bufferinfo[(atsegment+1)%numdatasegments].seconds - bufferinfo[atsegment].seconds)*1000000000 + bufferinfo[(atsegment+1)%numdatasegments].nanoseconds - bufferinfo[atsegment].nanoseconds) == bufferinfo[atsegment].nsinc) //they're all ok
  {
    for(int i=count;i<bufferinfo[atsegment].controllength - 1;i++)
    {
      bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][i+1] = quadinterpolatedelay(intdelayseconds, offsetns + int(i*2*bufferinfo[atsegment].numchannels*bufferinfo[atsegment].sampletimens + 0.5));
    }
  }
  else
  {
    double dbufferindex = atsegment*readbytes + (((double(offsetsec - bufferinfo[atsegment].seconds)*1000000000.0 + (firstoffsetns - bufferinfo[atsegment].nanoseconds))/bufferinfo[atsegment].sampletimens)*bufferinfo[atsegment].bytespersamplenum)/bufferinfo[atsegment].bytespersampledenom;
    if(dbufferindex < double(-blockbytes*bufferinfo[atsegment].controllength) || dbufferindex > double(bufferbytes)) //its way off, bail out
    {
      for(int i=0;i<bufferinfo[atsegment].controllength;i++)
        bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][i] = MAX_NEGATIVE_DELAY;
      return 0; //note exit here!!!!
    }
    for(int i=count;i<bufferinfo[atsegment].controllength - 1;i++)
    {
      if(bufferindex + i*blockbytes - atsegment*readbytes < bufferinfo[atsegment].validbytes)
        bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][i+1] = quadinterpolatedelay(intdelayseconds, offsetns + int(i*2*bufferinfo[atsegment].numchannels*bufferinfo[atsegment].sampletimens + 0.5));
      else
      {
        bufferinfo[atsegment].controlbuffer[bufferinfo[atsegment].numsent][i+1] = MAX_NEGATIVE_DELAY;
      }
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

void DataStream::calculateQuadraticParameters(int intdelayseconds, int offsetns)
{
  int snindex;
  double delayoffsetseconds;

  delayoffsetseconds = intdelayseconds + double(offsetns)/1000000000.0;
  snindex = static_cast<int>((delayoffsetseconds*1000)/delayincms);
  if(snindex < 0)
  {
    cerror << startl << "Error - attempting to get a delay from offset time " << intdelayseconds << "." << offsetns << ", will take first source" << endl;
    snindex = 0;
  }
  else if (snindex >= totaldelays)
  {
    cerror << startl << "Error - attempting to get a delay from offset time " << intdelayseconds << "." << offsetns << ", will take last source" << endl;
    snindex = totaldelays-1;
  }
  lastscan = scannumbers[snindex];
  lastnearestindex = static_cast<int>(delayoffsetseconds*1000 + 0.5)/delayincms - scanstarts[lastscan] + 1;
  
  //recalculate a, b, c
  a = (delays[lastscan][lastnearestindex+1]+delays[lastscan][lastnearestindex-1])/2 - delays[lastscan][lastnearestindex];
  b = (delays[lastscan][lastnearestindex+1]-delays[lastscan][lastnearestindex-1])/2 + (clockrate*delayincms)/1000.0;
  c = delays[lastscan][lastnearestindex] + clockoffset + clockrate*intdelayseconds;
}

double DataStream::quadinterpolatedelay(int intdelayseconds, int offsetns)
{
  double offset, delayoffsetseconds;

  delayoffsetseconds = intdelayseconds + double(offsetns)/1000000000.0;
  offset = ((delayoffsetseconds*1000)/delayincms - (lastnearestindex + scanstarts[lastscan] - 1));

  return (a*offset + b)*offset + c;
}

void DataStream::updateConfig(int segmentindex)
{
  //work out what the new config will be
  bufferinfo[segmentindex].configindex = config->getConfigIndex(readseconds);
  if(bufferinfo[segmentindex].configindex < 0)
  {
    cerror << startl << "Warning - Datastream " << mpiid << " read a file containing data from a source for which we had no configuration - leaving parameters unchanged" << endl;
    return;
  }

  //set everything as directed by the config object
  bufferinfo[segmentindex].sendbytes = int((((long long)config->getDataBytes(bufferinfo[segmentindex].configindex,streamnum))*((long long)(config->getBlocksPerSend(bufferinfo[segmentindex].configindex) + config->getGuardBlocks(bufferinfo[segmentindex].configindex))))/ config->getBlocksPerSend(bufferinfo[segmentindex].configindex));
  bufferinfo[segmentindex].controllength = config->getBlocksPerSend(bufferinfo[segmentindex].configindex) + config->getGuardBlocks(bufferinfo[segmentindex].configindex) + 1; //extra one for the start time
  bufferinfo[segmentindex].bytespersamplenum = config->getDBytesPerSampleNum(bufferinfo[segmentindex].configindex, streamnum);
  bufferinfo[segmentindex].bytespersampledenom = config->getDBytesPerSampleDenom(bufferinfo[segmentindex].configindex, streamnum);
  bufferinfo[segmentindex].numchannels = config->getNumChannels(bufferinfo[segmentindex].configindex);
  bufferinfo[segmentindex].sampletimens = 500.0/config->getDBandwidth(bufferinfo[segmentindex].configindex,streamnum,0);
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

  //lock the first section to start reading
  openfile(bufferinfo[0].configindex, 0);
  filesread[bufferinfo[0].configindex]++;
  if(keepreading) {
    diskToMemory(numread++);
    diskToMemory(numread++);
    perr = pthread_mutex_lock(&(bufferlock[numread]));
    if(perr != 0)
      csevere << startl << "Error in initial telescope readthread lock of first buffer section!!!" << endl;
  }
  readthreadstarted = true;
  perr = pthread_cond_signal(&initcond);
  if(perr != 0)
    csevere << startl << "Datastream readthread " << mpiid << " error trying to signal main thread to wake up!!!" << endl;
  if(keepreading)
    diskToMemory(numread++);

  lastvalidsegment = (numread-1)%numdatasegments;
  while((bufferinfo[lastvalidsegment].configindex < 0 || filesread[bufferinfo[lastvalidsegment].configindex] <= confignumfiles[bufferinfo[lastvalidsegment].configindex]) && keepreading)
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
      int nextconfigindex = config->getConfigIndex(readseconds);
      while(nextconfigindex < 0 && readseconds < config->getExecuteSeconds())
        nextconfigindex = config->getConfigIndex(++readseconds);
      if(readseconds == config->getExecuteSeconds())
      {
        bufferinfo[(lastvalidsegment+1)%numdatasegments].seconds = config->getExecuteSeconds();
        bufferinfo[(lastvalidsegment+1)%numdatasegments].nanoseconds = 0;
        keepreading = false;
      }
      else
      {
        if(config->getConfigIndex(readseconds) != bufferinfo[(lastvalidsegment + 1)%numdatasegments].configindex)
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

  cverbose << startl << "DATASTREAM " << mpiid << "'s readthread is exiting!!! Filecount was " << filesread[bufferinfo[lastvalidsegment].configindex] << ", confignumfiles was " << confignumfiles[bufferinfo[lastvalidsegment].configindex] << ", dataremaining was " << dataremaining << ", keepreading was " << keepreading << endl;
}

void DataStream::loopnetworkread()
{
  int perr, framebytesremaining, lastvalidsegment;

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
      if(config->getConfigIndex(readseconds) != bufferinfo[(lastvalidsegment + 1)%numdatasegments].configindex)
        updateConfig((lastvalidsegment + 1)%numdatasegments);
    }
  }
  closestream();
  
  perr = pthread_mutex_unlock(&(bufferlock[lastvalidsegment]));
  if(perr != 0)
    csevere << startl << "Error in telescope readthread unlock of buffer section!!!" << lastvalidsegment << endl;

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

  // Read file header and extract time from it
  status = readnetwork(socketnumber, buf, LBA_HEADER_LENGTH, &nread);
  if (status==-1) { // Error reading socket
    cerror << startl << "Error reading socket" << endl;
    keepreading=false;
    return(0);
  } else if (status==0) {  // Socket closed remotely
    keepreading=false;
    return(0);
  } else if (nread!=LBA_HEADER_LENGTH) { // This should never happen
    keepreading=false;
    cerror << startl << "Error file header" << endl;
    return(0);
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

  year = atoi((inputline.substr(0,4)).c_str());
  month = atoi((inputline.substr(4,2)).c_str());
  day = atoi((inputline.substr(6,2)).c_str());
  hour = atoi((inputline.substr(9,2)).c_str());
  minute = atoi((inputline.substr(11,2)).c_str());
  second = atoi((inputline.substr(13,2)).c_str());

  config->getMJD(filestartday, filestartseconds, year, month, day, hour, minute, second);

  cinfo << startl << "DATASTREAM " << mpiid << " worked out a filestartday of " << filestartday << " and a filestartseconds of " << filestartseconds << endl;

  readseconds = 86400*(filestartday-corrstartday) + (filestartseconds-corrstartseconds) + intclockseconds;
  readnanoseconds = 0;
  
  return 0;
}

void DataStream::networkToMemory(int buffersegment, int & framebytesremaining)
{
  char *ptr;
  int bytestoread, nread, status;

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
  }

  if (framebytesremaining<=0) {
    dataremaining = false;
  }

  readnanoseconds += bufferinfo[buffersegment].nsinc;
  readseconds += readnanoseconds/1000000000;
  readnanoseconds %= 1000000000;

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

void DataStream::waitForBuffer(int buffersegment)
{
  int perr;
  bufferinfo[buffersegment].nanoseconds = readnanoseconds;
  bufferinfo[buffersegment].seconds = readseconds;

  //if we need to, change the config
  if(config->getConfigIndex(readseconds) != bufferinfo[buffersegment].configindex)
    updateConfig(buffersegment);
    
  //ensure all the sends from this index have actually been made
  while(bufferinfo[buffersegment].numsent > 0)
  {
    perr = pthread_cond_wait(&readcond, &(bufferlock[buffersegment]));
    if (perr != 0)
      csevere << startl << "Error waiting on ok to read condition!!!!" << endl;
    usleep(20);
  }
}

void DataStream::waitForSendComplete()
{
  int perr, finished;
  bool testonly = (atsegment != (waitsegment - 2 + numdatasegments)%numdatasegments);
  
  if(bufferinfo[waitsegment].numsent > 0)
  {
    if((atsegment - waitsegment + numdatasegments)%numdatasegments <= 2) //we are very close so don't bother
      return;
  
    if(testonly) // we only need to test, we're close enough that we can afford to go one segment further ahead
    {
      MPI_Testall(bufferinfo[waitsegment].numsent, bufferinfo[waitsegment].datarequests, &finished, datastatuses);
      MPI_Testall(bufferinfo[waitsegment].numsent, bufferinfo[waitsegment].controlrequests, &finished, controlstatuses);
    }
    else
    {
      //have to wait for this segment before advancing
      MPI_Waitall(bufferinfo[waitsegment].numsent, bufferinfo[waitsegment].datarequests, datastatuses);
      MPI_Waitall(bufferinfo[waitsegment].numsent, bufferinfo[waitsegment].controlrequests, controlstatuses);
    }
    if(!testonly || finished) // all the sends from this segment are finished
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

void DataStream::diskToMemory(int buffersegment)
{
  //do the buffer housekeeping
  waitForBuffer(buffersegment);
  
  //read some data
  input.read((char*)&databuffer[buffersegment*(bufferbytes/numdatasegments)], readbytes);
  bufferinfo[buffersegment].validbytes = input.gcount();
  readnanoseconds += bufferinfo[buffersegment].nsinc;
  readseconds += readnanoseconds/1000000000;
  readnanoseconds %= 1000000000;

  if(input.eof() || input.peek() == EOF)
  {
    dataremaining = false;
  }
}

void DataStream::processDelayFile(string delayfilename)
{
  string line;
  int year, month, day, hour, minute, second, numdelaytelescopes, numscans, at, next, numfound, columnoffset;
  string * delayTelescopeNames;
  double * linedelays;

  columnoffset = -1;

  //read in the delays and store in the Telescope objects
  ifstream delayinput(delayfilename.c_str(),ios::in);
  if(!delayinput.is_open() || delayinput.bad()) {
    cfatal << startl << "Error opening delay file " << delayfilename << " - aborting!!!" << endl;
    MPI_Abort(MPI_COMM_WORLD, 1);
  }

  config->getinputline(&delayinput, &line, "START YEAR");
  year = atoi(line.c_str());
  config->getinputline(&delayinput, &line, "START MONTH");
  month = atoi(line.c_str());
  config->getinputline(&delayinput, &line, "START DAY");
  day = atoi(line.c_str());
  config->getinputline(&delayinput, &line, "START HOUR");
  hour = atoi(line.c_str());
  config->getinputline(&delayinput, &line, "START MINUTE");
  minute = atoi(line.c_str());
  config->getinputline(&delayinput, &line, "START SECOND");
  second = atoi(line.c_str());
  config->getinputline(&delayinput, &line, "INCREMENT (SECS)");
  delayincms = int(atof(line.c_str())*1000);
  config->getinputline(&delayinput, &line, "NUM TELESCOPES");
  numdelaytelescopes = atoi(line.c_str());

  config->getMJD(delaystartday, delaystartseconds, year, month, day, hour, minute, second);

  delayTelescopeNames = new string[numdelaytelescopes];
  linedelays = new double[numdelaytelescopes];
  for(int i=0;i<numdelaytelescopes;i++)
  {
    config->getinputline(&delayinput, &delayTelescopeNames[i], "TELESCOPE ", i);
    if(delayTelescopeNames[i].substr(0, delayTelescopeNames[i].find_first_of(' ')) == stationname.substr(0, stationname.find_first_of(' ')))
      columnoffset = i;
  }
  if(columnoffset < 0)
  {
    cfatal << startl << "Error!!! " << mpiid << " could not locate " << stationname << " in delay file " << delayfilename << " - ABORTING!!!" << endl;
    MPI_Abort(MPI_COMM_WORLD, 1);
  }

  config->getinputline(&delayinput, &line, "NUM SCANS");
  numscans = atoi(line.c_str());
  delays = new double*[numscans];

  totaldelays = 0;
  numfound = 0;
  scanstarts = new int[numscans];
  scanlengths = new int[numscans];
  for(int i=0;i<numscans;i++)
  {
    config->getinputline(&delayinput, &line, "SCAN ", i);
    scanlengths[i] = atoi(line.c_str());
    config->getinputline(&delayinput, &line, "SCAN ", i);
    scanstarts[i] = atoi(line.c_str());
    //throw away the line, its just source name
    config->getinputline(&delayinput, &line, "SCAN ", i);
    delays[i] = new double[scanlengths[i]+3];

    for(int j=0;j<scanlengths[i] + 3;j++)
    {
      at = 0;
      config->getinputline(&delayinput, &line, "RELATIVE INC ", j-1);
      for(int k=0;k<numdelaytelescopes;k++)
      {
        next = line.find_first_of("\t", at);
        linedelays[k] = atof((line.substr(at, next-at)).c_str());
        at = next+1;
      }
      delays[i][j] = linedelays[columnoffset];
      if(delays[i][j] <= MAX_NEGATIVE_DELAY) {
        cfatal << startl << "Error - cannot handle delays more negative than " << MAX_NEGATIVE_DELAY << "!!! Need to unimplement the datastream check for negative delays to indicate bad data.  Aborting now - contact the developer!" << endl;
	MPI_Abort(MPI_COMM_WORLD, 1);
      }
      if(j<scanlengths[i])
      {
        scannumbers.push_back(i);
        totaldelays++;
      }
    }
  }

  cinfo << startl << "Telescope " << stationname << " has finished processing delay file, numdelays = " << totaldelays << ", numtelescopes = " << numdelaytelescopes << endl;

  delete [] delayTelescopeNames;
}
