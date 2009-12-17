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
#include "config.h"
#include "fxmanager.h"
#include <iostream>
#include "datastream.h"
#include "core.h"
#include <sys/stat.h>
#include <stdio.h>     /* standard I/O functions                         */
#include <unistd.h>    /* standard unix functions, like getpid()         */
#include <sys/types.h> /* various type definitions, like pid_t           */
#include <signal.h>
#include <difxmessage.h>
#include "alert.h"

//includes for socket stuff - for monitoring
//#include <sys/socket.h>
//#include <netdb.h>
//#include <netinet/in.h>
//#include <arpa/inet.h>

bool terminatenow;

/* first, here is the signal handler */
void catch_pipe(int sig_num)
{
    /* re-set the signal handler again to catch_int, for next time */
    signal(SIGPIPE, catch_pipe);
    /* and print the message */
    cwarn << startl << "Caught a pipe signal - the monitor probably just dropped out..." << endl;
}

using namespace std;

const string FxManager::CIRCULAR_POL_NAMES[4] = {"RR", "LL", "RL", "LR"};
const string FxManager::LL_CIRCULAR_POL_NAMES[4] = {"LL", "RR", "LR", "RL"};
const string FxManager::LINEAR_POL_NAMES[4] = {"XX", "YY", "XY", "YX"};

FxManager::FxManager(Configuration * conf, int ncores, int * dids, int * cids, int id, MPI_Comm rcomm, bool mon, char * hname, int port, int monitor_skip)
  : config(conf), return_comm(rcomm), numcores(ncores), mpiid(id), visibilityconfigok(true), monitor(mon), hostname(hname), monitorport(port)
{
  bool startskip;
  int perr, minchans, confresultbytes, todiskbufferlen;
  double headerbloatfactor;
  const string * polnames;

  estimatedbytes = 0;
  cinfo << startl << "STARTING " << PACKAGE_NAME << " version " << VERSION << endl;

  difxMessageSendDifxStatus(DIFX_STATE_STARTING, "Version " VERSION, 0.0, 0, 0);

  /* set the PIPE signal handler to 'catch_pipe' */
  signal(SIGPIPE, catch_pipe);

  terminatenow = false;
  numdatastreams = config->getNumDataStreams();
  startmjd = config->getStartMJD();
  startseconds = config->getStartSeconds();
  initns = config->getStartNS();
  executetimeseconds = config->getExecuteSeconds();
  model = config->getModel();
  estimatedbytes += config->getEstimatedBytes();

  initscan = 0;
  while(model->getScanEndSec(initscan, startmjd, startseconds) < 0)
    initscan++;

  startskip = false;
  currentconfigindex = config->getScanConfigIndex(initscan);
  while(currentconfigindex < 0 && initscan < model->getNumScans()) {
    currentconfigindex = config->getScanConfigIndex(++initscan);
    startskip = true;
  }

  if(initscan == model->getNumScans())
  {
    cfatal << startl << "Did not find any scans to correlate in the specified time range - aborting!!!" << endl;
    MPI_Abort(MPI_COMM_WORLD, 1);
  }
  if(startskip && config->getStartNS() != 0) {
    cwarn << startl << "WARNING!!! Fractional start time of " << startseconds << " seconds plus " << initns << " ns was specified, but the start time corresponded to a configuration not specified in the input file and hence we are skipping to the first valid scan after the specified start (" << initscan << ")! The ns offset will be set to 0!!!" << endl;
    initns = 0;
  }
  inttime = config->getIntTime(currentconfigindex);
  nsincrement = config->getSubintNS(currentconfigindex);
  initsec = -(model->getScanStartSec(initscan, startmjd, startseconds));
  if(initsec < 0) {
    cwarn << startl << "Asked to start correlation before the beginning of the model! Will start from first possible time (" << (-initsec) << ") seconds later than requested" << endl;
    initsec = 0;
   }

  numbaselines = (numdatastreams*(numdatastreams-1))/2;
  resultlength = config->getMaxCoreResultLength();
  resultbuffer = vectorAlloc_cf32(resultlength);
  estimatedbytes += resultlength*8;

  todiskbufferlen = resultlength*8;
  for(int i=0;i<config->getNumConfigs();i++)
  {
    confresultbytes = config->getCoreResultLength(i)*8;
    minchans = 999999;
    for(int j=0;j<config->getFreqTableLength();j++)
    {
      if(config->isFrequencyUsed(i,j) && config->getFNumChannels(j) < minchans)
        minchans = config->getFNumChannels(j);
    }
    headerbloatfactor = 1.0 + ((double)(Visibility::HEADER_BYTES))/(minchans*8);
    if(confresultbytes*headerbloatfactor > todiskbufferlen)
      todiskbufferlen = int(1.02*confresultbytes*headerbloatfactor); //a little extra margin to be sure
  }

  todiskbuffer = (char*)vectorAlloc_u8(todiskbufferlen);
  estimatedbytes += todiskbufferlen;
  datastreamids = new int[numdatastreams];
  coreids = new int[numcores];
  corecounts = new int[numcores];
  recentcorecounts = new int[numcores];
  for(int i=0;i<numdatastreams;i++)
    datastreamids[i] = dids[i];
  for(int i=0;i<numcores;i++) {
    corecounts[i] = 0;
    recentcorecounts[i] = 0;
  }
  coretimes = new int**[Core::RECEIVE_RING_LENGTH];
  numsent = new int[numcores];
  extrareceived = new int[numcores];
  for(int i=0;i<numcores;i++)
  {
    numsent[i] = 0;
    extrareceived[i] = 0;
    coreids[i] = cids[i];
  }
  for(int i=0;i<Core::RECEIVE_RING_LENGTH;i++)
  {
    coretimes[i] = new int*[numcores];
    for(int j=0;j<numcores;j++)
      coretimes[i][j] = new int[3];
  }

  //create the visbuffer array
  //integrationsamples = int(DataStream::SAMPLES_PER_SECOND*dumptime + 0.5);
  visbuffer = new Visibility*[config->getVisBufferLength()];
  bufferlock = new pthread_mutex_t[config->getVisBufferLength()];
  islocked = new bool[config->getVisBufferLength()];
  if(config->circularPolarisations())
    polnames = ((config->getMaxProducts() == 1)&&(config->getDRecordedBandPol(0,0,0)=='L'))?LL_CIRCULAR_POL_NAMES:CIRCULAR_POL_NAMES;
  else
    polnames = LINEAR_POL_NAMES;
  for(int i=0;i<config->getVisBufferLength();i++)
  {
    visbuffer[i] = new Visibility(config, i, config->getVisBufferLength(), todiskbuffer, todiskbufferlen, executetimeseconds, initscan, initsec, initns, polnames, monitor, monitorport, hostname, &mon_socket, monitor_skip);
    pthread_mutex_init(&(bufferlock[i]), NULL);
    islocked[i] = false;
    if(!visbuffer[i]->configuredOK()) { //problem with finding a polyco, probably
      cfatal << startl << "Manager aborting correlation!" << endl;
      MPI_Abort(MPI_COMM_WORLD, 1);
    }
    estimatedbytes += visbuffer[i]->getEstimatedBytes();
  }

  //create the threaded writing stuff
  keepwriting = true;
  writethreadinitialised = false;
  pthread_cond_init(&writecond, NULL);
  
  newestlockedvis = 1;
  oldestlockedvis = 0;
  islocked[0] = true;
  islocked[1] = true;
  perr = pthread_mutex_lock(&(bufferlock[0]));
  if(perr != 0)
    csevere << startl << "FxManager: Error locking first visibility!!" << endl;
  perr = pthread_mutex_lock(&(bufferlock[1]));
  if(perr != 0)
    csevere << startl << "FxManager: Error locking second visibility!!" << endl;
  perr = pthread_create(&writethread, NULL, FxManager::launchNewWriteThread, (void *)(this));
  if(perr != 0)
    csevere << startl << "FxManager: Error in launching writethread!!" << endl;
  while(!writethreadinitialised)
  {
    perr = pthread_cond_wait(&writecond, &(bufferlock[1]));
    if (perr != 0)
      csevere << startl << "Error waiting on writethreadstarted condition!!!!" << endl;
  }

  lastsource = numdatastreams;

  //cinfo << startl << "Estimated memory usage by FXManager: " << float(model->getEstimatedBytes() + config->getVisBufferLength()*visbuffer[0]->getEstimatedBytes() + resultlength*8)/1048576.0 << " MB" << endl;
}


FxManager::~FxManager()
{
  for(int i=0;i<Core::RECEIVE_RING_LENGTH;i++)
  {
    for(int j=0;j<numcores;j++)
      delete [] coretimes[i][j];
    delete [] coretimes[i];
  }
  delete [] coretimes;
  delete [] numsent;
  delete [] datastreamids;
  delete [] coreids;
  delete [] extrareceived;
  vectorFree(todiskbuffer);
  vectorFree(resultbuffer);
  for(int i=0;i<config->getVisBufferLength();i++)
    delete visbuffer[i];
  delete [] visbuffer;
  //delete [] writequeue;
  //delete [] fileopened;
  delete [] islocked;
  delete [] bufferlock;

  if(terminatenow)
  {
    difxMessageSendDifxStatus(DIFX_STATE_TERMINATED, "", 0.0, 0, 0);
  }
  else
  {
    difxMessageSendDifxStatus(DIFX_STATE_DONE, "", 0.0, 0, 0);
  }
}

void interrupthandler(int sig)
{
  cwarn << startl << "FXMANAGER caught a signal and is going to shut down the correlator" << endl;
  terminatenow = true;
}

void FxManager::terminate()
{
  if(terminatenow)
  {
    difxMessageSendDifxStatus(DIFX_STATE_TERMINATING, "", 0.0, 0, 0);
  }
  else
  {
    difxMessageSendDifxStatus(DIFX_STATE_ENDING, "", 0.0, 0, 0);
  }
  cinfo << startl << "FXMANAGER: Sending terminate signals" << endl;
  for(int i=0;i<numcores;i++)
    MPI_Send(senddata, 1, MPI_INT, coreids[i], CR_TERMINATE, return_comm);
  for(int i=0;i<numdatastreams;i++)
    MPI_Send(senddata, 4, MPI_INT, datastreamids[i], DS_TERMINATE, MPI_COMM_WORLD);
}
/*!
    \fn FxManager::execute()
 */
void FxManager::execute()
{
  int perr;
  long long sendcount = 0;

  cinfo << startl << "Hello World, I am the FxManager" << endl;

  //loop over all scans in the Model
  for(int i=initscan;i<model->getNumScans();i++)
  {
    currentconfigindex = config->getScanConfigIndex(i);
    if(currentconfigindex < 0)
      continue; //can skip this scan - not interested
    inttime = config->getIntTime(config->getScanConfigIndex(i));
    nsincrement = config->getSubintNS(config->getScanConfigIndex(i));
    if(model->getScanStartSec(i, startmjd, startseconds) >= executetimeseconds)
      break; //can stop here

    senddata[3] = initns; //will be zero for all scans except (maybe) the first
    senddata[2] = initsec; //ditto to initns
    senddata[1] = i;

    //do as many sends as we need to for this scan
    while(senddata[2] < model->getScanDuration(i) && (senddata[2]+model->getScanStartSec(i, startmjd, startseconds) < executetimeseconds) && !terminatenow) {
      if(senddata[2] == model->getScanDuration(i)-1 || 
        (senddata[2]+model->getScanStartSec(i, startmjd, startseconds)) == executetimeseconds-1)
      {
        if((1000000000-senddata[3]) <= nsincrement/2)
          break;
      }
      if(sendcount < Core::RECEIVE_RING_LENGTH*numcores) {//still in the "filling up" phase
        senddata[0] = coreids[((int)sendcount)%numcores];
        sendData(senddata, ((int)sendcount)%numcores);
      }
      else { //normal receive/resend
        receiveData(true);
      }
      sendcount++;
      if(sendcount == Core::RECEIVE_RING_LENGTH*numcores) //just finished "filling up"
        signal(SIGINT, &interrupthandler);
      if(!visibilityconfigok) { //problem with finding a polyco, probably
        cfatal << startl << "Manager aborting correlation due to visibility configuration problem!" << endl;
        MPI_Abort(MPI_COMM_WORLD, 1);
      }
    }

    //make sure the offset from start of scan is zero for all scans but the first
    initns = 0;
    initsec = 0;
  }

  //must be done - send the terminate signal to each datastream and each core
  terminate();
  
  //receive the final data from each core
  for(int i=0;i<Core::RECEIVE_RING_LENGTH;i++)
  {
    for(int j=0;j<numcores;j++) {
      if(sendcount==0)
        break;
      receiveData(false);
      sendcount--;
    }
  }
  
  //ensure the thread writes out all waiting visibilities
  keepwriting = false;
  for(int i=0;i<=(newestlockedvis+config->getVisBufferLength()-oldestlockedvis)%config->getVisBufferLength();i++)
  {
    perr = pthread_mutex_unlock(&(bufferlock[(oldestlockedvis+i)%config->getVisBufferLength()]));
    if(perr!=0)
      csevere << startl << "FxManager error trying to unlock bufferlock[" << (oldestlockedvis+i)%config->getVisBufferLength() << "] for the last time" << endl; 
  }

  //join the writing thread
  perr = pthread_join(writethread, NULL);
  if(perr != 0)
    csevere << startl << "Error in closing writethread!!!" << endl;

  cinfo << startl << "FxManager is finished" << endl;
}

void FxManager::sendData(int data[], int coreindex)
{
  //send the command to the Core
  MPI_Send(&data[1], 3, MPI_INT, coreids[coreindex], CR_RECEIVETIME, return_comm);

  for(int j=0;j<numdatastreams;j++)
  {
    //send the commands to the Datastreams
    MPI_Ssend(data, 4, MPI_INT, datastreamids[j], DS_PROCESS, MPI_COMM_WORLD);
  }
  coretimes[numsent[coreindex]%Core::RECEIVE_RING_LENGTH][coreindex][0] = data[1];
  coretimes[numsent[coreindex]%Core::RECEIVE_RING_LENGTH][coreindex][1] = data[2];
  coretimes[numsent[coreindex]%Core::RECEIVE_RING_LENGTH][coreindex][2] = data[3];
  numsent[coreindex]++;
  data[3] += nsincrement;
  if(data[3] >= 1000000000)
  {
    data[3] -= 1000000000;
    data[2]++;
  }
  //cinfo << startl << "FXMANAGER has finished sending data" << endl;
}

void FxManager::receiveData(bool resend)
{
  MPI_Status mpistatus;
  int sourcecore, sourceid=0, visindex, perr, infoindex;
  bool viscomplete;
  double scantime;
  int i, flag, subintscan;

  // Work around MPI_Recv's desire to prioritize receives by MPI rank
  for(i = 0; i < numcores; i++)
  {
      lastsource++;
      if(lastsource > numcores + numdatastreams)
      {
      	lastsource = numdatastreams+1;
      }
      MPI_Iprobe(lastsource, MPI_ANY_TAG, return_comm, &flag, &mpistatus);
      if(flag) break;
  }
  if(i == numcores)
  {
  	// No core has sent data yet -- wait for first message to come
  	MPI_Recv(resultbuffer, resultlength*2, MPI_FLOAT, MPI_ANY_SOURCE, MPI_ANY_TAG, return_comm, &mpistatus);
  }
  else
  {
  	// Receive message from the core that is both ready and has been waiting the longest
  	MPI_Recv(resultbuffer, resultlength*2, MPI_FLOAT, lastsource, MPI_ANY_TAG, return_comm, &mpistatus);
  }


  sourcecore = mpistatus.MPI_SOURCE;
  MPI_Get_count(&mpistatus, MPI_FLOAT, &perr);

  for(int i=0;i<numcores;i++)
  {
    if(coreids[i] == sourcecore)
      sourceid = i;
  }

  corecounts[sourceid]++;
  recentcorecounts[sourceid]++;
  infoindex = (numsent[sourceid]+extrareceived[sourceid])%Core::RECEIVE_RING_LENGTH;
  if(numsent[sourceid] < Core::RECEIVE_RING_LENGTH)
    infoindex = extrareceived[sourceid];
  subintscan = coretimes[infoindex][sourceid][0];
  scantime = coretimes[infoindex][sourceid][1] + double(coretimes[infoindex][sourceid][2])/1000000000.0;

  //put the data in the appropriate slot
  if(mpistatus.MPI_TAG == CR_VALIDVIS) // the data is valid
  {
    //find where it belongs
    visindex = locateVisIndex(sourceid);

    //immediately get some more data heading to that node
    if(resend)
    {
      senddata[0] = sourcecore;
      sendData(senddata, sourceid);
    }
    else
    {
      //still need to acknowledge that we have received from this core
      extrareceived[sourceid]++;
    }
    if (visindex < 0)
      cerror << startl << "Error - stale data was received from core " << sourceid << " regarding scan " << subintscan << ", time " << scantime << " seconds - it will be ignored!!!" << endl;
    else
    {
      //now store the data - if we have sufficient sub-accumulations received, release this 
      //Visibility so the writing thread can write it out
      viscomplete = visbuffer[visindex]->addData(resultbuffer);
      if(viscomplete)
      {
        cinfo << startl << "Vis. " << visindex << " to write out time " << visbuffer[visindex]->getTime() << endl;
        //better make sure we have at least locked the next section
        if(visindex == newestlockedvis)
        {
          newestlockedvis = (newestlockedvis + 1)%config->getVisBufferLength();
          perr = pthread_mutex_lock(&(bufferlock[newestlockedvis]));
          if(perr != 0)
            csevere << startl << "FxManager error trying to lock bufferlock[" << newestlockedvis << "]!!!" << endl;
          islocked[newestlockedvis] = true;
        }
        perr = pthread_mutex_unlock(&(bufferlock[visindex]));
        if(perr != 0)
          csevere << startl << "FxManager error trying to unlock bufferlock[" << visindex << "]!!!" << endl;
        islocked[visindex] = false;
        if(oldestlockedvis == visindex)
        {
          while(!islocked[oldestlockedvis])
            oldestlockedvis = (oldestlockedvis + 1)%config->getVisBufferLength();
        }
        printSummary(visindex);
      }
    }
  }
  else
  {
    cinfo << startl << "Invalid data was recieved from core " << sourcecore << " regarding scan " << subintscan << ", offset " << scantime << " seconds" << endl;

    //immediately get some more data heading to that node
    if(resend)
    {
      senddata[0] = sourcecore;
      sendData(senddata, sourceid);
    }
  }
}

void FxManager::printSummary(int visindex)
{
  int minsubints, maxsubints, minsubintindex, maxsubintindex, numvis;
  double meansubints, visbufferduration;

  numvis = (newestlockedvis+config->getVisBufferLength()-oldestlockedvis)%config->getVisBufferLength();
  cinfo << startl << numvis << "/" << config->getVisBufferLength() << " visibilities locked for accumulation, most recent index is " << newestlockedvis << endl;
  numvis = (oldestlockedvis+config->getVisBufferLength()-writesegment)%config->getVisBufferLength();
  cinfo << startl << numvis << "/" << config->getVisBufferLength() << " visibilities ready to write out from " << writesegment << endl;

  visbufferduration = inttime*config->getVisBufferLength();
  minsubints = MAX_S32;
  maxsubints = 0;
  meansubints = 0.0;
  minsubintindex = 0;
  maxsubintindex = 0;
  for(int c=0;c<numcores;c++)
  {
    if(corecounts[c] < minsubints)
    {
      minsubints = corecounts[c];
      minsubintindex = c;
    }
    if(corecounts[c] > maxsubints)
    {
      maxsubints = corecounts[c];
      maxsubintindex = c;
    }
    meansubints += ((double)corecounts[c])/numcores;
  }
  cinfo << startl << "Min/Mean/Max number of subints processed is " << minsubints << "/" << meansubints << "/" << maxsubints << ", mincoreindex is " << minsubintindex << ", maxcoreindex is " << maxsubintindex << endl;
  if(visindex == config->getVisBufferLength()-1)
  {
    minsubints = MAX_S32;
    maxsubints = 0;
    meansubints = 0.0;
    minsubintindex = 0;
    maxsubintindex = 0;
    for(int c=0;c<numcores;c++)
    {
      if(recentcorecounts[c] < minsubints)
      {
        minsubints = recentcorecounts[c];
        minsubintindex = c;
      }
      if(recentcorecounts[c] > maxsubints)
      {
        maxsubints = recentcorecounts[c];
        maxsubintindex = c;
      }
      meansubints += ((double)recentcorecounts[c])/numcores;
      recentcorecounts[c] = 0;
    }
    cinfo << startl << "Min/Mean/Max number of subints processed in last " << visbufferduration << " seconds is " << minsubints << "/" << meansubints << "/" << maxsubints << ", mincoreindex is " << minsubintindex << ", maxcoreindex is " << maxsubintindex << endl;
  }
}

void * FxManager::launchNewWriteThread(void * thismanager)
{
  FxManager * me = (FxManager *)thismanager;

  me->initialiseOutput();
  me->loopwrite();

  return 0;
}

void FxManager::initialiseOutput()
{
  if(config->getOutputFormat() == Configuration::DIFX)
  {
    //create the directory - if that doesn't work, abort as we can't guarantee no overwriting data
    int flag = mkdir(config->getOutputFilename().c_str(), 0775);
    if(flag < 0) {
      cfatal << startl << "Error trying to create directory " << config->getOutputFilename() << ": " << flag << ", ABORTING!" << endl;
      MPI_Abort(MPI_COMM_WORLD, 1);
    }
  }
}

void FxManager::loopwrite()
{
  int perr;
  int lastconfigindex = currentconfigindex;

  writesegment = 0;
  perr = pthread_mutex_lock(&(bufferlock[config->getVisBufferLength()-1]));
  if(perr != 0)
    csevere << startl << "Error in initial fxmanager writethread lock of the end section!!!" << endl;
  writethreadinitialised = true;
  perr = pthread_cond_signal(&writecond);
  if(perr != 0)
    csevere << startl << "FXMANAGER: Writethread error trying to signal main thread to wake up!!!" << endl;

  while(keepwriting)
  {
    //get the lock on the queue
    perr = pthread_mutex_lock(&(bufferlock[writesegment]));
    if(perr != 0)
      csevere << startl << "Writethread error trying to lock bufferlock[" << writesegment << "]!!!" << endl;
    //unlock the previous section
    perr = pthread_mutex_unlock(&(bufferlock[(writesegment+config->getVisBufferLength()-1)%config->getVisBufferLength()]));
    if(perr != 0)
      csevere << startl << "Writethread error trying to unlock bufferlock[" << (writesegment+config->getVisBufferLength()-1)%config->getVisBufferLength() << "]!!!" << endl;
    if(visbuffer[writesegment]->getCurrentConfig() != lastconfigindex)
    {
      lastconfigindex = visbuffer[writesegment]->getCurrentConfig();
    }
    visbuffer[writesegment]->writedata();
    visbuffer[writesegment]->multicastweights();
    visbuffer[writesegment]->increment();
    if(!visbuffer[writesegment]->configuredOK()) { //problem with finding a polyco, probably
      visibilityconfigok = false;
    }
    writesegment=(writesegment+1)%config->getVisBufferLength();
  }

  //now we're done, so run thru everyone just to be sure
  perr = pthread_mutex_unlock(&(bufferlock[(writesegment+config->getVisBufferLength()-1)%config->getVisBufferLength()]));
  if(perr != 0)
    csevere << startl << "Writethread error trying to unlock bufferlock[" << (writesegment+config->getVisBufferLength()-1)%config->getVisBufferLength() << "]!!!" << endl;
  for(int i=0;i<config->getVisBufferLength();i++)
  {
    visbuffer[(writesegment+i)%config->getVisBufferLength()]->writedata();
    visbuffer[(writesegment+i)%config->getVisBufferLength()]->multicastweights();
  }
}

int FxManager::locateVisIndex(int coreid)
{
  bool tooold = true;
  int perr, count, infoindex, vblength;
  int corescan, coresec, corens;
  s64 difference;
  Visibility * vis;

  vblength = config->getVisBufferLength();
  infoindex = (numsent[coreid]+extrareceived[coreid]) % Core::RECEIVE_RING_LENGTH;
  if(numsent[coreid] < Core::RECEIVE_RING_LENGTH)
    infoindex = extrareceived[coreid];

  corescan = coretimes[infoindex][coreid][0];
  coresec = coretimes[infoindex][coreid][1];
  corens = coretimes[infoindex][coreid][2] + config->getSubintNS(config->getScanConfigIndex(corescan))/2;

  if((newestlockedvis-oldestlockedvis+vblength)%vblength >= vblength/2) 
  { 
    cerror << startl << "Error - data was received which is too recent (scan " << corescan << ", " << coresec << " sec + " << corens << "ns)!  Will force write-out of oldest Visibility" << endl; 
    //abandon the oldest vis, even though it hasn't been filled yet 
    perr = pthread_mutex_unlock(&(bufferlock[oldestlockedvis])); 
    if(perr != 0) 
      csevere << startl << "Error in fxmanager unlocking visibility " << oldestlockedvis << endl; 
    islocked[oldestlockedvis] = false; 
    while(!islocked[oldestlockedvis]) 
    { 
      oldestlockedvis = (oldestlockedvis+1)%vblength; 
    } 
  } 

  for(int i=0;i<=(newestlockedvis-oldestlockedvis+vblength)%vblength;i++)
  {
    vis = visbuffer[(oldestlockedvis+i)%vblength];
    if(corescan > vis->getCurrentScan())
      difference = (s64)1e15; //its in the future cf the start of this vis, but doesn't belong here
    else if (corescan < vis->getCurrentScan())
      difference = (s64)-1e15; //its in a previous scan, so definitely doesn't belong here
    else //does belong to this scan - safe to call timeDifference
      difference = vis->timeDifference(coresec, corens);
    if(difference >= 0)
    {
      tooold = false;
      if(difference < (s64)(inttime*1000000000.0)) //we have found the correct Visibility
      {
        return (oldestlockedvis+i)%vblength;
      }
    }
  }
  if(tooold)
    return -1;
  else
  {
    //try locking some more visibilities til we get to what we need
    while((newestlockedvis-oldestlockedvis+vblength)%vblength < vblength/2)
    {
      newestlockedvis = (newestlockedvis+1)%vblength;
      //lock another visibility
      perr = pthread_mutex_lock(&(bufferlock[newestlockedvis]));
      if(perr != 0)
        csevere << startl << "Error in fxmanager locking visibility " << newestlockedvis << endl;
      islocked[newestlockedvis] = true;
      //check if its good
      vis = visbuffer[newestlockedvis];
      if(corescan > vis->getCurrentScan())
        difference = (s64)1e15; //its in the future cf the start of this vis, but doesn't belong here
      else if (corescan < vis->getCurrentScan())
        difference = (s64)-1e15; //its in a previous scan, so definitely doesn't belong here
      else //does belong to this scan - safe to call timeDifference
        difference = vis->timeDifference(coresec, corens);
      if(difference < (s64)(inttime*1000000000.0))
        return newestlockedvis;
    }
    //d'oh - its newer than we can handle - have to drop old data until we catch up
    cerror << startl << "Error - data was received which is too recent (scan " << corescan << ", " << coresec << " sec + " << corens << "ns)!  Will force existing data to be dropped until we have caught up coreid="<< coreid << endl;

    while(difference > inttime)
    {
      count = 0;
      //abandon the oldest vis, even though it hasn't been filled yet
      perr = pthread_mutex_unlock(&(bufferlock[oldestlockedvis]));
      if(perr != 0)
        csevere << startl << "Error in fxmanager locking visibility " << newestlockedvis << endl;
      islocked[oldestlockedvis] = false;
      while(!islocked[oldestlockedvis])
      {
        oldestlockedvis = (oldestlockedvis+1)%vblength;
        count++;
      }
      for(int j=0;j<count;j++)
      {
        newestlockedvis = (newestlockedvis+1)%vblength;
        perr = pthread_mutex_lock(&(bufferlock[newestlockedvis]));
        if(perr != 0)
          csevere << startl << "Error in fxmanager locking visibility " << newestlockedvis << endl;
        islocked[newestlockedvis] = true;
        vis = visbuffer[newestlockedvis];
        if(corescan > vis->getCurrentScan())
          difference = (s64)1e15; //its in the future cf the start of this vis, but doesn't belong here
        else if (corescan < vis->getCurrentScan())
          difference = (s64)-1e15; //its in a previous scan, so definitely doesn't belong here
        else //does belong to this scan - safe to call timeDifference
          difference = vis->timeDifference(coresec, corens);
        if(difference < (s64)(inttime*1000000000.0)) //we've finally caught up
          break;
      }
    }
    return newestlockedvis;
  }

  return -1; //unreachable
}

