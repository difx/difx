/***************************************************************************
 *   Copyright (C) 2006-2016 by Adam Deller                                *
 *                                                                         *
 *   This program is free software: you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation, either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>. *
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
#include <limits.h>
#include <sys/stat.h>
#include <stdio.h>     /* standard I/O functions                         */
#include <unistd.h>    /* standard unix functions, like getpid()         */
#include <sys/types.h> /* various type definitions, like pid_t           */
#include <signal.h>
#include <difxmessage.h>
#include "alert.h"
#include <dirent.h>
#include <errno.h>
#include <sys/socket.h>
#include <poll.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <netdb.h>
#include <string.h>

/* Cf. C99 <features.h> */
#ifndef LLONG_MAX
# define LLONG_MAX LONG_LONG_MAX
#endif /* LLONG_MAX */
#ifndef LLONG_MIN
# define LLONG_MIN LONG_LONG_MIN
#endif /* LLONG_MIN */

bool terminatenow;

/* first, here is the signal handler */
void catch_pipe(int sig_num)
{
    /* re-set the signal handler again to catch_int, for next time */
    signal(SIGPIPE, catch_pipe);
}

using namespace std;

const string FxManager::CIRCULAR_POL_NAMES[4] = {"RR", "LL", "RL", "LR"};
const string FxManager::LL_CIRCULAR_POL_NAMES[4] = {"LL", "RR", "LR", "RL"};
const string FxManager::LINEAR_POL_NAMES[4] = {"XX", "YY", "XY", "YX"};

FxManager::FxManager(Configuration * conf, int ncores, int * dids, int * cids, int id, MPI_Comm rcomm, bool mon, char * hname, int port, int monitor_skip)
  : config(conf), return_comm(rcomm), numcores(ncores), mpiid(id), visibilityconfigok(true), monitor(mon), hostname(hname), monitor_skip(monitor_skip), monitorport(port)
{
  bool startskip;
  int perr, minchans, confresultbytes, todiskbufferlen;
  double headerbloatfactor;
  const string * polnames;
  pthread_attr_t attr;

  cinfo << startl << "STARTING " << PACKAGE_NAME << " version " << VERSION << endl;

  difxMessageSendDifxStatus(DIFX_STATE_STARTING, "Version " VERSION, 0.0, 0, 0);

  /* set the PIPE signal handler to 'catch_pipe' */
  signal(SIGPIPE, catch_pipe);

  terminatenow = false;
  numdatastreams = config->getNumDataStreams();
  startmjd = config->getStartMJD();
  startseconds = config->getStartSeconds();
  initns = config->getStartNS();
  model = config->getModel();
  estimatedbytes = config->getEstimatedBytes();

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
    cwarn << startl << "Fractional start time of " << startseconds << " seconds plus " << initns << " ns was specified, but the start time corresponded to a configuration not specified in the input file and hence we are skipping to the first valid scan after the specified start (" << initscan << ")! The ns offset will be set to 0!!!" << endl;
    initns = 0;
  }
  inttime = config->getIntTime(currentconfigindex);
  nsincrement = config->getSubintNS(currentconfigindex);
  initsec = -(model->getScanStartSec(initscan, startmjd, startseconds));
  if(initsec < 0) {
    cwarn << startl << "Asked to start correlation before the beginning of the model! Will start from first possible time (" << (-initsec) << ") seconds later than requested" << endl;
    initsec = 0;
   }

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
      if(config->isFrequencyOutput(i,j) && config->getFNumChannels(j)/config->getFChannelsToAverage(j) < minchans)
        minchans = config->getFNumChannels(j)/config->getFChannelsToAverage(j);
    }
    headerbloatfactor = 1.0 + ((double)(Visibility::HEADER_BYTES))/(minchans*8);
    if(confresultbytes*headerbloatfactor > todiskbufferlen)
      todiskbufferlen = int(1.02*confresultbytes*headerbloatfactor); //a little extra margin to be sure
  }

  todiskbuffer = (char*)vectorAlloc_u8(todiskbufferlen);
  if(!todiskbuffer)
  {
    cfatal << "Failed to allocate " << todiskbufferlen << " bytes in fxmanager for 'todiskbuffer' output writer buffer" << endl;
    MPI_Abort(MPI_COMM_WORLD, 1);
  }
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
    visbuffer[i] = new Visibility(config, i, config->getVisBufferLength(), todiskbuffer, todiskbufferlen, config->getExecuteSeconds(), initscan, initsec, initns, polnames);
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

  //ensure these new threads are joinable
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

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
  perr = pthread_create(&writethread, &attr, FxManager::launchNewWriteThread, (void *)(this));
  if(perr != 0)
    csevere << startl << "FxManager: Error in launching writethread!!" << endl;
  while(!writethreadinitialised)
  {
    perr = pthread_cond_wait(&writecond, &(bufferlock[1]));
    if (perr != 0)
      csevere << startl << "Error waiting on writethreadstarted condition!!!!" << endl;
  }

  lastsource = numdatastreams;

  // Launch a thread to send monitoring data
  if (monitor) {
    pthread_cond_init(&monitorcond, NULL);
    pthread_mutex_init(&moncondlock, NULL);
    pthread_mutex_init(&monitorwritelock, NULL);
    monsockStatus = CLOSED;
    buf = NULL;
    bufsize = 0;
    nbuf = 0;

    perr = pthread_create(&monthread, &attr, FxManager::launchMonitorThread, (void *)(this));
    if(perr != 0)
      csevere << startl << "FxManager: Error in launching monitorthread!!" << endl;
  }

  pthread_attr_destroy(&attr);

  //cinfo << startl << "Estimated memory usage by FXManager: " << float(uvw->getNumUVWPoints()*24 + config->getVisBufferLength()*config->getMaxResultLength()*8)/1048576.0 << " MB" << endl;

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

  // This line helps with jobs not reporting fully finishing -WFB 20101223
  usleep(10000);

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
    if(model->getScanStartSec(i, startmjd, startseconds) >= config->getExecuteSeconds())
      break; //can stop here

    senddata[3] = initns; //will be zero for all scans except (maybe) the first
    senddata[2] = initsec; //ditto to initns
    senddata[1] = i;

    //do as many sends as we need to for this scan
    while(senddata[2] < model->getScanDuration(i) && (senddata[2]+model->getScanStartSec(i, startmjd, startseconds) < config->getExecuteSeconds()) && !terminatenow) {
      if(senddata[2] == model->getScanDuration(i)-1 || 
        (senddata[2]+model->getScanStartSec(i, startmjd, startseconds)) == config->getExecuteSeconds()-1)
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

  if (monitor) {
    // Signal Monitor Thread to quit
    pthread_mutex_lock(&moncondlock);
    pthread_cond_signal(&writecond);
    pthread_mutex_unlock(&moncondlock);
  }

  perr = pthread_join(writethread, NULL);
  if(perr != 0)
    csevere << startl << "Error in closing writethread!!!" << endl;

  if (monitor) {
    perr = pthread_join(monthread, NULL);
    if(perr != 0)
      csevere << startl << "Error in closing monitorthread!!!" << endl;
  }

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
  data[3] += (nsincrement%1000000000);
  data[2] += (nsincrement/1000000000);
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
  scantime = coretimes[infoindex][sourceid][1] + coretimes[infoindex][sourceid][2]/1000000000.0;

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
      cwarn << startl << "Stale data was received from core " << sourceid << " regarding scan " << subintscan << ", time " << scantime << " seconds - it will be ignored!!!" << endl;
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
    cwarn << startl << "Invalid data was received from core " << sourcecore << " regarding scan " << subintscan << ", offset " << scantime << " seconds" << endl;

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
  cverbose << startl << numvis << "/" << config->getVisBufferLength() << " visibilities ready to write out from " << writesegment << endl;

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
  cverbose << startl << "Min/Mean/Max number of subints processed is " << minsubints << "/" << meansubints << "/" << maxsubints << ", mincoreindex is " << minsubintindex << ", maxcoreindex is " << maxsubintindex << endl;
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
  ofstream output;
  char filename[256];
  int maxbinfiles = 1;
  int maxphasecentres = 1;
  for(int i=0;i<config->getNumConfigs();i++) {
    if(config->pulsarBinOn(i) && !config->scrunchOutputOn(i))
      maxbinfiles = config->getNumPulsarBins(i);
    if(config->getMaxPhaseCentres(i) > maxphasecentres)
      maxphasecentres = config->getMaxPhaseCentres(i);
  }

  if(config->getOutputFormat() == Configuration::DIFX)
  {
    //create the directory if it does not exist
    if(opendir(config->getOutputFilename().c_str()) == NULL)
    {
      int flag = mkdir(config->getOutputFilename().c_str(), 0775);
      if(flag < 0) {
        cfatal << startl << "Error trying to create directory " << config->getOutputFilename() << ": " << flag << " which apparently didn't exist. ABORTING!" << endl;
        MPI_Abort(MPI_COMM_WORLD, 1);
      }
    }
    else if (!config->isRestart())
    {
      cwarn << startl << "Not a restart job, but directory " << config->getOutputFilename() << " already exists - this is unusual" << endl;
    }

    //check if the specific filename we will want to create actually exists - complain if it does, create it if it doesn't
    for(int s=0;s<maxphasecentres;s++)
    {
      for(int b=0;b<maxbinfiles;b++)
      {
        sprintf(filename, "%s/DIFX_%05d_%06d.s%04d.b%04d", config->getOutputFilename().c_str(), config->getStartMJD(), config->getStartSeconds(), s, b);
        ifstream testfile(filename);
        if (testfile) {
          cfatal << startl << "Output DIFX file " << filename << " already exists.  ABORTING!" << endl;
          MPI_Abort(MPI_COMM_WORLD, 1);
        }
        else {
          output.open(filename, ios::trunc);
          output.close();
        }
      }
    }

    //write comments at top of pcal files
    visbuffer[0]->initialisePcalFiles();
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
    if (monitor) sendMonitorData(writesegment);
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
    if (monitor) sendMonitorData(writesegment);
  }
}

int FxManager::locateVisIndex(int coreid)
{
  bool tooold = true;
  int perr, count, infoindex, vblength;
  int corescan, coresec, corens;
  s64 difference; // difference in nanosec
  Visibility * vis;

  vblength = config->getVisBufferLength();
  infoindex = (numsent[coreid]+extrareceived[coreid]) % Core::RECEIVE_RING_LENGTH;
  if(numsent[coreid] < Core::RECEIVE_RING_LENGTH)
    infoindex = extrareceived[coreid];

  corescan = coretimes[infoindex][coreid][0];
  coresec = coretimes[infoindex][coreid][1];
  corens = coretimes[infoindex][coreid][2] + config->getSubintNS(config->getScanConfigIndex(corescan))/2;

  if((newestlockedvis-oldestlockedvis+vblength)%vblength >= vblength-3) 
  { 
    cwarn << startl << "Data was received which is too recent (scan " << corescan << ", " << coresec << " sec + " << corens << "ns).  Will force write-out of oldest Visibility" << endl; 
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
      difference = LLONG_MAX; //its in the future cf the start of this vis, but doesn't belong here
    else if (corescan < vis->getCurrentScan())
      difference = LLONG_MIN; //its in a previous scan, so definitely doesn't belong here
    else //does belong to this scan - safe to call timeDifference
      difference = vis->timeDifference(coresec, corens);
    if(difference >= 0)
    {
      tooold = false;
      if(difference < static_cast<s64>(inttime*1000000000.0)) //we have found the correct Visibility
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
    while((newestlockedvis-oldestlockedvis+vblength)%vblength < vblength-3)
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
        difference = LLONG_MAX; //its in the future cf the start of this vis, but doesn't belong here
      else if (corescan < vis->getCurrentScan())
        difference = LLONG_MIN; //its in a previous scan, so definitely doesn't belong here
      else //does belong to this scan - safe to call timeDifference
        difference = vis->timeDifference(coresec, corens);
      if(difference < static_cast<s64>(inttime*1000000000.0))
        return newestlockedvis;
    }
    //d'oh - its newer than we can handle - have to drop old data until we catch up
    cwarn << startl << "Data was received which is too recent (scan " << corescan << ", " << coresec << " sec + " << corens << "ns).  Will force existing data to be dropped until we have caught up coreid="<< coreid << endl;

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
          difference = LLONG_MAX; //its in the future cf the start of this vis, but doesn't belong here
        else if (corescan < vis->getCurrentScan())
          difference = LLONG_MIN; //its in a previous scan, so definitely doesn't belong here
        else //does belong to this scan - safe to call timeDifference
          difference = vis->timeDifference(coresec, corens);
        if(difference < static_cast<s64>(inttime*1000000000.0)) //we've finally caught up
          break;
      }
    }
    return newestlockedvis;
  }

  return -1; //unreachable
}

void * FxManager::launchMonitorThread(void * thismanager)
{
  FxManager * me = (FxManager *)thismanager;

  me->MonitorThread();

  return 0;
}

void FxManager::MonitorThread()
{
  int perr;
  ssize_t nwrote;

  openMonitorSocket();

  while(keepwriting) {

    pthread_mutex_lock(&moncondlock);
    perr = pthread_cond_wait(&writecond, &moncondlock);
    if (perr != 0)
      csevere << startl << "Error waiting on valid monitor data!!!!" << endl;
      // TODO QUIT HERE ON PERR?
    pthread_mutex_unlock(&moncondlock);
    if (!keepwriting) break;

    // Lock mutex until we have finished sending monitor data
    perr = pthread_mutex_lock(&monitorwritelock);

    if (nbuf==0) { // Spurious wakeup
      pthread_mutex_unlock(&monitorwritelock);
      continue;
    }

    if (checkSocketStatus()) {
      if (nbuf==-1) { // Indicate nothing to send
	int32_t atsec = -1;
	nbuf = sizeof(int32_t);
	nwrote = send(mon_socket, &atsec, nbuf, 0);
      } else {
	nwrote = send(mon_socket, buf, nbuf, 0);
      }
      if (nwrote==-1)
      {
	if (errno==EPIPE) {
	  cerror << startl << "Monitor connection seems to have dropped out!  Will try to reconnect shortly...!" << endl;
	}
        else
	{
	  cerror << startl << "Monitor socket returns \"" << strerror(errno) << "\"" << endl;
	}
	close(mon_socket);
	monsockStatus = CLOSED;

      }
      else if (nwrote != nbuf)
      {
	cerror << startl << "Error writing to network - will try to reconnect next Visibility 0 integration!" << endl;
	close(mon_socket);
	monsockStatus = CLOSED;
      }
    }
    nbuf = 0;
    perr = pthread_mutex_unlock(&monitorwritelock);
  }

  if (monsockStatus!=CLOSED) {
    close(mon_socket);
  }
  return;
}


void FxManager::sendMonitorData(int visID) {
  int perr;

  if (visID % monitor_skip !=0) return;  // Only send every monitor_skip visibilities

  perr = pthread_mutex_trylock(&monitorwritelock);
  if (perr==EBUSY) {
    cdebug << startl << "Monitor still sending, skipping this visibility" << endl;
  } else if (perr) {
    csevere << startl << "Error acquiring mutex lock for monitoring" << endl;
  } else { // Clear to go
    
    visbuffer[visID]->copyVisData(&buf, &bufsize, &nbuf);
    pthread_mutex_unlock(&monitorwritelock);

    // Tell monitor write thread to go
    pthread_mutex_lock(&moncondlock);
    pthread_cond_signal(&writecond);
    pthread_mutex_unlock(&moncondlock);
  }
}


bool FxManager::checkSocketStatus()
{
  if(monsockStatus!=OPENED)
  {
    if (monsockStatus==PENDING)
    {
      int status;
      struct pollfd fds[1];

      fds[0].fd = mon_socket;
      fds[0].events = POLLOUT|POLLWRBAND;

      status = poll(fds, 1, 0);
      if(status < 0)
      {
        cdebug << startl << "POLL FAILED" << endl;
        perror("poll");
        return false;
      }
      else if (status==0)
      { // Nothing ready
        cwarn << startl << "Connection to monitor socket still pending" << endl;
        return false;
      }
      else
      { // Either connected or error

        /* Get the return code from the connect */
        int ret;
        socklen_t len=sizeof(ret);
        status=getsockopt(mon_socket,SOL_SOCKET,SO_ERROR,&ret,&len);
        if (status<0) {
          mon_socket = -1;
          monsockStatus=CLOSED;
          perror("getsockopt");
          cdebug << startl << "GETSOCKOPT FAILED" << endl;
          return false;
        }

        /* ret=0 means success, otherwise it contains the errno */
        if(ret) {
          mon_socket = -1;
          monsockStatus=CLOSED;
          errno=ret;
          //perror("connect");
          cinfo << startl << "Connection to monitor server failed" << endl;
          return false;
        }
        else
        {
          // Connected!
          cinfo << startl << "Connection to monitor server succeeded" << endl;
          monsockStatus=OPENED;
          return true;
        }
      }
    }
    else if(openMonitorSocket() != 0)
    {
      if (monsockStatus != PENDING)
      {
        cerror << startl << "Monitor socket could not be opened; monitoring not proceeding! Will try again after " << config->getVisBufferLength() << " integrations..." << endl;
      }
      return false;
    }
  }
  return true;
}


//setup monitoring socket
int FxManager::openMonitorSocket() {
  int status, window_size;
  unsigned long ip_addr;
  struct hostent     *hostptr;
  struct sockaddr_in server;    /* Socket address */
  int saveflags;

  hostptr = gethostbyname(hostname);
  if (hostptr==NULL) {
    cerror << startl << "Failed to look up hostname " << hostname << endl;
    return(1);
  }
  
  memcpy(&ip_addr, (char *)hostptr->h_addr, sizeof(ip_addr));
  memset((char *) &server, 0, sizeof(server));
  server.sin_family = AF_INET;
  server.sin_port = htons((unsigned short)monitorport); 
  server.sin_addr.s_addr = ip_addr;
  
  cinfo << startl << "Trying to connect to " << inet_ntoa(server.sin_addr) << endl;
    
  mon_socket = socket(AF_INET, SOCK_STREAM, 0);
  if (mon_socket==-1) {
    monsockStatus = CLOSED;
    cerror << startl << "Failed to allocate socket: " << strerror(errno) << endl;
    return(1);
  }

  /* Set the window size to TCP actually works */
  window_size =  Configuration::MONITOR_TCP_WINDOWBYTES;
  status = setsockopt(mon_socket, SOL_SOCKET, SO_SNDBUF,
                      (char *) &window_size, sizeof(window_size));
  if (status!=0) {
    close(mon_socket);
    mon_socket = -1;
    monsockStatus = CLOSED;
    cerror << startl << "Setting socket options: " << strerror(errno) << endl;
    return(1);
  }

  saveflags=fcntl(mon_socket,F_GETFL,0);
  if(saveflags<0) {
    perror("fcntl1");
    return 1;
  }

  /* Set non blocking */
  if(fcntl(mon_socket,F_SETFL,saveflags|O_NONBLOCK)<0) {
    perror("fcntl2");
    close(mon_socket);
    mon_socket = -1;
    monsockStatus = CLOSED;
    return 1;
  }

  // try to connect    
  status = connect(mon_socket, (struct sockaddr *) &server, sizeof(server));

  // Return original flags, ie blocking
  if(fcntl(mon_socket,F_SETFL,saveflags)<0) {
    perror("fcntl3");
    close(mon_socket);
    mon_socket = -1;
    monsockStatus = CLOSED;
    return 1;
  }

  /* return unless the connection was successful or the connect is
           still in progress. */

  if(status==0) {
    monsockStatus = OPENED;
    cinfo << startl << "Immediate connection to monitor server" << endl;
    return 0;
  } else {
    if (errno!=EINPROGRESS) {
      monsockStatus = CLOSED;
      mon_socket = -1;
      cerror << startl << "Connection to monitor_server failed: " << strerror(errno) << endl;
      return 1;
    } else {
      monsockStatus = PENDING;
      return 1;
    }
  }

  return 1;
} /* Setup Net */
// vim: shiftwidth=2:softtabstop=2:expandtab
