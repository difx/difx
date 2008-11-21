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
#ifdef HAVE_RPFITS
#include <RPFITS.h>
#endif
#ifdef HAVE_DIFXMESSAGE
#include <difxmessage.h>
#endif
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
  : config(conf), return_comm(rcomm), numcores(ncores), mpiid(id), monitor(mon), hostname(hname), monitorport(port)
{
  int perr;
  const string * polnames;

  cinfo << startl << "STARTING " << PACKAGE_NAME << " version " << VERSION << endl;

#ifdef HAVE_DIFXMESSAGE
  difxMessageSendDifxStatus(DIFX_STATE_STARTING, "Version " VERSION, 0.0, 0, 0);
#endif

  /* set the PIPE signal handler to 'catch_pipe' */
  signal(SIGPIPE, catch_pipe);

  terminatenow = false;
  numdatastreams = config->getNumDataStreams();
  startmjd = config->getStartMJD();
  startseconds = config->getStartSeconds();
  startns = config->getStartNS();
  executetimeseconds = config->getExecuteSeconds();
  config->loaduvwinfo(false);
  uvw = config->getUVW();
  skipseconds = 0;
  currentconfigindex = config->getConfigIndex(skipseconds);
  while(currentconfigindex < 0 && skipseconds < executetimeseconds)
  {
    //cinfo << startl << "Skipping ahead to " << skipseconds << " seconds" << endl;
    currentconfigindex = config->getConfigIndex(++skipseconds);
  }
  if(skipseconds == executetimeseconds)
  {
    cfatal << startl << "Could not locate any of the specified sources in the specified time range - aborting!!!" << endl;
    MPI_Abort(MPI_COMM_WORLD, 1);
  }
  if(skipseconds != 0 && config->getStartNS() != 0) {
    cwarn << startl << "WARNING!!! Fractional start time of " << startseconds << " seconds plus " << startns << " ns was specified, but the start time corresponded to a configuration not specified in the input file and hence we are skipping " << skipseconds << " seconds ahead! The ns offset will be set to 0!!!" << endl;
    startns = 0;
  }
  halfsampleseconds = 1.0/(config->getDBandwidth(currentconfigindex, 0, 0)*4000000.0);
  inttime = config->getIntTime(currentconfigindex);
  nsincrement = int(1000.0*(config->getBlocksPerSend(currentconfigindex)*config->getNumChannels(currentconfigindex))/(config->getDBandwidth(currentconfigindex, 0, 0))+ 0.5);
  //numchannels = config->getNumChannels(currentconfigindex);
  //samplespersecond = int(2000000*config->getDBandwidth(currentconfigindex, 0, 0) + 0.5);

  numbaselines = (numdatastreams*(numdatastreams-1))/2;
  resultlength = config->getMaxResultLength();
  resultbuffer = vectorAlloc_cf32(resultlength);
  datastreamids = new int[numdatastreams];
  coreids = new int[numcores];
  for(int i=0;i<numdatastreams;i++)
    datastreamids[i] = dids[i];
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
      coretimes[i][j] = new int[2];
  }

  //create the visbuffer array
  //integrationsamples = int(DataStream::SAMPLES_PER_SECOND*dumptime + 0.5);
  visbuffer = new Visibility*[config->getVisBufferLength()];
  bufferlock = new pthread_mutex_t[config->getVisBufferLength()];
  islocked = new bool[config->getVisBufferLength()];
  if(config->circularPolarisations())
    polnames = ((config->getMaxProducts() == 1)&&(config->getDBandPol(0,0,0)=='L'))?LL_CIRCULAR_POL_NAMES:CIRCULAR_POL_NAMES;
  else
    polnames = LINEAR_POL_NAMES;
  for(int i=0;i<config->getVisBufferLength();i++)
  {
    visbuffer[i] = new Visibility(config, i, config->getVisBufferLength(), executetimeseconds, skipseconds, startns, polnames, monitor, monitorport, hostname, &mon_socket, monitor_skip);
    pthread_mutex_init(&(bufferlock[i]), NULL);
    islocked[i] = false;
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

  cinfo << startl << "Estimated memory usage by FXManager: " << float(uvw->getNumUVWPoints()*24 + config->getVisBufferLength()*config->getMaxResultLength()*8)/1048576.0 << " MB" << endl;
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
  vectorFree(resultbuffer);
  for(int i=0;i<config->getVisBufferLength();i++)
    delete visbuffer[i];
  delete [] visbuffer;
  //delete [] writequeue;
  //delete [] fileopened;
  delete [] islocked;
  delete [] bufferlock;

#ifdef HAVE_DIFXMESSAGE
  if(terminatenow)
  {
    difxMessageSendDifxStatus(DIFX_STATE_TERMINATED, "", 0.0, 0, 0);
  }
  else
  {
    difxMessageSendDifxStatus(DIFX_STATE_DONE, "", 0.0, 0, 0);
  }
#endif
}

void interrupthandler(int sig)
{
  cwarn << startl << "FXMANAGER caught a signal and is going to shut down the correlator" << endl;
  terminatenow = true;
}

void FxManager::terminate()
{
#ifdef HAVE_DIFXMESSAGE
  if(terminatenow)
  {
    difxMessageSendDifxStatus(DIFX_STATE_TERMINATING, "", 0.0, 0, 0);
  }
  else
  {
    difxMessageSendDifxStatus(DIFX_STATE_ENDING, "", 0.0, 0, 0);
  }
#endif
  cinfo << startl << "FXMANAGER: Sending terminate signals" << endl;
  for(int i=0;i<numcores;i++)
    MPI_Send(senddata, 1, MPI_INT, coreids[i], CR_TERMINATE, return_comm);
  for(int i=0;i<numdatastreams;i++)
    MPI_Send(senddata, 3, MPI_INT, datastreamids[i], DS_TERMINATE, MPI_COMM_WORLD);
}
/*!
    \fn FxManager::execute()
 */
void FxManager::execute()
{
  cinfo << startl << "Hello World, I am the FxManager" << endl;
  int perr;
  senddata[1] = skipseconds;
  senddata[2] = startns;

  //start by sending a job to each core
  for(int i=0;i<Core::RECEIVE_RING_LENGTH;i++)
  {
    for(int j=0;j<numcores;j++)
    {
      senddata[0] = coreids[j];
      //cinfo << startl << "FXMANAGER: Telling the datastreams to send data to core " << coreids[i] << endl;
      sendData(senddata, j);
    }
  }
  
  signal(SIGINT, &interrupthandler);
  
  //now receive and send until there are no more jobs to send
  //for(long long i=0;i<runto;i++)
  while(senddata[1] < executetimeseconds && terminatenow == false)
  {
    //receive from any core, and send data straight back
    receiveData(true);
  }

  //now send the terminate signal to each datastream and each core
  terminate();
  
  //now receive the final data from each core
  for(int i=0;i<Core::RECEIVE_RING_LENGTH;i++)
  {
    for(int j=0;j<numcores;j++)
      receiveData(false);
  }
  
  //ensure the thread writes out all waiting visibilities
  keepwriting = false;
  for(int i=0;i<=(newestlockedvis+config->getVisBufferLength()-oldestlockedvis)%config->getVisBufferLength();i++)
  {
    perr = pthread_mutex_unlock(&(bufferlock[(oldestlockedvis+i)%config->getVisBufferLength()]));
    if(perr!=0)
      csevere << startl << "FxManager error trying to unlock bufferlock[" << (oldestlockedvis+i)%config->getVisBufferLength() << "] for the last time" << endl; 
  }
  //perr = pthread_mutex_lock(&queuelock);
  //if(perr!=0)
  //  csevere << startl << "FxManager error trying to lock queue for the last time" << endl;
  //writewaiting = 0;
  //double mintime = 0.0;
  //int minindex = 0;
  //for(int i=1;i<config->getVisBufferLength();i++)
  //{
  //  if(visbuffer[i]->getTime() < mintime)
  //  {
  //    mintime = visbuffer[i]->getTime();
  //    minindex = i;
  //  }
  //}
  //for(int i=minindex;i<minindex+config->getVisBufferLength();i++)
  //  writequeue[writewaiting++] = visbuffer[i%config->getVisBufferLength()];
  //perr = pthread_mutex_unlock(&queuelock);
  //if(perr!=0)
  //  csevere << startl << "FxManager error trying to unlock queue for the last time" << endl; 
  
  //join up the write thread
  //perr = pthread_cond_signal(&queuecond);
  //if(perr != 0)
  //  csevere << startl << "FxManager error trying to signal writethread to wake up!!!" << endl;
  perr = pthread_join(writethread, NULL);
  if(perr != 0)
    csevere << startl << "Error in closing writethread!!!" << endl;

  cinfo << startl << "FxManager is finished" << endl;
}

void FxManager::sendData(int data[], int coreindex)
{
  int configindex;
  //cinfo << startl << "FXMANAGER is about to send data of length 3 to the telescopes" << endl;
  MPI_Send(&data[1], 2, MPI_INT, coreids[coreindex], CR_RECEIVETIME, return_comm);

  for(int j=0;j<numdatastreams;j++)
  {
    //cinfo << startl << "FXMANAGER about to send to telescope " << telescopeids[j] << endl;
    MPI_Ssend(data, 3, MPI_INT, datastreamids[j], DS_PROCESS, MPI_COMM_WORLD);
  }
  coretimes[numsent[coreindex]%Core::RECEIVE_RING_LENGTH][coreindex][0] = data[1];
  coretimes[numsent[coreindex]%Core::RECEIVE_RING_LENGTH][coreindex][1] = data[2];
  numsent[coreindex]++;
  data[2] += nsincrement;
  if(data[2] >= 1000000000)
  {
    data[2] -= 1000000000;
    data[1]++;
    //check that we haven't changed configs
    configindex = config->getConfigIndex(data[1]);
    while(configindex < 0 && data[1] < executetimeseconds)
    {
      configindex = config->getConfigIndex(++data[1]); //we won't send out data for this time
      data[2] = 0;
    }
    if(configindex != currentconfigindex && !(configindex < 0))
    {
      currentconfigindex = configindex;
      inttime = config->getIntTime(currentconfigindex);
      nsincrement = int(1000.0*(config->getBlocksPerSend(currentconfigindex)*config->getNumChannels(currentconfigindex))/(config->getDBandwidth(currentconfigindex, 0, 0))+ 0.5);
      halfsampleseconds = 1.0/(config->getDBandwidth(currentconfigindex, 0, 0)*4000000.0);
      //numchannels = config->getNumChannels(currentconfigindex);
      //samplespersecond = int(2000000*config->getDBandwidth(currentconfigindex, 0, 0) + 0.5);
    }
  }
  //cinfo << startl <<  "FXMANAGER has finished sending data" << endl;
}

void FxManager::receiveData(bool resend)
{
  MPI_Status mpistatus;
  int sourcecore, sourceid=0, visindex, perr;
  bool viscomplete;
  double time;
  int i, flag;

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

  //put the data in the appropriate slot
  if(mpistatus.MPI_TAG == CR_VALIDVIS) // the data is valid
  {
    //find where it belongs
    visindex = locateVisIndex(sourceid);
    time = coretimes[(numsent[sourceid]+extrareceived[sourceid]) % Core::RECEIVE_RING_LENGTH][sourceid][0] + double(coretimes[(numsent[sourceid]+extrareceived[sourceid]) % Core::RECEIVE_RING_LENGTH][sourceid][1])/1000000000.0;

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
      cerror << startl << "Error - stale data was received from core " << sourceid << " regarding time " << time << " seconds - it will be ignored!!!" << endl;
    else
    {
      //now store the data appropriately - if we have reached sufficient sub-accumulations, release this Visibility so the writing thread can write it out
      viscomplete = visbuffer[visindex]->addData(resultbuffer);
      if(viscomplete)
      {
        visbuffer[visindex]->multicastweights();

        //cinfo << startl << "FXMANAGER telling Vis. " << visindex << " to write out - this refers to time " << visbuffer[visindex]->getTime() << " - the previous buffer has time " << visbuffer[(visindex-1+config->getVisBufferLength())%config->getVisBufferLength()]->getTime() << ", and the next one has " << visbuffer[(visindex +1)%config->getVisBufferLength()]->getTime() << endl;
        cinfo << startl << "Vis. " << visindex << " to write out time " << visbuffer[visindex]->getTime() << endl;
        cverbose << startl << "Vis. " << visindex << " Newestlockedvis is " << newestlockedvis << ", while oldestlockedvis is " << oldestlockedvis << endl;
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
      }
    }
  }
  else
  {
    cinfo << startl << "Invalid data was recieved from core " << sourcecore << " regarding time " << coretimes[(numsent[sourceid]) % Core::RECEIVE_RING_LENGTH][sourceid][0] << " seconds plus " << coretimes[(numsent[sourceid]) % Core::RECEIVE_RING_LENGTH][sourceid][1] << " ns" << endl;

    //immediately get some more data heading to that node
    if(resend)
    {
      senddata[0] = sourcecore;
      sendData(senddata, sourceid);
    }
  }
}

void * FxManager::launchNewWriteThread(void * thismanager)
{
  FxManager * me = (FxManager *)thismanager;

  me->initialiseOutput();
  me->loopwrite();
  me->finaliseOutput();

  return 0;
}

void FxManager::initialiseOutput()
{
  int flag = -2; //open a new file
  if(config->getOutputFormat() == Configuration::RPFITS)  //if its RPFITS output create the file
  {
#ifdef HAVE_RPFITS
    writeheader();
    rpfitsout_(&flag, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    if(flag < 0)
    {
      cfatal << startl << "Error - could not open output file " << config->getOutputFilename() << " - aborting!!!" << endl;
      MPI_Abort(MPI_COMM_WORLD, 1);
    }
#else
    cfatal << startl << "RPFITS not compiled in.  quitting" << endl;
    MPI_Abort(MPI_COMM_WORLD, 1);
#endif
  }
  else if(config->getOutputFormat() == Configuration::DIFX)
  {
    //create the directory - if that doesn't work, abort as we can't guarantee no overwriting data
    flag = mkdir(config->getOutputFilename().c_str(), 0775);
    if(flag < 0) {
      cfatal << startl << "Error trying to create directory " << config->getOutputFilename() << ": " << flag << ", ABORTING!" << endl;
      MPI_Abort(MPI_COMM_WORLD, 1);
    }
  }
}

void FxManager::finaliseOutput()
{
#ifdef HAVE_RPFITS
  int flag = 1;
  if(config->getOutputFormat() == Configuration::RPFITS)  //only if its RPFITS output do we need to do anything
  {
    //close the RPFits file
    rpfitsout_(&flag, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  }
#endif
}

void FxManager::loopwrite()
{
  int perr;
  int lastconfigindex = currentconfigindex;
  int atsegment = 0;
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
    perr = pthread_mutex_lock(&(bufferlock[atsegment]));
    if(perr != 0)
      csevere << startl << "Writethread error trying to lock bufferlock[" << atsegment << "]!!!" << endl;
    //unlock the previous section
    perr = pthread_mutex_unlock(&(bufferlock[(atsegment+config->getVisBufferLength()-1)%config->getVisBufferLength()]));
    if(perr != 0)
      csevere << startl << "Writethread error trying to unlock bufferlock[" << (atsegment+config->getVisBufferLength()-1)%config->getVisBufferLength() << "]!!!" << endl;
    if(visbuffer[atsegment]->getCurrentConfig() != lastconfigindex)
    {
      lastconfigindex = visbuffer[atsegment]->getCurrentConfig();
#ifdef HAVE_RPFITS
      param_.intbase = float(config->getIntTime(lastconfigindex));
#endif
    }
    visbuffer[atsegment]->writedata();
    visbuffer[atsegment]->increment();
    atsegment=(atsegment+1)%config->getVisBufferLength();
  }
  
  //now we're done, so run thru everyone just to be sure
  perr = pthread_mutex_unlock(&(bufferlock[(atsegment+config->getVisBufferLength()-1)%config->getVisBufferLength()]));
  if(perr != 0)
    csevere << startl << "Writethread error trying to unlock bufferlock[" << (atsegment+config->getVisBufferLength()-1)%config->getVisBufferLength() << "]!!!" << endl;
  for(int i=0;i<config->getVisBufferLength();i++)
  {
    visbuffer[(atsegment+i)%config->getVisBufferLength()]->writedata();
  }
}

void FxManager::writeheader()
{
#ifdef HAVE_RPFITS
  int numproducts, maxfrequencies, year, month, day, uindex;
  char obsdate[12];
  
  config->mjd2ymd(startmjd, year, month, day);
  sprintf(obsdate, "%04u-%02u-%02u", year, month, day);

  //set up the outputfilename
  config->makeFortranString(config->getOutputFilename(), 256, names_.file);
  
  //set up the parameters
  param_.write_wt = 0; //don't write weights
  param_.ncard = 0;
  param_.intbase = float(config->getIntTime(config->getConfigIndex(0)));
  param_.data_format = 2; //for complex visibilities, no weights
  doubles_.x_array = 0.0;
  doubles_.y_array = 0.0;
  doubles_.z_array = 0.0;
  config->makeFortranString("J2000", 8, names_.coord);
  config->makeFortranString("ATLBA", 16, names_.instrument);
  config->makeFortranString(string(obsdate), 12, names_.datobs); 

  //set up the antenna info
  anten_.nant = config->getTelescopeTableLength();
  cinfo << startl << "Number of antennas is " << anten_.nant << endl;
  for(int i=0;i<anten_.nant;i++)
  {
    anten_.ant_num[i] = i+1;
    config->makeFortranString(config->getTelescopeName(i), ANTENNA_NAME_LENGTH, &(names_.sta[i*ANTENNA_NAME_LENGTH]));
    config->makeFortranString("R", 2, &(names_.feed_type[i*4]));
    config->makeFortranString("L", 2, &(names_.feed_type[i*4 + 2]));

    uindex = -1;
    //work out the index of the telescope in the uvw file
    for(int j=0;j<uvw->getNumStations();j++)
    {
      if(config->getTelescopeName(i) == uvw->getStationName(j))
        uindex = j;
    }
    if(uindex < 0)
    {
      cerror << startl << "Error - could not find station " << config->getTelescopeName(i) << " in the uvw file when making rpfits header!!!" << endl;
      if(config->stationUsed(i))
      {
        cfatal << startl << "This station is used in the correlation so I will abort!!!" << endl;
        MPI_Abort(MPI_COMM_WORLD, 1);
      }
      else
      {
        cerror << startl << "Station is not used in this correlation so its parameters will be initialised to 0!!!" << endl;
        anten_.ant_mount[i] = 1;
        doubles_.x[i] = 0.0;
        doubles_.y[i] = 0.0;
        doubles_.z[i] = 0.0;
      }
    }
    else
    {
      anten_.ant_mount[i] = uvw->getStationMount(uindex);
      doubles_.x[i] = uvw->getStationX(uindex);
      doubles_.y[i] = uvw->getStationY(uindex);
      doubles_.z[i] = uvw->getStationZ(uindex);
      doubles_.axis_offset[i] = 0.0; //THIS IS AN OVERSIGHT THAT NEEDS TO BE CORRECTED SOON!
    }
  }

  //set up the IF info
  numproducts = config->getMaxProducts();
  maxfrequencies = config->getFreqTableLength();
  if_.n_if = maxfrequencies*config->getNumIndependentChannelConfigs();
  cinfo << startl << "Number of IFs is " << if_.n_if*config->getNumIndependentChannelConfigs() << endl;
  string blank = "  ";
  for(int j=0;j<config->getNumIndependentChannelConfigs();j++)
  {
    for(int i=0;i<maxfrequencies;i++)
    {
      if_.if_invert[i] = 1; //should never be inverted as the correlator can invert any inverted bands
      if_.if_nfreq[i] = config->getNumChannels(config->getFirstNaturalConfigIndex(j)) + 1;
      if_.if_nstok[i] = numproducts;
      if_.if_num[i] = i+1 + j*maxfrequencies;
      if_.if_sampl[i] = config->getDNumBits(0, 0);
      if_.if_simul[i] = 1; //don't know what these do...?
      if_.if_chain[i] = 1;
      doubles_.if_bw[i] = config->getFreqTableBandwidth(i)*1000000; //convert from MHz to Hz
      doubles_.if_ref[i] = (config->getFreqTableLowerSideband(i))?config->getNumChannels(config->getFirstNaturalConfigIndex(j))+1.0:1.0;
      doubles_.if_freq[i] = config->getFreqTableFreq(i)*1000000; //convert from MHz to Hz
      for(int j=0;j<numproducts;j++)
        config->makeFortranString((config->circularPolarisations())?CIRCULAR_POL_NAMES[j]:LINEAR_POL_NAMES[j], STOKES_NAME_LENGTH, &(names_.if_cstok[(4*i + j)*STOKES_NAME_LENGTH]));
      for(int j=numproducts;j<4;j++)
        config->makeFortranString(blank, STOKES_NAME_LENGTH, &(names_.if_cstok[(4*i + j)*STOKES_NAME_LENGTH]));
    }
  }

  //set up the source info
  su_.n_su = uvw->getNumSources();
  for(int i=0;i<su_.n_su;i++)
  {
    su_.su_num[i] = i+1;
    doubles_.su_ra[i] = uvw->getSourceRA(i);
    doubles_.su_dec[i] = uvw->getSourceDec(i);
    doubles_.su_pra[i] = uvw->getSourceRA(i);
    doubles_.su_pdec[i] = uvw->getSourceDec(i);
    config->makeFortranString(uvw->getSourceName(i), SOURCE_NAME_LENGTH, &(names_.su_name[i*SOURCE_NAME_LENGTH]));
    config->makeFortranString(blank, SOURCE_CALCODE_LENGTH, &(names_.su_cal[i*SOURCE_CALCODE_LENGTH]));
  }

  //set up the proper motion
  proper_.pm_epoch = 2000.0;
#endif
}

int FxManager::locateVisIndex(int coreid)
{
  bool tooold = true;
  int perr, count;
  double difference;

  for(int i=0;i<=(newestlockedvis-oldestlockedvis+config->getVisBufferLength())%config->getVisBufferLength();i++)
  {
    difference = visbuffer[(oldestlockedvis+i)%config->getVisBufferLength()]->timeDifference(coretimes[(numsent[coreid]+extrareceived[coreid]) % Core::RECEIVE_RING_LENGTH][coreid][0], coretimes[(numsent[coreid]+extrareceived[coreid])%Core::RECEIVE_RING_LENGTH][coreid][1]);
    if(difference > halfsampleseconds)
    {
      tooold = false;
      if(difference - inttime < halfsampleseconds) //we have found the correct Visibility
      {
        return (oldestlockedvis+i)%config->getVisBufferLength();
      }
    }
  }
  if(tooold)
    return -1;
  else
  {
    //try locking some more visibilities til we get to what we need
    while((newestlockedvis-oldestlockedvis+config->getVisBufferLength())%config->getVisBufferLength() < config->getVisBufferLength()/2)
    {
      newestlockedvis = (newestlockedvis+1)%config->getVisBufferLength();
      //lock another visibility
      perr = pthread_mutex_lock(&(bufferlock[newestlockedvis]));
      if(perr != 0)
        csevere << startl << "Error in fxmanager locking visibility " << newestlockedvis << endl;
      islocked[newestlockedvis] = true;
      //check if its good
      difference = visbuffer[newestlockedvis]->timeDifference(coretimes[(numsent[coreid]+extrareceived[coreid]) % Core::RECEIVE_RING_LENGTH][coreid][0], coretimes[(numsent[coreid]+extrareceived[coreid])%Core::RECEIVE_RING_LENGTH][coreid][1]);
      if(difference <= inttime)
        return newestlockedvis;
    }
    //d'oh - its newer than we can handle - have to drop old data until we catch up
    cerror << startl << "Error - data was received which is too recent (" << coretimes[(numsent[coreid])% Core::RECEIVE_RING_LENGTH][coreid][0] << "sec + " << coretimes[(numsent[coreid])%Core::RECEIVE_RING_LENGTH][coreid][1] << "ns)!  Will force existing data to be dropped until we have caught up coreid="<< coreid << endl;
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
        oldestlockedvis = (oldestlockedvis+1)%config->getVisBufferLength();
        count++;
      }
      for(int j=0;j<count;j++)
      {
        newestlockedvis = (newestlockedvis+1)%config->getVisBufferLength();
        perr = pthread_mutex_lock(&(bufferlock[newestlockedvis]));
        if(perr != 0)
          csevere << startl << "Error in fxmanager locking visibility " << newestlockedvis << endl;
        islocked[newestlockedvis] = true;
        difference = visbuffer[newestlockedvis]->timeDifference(coretimes[(numsent[coreid])% Core::RECEIVE_RING_LENGTH][coreid][0], coretimes[(numsent[coreid])%Core::RECEIVE_RING_LENGTH][coreid][1]);
        if(difference <= inttime) //we've finally caught up
          break;
      }
    }
    return newestlockedvis;
  }

  return -1; //unreachable
}

