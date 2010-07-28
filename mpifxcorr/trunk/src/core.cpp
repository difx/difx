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
#include <mpi.h>
#include "core.h"
#include "fxmanager.h"
#include "alert.h"

Core::Core(int id, Configuration * conf, int * dids, MPI_Comm rcomm)
  : mpiid(id), config(conf), return_comm(rcomm)
{
  int status, perr;
  double guardratio, maxguardratio;
  estimatedbytes = config->getEstimatedBytes();

  //Get all the correlation parameters from config
  model = config->getModel();
  numdatastreams = config->getNumDataStreams();
  numbaselines = config->getNumBaselines();
  maxthreadresultlength = config->getMaxThreadResultLength();
  maxcoreresultlength = config->getMaxCoreResultLength();
  numprocessthreads = config->getCNumProcessThreads(mpiid - numdatastreams - fxcorr::FIRSTTELESCOPEID);
  currentconfigindex = 0;
  startmjd = config->getStartMJD();
  startseconds = config->getStartSeconds();

  //work out the biggest overhead from any of the active configurations
  maxguardratio = 1.0;
  databytes = config->getMaxDataBytes();
  for(int i=0;i<config->getNumConfigs();i++)
  {
    guardratio = double(config->getSubintNS(i) + config->getGuardNS(i))/double(config->getSubintNS(i));
    if(guardratio > maxguardratio)
    {
      databytes = int((((long long)config->getMaxDataBytes())*((long long)(config->getSubintNS(i)+config->getGuardNS(i))))/config->getSubintNS(i));
      maxguardratio = guardratio;
    }
  }

  //if we have a mark5 datastream, make this number a bit bigger to be safe
  int overheadbytes = 0;
  for(int i=0;i<numdatastreams;i++)
  {
    if(config->isMkV(i) || config->isNativeMkV(i))
      overheadbytes = config->getFrameBytes(0, i);
  }
  databytes += overheadbytes;

  //allocate the send/receive circular buffer (length RECEIVE_RING_LENGTH)
  controllength = config->getMaxBlocksPerSend() + 4;
  procslots = new processslot[RECEIVE_RING_LENGTH];
  for(int i=0;i<RECEIVE_RING_LENGTH;i++)
  {
    procslots[i].results = vectorAlloc_cf32(maxcoreresultlength);
    procslots[i].floatresults = (f32*)procslots[i].results;
    //set up the info for this slot, using the first configuration
    status = vectorZero_cf32(procslots[i].results, maxcoreresultlength);
    if(status != vecNoErr)
      csevere << startl << "Error trying to zero results in core " << mpiid << ", processing slot " << i << endl;
    procslots[i].resultsvalid = CR_VALIDVIS;
    procslots[i].configindex = currentconfigindex;
    procslots[i].threadresultlength = config->getThreadResultLength(currentconfigindex);
    procslots[i].coreresultlength = config->getCoreResultLength(currentconfigindex);
    procslots[i].slotlocks = new pthread_mutex_t[numprocessthreads];
    procslots[i].viscopylocks = new pthread_mutex_t*[config->getFreqTableLength()];
    for(int j=0;j<config->getFreqTableLength();j++)
      procslots[i].viscopylocks[j] = new pthread_mutex_t[numbaselines];
    for(int j=0;j<numprocessthreads;j++) {
      perr = pthread_mutex_init(&(procslots[i].slotlocks[j]), NULL);
      if(perr != 0)
        csevere << startl << "Problem initialising processthread " << j << " lock in slot " << i << "(" << perr << ")" << endl;
    }
    for(int j=0;j<config->getFreqTableLength();j++)
    {
      for(int k=0;k<numbaselines;k++) {
        perr = pthread_mutex_init(&(procslots[i].viscopylocks[j][k]), NULL);
        if(perr != 0)
          csevere << startl << "Problem initialising viscopylock for freq " << j << ", baseline " << k << " in slot " << i << "(" << perr << ")" << endl;
      }
    }
    perr = pthread_mutex_init(&(procslots[i].autocorrcopylock), NULL);
    if(perr != 0)
      csevere << startl << "Problem initialising autocorrcopylock in slot " << i << "(" << perr << ")" << endl;
    perr = pthread_mutex_init(&(procslots[i].bweightcopylock), NULL);
    if(perr != 0)
      csevere << startl << "Problem initialising bweightcopylock in slot " << i << "(" << perr << ")" << endl;
    perr = pthread_mutex_init(&(procslots[i].acweightcopylock), NULL);
    if(perr != 0)
      csevere << startl << "Problem initialising acweightcopylock in slot " << i << "(" << perr << ")" << endl;
    perr = pthread_mutex_init(&(procslots[i].pcalcopylock), NULL);
    if(perr != 0)
      csevere << startl << "Problem initialising pcalcopylock in slot " << i << "(" << perr << ")" << endl;
    procslots[i].datalengthbytes = new int[numdatastreams];
    procslots[i].databuffer = new u8*[numdatastreams];
    procslots[i].controlbuffer = new s32*[numdatastreams];
    procslots[i].keepprocessing = true;
    procslots[i].numpulsarbins = config->getNumPulsarBins(currentconfigindex);
    procslots[i].scrunchoutput = config->scrunchOutputOn(currentconfigindex);
    procslots[i].pulsarbin = config->pulsarBinOn(currentconfigindex);
    for(int j=0;j<numdatastreams;j++)
    {
      procslots[i].databuffer[j] = vectorAlloc_u8(databytes);
      procslots[i].controlbuffer[j] = vectorAlloc_s32(controllength);
      estimatedbytes += databytes;
      estimatedbytes += controllength*4;
    }
  }

  //initialise the threads that do the actual processing
  numcomplete = 0;
  processthreads = new pthread_t[numprocessthreads];
  processconds = new pthread_cond_t[numprocessthreads];
  processthreadinitialised = new bool[numprocessthreads];
  threadbytes = new int[numprocessthreads];
  for(int i=0;i<numprocessthreads;i++)
  {
    pthread_cond_init(&processconds[i], NULL);
    processthreadinitialised[i] = false;
    threadbytes[i] = 8*maxthreadresultlength;
  }

  //initialise the MPI communication objects
  datarequests = new MPI_Request[numdatastreams];
  controlrequests = new MPI_Request[numdatastreams];
  msgstatuses = new MPI_Status[numdatastreams];

  //copy the datastream ids
  datastreamids = new int[numdatastreams];
  for(int i=0;i<numdatastreams;i++)
    datastreamids[i] = dids[i];

  //initialise the binary message infrastructure
  difxMessageInitBinary();
}


Core::~Core()
{
  for(int i=0;i<RECEIVE_RING_LENGTH;i++)
  {
    for(int j=0;j<numdatastreams;j++)
    {
      vectorFree(procslots[i].databuffer[j]);
      vectorFree(procslots[i].controlbuffer[j]);
    }
    delete [] procslots[i].slotlocks;
    for(int j=0;j<config->getFreqTableLength();j++)
      delete [] procslots[i].viscopylocks[j];
    delete [] procslots[i].viscopylocks;
    delete [] procslots[i].datalengthbytes;
    delete [] procslots[i].databuffer;
    delete [] procslots[i].controlbuffer;
    vectorFree(procslots[i].results);
  }
  delete [] threadbytes;
  delete [] processthreads;
  delete [] processconds;
  delete [] processthreadinitialised;
  delete [] procslots;
  delete [] datarequests;
  delete [] controlrequests;
  delete [] msgstatuses;
  delete [] datastreamids;
}


void Core::execute()
{
  int perr, status, lastconfigindex, adjust, countdown;
  bool terminate;
  processthreadinfo * threadinfos = new processthreadinfo[numprocessthreads];
  
  terminate = false;
  numreceived = 0;
  cverbose << startl << "Core " << mpiid << " has started executing!!! Numprocessthreads is " << numprocessthreads << endl;

  //get the lock for the first slot, one per thread
  for(int i=0;i<numprocessthreads;i++)
  {
    perr = pthread_mutex_lock(&(procslots[numreceived].slotlocks[i]));
    if(perr != 0)
      csevere << startl << "Error in Core " << mpiid << " attempt to lock mutex" << numreceived << " of thread " << i << endl;
  }

  //cverbose << startl << "Core about to fill up receive ring buffer" << endl;
  //start off by filling up the data and control buffers for all slots
  for(int i=0;i<RECEIVE_RING_LENGTH-1;i++)
  {
    if(!terminate)
      numreceived += receivedata(numreceived, &terminate);
  }
  //cverbose << startl << "Core has filled up receive ring buffer" << endl;

  //also lock the second last slot, to keep any cheeky thread from getting round the entire
  //RECEIVE_RING before we wake back up
  for(int i=0;i<numprocessthreads;i++)
  {
    perr = pthread_mutex_lock(&(procslots[RECEIVE_RING_LENGTH-2].slotlocks[i]));
    if(perr != 0)
      csevere << startl << "Error in main thread attempting to lock mutex " << RECEIVE_RING_LENGTH-2 << " of thread " << i << " during startup" << endl;
  }

  //now we have the lock on the last two slots in the ring.  Launch processthreads
  for(int i=0;i<numprocessthreads;i++)
  {
    threadinfos[i].thiscore = this;
    threadinfos[i].processthreadid = i;
    perr = pthread_create(&processthreads[i], NULL, Core::launchNewProcessThread, (void *)(&threadinfos[i]));
    if(perr != 0)
      csevere << startl << "Error in launching Core " << mpiid << " processthread " << i << "!!!" << endl;
  }

  //wait til they are all initialised (and hence have a lock of their own)
  for(int i=0;i<numprocessthreads;i++)
  {
    while(!processthreadinitialised[i])
    {
      perr = pthread_cond_wait(&processconds[i], &(procslots[numreceived].slotlocks[i]));
      if (perr != 0)
        csevere << startl << "Error waiting on processthreadinitialised condition!!!!" << endl;
    }
  }

  //now we definitely have our lock back on the last slot in the buffer,
  //release that supplementary lock (2nd last in buffer)
  for(int i=0;i<numprocessthreads;i++)
  {
    perr = pthread_mutex_unlock(&(procslots[RECEIVE_RING_LENGTH-2].slotlocks[i]));
    if(perr != 0)
      csevere << startl << "Error in main thread attempting to lock mutex " << RECEIVE_RING_LENGTH-2 << " of thread " << i << " during startup" << endl;
  }

  cverbose << startl << "Estimated memory usage by Core is now " << getEstimatedBytes()/(1024.0*1024.0) << " MB" << endl;
  lastconfigindex = procslots[0].configindex;
  while(!terminate) //the data is valid, so keep processing
  {
    //increment and receive some more data
    numreceived += receivedata(numreceived % RECEIVE_RING_LENGTH, &terminate);

    //send off a message if we are back at the start of the buffer
    if(numreceived % RECEIVE_RING_LENGTH == 0)
      cinfo << startl << "CORE: " << numreceived-(numcomplete+1) << " unprocessed segments, 1 being processed, and " << RECEIVE_RING_LENGTH-(numreceived-numcomplete) << " to be sent" << endl;

    if(terminate)
      break;

    //send the results back
    MPI_Ssend(procslots[numreceived%RECEIVE_RING_LENGTH].results, procslots[numreceived%RECEIVE_RING_LENGTH].coreresultlength*2, MPI_FLOAT, fxcorr::MANAGERID, procslots[numreceived%RECEIVE_RING_LENGTH].resultsvalid, return_comm);
    if(procslots[numreceived%RECEIVE_RING_LENGTH].configindex != lastconfigindex)
    {
      cverbose << startl << "After config change, estimated memory usage by Core is " << getEstimatedBytes()/(1024.0*1024.0) << " MB" << endl;
    }

    //zero the results buffer for this slot and set the status back to valid
    status = vectorZero_cf32(procslots[numreceived%RECEIVE_RING_LENGTH].results, procslots[numreceived%RECEIVE_RING_LENGTH].coreresultlength);
    if(status != vecNoErr)
      csevere << startl << "Error trying to zero results in Core!!!" << endl;
    procslots[numreceived%RECEIVE_RING_LENGTH].resultsvalid = CR_VALIDVIS;
  }

  //Run through the shutdown sequence
  for(int i=0;i<numprocessthreads;i++)
  {
    //Unlock the mutex we are currently holding for this thread
    perr = pthread_mutex_unlock(&(procslots[numreceived % RECEIVE_RING_LENGTH].slotlocks[i]));
    if(perr != 0)
      csevere << startl << "Error in Core " << mpiid << " attempt to unlock mutex" << (numreceived+RECEIVE_RING_LENGTH-1) % RECEIVE_RING_LENGTH << " of thread " << i << endl;
  }

  adjust = 0;
  countdown = RECEIVE_RING_LENGTH-1;
  if(numreceived < RECEIVE_RING_LENGTH-1) {
    adjust = (RECEIVE_RING_LENGTH-1)-numreceived;
    countdown = numreceived;
  }

  //ensure all the results we have sitting around have been sent
  for(int i=1;i<RECEIVE_RING_LENGTH;i++)
  {
    if(countdown == 0)
      break;

//    cinfo << startl << "Core " << mpiid << " about to send final values from section " << i << endl;
    for(int j=0;j<numprocessthreads;j++)
    {
      //Lock and unlock first to ensure the threads have finished working on this slot
      perr = pthread_mutex_lock(&(procslots[(numreceived+i+adjust) % RECEIVE_RING_LENGTH].slotlocks[j]));
      if(perr != 0)
        csevere << startl << "Error in Core " << mpiid << " attempt to unlock mutex" << (numreceived+i+adjust) % RECEIVE_RING_LENGTH << " of thread " << j << endl;
      perr = pthread_mutex_unlock(&(procslots[(numreceived+i+adjust) % RECEIVE_RING_LENGTH].slotlocks[j]));
      if(perr != 0)
        csevere << startl << "Error in Core " << mpiid << " attempt to unlock mutex" << (numreceived+i+adjust) % RECEIVE_RING_LENGTH << " of thread " << j << endl;
    }
    //send the results
    MPI_Ssend(procslots[(numreceived+i+adjust)%RECEIVE_RING_LENGTH].results, procslots[(numreceived+i+adjust)%RECEIVE_RING_LENGTH].coreresultlength*2, MPI_FLOAT, fxcorr::MANAGERID, procslots[(numreceived+i+adjust)%RECEIVE_RING_LENGTH].resultsvalid, return_comm);

    countdown--;
  }

//  cinfo << startl << "CORE " << mpiid << " is about to join the processthreads" << endl;

  //join the process threads, they have to already be finished anyway
  for(int i=0;i<numprocessthreads;i++)
  {
    perr = pthread_join(processthreads[i], NULL);
    if(perr != 0)
      csevere << startl << "Error in Core " << mpiid << " attempt to join processthread " << i << endl;
  }
  delete [] threadinfos;

//  cinfo << startl << "CORE " << mpiid << " terminating" << endl;
}

void * Core::launchNewProcessThread(void * tdata)
{
  processthreadinfo * mydata = (processthreadinfo *)tdata;
  (mydata->thiscore)->loopprocess(mydata->processthreadid);

  return 0;
}

int Core::getEstimatedBytes()
{
  int toreturn = estimatedbytes;
  for(int i=0;i<numprocessthreads;i++)
    toreturn += threadbytes[i];
  return toreturn;
}

void Core::loopprocess(int threadid)
{
  int perr, numprocessed, startblock, numblocks, lastconfigindex, numpolycos, maxbins, maxchan, maxpolycos, stadumpchannels, strideplussteplen, maxrotatestrideplussteplength, maxxmaclength, slen;
  double sec;
  bool pulsarbin, somepulsarbin, somescrunch, dumpingsta, nowdumpingsta;
  processslot * currentslot;
  Polyco ** polycos=0;
  Polyco * currentpolyco=0;
  Mode ** modes;
  threadscratchspace * scratchspace = new threadscratchspace;
  scratchspace->threadcrosscorrs = vectorAlloc_cf32(maxthreadresultlength);
  scratchspace->baselineweight = new f32***[config->getFreqTableLength()];
  scratchspace->dsweights = new f32*[numdatastreams];
  for(int i=0;i<numdatastreams;i++)
    scratchspace->dsweights[i] = new f32[config->getMaxNumBufferedFFTs()];
  if(scratchspace->threadcrosscorrs == NULL) {
    cfatal << startl << "Could not allocate thread cross corr space (tried to allocate " << maxthreadresultlength/(1024*1024) << " MB)!!! Aborting." << endl;
    MPI_Abort(MPI_COMM_WORLD, 1);
  }

  scratchspace->pulsarscratchspace=0;
  scratchspace->pulsaraccumspace=0;
  scratchspace->starecord = 0;

  pulsarbin = false;
  somepulsarbin = false;
  somescrunch = false;
  dumpingsta = false;
  maxpolycos = 0;
  maxchan = config->getMaxNumChannels();
  maxbins = config->getMaxNumPulsarBins();
  slen = config->getArrayStrideLength(0);
  if(slen>config->getXmacStrideLength(0))
    slen = config->getXmacStrideLength(0);
  maxrotatestrideplussteplength = slen + maxchan/slen;
  maxxmaclength = config->getXmacStrideLength(0);
  for(int i=1;i<config->getNumConfigs();i++)
  {
    slen = config->getArrayStrideLength(i);
    if(slen>config->getXmacStrideLength(i))
      slen = config->getXmacStrideLength(i);
    strideplussteplen = slen + maxchan/slen;
    if(strideplussteplen > maxrotatestrideplussteplength)
      maxrotatestrideplussteplength = strideplussteplen;
    if(config->getXmacStrideLength(i) > maxxmaclength)
      maxxmaclength = config->getXmacStrideLength(i);
  }
  scratchspace->chanfreqs = vectorAlloc_f64(maxrotatestrideplussteplength);
  scratchspace->rotator = vectorAlloc_cf32(maxrotatestrideplussteplength);
  scratchspace->rotated = vectorAlloc_cf32(maxchan);
  scratchspace->channelsums = vectorAlloc_cf32(maxchan);
  scratchspace->argument = vectorAlloc_f32(3*maxrotatestrideplussteplength);
  threadbytes[threadid] += 20*maxchan + 100*maxrotatestrideplussteplength;

  //work out whether we'll need to do any pulsar binning, and work out the maximum # channels (and # polycos if applicable)
  for(int i=0;i<config->getNumConfigs();i++)
  {
    if(config->pulsarBinOn(i))
    {
      somepulsarbin = true;
      somescrunch = somescrunch || config->scrunchOutputOn(i);
      numpolycos = config->getNumPolycos(i);
      if(numpolycos > maxpolycos)
        maxpolycos = numpolycos;
    }
  }

  //create the necessary pulsar scratch space if required
  if(somepulsarbin)
  {
    scratchspace->pulsarscratchspace = vectorAlloc_cf32(maxxmaclength);
    if(somescrunch) //need separate accumulation space
    {
      scratchspace->pulsaraccumspace = new cf32******[config->getFreqTableLength()];
    }
    createPulsarVaryingSpace(scratchspace->pulsaraccumspace, &(scratchspace->bins), procslots[0].configindex, -1, threadid); //don't need to delete old space
  }

  //create the baselineweight and xmacstrideoffset arrays
  allocateConfigSpecificThreadArrays(scratchspace->baselineweight, procslots[0].configindex, -1, threadid); //don't need to delete old space

  //set to first configuration and set up, creating Modes, Polycos etc
  lastconfigindex = procslots[0].configindex;
  modes = new Mode*[numdatastreams];
  if(somepulsarbin)
    polycos = new Polyco*[maxpolycos];
  updateconfig(lastconfigindex, lastconfigindex, threadid, startblock, numblocks, numpolycos, pulsarbin, modes, polycos, true);
  numprocessed = 0;
//  cinfo << startl << "Core thread id " << threadid << " will be processing from block " << startblock << ", length " << numblocks << endl;

  //lock the end section
  perr = pthread_mutex_lock(&(procslots[RECEIVE_RING_LENGTH-1].slotlocks[threadid]));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << mpiid << "/" << threadid << " error trying lock mutex " << RECEIVE_RING_LENGTH-1 << endl;

  //grab the lock we really want, unlock the end section and signal the main thread we're ready to go
  perr = pthread_mutex_lock(&(procslots[0].slotlocks[threadid]));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << mpiid << "/" << threadid << " error trying lock mutex 0" << endl; 
  perr = pthread_mutex_unlock(&(procslots[RECEIVE_RING_LENGTH-1].slotlocks[threadid]));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << mpiid << "/" << threadid << " error trying unlock mutex " << RECEIVE_RING_LENGTH-1 << endl;
  processthreadinitialised[threadid] = true;
  perr = pthread_cond_signal(&processconds[threadid]);
  if(perr != 0)
    csevere << startl << "Core processthread " << mpiid << "/" << threadid << " error trying to signal main thread to wake up!!!" << endl;
  if(threadid == 0)
    cinfo << startl << "Core " << mpiid << " PROCESSTHREAD " << threadid+1 << "/" << numprocessthreads << " is about to start processing" << endl;

  //while valid, process data
  while(procslots[(numprocessed)%RECEIVE_RING_LENGTH].keepprocessing)
  {
    currentslot = &(procslots[numprocessed%RECEIVE_RING_LENGTH]);
    if(pulsarbin)
    {
      sec = double(startseconds + model->getScanStartSec(currentslot->offsets[0], startmjd, startseconds) + currentslot->offsets[1]) + ((double)currentslot->offsets[2])/1000000000.0;
      //get the correct Polyco for this time range and set it up correctly
      currentpolyco = Polyco::getCurrentPolyco(currentslot->configindex, startmjd, sec/86400.0, polycos, numpolycos, false);
      if(currentpolyco == NULL)
      {
        cfatal << startl << "Could not locate a polyco to cover time " << startmjd + sec/86400.0<< " - aborting!!!" << endl;
        currentpolyco = Polyco::getCurrentPolyco(currentslot->configindex, startmjd, sec/86400.0, polycos, numpolycos, true);
	MPI_Abort(MPI_COMM_WORLD, 1);
      }
      currentpolyco->setTime(startmjd, sec/86400.0);
    }

    //if necessary, allocate/reallocate space for the STAs
    nowdumpingsta = config->dumpSTA() || config->dumpKurtosis();
    scratchspace->dumpsta = config->dumpSTA();
    scratchspace->dumpkurtosis = config->dumpKurtosis();
    if(nowdumpingsta != dumpingsta) {
      if (scratchspace->starecord != 0) {
        delete scratchspace->starecord;
        scratchspace->starecord = 0;
      }
      if(nowdumpingsta) {
        stadumpchannels = config->getSTADumpChannels();
        scratchspace->starecord = (DifxMessageSTARecord*)malloc(sizeof(DifxMessageSTARecord) + sizeof(f32)*stadumpchannels);
        scratchspace->starecord->coreindex = mpiid - (config->getNumDataStreams()+1);
        scratchspace->starecord->threadindex = threadid;
        scratchspace->starecord->nChan = stadumpchannels;
        sprintf(scratchspace->starecord->identifier, "%s", config->getJobName().substr(0,DIFX_MESSAGE_PARAM_LENGTH-1).c_str());
      }
      dumpingsta = nowdumpingsta;
    }

    //process our section of responsibility for this time range
    processdata(numprocessed++ % RECEIVE_RING_LENGTH, threadid, startblock, numblocks, modes, currentpolyco, scratchspace);

    if(threadid == 0)
      numcomplete++;

    currentslot = &(procslots[numprocessed%RECEIVE_RING_LENGTH]);
    //if the configuration changes from this segment to the next, change our setup accordingly
    if(currentslot->configindex != lastconfigindex)
    {
      cinfo << startl << "Core " << mpiid << " threadid " << threadid << ": changing config to " << currentslot->configindex << endl;
      updateconfig(lastconfigindex, currentslot->configindex, threadid, startblock, numblocks, numpolycos, pulsarbin, modes, polycos, false);
      cinfo << startl << "Core " << mpiid << " threadid " << threadid << ": config changed successfully - pulsarbin is now " << pulsarbin << endl;
      createPulsarVaryingSpace(scratchspace->pulsaraccumspace, &(scratchspace->bins), currentslot->configindex, lastconfigindex, threadid);
      allocateConfigSpecificThreadArrays(scratchspace->baselineweight, currentslot->configindex, lastconfigindex, threadid);
      lastconfigindex = currentslot->configindex;
    }
  }

  //fallen out of loop, so must be finished.  Unlock held mutex
//  cinfo << startl << "PROCESS " << mpiid << "/" << threadid << " process thread about to free resources and exit" << endl;
  perr = pthread_mutex_unlock(&(procslots[numprocessed % RECEIVE_RING_LENGTH].slotlocks[threadid]));
  if (perr != 0)
    csevere << startl << "PROCESSTHREAD " << mpiid << "/" << threadid << " error trying unlock mutex " << (numprocessed)%RECEIVE_RING_LENGTH << endl;

  //free resources
  for(int j=0;j<numdatastreams;j++)
    delete modes[j];
  delete [] modes;
  if(somepulsarbin)
  {
    if(threadid > 0 && pulsarbin)
    {
      for(int i=0;i<numpolycos;i++)
        delete polycos[i];
    }
    delete [] polycos;
    vectorFree(scratchspace->pulsarscratchspace);
    createPulsarVaryingSpace(scratchspace->pulsaraccumspace, &(scratchspace->bins), -1,
    procslots[(numprocessed+1)%RECEIVE_RING_LENGTH].configindex, threadid);
    if(somescrunch)
    {
      delete [] scratchspace->pulsaraccumspace;
    }
  }
  for(int i=0;i<numdatastreams;i++)
    delete [] scratchspace->dsweights[i];
  delete [] scratchspace->dsweights;
  vectorFree(scratchspace->threadcrosscorrs);
  vectorFree(scratchspace->chanfreqs);
  vectorFree(scratchspace->rotator);
  vectorFree(scratchspace->rotated);
  vectorFree(scratchspace->channelsums);
  vectorFree(scratchspace->argument);
  if(scratchspace->starecord != 0) {
    delete scratchspace->starecord;
  }
  delete scratchspace;

  cinfo << startl << "PROCESS " << mpiid << "/" << threadid << " process thread exiting!!!" << endl;
}

int Core::receivedata(int index, bool * terminate)
{
  MPI_Status mpistatus;
  int perr;

  if(*terminate)
    return 0; //don't try to read, we've already finished

  //Get the instructions on the time offset from the FxManager node
  MPI_Recv(&(procslots[index].offsets), 3, MPI_INT, fxcorr::MANAGERID, MPI_ANY_TAG, return_comm, &mpistatus);
  if(mpistatus.MPI_TAG == CR_TERMINATE)
  {
    *terminate = true;
//    cinfo << startl << "Core " << mpiid << " has received a terminate signal!!!" << endl;
    procslots[index].keepprocessing = false;
    return 0; //note return here!!!
  }

  //work out if the source has changed, and if so, whether we need to change the modes and baselines
  currentconfigindex = config->getScanConfigIndex(procslots[index].offsets[0]);
  if(procslots[index].configindex != currentconfigindex)
  {
    cinfo << startl << "Config has changed! Old config: " << procslots[index].configindex << ", new config " << currentconfigindex << endl;
    procslots[index].configindex = currentconfigindex;
    if(procslots[index].configindex < 0)
    {
      cfatal << startl << "Core received a request to process data from scan " << procslots[index].offsets[0] << " which does not have a config - aborting!!!" << endl;
      MPI_Abort(MPI_COMM_WORLD, 1);
    }
    procslots[index].threadresultlength = config->getThreadResultLength(currentconfigindex);
    procslots[index].coreresultlength = config->getCoreResultLength(currentconfigindex);
    procslots[index].numpulsarbins = config->getNumPulsarBins(currentconfigindex);
    procslots[index].scrunchoutput = config->scrunchOutputOn(currentconfigindex);
    procslots[index].pulsarbin = config->pulsarBinOn(currentconfigindex);
  }

  //now grab the data and delay info from the individual datastreams
  for(int i=0;i<numdatastreams;i++)
  {
    //cinfo << startl << "Core is about to post receive request for datastream " << i << endl;
    //get data
    MPI_Irecv(procslots[index].databuffer[i], databytes, MPI_UNSIGNED_CHAR, datastreamids[i], CR_PROCESSDATA, MPI_COMM_WORLD, &datarequests[i]);
    //also receive the offsets and rates
    MPI_Irecv(procslots[index].controlbuffer[i], controllength, MPI_INT, datastreamids[i], CR_PROCESSCONTROL, MPI_COMM_WORLD, &controlrequests[i]);
  }

  //wait for everything to arrive, store the length of the messages
  MPI_Waitall(numdatastreams, datarequests, msgstatuses);
  for(int i=0;i<numdatastreams;i++)
    MPI_Get_count(&(msgstatuses[i]), MPI_UNSIGNED_CHAR, &(procslots[index].datalengthbytes[i]));
  MPI_Waitall(numdatastreams, controlrequests, msgstatuses);

  //lock the next slot, unlock the one we just finished with
  for(int i=0;i<numprocessthreads;i++)
  {
    perr = pthread_mutex_lock(&(procslots[(index+1)%RECEIVE_RING_LENGTH].slotlocks[i]));
    if(perr != 0)
      csevere << startl << "CORE " << mpiid << " error trying lock mutex " << (index+1)%RECEIVE_RING_LENGTH << endl;
  }

  for(int i=0;i<numprocessthreads;i++)
  {
    perr = pthread_mutex_unlock(&(procslots[index].slotlocks[i]));
    if(perr != 0)
      csevere << startl << "CORE " << mpiid << " error trying unlock mutex " << index << endl;
  }

  return 1;
}

void Core::processdata(int index, int threadid, int startblock, int numblocks, Mode ** modes, Polyco * currentpolyco, threadscratchspace * scratchspace)
{
  int status, perr, i, numfftloops, numfftsprocessed;
  int resultindex, cindex, ds1index, ds2index, binloop;
  int xcblockcount, maxxcblocks, xcshiftcount;
  int acblockcount, maxacblocks, acshiftcount;
  int freqchannels;
  int xmacstridelength, xmacpasses, xmacstart, destbin, destchan, localfreqindex;
  int outputoffset, input1offset, input2offset, xmacmullength;
  int dsfreqindex;
  char papol;
  double offsetmins, blockns;
  f32 bweight;
  Mode * m1, * m2;
  cf32 * vis1;
  cf32 * vis2;
  uint64_t offsetsamples;
  double sampletimens;
  int starttimens;
  int fftsize;

//following statement used to cut all all processing for "Neutered DiFX"
#ifndef NEUTERED_DIFX
  xmacstridelength = config->getXmacStrideLength(procslots[index].configindex);
  binloop = 1;
  if(procslots[index].pulsarbin && !procslots[index].scrunchoutput)
    binloop = procslots[index].numpulsarbins;

  //set up the mode objects that will do the station-based processing
  for(int j=0;j<numdatastreams;j++)
  {
    //zero the autocorrelations and set delays
    modes[j]->zeroAutocorrelations();
    modes[j]->setValidFlags(&(procslots[index].controlbuffer[j][3]));
    modes[j]->setData(procslots[index].databuffer[j], procslots[index].datalengthbytes[j], procslots[index].controlbuffer[j][0], procslots[index].controlbuffer[j][1], procslots[index].controlbuffer[j][2]);
    modes[j]->setOffsets(procslots[index].offsets[0], procslots[index].offsets[1], procslots[index].offsets[2]);
    modes[j]->setDumpKurtosis(scratchspace->dumpkurtosis);
    if(scratchspace->dumpkurtosis)
      modes[j]->zeroKurtosis();
    
    //reset pcal
    if(config->getDPhaseCalIntervalMHz(procslots[index].configindex, j) > 0)
    {
      // Calculate the sample time. Every band has the same bandwidth.
      sampletimens = 1.0/(2.0*config->getDRecordedBandwidth(procslots[index].configindex, j, 0))*1e+3;
      
      // Get the ns start time of the whole block.
      starttimens = procslots[index].offsets[2];
      
      // Calculate the FFT size in number of samples.
      fftsize = 2*config->getFNumChannels(config->getDRecordedFreqIndex(procslots[index].configindex, j, 0));
      
      // Calculate the number of offset samples. The modulo PCal bins is done in the pcal object!
      offsetsamples = starttimens/sampletimens + startblock*fftsize;
      modes[j]->resetpcal();
    }
  }

  //zero the results for this thread
  status = vectorZero_cf32(scratchspace->threadcrosscorrs, procslots[index].threadresultlength);
  if(status != vecNoErr)
    csevere << startl << "Error trying to zero threadcrosscorrs!!!" << endl;

  //zero the baselineweights for this thread
  for(int i=0;i<config->getFreqTableLength();i++)
  {
    if(config->isFrequencyUsed(procslots[index].configindex, i))
    {
      for(int b=0;b<binloop;b++)
      {
        for(int j=0;j<numbaselines;j++)
        {
          localfreqindex = config->getBLocalFreqIndex(procslots[index].configindex, j, i);
          if(localfreqindex >= 0)
          {
            status = vectorZero_f32(scratchspace->baselineweight[i][b][j], config->getBNumPolProducts(procslots[index].configindex, j, localfreqindex));
            if(status != vecNoErr)
              csevere << startl << "Error trying to zero baselineweight[" << i << "][" << b << "][" << j << "]!!!" << endl;
          }
        }
      }
    }
  }

  //set up variables which control the number of loops through buffered FFT results
  xcblockcount = 0;
  xcshiftcount = 0;
  acblockcount = 0;
  acshiftcount = 0;
  numfftloops = numblocks/config->getNumBufferedFFTs(procslots[index].configindex);
  if(numblocks%config->getNumBufferedFFTs(procslots[index].configindex) != 0)
    numfftloops++;
  blockns = ((double)(config->getSubintNS(procslots[index].configindex)))/((double)(config->getBlocksPerSend(procslots[index].configindex)));

  maxxcblocks = ((int)(model->getMaxNSBetweenXCAvg(procslots[index].offsets[0])/blockns));
  maxxcblocks -= maxxcblocks%config->getNumBufferedFFTs(procslots[index].configindex);
  if(maxxcblocks == 0) {
    maxxcblocks = config->getNumBufferedFFTs(procslots[index].configindex);
    cwarn << startl << "Requested cross-correlation shift/average time of " << model->getMaxNSBetweenXCAvg(procslots[index].offsets[0]) << " ns cannot be met with " << config->getNumBufferedFFTs(procslots[index].configindex) << " FFTs being buffered - the time resolution which will be attained is " << maxxcblocks*blockns << " ns" << endl;
  }

  maxacblocks = ((int)(model->getMaxNSBetweenACAvg(procslots[index].offsets[0])/blockns));
  maxacblocks -= maxacblocks%config->getNumBufferedFFTs(procslots[index].configindex);
  if(maxacblocks == 0) {
    maxacblocks = config->getNumBufferedFFTs(procslots[index].configindex);
    cwarn << startl << "Requested cross-correlation shift/average time of " << model->getMaxNSBetweenACAvg(procslots[index].offsets[0]) << " ns cannot be met with " << config->getNumBufferedFFTs(procslots[index].configindex) << " FFTs being buffered - the time resolution which will be attained is " << maxacblocks*blockns << " ns" << endl;
  }

  //process each chunk of FFTs in turn
  for(int fftloop=0;fftloop<numfftloops;fftloop++)
  {
    //do the station-based processing for this batch of FFT chunks
    for(int j=0;j<numdatastreams;j++)
    {
      numfftsprocessed = 0;
      for(int fftsubloop=0;fftsubloop<config->getNumBufferedFFTs(procslots[index].configindex);fftsubloop++)
      {
        i = fftloop*config->getNumBufferedFFTs(procslots[index].configindex) + fftsubloop + startblock;
	if(i >= startblock+numblocks)
	  break; //may not have to fully complete last fftloop
        scratchspace->dsweights[j][fftsubloop] = modes[j]->process(i, fftsubloop);
        numfftsprocessed++;
      }
    }

    //if necessary, work out the pulsar bins
    if(procslots[index].pulsarbin)
    {
      for(int fftsubloop=0;fftsubloop<config->getNumBufferedFFTs(procslots[index].configindex); fftsubloop++)
      {
        i = fftloop*config->getNumBufferedFFTs(procslots[index].configindex) + fftsubloop + startblock;
        offsetmins = ((double)i)*blockns/60000000000.0;
        currentpolyco->getBins(offsetmins, scratchspace->bins[fftsubloop]);
      }
    }

    //do the baseline-based processing for this batch of FFT chunks
    resultindex = 0;
    for(int f=0;f<config->getFreqTableLength();f++)
    {
      if(config->phasedArrayOn(procslots[index].configindex)) //phased array processing
      {
        freqchannels = config->getFNumChannels(procslots[index].configindex);
        for(int j=0;j<config->getFPhasedArrayNumPols(procslots[index].configindex, f);j++)
        {
          papol = config->getFPhaseArrayPol(procslots[index].configindex, f, j);

          //weight and add the results for each baseline
          for(int fftsubloop=0;fftsubloop<config->getNumBufferedFFTs(procslots[index].configindex);fftsubloop++)
          {
            for(int k=0;k<numdatastreams;k++)
            {
              vis1 = 0;
              for(int l=0;l<config->getDNumRecordedBands(procslots[index].configindex, k);l++)
              {
                dsfreqindex = config->getDRecordedFreqIndex(procslots[index].configindex, k, l);
                if(dsfreqindex == f && config->getDRecordedBandPol(procslots[index].configindex, k, l) == papol) {
                  vis1 = modes[k]->getFreqs(l, fftsubloop);
                  break;
                }
              }
              if(vis1 == 0)
              {
                for(int l=0;l<config->getDNumZoomBands(procslots[index].configindex, k);l++)
                {
                  dsfreqindex = config->getDZoomFreqIndex(procslots[index].configindex, k, l);
                  if(dsfreqindex == f && config->getDZoomBandPol(procslots[index].configindex, k, l) == papol) {
                    vis1 = modes[k]->getFreqs(config->getDNumRecordedBands(procslots[index].configindex, k) + l, fftsubloop);
                    break;
                  }
                }
              }
              if(vis1 != 0)
              {
                //weight the data
                status = vectorMulC_f32((f32*)vis1, config->getFPhasedArrayDWeight(procslots[index].configindex, f, k), (f32*)scratchspace->rotated, freqchannels*2);
                if(status != vecNoErr)
                  cerror << startl << "Error trying to scale phased array results!" << endl;

                //add it to the result
                status = vectorAdd_cf32_I(scratchspace->rotated, &(scratchspace->threadcrosscorrs[resultindex]), freqchannels);
                if(status != vecNoErr)
                  cerror << startl << "Error trying to add phased array results!" << endl;
              }
            }
          }
          resultindex += freqchannels;
        }
      }
      else if(config->isFrequencyUsed(procslots[index].configindex, f)) //normal processing
      {
        //All baseline freq indices into the freq table are determined by the *first* datastream
        //in the event of correlating USB with LSB data.  Hence all Nyquist offsets/channels etc
        //are determined by the freq corresponding to the *first* datastream
        freqchannels = config->getFNumChannels(f);
        xmacpasses = config->getNumXmacStrides(procslots[index].configindex, f);
        for(int x=0;x<xmacpasses;x++)
        {
          xmacstart = x*xmacstridelength;

          //do the cross multiplication - gets messy for the pulsar binning
          for(int j=0;j<numbaselines;j++)
          {
            //get the localfreqindex for this frequency
            localfreqindex = config->getBLocalFreqIndex(procslots[index].configindex, j, f);
            if(localfreqindex >= 0)
            {
              outputoffset = 0;
              input1offset = 0;
              input2offset = 0;
              xmacmullength = xmacstridelength;
              //check for the annoying case of correlating USB with LSB
              if(config->anyUsbXLsb(procslots[index].configindex))
              {
                if(config->getBFreqOddLSB(procslots[index].configindex, j, localfreqindex) > 0)
                {
                  //uh-oh.  Need to move any LSB datastreams' FFT results appropriately
                  if(config->getBFreqOddLSB(procslots[index].configindex, j, localfreqindex) > 0)
                  {
                    outputoffset = 1;
                    if(x == xmacpasses-1)
                      xmacmullength = xmacstridelength-1;
                    if(config->getBFreqOddLSB(procslots[index].configindex, j, localfreqindex) == 1)
                      //just the first is LSB
                      input2offset = 1;
                    else if(config->getBFreqOddLSB(procslots[index].configindex, j, localfreqindex) == 2)
                      //just the second is LSB
                      input1offset = 1;
                  }
                }
              }

              //get the two modes that contribute to this baseline
              ds1index = config->getBOrderedDataStream1Index(procslots[index].configindex, j);
	      ds2index = config->getBOrderedDataStream2Index(procslots[index].configindex, j);
	      m1 = modes[ds1index];
	      m2 = modes[ds2index];

              //do the baseline-based processing for this batch of FFT chunks
              for(int fftsubloop=0;fftsubloop<config->getNumBufferedFFTs(procslots[index].configindex);fftsubloop++)
              {
	        i = fftloop*config->getNumBufferedFFTs(procslots[index].configindex) + fftsubloop + startblock;
	        if(i >= startblock+numblocks)
		  break; //may not have to fully complete last fftloop

                //add the desired results into the resultsbuffer, for each polarisation pair [and pulsar bin]
                //loop through each polarisation for this frequency
                for(int p=0;p<config->getBNumPolProducts(procslots[index].configindex,j,localfreqindex);p++)
                {
                  //get the appropriate arrays to multiply
                  vis1 = &(m1->getFreqs(config->getBDataStream1BandIndex(procslots[index].configindex, j, localfreqindex, p), fftsubloop)[xmacstart+input1offset]);
                  vis2 = &(m2->getConjugatedFreqs(config->getBDataStream2BandIndex(procslots[index].configindex, j, localfreqindex, p), fftsubloop)[xmacstart+input2offset]);

                  if(procslots[index].pulsarbin)
                  {
                    //multiply into scratch space
                    status = vectorMul_cf32(vis1, vis2, scratchspace->pulsarscratchspace, xmacmullength);
                    if(status != vecNoErr)
                      csevere << startl << "Error trying to xmac baseline " << j << " frequency " << localfreqindex << " polarisation product " << p << ", status " << status << endl;

                    //if scrunching, add into temp accumulate space, otherwise add into normal space
                    if(procslots[index].scrunchoutput)
                    {
                      destchan = xmacstart+outputoffset;
                      for(int l=0;l<xmacmullength;l++)
                      {
                        //the first zero (the source slot) is because we are limiting to one pulsar ephemeris for now
                        destbin = scratchspace->bins[fftsubloop][f][destchan];
                        scratchspace->pulsaraccumspace[f][x][j][0][p][destbin][destchan].re += scratchspace->pulsarscratchspace[l].re;
                        scratchspace->pulsaraccumspace[f][x][j][0][p][destbin][destchan].im += scratchspace->pulsarscratchspace[l].im;
                        destchan++;
                      }
                    }
                    else
                    {
                      bweight = scratchspace->dsweights[ds1index][fftsubloop]*scratchspace->dsweights[ds2index][fftsubloop]/(freqchannels);
                      destchan = xmacstart+outputoffset;
                      for(int l=0;l<xmacmullength;l++)
                      {
                        destbin = scratchspace->bins[fftsubloop][f][destchan];
                        //cindex = resultindex + (scratchspace->bins[freqindex][destchan]*config->getBNumPolProducts(procslots[index].configindex,j,localfreqindex) + p)*(freqchannels+1) + destchan;
                        cindex = resultindex + (destbin*config->getBNumPolProducts(procslots[index].configindex,j,localfreqindex) + p)*xmacstridelength + destchan;
                        scratchspace->threadcrosscorrs[cindex].re += scratchspace->pulsarscratchspace[l].re;
                        scratchspace->threadcrosscorrs[cindex].im += scratchspace->pulsarscratchspace[l].im;
                        scratchspace->baselineweight[f][destbin][j][p] += bweight;
                        destchan++;
                      }
                    }
                  }
                  else
                  {
                    //not pulsar binning, so this is nice and simple - just cross multiply accumulate
                    status = vectorAddProduct_cf32(vis1, vis2, &(scratchspace->threadcrosscorrs[resultindex+outputoffset+p*xmacstridelength]), xmacmullength);

                    if(status != vecNoErr)
                      csevere << startl << "Error trying to xmac baseline " << j << " frequency " << localfreqindex << " polarisation product " << p << ", status " << status << endl;
                  }
                }
              }
	      if(procslots[index].pulsarbin && !procslots[index].scrunchoutput)
	        resultindex += config->getBNumPolProducts(procslots[index].configindex,j,localfreqindex)*  procslots[index].numpulsarbins*xmacstridelength;
              else
	        resultindex += config->getBNumPolProducts(procslots[index].configindex,j,localfreqindex)*xmacstridelength;
            }
          }
        }
      }
    }

    xcblockcount += numfftsprocessed;
    if(xcblockcount == maxxcblocks)
    {
      //shift/average and then lock results and copy data
      uvshiftAndAverage(index, threadid, (startblock+xcshiftcount*maxxcblocks+((double)maxxcblocks)/2.0)*blockns, currentpolyco, scratchspace);
      //reset the xcblockcount, increment xcshiftcount
      xcblockcount = 0;
      xcshiftcount++;
    }
    acblockcount += config->getNumBufferedFFTs(procslots[index].configindex);
    if(acblockcount == maxacblocks)
    {
      //shift/average and then lock results and copy data
      averageAndSendAutocorrs(index, threadid, (startblock+acshiftcount*maxacblocks+((double)maxacblocks)/2.0)*blockns, maxacblocks*blockns, modes, scratchspace);
      //reset the acblockcount, increment acshiftcount, zero the autocorrelations
      acblockcount = 0;
      acshiftcount++;
      for(int j=0;j<numdatastreams;j++)
        modes[j]->zeroAutocorrelations();
    }

    //finally, update the baselineweight if not doing any pulsar stuff or if scrunching
    if(!procslots[index].pulsarbin || procslots[index].scrunchoutput)
    {
      for(int fftsubloop=0;fftsubloop<config->getNumBufferedFFTs(procslots[index].configindex);fftsubloop++)
      {
        i = fftloop*config->getNumBufferedFFTs(procslots[index].configindex) + fftsubloop + startblock;
        if(i >= startblock+numblocks)
          break; //may not have to fully complete last fftloop
        for(int f=0;f<config->getFreqTableLength();f++)
        {
          if(config->isFrequencyUsed(procslots[index].configindex, f))
          {
            for(int j=0;j<numbaselines;j++)
	    {
	      localfreqindex = config->getBLocalFreqIndex(procslots[index].configindex, j, f);
	      if(localfreqindex >= 0)
	      {
	        ds1index = config->getBOrderedDataStream1Index(procslots[index].configindex, j);
	        ds2index = config->getBOrderedDataStream2Index(procslots[index].configindex, j);
	        bweight = scratchspace->dsweights[ds1index][fftsubloop]*scratchspace->dsweights[ds2index][fftsubloop];
	        for(int p=0;p<config->getBNumPolProducts(procslots[index].configindex,j,localfreqindex);p++)
		{
		  scratchspace->baselineweight[f][0][j][p] += bweight;
		}
	      }
	    }
	  }
        }
      }
    }
  }

  if(xcblockcount != 0) {
    uvshiftAndAverage(index, threadid, (startblock+xcshiftcount*maxxcblocks+((double)xcblockcount)/2.0)*blockns, currentpolyco, scratchspace);
  }
  if(acblockcount != 0) {
    averageAndSendAutocorrs(index, threadid, (startblock+acshiftcount*maxacblocks+((double)acblockcount)/2.0)*blockns, acblockcount*blockns, modes, scratchspace);
  }
  if(scratchspace->dumpkurtosis) {
    averageAndSendKurtosis(index, threadid, (startblock + numblocks)*blockns/2.0, numblocks*blockns, numblocks, modes, scratchspace);
  }

  //lock the bweight copylock, so we're the only one adding to the result array (baseline weight section)
  perr = pthread_mutex_lock(&(procslots[index].bweightcopylock));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << mpiid << "/" << threadid << " error trying lock bweight copy mutex!!!" << endl;

  for(int f=0;f<config->getFreqTableLength();f++)
  {
    if(config->isFrequencyUsed(procslots[index].configindex, f))
    {
      for(int i=0;i<numbaselines;i++)
      {
        localfreqindex = config->getBLocalFreqIndex(procslots[index].configindex, i, f);
        if(localfreqindex >= 0)
        {
          resultindex = config->getCoreResultBWeightOffset(procslots[index].configindex, f, i)*2;
          for(int b=0;b<binloop;b++)
          {
            for(int j=0;j<config->getBNumPolProducts(procslots[index].configindex,i,localfreqindex);j++)
            {
              procslots[index].floatresults[resultindex] += scratchspace->baselineweight[f][b][i][j];
              resultindex++;
            }
          }
        }
      }
    }
  }

  //unlock the bweight copylock
  perr = pthread_mutex_unlock(&(procslots[index].bweightcopylock));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << mpiid << "/" << threadid << " error trying unlock copy mutex!!!" << endl;

  //copy the PCal results
  copyPCalTones(index, threadid, modes);

//end the cutout of processing in "Neutered DiFX"
#endif

  //grab the next slot lock
  perr = pthread_mutex_lock(&(procslots[(index+1)%RECEIVE_RING_LENGTH].slotlocks[threadid]));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << mpiid << "/" << threadid << " error trying lock mutex " << (index+1)%RECEIVE_RING_LENGTH << endl;

  //unlock the one we had
  perr = pthread_mutex_unlock(&(procslots[index].slotlocks[threadid]));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << mpiid << "/" << threadid << " error trying unlock mutex " << index << endl;
}

void Core::copyPCalTones(int index, int threadid, Mode ** modes)
{
  int resultindex, localfreqindex, perr;
  cf32 pcal;

  //lock the pcal copylock, so we're the only one adding to the result array (pcal section)
  perr = pthread_mutex_lock(&(procslots[index].pcalcopylock));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << mpiid << "/" << threadid << " error trying lock pcal copy mutex!!!" << endl;

  //copy the pulse cal
  for(int i=0;i<numdatastreams;i++)
  {
    if(config->getDPhaseCalIntervalMHz(procslots[index].configindex, i) > 0)
    {
      modes[i]->finalisepcal();
      resultindex = config->getCoreResultPCalOffset(procslots[index].configindex, i);
      for(int j=0;j<config->getDNumRecordedBands(procslots[index].configindex, i);j++)
      {
        localfreqindex = config->getDLocalRecordedFreqIndex(procslots[index].configindex, i, j);
        for(int k=0;k<config->getDRecordedFreqNumPCalTones(procslots[index].configindex, i, localfreqindex);k++)
        {
          procslots[index].results[resultindex].re += modes[i]->getPcal(j,k).re;
          procslots[index].results[resultindex].im += modes[i]->getPcal(j,k).im;
	  resultindex++;
        }
      }
    }
  }

  //unlock the thread pcal copylock
  perr = pthread_mutex_unlock(&(procslots[index].pcalcopylock));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << mpiid << "/" << threadid << " error trying unlock pcal copy mutex!!!" << endl;
}

void Core::averageAndSendAutocorrs(int index, int threadid, double nsoffset, double nswidth, Mode ** modes, threadscratchspace * scratchspace)
{
  int maxproducts, resultindex, perr, status;
  int freqindex, parentfreqindex, channelinc, freqchannels;
  bool datastreamsaveraged, writecrossautocorrs;
  f32 * acdata;

  datastreamsaveraged = false;
  writecrossautocorrs = modes[0]->writeCrossAutoCorrs();
  maxproducts = config->getMaxProducts();

  //if STA send needed but we can average datastream results in freq first, do so
  if(scratchspace->dumpsta && config->getMinPostAvFreqChannels(procslots[index].configindex) >= config->getSTADumpChannels())
  {
    for(int i=0;i<numdatastreams;i++) {
      modes[i]->averageFrequency();
    }
    datastreamsaveraged = true;
  }

  //if required, send off a message with the STA results
  //(before averaging in case high spectral resolution for the dump is required)
  if(scratchspace->dumpsta) {
    scratchspace->starecord->messageType = STA_AUTOCORRELATION;
    scratchspace->starecord->scan = procslots[index].offsets[0];
    scratchspace->starecord->sec = model->getScanStartSec(procslots[index].offsets[0], startmjd, startseconds) + procslots[index].offsets[1];
    scratchspace->starecord->ns = procslots[index].offsets[2] + int(nsoffset);
    if(scratchspace->starecord->ns >= 1000000000) {
      scratchspace->starecord->ns -= 1000000000;
      scratchspace->starecord->sec++;
    }
    scratchspace->starecord->nswidth = int(nswidth);
    for (int i=0;i<numdatastreams;i++) {
      scratchspace->starecord->dsindex = i;
      for (int j=0;j<config->getDNumRecordedBands(procslots[index].configindex, i);j++) {
        scratchspace->starecord->nChan = config->getSTADumpChannels();
        freqindex = config->getDRecordedFreqIndex(procslots[index].configindex, i, j);
        freqchannels = config->getFNumChannels(freqindex);
        if(datastreamsaveraged)
          freqchannels /= config->getFChannelsToAverage(freqindex);
        if (freqchannels < scratchspace->starecord->nChan)
          scratchspace->starecord->nChan = freqchannels;
        channelinc = freqchannels/scratchspace->starecord->nChan;
        scratchspace->starecord->bandindex = j;
        acdata = (f32*)(modes[i]->getAutocorrelation(false, j));
        for (int k=0;k<scratchspace->starecord->nChan;k++) {
          scratchspace->starecord->data[k] = acdata[2*k*channelinc];
          for (int l=1;l<channelinc;l++)
            scratchspace->starecord->data[k] += acdata[2*(k*channelinc+l)];
        }
        //cout << "About to send the binary message" << endl;
        difxMessageSendBinary((const char *)(scratchspace->starecord), BINARY_STA, sizeof(DifxMessageSTARecord) + sizeof(f32)*scratchspace->starecord->nChan);
      }
    }
    //cout << "Finished doing some STA stuff" << endl;
  }

  //if required, average the datastreams down in frequency
  if(!datastreamsaveraged) {
    for(int i=0;i<numdatastreams;i++) {
      modes[i]->averageFrequency();
    }
  }

  //lock the autocorr copylock, so we're the only one adding to the result array (datastream section)
  perr = pthread_mutex_lock(&(procslots[index].autocorrcopylock));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << mpiid << "/" << threadid << " error trying lock autocorr copy mutex!!!" << endl;

  //copy the autocorrelations
  for(int j=0;j<numdatastreams;j++)
  {
    resultindex = config->getCoreResultAutocorrOffset(procslots[index].configindex, j);
    for(int k=0;k<config->getDNumTotalBands(procslots[index].configindex, j);k++)
    {
      freqindex = config->getDTotalFreqIndex(procslots[index].configindex, j, k);
      if(config->isFrequencyUsed(procslots[index].configindex, freqindex)) {
        freqchannels = config->getFNumChannels(freqindex)/config->getFChannelsToAverage(freqindex);
        //put autocorrs in resultsbuffer
        status = vectorAdd_cf32_I(modes[j]->getAutocorrelation(false, k), &procslots[index].results[resultindex], freqchannels);
        if(status != vecNoErr)
          csevere << startl << "Error copying autocorrelations for datastream " << j << ", band " << k << endl;
        resultindex += freqchannels;
      }
    }
    if(writecrossautocorrs && maxproducts > 2) //want the cross-polarisation autocorrs as well
    {
      for(int k=0;k<config->getDNumTotalBands(procslots[index].configindex, j);k++)
      {
        freqindex = config->getDTotalFreqIndex(procslots[index].configindex, j, k);
        if(config->isFrequencyUsed(procslots[index].configindex, freqindex)) {
          freqchannels = config->getFNumChannels(freqindex)/config->getFChannelsToAverage(freqindex);
          status = vectorAdd_cf32_I(modes[j]->getAutocorrelation(true, k), &procslots[index].results[resultindex], freqchannels);
          if(status != vecNoErr)
            csevere << startl << "Error copying cross-polar autocorrelations for datastream " << j << ", band " << k << endl;
          resultindex += freqchannels;
        }
      }
    }
  }

  //unlock the thread autocorr copylock
  perr = pthread_mutex_unlock(&(procslots[index].autocorrcopylock));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << mpiid << "/" << threadid << " error trying unlock autocorr copy mutex!!!" << endl;

  //lock the acweight copylock, so we're the only one adding to the result array (autocorr weight section)
  perr = pthread_mutex_lock(&(procslots[index].acweightcopylock));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << mpiid << "/" << threadid << " error trying lock acweight copy mutex!!!" << endl;

  //copy the autocorr weights
  for(int j=0;j<numdatastreams;j++)
  {
    resultindex = config->getCoreResultACWeightOffset(procslots[index].configindex, j)*2;
    for(int k=0;k<config->getDNumTotalBands(procslots[index].configindex, j);k++)
    {
      freqindex = config->getDTotalFreqIndex(procslots[index].configindex, j, k);
      if(config->isFrequencyUsed(procslots[index].configindex, freqindex))
      {
        if(k>=config->getDNumRecordedBands(procslots[index].configindex, j))
        {
          //need to get the weight from the parent band
          parentfreqindex = config->getDZoomFreqParentFreqIndex(procslots[index].configindex, j, freqindex);
          for(int l=0;l<config->getDNumRecordedBands(procslots[index].configindex, j);l++) {
            if(config->getDRecordedFreqIndex(procslots[index].configindex, j, l) == parentfreqindex && config->getDZoomBandPol(procslots[index].configindex, j, k-config->getDNumRecordedBands(procslots[index].configindex, j)) == config->getDRecordedBandPol(procslots[index].configindex, j, l)) {
              procslots[index].floatresults[resultindex] += modes[j]->getWeight(false, l);
            }
          }
        }
        else
        {
          procslots[index].floatresults[resultindex] += modes[j]->getWeight(false, k);
        }
        resultindex++;
      }
    }
    if(writecrossautocorrs && maxproducts > 2) //want the cross-polarisation autocorrs as well
    {
      for(int k=0;k<config->getDNumTotalBands(procslots[index].configindex, j);k++)
      {
        freqindex = config->getDTotalFreqIndex(procslots[index].configindex, j, k);
        if(config->isFrequencyUsed(procslots[index].configindex, freqindex)) {
          if(k>=config->getDNumRecordedBands(procslots[index].configindex, j))
          {
            //need to get the weight from the parent band
            parentfreqindex = config->getDZoomFreqParentFreqIndex(procslots[index].configindex, j, freqindex);
            for(int l=0;l<config->getDNumRecordedBands(procslots[index].configindex, j);l++)
            {
              if(config->getDRecordedFreqIndex(procslots[index].configindex, j, l) == parentfreqindex && config->getDZoomBandPol(procslots[index].configindex, j, k-config->getDNumRecordedBands(procslots[index].configindex, j)) == config->getDRecordedBandPol(procslots[index].configindex, j, l))
              {
                procslots[index].floatresults[resultindex] += modes[j]->getWeight(false, l);
              }
            }
          }
          else
          {
            procslots[index].floatresults[resultindex] += modes[j]->getWeight(false, k);
          }
          resultindex++;
        }
      }
    }
  }

  //unlock the acweight copylock
  perr = pthread_mutex_unlock(&(procslots[index].acweightcopylock));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << mpiid << "/" << threadid << " error trying unlock acweight copy mutex!!!" << endl;
}

void Core::averageAndSendKurtosis(int index, int threadid, double nsoffset, double nswidth, int numblocks, Mode ** modes, threadscratchspace * scratchspace)
{
  int status, freqchannels, freqindex;

  //tell the modes to calculate the kurtosis and average it if need be
  for(int i=0;i<numdatastreams;i++) {
    modes[i]->calculateAndAverageKurtosis(numblocks, config->getSTADumpChannels());
  }

  scratchspace->starecord->messageType = STA_KURTOSIS;
  scratchspace->starecord->scan = procslots[index].offsets[0];
  scratchspace->starecord->sec = model->getScanStartSec(procslots[index].offsets[0], startmjd, startseconds) + procslots[index].offsets[1];
  scratchspace->starecord->ns = procslots[index].offsets[2] + int(nsoffset);
  if(scratchspace->starecord->ns >= 1000000000) {
    scratchspace->starecord->ns -= 1000000000;
    scratchspace->starecord->sec++;
  }
  scratchspace->starecord->nswidth = int(nswidth);
  for (int i=0;i<numdatastreams;i++) {
    scratchspace->starecord->dsindex = i;
    for (int j=0;j<config->getDNumRecordedBands(procslots[index].configindex, i);j++) {
      scratchspace->starecord->nChan = config->getSTADumpChannels();
      freqindex = config->getDRecordedFreqIndex(procslots[index].configindex, i, j);
      freqchannels = config->getFNumChannels(freqindex)/config->getFChannelsToAverage(freqindex);
      if (freqchannels < scratchspace->starecord->nChan)
        scratchspace->starecord->nChan = freqchannels;
      scratchspace->starecord->bandindex = j;
      status = vectorCopy_f32(modes[i]->getKurtosis(j), scratchspace->starecord->data, scratchspace->starecord->nChan);
      if(status != vecNoErr)
        cerror << startl << "Problem copying kurtosis results from mode to sta record!" << endl;
      difxMessageSendBinary((const char *)(scratchspace->starecord), BINARY_STA, sizeof(DifxMessageSTARecord) + sizeof(f32)*scratchspace->starecord->nChan);
    }
  }
}

void Core::uvshiftAndAverage(int index, int threadid, double nsoffset, Polyco * currentpolyco, threadscratchspace * scratchspace)
{
  int status, startbaselinefreq, atbaselinefreq, startbaseline, startfreq, endbaseline, binloop;
  int localfreqindex, baselinefreqs;
  int numxmacstrides, xmaclen;

  //first scale the pulsar data if necessary
  if(procslots[index].pulsarbin && procslots[index].scrunchoutput)
  {
    f64 * binweights = currentpolyco->getBinWeights();

    for(int f=0;f<config->getFreqTableLength();f++)
    {
      if(config->isFrequencyUsed(procslots[index].configindex, f))
      {
        numxmacstrides = config->getNumXmacStrides(procslots[index].configindex, f);
        xmaclen = config->getXmacStrideLength(procslots[index].configindex);
        for(int x=0;x<numxmacstrides;x++)
        {
          for(int i=0;i<numbaselines;i++)
          {
            localfreqindex = config->getBLocalFreqIndex(procslots[index].configindex, i, f);
            if(localfreqindex >= 0)
            {
              for(int s=0;s<1;s++) //forced to single pulsar ephemeris for now
              {
                for(int j=0;j<config->getBNumPolProducts(procslots[index].configindex,i,localfreqindex);j++)
                {
                  for(int k=0;k<config->getNumPulsarBins(procslots[index].configindex);k++)
                  {
                    status = vectorMulC_f32_I((f32)(binweights[k]), (f32*)(scratchspace->pulsaraccumspace[f][x][i][s][j][k]), 2*xmaclen);
                    if(status != vecNoErr)
                      csevere << startl << "Error trying to scale for scrunch!!!" << endl;
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  baselinefreqs = 0;
  for(int i=0;i<numbaselines;i++)
    baselinefreqs += config->getBNumFreqs(procslots[index].configindex, i);

  //for(int f=0;f<config->getFreqTableLength();f++)
  //{
  //  if(config->isFrequencyUsed(procslots[index].configindex, f))
  //  {
  //    for(int i=0;i<numbaselines;i++)
  //    {
  //      localfreqindex = config->getBLocalFreqIndex(procslots[index].configindex, i, f);
  //      if(localfreqindex >= 0)
  //        baselinefreqs++;
  //    }
  //  }
  //}

  startbaselinefreq = (threadid*baselinefreqs)/numprocessthreads;
  binloop = 1;
  if(procslots[index].pulsarbin && !procslots[index].scrunchoutput)
    binloop = procslots[index].numpulsarbins;

  atbaselinefreq = 0;
  startfreq = -1;
  startbaseline = -1;
  //skip ahead to the position in threadresults where this thread will start
  for(int f=0;f<config->getFreqTableLength();f++)
  {
    if(startfreq >= 0)
      break;
    if(config->isFrequencyUsed(procslots[index].configindex, f))
    {
      for(int i=0;i<numbaselines;i++)
      {
        localfreqindex = config->getBLocalFreqIndex(procslots[index].configindex, i, f);
        if(localfreqindex >= 0)
        {
          if(atbaselinefreq == startbaselinefreq)
          {
            startfreq = f;
            startbaseline = i;
            break;
          }
          atbaselinefreq++;
        }
      }
    }
  }
  endbaseline = startbaseline;

  //now rotate (if necessary) and average in frequency (if necessary) while copying
  //from threadresults to the slot results
  for(int f=startfreq;f<config->getFreqTableLength();f++)
  {
    if(config->isFrequencyUsed(procslots[index].configindex, f))
    {
      for(int i=startbaseline;i<numbaselines;i++)
      {
        uvshiftAndAverageBaselineFreq(index, threadid, nsoffset, scratchspace, f, i);
      }
      startbaseline = 0;
    }
  }

  //back to the start and do the others we skipped earlier
  for(int f=0;f<startfreq;f++)
  {
    if(config->isFrequencyUsed(procslots[index].configindex, f))
    {
      for(int i=0;i<numbaselines;i++)
      {
        uvshiftAndAverageBaselineFreq(index, threadid, nsoffset, scratchspace, f, i);
      }
    }
  }
  for(int i=0;i<endbaseline;i++)
  {
    uvshiftAndAverageBaselineFreq(index, threadid, nsoffset, scratchspace, startfreq, i);
  }

  //clear the thread cross-corr results
  status = vectorZero_cf32(scratchspace->threadcrosscorrs, procslots[index].threadresultlength);
  if(status != vecNoErr)
    csevere << startl << "Error trying to zero threadcrosscorrs!!!" << endl;

  //clear the pulsar accumulation vector if necessary
  if(procslots[index].pulsarbin && procslots[index].scrunchoutput)
  {
    for(int f=0;f<config->getFreqTableLength();f++)
    {
      if(config->isFrequencyUsed(procslots[index].configindex, f))
      {
        numxmacstrides = config->getNumXmacStrides(procslots[index].configindex, f);
        xmaclen = config->getXmacStrideLength(procslots[index].configindex);
        for(int x=0;x<numxmacstrides;x++)
        {
          for(int i=0;i<numbaselines;i++)
          {
            localfreqindex = config->getBLocalFreqIndex(procslots[index].configindex, i, f);
            if(localfreqindex >= 0)
            {
              for(int s=0;s<1;s++) //forced to single pulsar ephemeris for now
              {
                for(int j=0;j<config->getBNumPolProducts(procslots[index].configindex,i,localfreqindex);j++)
                {
                  for(int k=0;k<config->getNumPulsarBins(procslots[index].configindex);k++)
                  {
                    //zero the accumulation space for next time
                    status = vectorZero_cf32(scratchspace->pulsaraccumspace[f][x][i][s][j][k], xmaclen);
                    if(status != vecNoErr)
                      csevere << startl << "Error trying to zero pulsaraccumspace!!!" << endl;
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

void Core::uvshiftAndAverageBaselineFreq(int index, int threadid, double nsoffset, threadscratchspace * scratchspace, int freqindex, int baseline)
{
  int status, perr, threadbinloop, threadindex, threadstart, numstrides, numaverages;
  int localfreqindex, freqchannels, coreindex, coreoffset, corebinloop, channelinc, rotatorlength, dest;
  int antenna1index, antenna2index;
  int rotatestridelen, rotatesperstride, xmacstridelen, xmaccopylen, stridestoaverage, averagesperstride, averagelength;
  double bandwidth, lofrequency, channelbandwidth, stepbandwidth;
  double applieddelay, applieddelay1, applieddelay2, turns, edgeturns;
  double pointingcentredelay1approx[2];
  double pointingcentredelay2approx[2];
  double * phasecentredelay1;
  double * phasecentredelay2;
  cf32* srcpointer;
  cf32 meanresult;

  localfreqindex = config->getBLocalFreqIndex(procslots[index].configindex, baseline, freqindex);
  xmacstridelen = config->getXmacStrideLength(procslots[index].configindex);
  rotatestridelen = config->getArrayStrideLength(procslots[index].configindex);
  if(rotatestridelen > xmacstridelen)
    rotatestridelen = xmacstridelen;
  threadbinloop = 1;
  corebinloop = 1;
  if(procslots[index].pulsarbin)
    threadbinloop = procslots[index].numpulsarbins;
  if(procslots[index].pulsarbin && !procslots[index].scrunchoutput)
    corebinloop = procslots[index].numpulsarbins;

  //allocate space for the phase centre delays if necessary, and calculate pointing centre delays
  if(model->getNumPhaseCentres(procslots[index].offsets[0]) > 1)
  {
    phasecentredelay1 = new double[model->getNumPhaseCentres(procslots[index].offsets[0])];
    phasecentredelay2 = new double[model->getNumPhaseCentres(procslots[index].offsets[0])];

    antenna1index = config->getDModelFileIndex(procslots[index].configindex, config->getBDataStream1Index(procslots[index].configindex, baseline));
    antenna2index = config->getDModelFileIndex(procslots[index].configindex, config->getBDataStream2Index(procslots[index].configindex, baseline));
    //get the pointing centre interpolator, validity range aribitrarily set to 1us (approximately the tangent)
    model->calculateDelayInterpolator(procslots[index].offsets[0], procslots[index].offsets[1] + double(procslots[index].offsets[2]+nsoffset)/1000000000.0, 0.000001, 1, antenna1index, 0, 1, pointingcentredelay1approx);
    model->calculateDelayInterpolator(procslots[index].offsets[0], procslots[index].offsets[1] + double(procslots[index].offsets[2]+nsoffset)/1000000000.0, 0.000001, 1, antenna2index, 0, 1, pointingcentredelay2approx);
    for(int s=0;s<model->getNumPhaseCentres(procslots[index].offsets[0]);s++)
    {
      model->calculateDelayInterpolator(procslots[index].offsets[0], procslots[index].offsets[1] + double(procslots[index].offsets[2]+nsoffset)/1000000000.0, 0.0, 0, antenna1index, s+1, 0, &(phasecentredelay1[s]));
      model->calculateDelayInterpolator(procslots[index].offsets[0], procslots[index].offsets[1] + double(procslots[index].offsets[2]+nsoffset)/1000000000.0, 0.0, 0, antenna2index, s+1, 0, &(phasecentredelay2[s]));
    }
  }

  //lock the mutex for this segment of the copying
  perr = pthread_mutex_lock(&(procslots[index].viscopylocks[freqindex][baseline]));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << threadid << " error trying lock copy mutex for frequency table entry " << freqindex << ", baseline " << baseline << "!!!" << endl;

  //cinfo << startl << "For index " << index << ", thread " << threadid << " is working on freqindex " << freqindex << ", baseline " << baseline << endl;

  freqchannels = config->getFNumChannels(freqindex);
  channelinc = config->getFChannelsToAverage(freqindex);
  bandwidth = config->getFreqTableBandwidth(freqindex);
  lofrequency = config->getFreqTableFreq(freqindex);
  numaverages = freqchannels/channelinc;
  stridestoaverage = channelinc/xmacstridelen;
  rotatesperstride = xmacstridelen/rotatestridelen;
  if(stridestoaverage == 0)
    stridestoaverage = 1;
  averagesperstride = xmacstridelen/channelinc;
  if(averagesperstride == 0)
    averagesperstride = 1;
  averagelength = xmacstridelen/averagesperstride;
  numstrides = freqchannels/xmacstridelen;
  channelbandwidth = bandwidth/double(freqchannels);
  rotatorlength = rotatestridelen+numstrides*rotatesperstride;
  stepbandwidth = rotatestridelen*channelbandwidth;
  if(model->getNumPhaseCentres(procslots[index].offsets[0]) > 1)
  {
    if(config->getFreqTableLowerSideband(freqindex))
    {
      for(int c=0;c<rotatestridelen;c++)
        scratchspace->chanfreqs[c] = -(rotatestridelen-(c+1))*channelbandwidth;
      for(int c=0;c<numstrides*rotatesperstride;c++)
        scratchspace->chanfreqs[rotatestridelen+c] = -(numstrides*rotatesperstride-(c+1))*stepbandwidth;
    }
    else
    {
      for(int c=0;c<rotatestridelen;c++) {
        scratchspace->chanfreqs[c] = c*channelbandwidth;
      }
      for(int c=0;c<numstrides*rotatesperstride;c++) {
        scratchspace->chanfreqs[rotatestridelen+c] = c*stepbandwidth;
      }
    }
  }

  coreindex = config->getCoreResultBaselineOffset(procslots[index].configindex, freqindex, baseline);
  for(int s=0;s<model->getNumPhaseCentres(procslots[index].offsets[0]);s++)
  {
    if(model->getNumPhaseCentres(procslots[index].offsets[0]) > 1)
    {
      //work out the correct rotator for this frequency and phase centre
      //cout << "phasecentredelay1[" << s << "] is " << phasecentredelay1[s] << endl;
      //cout << "phasecentredelay2[" << s << "] is " << phasecentredelay2[s] << endl;
      //cout << "pointingcentredelay1 is " << pointingcentredelay1 << endl;
      //cout << "pointingcentredelay2 is " << pointingcentredelay2 << endl;
      applieddelay1 = phasecentredelay1[s] - pointingcentredelay1approx[1];
      applieddelay2 = phasecentredelay2[s] - pointingcentredelay2approx[1];
      //make correction for geometric rate over the shifted sample range
      applieddelay1 += applieddelay1*pointingcentredelay1approx[0];
      applieddelay2 += applieddelay2*pointingcentredelay2approx[0];
      //cout << "Applied delay is " << applieddelay2 - applieddelay1 << ", correction is " << applieddelay2 - applieddelay1 - saveddelay << endl;
      applieddelay = applieddelay2 - applieddelay1;
      if(fabs(applieddelay) > 1.0e-20)
      {
        edgeturns = applieddelay*lofrequency;
        edgeturns -= floor(edgeturns);
        for(int r=0;r<rotatestridelen;r++)
        {
          turns = applieddelay*scratchspace->chanfreqs[r] + edgeturns;
          scratchspace->argument[r] = (turns-floor(turns))*TWO_PI;
        }
        for(int r=rotatestridelen;r<rotatorlength;r++)
        {
          turns = applieddelay*scratchspace->chanfreqs[r];
          scratchspace->argument[r] = (turns-floor(turns))*TWO_PI;
        }
        status = vectorSinCos_f32(scratchspace->argument, &(scratchspace->argument[rotatorlength]), &(scratchspace->argument[2*rotatorlength]), rotatorlength);
        if(status != vecNoErr)
          csevere << startl << "Error in phase shift, sin/cos!!!" << status << endl;
        status = vectorRealToComplex_f32(&(scratchspace->argument[2*rotatorlength]), &(scratchspace->argument[rotatorlength]), scratchspace->rotator, rotatorlength);
        if(status != vecNoErr)
          csevere << startl << "Error in phase shift, real to complex!!!" << status << endl;
      }
    }
    threadstart = config->getThreadResultFreqOffset(procslots[index].configindex, freqindex) + config->getThreadResultBaselineOffset(procslots[index].configindex, freqindex, baseline);
    //cout << "Threadstart is " << threadstart << " since threadresultfreqoffset is " << config->getThreadResultFreqOffset(procslots[index].configindex, freqindex) << endl;
    for(int x=0;x<config->getNumXmacStrides(procslots[index].configindex, freqindex);x++)
    {
      threadindex = threadstart+x*config->getCompleteStrideLength(procslots[index].configindex, freqindex);
      for(int b=0;b<threadbinloop;b++)
      {
        for(int k=0;k<config->getBNumPolProducts(procslots[index].configindex,baseline,localfreqindex);k++)
        {
          if(corebinloop > 1)
            coreoffset = ((b*config->getBNumPolProducts(procslots[index].configindex,baseline,localfreqindex)+k)*freqchannels + x*xmacstridelen)/channelinc;
          else
            coreoffset = (k*freqchannels + x*xmacstridelen)/channelinc;
          if(model->getNumPhaseCentres(procslots[index].offsets[0]) > 1 && fabs(applieddelay) > 1.0e-20)
          {
            if(procslots[index].pulsarbin && procslots[index].scrunchoutput)
              srcpointer = scratchspace->pulsaraccumspace[freqindex][x][baseline][0][k][b];
            else
              srcpointer = &(scratchspace->threadcrosscorrs[threadindex]);
            //cout << "About to rotate from threadindex " << threadindex << " (cf max thread result length " << config->getMaxThreadResultLength() << ") for which the 10th offset re value is " << scratchspace->threadcrosscorrs[threadindex+10].re << endl;
            for(int r=0;r<rotatesperstride;r++)
            {
              //atrotate = (r + x*rotatesperstride)*rotatestridelength;
              status = vectorMul_cf32(scratchspace->rotator, &(srcpointer[r*rotatestridelen]), &(scratchspace->rotated[r*rotatestridelen]), rotatestridelen);
              if(status != vecNoErr)
                csevere << startl << "Error in phase shift, multiplication1!!!" << status << endl;
              status = vectorMulC_cf32_I(scratchspace->rotator[rotatestridelen+r+x*rotatesperstride], &(scratchspace->rotated[r*rotatestridelen]), rotatestridelen);
              if(status != vecNoErr)
                csevere << startl << "Error in phase shift, multiplication2!!!" << status << endl;
            }
            srcpointer = scratchspace->rotated;
          }
          else
          {
            if(procslots[index].pulsarbin && procslots[index].scrunchoutput)
              srcpointer = scratchspace->pulsaraccumspace[freqindex][x][baseline][0][k][b];
            else
              srcpointer = &(scratchspace->threadcrosscorrs[threadindex]);
          }

          //now average (or just copy) from the designated pointer to the main result buffer
          if(channelinc == 1) //this frequency is not averaged
          {
            xmaccopylen = xmacstridelen;
            status = vectorAdd_cf32_I(srcpointer, &(procslots[index].results[coreindex+coreoffset]), xmaccopylen);
            if(status != vecNoErr)
              cerror << startl << "Error trying to copy frequency index " << freqindex << ", baseline " << baseline << " when not averaging in frequency" << endl;
          }
          else //this frequency *is* averaged - deal with it
          {
            dest = coreindex+coreoffset;
            for(int l=0;l<averagesperstride;l++)
            {
              //status = vectorMean_cf32(srcpointer + l*channelinc, channelinc, &(scratchspace->channelsums[l]), vecAlgHintFast);
              //cout << "about to average from " << srcpointer[l*averagelength].re << " for length " << averagelength << endl;
              status = vectorMean_cf32(srcpointer + l*averagelength, averagelength, &meanresult, vecAlgHintFast);
              if(status != vecNoErr)
                cerror << startl << "Error trying to average frequency " << freqindex << ", baseline " << baseline << endl;
              procslots[index].results[dest].re += meanresult.re/stridestoaverage;
              procslots[index].results[dest].im += meanresult.im/stridestoaverage;
              dest++;
            }
          }
          threadindex += xmacstridelen;
        }
      }
    }
    coreindex += corebinloop*config->getBNumPolProducts(procslots[index].configindex,baseline,localfreqindex)*freqchannels/channelinc;
  }

  //cinfo << startl << "For index " << index << ", thread " << threadid << " is about to finish freqindex " << freqindex << ", baseline " << baseline << endl;

  //unlock the mutex for this segment of the copying
  perr = pthread_mutex_unlock(&(procslots[index].viscopylocks[freqindex][baseline]));
  if(perr != 0)
    csevere << startl << "PROCESSTHREAD " << threadid << " error trying unlock copy mutex for frequency table entry " << freqindex << ", baseline " << baseline << "!!! Perr is " << perr << endl;

  //free the phasecentredelay vectors if necessary
  if(model->getNumPhaseCentres(procslots[index].offsets[0]) > 1)
  {
    delete [] phasecentredelay1;
    delete [] phasecentredelay2;
  }
}

void Core::createPulsarVaryingSpace(cf32******* pulsaraccumspace, s32**** bins, int newconfigindex, int oldconfigindex, int threadid)
{
  int status, freqchannels, localfreqindex;

  if(oldconfigindex >= 0 && config->pulsarBinOn(oldconfigindex))
  {
    for(int i=0;i<config->getNumBufferedFFTs(oldconfigindex);i++)
    {
      for(int f=0;f<config->getFreqTableLength();f++)
      {
        if(config->isFrequencyUsed(oldconfigindex, f))
	{
	  freqchannels = config->getFNumChannels(f);
	  vectorFree((*bins)[i][f]);
          threadbytes[threadid] -= 4*freqchannels;
	}
      }
      delete [] (*bins)[i];
    }
    delete [] *bins;
    if(config->scrunchOutputOn(oldconfigindex))
    {
      //need to delete the old pulsar accumulation space/bin space
      for(int f=0;f<config->getFreqTableLength();f++)
      {
        if(config->isFrequencyUsed(oldconfigindex, f))
        {
          freqchannels = config->getFNumChannels(f);
	  for(int x=0;x<config->getNumXmacStrides(oldconfigindex, f);x++)
          {
            for(int i=0;i<numbaselines;i++)
            {
              localfreqindex = config->getBLocalFreqIndex(oldconfigindex, i, f);
              if(localfreqindex >= 0)
              {
                for(int s=0;s<1;s++) //forced to single pulsar ephemeris for now
                {
                  for(int j=0;j<config->getBNumPolProducts(oldconfigindex,i,localfreqindex);j++)
                  {
                    for(int k=0;k<config->getNumPulsarBins(oldconfigindex);k++)
                    {
                      threadbytes[threadid] -= 8*freqchannels;
                      vectorFree(pulsaraccumspace[f][x][i][s][j][k]);
                    }
                    delete [] pulsaraccumspace[f][x][i][s][j];
                  }
                  delete [] pulsaraccumspace[f][x][i][s];
                }
              }
              delete [] pulsaraccumspace[f][x][i];
            }
            delete [] pulsaraccumspace[f][x];
          }
          delete [] pulsaraccumspace[f];
	}
      }
    }
  }

  if(newconfigindex >= 0 && config->pulsarBinOn(newconfigindex))
  {
    *bins = new s32**[config->getNumBufferedFFTs(newconfigindex)];
    for(int i=0;i<config->getNumBufferedFFTs(newconfigindex);i++)
    {
      (*bins)[i] = new s32*[config->getFreqTableLength()];
      for(int f=0;f<config->getFreqTableLength();f++)
      {
        if(config->isFrequencyUsed(newconfigindex, f))
	{
	  freqchannels = config->getFNumChannels(f);
          (*bins)[i][f] = vectorAlloc_s32(freqchannels);
	  threadbytes[threadid] += 4*freqchannels;
	}
      }
    }
    if(config->scrunchOutputOn(newconfigindex))
    {
      //need to create a new pulsar accumulation space
      for(int f=0;f<config->getFreqTableLength();f++)
      {
        if(config->isFrequencyUsed(newconfigindex, f))
        {
          freqchannels = config->getFNumChannels(f);
          pulsaraccumspace[f] = new cf32*****[config->getNumXmacStrides(newconfigindex, f)];
          for(int x=0;x<config->getNumXmacStrides(newconfigindex, f);x++)
          {
            pulsaraccumspace[f][x] = new cf32****[numbaselines];
            for(int i=0;i<numbaselines;i++)
            {
              localfreqindex = config->getBLocalFreqIndex(newconfigindex, i, f);
              if(localfreqindex >= 0)
              {
                pulsaraccumspace[f][x][i] = new cf32***[1]; //just 1 source for now!
                //for(int s=0;s<config->getMaxPhaseCentres(newconfigindex);s++)
                for(int s=0;s<1;s++) //forced to single pulsar ephemeris for now
                {
                  pulsaraccumspace[f][x][i][s] = new cf32**[config->getBNumPolProducts(newconfigindex,i,localfreqindex)];
                  for(int j=0;j<config->getBNumPolProducts(newconfigindex,i,localfreqindex);j++)
                  {
                    pulsaraccumspace[f][x][i][s][j] = new cf32*[config->getNumPulsarBins(newconfigindex)];
                    for(int k=0;k<config->getNumPulsarBins(newconfigindex);k++)
                    {
                      pulsaraccumspace[f][x][i][s][j][k] = vectorAlloc_cf32(freqchannels);
                      threadbytes[threadid] += 8*freqchannels;
                      status = vectorZero_cf32(pulsaraccumspace[f][x][i][s][j][k], freqchannels);
                      if(status != vecNoErr)
                        csevere << startl << "Error trying to zero pulsaraccumspace!!!" << endl;
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

void Core::allocateConfigSpecificThreadArrays(f32 **** baselineweight, int newconfigindex, int oldconfigindex, int threadid)
{
  int localfreqindex, numstrides, binloop;

  if(oldconfigindex >= 0)
  {
    binloop = 1;
    if(config->pulsarBinOn(oldconfigindex) && !config->scrunchOutputOn(oldconfigindex))
      binloop = config->getNumPulsarBins(oldconfigindex);
    for(int i=0;i<config->getFreqTableLength();i++)
    {
      if(config->isFrequencyUsed(oldconfigindex, i))
      {
        for(int b=0;b<binloop;b++)
        {
          for(int j=0;j<numbaselines;j++)
          {
            localfreqindex = config->getBLocalFreqIndex(oldconfigindex, j, i);
            if(localfreqindex > 0)
            {
              threadbytes[threadid] -= 4*config->getBNumPolProducts(oldconfigindex, j, localfreqindex);
              vectorFree(baselineweight[i][b][j]);
            }
            delete [] baselineweight[i][b];
          }
          delete [] baselineweight[i];
        }
        numstrides = config->getFNumChannels(i)/config->getXmacStrideLength(oldconfigindex);
        if(config->getFNumChannels(i)%config->getXmacStrideLength(oldconfigindex) != 0)
          numstrides++;
        threadbytes[threadid] -= 4*numstrides;
      }
    }
  }

  if(newconfigindex >= 0)
  {
    binloop = 1;
    if(config->pulsarBinOn(newconfigindex) && !config->scrunchOutputOn(newconfigindex))
      binloop = config->getNumPulsarBins(newconfigindex);
    for(int i=0;i<config->getFreqTableLength();i++)
    {
      if(config->isFrequencyUsed(newconfigindex, i))
      {
        //allocate the baselineweight array
        baselineweight[i] = new f32**[binloop];
        for(int b=0;b<binloop;b++)
        {
          baselineweight[i][b] = new f32*[numbaselines];
          for(int j=0;j<numbaselines;j++)
          {
            localfreqindex = config->getBLocalFreqIndex(newconfigindex, j, i);
            if(localfreqindex >= 0)
            {
              threadbytes[threadid] += 4*config->getBNumPolProducts(newconfigindex, j, localfreqindex);
              baselineweight[i][b][j] = vectorAlloc_f32(config->getBNumPolProducts(newconfigindex, j, localfreqindex));
            }
          }
        }
      }
    }
  }
}

void Core::updateconfig(int oldconfigindex, int configindex, int threadid, int & startblock, int & numblocks, int & numpolycos, bool & pulsarbin, Mode ** modes, Polyco ** polycos, bool first)
{
  Polyco ** currentpolycos;
  int blockspersend = config->getBlocksPerSend(configindex);
  startblock = 0;
  numblocks = 0;

  //figure out what section we are responsible for
  for(int i=0;i<=threadid;i++)
  {
    startblock += numblocks;
    numblocks = blockspersend/numprocessthreads + ((i < blockspersend%numprocessthreads)?1:0);
  }

  if(!first) //need to delete the old stuff
  {
    for(int i=0;i<numdatastreams;i++) {
      threadbytes[threadid] -= modes[i]->getEstimatedBytes();
      delete modes[i];
    }
    if(threadid > 0 && pulsarbin)
    {
      //only delete the polycos if they were a copy (threadid > 0)
      for(int i=0;i<numpolycos;i++) {
        threadbytes[threadid] -= polycos[i]->getEstimatedBytes();
        delete polycos[i];
      }
    }
  }

  //get the config to create the appropriate Modes for us
  for(int i=0;i<numdatastreams;i++) {
    modes[i] = config->getMode(configindex, i);
    if(modes[i] == NULL || !modes[i]->initialisedOK())
      MPI_Abort(MPI_COMM_WORLD, 1);
    threadbytes[threadid] += modes[i]->getEstimatedBytes();
  }

  pulsarbin = config->pulsarBinOn(configindex);
  if(pulsarbin)
  {
    currentpolycos = config->getPolycos(configindex);
    numpolycos = config->getNumPolycos(configindex);
    for(int i=0;i<numpolycos;i++)
    {
      //if we are not the first thread, create a copy of the Polyco for our use
      polycos[i] = (threadid==0)?currentpolycos[i]:new Polyco(*currentpolycos[i]);
      if(threadid != 0)
        threadbytes[threadid] += polycos[i]->getEstimatedBytes();
    }
    //cinfo << startl << "Core " << mpiid << " thread " << threadid << ": polycos created/copied successfully!"  << endl;
  }
}
// vim: shiftwidth=2:softtabstop=2:expandtab
