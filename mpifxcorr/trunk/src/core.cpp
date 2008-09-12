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
#include "core.h"
#include "fxmanager.h"
#include "mpifxcorr.h"
#include <difxmessage.h>

Core::Core(int id, Configuration * conf, int * dids, MPI_Comm rcomm)
  : mpiid(id), config(conf), return_comm(rcomm)
{
  int status;
  double guardratio, maxguardratio;

  //Get all the correlation parameters from config
  config->loaduvwinfo(true);
  numdatastreams = config->getNumDataStreams();
  numbaselines = config->getNumBaselines();
  maxresultlength = config->getMaxResultLength();
  numprocessthreads = config->getCNumProcessThreads(mpiid - numdatastreams - fxcorr::FIRSTTELESCOPEID);
  currentconfigindex = 0;
  startmjd = config->getStartMJD();
  startseconds = config->getStartSeconds();

  //work out the biggest overhead from any of the active configurations
  maxguardratio = 1.0;
  databytes = config->getMaxDataBytes();
  for(int i=0;i<config->getNumConfigs();i++)
  {
    guardratio = double(config->getBlocksPerSend(i) + config->getGuardBlocks(i))/double(config->getBlocksPerSend(i));
    if(guardratio > maxguardratio)
    {
      databytes = int((((long long)config->getMaxDataBytes())*((long long)(config->getBlocksPerSend(i)+config->getGuardBlocks(i))))/ config->getBlocksPerSend(i));
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
  controllength = config->getMaxSendBlocks() + 1;
  procslots = new processslot[RECEIVE_RING_LENGTH];
  for(int i=0;i<RECEIVE_RING_LENGTH;i++)
  {
    procslots[i].numchannels = config->getNumChannels(currentconfigindex);
    procslots[i].results = vectorAlloc_cf32(maxresultlength);
    if(config->getMaxNumPulsarBins() > 0)
    {
      //There is a configuration that does pulsar binning, so allocate the bincounts arrays
      procslots[i].bincounts = new s32**[config->getMaxNumPulsarBins()];
      for(int j=0;j<config->getMaxNumPulsarBins();j++)
      {
        procslots[i].bincounts[j] = new s32*[config->getMaxNumFreqs()];
        for(int k=0;k<config->getMaxNumFreqs();k++)
        {
          procslots[i].bincounts[j][k] = vectorAlloc_s32(config->getMaxNumChannels()+1);
          status = vectorZero_s32(procslots[i].bincounts[j][k], procslots[i].numchannels+1);
          if(status != vecNoErr)
	    difxMessageSendDifxAlert("Error trying to zero bincounts!!!", DIFX_ALERT_LEVEL_SEVERE);
        }
      }
    }
    //set up the remainder of the info in this slot, using the first configuration
    status = vectorZero_cf32(procslots[i].results, maxresultlength);
    if(status != vecNoErr)
    {
      char message[80];
      sprintf(message, "Error trying to zero results in core %d, processing slot %d", mpiid, i);
      difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_SEVERE);
    }
    procslots[i].resultsvalid = CR_VALIDVIS;
    procslots[i].configindex = currentconfigindex;
    procslots[i].resultlength = config->getResultLength(currentconfigindex);
    procslots[i].slotlocks = new pthread_mutex_t[numprocessthreads];
    for(int j=0;j<numprocessthreads;j++)
      pthread_mutex_init(&(procslots[i].slotlocks[j]), NULL);
    pthread_mutex_init(&(procslots[i].copylock), NULL);
    procslots[i].datalengthbytes = new int[numdatastreams];
    procslots[i].databuffer = new u8*[numdatastreams];
    procslots[i].controlbuffer = new double*[numdatastreams];
    procslots[i].keepprocessing = true;
    procslots[i].numpulsarbins = config->getNumPulsarBins(currentconfigindex);
    procslots[i].scrunchoutput = config->scrunchOutputOn(currentconfigindex);
    procslots[i].pulsarbin = config->pulsarBinOn(currentconfigindex);
    for(int j=0;j<numdatastreams;j++)
    {
      procslots[i].databuffer[j] = vectorAlloc_u8(databytes);
      procslots[i].controlbuffer[j] = vectorAlloc_f64(controllength);
    }
  }

  //initialise the threads that do the actual processing
  processthreads = new pthread_t[numprocessthreads];
  processconds = new pthread_cond_t[numprocessthreads];
  processthreadinitialised = new bool[numprocessthreads];
  for(int i=0;i<numprocessthreads;i++)
  {
    pthread_cond_init(&processconds[i], NULL);
    processthreadinitialised[i] = false;
  }

  //initialise the MPI communication objects
  datarequests = new MPI_Request[numdatastreams];
  controlrequests = new MPI_Request[numdatastreams];
  msgstatuses = new MPI_Status[numdatastreams];

  //copy the datastream ids
  datastreamids = new int[numdatastreams];
  for(int i=0;i<numdatastreams;i++)
    datastreamids[i] = dids[i];
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
    if(config->getMaxNumPulsarBins() > 0)
    {
      for(int j=0;j<config->getMaxNumPulsarBins();j++)
      {
        for(int k=0;k<config->getMaxNumFreqs();k++)
        {
          vectorFree(procslots[i].bincounts[j][k]);
        }
        delete [] procslots[i].bincounts[j];
      }
      delete [] procslots[i].bincounts;
    }
    delete [] procslots[i].slotlocks;
    delete [] procslots[i].datalengthbytes;
    delete [] procslots[i].databuffer;
    delete [] procslots[i].controlbuffer;
    vectorFree(procslots[i].results);
  }
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
  int perr, status;
  bool terminate;
  processthreadinfo * threadinfos = new processthreadinfo[numprocessthreads];
  char message[200];
  
  terminate = false;
  numreceived = 0;
  sprintf(message, "CORE %d has started executing!!!", mpiid);
  difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

  //get the lock for the first slot, one per thread
  for(int i=0;i<numprocessthreads;i++)
  {
    perr = pthread_mutex_lock(&(procslots[numreceived].slotlocks[i]));
    if(perr != 0)
    {
      sprintf(message, "CORE %d error attempting to lock mutex %d of thread %d", mpiid, numreceived, i);
      difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
    }
  }

  //start off by filling up the data and control buffers for all slots
  for(int i=0;i<RECEIVE_RING_LENGTH-1;i++)
    receivedata(numreceived++, &terminate);

  //now we have the lock on the last slot in the ring.  Launch processthreads
  for(int i=0;i<numprocessthreads;i++)
  {
    threadinfos[i].thiscore = this;
    threadinfos[i].processthreadid = i;
    perr = pthread_create(&processthreads[i], NULL, Core::launchNewProcessThread, (void *)(&threadinfos[i]));
    if(perr != 0)
    {
      sprintf(message, "Error launching core/thread %d/%d", mpiid, i);
      difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_SEVERE);
    }
    while(!processthreadinitialised[i])
    {
      perr = pthread_cond_wait(&processconds[i], &(procslots[numreceived].slotlocks[i]));
      if (perr != 0)
        difxMessageSendDifxAlert("Error waiting on receivethreadinitialised condition!!!", DIFX_ALERT_LEVEL_SEVERE);
    }
  }
  delete [] threadinfos;

  while(!terminate) //the data is valid, so keep processing
  {
    //increment and receive some more data
    receivedata(numreceived++ % RECEIVE_RING_LENGTH, &terminate);
    
    //send the results back
    MPI_Ssend(procslots[numreceived%RECEIVE_RING_LENGTH].results, procslots[numreceived%RECEIVE_RING_LENGTH].resultlength*2, MPI_FLOAT, fxcorr::MANAGERID, procslots[numreceived%RECEIVE_RING_LENGTH].resultsvalid, return_comm);

    //zero the results buffer for this slot and set the status back to valid
    status = vectorZero_cf32(procslots[numreceived%RECEIVE_RING_LENGTH].results, procslots[numreceived%RECEIVE_RING_LENGTH].resultlength);
    if(status != vecNoErr)
      difxMessageSendDifxAlert("Error trying to zero results in Core!!!", DIFX_ALERT_LEVEL_SEVERE);
    procslots[numreceived%RECEIVE_RING_LENGTH].resultsvalid = CR_VALIDVIS;
  }

  //Run through the shutdown sequence
  for(int i=0;i<numprocessthreads;i++)
  {
    //Unlock the mutex we are currently holding for this thread
    perr = pthread_mutex_unlock(&(procslots[(numreceived+RECEIVE_RING_LENGTH-1) % RECEIVE_RING_LENGTH].slotlocks[i]));
    if(perr != 0)
    {
      sprintf(message, "Error in Core %d attempt to unlock mutex %d of thread %d", mpiid, (numreceived+RECEIVE_RING_LENGTH-1), i);
      difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
    }
  }

  //ensure all the results we have sitting around have been sent
  for(int i=1;i<RECEIVE_RING_LENGTH-1;i++)
  {
    sprintf(message, "CORE %d about to send final values from section %d", mpiid, i);
    difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_DEBUG);
    for(int j=0;j<numprocessthreads;j++)
    {
      //Lock and unlock first to ensure the threads have finished working on this slot
      perr = pthread_mutex_lock(&(procslots[(numreceived+i) % RECEIVE_RING_LENGTH].slotlocks[j]));
      if(perr != 0)
      {
        sprintf(message, "Error in Core %d attempt to lock mutex %d of thread %d", mpiid, (numreceived+i) % RECEIVE_RING_LENGTH, j);
        difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
      }
      perr = pthread_mutex_unlock(&(procslots[(numreceived+i) % RECEIVE_RING_LENGTH].slotlocks[j]));
      if(perr != 0)
      {
        sprintf(message, "Error in Core %d attempt to unlock mutex %d of thread %d", mpiid, (numreceived+i) % RECEIVE_RING_LENGTH, j);
        difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
      }
    }
    //send the results
    MPI_Ssend(procslots[(numreceived+i)%RECEIVE_RING_LENGTH].results, procslots[(numreceived+i)%RECEIVE_RING_LENGTH].resultlength*2, MPI_FLOAT, fxcorr::MANAGERID, procslots[numreceived%RECEIVE_RING_LENGTH].resultsvalid, return_comm);
  }

  //join the process threads, they have to already be finished anyway
  for(int i=0;i<numprocessthreads;i++)
  {
    perr = pthread_join(processthreads[i], NULL);
    if(perr != 0)
    {
      sprintf(message, "Error in Core %d attempt to join processthread %d", mpiid, i);
      difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
    }
  }
}

void * Core::launchNewProcessThread(void * tdata)
{
  processthreadinfo * mydata = (processthreadinfo *)tdata;
  (mydata->thiscore)->loopprocess(mydata->processthreadid);

  return 0;
}

void Core::loopprocess(int threadid)
{
  int perr, numprocessed, startblock, numblocks, lastconfigindex, numpolycos,  numchan, maxbins, maxchan, maxpolycos, maxfreqs;
  bool pulsarbin, somepulsarbin, scrunch = false;
  Polyco ** polycos=0;
  Polyco * currentpolyco=0;
  Mode ** modes;
  s32 ** bins;
  cf32 * threadresults = vectorAlloc_cf32(maxresultlength);
  cf32 * pulsarscratchspace=0;
  cf32 ***** pulsaraccumspace=0;
  char message[200];
  
  pulsarbin = false;
  somepulsarbin = false;
  maxpolycos = 0;
  maxchan = 0;
  maxfreqs = config->getMaxNumFreqs();
  maxbins = config->getMaxNumPulsarBins();

  //work out whether we'll need to do any pulsar binning, and work out the maximum # channels (and # polycos if applicable)
  for(int i=0;i<config->getNumConfigs();i++)
  {
    if(config->pulsarBinOn(i))
    {
      pulsarbin = true;
      somepulsarbin = true;
      scrunch = scrunch || config->scrunchOutputOn(i);
      numpolycos = config->getNumPolycos(i);
      numchan = config->getNumChannels(i);
      if(numpolycos > maxpolycos)
        maxpolycos = numpolycos;
      if(numchan > maxchan)
        maxchan = numchan;
    }
  }

  //create the necessary pulsar scratch space if required
  if(pulsarbin)
  {
    pulsarscratchspace = vectorAlloc_cf32(maxchan+1);
    if(scrunch) //need separate accumulation space
    {
      pulsaraccumspace = new cf32****[numbaselines];
      createPulsarAccumSpace(pulsaraccumspace, procslots[0].configindex, -1); //don't need to delete old space
    }
  }

  //set to first configuration and set up, creating Modes, Polycos etc
  lastconfigindex = procslots[0].configindex;
  modes = new Mode*[numdatastreams];
  if(somepulsarbin)
    polycos = new Polyco*[maxpolycos];
  updateconfig(lastconfigindex, lastconfigindex, threadid, startblock, numblocks, numpolycos, pulsarbin, modes, polycos, true, &bins);
  numprocessed = 0;

  //lock the end section
  perr = pthread_mutex_lock(&(procslots[RECEIVE_RING_LENGTH-1].slotlocks[threadid]));
  if(perr != 0)
  {
    sprintf(message, "PROCESSTHREAD %d/%d error trying to lock mutex %d", mpiid, threadid, RECEIVE_RING_LENGTH-1);
    difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_SEVERE);
  }

  //grab the lock we really want, unlock the end section and signal the main thread we're ready to go
  perr = pthread_mutex_lock(&(procslots[0].slotlocks[threadid]));
  if(perr != 0)
  {
    sprintf(message, "PROCESSTHREAD %d/%d error trying lock mutex 0", mpiid, threadid);
    difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_SEVERE);
  }
  perr = pthread_mutex_unlock(&(procslots[RECEIVE_RING_LENGTH-1].slotlocks[threadid]));
  if(perr != 0)
  {
    sprintf(message, "PROCESSTHREAD %d/%d error trying to unlock mutex %d", mpiid, threadid, RECEIVE_RING_LENGTH-1);
    difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_SEVERE);
  }
  processthreadinitialised[threadid] = true;
  perr = pthread_cond_signal(&processconds[threadid]);
  if(perr != 0)
  {
    sprintf(message, "Core processthread %d/%d error trying to signal main thread to wake up!!!", mpiid, threadid);
    difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_SEVERE);
  }
  sprintf(message, "PROCESSTHREAD %d/%d is about to start processing", mpiid, threadid);
  difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

  //while valid, process data
  while(procslots[(numprocessed)%RECEIVE_RING_LENGTH].keepprocessing)
  {
    if(pulsarbin)
    {
      //get the correct Polyco for this time range and set it up correctly
      currentpolyco = Polyco::getCurrentPolyco(procslots[numprocessed%RECEIVE_RING_LENGTH].configindex, startmjd, double(startseconds + procslots[numprocessed%RECEIVE_RING_LENGTH].offsets[0])/86400.0, polycos, numpolycos);
      if(currentpolyco == NULL)
      {
	sprintf(message, "Could not locate a polyco to cover time %5.3f -- aborting", startmjd+double(startseconds + procslots[numprocessed%RECEIVE_RING_LENGTH].offsets[0])/86400.0);
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_FATAL);
	MPI_Abort(MPI_COMM_WORLD, 1);
      }
      currentpolyco->setTime(startmjd, double(startseconds + procslots[numprocessed%RECEIVE_RING_LENGTH].offsets[0] + double(procslots[numprocessed%RECEIVE_RING_LENGTH].offsets[1])/1000000000.0)/86400.0);
    }

    //process our section of responsibility for this time range
    processdata(numprocessed++ % RECEIVE_RING_LENGTH, threadid, startblock, numblocks, modes, currentpolyco, threadresults, bins, pulsarscratchspace, pulsaraccumspace);

    //if the configuration changes from this segment to the next, change our setup accordingly
    if(procslots[numprocessed%RECEIVE_RING_LENGTH].configindex != lastconfigindex)
    {
      sprintf(message, "CORE %d threadid %d changing config to %d", mpiid, threadid, procslots[numprocessed%RECEIVE_RING_LENGTH].configindex);
      difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
      updateconfig(lastconfigindex, procslots[numprocessed%RECEIVE_RING_LENGTH].configindex, threadid, startblock, numblocks, numpolycos, pulsarbin, modes, polycos, false, &bins);
      sprintf(message, "CORE %d threadid %d config changed successfully - pulsarbin is now %d", mpiid, threadid, pulsarbin);
      difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_VERBOSE);
      createPulsarAccumSpace(pulsaraccumspace, procslots[numprocessed%RECEIVE_RING_LENGTH].configindex, lastconfigindex);
      lastconfigindex = procslots[numprocessed%RECEIVE_RING_LENGTH].configindex;
    }
  }

  //fallen out of loop, so must be finished.  Unlock held mutex
  perr = pthread_mutex_unlock(&(procslots[numprocessed % RECEIVE_RING_LENGTH].slotlocks[threadid]));
  if (perr != 0)
  {
    sprintf(message, "CORE %d threadid %d error trying unlock mutex %d", mpiid, threadid, (numprocessed)%RECEIVE_RING_LENGTH);
    difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_SEVERE);
  }

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
    if(pulsarbin)
    {
      for(int i=0;i<maxfreqs;i++)
      {
        vectorFree(bins[i]);
      }
      delete [] bins;
    }
    delete [] polycos;
    vectorFree(pulsarscratchspace);
    if(scrunch)
    {
      createPulsarAccumSpace(pulsaraccumspace, -1, procslots[(numprocessed+1)%RECEIVE_RING_LENGTH].configindex);
      delete [] pulsaraccumspace;
    }
  }
  vectorFree(threadresults);

  sprintf(message, "PROCESS %d/%d process thread exiting!!!", mpiid, threadid);
  difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
}

void Core::receivedata(int index, bool * terminate)
{
  MPI_Status mpistatus;
  int perr;
  char message[128];

  if(*terminate)
    return; //don't try to read, we've already finished

  //Get the instructions on the time offset from the FxManager node
  MPI_Recv(&(procslots[index].offsets), 2, MPI_INT, fxcorr::MANAGERID, MPI_ANY_TAG, return_comm, &mpistatus);
  if(mpistatus.MPI_TAG == CR_TERMINATE)
  {
    *terminate = true;
    procslots[index].keepprocessing = false;
    return; //note return here!!!
  }

  //work out if the source has changed, and if so, whether we need to change the modes and baselines
  currentconfigindex = config->getConfigIndex(procslots[index].offsets[0]);
  if(procslots[index].configindex != currentconfigindex)
  {
    sprintf(message, "Config has changed! Old config: %d new config %d", procslots[index].configindex, currentconfigindex);
    difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
    procslots[index].configindex = currentconfigindex;
    if(procslots[index].configindex < 0)
    {
      sprintf(message, "Core received a request to process data from time %d which does not have a config - aborting!!!", procslots[index].offsets[0]);
      difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_FATAL);
      MPI_Abort(MPI_COMM_WORLD, 1);
    }
    procslots[index].resultlength = config->getResultLength(currentconfigindex);
    procslots[index].numchannels = config->getNumChannels(currentconfigindex);
    procslots[index].numpulsarbins = config->getNumPulsarBins(currentconfigindex);
    procslots[index].scrunchoutput = config->scrunchOutputOn(currentconfigindex);
    procslots[index].pulsarbin = config->pulsarBinOn(currentconfigindex);
  }

  //now grab the data and delay info from the individual datastreams
  for(int i=0;i<numdatastreams;i++)
  {
    //get data
    MPI_Irecv(procslots[index].databuffer[i], databytes, MPI_UNSIGNED_CHAR, datastreamids[i], CR_PROCESSDATA, MPI_COMM_WORLD, &datarequests[i]);
    //also receive the offsets and rates
    MPI_Irecv(procslots[index].controlbuffer[i], controllength, MPI_DOUBLE, datastreamids[i], CR_PROCESSCONTROL, MPI_COMM_WORLD, &controlrequests[i]);
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
    {
      sprintf(message, "CORE %d error trying lock mutex %d", mpiid, (index+1)%RECEIVE_RING_LENGTH);
      difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
    }

    perr = pthread_mutex_unlock(&(procslots[index].slotlocks[i]));
    if(perr != 0)
    {
      sprintf(message, "CORE %d error trying unlock mutex %d", mpiid, index);
      difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
    }
  }
}

void Core::processdata(int index, int threadid, int startblock, int numblocks, Mode ** modes, Polyco * currentpolyco, cf32 * threadresults, s32 ** bins, cf32* pulsarscratchspace, cf32***** pulsaraccumspace)
{
  int status, perr, resultindex=0, currentnumoutputbands, cindex, maxproducts, ds1index, ds2index, nyquistchannel;
  double offsetmins;
  float * dsweights = new float[numdatastreams];
  Mode * m1, * m2;
  s32 *** polycobincounts;
  cf32 * vis1;
  cf32 * vis2;
  bool writecrossautocorrs;
  char message[128];

  writecrossautocorrs = modes[0]->writeCrossAutoCorrs();
  maxproducts = config->getMaxProducts();

  //set up the mode objects that will do the station-based processing
  for(int j=0;j<numdatastreams;j++)
  {
    //zero the autocorrelations and set delays
    modes[j]->zeroAutocorrelations();
    modes[j]->setDelays(&(procslots[index].controlbuffer[j][1]));
    modes[j]->setData(procslots[index].databuffer[j], procslots[index].datalengthbytes[j], procslots[index].controlbuffer[j][0]);
    modes[j]->setOffsets(procslots[index].offsets[0], procslots[index].offsets[1]);
  }
  //zero the results for this slot, for this thread
  status = vectorZero_cf32(threadresults, procslots[index].resultlength);
  if(status != vecNoErr)
    difxMessageSendDifxAlert("Error trying to zero threadresults!!!", DIFX_ALERT_LEVEL_SEVERE);

  //process each FFT chunk in turn
  for(int i=startblock;i<startblock+numblocks;i++)
  {
    resultindex = 0;

    //do the station-based processing for this FFT chunk
    for(int j=0;j<numdatastreams;j++)
    {
      dsweights[j] = modes[j]->process(i);
    }

    //if necessary, work out the pulsar bins
    if(procslots[index].pulsarbin)
    {
      offsetmins = double(i*procslots[index].numchannels)/(60.0*1000000.0*config->getConfigBandwidth(procslots[index].configindex));
      currentpolyco->getBins(offsetmins, bins);
    }

    //do the cross multiplication - gets messy for the pulsar binning
    for(int j=0;j<numbaselines;j++)
    {
      //get the two modes that contribute to this baseline
      ds1index = config->getBOrderedDataStream1Index(procslots[index].configindex, j);
      ds2index = config->getBOrderedDataStream2Index(procslots[index].configindex, j);
      m1 = modes[ds1index];
      m2 = modes[ds2index];

      //add the desired results into the resultsbuffer
      for(int k=0;k<config->getBNumFreqs(procslots[index].configindex, j);k++)
      {
        //the Nyquist channel referred to here is for the *first* datastream of the baseline, in the event
        //that one datastream has USB and the other has LSB
        nyquistchannel = procslots[index].numchannels;
        if(config->getFreqTableLowerSideband(config->getBFreqIndex(procslots[index].configindex, j, k)))
          nyquistchannel = 0;

        //loop through each polarisation for this frequency
        for(int p=0;p<config->getBNumPolProducts(procslots[index].configindex,j,k);p++)
        {
          if(procslots[index].pulsarbin)
          {
            //get the appropriate arrays to multiply
            vis1 = m1->getFreqs(config->getBDataStream1BandIndex(procslots[index].configindex, j, k, p));
            vis2 = m2->getConjugatedFreqs(config->getBDataStream2BandIndex(procslots[index].configindex, j, k, p));

            //multiply into scratch space
            status = vectorMul_cf32(m1->getFreqs(config->getBDataStream1BandIndex(procslots[index].configindex, j, k, p)), m2->getConjugatedFreqs(config->getBDataStream2BandIndex(procslots[index].configindex, j, k, p)), pulsarscratchspace, procslots[index].numchannels+1);
            if(status != vecNoErr)
	    {
	      sprintf(message, "Error trying to xmac baseline %d freq %d pol product %d  status=%d", j, k, p, status);
	      difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_SEVERE);
	    }

            //if scrunching, add into temp accumulate space, otherwise add into normal space
            if(procslots[index].scrunchoutput)
            {
              for(int l=0;l<procslots[index].numchannels+1;l++)
              {
                pulsaraccumspace[j][k][p][bins[config->getBFreqIndex(procslots[index].configindex, j, k)][l]][l].re += pulsarscratchspace[l].re;
                pulsaraccumspace[j][k][p][bins[config->getBFreqIndex(procslots[index].configindex, j, k)][l]][l].im += pulsarscratchspace[l].im;
              }
              pulsaraccumspace[j][k][p][0][nyquistchannel].im += dsweights[ds1index]*dsweights[ds2index];
            }
            else
            {
              for(int l=0;l<procslots[index].numchannels+1;l++)
              {
                cindex = resultindex + bins[config->getBFreqIndex(procslots[index].configindex, j, k)][l]*config->getBNumPolProducts(procslots[index].configindex,j,k)*(procslots[index].numchannels+1) + p*(procslots[index].numchannels+1) + l;
                threadresults[cindex].re += pulsarscratchspace[l].re;
                threadresults[cindex].im += pulsarscratchspace[l].im;
              }
              cindex = resultindex + p*(procslots[index].numchannels+1) + nyquistchannel;
              threadresults[cindex].im += dsweights[ds1index]*dsweights[ds2index];
            }
          }
          else
          {
            //not pulsar binning, so this is nice and simple - just cross multiply accumulate
            status = vectorAddProduct_cf32(m1->getFreqs(config->getBDataStream1BandIndex(procslots[index].configindex, j, k, p)), m2->getConjugatedFreqs(config->getBDataStream2BandIndex(procslots[index].configindex, j, k, p)), &(threadresults[resultindex]), procslots[index].numchannels+1);
            if(status != vecNoErr)
	    {
	      sprintf(message, "Error trying to xmac baseline %d freq %d pol product %d  status=%d", j, k, p, status);
	      difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_SEVERE);
	    }
            threadresults[resultindex+nyquistchannel].im += dsweights[ds1index]*dsweights[ds2index];
            resultindex += procslots[index].numchannels+1;
          }
        }
        if(procslots[index].pulsarbin && !procslots[index].scrunchoutput)
          //we've gone through all the products of this frequency, so add the requisite increment to resultindex
          resultindex += config->getBNumPolProducts(procslots[index].configindex,j,k)*procslots[index].numpulsarbins*(procslots[index].numchannels+1);
      }
    }
  }

  //grab the bin counts if necessary
  if(procslots[index].pulsarbin)
    polycobincounts = currentpolyco->getBinCounts();

  //if we are pulsar binning, do the necessary scaling (from scratch space to results if scrunching, otherwise in-place)
  if(procslots[index].pulsarbin)
  {
    resultindex = 0;
    f64 * binweights = currentpolyco->getBinWeights();

    //do the scrunch
    for(int i=0;i<numbaselines;i++)
    {
      for(int j=0;j<config->getBNumFreqs(procslots[index].configindex, i);j++)
      {
        //the Nyquist channel referred to here is for the *first* datastream of the baseline, in the event
        //that one datastream has USB and the other has LSB
        nyquistchannel = procslots[index].numchannels;
        if(config->getFreqTableLowerSideband(config->getBFreqIndex(procslots[index].configindex, i, j)))
          nyquistchannel = 0;
        if(procslots[index].scrunchoutput)
        {
          for(int k=0;k<config->getBNumPolProducts(procslots[index].configindex,i,j);k++)
          {
            float baselineweight = pulsaraccumspace[i][j][k][0][nyquistchannel].im;
            for(int l=0;l<procslots[index].numpulsarbins;l++)
            {
              //Scale the accumulation space, and scrunch it into the results vector
              status = vectorMulC_f32_I((f32)(binweights[l]), (f32*)(pulsaraccumspace[i][j][k][l]), 2*procslots[index].numchannels+2);
              if(status != vecNoErr)
		difxMessageSendDifxAlert("Error trying to scale for scrunch!!!", DIFX_ALERT_LEVEL_SEVERE);
              status = vectorAdd_cf32_I(pulsaraccumspace[i][j][k][l], &(threadresults[resultindex]), procslots[index].numchannels+1);
              if(status != vecNoErr)
		difxMessageSendDifxAlert("Error trying to accumulate for scrunch!!!", DIFX_ALERT_LEVEL_SEVERE);
  
              //zero the accumulation space for next time
              status = vectorZero_cf32(pulsaraccumspace[i][j][k][l], procslots[index].numchannels+1);
              if(status != vecNoErr)
		difxMessageSendDifxAlert("Error trying to zero pulsaraccumspace!!!", DIFX_ALERT_LEVEL_SEVERE);
            }
            //store the correct weight
            threadresults[resultindex + nyquistchannel].im = baselineweight;
            resultindex += procslots[index].numchannels+1;
          }
        }
        else
        {
          for(int k=0;k<procslots[index].numpulsarbins;k++)
          {
            for(int l=0;l<config->getBNumPolProducts(procslots[index].configindex,i,j);l++)
            {
              //Scale the bin
              status = vectorMulC_f32_I((f32)(binweights[k]), (f32*)(&(threadresults[resultindex])), 2*procslots[index].numchannels+2);
              if(status != vecNoErr)
		difxMessageSendDifxAlert("Error trying to scale pulsar binned (non-scrunched) results!!!", DIFX_ALERT_LEVEL_SEVERE);
              if(k==0)
                //renormalise the weight
                threadresults[resultindex + nyquistchannel].im /= binweights[k];
              resultindex += procslots[index].numchannels+1;
            }
          }
        }
      }
    }
  }

  //lock the thread "copy" lock, meaning we're the only one adding to the result array
  perr = pthread_mutex_lock(&(procslots[index].copylock));
  if(perr != 0)
  {
    sprintf(message, "PROCESSTHREAD %d/%d error trying lock copy mutex!!!", mpiid, threadid);
    difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
  }

  //copy the baseline results
  status = vectorAdd_cf32_I(threadresults, procslots[index].results, resultindex);
  if(status != vecNoErr)
    difxMessageSendDifxAlert("Error trying to add thread results to final results!!!", DIFX_ALERT_LEVEL_SEVERE);

  //copy the autocorrelations
  for(int j=0;j<numdatastreams;j++)
  {
    currentnumoutputbands = modes[j]->getNumOutputBands();
    for(int k=0;k<currentnumoutputbands;k++)
    {
      //put autocorrs in resultsbuffer
      status = vectorAdd_cf32_I(modes[j]->getAutocorrelation(false, k), &procslots[index].results[resultindex], procslots[index].numchannels+1);
      if(status != vecNoErr)
        difxMessageSendDifxAlert("Error copying autocorrelations", DIFX_ALERT_LEVEL_SEVERE);
      resultindex += procslots[index].numchannels+1;
    }
    if(writecrossautocorrs && maxproducts > 1) //want the cross-polarisation autocorrs as well
    {
      for(int k=0;k<currentnumoutputbands;k++)
      {
        //put autocorrs in resultsbuffer
        status = vectorAdd_cf32_I(modes[j]->getAutocorrelation(true, k), &procslots[index].results[resultindex], procslots[index].numchannels+1);
        if(status != vecNoErr)
          difxMessageSendDifxAlert("Error copying cross-polar autocorrelations", DIFX_ALERT_LEVEL_SEVERE);
        resultindex += procslots[index].numchannels+1;
      }
    }
  }
  //clear the bin count if necessary - NO LONGER NECESSARY
  //if(config->pulsarBinOn(procslots[index].configindex))
  //  currentpolyco->incrementBinCount();

  //unlock the copy lock
  perr = pthread_mutex_unlock(&(procslots[index].copylock));
  if(perr != 0)
  {
    sprintf(message, "PROCESSTHREAD %d/%d error trying unlock copy mutex!!!", mpiid, threadid);
    difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
  }

  //grab the next lock
  perr = pthread_mutex_lock(&(procslots[(index+1)%RECEIVE_RING_LENGTH].slotlocks[threadid]));
  if(perr != 0)
  {
    sprintf(message, "PROCESSTHREAD %d/%d error trying lock copy mutex %d", mpiid, threadid, (index+1)%RECEIVE_RING_LENGTH);
    difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
  }

  //unlock the one we had
  perr = pthread_mutex_unlock(&(procslots[index].slotlocks[threadid]));
  if(perr != 0)
  {
    sprintf(message, "PROCESSTHREAD %d/%d error trying ulock copy mutex %d", mpiid, threadid, index);
    difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
  }

  delete [] dsweights;
}

void Core::createPulsarAccumSpace(cf32***** pulsaraccumspace, int newconfigindex, int oldconfigindex)
{
  int status;


  if(oldconfigindex >= 0 && config->pulsarBinOn(oldconfigindex) && config->scrunchOutputOn(oldconfigindex))
  {
    //need to delete the old pulsar accumulation space
    for(int i=0;i<numbaselines;i++)
    {
      for(int j=0;j<config->getBNumFreqs(oldconfigindex, i);j++)
      {
        for(int k=0;k<config->getBNumPolProducts(oldconfigindex,i,j);k++)
        {
          for(int l=0;l<config->getNumPulsarBins(oldconfigindex);l++)
          {
            vectorFree(pulsaraccumspace[i][j][k][l]);
          }
          delete [] pulsaraccumspace[i][j][k];
        }
        delete [] pulsaraccumspace[i][j];
      }
      delete [] pulsaraccumspace[i];
    }
  }

  if(newconfigindex >= 0 && config->pulsarBinOn(newconfigindex) && config->scrunchOutputOn(newconfigindex))
  {
    //need to create a new pulsar accumulation space
    for(int i=0;i<numbaselines;i++)
    {
      pulsaraccumspace[i] = new cf32***[config->getBNumFreqs(newconfigindex, i)];
      for(int j=0;j<config->getBNumFreqs(newconfigindex, i);j++)
      {
        pulsaraccumspace[i][j] = new cf32**[config->getBNumPolProducts(newconfigindex,i,j)];
        for(int k=0;k<config->getBNumPolProducts(newconfigindex,i,j);k++)
        {
          pulsaraccumspace[i][j][k] = new cf32*[config->getNumPulsarBins(newconfigindex)];
          for(int l=0;l<config->getNumPulsarBins(newconfigindex);l++)
          {
            pulsaraccumspace[i][j][k][l] = vectorAlloc_cf32(config->getNumChannels(newconfigindex) + 1);
            status = vectorZero_cf32(pulsaraccumspace[i][j][k][l], config->getNumChannels(newconfigindex)+1);
            if(status != vecNoErr)
	      difxMessageSendDifxAlert("Error trying to zero pulsaraccumspace!!!", DIFX_ALERT_LEVEL_SEVERE);
          }
        }
      }
    }
  }
}

void Core::updateconfig(int oldconfigindex, int configindex, int threadid, int & startblock, int & numblocks, int & numpolycos, bool & pulsarbin, Mode ** modes, Polyco ** polycos, bool first, s32 *** bins)
{
  Polyco ** currentpolycos;
  int blockspersend = config->getBlocksPerSend(configindex);
  startblock = 0;
  numblocks = 0;
  int maxfreqs = config->getMaxNumFreqs();

  if(!first) //need to delete the old stuff
  {
    for(int i=0;i<numdatastreams;i++)
      delete modes[i];
    if(threadid > 0 && pulsarbin)
    {
      //only delete the polycos if they were a copy (threadid > 0)
      for(int i=0;i<numpolycos;i++)
        delete polycos[i];
    }
    if(pulsarbin)
    {
      //free the bins
      for(int i=0;i<maxfreqs;i++)
      {
        vectorFree((*bins)[i]);
      }
      delete [] *bins;
    }
  }

  //get the config to create the appropriate Modes for us
  for(int i=0;i<numdatastreams;i++)
    modes[i] = config->getMode(configindex, i);
  
  pulsarbin = config->pulsarBinOn(configindex);
  if(pulsarbin)
  {
    currentpolycos = config->getPolycos(configindex);
    numpolycos = config->getNumPolycos(configindex);
    for(int i=0;i<numpolycos;i++)
    {
      //if we are not the first thread, create a copy of the Polyco for our use
      polycos[i] = (threadid==0)?currentpolycos[i]:new Polyco(*currentpolycos[i]);
    }
    {
      char message[80];
      sprintf(message, "CORE %d thread %d polycos created/copied successfully!");
      difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
    }

    //create the bins array
    *bins = new s32*[maxfreqs];
    for(int i=0;i<maxfreqs;i++)
    {
      (*bins)[i] = vectorAlloc_s32(config->getNumChannels(configindex) + 1);
    }
  }

  //figure out what section we are responsible for
  for(int i=0;i<=threadid;i++)
  {
    startblock += numblocks;
    numblocks = blockspersend/numprocessthreads + ((i < blockspersend%numprocessthreads)?1:0);
  }
}

