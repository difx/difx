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
#ifndef CORE_H
#define CORE_H

#include "architecture.h"
#include "datastream.h"
#include "configuration.h"
#include "mode.h"
#include "difxmessage.h"
#include <pthread.h>

/**
@class Core
@brief Accepts messages containing raw data, does the correlation, and sends off visibilities

This class provides the framework for doing the actual correlation, accepting baseband data from all telescopes, using Mode objects
to do the station-based processing and then performing the cross-multiplication and accumulation.  The accumulated visibilities
are then sent back to the FxManager.  An allocatable number of processing 

@author Adam Deller
*/
class Core{
public:
 /**
  * Constructor: Allocates the required arrays, creates the circular buffer used for sending and receiving, and sets up the MPI comms
  * @param conf The configuration object, containing all information about the duration and setup of this correlation
  * @param dids Array containing the MPI ids of each Datastream
  * @param id The Core's MPI id
  * @param rcomm An MPI_Comm object used for communicating to the FxManager only
  */
  Core(int id, Configuration * conf, int * dids, MPI_Comm rcomm);
  ~Core();

 /**
  * Until told to terminate, sits in a loop receiving raw data from the Datastreams into the circular buffer and processing it
  */
  void execute();

  /// The number of elements in the send/receive circular buffer
  static const int RECEIVE_RING_LENGTH = 4;

protected:
 /** 
  * Launches a new processing thread, which will work on a portion of the time slice every time an element in the circular buffer is processed
  * @param tdata Pointer to a processthreadinfo structure, describing which number thread this will be for this Core
  */
  static void * launchNewProcessThread(void * tdata);

private:
  /// Structure containing all the information necessary to describe one element in the circular send/receive buffer, and all the necessary space to
  /// store data and results
  typedef struct {
    u8 ** databuffer;
    double ** controlbuffer;
    cf32 * results;
    s32 *** bincounts;
    int resultlength;
    int * datalengthbytes;
    int numchannels;
    int resultsvalid;
    int configindex;
    int offsets[2]; //0=seconds, 1=nanoseconds
    bool keepprocessing;
    int numpulsarbins;
    bool pulsarbin;
    bool scrunchoutput;
    pthread_mutex_t * slotlocks;
    pthread_mutex_t copylock;
  } processslot;

  /// Structure containing a pointer to the current Core and the sequence id of the thread that will be launched, so it knows which part of the time slice to process
  typedef struct {
    Core * thiscore;
    int processthreadid;
  } processthreadinfo;

 /**
  * Allocates or deallocates the required accumulation scratch space for pulsar binning
  * @param pulsaraccumspace The array of scratch space
  * @param newconfigindex The index of the config which is to be used
  * @param oldconfigindex The index of the config which was previously being used
  */
  void createPulsarAccumSpace(cf32***** pulsaraccumspace, int newconfigindex, int oldconfigindex);

 /**
  * While the correlation is continuing, processes the given thread's share of the next element in the send/receive circular buffer
  * @param threadid The id of the thread which is doing the processing, which tells us which section of the time slice this call will process
  */
  void loopprocess(int threadid);

 /**
  * Receives data from all telescopes into the given index of the circular send/receive buffer, as well as control info from the FxManager
  * @param index The index in the circular send/receive buffer in which the received data should be stored
  * @param terminate Set if the received instruction from FxManager is to terminate
  */
  void receivedata(int index, bool * terminate);

 /**
  * Receives data from all telescopes into the given index of the circular send/receive buffer, as well as control info from the FxManager
  * @param index The index in the circular send/receive buffer to be processed
  * @param threadid The id of the thread which is doing the processing
  * @param startblock The first FFT block which is this thread's responsibility
  * @param numblocks The number of FFT blocks which this thread will take care of
  * @param modes The Mode objects which handle the station-based processing
  * @param currentpolyco The correct Polyco object for this time slice - null if not pulsar binning
  * @param threadresults Pre-allocated space for this thread to place its partial results (Nbaselines*nproducts*(numchannels+1) long)
  * @param bins Pre-allocated space for the bins for each subband/channel combination - null if not pulsar binning
  * @param pulsarscratchspace Room to perform the pulsar binning if required - null if not pulsar binning (numchannels + 1 long)
  * @param pulsaraccumspace Room in which to accumulate the binned results ([#baselines][#frequencies][#polproducts][#bins][#channels+1])
  * @param starecord Message to be sent to a process listening for STA results
  */
  void processdata(int index, int threadid, int startblock, int numblocks, Mode ** modes, Polyco * currentpolyco, cf32 * threadresults, s32 ** bins, cf32* pulsarscratchspace, cf32***** pulsaraccumspace, DifxMessageSTARecord * starecord);

 /**
  * Updates all the parameters for processing thread when the configuration changes
  * @param oldconfigindex The index of the configuration we are changing from
  * @param configindex The index of the configuration we are changing to
  * @param threadid The thread for which we are setting the parameters
  * @param startblock The FFT block which the thread will start processing at for the new configuration
  * @param numblocks The number of FFT blocks which the thread will process for the new configuration
  * @param numpolycos The number of Polycos which are associated with this configuration (if it is a pulsar binning configuration)
  * @param pulsarbin Whether this configuration is does pulsar binning or not
  * @param modes The Mode objects that will be used to do the station-based processing for this configuration
  * @param polycos The polyco objects to be used with this configuration (null if not pulsar binning)
  * @param first Whether this is the first time the config has been updated (ie do the arrays exist already and need to be deallocated)
  * @param bins Space to store the bins [#freqs][#channels+1] for this configuration (null if not pulsar binning)
  */
  void updateconfig(int oldconfigindex, int configindex, int threadid, int & startblock, int & numblocks, int & numpolycos, bool & pulsarbin, Mode ** modes, Polyco ** polycos, bool first, s32 *** bins);

  MPI_Comm return_comm;
  MPI_Request * datarequests;
  MPI_Request * controlrequests;
  MPI_Status * msgstatuses;
  int mpiid, numdatastreams, numbaselines, databytes, controllength, numreceived, currentconfigindex, numprocessthreads, maxresultlength, startmjd, startseconds;
  int * datastreamids;
  processslot * procslots;
  pthread_t * processthreads;
  pthread_cond_t * processconds;
  bool * processthreadinitialised;
  Configuration * config;
};

#endif
