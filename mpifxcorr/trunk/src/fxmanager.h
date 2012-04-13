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
#ifndef FXMANAGER_H
#define FXMANAGER_H

#include <mpi.h>
#include "configuration.h"
#include "architecture.h"
#include "visibility.h"
#include "core.h"
#include <pthread.h>


/**
@class FxManager
@brief One object of this class manages the correlation

This class provides the functionality to control a correlation, by sending requests to Datastreams for data messages to be sent to 
specified Cores for correlation, and receiving the correlated visibilities from Cores.  After receiving the short-term accumulated
visibilities from Cores, it performs long-term accumulation in an array of Visibility objects and writes results to disk.

@author Adam Deller
*/
class FxManager{
public:
 /**
  * Constructor: Allocates the required arrays, creates the Visibility objects and initialises the writing thread
  * @param conf The configuration object, containing all information about the duration and setup of this correlation
  * @param ncores The number of Cores (processing nodes) to be used in this correlation
  * @param dids Array containing the MPI ids of each Datastream
  * @param cids Array containing the MPI ids of each Core
  * @param id The FXManager's MPI id
  * @param rcomm An MPI_Comm object used for communicating to the Cores only
  * @param mon Whether data is sent to a remote monitor using a socket
  * @param hname The hostname to send monitoring data to
  * @param port The port to send monitoring data down
  * @param monitor_skip Only send 1 in every monitor_skip visibilities to the monitor
  */

  enum monsockStatusType {CLOSED, PENDING, OPENED};

  FxManager(Configuration * conf, int ncores, int * dids, int * cids, int id, MPI_Comm rcomm, bool mon, char * hname, int port, int monitor_skip);
  ~FxManager();

 /**
  * Sends terminate signals to all cores and datastreams
  */
  void terminate();

 /**
  * Runs the correlation from the start time to end time
  */
  void execute();

 /**
  * Returns the estimated number of bytes used by the FxManager
  * @return Estimated memory size of the FxManager (bytes)
  */
  inline long long getEstimatedBytes() { return estimatedbytes; }

  void MonitorThread();

protected:
 /** 
  * Launches a new writing thread, that will loop through the Visibility array as fast as possible, writing out results as soon as it is allowed
  * @param thismanager Pointer to this FxManager object
  */
  static void * launchNewWriteThread(void * thismanager);

  static void * launchMonitorThread(void * thismanager);

  
private:
  //constants
  static const string CIRCULAR_POL_NAMES[4];
  static const string LL_CIRCULAR_POL_NAMES[4];
  static const string LINEAR_POL_NAMES[4];

  //methods
 /** 
  * Sends an instruction to all Datastreams to send data for the specified timerange to the specified Core
  * @param data The MPI id of the core to send to, the offset from the start of the experiment in seconds, and the offset from this second in nanoseconds
  * @param coreindex The Core index (to the coreids array), telling the Manager which core to send to.
  */
  void sendData(int data[], int coreindex);

 /** 
  * Receives one short-term accumulated result from a Core, and optionally arranges for some more data to be sent to this Core
  * @param resend Whether to call sendData to get more data sent immediately to this Core
  */
  void receiveData(bool resend);

 /** 
  * Locates the Visibility that the most recent data to have arrived should go to
  * @param coreid The core from which the most recent data was received
  * @return The index of the correct Visibility in the buffer, or -1 if the data is stale and cannot be stored
  */
  int locateVisIndex(int coreid);

 /** 
  * Open the files
  */
  void initialiseOutput();

 /**
  * Prints a summary of the internal buffers and how many subints were received from each core
  * @param visindex The index in the visibility buffer which was just completed
  */
  void printSummary(int visindex);

 /** 
  * While the correlation is active, continually tries to obtain a lock on the next Visibility, write it out, and increment it
  */
  void loopwrite();

  /* 
   * Copy vis data into buffer and signal to monitor thread to start copying 
   */

  void sendMonitorData(int visID);
  bool checkSocketStatus();
  int openMonitorSocket();

  //variables
  Configuration * config;
  MPI_Comm return_comm;
  int numcores, mpiid, numdatastreams, startmjd, startseconds, initns, initsec, initscan, executetimeseconds, resultlength, numbaselines, nsincrement, currentconfigindex, newestlockedvis, oldestlockedvis, writesegment;
  long long estimatedbytes;
  double inttime;
  bool keepwriting, circularpols, writethreadinitialised, visibilityconfigok;
  int senddata[4];
  Model * model;
  int * datastreamids;
  int * coreids;
  int * corecounts;
  int * recentcorecounts;
  int * numsent;
  int * extrareceived;
  int *** coretimes;
  bool monitor;
  char * hostname;
  cf32 * resultbuffer;
  char * todiskbuffer;
  Visibility ** visbuffer;
  pthread_mutex_t * bufferlock, startlock;
  bool * islocked;
  pthread_cond_t writecond;
  pthread_t writethread;

  int lastsource;

  // Variables needed for monitoring
  int monitor_skip;
  int mon_socket;
  int monitorport;
  pthread_cond_t monitorcond;
  pthread_mutex_t moncondlock, monitorwritelock;
  pthread_t monthread;
  monsockStatusType monsockStatus;
  char *buf;
  int bufsize, nbuf;

};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
