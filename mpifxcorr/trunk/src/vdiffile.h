/***************************************************************************
 *   Copyright (C) 2006-2020 by Adam Deller and Walter Brisken             *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/mpifxcorr/trunk/src/mk5.h $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#ifndef __VDIFFILE_H__
#define __VDIFFILE_H__

#include <vdifio.h>
#include <pthread.h>
#include "datastream.h"

/**
@class VDIFDataStream 
@brief Datastream which can handle Mk5 formatted data

This class manages a stream of data from a disk or memory, coarsely aligning it with the geocentre and sending segments of 
data to Core nodes for processing as directed by the FxManager.  Mk5Datastream overrides the LBA-style defaults and
implements appropriate functionality for Mk5 formatted data

@author Adam Deller
*/
class VDIFDataStream : public DataStream
{
public:
 /**
  * Constructor: Just passes the parameters on to the Datastream constructor
  * @param conf The configuration object, containing all information about the duration and setup of this correlation
  * @param snum This Datastream's index (numbered from 0)
  * @param id This Datastream's MPI id
  * @param ncores The number of Cores in this correlation
  * @param cids Array containing the MPI ids of each Core
  * @param bufferfactor The size of the buffer, in terms of number of "max send sizes" - the biggest "blocks per send*numchannels" from the possible configurations
  * @param numsegments The number of separate segments this buffer will be divided into
  */
  VDIFDataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments);
  virtual ~VDIFDataStream();

protected:
 /** 
  * Calculates the correct offset from the start of the databuffer for a given time in the correlation, 
  * and calculates valid bits for each FFT block as control information to pass to the Cores
  * @param scan The scan to calculate for
  * @param offsetsec The offset in seconds from the start of the scan
  * @param offsetns The offset in nanoseconds from the given second
  * @return The offset in bytes from the start of the databuffer that this block should start from - must be between 0 and bufferlength
  */
  virtual int calculateControlParams(int scan, int offsetsec, int offsetns);

 /** 
  * Updates all the parameters (numchannels, sendbytes etc) for the specified segment of the databuffer.  Allows for
  * the fact that VDIF data must be sent as an integer number of frames, starting at a frame boundary
  * @param segmentindex The index of the segment to be updated
  */
  virtual void updateConfig(int segmentindex);

 /** 
  * Reads in the header information from a Mk5 formatted file and sets the current segment time information accordingly
  * @param configindex The config index at the current time
  * @param fileindex The number of the file to be opened
  */
  virtual void initialiseFile(int configindex, int fileindex);

  virtual void startReaderThread();

  virtual int dataRead(int buffersegment);

  static void *launchreadthreadfunction(void *self);

  void readthreadfunction();

  virtual void diskToMemory(int buffersegment);

  virtual int testForSync(int configindex, int buffersegment);

  void lockSlot(int slot, int processNum = 1);
  void unlockSlot(int slot, int processNum = 1);
  void unlockAllSlots(int processNum = 1);

  char formatname[64];

  unsigned char *readbuffer;
  int readbuffersize;
  int readbufferslots;
  unsigned int readbufferslotsize;
  int readbufferleftover;
  int minleftoverdata;
  int nSort, nGap;  // muxer tuning parameters
  struct vdif_mux vm;
  struct vdif_mux_statistics vstats;
  long long startOutputFrameNumber;
  int invalidtime;
  double jobEndMJD;

  int nbits, framespersecond, nthreads, inputframebytes;
  const int *threads;

  Configuration::datasampling samplingtype;
  Configuration::filechecklevel filecheck;

  pthread_t readthread;
  pthread_mutex_t *readthreadmutex;
  int *slotMutexOwner;
  int lockstart, lockend, lastslot, lockmod;
  unsigned int endindex, muxindex;
  int readbufferwriteslot;
  bool readfail;
  double vdifmjd;

  int nGapWarn;
  int nExcessWarn;
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
