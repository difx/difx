/***************************************************************************
 *   Copyright (C) 2011 by Adam Deller                                     *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au/~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: datamuxer.h 2241 2010-06-22 04:32:51Z ChrisPhillips $
// $HeadURL: https://svn.atnf.csiro.au/difx/mpifxcorr/trunk/src/datamuxer.h $
// $LastChangedRevision: 2241 $
// $Author: ChrisPhillips $
// $LastChangedDate: 2010-06-21 22:32:51 -0600 (Mon, 21 Jun 2010) $
//
//============================================================================
#ifndef DATAMUXER_H
#define DATAMUXER_H

#include "configuration.h"

/**
@class DataMuxer
@brief Provides scratch space for muxing of data from multiple threads (consisting of sequential interleaved frames) into a single thread

Provides a buffer for data in one (demultiplexed) VLBI data format, and translates that data into a multiplexed format and stores it in a provided buffer.  This is a virtual superclass for specific instantiations.

@author Adam Deller
*/
class DataMuxer{
public:
 /**
  * Constructor: Allocates the required buffers
  * @param conf The configuration object, containing all information about the duration and setup of this correlation
  * @param dsindex The index of the parent datastream (numbered from 0)
  * @param id The parent datastream's  MPI id
  * @param nthreads The number of source threads
  * @param sbytes The number of bytes in an individual segment in the demuxedbuffer
  */
  DataMuxer(const Configuration * conf, int dsindex, int id, int nthreads, int sbytes);

  virtual ~DataMuxer();

 /**
  * Looks for any bad data in the demux buffer and excises it
  * @param checkbuffer The place to start checking
  * @param bytestocheck The number of bytes which must be checked
  * @param startfrom Up to this far was already checked, only need to do from here onwards
  * @return Number of additional bytes which must be read in at the end of the buffer to refill it
  */
  virtual int datacheck(u8 * checkbuffer, int bytestocheck, int startfrom) = 0;

 /**
  * Does the initialising - sets up reference times, etc
  * @return True for success, false for some kind of failure
  */
  virtual bool initialise() = 0;

 /**
  * Resets all counters - done in conjunction with an initialisations
  */
  virtual void resetcounters();

 /**
  * Accessor method for demuxbuffer
  * @return pointer to start of demuxbuffer section to be written to
  */
  virtual u8* getCurrentDemuxBuffer() const = 0;

 /**
  * Accessor method for demux buffer segment byte size
  * @return segment size in bytes
  */
  inline int getSegmentBytes() const { return segmentbytes; }

 /**
  * Add to the skip count for this muxer
  * @param sframes The number of frames to add to the skipcount
  */
 inline void addSkipFrames(int sframes) { skipframes += sframes; }

 /**
  * Accessor method for estimated number of bytes
  * @return estimated amount of memory allocated by this object, in bytes
  */
  inline long long getEstimatedBytes() const { return estimatedbytes; }

 /**
  * Increments the read counter, used after external source has read data into the demux buffer
  */
  inline void incrementReadCounter() { readcount++; }

 /**
  * Returns the number of threads in this datamuxer
  * @return The number of threads in this datamuxer
  */
  inline int getNumThreads() { return numthreads; }

 /**
  * De-interlaces one segments worth of data
  * @param validbytes The number of bytes which contain valid data to be de-interlaced
  * @return True for success, false for some kind of failure
  */
  virtual bool deinterlace(int validbytes) = 0;

 /**
  * Does the multiplexing for one segment's worth of data (pure virtual - must be overridden in derived class)
  * @param outputbuffer Location to put the multiplexed data
  * @return Number of good bytes multiplexed
  */
  virtual int multiplex(u8 * outputbuffer) = 0;

  ///constants
  static const int DEMUX_BUFFER_FACTOR = 4;

protected:
  ///additional buffers
  u8 * demuxbuffer;
  u8** threadbuffers;

  ///other variables
  const Configuration * config;
  long long readcount, muxcount, deinterlacecount, estimatedbytes, skipframes, lastskipframes;
  int datastreamindex, mpiid, numthreads, segmentbytes;
};

/**
 @class VDIFMuxer
 @brief A muxer for multiple-thread VDIF to single-thread VDIF

 Assumes running on a LITTLE-ENDIAN MACHINE, restricted to a power of two threads
 @author Adam Deller
 */
class VDIFMuxer : public DataMuxer{
public:
 /**
  * Constructor: Allocates the required buffers
  * @param conf The configuration object, containing all information about the duration and setup of this correlation
  * @param dsindex The index of the parent datastream (numbered from 0)
  * @param id The parent datastream's  MPI id
  * @param nthreads The number of source threads
  * @param iframebytes The size of an input VDIF frame in bytes (including header)
  * @param rframes The number of frames to read at a time
  * @param fpersec The number of frames expected per second
  * @param bitspersamp The number of bits per sample
  * @param tmap Array containing mapping from origin threadIds to position in output single-thread file
  */
  VDIFMuxer(const Configuration * conf, int dsindex, int id, int nthreads, int iframebytes, int rframes, int fpersec, int bitspersamp, int * tmap);

  virtual ~VDIFMuxer();

 /**
  * Looks for any bad data in the demux buffer and excises it
  * @param checkbuffer The place to start checking
  * @param bytestocheck The number of bytes which must be checked
  * @param startfrom Up to this far was already checked, only need to do from here onwards
  * @return Number of additional bytes which must be read in at the end of the buffer to refill it
  */
  virtual int datacheck(u8 * checkbuffer, int bytestocheck, int startfrom);

 /**
  * Does the initialising - looks at first frame and sets reference times etc
  * @return True for success, false for some kind of failure
  */
  virtual bool initialise();

 /**
  * Resets all counters - done in conjunction with an initialisations
  */
  virtual void resetcounters();


 /**
  * Accessor method for demuxbuffer
  * @return pointer to start of demuxbuffer section to be written to
  */
  virtual inline u8* getCurrentDemuxBuffer() const { return demuxbuffer + (readcount%DEMUX_BUFFER_FACTOR)*segmentbytes; }

protected:
 /**
  * Checks if all required frames are present from all threads
  * @param bufferframe The buffer frame index to check
  * @return True if this set of frames is ok for all threads
  */
  bool validData(int bufferframe) const;

 /**
  * De-interlaces one segments worth of data
  * @param validbytes The number of bytes which contain valid data to be de-interlaced
  * @return True for success, false for some kind of failure
  */
  virtual bool deinterlace(int validbytes);

 /**
  * Does the multiplexing for one segment's worth of data
  * @param outputbuffer Location to put the multiplexed data
  * @return Number of good bytes multiplexed
  */
  virtual int multiplex(u8 * outputbuffer);

  ///other variables
  int  *  threadindexmap;     // [numthreads]
  unsigned int * threadwords; // [numthreads]
  bool ** bufferframefull;    // [numthreads][numbufferframes]
  int inputframebytes, outputframebytes, readframes, framespersecond, bitspersample, numthreadbufframes;
  int refframemjd, refframesecond, refframenumber;
  int samplesperframe, wordsperinputframe, wordsperoutputframe, samplesperinputword, samplesperoutputword;
  unsigned int copyword, activemask;
  long long processframenumber;
  void (VDIFMuxer::*cornerturn)(u8 * outputbuffer, int processindex, int outputframecount);

private:
  void cornerturn_generic(u8 * outputbuffer, int processindex, int outputframecount);
  void cornerturn_1thread(u8 * outputbuffer, int processindex, int outputframecount);
  void cornerturn_2thread_2bit(u8 * outputbuffer, int processindex, int outputframecount);
  void cornerturn_4thread_2bit(u8 * outputbuffer, int processindex, int outputframecount);
  void cornerturn_8thread_2bit(u8 * outputbuffer, int processindex, int outputframecount);
};


#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
