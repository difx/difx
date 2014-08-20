/***************************************************************************
 *   Copyright (C) 2006-2013 by Adam Deller and Walter Brisken             *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/mpifxcorr/trunk/src/mk5.h $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#ifndef VDIFFILE_H
#define VDIFFILE_H

#include <vdifio.h>
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

  virtual void initialise();

protected:
  #include <vector>
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

  virtual int dataRead(int buffersegment);

  virtual void diskToMemory(int buffersegment);

  virtual int testForSync(int configindex, int buffersegment);

  virtual void loopfileread();

  int lastconfig;

  char formatname[64];

  unsigned char *readbuffer;
  int readbuffersize;
  int readbufferleftover;
  int minleftoverdata;
  int nSort, nGap;  // muxer tuning parameters
  struct vdif_mux vm;
  struct vdif_mux_statistics vstats;
  long long startOutputFrameNumber;
  int invalidtime;

  int nbits, framespersecond, nthreads, inputframebytes;
  const int *threads;

  Configuration::datasampling samplingtype;
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
