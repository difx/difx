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
#ifndef Mark5BFILE_H
#define Mark5BFILE_H

#include <mark5access/mark5bfix.h>
#include <mark5access/mark5bfile.h>
#include "datastream.h"

/**
@class VDIFDataStream 
@brief Datastream which can handle Mk5 formatted data

This class manages a stream of data from a disk or memory, coarsely aligning it with the geocentre and sending segments of 
data to Core nodes for processing as directed by the FxManager.  Mk5Datastream overrides the LBA-style defaults and
implements appropriate functionality for Mk5 formatted data

@author Adam Deller
*/
class Mark5BDataStream : public DataStream
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
  Mark5BDataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments);
  virtual ~Mark5BDataStream();

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
  * the fact that Mark5B data must be sent as an integer number of frames, starting at a frame boundary
  * @param segmentindex The index of the segment to be updated
  */
  virtual void updateConfig(int segmentindex);

 /** 
  * Reads in the header information from a Mk5 formatted file and sets the current segment time information accordingly
  * @param configindex The config index at the current time
  * @param fileindex The number of the file to be opened
  */
  virtual void initialiseFile(int configindex, int fileindex);

  virtual void initialiseFake(int configindex);

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
  struct mark5b_fix_statistics m5bstats;
  int startOutputFrameNumber;   // This is the Mark5B frame number within 1 second.  It should always be in [0, 25600) or -1.
  int invalidtime;

  int nbits, framespersecond, framebytes;
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
