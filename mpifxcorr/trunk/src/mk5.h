/***************************************************************************
 *   Copyright (C) 2006-2016 by Adam Deller and Walter Brisken             *
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
#ifndef MK5_H
#define MK5_H

#include "datastream.h"

/**
@class Mk5DataStream 
@brief Datastream which can handle Mk5 formatted data

This class manages a stream of data from a disk or memory, coarsely aligning it with the geocentre and sending segments of 
data to Core nodes for processing as directed by the FxManager.  Mk5Datastream overrides the LBA-style defaults and
implements appropriate functionality for Mk5 formatted data

@author Adam Deller
*/
class Mk5DataStream : public DataStream
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
  Mk5DataStream(const Configuration * conf, int snum, int id, int ncores, int * cids, int bufferfactor, int numsegments);
  virtual ~Mk5DataStream();

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

  void deriveFormatName(int configindex);

 /** 
  * Updates all the parameters (numchannels, sendbytes etc) for the specified segment of the databuffer.  Allows for
  * the fact that Mk5 data must be sent as an integer number of frames, starting at a frame boundary
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

  virtual void fakeToMemory(int buffersegment);

  virtual void networkToMemory(int buffersegment, uint64_t & framebytesremaining);

  virtual int readnetwork(int sock, char* ptr, int bytestoread, unsigned int* nread);

  virtual int testForSync(int configindex, int buffersegment);

  virtual int checkData(int buffersegment);

  virtual uint64_t openframe();

  int lastconfig;

  // UDP values and statistics
  int npacketperframe, udpsize;
  unsigned long packet_drop, packet_oo, packet_duplicate, npacket;
  long udp_offset;
  unsigned long long packet_sum, packet_head, packet_segmentstart;
  double lasttime, udpstats_update;
  char *udp_buf, *invalid_buf;
  vector<bool> packets_arrived;
  char formatname[64];
  struct mark5_stream * syncteststream;
  int framegranularity;
};

#endif
// vim: shiftwidth=2:softtabstop=2:expandtab
