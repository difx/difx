/***************************************************************************
 *   Copyright (C) 2009-2013 by Adam Deller / Walter Brisken               *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
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
	
#ifndef __VDIFIO_H__
#define __VDIFIO_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <time.h>

#define VDIF_HEADER_BYTES		32
#define VDIF_LEGACY_HEADER_BYTES	16
#define MAX_VDIF_FRAME_BYTES		9032

#define VDIF_NOERROR 0
#define VDIF_ERROR 1

typedef struct vdif_header {
   uint32_t seconds : 30;
   uint32_t legacymode : 1;
   uint32_t invalid : 1;
   uint32_t frame : 24;
   uint32_t epoch : 6;
   uint32_t unassigned : 2;
   uint32_t framelength8 : 24;
   uint32_t nchan : 5;
   uint32_t version : 3;
   uint32_t stationid : 16;
   uint32_t threadid : 10;
   uint32_t nbits : 5;
   uint32_t iscomplex : 1;
   uint32_t eversion : 8;
   uint32_t extended1 : 24;
   uint32_t extended2;
   uint32_t extended3;
   uint32_t extdended4;
 } vdif_header;

/* Date manipulation functions */
int ymd2doy(int yr, int mo, int day);
int ymd2mjd(int yr, int mo, int day);

/* Function to completely fill header struct, returns non-zero on error */
int createVDIFHeader(vdif_header *header, int framelength, int threadid, int bits, int nchan,
		     int iscomplex, char stationid[3]);

/* Functions to grab just one value from the raw header */
static inline int getVDIFThreadID(const vdif_header *header) { return (int)header->threadid; }
static inline int getVDIFFrameBytes(const vdif_header *header) { return (int)(header->framelength8)*8; }
int getVDIFFrameMJD(const vdif_header *header);
double getVDIFDMJD(const vdif_header *header, int framepersec);
static inline int getVDIFFrameSecond(const vdif_header *header) { return ((int)header->seconds)%86400; }
static inline int getVDIFFrameNumber(const vdif_header *header) { return (int)header->frame; }
static inline int getVDIFStationID(const vdif_header *header) { return (int)header->stationid; }
static inline int getVDIFBitsPerSample(const vdif_header *header) { return ((int)header->nbits+1); }
static inline int getVDIFNumChannels(const vdif_header *header) { return 1 << (header->nchan); }
static inline int getVDIFFrameInvalid(const vdif_header *header) { return (int)header->invalid; }
static inline int getVDIFFullSecond(const vdif_header *header) { return (int)header->seconds; }
static inline int getVDIFEpoch(const vdif_header *header) { return (int)header->epoch; }

/* Functions to set just one value from a raw header */
void setVDIFFrameMJD(vdif_header *header, int framemjd);
void setVDIFMJDSec(vdif_header *header, uint64_t mjdsec);
static inline void setVDIFFrameSecond(vdif_header *header, int framesecond) { header->seconds = framesecond; }
static inline void setVDIFFrameNumber(vdif_header *header, int framenumber) { header->frame = framenumber; }
static inline void setVDIFFrameInvalid(vdif_header *header, unsigned int invalid) { header->invalid = invalid; }
void setVDIFFrameBytes(vdif_header *header, int bytes);
void setVDIFNumChannels(vdif_header *header, int numchannels);
void setVDIFThreadID(vdif_header *header, int threadid);
int setVDIFTime(vdif_header *header, time_t time);
void setVDIFEpoch(vdif_header *header, int mjd);
int nextVDIFHeader(vdif_header *header, int framepersec);


struct vdif_mux_statistics {
  /* The first 8 accumulate over multiple calls to vdifmux */
  long long nValidFrame;		/* number of valid VDIF input frames encountered */
  long long nInvalidFrame;		/* number of real VDIF frames discarded because of invalid bit being set */
  long long nDiscardedFrame;		/* number of valid input frames discarded because of out-of-order issues */
  long long nWrongThread;		/* number of otherwise good frames with incorrect thread */
  long long nSkippedByte;		/* number of bytes skipped (interloper frames) */
  long long nFillByte;			/* counts number of bytes skipped that were identified as fill pattern */
  long long bytesProcessed;		/* total bytes consumed from */
  long long nGoodFrame;			/* number of fully usable output frames */
  int nCall;				/* how many calls to vdifmux since last reset */

  /* These remaining fields are set each time */
  int srcSize;			/* length of input array (bytes) */
  int srcUsed;			/* amount of input array consumed (bytes) */
  int destSize;			/* length of output array (bytes) */
  int destUsed;			/* amount of output array populated */
  int inputFrameSize;		/* length in bytes of one input data frame (provided to call) */
  int outputFrameSize;		/* length in bytes of one output data frame (calculated) */
  int outputFrameGranularity;	/* number of output frames required to make an integer number of nanoseconds */
  int outputFramesPerSecond;	/* from call */
  int nOutputFrame;		/* length of usable output data measured in frames */
  long long startFrameNumber;

  /* start time of output data */
  /* duration of output data */
};

int vdifmux(unsigned char *dest, int nFrame, const unsigned char *src, int length, int inputFrameSize, int inputFramesPerSecond, int nBit, int nThread, const int *threadIds, int nSort, int nGap, long long startOutputFrameNumber, struct vdif_mux_statistics *stats);

void printvdifmuxstatistics(const struct vdif_mux_statistics *stats);

void resetvdifmuxstatistics(struct vdif_mux_statistics *stats);

#ifdef __cplusplus
}
#endif

#endif
