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
   uint32_t seconds: 30;
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
int createVDIFHeader(vdif_header *header, int framelength, int threadid,  int bits, int nchan,
		     int iscomplex, char stationid[3]);

/* Functions to grab just one value from the raw header */
int getVDIFThreadID(vdif_header *header);
int getVDIFFrameBytes(vdif_header *header);
int getVDIFFrameMJD(vdif_header *header);
double getVDIFDMJD(vdif_header *header, int framepersec);
int getVDIFFrameSecond(vdif_header *header);
int getVDIFFrameNumber(vdif_header *header);
int getVDIFStationID(vdif_header *header);
int getVDIFBitsPerSample(vdif_header *header);
int getVDIFNumChannels(vdif_header *header);
int getVDIFFrameInvalid(vdif_header *header);
int getVDIFFullSecond(vdif_header *header);
int getVDIFEpoch(vdif_header *header);

/* Functions to set just one value from a raw header */
void setVDIFFrameMJD(vdif_header *header, int framemjd);
void setVDIFMJDSec(vdif_header *header, uint64_t mjdsec);
void setVDIFFrameSecond(vdif_header *header, int framesecond);
void setVDIFFrameNumber(vdif_header *header, int framenumber);
void setVDIFFrameInvalid(vdif_header *header, unsigned int invalid);
void setVDIFFrameBytes(vdif_header *header, int bytes);
void setVDIFNumChannels(vdif_header *header, int numchannels);
void setVDIFThreadID(vdif_header *header, int threadid);
int setVDIFTime(vdif_header *header, time_t time);
void setVDIFEpoch(vdif_header *header, int mjd);
int nextVDIFHeader(vdif_header *header, int framepersec);

int vdifmux(unsigned char *dest, int nFrame, const unsigned char *src, int length, int inputFrameSize, int inputFramesPerSecond, int nBit, int nThread, const int *threadIds, int nSort, int nGap);

#ifdef __cplusplus
}
#endif

#endif
