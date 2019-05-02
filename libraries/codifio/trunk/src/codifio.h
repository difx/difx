/***************************************************************************
 *   Copyright (C) 2016-2017 by Chris Phillips                             *
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
// $Id: vdifio.h 7356 2016-06-24 14:34:03Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/vdifio/trunk/src/vdifio.h $
// $LastChangedRevision: 7356 $
// $Author: WalterBrisken $
// $LastChangedDate: 2016-06-25 00:34:03 +1000 (Sat, 25 Jun 2016) $
//
//============================================================================
	
#ifndef __CODIFIO_H__
#define __CODIFIO_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdio.h>
#include <time.h>

#define CODIF_HEADER_BYTES		64
  //#define MAX_CODIF_FRAME_BYTES		9032
#define CODIF_MAX_THREAD_ID		65535

#define CODIF_SUMMARY_MAX_THREADS	64
#define CODIF_SUMMARY_FILE_LENGTH	256

#define CODIF_NOERROR			0
#define CODIF_ERROR			1

  /* *** implemented in codifio.c *** */

typedef struct codif_header {
  uint32_t frame : 32;

  uint32_t seconds : 30;
  uint32_t iscomplex : 1;
  uint32_t invalid : 1;


  uint32_t stationid : 16;
  uint32_t unassigned : 6;
  uint32_t representation : 4;
  uint32_t epoch : 6;

  uint32_t framelength8 : 24;	// Frame length (including header) divided by 8 
  uint32_t nbits : 5;
  uint32_t version : 3;

  uint32_t groupid : 16;
  uint32_t threadid : 16;
   
  uint32_t nchan : 16;
  uint32_t blocklength : 16;

  uint32_t reserved2 : 32;

  uint32_t period : 16;
  uint32_t reserved1 : 16;


  uint64_t totalsamples;
   
  uint32_t reserved3;
  uint32_t sync;

  uint32_t extended2;

  uint32_t eversion : 8;
  uint32_t extended1 : 24;
  
  uint32_t extended4;
  uint32_t extended3;

} codif_header;

/* Date manipulation functions */
int ymd2doy(int yr, int mo, int day);
int ymd2mjd(int yr, int mo, int day);

/* Function to completely fill header struct, returns non-zero on error */

int createCODIFHeader(codif_header *header, int dataarraylength, uint16_t threadid, uint16_t groupid, int bits, uint16_t nchan,
		      int sampleblocklength, int period, uint64_t totalsamples, int iscomplex, char stationid[3]);

static inline uint64_t wordswap(uint64_t a) {uint64_t b; uint32_t *c = (uint32_t*)&a; uint32_t *d = (uint32_t*)&b; d[0] = c[1]; d[1]=c[0]; return b;}

/* Functions to grab just one value from the raw header */
static inline int getCODIFComplex(const codif_header *header) { return (int)header->iscomplex; }
static inline int getCODIFThreadID(const codif_header *header) { return (int)header->threadid; }
static inline int getCODIFGroupID(const codif_header *header) { return (int)header->groupid; }
static inline int getCODIFHeaderBytes(const codif_header *header) { return CODIF_HEADER_BYTES; }
static inline int getCODIFFrameBytes(const codif_header *header) { return (int)(header->framelength8)*8; }
static inline int getCODIFPeriod(const codif_header *header) { return (int)header->period; }
static inline int getCODIFSync(const codif_header *header) { return (int)header->sync; }
//static inline uint64_t getCODIFTotalSamples(const codif_header *header) { return wordswap(header->totalsamples); }
static inline uint64_t getCODIFTotalSamples(const codif_header *header) { return header->totalsamples; }
uint64_t getCODIFFrameMJDSec(codif_header *header);
int getCODIFFrameMJD(const codif_header *header);
double getCODIFFrameDMJD(const codif_header *header, double framepersec);
static inline int getCODIFFrameSecond(const codif_header *header) { return ((int)header->seconds)%86400; }
static inline int getCODIFFrameNumber(const codif_header *header) { return (int)header->frame; }
static inline int getCODIFStationID(const codif_header *header) { return (int)header->stationid; }
static inline int getCODIFBitsPerSample(const codif_header *header) { return ((int)header->nbits); }
static inline int getCODIFRepresentation(const codif_header *header) { return ((int)header->representation); }
static inline int getCODIFVersion(const codif_header *header) { return ((int)header->version); }
static inline int getCODIFNumChannels(const codif_header *header) {return (int)header->nchan+1;} 
static inline int getCODIFSampleblockLength(codif_header *header) { return (int)header->blocklength;}
static inline int getCODIFFrameInvalid(const codif_header *header) { return (int)header->invalid; }
static inline int getCODIFFrameEpochSecOffset(const codif_header *header) { return (int)header->seconds; }
static inline int getCODIFEpoch(const codif_header *header) { return (int)header->epoch; }
int getCODIFEpochMJD(const codif_header *header);

/* Functions to set just one value from a raw header */
int setCODIFFrameMJD(codif_header *header, int framemjd);
int setCODIFFrameMJDSec(codif_header *header, uint64_t mjdsec);
static inline void setCODIFFrameEpochSecOffset(codif_header *header, int seconds) { header->seconds = seconds; }
static inline void setCODIFFrameNumber(codif_header *header, int framenumber) { header->frame = framenumber; }
static inline void setCODIFFrameInvalid(codif_header *header, unsigned int invalid) { header->invalid = invalid; }
static inline void setCODIFBitsPerSample(codif_header *header, int nbits) { header->nbits = nbits; }
static inline void setCODIFRepresentation(codif_header *header, int representation) { header->representation = representation; }
static inline void setCODIFSampleblockLength(codif_header *header, int sampleblock) { header->blocklength = sampleblock; }
static inline void setCODIFPeriod(codif_header *header, uint32_t period) { header->period = period; }
static inline void setCODIFVersion(codif_header *header, int version) { header->version = version; }
static inline int setCODIFThreadID(codif_header *header, int threadid) { header->threadid = threadid; return(CODIF_NOERROR);}
static inline int setCODIFGroupID(codif_header *header, int groupid) { header->groupid = groupid; return(CODIF_NOERROR);}

int setCODIFFrameBytes(codif_header *header, int bytes);
int setCODIFFrameSecond(codif_header *header, int seconds);
int setCODIFNumChannels(codif_header *header, int numchannels);
int setCODIFFrameTime(codif_header *header, time_t time);
int setCODIFEpochTime(codif_header *header, time_t time);
static inline void setCODIFEpoch(codif_header *header, int epoch) { header->epoch = epoch; }
int setCODIFEpochMJD(codif_header *header, int mjd);
int nextCODIFHeader(codif_header *header, int framepersec);
int incrementCODIFHeader(codif_header *header, int framepersec, int64_t inc);

double CODIFframe2mjd(const codif_header *header, int sec, int frame, int frampersec);

  //void fprintCODIFHeader(FILE *out, const codif_header *header, enum CODIFHeaderPrintLevel);
  //void printCODIFHeader(const codif_header *header, enum CODIFHeaderPrintLevel);


#ifdef __cplusplus
}
#endif

#endif
