/***************************************************************************
 *   Copyright (C) 2009-2018 by Adam Deller / Walter Brisken               *
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
#include <stdio.h>
#include <time.h>

#define VDIF_HEADER_BYTES		32
#define VDIF_LEGACY_HEADER_BYTES	16
#define MAX_VDIF_FRAME_BYTES		9032
#define VDIF_MAX_THREAD_ID		1023

#define VDIF_SUMMARY_MAX_THREADS	64
#define VDIF_SUMMARY_FILE_LENGTH	256

#define VDIF_NOERROR			0
#define VDIF_ERROR			1

#define VDIF_EDV2_SUBVER_ALMA		0x0A5AE5
#define VDIF_EDV2_SUBVER_R2DBE		0x000000

/* *** implemented in vdifio.c *** */

typedef struct vdif_header {
   uint32_t seconds : 30;
   uint32_t legacymode : 1;
   uint32_t invalid : 1;

   uint32_t frame : 24;
   uint32_t epoch : 6;
   uint32_t unassigned : 2;
   
   uint32_t framelength8 : 24;	// Frame length (including header) divided by 8 
   uint32_t nchan : 5;
   uint32_t version : 3;
   
   uint32_t stationid : 16;
   uint32_t threadid : 10;
   uint32_t nbits : 5;
   uint32_t iscomplex : 1;
   
   uint32_t extended1 : 24;
   uint32_t eversion : 8;
   
   uint32_t extended2;
   uint32_t extended3;
   uint32_t extended4;
} vdif_header;

typedef struct vdif_edv1_header {	/* NICT extensions: see http://www.vlbi.org/vdif/docs/VDIF_Extension_NICT_Version2014.pdf */
   uint32_t seconds : 30;
   uint32_t legacymode : 1;
   uint32_t invalid : 1;
   
   uint32_t frame : 24;
   uint32_t epoch : 6;
   uint32_t unassigned : 2;
   
   uint32_t framelength8 : 24;	// Frame length (including header) divided by 8 
   uint32_t nchan : 5;
   uint32_t version : 3;	// Set to 1
   
   uint32_t stationid : 16;
   uint32_t threadid : 10;
   uint32_t nbits : 5;
   uint32_t iscomplex : 1;

   uint32_t samprate : 23;	// in samprateunits
   uint32_t samprateunits : 1;	// 0 = kHz, 1 = MHz
   uint32_t eversion : 8;
   
   uint32_t syncword;		// 0xACABFEED
   
   char name[8];		// DAS/Station Name
 } vdif_edv1_header;

typedef struct vdif_edv2_header_generic { /* Nominally for ALMA: https://vlbi.org/vdif/docs/alma-vdif-edv.pdf. Has sub-subversion field, defines upto 2^24 sub-versions for EDV2. */
   uint32_t seconds : 30;
   uint32_t legacymode : 1;
   uint32_t invalid : 1;
   
   uint32_t frame : 24;
   uint32_t epoch : 6;
   uint32_t unassigned : 2;
   
   uint32_t framelength8 : 24;	// Frame length (including header) divided by 8
   uint32_t nchan : 5;
   uint32_t version : 3;	// Set to 0 according to alma-vdif-edv.pdf; VDIF Version 0
   
   uint32_t stationid : 16;
   uint32_t threadid : 10;
   uint32_t nbits : 5;		// Set to 1 according to alma-vdif-edv.pdf
   uint32_t iscomplex : 1;	// Set to 0 according to alma-vdif-edv.pdf

   uint32_t notrelevant : 4;	// Low 4 bit of Sub(sub)-version of EDV, meaning seems to vary by particular subsubversion
   uint32_t subsubversion : 20;	// Sub(sub)-version of EDV;  ALMA = 0x0A5AE, R2DBE = 0x00000, others...?
   uint32_t eversion : 8;	// EDV 0x02

   uint32_t word5 : 32;		// use may vary
   uint32_t word6 : 32;		// use may vary
   uint32_t word7 : 32;		// use may vary
} vdif_edv2_header_generic;

typedef struct vdif_edv2_header_alma {	/* For ALMA, ratified very poorly documented on vlbi.org, incomplete document describing this from Chet to Walter via email: 20150615 */
   uint32_t seconds : 30;
   uint32_t legacymode : 1;
   uint32_t invalid : 1;
   
   uint32_t frame : 24;
   uint32_t epoch : 6;
   uint32_t unassigned : 2;
   
   uint32_t framelength8 : 24;	// Frame length (including header) divided by 8 
   uint32_t nchan : 5;
   uint32_t version : 3;	// Set to 2 (really?); VDIF Version 2
   
   uint32_t stationid : 16;
   uint32_t threadid : 10;
   uint32_t nbits : 5;
   uint32_t iscomplex : 1;

   uint32_t polblock : 1;	// 0 = X-pol PIC, 1 = Y-pol PIC
   uint32_t quadrantminus1 : 2;	// 0 = quadrant 1, 1 = quadrant 2, ..., 3 = quadrant 4
   uint32_t correlator : 1;	// 0 = 2-ant correlator, 1 = BL correlator
   uint32_t subsubversion : 20;	// Sub(sub)-version of EDV;  ALMA = 0x0A5AE
   uint32_t eversion : 8;	// EDV 0x02

   uint32_t picstatus;		// essentially undefined; "ALMA: A PIC status word (details TBD)"

   uint64_t psn;		// packet serial number
} vdif_edv2_header_alma;

typedef struct vdif_edv2_header_r2dbe { /* Not documented on vlbi.org, not ratified, just emails, and sao-eht git repo roach2.py */
   uint32_t seconds : 30;
   uint32_t legacymode : 1;
   uint32_t invalid : 1;
   
   uint32_t frame : 24;
   uint32_t epoch : 6;
   uint32_t unassigned : 2;
   
   uint32_t framelength8 : 24;	// Frame length (including header) divided by 8
   uint32_t nchan : 5;
   uint32_t version : 3;	// Set to 0 according to alma-vdif-edv.pdf; VDIF Version 0
   
   uint32_t stationid : 16;
   uint32_t threadid : 10;
   uint32_t nbits : 5;		// Set to 1 according to alma-vdif-edv.pdf
   uint32_t iscomplex : 1;	// Set to 0 according to alma-vdif-edv.pdf

   uint32_t polblock : 1;	// polarization label
   uint32_t bdcsideband : 1;	// BDC sideband (U, L)
   uint32_t rxsideband  : 1;	// Receiver sideband (U, L)
   uint32_t undefined : 1;
   uint32_t subsubversion : 20;	// Sub(sub)-version of EDV;  R2DBE = 0x00000 (apparently)
   uint32_t eversion : 8;	// EDV 0x02

   int32_t ppsdiff : 32;	// apparently, count of 1PPS int/ext diff in 256 MHz clock ticks

   uint64_t psn;		// apparently, packet serial number
} vdif_edv2_header_r2dbe;

typedef struct vdif_edv2_header {	/* For ALMA & R2DBE, incomplete document describing this from Chet to Walter via email: 20150615 */
   uint32_t seconds : 30;
   uint32_t legacymode : 1;
   uint32_t invalid : 1;
   
   uint32_t frame : 24;
   uint32_t epoch : 6;
   uint32_t unassigned : 2;
   
   uint32_t framelength8 : 24;	// Frame length (including header) divided by 8 
   uint32_t nchan : 5;
   uint32_t version : 3;	// Set to 2
   
   uint32_t stationid : 16;
   uint32_t threadid : 10;
   uint32_t nbits : 5;
   uint32_t iscomplex : 1;

   uint32_t polblock : 1;
   uint32_t quadrantminus1 : 2;	// 0 = quad 1, 1 = quad 2, ...
   uint32_t correlator : 1;	// 0 = 2-ant corr, 1 = BL corr
   uint32_t sync : 20;		// For ALMA this is set to 0xA5AE5
   uint32_t eversion : 8;

   uint32_t status;		// use may vary

   uint64_t psn;		// packet serial number
} vdif_edv2_header;

typedef struct vdif_edv3_header {	/* VLBA extensions: see http://www.vlbi.org/vdif/docs/vlbaupgradememo42.pdf */
   uint32_t seconds : 30;
   uint32_t legacymode : 1;
   uint32_t invalid : 1;
   
   uint32_t frame : 24;
   uint32_t epoch : 6;
   uint32_t unassigned : 2;
   
   uint32_t framelength8 : 24;	// Frame length (including header) divided by 8 
   uint32_t nchan : 5;
   uint32_t version : 3;
   
   uint32_t stationid : 16;
   uint32_t threadid : 10;
   uint32_t nbits : 5;
   uint32_t iscomplex : 1;

   uint32_t samprate : 23;	// in samprateunits; note this is internal complex sample rate; double for real value output
   uint32_t samprateunits : 1;	// 0 = kHz, 1 = MHz
   uint32_t eversion : 8;	// set to 3
   
   uint32_t syncword;		// 0xACABFEED
   
   uint32_t tuning;		// In DDS units of 1/2^24 MHz
   
   uint32_t personalitytype : 8;
   uint32_t minorrev : 4;	// minor part of rev number
   uint32_t majorrev : 4;	// major part of revision number
   uint32_t sideband : 1;	// 1 = upper, 0 = lower
   uint32_t subband : 3;	// subband selector (RDBE specific)
   uint32_t ifnumber : 4;	// which input IF
   uint32_t dbeunit : 4;	// which unit produced this data
   uint32_t unassigned2 : 4;
 } vdif_edv3_header;

typedef struct vdif_edv4_header {	/* proposed extension extensions: (WFB email to VDIF committee 2015/10/09) */
   uint32_t seconds : 30;
   uint32_t legacymode : 1;
   uint32_t invalid : 1;
   
   uint32_t frame : 24;
   uint32_t epoch : 6;
   uint32_t unassigned : 2;
   
   uint32_t framelength8 : 24;	// Frame length (including header) divided by 8 
   uint32_t nchan : 5;
   uint32_t version : 3;
   
   uint32_t stationid : 16;
   uint32_t threadid : 10;
   uint32_t nbits : 5;
   uint32_t iscomplex : 1;

   uint32_t dummy : 16;
   uint32_t masklength : 8;	// number of bits in the validity mask.  Should be equal to, or exact divisor of number of channels
   uint32_t eversion : 8;	// Should be set to 4
   
   uint32_t syncword;		// 0xACABFEED
   
   uint64_t validitymask;	// bits set if data is present
 } vdif_edv4_header;

enum VDIFHeaderPrintLevel	// for printVDIFHeader function
{
	VDIFHeaderPrintLevelHex = 0,	// Just dump header as hex
	VDIFHeaderPrintLevelColumns,	// Just print the column headers for VDIFHeaderPrintLevelShort
	VDIFHeaderPrintLevelShort,	// Decode header and print one-liner
	VDIFHeaderPrintLevelLong	// Print full header details
};

/* Date manipulation functions */
int ymd2doy(int yr, int mo, int day);
int ymd2mjd(int yr, int mo, int day);

/* Function to completely fill header struct, returns non-zero on error */
int createVDIFHeader(vdif_header *header, int dataarraylength, int threadid, int bits, int nchan,
		     int iscomplex, char stationid[3]);

/* Functions to grab just one value from the raw header */
static inline int getVDIFComplex(const vdif_header *header) { return (int)header->iscomplex; }
static inline int getVDIFLegacy(const vdif_header *header) { return (int)header->legacymode; }
static inline int getVDIFThreadID(const vdif_header *header) { return (int)header->threadid; }
static inline int getVDIFHeaderBytes(const vdif_header *header) { return header->legacymode ? VDIF_LEGACY_HEADER_BYTES : VDIF_HEADER_BYTES; }
static inline int getVDIFFrameBytes(const vdif_header *header) { return (int)(header->framelength8)*8; }
uint64_t getVDIFFrameMJDSec(vdif_header *header);
int getVDIFFrameMJD(const vdif_header *header);
double getVDIFFrameDMJD(const vdif_header *header, int framepersec);
static inline int getVDIFFrameSecond(const vdif_header *header) { return ((int)header->seconds)%86400; }
static inline int getVDIFFrameNumber(const vdif_header *header) { return (int)header->frame; }
static inline int getVDIFStationID(const vdif_header *header) { return (int)header->stationid; }
static inline int getVDIFBitsPerSample(const vdif_header *header) { return ((int)header->nbits+1); }
int getVDIFNumChannels(const vdif_header *header);
static inline int getVDIFFrameInvalid(const vdif_header *header) { return (int)header->invalid; }
static inline int getVDIFFrameEpochSecOffset(const vdif_header *header) { return (int)header->seconds; }
static inline int getVDIFEpoch(const vdif_header *header) { return (int)header->epoch; }
int getVDIFEpochMJD(const vdif_header *header);

/* Functions to set just one value from a raw header */
int setVDIFFrameMJD(vdif_header *header, int framemjd);
int setVDIFFrameMJDSec(vdif_header *header, uint64_t mjdsec);
static inline void setVDIFFrameEpochSecOffset(vdif_header *header, int seconds) { header->seconds = seconds; }
static inline void setVDIFFrameNumber(vdif_header *header, int framenumber) { header->frame = framenumber; }
static inline void setVDIFFrameInvalid(vdif_header *header, unsigned int invalid) { header->invalid = invalid; }
static inline void setVDIFBitsPerSample(vdif_header *header, int nbits) { header->nbits = nbits-1; }
static inline void setVDIFComplex(vdif_header *header, int iscomplex) { header->iscomplex = (iscomplex>0); }
int setVDIFFrameBytes(vdif_header *header, int bytes);
int setVDIFFrameSecond(vdif_header *header, int seconds);
int setVDIFNumChannels(vdif_header *header, int numchannels);
int setVDIFThreadID(vdif_header *header, int threadid);
int setVDIFFrameTime(vdif_header *header, time_t time);
int setVDIFEpochTime(vdif_header *header, time_t time);
static inline void setVDIFEpoch(vdif_header *header, int epoch) { header->epoch = epoch; }
int setVDIFEpochMJD(vdif_header *header, int mjd);
int nextVDIFHeader(vdif_header *header, int framepersec);
int incrementVDIFHeader(vdif_header *header, int framepersec, int64_t inc);

void fprintVDIFHeader(FILE *out, const vdif_header *header, enum VDIFHeaderPrintLevel);
void printVDIFHeader(const vdif_header *header, enum VDIFHeaderPrintLevel);


/* *** implemented in vdifbuffer.c *** */

int determinevdifframesize(const unsigned char *buffer, int bufferSize);

int determinevdifframeoffset(const unsigned char *buffer, int bufferSize, int frameSize);


/* *** implemented in cornerturners.c *** */

void (*getCornerTurner(int nThread, int nBit))(unsigned char *, const unsigned char * const *, int);


/* *** implemented in vdifmux.c *** */

#define VDIF_MUX_FLAG_GOTOEND			0x01		/* risk inability to sort in order to possibly reach end of input array */
#define VDIF_MUX_FLAG_RESPECTGRANULARITY	0x02		/* always produce output startFrame that is multiple of frame granularity */
#define VDIF_MUX_FLAG_ENABLEVALIDITY		0x04		/* if set, throw away VDIF frames coming in with invalid bit set */
#define	VDIF_MUX_FLAG_INPUTLEGACY		0x08		/* if set, accept LEGACY frames; NOT YET IMPLEMENTED */
#define	VDIF_MUX_FLAG_OUTPUTLEGACY		0x10		/* if set, produce LEGACY frames; NOT YET IMPLEMENTED */
#define VDIF_MUX_FLAG_COMPLEX			0x20		/* if set, data is complex (so 2x as many bits per logical sample) */
#define VDIF_MUX_FLAG_PROPAGATEVALIDITY		0x40		/* if set, change output VDIF to EDV 4 with per-input-thread validity */


struct vdif_mux {
  int inputFrameSize;					/* size of one input data frame, inc header */
  int inputDataSize;					/* size of one input data frame, without header */
  int outputFrameSize;					/* size of one output data frame, inc header */
  int outputDataSize;					/* size of one output data frame, without header */
  int inputFramesPerSecond;				/* per thread */
  int inputChannelsPerThread;				/* default is 1, unless this is changed with setvdifmuxinputchannels() */
  int bitsPerSample;					/* per sample (a complex number is considered 2 samples here) */
  int nThread;
  int nSort;						/* number of input frames over which out-of-order is to be tolerated */
  int nGap;						/* frame number jump to tolerate before returning with incomplete muxing */
  int frameGranularity;
  int nOutputChan;					/* nThread rounded up to nearest power of 2, then multiplied by input chans per thread */
  int complexFactor;					/* should be 1 (real) or 2 (complex).  Used in selecting cornerturner */
  int fanoutFactor;					/* if > 1 _and_ if input frames have a single channel, will combine multiple threads into a single output channel; this is for DBBC3 */
  unsigned int flags;
  uint16_t chanIndex[VDIF_MAX_THREAD_ID+1];		/* map from threadId to channel number (0 to nThread-1) */
  uint64_t goodMask;
  void (*cornerTurner)(unsigned char *, const unsigned char * const *, int);
};

struct vdif_mux_statistics {
  /* The first 8 accumulate over multiple calls to vdifmux */
  long long nValidFrame;		/* number of valid VDIF input frames encountered */
  long long nInvalidFrame;		/* number of real VDIF frames discarded because of invalid bit being set */
  long long nDiscardedFrame;		/* number of valid input frames discarded because of out-of-order issues */
  long long nWrongThread;		/* number of otherwise good frames with incorrect thread */
  long long nSkippedByte;		/* number of bytes skipped (interloper frames) */
  long long nFillByte;			/* counts number of bytes skipped that were identified as fill pattern */
  long long nDuplicateFrame;		/* number of frames found with the same time & thread */
  long long bytesProcessed;		/* total bytes consumed from */
  long long nGoodFrame;			/* number of fully usable output frames */
  long long nPartialFrame;		/* number of partial frames produced (EDV4 only) */
  long long nFillerFrame;		/* number of filler frames produced */
  int nCall;				/* how many calls to vdifmux since last reset */
  int nOutOfDataConditions;             /* how many calls did not have enough source data to produce any output */

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
  int epoch;			/* from first header */
  int64_t startFrameNumber;

  /* start time of output data */
  /* duration of output data */
};

/* return 0 on success, or code on error */
int configurevdifmux(struct vdif_mux *vm, int inputFrameSize, int inputFramesPerSecond, int bitsPerSample, int nThread, const int *threadIds, int nSort, int nGap, int flags);

int setvdifmuxinputchannels(struct vdif_mux *vm, int inputChannelsPerThread);
int setvdifmuxfanoutfactor(struct vdif_mux *vm, int fanoutFactor);

void printvdifmux(const struct vdif_mux *vm);

int vdifmux(unsigned char *dest, int destSize, const unsigned char *src, int srcSize, const struct vdif_mux *vm, int64_t startOutputFrameNumber, struct vdif_mux_statistics *stats);

void printvdifmuxstatistics(const struct vdif_mux_statistics *stats);

void resetvdifmuxstatistics(struct vdif_mux_statistics *stats);

void testvdifcornerturners(int outputBytes, int nTest);


/* *** implemented in vdiffile.c *** */

struct vdif_file_summary {
  char fileName[VDIF_SUMMARY_FILE_LENGTH];  
  long long fileSize;	/* [bytes] */
  int nBit;
  int nThread;
  int threadIds[VDIF_SUMMARY_MAX_THREADS];
  int frameSize;	/* [bytes], including header */
  int framesPerSecond;	/* set to 0 if not known */
  int epoch;		/* from VDIF header */
  int startSecond;	/* from earliest VDIF header */
  int startFrame;	/* since startSecond, from VDIF header */
  int endSecond;	/* from latest VDIF header */
  int endFrame;		/* since endSecond, from VDIF header */
  int firstFrameOffset;	/* bytes to skip to get to first valid frame */
};

void resetvdiffilesummary(struct vdif_file_summary *sum);

void printvdiffilesummary(const struct vdif_file_summary *sum);

void snprintvdiffilesummary(char *str, int maxLength, const struct vdif_file_summary *sum);

static inline void vdiffilesummarysetsamplerate(struct vdif_file_summary *sum, int64_t sampRateHz) { sum->framesPerSecond = sampRateHz*sum->nBit/(8*(sum->frameSize-VDIF_HEADER_BYTES)); }

/* does same as above, but with different input */
static inline void vdiffilesummarysetbitrate(struct vdif_file_summary *sum, int bitRateMbps) { sum->framesPerSecond = bitRateMbps*125000/(sum->nThread*(sum->frameSize-VDIF_HEADER_BYTES)); }

/* returns samples per second */
static inline int vdiffilesummarygetsamplerate(const struct vdif_file_summary *sum) { return sum->framesPerSecond*(sum->frameSize-VDIF_HEADER_BYTES)*8/sum->nBit; }

/* returns Mbps */
static inline int vdiffilesummarygetbitrate(const struct vdif_file_summary *sum) { return sum->framesPerSecond*(sum->frameSize-VDIF_HEADER_BYTES)*sum->nThread/125000; }

static inline int vdiffilesummarygetstartns(const struct vdif_file_summary *sum) { return (int)((int64_t)(sum->startFrame)*1000000000LL/sum->framesPerSecond); }

static inline int vdiffilesummarygetstartsecond(const struct vdif_file_summary *sum) { return sum->startSecond % 86400; }

int vdiffilesummarygetstartmjd(const struct vdif_file_summary *sum);

static inline int vdiffilesummarygetbytespersecond(const struct vdif_file_summary *sum) { return sum->frameSize*sum->framesPerSecond; }

int summarizevdiffile(struct vdif_file_summary *sum, const char *fileName, int frameSize);


/* *** implemented in vdiffilereader.c *** */

// Assistive VDIF reader. Allows opening a (multi-threaded) VDIF file and reading
// it back with the frames of threads tightly "interleaved". The resulting data
// may be passed to vdifmux().
//
// The file-based VDIF reader with pre-interleaving logic helps to counteract an
// issue stemming from the comparably tiny memory window that vdifmux() operates on.
//
// When the underlying VDIF file is highly "clumpy" and its data are passed directly
// to vdifmux(), it can happen that a thread is not found at all inside the small
// window that vdifmux() operates on. This leads to outlier frames erroneusly considered
// as "missing" and results in excess Invalid -marked VDIF frames from vdifmux().

struct vdif_file_reader {
  struct vdif_file_summary details;
  FILE *fd[VDIF_SUMMARY_MAX_THREADS];
  uint32_t frame[VDIF_SUMMARY_MAX_THREADS];
  uint32_t sec[VDIF_SUMMARY_MAX_THREADS];
  off_t firstframeoffset;
  off_t offset;
};

struct vdif_file_reader_stats {
  int nThread;
  off_t threadOffsets[VDIF_SUMMARY_MAX_THREADS];
  off_t maxOffset;
};

/** Assistive VDIF reader, open a VDIF file based upon VDIF details in its summary. */
int vdifreaderOpen(const struct vdif_file_summary *sum, struct vdif_file_reader *rd);

/** Read VDIF file, de-clumping VDIF threads in the process. */
size_t vdifreaderRead(struct vdif_file_reader *rd, void *buf, size_t count);

/** Seek the VDIF reader */
size_t vdifreaderSeek(struct vdif_file_reader *rd, size_t offset);

/** Statistics to help deduce VDIF thread "clumpiness" */
int vdifreaderStats(struct vdif_file_reader *rd, struct vdif_file_reader_stats *st);

/** Close the VDIF reader */
int vdifreaderClose(struct vdif_file_reader *rd);

#ifdef __cplusplus
}
#endif

#endif
