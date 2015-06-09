/***************************************************************************
 *  Copyright (C) 2015 by Walter Brisken                                   *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/vdifio/trunk/src/vdifio.c $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#ifndef __VDIF_MARK6_H__
#define __VDIF_MARK6_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include "vdifio.h"

#define MARK6_SYNC	0xfeed6666

typedef struct
{
	uint32_t sync_word;	/* nominally the number above */
	int32_t version;
	int block_size;		/* taken to be the largest block size to be encountered.  Can be less though.  Use wb_size on a per-packet basis. */
	int packet_format;
	int packet_size;
} Mark6Header;

typedef struct
{
	int32_t blocknum;	/* block number.  Note: when subblocks are used (multiple simulaneous output streams) each stream will share the sequence of block numbers */
	int32_t wb_size;	/* size of a written block, including this header */
} Mark6BlockHeader_ver2;

typedef struct
{
	int32_t blocknum;
} Mark6BlockHeader_ver1;

typedef struct
{
	FILE *in;				/* actual file descriptor */
	char *fileName;
	int version;				/* from Mark6Header */
	int maxBlockSize;			/* from Mark6Header */
	int blockHeaderSize;			/* [bytes] from mark6BlockHeaderSize() */
	int packetSize;				/* [bytes] from Mark6Header */
	int payloadBytes;			/* [bytes] actual number of payload bytes (usually == payload_size) */
	char *data;				/* points to payload within buffer */
	Mark6BlockHeader_ver2 blockHeader;	/* header corresponding to recent data */
} Mark6File;

typedef struct
{
	int nFile;
	Mark6File *mk6Files;
	int currentFileNum;		/* -1 if none, or 0 to nFile-1 */
	int currentBlockNum;		/* -1 on init */
	int index;			/* index to buffer of currentFileNum */
} Mark6Descriptor;


const char *mark6PacketFormat(int formatId);

int mark6BlockHeaderSize(int version);

void printMark6Header(const Mark6Header *header);


int openMark6File(Mark6File *m6f, const char *filename);

int closeMark6File(Mark6File *m6f);

void printMark6File(const Mark6File *m6f);

ssize_t Mark6FileReadBlock(Mark6File *m6f);


Mark6Descriptor *openMark6(int nFile, char **fileList);

int closeMark6(Mark6Descriptor *m6d);

void printMark6(const Mark6Descriptor *m6d);

ssize_t readMark6(Mark6Descriptor *m6d, void *buf, size_t count);


/* *** implemented in vdifmark6mux.c *** */

/* used to mux multiple Mark6 files together, possibly muxing threads, streams, and interleaved samples */
/* Assumed constant for all input files: nBit, nChan/thread, same interleave */
/* If interleaved samples (a la DDC3), nChan/thread must = 1 */

/* This structure can be constructed with the function configurevdifmark6mux() which reads a template file and a file parameter that replaces a wildcard */

struct vdif_mark6_mux_stream
{
	Mark6Descriptor *m6d;				/* handle for a block of mark6 data files */
	uint16_t slotIndex[VDIF_MAX_THREAD_ID+1];	/* map from threadId to multiplexed data slot */
};

/* Unclear if this is needed yet */
struct vdif_mark6_mux_slot
{
	int streamId;
	int threadId;
};

struct vdif_mark6_mux
{
	int inputFrameSize;			/* size of one input data frame, inc header */
	int inputDataSize;			/* size of one input data frame, without header */
	int outputFrameSize;			/* size of one output data frame, inc header */
	int outputDataSize;			/* size of one output data frame, without header */
	int inputFramesPerSecond;		/* per thread */
	int bitsPerSample;			/* per sample */
	int bitsPerSlot;			/* effectively bitsPerSample * chans per thread */
	int nThread;
	int nSort;
	int nGap;
	int frameGranularity;
	int nOutputChan;			/* rounded up to nearest power of 2 */

	int nStream;
	struct vdif_mark6_mux_stream *streams;

	int nSlot;
	struct vdif_mark6_mux_slot *slots;

	uint64_t goodMask;			/* specify criterion for successful reassembly.  bit field: set to 1 per slot desired. */
};

struct vdif_mark6_mux_statistics
{
};

struct vdif_mark6_mux *configurevdifmark6mux(const char *templateFilename, const char *fileParameter);

void deletevdifmark6mux(struct vdif_mark6_mux *vm);

void printvdifmark6mux(const struct vdif_mark6_mux *vm);

int vdifmark6mux(unsigned char *dest, int destSize, const unsigned char **src, const int *srcSize, const struct vdif_mark6_mux *vm, int64_t startOutputFrameNumber, struct vdif_mark6_mux_statistics *stats);

struct vdif_mark6_mux_statistics *newvdifmark6muxstatistics(const struct vdif_mark6_mux *vm);

void deletevdifmark6muxstatistics(struct vdif_mark6_mux_statistics *stats);

void printvdifmark6muxstatistics(const struct vdif_mark6_mux_statistics *stats);

void resetvdifmark6muxstatistics(struct vdif_mark6_mux_statistics *stats);

#ifdef __cplusplus
}
#endif

#endif
