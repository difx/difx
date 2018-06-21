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
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <pthread.h>
#include "vdifio.h"

#ifdef __APPLE__
#include "pthreadbarrier.h"
#endif

#define MARK6_SYNC		0xfeed6666
#define MAX_VDIF_MUX_SLOTS	64
#define MARK6_BUFFER_SLOTS	10

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
	int payloadBytes;			/* [bytes] actual number of payload bytes (usually == payload_size) */
	int index;				/* index into data[] */
	uint64_t frame;				/* from VDIF header */
	char *data;				/* points to payload within buffer */
	Mark6BlockHeader_ver2 blockHeader;	/* header corresponding to recent data */
} Mark6BufferSlot;

typedef struct
{
	FILE *in;				/* actual file descriptor */
	char *fileName;
	int version;				/* from Mark6Header */
	int maxBlockSize;			/* from Mark6Header */
	int blockHeaderSize;			/* [bytes] from mark6BlockHeaderSize() */
	int packetSize;				/* [bytes] from Mark6Header */
	struct stat stat;			/* stat, as read before file open */
	int32_t block1, block2;			/* set at open: the first two block numbers in the file */

	Mark6BufferSlot slot[MARK6_BUFFER_SLOTS];	/* Allow MARK6_BUFFER_SLOTS blocks to be visible to gatherer at once */

	/* some parallel-read infrastructure */
	int stopReading;			/* if > 0, get out of read loop */
	pthread_t readThread;
	pthread_barrier_t readBarrier;
	Mark6BlockHeader_ver2 readHeader;	/* header corresponding to recent data */
	char *readBuffer;			/* same size as data[] above */
	int readBytes;				/* how many bytes were actually read */


} Mark6File;

typedef struct
{
        int nFile;
	Mark6File *mk6Files;
	int packetSize;
	char *activeVSN;
} Mark6Gatherer;



const char *mark6PacketFormat(int formatId);

int mark6BlockHeaderSize(int version);

void printMark6Header(const Mark6Header *header);


int openMark6File(Mark6File *m6f, const char *filename);

int closeMark6File(Mark6File *m6f);

void printMark6File(const Mark6File *m6f);


Mark6Gatherer *newMark6Gatherer();

Mark6Gatherer *openMark6Gatherer(int nFile, char **fileList);

off_t getMark6GathererFileSize(const Mark6Gatherer *m6g);

/* pass, e.g., /mnt/disks/?/?/data/exp1_stn1_scan1.vdif */
Mark6Gatherer *openMark6GathererFromTemplate(const char *fileTemplate);

int addMark6GathererFiles(Mark6Gatherer *m6g, int nFile, char **fileList);

int closeMark6Gatherer(Mark6Gatherer *m6g);

int isMark6GatherComplete(const Mark6Gatherer *m6g);

void printMark6Gatherer(const Mark6Gatherer *m6g);

int seekMark6Gather(Mark6Gatherer *m6g, off_t position);

int mark6Gather(Mark6Gatherer *m6g, void *buf, size_t count);


/* scan name should be the template file to match */
int summarizevdifmark6(struct vdif_file_summary *sum, const char *scanName, int frameSize);

const char *getMark6Root();

int getMark6FileList(char ***fileList);

int getMark6SlotFileList(int slot, char ***fileList);



#ifdef __cplusplus
}
#endif

#endif
