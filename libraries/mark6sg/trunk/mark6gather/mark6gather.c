/***************************************************************************
 *  Copyright (C) 2015-2020 by Walter Brisken                              *
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
// $Id: vdifmark6.c 8356 2018-06-21 20:23:00Z MarkWainright $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/vdifio/trunk/src/vdifio.c $
// $LastChangedRevision: 8356 $
// $Author: MarkWainright $
// $LastChangedDate: 2018-06-21 15:23:00 -0500 (Thu, 21 Jun 2018) $
//
//============================================================================

#include <stdio.h>
#define _GNU_SOURCE
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <glob.h>
#include "mark6gather.h"

/* Macro to turn an expanded macro into a string */
#define str(s) #s

#define M6SG_PACKET_FORMAT_VDIF   0
#define M6SG_PACKET_FORMAT_MARK5B 1
#define M6SG_PACKET_FORMAT_UNK    2

/* Note: this is actually defined in vdifio.h, but vdifio has mark6sg as dependency, so this just allows the small amount of local VDIF functionality to properly build */
typedef struct vdif_header
{
	uint32_t seconds : 30;
	uint32_t legacymode : 1;
	uint32_t invalid : 1;

	uint32_t frame : 24;
	uint32_t epoch : 6;
	uint32_t unassigned : 2;

	uint32_t framelength8 : 24;  // Frame length (including header) divided by 8 
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

const char DefaultMark6Root[] = "/mnt/disks/*/*/data";
const char DefaultMark6Meta[] = "/mnt/disks/.meta/"; // path, not wildcard

static inline int getVDIFFrameEpochSecOffset(const vdif_header *header)
{
	return (int)header->seconds;
}

static inline int getVDIFFrameNumber(const vdif_header *header)
{
	return (int)header->frame;
}

static inline uint64_t vdifFrame(vdif_header *vh)
{
	return ((uint64_t)(getVDIFFrameEpochSecOffset(vh)) << 24LL) | getVDIFFrameNumber(vh); 
}

static inline uint64_t mark5bFrame(char *data)
{
	unsigned char *udata;
	int second;
	int nFrame;
	udata = (unsigned char*)data;
	second = (udata[10] & 0x0F)*10000 + (udata[9] >> 4)*1000 + (udata[9] & 0x0F)*100 + (udata[8] >> 4)*10 + (udata[8] & 0x0F);
	nFrame = (udata[5] * 256) + udata[4];
	return ((uint64_t)(second) << 24LL) | nFrame; 
}

static inline int checkMark5BPacket(char *data)
{
	unsigned char *udata;
	udata = (unsigned char*)data;
	if(udata[0] == 0xED && udata[1] == 0xDE && udata[2] == 0xAD && udata[3] == 0xAB)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

static inline int checkFillData(char *data)
{
	const unsigned char filltest[8] = { 0x44, 0x33, 0x22, 0x11, 0x44, 0x33, 0x22, 0x11 };
	unsigned char *udata;
	int i;

	udata = (unsigned char*)data;
	for(i = 0; i < 16; i++)
	{
		if(memcmp(filltest, udata + i, 8) == 0)
		{
			return 1;
		}
	}

	return 0;
}

const char *mark6PacketFormat(int formatId)
{
	if(formatId == M6SG_PACKET_FORMAT_VDIF)
	{
		return "VDIF";
	}
	else if(formatId == M6SG_PACKET_FORMAT_MARK5B)
	{
		return "Mark5b";
	}
	else if(formatId == M6SG_PACKET_FORMAT_UNK)
	{
		return "Unknown";
	}
	else
	{
		return "Illegal value!";
	}
}

int mark6BlockHeaderSize(int version)
{
	if(version == 1)
	{
		return sizeof(Mark6BlockHeader_ver1);
	}
	else if(version == 2)
	{
		return sizeof(Mark6BlockHeader_ver2);
	}
	else
	{
		return -1;
	}
}

void printMark6Header(const Mark6Header *header)
{
	printf("Mark6 header\n");
	printf("  sync_word = 0x%8x%s\n", header->sync_word, (header->sync_word == MARK6_SYNC) ? "" : " which is weird; it should be " str(MARK6_SYNC) );
	printf("  version = %d\n", header->version);
	printf("  block_size = %d\n", header->block_size);
	printf("  packet_format = %d = %s\n", header->packet_format, mark6PacketFormat(header->packet_format));
	printf("  packet_size = %d\n", header->packet_size);
}

/* gets two blocks from file */
/* FIXME: do some error checking if file size is too short for this! */
static int getFirstBlocks(Mark6File *m6f)
{
	int32_t size;
	off_t offset;
	int v;

	/* get some information about blocks */
	offset = ftello(m6f->in);
	if(offset < 0)
	{
		return -1;
	}

	v = fread(&m6f->block1, sizeof(m6f->block1), 1, m6f->in);
	if(v < 1)
	{
		return -2;
	}
	if(m6f->version == 1)
	{
		size = m6f->maxBlockSize;
	}
	else
	{
		v = fread(&size, sizeof(size), 1, m6f->in);
		if(v < 1)
		{
			return -3;
		}
	}

	v = fseeko(m6f->in, offset + size, SEEK_SET);
	if(v != 0)
	{
		return -4;
	}
	v = fread(&m6f->block2, sizeof(m6f->block1), 1, m6f->in);
	if(v < 1)
	{
		return -5;
	}

	v = fseeko(m6f->in, offset, SEEK_SET);
	if(v != 0)
	{
		return -6;
	}

	return 0;
}

static void *mark6Reader(void *arg)
{
	Mark6File *m6f = (Mark6File *)arg;
	int corrupted = 0;

	while(1)
	{
		size_t v;

		pthread_barrier_wait(&m6f->readBarrier);

		if(!corrupted)
		{
			v = fread(&m6f->readHeader, m6f->blockHeaderSize, 1, m6f->in);
			if(v == 1)
			{
				int nrd;
				
				nrd = m6f->readHeader.wb_size - m6f->blockHeaderSize;
				if(nrd <= 0 || nrd > m6f->maxBlockSize - m6f->blockHeaderSize)
				{
					fprintf(stderr, "Warning: corrupt scatter-gather file! Size %d of current block exceeds maxBlockSize %d\n", m6f->readHeader.wb_size, m6f->maxBlockSize);
					memset(m6f->readBuffer, 0xFF, m6f->maxBlockSize - m6f->blockHeaderSize);
					m6f->readBytes = 0;
					corrupted = 1;
				}
				else
				{
					m6f->readBytes = fread(m6f->readBuffer, 1, m6f->readHeader.wb_size - m6f->blockHeaderSize, m6f->in);
				}
			}
			else
			{
				m6f->readBytes = 0;
			}
		}

		pthread_barrier_wait(&m6f->readBarrier);

		if(m6f->stopReading)
		{
			break;
		}
	}

	return 0;
}

/* position is the position of the reconstructed stream to seek to */
struct seekArgs
{
	Mark6File *m6f;
	off_t position;		/* SEEK_SET argument */
	int nFile;		/* number of files in the fileset */
	int fileIndex;		/* index of this file [0, nFile) */
};

/* Returns 0 on EOF */
static ssize_t Mark6FileReadBlock(Mark6File *m6f, int slotIndex)
{
	Mark6BufferSlot *slot;

	slot = m6f->slot + slotIndex;

	pthread_barrier_wait(&m6f->readBarrier);

	slot->payloadBytes = m6f->readBytes;

	if(slot->payloadBytes == 0)
	{
		slot->index = 0;
		slot->frame = 0;
	}
	else
	{
		char *tmp;
		vdif_header *vh;
		
		slot->payloadBytes -= (slot->payloadBytes % m6f->packetSize);

		memcpy(&slot->blockHeader, &m6f->readHeader, m6f->blockHeaderSize);
		
		tmp = slot->data;
		slot->data = m6f->readBuffer;
		m6f->readBuffer = tmp;

		slot->index = 0;

		if(m6f->packetFormat == M6SG_PACKET_FORMAT_VDIF)
		{
			vh = (vdif_header *)(slot->data);
			slot->frame = vdifFrame(vh);
		}
		else
		{
			if(!checkMark5BPacket(slot->data))
			{
				fprintf(stderr, "Error: Mark6FileReadBlock: file header claims data are Mark5B, yet frame header lacks Mark5B header magic\n");
			}
			slot->frame = mark5bFrame(slot->data);
		}
	}

	pthread_barrier_wait(&m6f->readBarrier);

	return slot->payloadBytes;
}

static void *mark6Seeker(void *arg)
{
	struct seekArgs *S = (struct seekArgs *)arg;
	off_t pos;
	long long int blockSize;
	long long int targetBlock;
	int slotIndex;
	int i;
	size_t v;

	blockSize = S->m6f->maxBlockSize - ((S->m6f->maxBlockSize-S->m6f->blockHeaderSize) % S->m6f->packetSize);
	targetBlock = S->m6f->stat.st_size/(blockSize - S->m6f->blockHeaderSize)/S->nFile;

	/*   1. wait for any ongoing reads to complete */
	pthread_barrier_wait(&S->m6f->readBarrier);

	/*   2. figure out where we need to be */ 
	if(S->position == 0)
	{
		pos = sizeof(Mark6Header);
	}
	else if(S->m6f->stat.st_size < blockSize + sizeof(Mark6Header))
	{
		pos = sizeof(Mark6Header);
	}
	else
	{
		targetBlock = S->position/(blockSize - S->m6f->blockHeaderSize);
		pos = sizeof(Mark6Header) + blockSize*(targetBlock/S->nFile);

		if(pos >= S->m6f->stat.st_size)
		{
			pos = sizeof(Mark6Header) + blockSize*targetBlock;
		}

		/* Iterate up to 5 times to get a better position */
		for(i = 0; i < 5; ++i)
		{
			int32_t block;
			int deltaBlock;

			fseeko(S->m6f->in, pos, SEEK_SET);
			v = fread(&block, sizeof(int32_t), 1, S->m6f->in);
			if(v != 1)
			{
				pos -= blockSize;
				fprintf(stderr, "Error: seek: fileindex=%d  i=%d  pos=%Ld : read of block number failed\n", S->fileIndex, i, (long long int)pos);
			}
			
			deltaBlock = block - targetBlock;

			/* if it is within 4 blocks of the target and positioned earlier than that, then call it good enough */
			if(deltaBlock <= 0 && deltaBlock > -(4*S->nFile))
			{
				break;
			}

			if(deltaBlock < S->nFile && deltaBlock > 0)
			{
				pos -= blockSize;
			}
			else
			{
				pos -= blockSize * (deltaBlock/S->nFile);
			}
			if(pos < 0)
			{
				pos = sizeof(Mark6Header);
			}
			if(pos >= S->m6f->stat.st_size)
			{
				pos -= blockSize;
			}
		}
	}

	/*   3. advance until we are at, or just beyond target block number */
	for(i = 0; i < 2*S->nFile; ++i)
	{
		int32_t block;
		
		fseeko(S->m6f->in, pos, SEEK_SET);
		
		v = fread(&block, sizeof(int32_t), 1, S->m6f->in);
		if(block >= targetBlock)
		{
			break;
		}
		
		pos += blockSize;
	}
	fseeko(S->m6f->in, pos, SEEK_SET);

	/*   4. give control back to reader thread */
	pthread_barrier_wait(&S->m6f->readBarrier);

	/*   5. explicitly load the next block for each slot */
	for(slotIndex = 0; slotIndex < MARK6_BUFFER_SLOTS; ++slotIndex)
	{
		Mark6FileReadBlock(S->m6f, slotIndex);
	}

	return 0;
}

static void deallocateMark6File(Mark6File *m6f)
{
	int s;

	if(m6f->in)
	{
		fclose(m6f->in);
		m6f->in = 0;
	}
	if(m6f->fileName)
	{
		free(m6f->fileName);
		m6f->fileName = 0;
	}
	for(s = 0; s < MARK6_BUFFER_SLOTS; ++s)
	{
		if(m6f->slot[s].data)
		{
			free(m6f->slot[s].data);
			m6f->slot[s].data = 0;
		}
	}
	if(m6f->readBuffer)
	{
		free(m6f->readBuffer);
		m6f->readBuffer = 0;
	}
	m6f->version = -1;
}

/* this assumes *m6f is already allocated but that the structures within are not. */
/* no attempt is made here to free existing data */

/* returns 0 on success, or error code otherwise */
int openMark6File(Mark6File *m6f, const char *filename)
{
	Mark6Header header;
	pthread_attr_t attr;
	int slotIndex;
	size_t v;

	stat(filename, &m6f->stat);
	m6f->in = fopen(filename, "r");
	if(!m6f->in)
	{
		deallocateMark6File(m6f);

		return -1;
	}
	m6f->fileName = strdup(filename);
	v = fread(&header, sizeof(header), 1, m6f->in);
	if(v!=1)
	{
		deallocateMark6File(m6f);

		return -6;
	}
	m6f->version = header.version;
	m6f->blockHeaderSize = mark6BlockHeaderSize(header.version);
	if(m6f->blockHeaderSize <= 0)
	{
		deallocateMark6File(m6f);

		return -2;
	}
	m6f->readHeader.wb_size = m6f->maxBlockSize;
	m6f->maxBlockSize = header.block_size;
	m6f->packetSize = header.packet_size;
	m6f->packetFormat = header.packet_format;
	m6f->readBuffer = (char *)malloc(m6f->maxBlockSize-m6f->blockHeaderSize);
	if(!m6f->readBuffer)
	{
		deallocateMark6File(m6f);

		return -5;
	}

	if(m6f->packetSize == 0 || m6f->maxBlockSize <= 16)
	{
		// TODO: try harder to figure out packetSize?
		// There are a few rare test recordings that have packet_size=0 block_size=8 in the file header,
		// their actual blocks have proper wb_size, but the "modulo by zero packet_size" in this library leads to segfaults.
		deallocateMark6File(m6f);

		return -7;
	}

	if(getFirstBlocks(m6f) < 0)
	{
		deallocateMark6File(m6f);

		return -4;
	}

	for(slotIndex = 0; slotIndex < MARK6_BUFFER_SLOTS; ++slotIndex)
	{
		Mark6BufferSlot *slot;

		slot = m6f->slot + slotIndex;

		slot->data = (char *)malloc(m6f->maxBlockSize-m6f->blockHeaderSize);
		if(!slot->data)
		{
			deallocateMark6File(m6f);

			return -3;
		}

		slot->blockHeader.blocknum = -1;
		slot->blockHeader.wb_size = m6f->maxBlockSize;

		slot->payloadBytes = 0;	/* nothing read yet */
	}

	/* start reading thread */
	pthread_barrier_init(&m6f->readBarrier, 0, 2);
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
	pthread_create(&m6f->readThread, &attr, mark6Reader, m6f);
	pthread_attr_destroy(&attr);
	pthread_barrier_wait(&m6f->readBarrier);

	return 0;
}

/* m6f is not freed itself in this call, but all internal structures are freed. */
int closeMark6File(Mark6File *m6f)
{
	if(!m6f)
	{
		fprintf(stderr, "Error: closeMark6File called with null pointer\n");

		return -1;
	}
	m6f->stopReading = 1;
	if(m6f->in)
	{
		pthread_barrier_wait(&m6f->readBarrier);
		pthread_join(m6f->readThread, 0);
	}

	deallocateMark6File(m6f);

	return 0;
}

void printMark6File(const Mark6File *m6f)
{
	unsigned char *udata;

	printf("Mark6 file:\n");
	if(m6f == 0)
	{
		printf("  Null\n");
	}
	else
	{
		int s;
		printf("  Filename = %s\n", m6f->fileName);
		printf("  Mark6 version = %d\n", m6f->version);
		printf("  Max block size = %d\n", m6f->maxBlockSize);
		printf("  Block header size = %d\n", m6f->blockHeaderSize);
		printf("  First two block numbers = %d, %d\n", m6f->block1, m6f->block2);
		printf("  File size = %lld\n", (long long)(m6f->stat.st_size));
		printf("  Packet size = %d\n", m6f->packetSize);

		for(s = 0; s < MARK6_BUFFER_SLOTS; ++s)
		{
			printf("  Slot %d\n", s);
			printf("    Payload bytes = %d (bytes currently residing in core)\n", m6f->slot[s].payloadBytes);
			printf("    Current block number = %d\n", m6f->slot[s].blockHeader.blocknum);
			printf("    Current index within block = %d\n", m6f->slot[s].index);
			if(m6f->packetFormat == M6SG_PACKET_FORMAT_VDIF)
			{
				printf("    Second = %d\n", (int)(m6f->slot[s].frame >> 24));
				printf("    Frame in second = %d\n", (int)(m6f->slot[s].frame & 0xFFFFFF));
			}
			else
			{
				udata = (unsigned char*)m6f->slot[s].data;
				printf("    Second = %d\n", (udata[10] & 0x0F)*10000 + (udata[9] >> 4)*1000 + (udata[9] & 0x0F)*100 + (udata[8] >> 4)*10 + (udata[8] & 0x0F));
				printf("    Frame in second = %d\n", (udata[5] * 256) + udata[4]);
			}
		}
	}
}


Mark6Gatherer *newMark6Gatherer()
{
	Mark6Gatherer *m6g;

	m6g = (Mark6Gatherer *)calloc(1, sizeof(Mark6Gatherer));
	if(!m6g)
	{
		fprintf(stderr, "Error: cannot allocate %d bytes for Mark6Gatherer\n", (int)(sizeof(Mark6Gatherer)));

		return 0;
	}

	m6g->nFile = 0;
	m6g->mk6Files = 0;
	m6g->packetSize = 0;
	m6g->activeVSN = malloc(sizeof(char) * 10);

	return m6g;
}

off_t getMark6GathererFileSize(const Mark6Gatherer *m6g)
{
	off_t size = 0;
	int i;

	for(i = 0; i < m6g->nFile; ++i)
	{
		Mark6File *m6f;
		int64_t blockSize;
		int64_t n;

		m6f = m6g->mk6Files + i;
		blockSize = m6f->maxBlockSize - ((m6f->maxBlockSize-m6f->blockHeaderSize) % m6f->packetSize);
		n = (m6f->stat.st_size - sizeof(Mark6Header))/blockSize;
		if(((m6f->stat.st_size - sizeof(Mark6Header)) % blockSize) != 0)
		{
			n += 1;
		}
		size += m6f->stat.st_size - sizeof(Mark6Header) - (n * m6f->blockHeaderSize);
	}

	return size;
}

off_t getMark6GathererSourceFileSize(const Mark6Gatherer *m6g)
{
	off_t size = 0;
	int i;

	for(i = 0; i < m6g->nFile; ++i)
	{
		size += m6g->mk6Files[i].stat.st_size;
	}

	return size;
}

Mark6Gatherer *openMark6Gatherer(int nFile, char **fileList)
{
	Mark6Gatherer *m6g;
	int nBad;

	m6g = newMark6Gatherer();
	if(!m6g)
	{
		fprintf(stderr, "Error: cannot create empty Mark6Gatherer\n");

		return 0;
	}

	nBad = addMark6GathererFiles(m6g, nFile, fileList);
	if(nBad > 0)
	{
		fprintf(stderr, "Cannot create Mark6Gatherer because %d/%d files could not be opened\n", nBad, nFile);
		closeMark6Gatherer(m6g);

		return 0;
	}
	m6g->packetSize = m6g->mk6Files[0].packetSize;

	return m6g;
}

/* pass, e.g., /mnt/disks/?/?/data/exp1_stn1_scan1.vdif */
Mark6Gatherer *openMark6GathererFromTemplate(const char *fileTemplate)
{
	const int MaxFilenameSize = 256;
	char fileName[MaxFilenameSize];
	glob_t G;
	int v;
	Mark6Gatherer *m6g;

	if(fileTemplate[0] == '/')
	{
		snprintf(fileName, MaxFilenameSize, "%s", fileTemplate);
	}
	else
	{
		snprintf(fileName, MaxFilenameSize, "%s/%s", getMark6Root(), fileTemplate);
	}

	v = glob(fileName, GLOB_NOSORT, 0, &G);
	if(v == 0)
	{
		m6g = openMark6Gatherer(G.gl_pathc, G.gl_pathv);

		globfree(&G);
	}
	else
	{
		fprintf(stderr, "Cannot create Mark6Gatherer because %s matched no files\n", fileName);

		return 0;
	}

	if(m6g)
	{
		if(m6g->nFile == 0)
		{
			closeMark6Gatherer(m6g);

			return 0;
		}

		m6g->packetSize = m6g->mk6Files[0].packetSize;
	}

	return m6g;
}

int addMark6GathererFiles(Mark6Gatherer *m6g, int nFile, char **fileList)
{
	int i;
	int nBad = 0;
	int startFile;
	int gotVSN = 0;
	char slot = ' ';
	char disk = ' ';
	FILE *fmsn;
	char msnname[30];

	startFile = m6g->nFile;
	m6g->nFile += nFile;

	m6g->mk6Files = (Mark6File *)realloc(m6g->mk6Files, m6g->nFile*sizeof(Mark6File));
	if(!m6g->mk6Files)
	{
		fprintf(stderr, "Error: cannot (re)allocate %d * %d bytes for Mark6Files\n", nFile, (int)sizeof(Mark6File));
	
		return 0;
	}
	memset(m6g->mk6Files + startFile, 0, nFile*sizeof(Mark6File));

	for(i = 0; i < nFile; ++i)
	{
		int v;

		v = openMark6File(m6g->mk6Files + startFile + i, fileList[i]);

		if(v < 0)
		{
			fprintf(stderr, "Mark6 file %s cannot be opened.  Error code = %d\n", fileList[i], v);
			++nBad;
		}
		else
		{
			int s;
			for(s = 0; s < MARK6_BUFFER_SLOTS; ++s)
			{
				Mark6FileReadBlock(m6g->mk6Files + i, s);
			}

			// get VSN from metadata
			// TO DO: This will get the VSN for the first module in a group.
			// We may want to support messaging for all modules in a group.
			if(gotVSN == 0 && !strncmp(fileList[i], "/mnt/disks", 10))
			{
				char *rv;

				slot = fileList[i][11];
				disk = fileList[i][13];
				sprintf(msnname, "%s/%c/%c/eMSN", getMark6MetaRoot(), slot, disk);
				fmsn = fopen(msnname, "r");
				if(!fmsn)
				{
					continue;
				}
				rv = fgets(m6g->activeVSN, 9, fmsn);
				fclose(fmsn);
				if(rv == 0)
				{
					continue;
				}
				gotVSN = 1;

				// FIXME: do we want to report errors in getting labels somehow?
			}
		}
	}
	m6g->packetSize = m6g->mk6Files[0].packetSize;

	return nBad;
}

/* return 1 if all files are present (with no extras) and 0 otherwise */
int isMark6GatherComplete(const Mark6Gatherer *m6g)
{
	int f;
	int a = 0, b = 1000000;

	if(m6g->nFile == 0)
	{
		return 0;
	}

	for(f = 0; f < m6g->nFile; ++f)
	{
		if(m6g->mk6Files[f].block1 > a)
		{
			a = m6g->mk6Files[f].block1;
		}
		if(m6g->mk6Files[f].block2 < b)
		{
			b = m6g->mk6Files[f].block2;
		}
	}

	if(b == a+1 && b == m6g->nFile)
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

int closeMark6Gatherer(Mark6Gatherer *m6g)
{
	int i;

	if(!m6g)
	{
		fprintf(stderr, "Error: closeMark6Gatherer called with null pointer\n");

		return -1;
	}
	for(i = 0; i < m6g->nFile; ++i)
	{
		closeMark6File(m6g->mk6Files + i);
	}

	free(m6g->mk6Files);
	m6g->mk6Files = 0;

	m6g->nFile = 0;

	free(m6g->activeVSN);

	free(m6g);

	return 0;
}

void printMark6Gatherer(const Mark6Gatherer *m6g)
{
	int i;

	printf("Mark6Gatherer:\n");
	if(m6g == 0)
	{
		printf("  Null\n");
	}
	else
	{
		printf("  packetSize = %d\n", m6g->packetSize);
		printf("  nFile = %d\n", m6g->nFile);
		for(i = 0; i < m6g->nFile; ++i)
		{
			printMark6File(m6g->mk6Files + i);
		}
	}
}

/* position is the position of the reconstructed stream to seek to */
int seekMark6Gather(Mark6Gatherer *m6g, off_t position)
{
	pthread_attr_t attr;
	pthread_t *seekThread;
	struct seekArgs *S;
	int t;

	if(!m6g)
	{
		return -1;
	}
	if(position >= getMark6GathererFileSize(m6g))
	{
		return -2;
	}

	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
	seekThread = (pthread_t *)malloc(m6g->nFile*sizeof(pthread_t));
	S = (struct seekArgs *)malloc(m6g->nFile*sizeof(struct seekArgs));

	for(t = 0; t < m6g->nFile; ++t)
	{
		S[t].m6f = &m6g->mk6Files[t];
		S[t].nFile = m6g->nFile;
		S[t].fileIndex = t;
		S[t].position = position;
		pthread_create(seekThread + t, &attr, mark6Seeker, S + t);
	}

	pthread_attr_destroy(&attr);

	for(t = 0; t < m6g->nFile; ++t)
	{
		pthread_join(seekThread[t], 0);
	}

	free(seekThread);
	free(S);

	return 0;
}

int mark6Gather(Mark6Gatherer *m6g, void *buf, size_t count)
{
	size_t n = 0;

	count -= (count % m6g->packetSize);

	while(n < count)
	{
		int v;
		int f, s, fileIndex, slotIndex;
		uint64_t lowestFrame;
		Mark6File *F;
		Mark6BufferSlot *slot;

		lowestFrame = 0;
		fileIndex = -1;
		slotIndex = -1;

		for(f = 0; f < m6g->nFile; ++f)
		{
			for(s = 0; s < MARK6_BUFFER_SLOTS; ++s)
			{
				if(m6g->mk6Files[f].slot[s].payloadBytes > 0 && (fileIndex < 0 || m6g->mk6Files[f].slot[s].frame < lowestFrame))
				{
					lowestFrame = m6g->mk6Files[f].slot[s].frame;
					fileIndex = f;
					slotIndex = s;
				}
			}
		}

		if(fileIndex < 0)
		{
			return n;
		}

		F = &m6g->mk6Files[fileIndex];
		slot = F->slot + slotIndex;
		// don't gather fill data
		v = checkFillData(slot->data + slot->index);
		if(v == 0)
		{
			memcpy(buf, slot->data + slot->index, m6g->packetSize);
			buf += m6g->packetSize;
			n += m6g->packetSize;
		}
		else
		{
			static int count = 0;

			if(count < 10)
			{
				printf("Fill data encountered: %d bytes\n", v);
				if(count == 9)
				{
					printf("Further fill data notices will not be printed.\n");
				}
			}
			++count;
		}
		slot->index += m6g->packetSize;
		if(slot->index >= slot->payloadBytes)
		{
			Mark6FileReadBlock(F, slotIndex);
		}
		else
		{
			if(F->packetFormat == M6SG_PACKET_FORMAT_VDIF)
			{
				vdif_header *vh = (vdif_header *)(slot->data + slot->index);
				slot->frame = vdifFrame(vh);
			}
			else
			{
				slot->frame = mark5bFrame(slot->data + slot->index);
			}
		}
	}

	return n;
}


/* qsort C-string comparison function */ 
int cstring_cmp(const void *a, const void *b) 
{ 
	const char **ia = (const char **)a;
	const char **ib = (const char **)b;
	
	return strcmp(*ia, *ib);
} 

const char *getMark6Root()
{
	static const char *root = 0;
	
	if(root == 0)
	{
		root = getenv("MARK6_ROOT");
		if(!root)
		{
			root = DefaultMark6Root;
		}
	}

	return root;
}

const char *getMark6MetaRoot()
{
	static const char *root = 0;
	
	if(root == 0)
	{
		root = getenv("MARK6_META_ROOT");
		if(!root)
		{
			root = DefaultMark6Meta;
		}
	}

	return root;
}

int getMark6FileList(char ***fileList)
{
	const int MaxFilenameSize = 256;
	char fileName[MaxFilenameSize];
	glob_t G;
	int i, n, v;
	char **ptrs;	/* points to first character after last / of each found file */
	const char *last;
	int uniq;

	snprintf(fileName, MaxFilenameSize, "%s/*", getMark6Root());

	v = glob(fileName, GLOB_NOSORT, 0, &G);
	if(v != 0)
	{
		return 0;	/* no matching files found */
	}

	n = G.gl_pathc;
	ptrs = (char **)malloc(n*sizeof(char *));
	
	/* store just the portion after last / into new pointer array */
	for(i = 0; i < n; ++i)
	{
		char *p;
                
		p = strrchr(G.gl_pathv[i], '/');
		if(p == 0)
		{
			ptrs[i] = G.gl_pathv[i];
		}
		else
		{
			ptrs[i] = p + 1;
		}
	}

	/* sort */
	qsort(ptrs, n, sizeof(char *), cstring_cmp);

	/* count non-duplicates */
	last = "///";
	uniq = 0;
	for(i = 0; i < n; ++i)
	{
		if(strcmp(ptrs[i], last) != 0)
		{
			last = ptrs[i];

			/* overwrite beginning of ptrs array with non-duplicates */
			ptrs[uniq] = ptrs[i];

			++uniq;
		}
	}

	/* populate fileList */
	*fileList = (char **)malloc(uniq*sizeof(char *));
	for(i = 0; i < uniq; ++i)
	{
		(*fileList)[i] = strdup(ptrs[i]);
	}
	free(ptrs);
	globfree(&G);

	return uniq;
}

typedef struct
{
	char *name;
	long long int size;
} FileAndSize;

static int compareFileAndSize(const void *a, const void *b)
{
	return strcmp(((const FileAndSize *)a)->name, ((const FileAndSize *)b)->name);
}

int getMark6FileListWithSizes(char ***fileList, long long int **sizeList)
{
	const int MaxFilenameSize = 256;
	char fileName[MaxFilenameSize];
	glob_t G;
	int i, n, v;
	FileAndSize *fands;	/* points to first character after last / of each found file */
	const char *last;
	int uniq;

	snprintf(fileName, MaxFilenameSize, "%s/*", getMark6Root());

	v = glob(fileName, GLOB_NOSORT, 0, &G);
	if(v != 0)
	{
		return 0;	/* no matching files found */
	}

	n = G.gl_pathc;
	fands = (FileAndSize *)malloc(n*sizeof(FileAndSize));

	/* store just the portion after last / into new pointer array */
	for(i = 0; i < n; ++i)
	{
		char *p;
		struct stat st;                

		stat(G.gl_pathv[i], &st);
		fands[i].size = st.st_size;

		p = strrchr(G.gl_pathv[i], '/');
		if(p == 0)
		{
			fands[i].name = G.gl_pathv[i];
		}
		else
		{
			fands[i].name = p + 1;
		}
	}

	/* sort */
	qsort(fands, n, sizeof(FileAndSize), compareFileAndSize);

	/* count non-duplicates */
	last = "///";
	uniq = 0;
	for(i = 0; i < n; ++i)
	{
		if(strcmp(fands[i].name, last) != 0)
		{
			last = fands[i].name;

			/* overwrite beginning of ptrs array with non-duplicates */
			fands[uniq].name = fands[i].name;
			fands[uniq].size = fands[i].size;

			++uniq;
		}
		else
		{
			fands[uniq-1].size += fands[i].size;
		}
	}

	/* populate fileList */
	*fileList = (char **)malloc(uniq*sizeof(char *));
	*sizeList = (long long int *)malloc(uniq*sizeof(long long int));
	for(i = 0; i < uniq; ++i)
	{
		(*fileList)[i] = strdup(fands[i].name);
		(*sizeList)[i] = fands[i].size;
	}
	free(fands);
	globfree(&G);

	return uniq;
}

int getMark6SlotFileList(int slot, char ***fileList)
{
	const int MaxFilenameSize = 256;
	char fileName[MaxFilenameSize];
	glob_t G;
	int i, n, v;
	char **ptrs;	/* points to first character after last / of each found file */
	const char *last;
	int uniq;

	snprintf(fileName, MaxFilenameSize, "/mnt/disks/%d/*/data/*", slot);

	v = glob(fileName, GLOB_NOSORT, 0, &G);
	if(v != 0)
	{
		return 0;	/* no matching files found */
	}

	n = G.gl_pathc;
	ptrs = (char **)malloc(n*sizeof(char *));
	
	/* store just the portion after last / into new pointer array */
	for(i = 0; i < n; ++i)
	{
		char *p;
                
		p = strrchr(G.gl_pathv[i], '/');
		if(p == 0)
		{
			ptrs[i] = G.gl_pathv[i];
		}
		else
		{
			ptrs[i] = p + 1;
		}
	}

	/* sort */
	qsort(ptrs, n, sizeof(char *), cstring_cmp);

	/* count non-duplicates */
	last = "///";
	uniq = 0;
	for(i = 0; i < n; ++i)
	{
		if(strcmp(ptrs[i], last) != 0)
		{
			last = ptrs[i];

			/* overwrite beginning of ptrs array with non-duplicates */
			ptrs[uniq] = ptrs[i];

			++uniq;
		}
	}

	/* populate fileList */
	*fileList = (char **)malloc(uniq*sizeof(char *));
	for(i = 0; i < uniq; ++i)
	{
		(*fileList)[i] = strdup(ptrs[i]);
	}
	free(ptrs);
	globfree(&G);

	return uniq;
}
