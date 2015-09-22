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

#include <stdio.h>
#define _GNU_SOURCE
#include <string.h>
#include <stdlib.h>
#include <glob.h>
#include "dateutils.h"
#include "vdifmark6.h"

/* Macro to turn an expanded macro into a string */
#define str(s) #s

const char DefaultMark6Root[] = "/mnt/disks/*/*/data";

static inline uint64_t vdifFrame(vdif_header *vh)
{
	return ((uint64_t)(getVDIFFrameEpochSecOffset(vh)) << 24LL) | getVDIFFrameNumber(vh);
}

const char *mark6PacketFormat(int formatId)
{
	if(formatId == 0)
	{
		return "VDIF";
	}
	else if(formatId == 1)
	{
		return "Mark5b";
	}
	else if(formatId == 2)
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

	v = fseeko(m6f->in, offset + size , SEEK_SET);
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

	do
	{
		int v;

		pthread_barrier_wait(&m6f->readBarrier);

		v = fread(&m6f->readHeader, m6f->blockHeaderSize, 1, m6f->in);
		if(v == 1)
		{
			m6f->readBytes = fread(m6f->readBuffer, 1, m6f->readHeader.wb_size - m6f->blockHeaderSize, m6f->in);
		}
		else
		{
			m6f->readBytes = 0;
		}
		
		pthread_barrier_wait(&m6f->readBarrier);
	} while(!m6f->stopReading);

	return 0;
}

struct seekArgs
{
	Mark6File *m6f;
	off_t position;		/* SEEK_SET argument */
	int nFile;		/* number of files in the fileset */
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

		vh = (vdif_header *)(slot->data);
		slot->frame = vdifFrame(vh);
	}

	pthread_barrier_wait(&m6f->readBarrier);

	return slot->payloadBytes;
}

static void *mark6Seeker(void *arg)
{
	struct seekArgs *S = (struct seekArgs *)arg;
	off_t pos;
	int blockSize;
	int targetBlock;
	int slotIndex;
	int i;

	/*   1. wait for any ongoing reads to complete */
	pthread_barrier_wait(&S->m6f->readBarrier);

	/*   2. figure out where we need to be */ 
	if(S->position == 0)
	{
		pos = sizeof(Mark6Header);
	}
	else
	{
		blockSize = S->m6f->maxBlockSize - ((S->m6f->maxBlockSize-S->m6f->blockHeaderSize) % S->m6f->packetSize);
		targetBlock = S->position/(blockSize - S->m6f->blockHeaderSize);
		pos = sizeof(Mark6Header) + blockSize*targetBlock/S->nFile;

		if(pos >= S->m6f->stat.st_size)
		{
			targetBlock = S->m6f->stat.st_size/(blockSize - S->m6f->blockHeaderSize)/S->nFile;
			pos = sizeof(Mark6Header) + blockSize*targetBlock;
		}

		/* Iterate up to 5 times to get a better position */
		for(i = 0; i < 5; ++i)
		{
			int32_t block;
			int deltaBlock;

			fseeko(S->m6f->in, pos, SEEK_SET);
			fread(&block, sizeof(int32_t), 1, S->m6f->in);

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
		}
	}

	/*   3. reposition the file at the correct location */
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

	stat(filename, &m6f->stat);
	m6f->in = fopen(filename, "r");
	if(!m6f->in)
	{
		deallocateMark6File(m6f);

		return -1;
	}
	m6f->fileName = strdup(filename);
	fread(&header, sizeof(header), 1, m6f->in);
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
	m6f->readBuffer = (char *)malloc(m6f->maxBlockSize-m6f->blockHeaderSize);
	if(!m6f->readBuffer)
	{
		deallocateMark6File(m6f);

		return -5;
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
			printf("    Second = %d\n", (int)(m6f->slot[s].frame >> 24));
			printf("    Frame in second = %d\n", (int)(m6f->slot[s].frame & 0xFFFFFF));
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

	return m6g;
}

off_t getMark6GathererFileSize(const Mark6Gatherer *m6g)
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
		fprintf(stderr, "Cannot create Mark6Gatherer because %s matched no files\n", fileTemplate);

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
		int f, s, fileIndex, slotIndex;
		uint64_t lowestFrame;
		Mark6File *F;
		Mark6BufferSlot *slot;

		lowestFrame = m6g->mk6Files[0].slot[0].frame+1;
		fileIndex = -1;
		slotIndex = -1;

		for(f = 0; f < m6g->nFile; ++f)
		{
			for(s = 0; s < MARK6_BUFFER_SLOTS; ++s)
			{
				if(m6g->mk6Files[f].slot[s].payloadBytes > 0 && m6g->mk6Files[f].slot[s].frame < lowestFrame)
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
		memcpy(buf, slot->data + slot->index, m6g->packetSize);
		buf += m6g->packetSize;
		n += m6g->packetSize;
		slot->index += m6g->packetSize;
		if(slot->index >= slot->payloadBytes)
		{
			Mark6FileReadBlock(F, slotIndex);
		}
		else
		{
			vdif_header *vh = (vdif_header *)(slot->data + slot->index);
			slot->frame = vdifFrame(vh);
		}
	}

	return n;
}

struct sumArgs
{
	Mark6File *m6f;
	char hasThread[VDIF_MAX_THREAD_ID + 1];
	int endSecond;
	int endFrame;
	int bufferSize;
	int frameSize;
	int epoch;
	int nBit;
};

static void *fileEndSummarizer(void *arg)
{
	struct sumArgs *S = (struct sumArgs *)arg;
	int offset;
	Mark6File *F;
	FILE *in;
	unsigned char *buffer;
	struct vdif_header *vh0;
	int i, N, rv;

	F = S->m6f;

	if(F->stat.st_size < S->bufferSize)
	{
		return 0;
	}

	in = fopen(F->fileName, "r");
		
	rv = fseeko(in, F->stat.st_size - S->bufferSize, SEEK_SET);
	if(rv != 0)
	{
		fclose(in);

		return (void *)(-7);
	}

	buffer = (unsigned char *)malloc(S->bufferSize);

	rv = fread(buffer, 1, S->bufferSize, in);
	if(rv < S->bufferSize)
	{
		fclose(in);
		free(buffer);

		return (void *)(-8);
	}

	offset = determinevdifframeoffset(buffer, S->bufferSize, S->frameSize);
	if(offset < 0)
	{
		fclose(in);
		free(buffer);

		return (void *)(-9);
	}
	vh0 = (struct vdif_header *)(buffer + offset);
	N = S->bufferSize - S->frameSize - VDIF_HEADER_BYTES;

	for(i = 0; i < N; )
	{
		struct vdif_header *vh;
		int f, s;

		vh = (struct vdif_header *)(buffer + i);
		s = getVDIFFrameEpochSecOffset(vh);
		
		if(getVDIFFrameBytes(vh) == S->frameSize &&
		   getVDIFEpoch(vh) == S->epoch &&
		   getVDIFBitsPerSample(vh) == S->nBit &&
		   abs(s - getVDIFFrameEpochSecOffset(vh0)) < 2)
		{
			S->hasThread[getVDIFThreadID(vh)] = 1;
			f = getVDIFFrameNumber(vh);

			if(s > S->endSecond)
			{
				S->endSecond = s;
				S->endFrame = f;
			}
			else if(s == S->endSecond && f > S->endFrame)
			{
				S->endFrame = f;
			}

			i += S->frameSize;
		}
		else
		{
			/* Not a good frame. */
			++i;
		}
	}

	fclose(in);
	free(buffer);

	return 0;
}


/* scan name should be the template file to match */
int summarizevdifmark6(struct vdif_file_summary *sum, const char *scanName, int frameSize)
{
	int bufferSize = 2000000;	/* 2 MB should encounter all threads of a usual VDIF file */
	unsigned char *buffer;
	int i, N, f;
	int readBytes;
	char hasThread[VDIF_MAX_THREAD_ID + 1];
	struct vdif_header *vh0;	/* pointer to the prototype header */
	Mark6Gatherer *G;
	struct sumArgs *S;
	pthread_t *sumThread;
	pthread_attr_t attr;

	/* Initialize things */

	resetvdiffilesummary(sum);
	strncpy(sum->fileName, scanName, VDIF_SUMMARY_FILE_LENGTH-1);
	memset(hasThread, 0, sizeof(hasThread));
	sum->startSecond = 1<<30;

	G = openMark6GathererFromTemplate(scanName);
	if(!G)
	{
		return -2;
	}

	sum->fileSize = getMark6GathererFileSize(G);

	if(sum->fileSize < 2*bufferSize)
	{
		bufferSize = sum->fileSize;
	}

	buffer = (unsigned char *)malloc(bufferSize);
	if(!buffer)
	{
		closeMark6Gatherer(G);

		return -3;
	}


	/* Get initial information */

	readBytes = mark6Gather(G, buffer, bufferSize);
	if(readBytes < bufferSize/2)
	{
		closeMark6Gatherer(G);
		free(buffer);

		return -4;
	}

	if(frameSize == 0)
	{
		/* we weren't given the hint of frame size, so try to figure it out... */

		frameSize = determinevdifframesize(buffer, readBytes);
		if(frameSize <= 0)
		{
			closeMark6Gatherer(G);
			free(buffer);

			return -5;
		}
	}

	sum->frameSize = frameSize;
	N = readBytes - frameSize - VDIF_HEADER_BYTES;

	/* Work on beginning of file */

	sum->firstFrameOffset = determinevdifframeoffset(buffer, readBytes, frameSize);
	if(sum->firstFrameOffset < 0)
	{
		closeMark6Gatherer(G);
		free(buffer);

		return -6;
	}
	vh0 = (struct vdif_header *)(buffer + sum->firstFrameOffset);
	sum->epoch = getVDIFEpoch(vh0);
	sum->nBit = getVDIFBitsPerSample(vh0);

	for(i = sum->firstFrameOffset; i < N; )
	{
		struct vdif_header *vh;
		int f, s;

		vh = (struct vdif_header *)(buffer + i);
		s = getVDIFFrameEpochSecOffset(vh);
		
		if(getVDIFFrameBytes(vh) == frameSize &&
		   getVDIFEpoch(vh) == sum->epoch &&
		   getVDIFBitsPerSample(vh) == sum->nBit &&
		   abs(s - getVDIFFrameEpochSecOffset(vh0)) < 2)
		{
			hasThread[getVDIFThreadID(vh)] = 1;
			f = getVDIFFrameNumber(vh);

			if(s < sum->startSecond)
			{
				sum->startSecond = s;
				sum->startFrame = f;
			}
			else if(s == sum->startSecond && f < sum->startFrame)
			{
				sum->startFrame = f;
			}

			if(s > sum->endSecond)
			{
				sum->endSecond = s;
				sum->endFrame = f;
			}
			else if(s == sum->endSecond && f > sum->endFrame)
			{
				sum->endFrame = f;
			}

			i += frameSize;
		}
		else
		{
			/* Not a good frame. */
			++i;
		}
	}

	free(buffer);

	/* Work on end of file, if file is long enough */

	S = (struct sumArgs *)calloc(G->nFile, sizeof(struct sumArgs));
	sumThread = (pthread_t *)malloc(G->nFile*sizeof(pthread_t));

	/* Here read bits of the ends of each file of the set to make sure the info we get is complete */
	/* Do this in parallel for each file */
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);

	for(f = 0; f < G->nFile; ++f)
	{
		S[f].m6f = G->mk6Files + f;
		S[f].bufferSize = bufferSize;
		S[f].frameSize = frameSize;
		S[f].epoch = sum->epoch;
		S[f].nBit = sum->nBit;

		pthread_create(&sumThread[f], &attr, fileEndSummarizer, S + f);
	}

	pthread_attr_destroy(&attr);

	for(f = 0; f < G->nFile; ++f)
	{
		pthread_join(sumThread[f], 0);

		for(i = 0; i <= VDIF_MAX_THREAD_ID; ++i)
		{
			hasThread[i] += S[f].hasThread[i];
		}
		if(S[f].endSecond > sum->endSecond)
		{
			sum->endSecond = S[f].endSecond;
			sum->endFrame = S[f].endFrame;
		}
		else if(S[f].endSecond == sum->endSecond && S[f].endFrame > sum->endFrame)
		{
			sum->endFrame = S[f].endFrame;
		}
	}

	free(sumThread);
	free(S);


	/* Finalize summary */

	for(i = 0; i <= VDIF_MAX_THREAD_ID; ++i)
	{
		if(hasThread[i])
		{
			if(sum->nThread < VDIF_SUMMARY_MAX_THREADS)
			{
				sum->threadIds[sum->nThread] = i;
			}
			++sum->nThread;
		}
	}


	/* Clean up */

	closeMark6Gatherer(G);

	return 0;
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

	return uniq;
}
