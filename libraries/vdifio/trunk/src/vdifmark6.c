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

	v = fread(&(m6f->block1), sizeof(m6f->block1), 1, m6f->in);
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
	v = fread(&(m6f->block2), sizeof(m6f->block1), 1, m6f->in);
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

/* this assumes *m6f is already allocated but that the structures within are not. */
/* no attempt is made here to free existing data */

/* returns 0 on success, or error code otherwise */
int openMark6File(Mark6File *m6f, const char *filename)
{
	Mark6Header header;

	stat(filename, &m6f->stat);
	m6f->in = fopen(filename, "r");
	if(!m6f->in)
	{
		m6f->fileName = 0;

		return -1;
	}
	m6f->fileName = strdup(filename);
	fread(&header, sizeof(header), 1, m6f->in);
	m6f->version = header.version;
	m6f->blockHeaderSize = mark6BlockHeaderSize(header.version);
	if(m6f->blockHeaderSize <= 0)
	{
		fclose(m6f->in);
		free(m6f->fileName);
		m6f->in = 0;
		m6f->fileName = 0;

		return -2;
	}
	m6f->maxBlockSize = header.block_size;
	m6f->packetSize = header.packet_size;
	m6f->data = (char *)malloc(m6f->maxBlockSize-m6f->blockHeaderSize);
	if(!m6f->data)
	{
		fclose(m6f->in);
		free(m6f->fileName);
		m6f->in = 0;
		m6f->fileName = 0;

		return -3;
	}
	m6f->blockHeader.blocknum = -1;
	m6f->blockHeader.wb_size = m6f->maxBlockSize;

	m6f->payloadBytes = 0;	/* nothing read yet */

	if(getFirstBlocks(m6f) < 0)
	{
		fclose(m6f->in);
		free(m6f->fileName);
		m6f->in = 0;
		m6f->fileName = 0;

		return -4;
	}

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
	if(m6f->data)
	{
		free(m6f->data);
		m6f->data = 0;
	}
	m6f->version = -1;

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
		printf("  Filename = %s\n", m6f->fileName);
		printf("  Mark6 version = %d\n", m6f->version);
		printf("  Max block size = %d\n", m6f->maxBlockSize);
		printf("  Block header size = %d\n", m6f->blockHeaderSize);
		printf("  First two block numbers = %d, %d\n", m6f->block1, m6f->block2);
		printf("  Packet size = %d\n", m6f->packetSize);
		printf("  Payload bytes = %d (bytes currently residing in core)\n", m6f->payloadBytes);
		printf("  Current block number = %d\n", m6f->blockHeader.blocknum);
		printf("  Current index within block = %d\n", m6f->index);
		printf("  Second = %d\n", (int)(m6f->frame >> 24));
		printf("  Frame in second = %d\n", (int)(m6f->frame & 0xFFFFFF));
		printf("  File size = %lld\n", (long long)(m6f->stat.st_size));
	}
}

/* Returns 0 on EOF */
ssize_t Mark6FileReadBlock(Mark6File *m6f)
{
	ssize_t v;

	/* Note: the following will only update the value of "blocknum" for Mark6 version 1 (which is hopefully out of circulation). */
	/* For Mark6 ver. 2, both "blocknum" and "wb_size" are updated. */
	v = fread(&m6f->blockHeader, m6f->blockHeaderSize, 1, m6f->in);
	if(v != 1)
	{
		m6f->payloadBytes = 0;
		m6f->index = 0;
		m6f->frame = 0;
	}
	else
	{
		vdif_header *vh = (vdif_header *)(m6f->data);
		
		m6f->payloadBytes = fread(m6f->data, 1, m6f->blockHeader.wb_size - m6f->blockHeaderSize, m6f->in);
		m6f->payloadBytes -= (m6f->payloadBytes % m6f->packetSize);
		m6f->index = 0;
		m6f->frame = ((uint64_t)(getVDIFFrameEpochSecOffset(vh)) << 24LL) | getVDIFFrameNumber(vh);
	}

	return m6f->payloadBytes;
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
Mark6Gatherer *openMark6GathererFromTemplate(const char *template)
{
	const int MaxFilenameSize = 256;
	char fileName[MaxFilenameSize];
	glob_t G;
	int v;
	Mark6Gatherer *m6g;

	if(template[0] == '/')
	{
		snprintf(fileName, MaxFilenameSize, "%s", template);
	}
	else
	{
		snprintf(fileName, MaxFilenameSize, "%s/%s", getMark6Root(), template);
	}

	v = glob(fileName, GLOB_NOSORT, 0, &G);
	if(v == 0)
	{
		m6g = openMark6Gatherer(G.gl_pathc, G.gl_pathv);

		globfree(&G);
	}
	else
	{
		fprintf(stderr, "Cannot create Mark6Gatherer because %s matched no files\n", template);

		return 0;
	}
	m6g->packetSize = m6g->mk6Files[0].packetSize;

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
	if(!m6g)
	{
		fprintf(stderr, "Error: cannot (re)allocate %d * %d bytes for Mark6Files\n", nFile, (int)sizeof(Mark6File));
	
		free(m6g);

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
			Mark6FileReadBlock(m6g->mk6Files + i);
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
	/* for now, declare success if the lowest block2 value is 1 more than the highest block1 value */

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

	if(b == a+1)
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

int mark6Gather(Mark6Gatherer *m6g, void *buf, size_t count)
{
	size_t n = 0;

	count -= (count % m6g->packetSize);

	while(n < count)
	{
		int i;
		uint64_t lowestFrame;
		int f;
		Mark6File *F;

		lowestFrame = m6g->mk6Files[0].frame+1;
		f = -1;

		for(i = 0; i < m6g->nFile; ++i)
		{
			if(m6g->mk6Files[i].payloadBytes > 0 && m6g->mk6Files[i].frame < lowestFrame)
			{
				lowestFrame = m6g->mk6Files[i].frame;
				f = i;
			}
		}
		if(f < 0)
		{
			return n;
		}

		F = &(m6g->mk6Files[f]);
		memcpy(buf, F->data + F->index, m6g->packetSize);
		buf += m6g->packetSize;
		n += m6g->packetSize;
		F->index += m6g->packetSize;
		if(F->index >= F->payloadBytes)
		{
			Mark6FileReadBlock(F);
		}
		else
		{
			vdif_header *vh = (vdif_header *)(F->data + F->index);
			F->frame = ((uint64_t)getVDIFFrameEpochSecOffset(vh) << 24LL) | getVDIFFrameNumber(vh);
		}
	}

	return n;
}

/* scan name should be the template file to match */
int summarizevdifmark6(struct vdif_file_summary *sum, const char *scanName, int frameSize)
{
	int bufferSize = 2000000;	/* 2 MB should encounter all threads of a usual VDIF file */
	unsigned char *buffer;
	int rv, i, N, f;
	int readBytes;
	char hasThread[VDIF_MAX_THREAD_ID + 1];
	struct vdif_header *vh0;	/* pointer to the prototype header */
	Mark6Gatherer *G;

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

	/* Work on end of file, if file is long enough */

	/* Here read bits of the ends of each file of the set to make sure the info we get is complete */
	for(f = 0; f < G->nFile; ++f)
	{
		int offset;
		Mark6File *F;

		F = &(G->mk6Files[f]);

		if(F->stat.st_size < bufferSize)
		{
			continue;
		}
			
		rv = fseeko(F->in, F->stat.st_size - bufferSize, SEEK_SET);
		if(rv != 0)
		{
			closeMark6Gatherer(G);
			free(buffer);

			return -7;
		}

		rv = fread(buffer, 1, bufferSize, F->in);
		if(rv < bufferSize)
		{
			closeMark6Gatherer(G);
			free(buffer);

			return -8;
		}

		offset = determinevdifframeoffset(buffer, bufferSize, frameSize);
		if(offset < 0)
		{
			closeMark6Gatherer(G);
			free(buffer);

			return -9;
		}
		vh0 = (struct vdif_header *)(buffer + offset);

		for(i = 0; i < N; )
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
	}


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

	free(buffer);
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
