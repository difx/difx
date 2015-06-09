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
#include <string.h>
#include <stdlib.h>

#include "vdifmark6.h"

#define str(s) #s

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


/* this assumes *m6f is already allocated but that the structures within are not. */
/* no attempt is made here to free existing data */

/* returns 0 on success, or error code otherwise */
int openMark6File(Mark6File *m6f, const char *filename)
{
	Mark6Header header;

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
		printf("  Packet size = %d\n", m6f->packetSize);
		printf("  Payload bytes = %d (bytes currently residing in core)\n", m6f->payloadBytes);
		printf("  Current block number = %d\n", m6f->blockHeader.blocknum);
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
	}
	else
	{
		m6f->payloadBytes = fread(m6f->data, 1, m6f->blockHeader.wb_size - m6f->blockHeaderSize, m6f->in);
	}

	return m6f->payloadBytes;
}

/* returns file index of next block (defined as smallest block greater than current block) */
/* returns -1 if no more data found */
static int nextMark6File(const Mark6Descriptor *m6d)
{
	int i;
	int nextFileNum = -1;
	int nextBlockNum = -1;

	for(i = 0; i < m6d->nFile; ++i)
	{
		int blockNum;

		if(m6d->mk6Files[i].payloadBytes <= 0)
		{
			continue;
		}
		blockNum = m6d->mk6Files[i].blockHeader.blocknum;
		if(blockNum > m6d->currentBlockNum && (blockNum < nextBlockNum || nextBlockNum == -1))
		{
			nextFileNum = i;
			nextBlockNum = blockNum;
		}
	}

//printf("Advancing from file:block %d:%d to %d:%d\n", m6d->currentFileNum, m6d->currentBlockNum, nextFileNum, nextBlockNum);

	return nextFileNum;
}

/* This call _does_ create the new Mark6Descriptor structure and fills it in. */
Mark6Descriptor *openMark6(int nFile, char **fileList)
{
	int i;
	int nBad = 0;
	Mark6Descriptor *m6d;

	m6d = (Mark6Descriptor *)calloc(1, sizeof(Mark6Descriptor));
	if(!m6d)
	{
		fprintf(stderr, "Error: cannot allocate %d bytes for Mark6Descriptor\n", (int)(sizeof(Mark6Descriptor)));

		return 0;
	}

	m6d->nFile = nFile;
	m6d->currentFileNum = -1;
	m6d->currentBlockNum = -1;
	m6d->index = 0;

	m6d->mk6Files = (Mark6File *)calloc(nFile, sizeof(Mark6File));
	if(!m6d)
	{
		fprintf(stderr, "Error: cannot allocate %d * %d bytes for Mark6Files\n", nFile, (int)sizeof(Mark6File));
	
		free(m6d);

		return 0;
	}

	for(i = 0; i < nFile; ++i)
	{
		int v;

		v = openMark6File(m6d->mk6Files + i, fileList[i]);

		if(v < 0)
		{
			fprintf(stderr, "Mark6 file %s cannot be opened.  Error code = %d\n", fileList[i], v);
			++nBad;
		}
	}

	if(nBad > 0)
	{
		fprintf(stderr, "Cannot create Mark6Descriptor because %d/%d files could not be opened\n", nBad, nFile);

		closeMark6(m6d);

		return 0;
	}

	/* load first block from each file */
	for(i = 0; i < nFile; ++i)
	{
		Mark6FileReadBlock(m6d->mk6Files + i);
	}

	m6d->currentFileNum = nextMark6File(m6d);
	if(m6d->currentFileNum >= 0)
	{
		m6d->currentBlockNum = m6d->mk6Files[m6d->currentFileNum].blockHeader.blocknum;
	}

	return m6d;
}

int closeMark6(Mark6Descriptor *m6d)
{
	int i;

	if(!m6d)
	{
		fprintf(stderr, "Error: closeMark6 called with null pointer\n");

		return -1;
	}
	for(i = 0; i < m6d->nFile; ++i)
	{
		closeMark6File(m6d->mk6Files + i);
	}

	free(m6d->mk6Files);
	m6d->mk6Files = 0;

	m6d->nFile = 0;

	free(m6d);

	return 0;
}

void printMark6(const Mark6Descriptor *m6d)
{
	int i;

	printf("Mark6Descriptor:\n");
	if(m6d == 0)
	{
		printf("  Null\n");
	}
	else
	{
		printf("  nFile = %d\n", m6d->nFile);
		printf("  currentFileNum = %d\n", m6d->currentFileNum);
		printf("  currentBlockNum = %d\n", m6d->currentBlockNum);
		printf("  index = %d\n", m6d->index);
		for(i = 0; i < m6d->nFile; ++i)
		{
			printMark6File(m6d->mk6Files + i);
		}
	}
}

ssize_t readMark6(Mark6Descriptor *m6d, void *buf, size_t count)
{
	ssize_t bytesRead = 0;
	char *buffer = (char *)buf;

	if(m6d->currentBlockNum < 0 || m6d->currentFileNum < 0)
	{
		return -1;
	}

	while(count > 0)
	{
		Mark6File *f;
		int toRead;

		f = m6d->mk6Files + m6d->currentFileNum;

		if(count <= f->payloadBytes - m6d->index)
		{
			toRead = count;
		}
		else
		{
			toRead = f->payloadBytes - m6d->index;
		}

		memcpy(buffer, f->data+m6d->index, toRead);
		
		m6d->index += toRead;
		count -= toRead;
		buffer += toRead;
		bytesRead += toRead;
		
		if(m6d->index == f->payloadBytes)
		{
			/* we read to end of this block; replenish it and find the next */
			Mark6FileReadBlock(f);
			m6d->index = 0;
			m6d->currentFileNum = nextMark6File(m6d);
			if(m6d->currentFileNum >= 0)
			{
				m6d->currentBlockNum = m6d->mk6Files[m6d->currentFileNum].blockHeader.blocknum;
			}
			else
			{
				/* Returning here means eof() for Mark6 fileset. */

				m6d->currentBlockNum = -1;

				return bytesRead;
			}
		}
	}

	return bytesRead;
}


