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

#define MARK6_SYNC	0xfeed6666

typedef struct
{
	uint32_t sync_word;	/* nominally the number above */
	int32_t version;
	int block_size;
	int packet_format;
	int packet_size;
} Mark6Header;

typedef struct
{
	int32_t blocknum;
	int32_t wb_size;
} Mark6BlockHeader_ver2;

typedef struct
{
	int32_t blocknum;
} Mark6BlockHeader_ver1;

typedef struct
{
	FILE *in;			/* actual file descriptor */
	char *fileName;
	int version;			/* from Mark6Header */
	int blockSize;			/* from Mark6Header */
	int blockHeaderSize;		/* from mark6BlockHeaderSize() */
	int packetSize;			/* from Mark6Header */
	int payloadSize;		/* block_size - blockHdeader_size */
	int payloadBytes;		/* actual number of payload bytes (usually == payload_size) */
	char *buffer;			/* to store a full block, including header.  Allocated upon fopen() */
	const char *data;		/* points to payload within buffer */
	const int32_t *blockNumPtr;	/* points to block number within buffer */
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


#ifdef __cplusplus
}
#endif

#endif
