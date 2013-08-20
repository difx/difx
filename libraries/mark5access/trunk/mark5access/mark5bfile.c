/***************************************************************************
 *   Copyright (C) 2013 Walter Brisken                                     *
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
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/stat.h>
#include <time.h>
#include "mark5bfile.h"
#include "config.h"

#define MARK5B_FRAME_SIZE	10016
#define MARK5B_HEADER_SIZE	16

#ifdef WORDS_BIGENDIAN
#define MARK5_FILL_WORD		0x44332211UL
#define MARK5B_SYNC_WORD	0xEDDEADABUL
#else
#define MARK5_FILL_WORD		0x11223344UL
#define MARK5B_SYNC_WORD	0xABADDEEDUL
#endif

#define MJD_UNIX0		40587.0

void resetmark5bfilesummary(struct mark5b_file_summary *sum)
{
	memset(sum, 0, sizeof(struct mark5b_file_summary));
	sum->nBit = 2;
	sum->framesPerSecond = 25600;	/* 2 Gbps */
}

void printmark5bfilesummary(const struct mark5b_file_summary *sum)
{
	printf("Mark5B file: %s\n", sum->fileName);
	printf("  size = %Ld bytes\n", sum->fileSize);
	if(sum->framesPerSecond > 0)
	{
		printf("  frame rate = %d per second\n", sum->framesPerSecond);
	}
	else
	{
		printf("  frame rate is unknown\n");
	}
	printf("  start MJD %s = %d\n", (sum->startDay < 1000 ? "(mod 1000)" : ""), sum->startDay);
	printf("  start second = %d\n", sum->startSecond);
	printf("  start frame = %d\n", sum->startFrame);
	printf("  end second = %d\n", sum->endSecond);
	printf("  end frame = %d\n", sum->endFrame);
	printf("  first frame offset = %d bytes\n", sum->firstFrameOffset);
}

int determinemark5bframeoffset(const unsigned char *buffer, int bufferSize)
{
	int i, j, N;

	N = bufferSize - MARK5B_FRAME_SIZE - MARK5B_HEADER_SIZE;
	for(j = 0; j < 4; ++j)
	{
		/* note two loops instead of 1 for efficiency sake */
		for(i = 0; i < N; i+=4)
		{
			uint32_t *word = (uint32_t *)(buffer + j + i);
			if(word[0] == MARK5B_SYNC_WORD && word[2504] == MARK5B_SYNC_WORD)
			{
				return i+j;
			}
		}
	}

	return -1;
}

int determinelastmark5bframeoffset(const unsigned char *buffer, int bufferSize)
{
	int i, j, N;

	N = bufferSize - MARK5B_FRAME_SIZE - MARK5B_HEADER_SIZE;
	for(j = 0; j < 4; ++j)
	{
		/* note two loops instead of 1 for efficiency sake */
		for(i = N-4; i >= 0; i-=4)
		{
			uint32_t *word = (uint32_t *)(buffer + j + i);
			if(word[0] == MARK5B_SYNC_WORD && word[2504] == MARK5B_SYNC_WORD)
			{
				return i+j+MARK5B_FRAME_SIZE;
			}
		}
	}

	return -1;
}

int summarizemark5bfile(struct mark5b_file_summary *sum, const char *fileName)
{
	int bufferSize = 200000;	/* 200 kB really should be sufficient */
	unsigned char *buffer, *p;
	struct stat st;
	int rv;
	int lastOffset;
	FILE *in;

	/* Initialize things */

	resetmark5bfilesummary(sum);
	strncpy(sum->fileName, fileName, MARK5B_SUMMARY_FILE_LENGTH-1);
	sum->startSecond = 1<<30;

	rv = stat(fileName, &st);
	if(rv < 0)
	{
		return -1;
	}

	sum->fileSize = st.st_size;

	in = fopen(fileName, "r");
	if(!in)
	{
		return -2;
	}

	if(sum->fileSize < 2*bufferSize)
	{
		bufferSize = sum->fileSize;
	}

	buffer = (unsigned char *)malloc(bufferSize);
	if(!buffer)
	{
		fclose(in);

		return -3;
	}

	
	/* Get initial information */

	rv = fread(buffer, 1, bufferSize, in);
	if(rv < bufferSize)
	{
		fclose(in);
		free(buffer);

		return -4;
	}

	sum->firstFrameOffset = determinemark5bframeoffset(buffer, bufferSize);
	if(sum->firstFrameOffset < 0)
	{
		fclose(in);
		free(buffer);

		return -6;
	}

	p = buffer + sum->firstFrameOffset;

	sum->startDay = (p[11] >> 4)*100 + (p[11] & 0x0F)*10 + (p[10] >> 4);
	sum->startSecond = (p[10] & 0x0F)*10000 + (p[9] >> 4)*1000 + (p[9] & 0x0F)*100 + (p[8] >> 4)*10 +  (p[8] & 0x0F);
	sum->startFrame = p[4] + (p[5] * 256);

	
	/* Work on end of file */

	if(sum->fileSize > bufferSize)
	{
		rv = fseeko(in, sum->fileSize - bufferSize, SEEK_SET);
		if(rv != 0)
		{
			fclose(in);
			free(buffer);

			return -7;
		}

		rv = fread(buffer, 1, bufferSize, in);
		if(rv < bufferSize)
		{
			fclose(in);
			free(buffer);

			return -8;
		}
	}

	lastOffset = determinelastmark5bframeoffset(buffer, bufferSize);
	if(lastOffset < 0)
	{
		fclose(in);
		free(buffer);

		return -9;
	}

	p = buffer + lastOffset;

	sum->endDay = (p[11] >> 4)*100 + (p[11] & 0x0F)*10 + (p[10] >> 4);
	sum->endSecond = (p[10] & 0x0F)*10000 + (p[9] >> 4)*1000 + (p[9] & 0x0F)*100 + (p[8] >> 4)*10 +  (p[8] & 0x0F);
	sum->endFrame = p[4] + (p[5] * 256);


	/* Clean up */

	free(buffer);
	fclose(in);

	return 0;
}

void mark5bfilesummaryfixmjd(struct mark5b_file_summary *sum, int mjd)
{
	int d, kd;

	d = mjd - sum->startDay;
	kd = (d + 500)/1000;
	sum->startDay += 1000*kd;
	sum->endDay += 1000*kd;
}

void mark5bfilesummaryfixmjdtoday(struct mark5b_file_summary *sum)
{
	double mjd;

	mjd = MJD_UNIX0 + time(0)/86400.0;

	mark5bfilesummaryfixmjd(sum, (int)mjd);
}
