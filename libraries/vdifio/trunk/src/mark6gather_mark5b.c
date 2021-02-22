/***************************************************************************
 *  Copyright (C) 2018 by Mark Wainrigh                                    *
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

#include <stdio.h>
#define _GNU_SOURCE
#include <string.h>
#include <stdlib.h>
#include <glob.h>
#include <mark6sg/mark6gather.h>
#include "mark6gather_mark5b.h"
#include "vdifio.h"

/* Macro to turn an expanded macro into a string */
#define str(s) #s


struct sumArgs
{
	Mark6File *m6f;
	int endDay;
	int endSecond;
	int endFrame;
	int framesPerSecond;
	int bufferSize;
};

static void *fileEndSummarizer(void *arg)
{
	struct sumArgs *S = (struct sumArgs *)arg;
	int offset;
	Mark6File *F;
	FILE *in;
	unsigned char *buffer, *p;
	int rv;

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

	offset = determinelastmark5bframeoffset(buffer, S->bufferSize);
	if(offset < 0)
	{
		fclose(in);
		free(buffer);

		return (void *)(-9);
	}

	p = buffer + offset;

	S->endDay = (p[11] >> 4)*100 + (p[11] & 0x0F)*10 + (p[10] >> 4);
	S->endSecond = (p[10] & 0x0F)*10000 + (p[9] >> 4)*1000 + (p[9] & 0x0F)*100 + (p[8] >> 4)*10 +  (p[8] & 0x0F);
	S->endFrame = p[4] + (p[5] * 256);

	fclose(in);
	free(buffer);

	return 0;
}

/* scan name should be the template file to match */
int summarizemark5bmark6(struct mark5b_file_summary *sum, const char *scanName)
{
	int bufferSize = 200000;	/* 200 kB should be sufficient */
	unsigned char *buffer, *p;
	int f, seconds0, seconds1;
	int readBytes;
	Mark6Gatherer *G;
	struct sumArgs *S;
	pthread_t *sumThread;
	pthread_attr_t attr;

	/* Initialize things */

	resetmark5bfilesummary(sum);
	strncpy(sum->fileName, scanName, MARK5B_SUMMARY_FILE_LENGTH-1);
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

	sum->firstFrameOffset = determinemark5bframeoffset(buffer, bufferSize);
	if(sum->firstFrameOffset < 0)
	{
		closeMark6Gatherer(G);
		free(buffer);

		return -5;
	}

	p = buffer + sum->firstFrameOffset;

	sum->startDay = (p[11] >> 4)*100 + (p[11] & 0x0F)*10 + (p[10] >> 4);
	sum->startSecond = (p[10] & 0x0F)*10000 + (p[9] >> 4)*1000 + (p[9] & 0x0F)*100 + (p[8] >> 4)*10 +  (p[8] & 0x0F);
	sum->startFrame = p[4] + (p[5] * 256);

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

		pthread_create(&sumThread[f], &attr, fileEndSummarizer, S + f);
	}

	pthread_attr_destroy(&attr);

	for(f = 0; f < G->nFile; ++f)
	{
		pthread_join(sumThread[f], 0);

		if(S[f].endSecond > sum->endSecond)
		{
			sum->endDay = S[f].endDay;
			sum->endSecond = S[f].endSecond;
			sum->endFrame = S[f].endFrame;
		}
		else if(S[f].endSecond == sum->endSecond && S[f].endFrame > sum->endFrame)
		{
			sum->endFrame = S[f].endFrame;
		}
	}

	seconds0 = sum->startDay*86400 + sum->startSecond;
	seconds1 = sum->endDay*86400 + sum->endSecond;

	if(seconds1 > seconds0)
	{
		sum->framesPerSecond = (sum->fileSize/10016 - sum->endFrame + sum->startFrame)/(seconds1 - seconds0);

		sum->framesPerSecond = ((sum->framesPerSecond + 50)/100)*100;
	}

	free(sumThread);
	free(S);

	/* Clean up */

	closeMark6Gatherer(G);

	return 0;
}

