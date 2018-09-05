/***************************************************************************
 *  Copyright (C) 2015-2018 by Walter Brisken                              *
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
#include <glob.h>
#include <mark6sg/mark6gather.h>
#include "mark6gather_vdif.h"

/* Macro to turn an expanded macro into a string */
#define str(s) #s


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

