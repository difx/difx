/***************************************************************************
 *   Copyright (C) 2013-2021 Walter Brisken                                *
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
#include <vdifio.h>
#include <sys/stat.h>
#include "dateutils.h"
#include "config.h"


void resetvdiffilesummary(struct vdif_file_summary *sum)
{
	memset(sum, 0, sizeof(struct vdif_file_summary));
}

void printvdiffilesummary(const struct vdif_file_summary *sum)
{
	int i;

	printf("VDIF file: %s\n", sum->fileName);
	printf("  size = %lld bytes\n", sum->fileSize);
	printf("  nThread = %d\n", sum->nThread);
	printf("  Thread Ids =");
	for(i = 0; i < sum->nThread; ++i)
	{
		if(i < VDIF_SUMMARY_MAX_THREADS)
		{
			printf(" %d", sum->threadIds[i]);
		}
		else
		{
			printf(" .");
		}
	}
	printf("\n");
	printf("  frame size = %d bytes (includes header)\n", sum->frameSize);
	if(sum->framesPerSecond > 0)
	{
		printf("  frame rate = %d per thread per second\n", sum->framesPerSecond);
	}
	else
	{
		printf("  frame rate is unknown\n");
	}
	printf("  bits per sample = %d\n", sum->nBit);
	printf("  VDIF epoch = %d\n", sum->epoch);
	printf("  start MJD = %d\n", vdiffilesummarygetstartmjd(sum));
	printf("  start second = %d\n", sum->startSecond % 86400);
	printf("  start frame = %d\n", sum->startFrame);
	printf("  end second = %d\n", sum->endSecond % 86400);
	printf("  end frame = %d\n", sum->endFrame);
	printf("  first frame offset = %d bytes\n", sum->firstFrameOffset);
	if(sum->framesPerSecond > 0)
	{
		printf("  data rate = %lld Mbps\n", (long long)(sum->nThread)*(long long)(sum->framesPerSecond)*(long long)(sum->frameSize - 32)/125000LL);
	}
}

void snprintvdiffilesummary(char *str, int maxLength, const struct vdif_file_summary *sum)
{
	int i, v;

	if(sum->nThread < 1)
	{
		v = snprintf(str, maxLength, "VDIF file=%s size=%lld No Threads Found!", sum->fileName, sum->fileSize);
	}
	else
	{
		v = snprintf(str, maxLength, "VDIF file=%s size=%lld frameSize=%d frameRate=%d bits=%d startMJD=%d startSecond=%d startFrame=%d endSecond=%d endFrame=%d, offset=%d threads", sum->fileName, sum->fileSize, sum->frameSize, sum->framesPerSecond, sum->nBit, vdiffilesummarygetstartmjd(sum), sum->startSecond % 86400, sum->startFrame, sum->endSecond % 86400, sum->endFrame, sum->firstFrameOffset);

		for(i = 0; i < sum->nThread; ++i)
		{
			maxLength -= v;

			if(maxLength < 2)
			{
				break;
			}
			str += v;

			v = snprintf(str, maxLength, "%c%d", (i == 0 ? '=' : ','), sum->threadIds[i]);
		}
	}
}

int vdiffilesummarygetstartmjd(const struct vdif_file_summary *sum)
{
	return ymd2mjd(2000 + sum->epoch/2, (sum->epoch%2)*6+1, 1) + sum->startSecond/86400;
}

int summarizevdiffile(struct vdif_file_summary *sum, const char *fileName, int frameSize)
{
	int bufferSize = 1215*8224*8;	/* 2 MB should've encountered all threads of a usual VDIF file; however VGOS DBBC3 uses single-thread 8224-byte x 1215-frame sequences before switching to another thread */
	unsigned char *buffer;
	struct stat st;
	int rv, i, N;
	FILE *in;
	char hasThread[VDIF_MAX_THREAD_ID + 1];
	struct vdif_header *vh0;	/* pointer to the prototype header */
	int hasEDV3 = 0;		/* no VLBA headers found yet */
	int lasts;

	/* Initialize things */

	resetvdiffilesummary(sum);
	strncpy(sum->fileName, fileName, VDIF_SUMMARY_FILE_LENGTH-1);
	memset(hasThread, 0, sizeof(hasThread));
	sum->startSecond = 1<<30;
 
	in = fopen(fileName, "r");
	if(!in)
	{
		return EVDIFCANTOPEN;
	}

	if(fseeko(in, 0, SEEK_SET) < 0)	/* can't seek: can't summarize. */
	{
		return EVDIFCANTSEEK;
	}

	// note: do stat() after open() (not before) as filesize may be updated during open()
	rv = stat(fileName, &st);
	if(rv < 0)
	{
		return EVDIFCANTSTAT;
	}

	sum->fileSize = st.st_size;


	if(sum->fileSize < 2*bufferSize)
	{
		bufferSize = sum->fileSize;
	}

	buffer = (unsigned char *)malloc(bufferSize);
	if(!buffer)
	{
		fclose(in);

		return EVDIFCANTMALLOC;
	}


	/* Get initial information */

	rv = fread(buffer, 1, bufferSize, in);
	if(rv < bufferSize)
	{
		fclose(in);
		free(buffer);

		return EVDIFSHORTREAD;
	}

	if(frameSize == 0)
	{
		/* we weren't given the hint of frame size, so try to figure it out... */

		frameSize = determinevdifframesize(buffer, bufferSize);
		if(frameSize <= 0)
		{
			fclose(in);
			free(buffer);

			return frameSize;
		}
	}

	sum->frameSize = frameSize;
	N = bufferSize - frameSize - VDIF_HEADER_BYTES;

	/* Work on beginning of file */

	sum->firstFrameOffset = determinevdifframeoffset(buffer, bufferSize, frameSize);
	if(sum->firstFrameOffset < 0)
	{
		fclose(in);
		free(buffer);

		return sum->firstFrameOffset;	/* pass error code on */
	}
	vh0 = (struct vdif_header *)(buffer + sum->firstFrameOffset);
	sum->epoch = getVDIFEpoch(vh0);
	sum->nBit = getVDIFBitsPerSample(vh0);
	lasts = getVDIFFrameEpochSecOffset(vh0);

	for(i = sum->firstFrameOffset; i < N; )
	{
		struct vdif_header *vh;
		int f, s;

		vh = (struct vdif_header *)(buffer + i);
		s = getVDIFFrameEpochSecOffset(vh);
		
		if(getVDIFFrameBytes(vh) == frameSize &&
		   getVDIFEpoch(vh) == sum->epoch &&
		   getVDIFBitsPerSample(vh) == sum->nBit &&
		   abs(s - lasts) < 2)
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

			if(vh->eversion == 3 && hasEDV3 == 0)
			{
				const vdif_edv3_header *edv3 = (const vdif_edv3_header *)vh0;
				long long int sampRate;
				int dataSize = frameSize - (vh->legacymode ? 16 : 32);

				hasEDV3 = 1;

				sampRate = edv3->samprate * 1000 * 2;	/* factor of 2 because header sample rate is complex */
				if(edv3->samprateunits == 1)
				{
					sampRate *= 1000;
				}

				sum->framesPerSecond = sampRate*sum->nBit/(8LL*dataSize);
			}

			i += frameSize;

			lasts = s;
		}
		else
		{
			/* Not a good frame. */
			++i;
		}
	}

	/* Work on end of file, if file is long enough */
	
	if(sum->fileSize > bufferSize)
	{
		int offset;
		off_t seeked;

		seeked = fseeko(in, sum->fileSize - bufferSize, SEEK_SET);
		if(seeked != 0)
		{
			fclose(in);
			free(buffer);

			return EVDIFCANTSEEK;
		}

		rv = fread(buffer, 1, bufferSize, in);
		if(rv < bufferSize)
		{
			fclose(in);
			free(buffer);

			return EVDIFSHORTREAD;
		}

		offset = determinevdifframeoffset(buffer, bufferSize, frameSize);
		if(offset < 0)
		{
			fclose(in);
			free(buffer);

			return offset;	/* pass error code on */
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
	fclose(in);

	return 0;
}

