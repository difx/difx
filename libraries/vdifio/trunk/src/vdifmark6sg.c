/***************************************************************************
 *   Copyright (C) 2015 Walter Brisken                                     *
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
#include <fcntl.h>
#include <mark6sg/mark6_sg_vfs.h>
#include "dateutils.h"
#include "config.h"

int summarizevdifmark6(struct vdif_file_summary *sum, const char *scanName, int frameSize)
{
	int bufferSize = 2000000;	/* 2 MB should encounter all threads of a usual VDIF file */
	unsigned char *buffer;
	struct stat st;
	int rv, i, N;
	int mk6fd;
	char hasThread[VDIF_MAX_THREAD_ID + 1];
	struct vdif_header *vh0;	/* pointer to the prototype header */

	/* Initialize things */

	resetvdiffilesummary(sum);
	strncpy(sum->fileName, scanName, VDIF_SUMMARY_FILE_LENGTH-1);
	memset(hasThread, 0, sizeof(hasThread));
	sum->startSecond = 1<<30;

	mk6fd = mark6_sg_open(scanName, O_RDONLY);
	if(!mk6fd)
	{
		return -2;
	}

	rv = mark6_sg_fstat(mk6fd, &st);
	if(rv < 0)
	{
		mark6_sg_close(mk6fd);

		return -1;
	}

	sum->fileSize = st.st_size;

	if(sum->fileSize < 2*bufferSize)
	{
		bufferSize = sum->fileSize;
	}

	buffer = (unsigned char *)malloc(bufferSize);
	if(!buffer)
	{
		mark6_sg_close(mk6fd);

		return -3;
	}


	/* Get initial information */

	rv = mark6_sg_read(mk6fd, buffer, bufferSize);
	if(rv < bufferSize)
	{
		mark6_sg_close(mk6fd);
		free(buffer);

		return -4;
	}

	if(frameSize == 0)
	{
		/* we weren't given the hint of frame size, so try to figure it out... */

		frameSize = determinevdifframesize(buffer, bufferSize);
		if(frameSize <= 0)
		{
			mark6_sg_close(mk6fd);
			free(buffer);

			return -5;
		}
	}

	sum->frameSize = frameSize;
	N = bufferSize - frameSize - VDIF_HEADER_BYTES;

	/* Work on beginning of file */

	sum->firstFrameOffset = determinevdifframeoffset(buffer, bufferSize, frameSize);
	if(sum->firstFrameOffset < 0)
	{
		mark6_sg_close(mk6fd);
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
	
	if(sum->fileSize > bufferSize)
	{
		int offset;

		rv = mark6_sg_pread(mk6fd, buffer, bufferSize, sum->fileSize - bufferSize);
		if(rv < bufferSize)
		{
			mark6_sg_close(mk6fd);
			free(buffer);

			return -8;
		}

		offset = determinevdifframeoffset(buffer, bufferSize, frameSize);
		if(offset < 0)
		{
			mark6_sg_close(mk6fd);
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
	mark6_sg_close(mk6fd);

	return 0;
}

