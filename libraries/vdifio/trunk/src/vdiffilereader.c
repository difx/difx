/***************************************************************************
 *   Copyright (C) 2013-2017 Walter Brisken                                *
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
// $Id: vdiffile.c 8223 2018-03-28 13:00:49Z JanWagner $
// $HeadURL: $
// $LastChangedRevision: 8223 $
// $Author: JanWagner $
// $LastChangedDate: 2018-03-28 15:00:49 +0200 (Wed, 28 Mar 2018) $
//
//============================================================================

// Assistive VDIF reader. Allows opening a (multi-threaded) VDIF file and reading
// it back with the frames of threads tightly "interleaved". The resulting data
// may be passed to vdifmux().
//
// The file-based VDIF reader with pre-interleaving logic helps to counteract an
// issue stemming from the comparably tiny memory window that vdifmux() operates on.
//
// When the underlying VDIF file is highly "clumpy" and its data are passed directly
// to vdifmux(), it can happen that a thread is not found at all inside the small
// window that vdifmux() operates on. This leads to outlier frames erroneusly considered
// as "missing" and results in excess Invalid -marked VDIF frames from vdifmux().

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <vdifio.h>
#include <sys/stat.h>
#include "dateutils.h"
#include "config.h"

static const size_t resyncbuffersize_ = 8*8224; // should fit 2 frames
static unsigned char *resyncbuffer_ = NULL;
static unsigned char *fillpattern_ = NULL;

static int    vdifreader_find_next_threadframe(struct vdif_file_reader *rd, int threadIdx);
static int    vdifreader_resync_next_frameoffset(struct vdif_file_reader *rd, int threadIdx);
static size_t vdifreader_reposition_all(struct vdif_file_reader *rd, size_t offset);
static int    vdifreader_check_eof(struct vdif_file_reader *rd);
static off_t  vdifreader_check_tailptr(struct vdif_file_reader *rd);


static int vdifreader_find_next_threadframe(struct vdif_file_reader *rd, int threadIdx)
{
	struct vdif_header vh;
	int n;
	off_t pos;
	off_t pos0;

	assert(rd != NULL);
	assert(threadIdx <= rd->details.nThread);

	// Latest frame; can be whatever thread
	pos = ftello(rd->fd[threadIdx]);
	if (pos == -1)
	{
		return -1;
	}
	pos0 = pos;

	// printf("vdifreader_find_next_threadframe() for th %d starting search from pos %zd\n", threadIdx, pos0);

	// Advance until a frame of the desired thread is found
	while (1)
	{
		size_t framesz;
		if (fread(&vh, sizeof(vh), 1, rd->fd[threadIdx]) < 1)
		{
			return -1;
		}

		framesz = getVDIFFrameBytes(&vh);
		if (framesz <= 0 || framesz != rd->details.frameSize)
		{
			//printf("th %d, pos %zd, odd frame size %zu\n", threadIdx, pos, framesz);
			int rc;
			fseeko(rd->fd[threadIdx], pos, SEEK_SET);
			rc = vdifreader_resync_next_frameoffset(rd, threadIdx);
			if (rc < 0)
			{
				printf("vdifreader_find_next_threadframe() encountered strange frame size and resync failed!\n");
				return -1;

			}
			continue;
		}

		if (getVDIFThreadID(&vh) == rd->details.threadIds[threadIdx])
		{
			break;
		}

		pos += framesz;
		if (fseeko(rd->fd[threadIdx], pos, SEEK_SET) != 0)
		{
			return -1;
		}
	}

	rd->frame[threadIdx] = getVDIFFrameNumber(&vh);
	rd->sec[threadIdx] = getVDIFFrameEpochSecOffset(&vh);
	rd->fps = (rd->frame[threadIdx] >= rd->fps) ? (rd->frame[threadIdx] + 1) : rd->fps;

	// Advanced by "too much" in a multithreaded VDIF?
	if (rd->details.nThread > 1)
	{
		off_t framedelta = (pos - rd->tail) / getVDIFFrameBytes(&vh);
		rd->desynched[threadIdx] = (framedelta > 15000) && (framedelta > rd->fps/8);
		if (rd->desynched[threadIdx])
		{
			// Severely desynchronized or dropped-out thread (>=0.125 sec),
			// stay at previous frame until other threads have caught up
			pos = rd->head[threadIdx];
			printf(" desync, frame delta %zd, fps %zu\n", framedelta, (size_t)rd->fps);
			// TODO: might propagage desynced[] flag into vdif header field 'Invalid' in the read() func
		}
	}

	n = fseeko(rd->fd[threadIdx], pos, SEEK_SET);
	if (n == -1)
	{
		return -1;
	}
	rd->head[threadIdx] = pos;

	return 0;
}

/** Attempt to resynchronize. Comparing raw data in a sliding window worth two consecutive frames and tries to find
    file offset that results in partially-matched VDIF headers again. */
static int vdifreader_resync_next_frameoffset(struct vdif_file_reader *rd, int threadIdx)
{
	struct vdif_header *vh1, *vh2;
	size_t frameSize;
	off_t pos0;
	int i;

	assert(rd != NULL);
	assert(threadIdx <= rd->details.nThread);

	frameSize = rd->details.frameSize;

	while (1)
	{
		pos0 = ftello(rd->fd[threadIdx]);
		//printf("vdifreader_resync_next_frameoffset() th %d invoked, current offset %zd\n", threadIdx, pos0);

		size_t nrd = fread(resyncbuffer_, 2*frameSize, 1, rd->fd[threadIdx]);
		if (nrd < 1)
		{
			break;
		}
		for (i = 0; i < frameSize-1; i++)
		{
	         	vh1 = (struct vdif_header *)(resyncbuffer_ + i);
         		vh2 = (struct vdif_header *)(resyncbuffer_ + i + frameSize);
			// Use some simple constraints to ensure we don't get unlucky
			if (getVDIFFrameBytes(vh1) == frameSize &&
				getVDIFFrameBytes(vh2) == frameSize &&
				getVDIFEpoch(vh1) == getVDIFEpoch(vh2) &&
				getVDIFFrameEpochSecOffset(vh2) - getVDIFFrameEpochSecOffset(vh1) < 2)
			{
				//printf("vdifreader_resync_next_frameoffset() th %d found valid frame at delta %d, framesize %d, new offset %zu\n", i, threadIdx, getVDIFFrameBytes(vh2), pos0+i);
				fseeko(rd->fd[threadIdx], pos0+i, SEEK_SET);
				return i;
			}
		}
		fseeko(rd->fd[threadIdx], -frameSize, SEEK_CUR);
		pos0 += frameSize;
        }

        return -1;
}


static size_t vdifreader_reposition_all(struct vdif_file_reader *rd, size_t offset)
{
	int n;

	assert(rd != NULL);

	// Location of first valid frame after @offset
	fseeko(rd->fd[0], offset, SEEK_SET);
	vdifreader_resync_next_frameoffset(rd, 0);
	rd->offset = ftello(rd->fd[0]);
	rd->tail = rd->offset;
	for (n=0; n<rd->details.nThread; n++)
	{
		rd->head[n] = rd->offset;
		rd->desynched[n] = 0;
		fseeko(rd->fd[n], rd->offset, SEEK_SET);
		vdifreader_find_next_threadframe(rd, n);
	}

	return rd->offset;
}


static int vdifreader_check_eof(struct vdif_file_reader *rd)
{
	int n;
	int eof = 1;
	for (n=0; n<rd->details.nThread; n++)
	{
		eof = eof & rd->feof[n];
	}
	return eof;
}


static off_t vdifreader_check_tailptr(struct vdif_file_reader *rd)
{
	int n;
	off_t tail = rd->head[0];
	for (n=1; n<rd->details.nThread; n++)
	{
		tail = (rd->head[n] < tail) ? rd->head[n] : tail;
	}
	return tail;
}


/** Assistive VDIF reader, open a VDIF file based upon VDIF details in its summary. */
int vdifreaderOpen(const struct vdif_file_summary *sum, struct vdif_file_reader *rd)
{
	int n;
	if (sum == NULL || rd == NULL)
	{
		return -1;
	}

	rd->details = *sum;
	rd->offset  = 0;
	rd->tail = 0;
	rd->eof = 0;
	rd->fps = 1;
	for (n=0; n<rd->details.nThread; n++)
	{
		rd->feof[n] = 0;
		rd->head[n] = 0;
		rd->desynched[n] = 0;
		rd->fd[n] = fopen(rd->details.fileName, "r");
	}
	if (rd->fd[0] == NULL)
	{
		return -1;
	}

	resyncbuffer_ = (unsigned char *)malloc(resyncbuffersize_);
	fillpattern_ = (unsigned char*)malloc(2*rd->details.frameSize);
	for (n=0; n<2*rd->details.frameSize; n++)
	{
		// Filler int32 0x11223344 : int8 0x44 0x33 0x22 0x11
		unsigned char v = 4 - (n % 4);
		fillpattern_[n] = (v<<4) + v;
	}

	vdifreader_reposition_all(rd, 0L);

	return 0;
}


/** Read VDIF file, de-clumping VDIF threads in the process. */
size_t vdifreaderRead(struct vdif_file_reader *rd, void *buf, size_t count)
{
	size_t nrd = 0, nremain = count;
	if (rd == NULL || buf == NULL || rd->eof)
	{
		return 0;
	}

	while (nrd < count)
	{
		int threadIdx = (((size_t)rd->offset) / (size_t)rd->details.frameSize) % rd->details.nThread;
		int inframeoffset = ((size_t)rd->offset) % ((size_t)rd->details.frameSize);
		int inframeremain = rd->details.frameSize - inframeoffset;
		int n = (inframeremain < nremain) ? inframeremain : nremain;
		int err;

		if (!rd->feof[threadIdx])
		{
			if (fread(buf, 1, n, rd->fd[threadIdx]) < 1)
			{
				rd->feof[threadIdx] = feof(rd->fd[threadIdx]);
				rd->eof = vdifreader_check_eof(rd);
				return nrd;
			}
			rd->tail = vdifreader_check_tailptr(rd);
			if (n >= inframeremain)
			{
				err = vdifreader_find_next_threadframe(rd, threadIdx);
				if (err)
				{
					rd->feof[threadIdx] = feof(rd->fd[threadIdx]);
					rd->eof = vdifreader_check_eof(rd);
				}
			}
		}
		else
		{
			memcpy(buf, fillpattern_ + inframeoffset, n);
		}

		rd->offset += n;
		nrd += n;
		nremain -= n;
		buf += n;
	}

        return nrd;
}


/** Seek the VDIF reader */
size_t vdifreaderSeek(struct vdif_file_reader *rd, size_t offset)
{
	int n;
	if (rd == NULL)
	{
		return 0;
	}

	return vdifreader_reposition_all(rd, offset);
}


/** Return statistics to help deduce VDIF thread "clumpiness" */
int vdifreaderStats(const struct vdif_file_reader *rd, struct vdif_file_reader_stats *st)
{
	int n;
	off_t minOffset;
	if (rd == NULL || st == NULL)
	{
		return -1;
	}

	// File offsets, in units of frames
	st->nThread = rd->details.nThread;
	for (n=0; n<rd->details.nThread; n++)
	{
		off_t offset = ftello(rd->fd[n]);
		if (offset == -1)
		{
			return -1;
		}
		offset -= rd->firstframeoffset;
		offset /= rd->details.frameSize;
		st->threadOffsets[n] = offset;
	}

	// Lowest offset
	minOffset = st->threadOffsets[0];
	for (n=0; n<rd->details.nThread; n++)
	{
		minOffset = (st->threadOffsets[n] < minOffset) ? st->threadOffsets[n] : minOffset;
	}

	// Shift all offsets to be relative to the lowest offset, and look for the largest offset overall
	st->maxOffset = 0;
	for (n=0; n<rd->details.nThread; n++)
	{
		st->threadOffsets[n] -= minOffset;
		st->maxOffset = (st->threadOffsets[n] > st->maxOffset) ? st->threadOffsets[n] : st->maxOffset;
	}
	return 0;
}


/** Close the VDIF reader */
int vdifreaderClose(struct vdif_file_reader *rd)
{
	int n;
	if (rd == NULL)
	{
		return -1;
	}

	for (n=0; n<rd->details.nThread; n++)
	{
		fclose(rd->fd[n]);
	}
	free(fillpattern_);
	free(resyncbuffer_);

	return 0;
}
