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

static int vdifreader_find_next_threadframe(struct vdif_file_reader *rd, int threadIdx);
static size_t vdifreader_reposition_all(struct vdif_file_reader *rd, size_t offset);

static int vdifreader_find_next_threadframe(struct vdif_file_reader *rd, int threadIdx)
{
	struct vdif_header vh;
	off_t pos;

	assert(rd != NULL);
	assert(threadIdx <= rd->details.nThread);

	pos = ftello(rd->fd[threadIdx]);
	if (pos == -1)
	{
		return -1;
	}

	while (1)
	{
		if (fread(&vh, sizeof(vh), 1, rd->fd[threadIdx]) < 1)
		{
			return -1;
		}
		if (getVDIFThreadID(&vh) == rd->details.threadIds[threadIdx])
		{
			break;
		}
		pos += getVDIFFrameBytes(&vh);
		if (fseeko(rd->fd[threadIdx], pos, SEEK_SET) != 0)
		{
			return -1;
		}
	}

	rd->frame[threadIdx] = getVDIFFrameNumber(&vh);
	rd->sec[threadIdx] = getVDIFFrameEpochSecOffset(&vh);

	pos = fseeko(rd->fd[threadIdx], pos, SEEK_SET);
	if (pos == -1)
	{
		return -1;
	}

	return 0;
}

static size_t vdifreader_reposition_all(struct vdif_file_reader *rd, size_t offset)
{
	const int bufferSize = 1215*8224*8;
	unsigned char *buffer;
	int n;

	assert(rd != NULL);

	buffer = (unsigned char *)malloc(bufferSize);

	fseeko(rd->fd[0], offset, SEEK_SET);
	fread(buffer, 1, bufferSize, rd->fd[0]);
	rd->firstframeoffset = determinevdifframeoffset(buffer, bufferSize, rd->details.frameSize);
	free(buffer);

	rd->offset = offset + rd->firstframeoffset;
	for (n=0; n<rd->details.nThread; n++)
	{
		fseeko(rd->fd[n], rd->offset, SEEK_SET);
		vdifreader_find_next_threadframe(rd, n);
	}

	return rd->offset;
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
	for (n=0; n<rd->details.nThread; n++)
	{
		rd->fd[n] = fopen(rd->details.fileName, "r");
	}
	vdifreader_reposition_all(rd, 0L);

	return 0;
}


/** Read VDIF file, de-clumping VDIF threads in the process. */
size_t vdifreaderRead(struct vdif_file_reader *rd, void *buf, size_t count)
{
	size_t nrd = 0, nremain = count;
	if (rd == NULL || buf == NULL)
	{
		return 0;
	}

	while (nrd < count)
	{
		int threadIdx = (((size_t)rd->offset) / (size_t)rd->details.frameSize) % rd->details.nThread;
		int inframeoffset = ((size_t)rd->offset) % ((size_t)rd->details.frameSize);
		int inframeremain = rd->details.frameSize - inframeoffset;
		int n = (inframeremain < nremain) ? inframeremain : nremain;

		if (fread(buf, 1, n, rd->fd[threadIdx]) < 1)
		{
			return nrd;
		}
		if (n >= inframeremain)
		{
			vdifreader_find_next_threadframe(rd, threadIdx);
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

	return 0;
}
