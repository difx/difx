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
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/vdifio/trunk/src/vdifio.h $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <vdifio.h>
#include "config.h"

/** local helper that looks at 3 back-to-back frames and checks that they are consistent for an assumed framesize */
static int comparethreeframes(const unsigned char *buffer, int offset, int frameSize)
{
	struct vdif_header *vh1, *vh2, *vh3;

	vh1 = (struct vdif_header *)(buffer + offset);
	vh2 = (struct vdif_header *)(buffer + offset + frameSize);
	vh3 = (struct vdif_header *)(buffer + offset + 2*frameSize);

	if(getVDIFFrameBytes(vh1) == frameSize &&
	   getVDIFFrameBytes(vh2) == frameSize &&
	   getVDIFFrameBytes(vh3) == frameSize &&
	   getVDIFEpoch(vh1) == getVDIFEpoch(vh2) &&
	   getVDIFEpoch(vh1) == getVDIFEpoch(vh3) &&
	   getVDIFFrameEpochSecOffset(vh2) - getVDIFFrameEpochSecOffset(vh1) < 2 &&
	   getVDIFFrameEpochSecOffset(vh3) - getVDIFFrameEpochSecOffset(vh2) < 2 &&
	   getVDIFFrameNumber(vh2) - getVDIFFrameNumber(vh1) < 5 &&
	   getVDIFFrameNumber(vh3) - getVDIFFrameNumber(vh1) < 5)
	{
		return frameSize;
	}
	return -1;
}

/* look for at least 3 back-to-back frames with consistent structure */
int determinevdifframesize(const unsigned char *buffer, int bufferSize)
{
	int frameSize;
	int likelyFrameSizes[] = {5032, 10032, 20032, 40032, 80032, 160032, 8032, 16032, 32032, 1032, 2032, 4032, 8224, 1312};	/* add more here as you like ... */
	const int nLikelyFrameSizes = sizeof(likelyFrameSizes)/sizeof(likelyFrameSizes[0]);
	int f;

	if(bufferSize < 3*(VDIF_HEADER_BYTES + 8))
	{
		/* no way this can be useful */

		return -1;
	}

	/* First check the VDIF header -reported frame size */
	frameSize = getVDIFFrameBytes((struct vdif_header *)buffer);
	if (comparethreeframes(buffer, 0, frameSize) > 0)
	{
		return frameSize;
	}

	/* Next check likely frame sizes */
	for(f = 0; f < nLikelyFrameSizes; ++f)
	{
		int i, N;

		frameSize = likelyFrameSizes[f];

		N = bufferSize - 2*frameSize - VDIF_HEADER_BYTES;
		for(i = 0; i < N ; ++i)
		{
			if (comparethreeframes(buffer, i, frameSize) > 0)
			{
				return frameSize;
			}
		}
	}

	/* Finally do exhaustive search */
	for(frameSize = VDIF_HEADER_BYTES + 8; (frameSize < bufferSize/4) && (frameSize < 524288); frameSize += 8)	/* step over legal frame sizes */
	{
		int i, N;

		N = bufferSize - 2*frameSize - VDIF_HEADER_BYTES;
		for(i = 0; i < N ; ++i)
		{
			if (comparethreeframes(buffer, i, frameSize) > 0)
			{
				return frameSize;
			}
		}
	}

	return -1;
}

/* Look for first pair of consecutive valid frames and return offset to start of the first of these */
/* return -1 on fail */
int determinevdifframeoffset(const unsigned char *buffer, int bufferSize, int frameSize)
{
	int i, N;

	N = bufferSize - frameSize - VDIF_HEADER_BYTES;
	if(N < frameSize || frameSize < VDIF_HEADER_BYTES + 8)
	{
		return -1;
	}

	for(i = 0; i < N ; ++i)
	{
		struct vdif_header *vh1, *vh2;

		vh1 = (struct vdif_header *)(buffer + i);
		vh2 = (struct vdif_header *)(buffer + i + frameSize);

		/* Use some simple constraints to ensure we don't get unlucky */
		if(getVDIFFrameBytes(vh1) == frameSize &&
		   getVDIFFrameBytes(vh2) == frameSize &&
		   getVDIFEpoch(vh1) == getVDIFEpoch(vh2) &&
		   getVDIFFrameEpochSecOffset(vh2) - getVDIFFrameEpochSecOffset(vh1) < 2)
		{
			return i;
		}
	}

	return -1;
}
