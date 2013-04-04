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
// $Id: vdifio.h 4140 2011-12-13 04:23:35Z ChrisPhillips $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/vdifio/trunk/src/vdifio.h $
// $LastChangedRevision: 4140 $
// $Author: ChrisPhillips $
// $LastChangedDate: 2011-12-12 21:23:35 -0700 (Mon, 12 Dec 2011) $
//
//============================================================================


#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <vdifio.h>
#include "config.h"

#ifdef WORDS_BIGENDIAN
#define FILL_PATTERN 0x44332211UL
#else
#define FILL_PATTERN 0x11223344UL
#endif


/* greatest common divisor, from wikipedia */
static unsigned int gcd(unsigned int u, unsigned int v)
{
  // simple cases (termination)
  if (u == v)
    return u;
  if (u == 0)
    return v;
  if (v == 0)
    return u;
 
  // look for factors of 2
  if ((~u) & 1) // u is even
  {
    if (v & 1) // v is odd
    {
      return gcd(u >> 1, v);
    }
    else // both u and v are even
    {
      return gcd(u >> 1, v >> 1) << 1;
    }
  }
  if ((~v) & 1) // u is odd, v is even
  {  
    return gcd(u, v >> 1);
  }
  // reduce larger argument
  if (u > v)
  {  
    return gcd((u - v) >> 1, v);
  }
  return gcd((v - u) >> 1, u);
}


/* Params are:
 *
 * dest:
 *	pointer to output (multiplexed, single-thread) VDIF data.
 *	Needs to be at least framesize*(nFrame + 2 + nSort) in size
 * nFrame:
 *	attempt to generate nFrame single-thread VDIF packets.
 * src:
 *	pointer to input (multi-thread) VDIF data.
 * length:
 *	length of input data.
 * inputFrameSize:
 *	length of single-thread VDIF data packet
 * inputFramesPerSecond:
 *	Number of frames per second per thread of input to expect
 * nBit:
 *	number of bits per sample
 * nThread:
 *	number of input threads.
 * threadIds:
 *	list of thread ids.
 * nSort:
 *	maximum out-of-orderness to allow.
 * nGap:
 *	maximum gap in frame number to allow before returning early.
 *
 * Will stop when one of three conditions occurs:
 * 1. nFrames of dest data are produced
 * 2. length of src data are used
 * 3. a gap longer than nGap frames is encountered
 *
 * Returns:
 *  < 0 on error
 *  Number of processed bytes from source on success
 */

int vdifmux(unsigned char *dest, int nFrame, const unsigned char *src, int length, int inputFrameSize, int inputFramesPerSecond, int nBit, int nThread, const int *threadIds, int nSort, int nGap)
{
	const int verbose = 3;

	const int maxThreads = 1024;
	unsigned char chanIndex[maxThreads];	/* map from threadId to channel number (0 to nThread-1) */
	int threadId;
	int nValidFrame = 0;			/* counts number of valid input frames found so far */
	int nSkip = 0;				/* counts number of bytes skipped (not what we are looking for) */
	int nFill = 0;				/* counts number of bytes skipped that were fill pattern */
	long long startFrameNumber = -1;	/* = seconds*inputFramesPerSecond + frameNumber */
	int frameGranularity;			/* number of frames required to make an integer number of nanoseconds */

	int i;					/* index into src */
	int N = length - inputFrameSize;	/* max value to allow i to be */
	int highestDestIndex = 0;
	int maxDestIndex;
	int outputFrameSize;
	int nOutputChan;			/* nThread rounded up to nearest power of 2 */
	uint32_t goodMask;			/* mask value that represents all channels are present */
	int bytesProcessed = 0;			/* the program return value */
	int nEnd = 0;				/* number of frames processed after the end of the buffer first reached */

	/* input checks and initialization */
	if(nBit != 1 && nBit != 2 && nBit != 4 && nBit != 8)
	{
		return -1;
	}
	if(nThread < 1 || nThread*nBit > 32)
	{
		return -2;
	}
	if(nGap < nSort)
	{
		nGap = nSort;
	}

	for(nOutputChan = 1; nOutputChan >= nThread; nOutputChan *= 2) ;

	memset(chanIndex, 255, maxThreads);
	for(threadId = 0; threadId < nThread; ++threadId)
	{
		if(threadIds[threadId] < 0 || threadIds[threadId] >= maxThreads)
		{
			return -3;
		}
		chanIndex[threadIds[threadId]] = threadId;
	}

	frameGranularity = inputFramesPerSecond/gcd(inputFramesPerSecond, 1000000000);
	outputFrameSize = (inputFrameSize-32)*nOutputChan+32;
	maxDestIndex = nFrame + nSort + 1;
	goodMask = (1 << nThread) - 1;	/* nThread 1s as LSBs and 0s above that */

	if(verbose > 1)
	{
		printf("frame granularity = %d\n", frameGranularity);
		printf("input frame size = %d\n", inputFrameSize);
		printf("output frame size = %d\n", outputFrameSize);
		printf("max dest index = %d\n", maxDestIndex);
		printf("good mask = %04x\n", goodMask);
		for(i = 0; i < nThread; ++i)
		{
			printf("ThreadId[%d] = %d\n", i, threadIds[i]);
		}
	}

	/* clear mask of presense */
	for(i = 0; i < maxDestIndex; ++i)
	{
		uint32_t *p = (uint32_t *)(dest + outputFrameSize*i);
		p[7] = 0;
	}

	/* the fun begins here */
	
	/* Stage 1: find good data and put in output array. */
	for(i = 0; i <= N;)
	{
		unsigned char *cur = src + i;
		vdif_header *vh;
		long long frameNumber;
		int destIndex;		/* frame index into destination array */
		int chanId;

		vh = (vdif_header *)cur;

		if(*((uint32_t *)(cur+inputFrameSize-4)) == FILL_PATTERN)
		{
			/* Fill pattern at end of frame */
			i += inputFrameSize;
			nFill += inputFrameSize;

			continue;
		}
		if(*((uint32_t *)cur) == FILL_PATTERN)
		{
			/* Fill pattern at beginning of frame */
			i += 8;
			nFill += 8;

			continue;
		}
		if(getVDIFFrameBytes(vh) != inputFrameSize ||
		   getVDIFNumChannels(vh) != 1 ||
		   getVDIFBitsPerSample(vh) != nBit)
		{
			i += 4;
			nSkip += 4;

			continue;
		}

		/* If we are here, it looks like we have a VDIF frame to work with */
		threadId = getVDIFThreadID(vh);
		chanId = chanIndex[threadId];
		if(chanId > 32)
		{
			/* Not one of the threads we are looking for */
			i += inputFrameSize;
			nSkip += inputFrameSize;

			if(verbose > 2) { printf("discarding VDIF frame with threadId = %d at position %d\n", threadId, i); }

			continue;
		}

		frameNumber = getVDIFFrameSecond(vh) * inputFramesPerSecond + getVDIFFrameNumber(vh);
		
		if(verbose > 2) { printf("frame with frame number %Ld and threadId %3d (chan %d) found at position %d\n", frameNumber, threadId, chanId, i); }

		if(startFrameNumber < 0)	/* we haven't seen data yet */
		{
			startFrameNumber = frameNumber - nSort;
			startFrameNumber -= (startFrameNumber % frameGranularity);	/* to ensure first frame starts on integer ns */
		
			if(verbose) { printf("startFrameNumber set to %Ld at position %d\n", startFrameNumber, i); }
		}
	
		/* add 1 to reserve the first slot for later semi-in-place corner turning */
		destIndex = frameNumber - startFrameNumber + 1;

		if(destIndex < 1)
		{
			/* no choice but to discard this data */
			i += inputFrameSize;
			nSkip += inputFrameSize;

			continue;
		}
		if(destIndex > maxDestIndex)
		{
			/* start the shut-down procedure */
			if(bytesProcessed == 0)
			{
				bytesProcessed = i;
			}
			i += inputFrameSize;
			++nEnd;
			if(nEnd >= nSort)
			{
				break;
			}
		}
		else 
		{
			uint32_t *p = (uint32_t *)(dest + outputFrameSize*destIndex);
			
			if(destIndex > highestDestIndex + nGap)
			{
				if(nValidFrame > nSort)
				{
					/* if we are out of the probationary nSort period, start the shut-down procedure */
					if(bytesProcessed == 0)
					{
						bytesProcessed = i;
					}
					++nEnd;
					if(nEnd >= nSort)
					{
						break;
					}
				}
				else
				{
					/* otherwise we take this opportunity to reset the startFrameNumber and clear data moved up to now */
					
					startFrameNumber = frameNumber - nSort;
					startFrameNumber -= (startFrameNumber % frameGranularity);	/* to ensure first frame starts on integer ns */

					if(verbose) { printf("startFrameNumber reset to %Ld at position %d\n", startFrameNumber, i); }
					
					/* clear mask of presense */
					for(destIndex = 0; destIndex < highestDestIndex; ++destIndex)
					{
						p = (uint32_t *)(dest + outputFrameSize*destIndex);
						p[7] = 0;
					}
					highestDestIndex = 0;

					/* at this point we're starting over, so there are no valid frames. */
					nSkip += nValidFrame*inputFrameSize;
					nValidFrame = 0;	

					destIndex = frameNumber - startFrameNumber + 1;
					p = (uint32_t *)(dest + outputFrameSize*destIndex);
				}
			}

			/* Finally, here we are at a point where we can copy data */
			
#if 0
			if(p[7] == 0)
			{
				/* frame header not yet copied.  Copy only first 16 bytes */
				memcpy(dest + outputFrameSize*i, src + i, 16);
			}
#endif

			/* set mask indicating valid data in place */
			p[7] |= (1 << chanId);
			memcpy(dest + outputFrameSize*destIndex + 32, src + i + 32, inputFrameSize-32);
			++nValidFrame;
			i += inputFrameSize;
		}

		if(destIndex > highestDestIndex)
		{
			highestDestIndex = destIndex;
		}
	}

	if(bytesProcessed == 0)
	{
		bytesProcessed = i;
	}


	printf("\nOutput frame header generation not yet implemented\n");

	/* Stage 2: do the corner turning in mass */

	printf("Corner turning not yet implemented\n\n");


	/* end */

	if(verbose)
	{
		printf("Number of valid frames converted: %d\n", nValidFrame);
		printf("Number of bytes processed: %d\n", bytesProcessed);
		printf("Number of bytes lost to fill pattern: %d\n", nFill);
		printf("Number of bytes otherwise skipped: %d\n", nSkip);
	}



	return bytesProcessed;
}
