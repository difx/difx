/***************************************************************************
 *   Copyright (C) 2013-2015 Walter Brisken                                *
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


/* TODO list

 * Constrain output lengths to frame granularity ???

*/



#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>
#include <time.h>
#include <vdifio.h>
#include "config.h"


#ifdef WORDS_BIGENDIAN
#define FILL_PATTERN 0x44332211UL
#else
#define FILL_PATTERN 0x11223344UL
#endif

#define MAGIC_BAD_THREAD	20000


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
 * Returns:
 *  < 0 on error
 *  0 on success
 *
 * see ../utils/vmux.c for example usage of this function
 */
int configurevdifmux(struct vdif_mux *vm, int inputFrameSize, int inputFramesPerSecond, int nBit, int nThread, const int *threadIds, int nSort, int nGap, int flags)
{
	int i;

	if(nThread > sizeof(vm->goodMask)*8)
	{
		fprintf(stderr, "Error: configurevdifmux: cannot run vdifmux on more than %d threads; %d requested.\n", (int)sizeof(vm->goodMask)*8, nThread);

		return -3;
	}

	vm->cornerTurner = getCornerTurner(nThread, nBit);
	if(vm->cornerTurner == 0)
	{
		fprintf(stderr, "No corner turner implemented for %d threads and %d bits\n", nThread, nBit);
		
		return -1;
	}

	vm->inputFrameSize = inputFrameSize;
	vm->inputFramesPerSecond = inputFramesPerSecond;
	vm->frameGranularity = inputFramesPerSecond/gcd(inputFramesPerSecond, 1000000000);
	vm->nBit = nBit;
	vm->nSort = nSort;
	vm->nGap = nGap;
	nThread = abs(nThread);	/* in case a special corner turner was requested */
	vm->nThread = nThread;
	vm->flags = flags;

	if(vm->nGap < vm->nSort)
	{
		vm->nGap = vm->nSort;
	}

	for(vm->nOutputChan = 1; vm->nOutputChan < vm->nThread; vm->nOutputChan *= 2) ;



	/* NOTE!!! legacy header support is not yet complete.  It is being designed so that it may be possible to come from and go to either LEGACY or nonLEGACY headers */
	/* to complete the feature, all reference to VDIF_HEADER_BYTES in vdifmux() need to be replaced */

	if(flags & VDIF_MUX_FLAG_INPUTLEGACY)
	{
		vm->inputDataSize = vm->inputFrameSize - VDIF_LEGACY_HEADER_BYTES;
	}
	else
	{
		vm->inputDataSize = vm->inputFrameSize - VDIF_HEADER_BYTES;
	}

	vm->outputDataSize = vm->inputDataSize*vm->nOutputChan;

	if(flags & VDIF_MUX_FLAG_OUTPUTLEGACY)
	{
		vm->outputFrameSize = vm->outputDataSize + VDIF_LEGACY_HEADER_BYTES;
	}
	else
	{
		vm->outputFrameSize = vm->outputDataSize + VDIF_HEADER_BYTES;
	}


	for(i = 0; i <= VDIF_MAX_THREAD_ID; ++i)
	{
		vm->chanIndex[i] = MAGIC_BAD_THREAD;
	}
	for(i = 0; i < nThread; ++i)
	{
		if(threadIds[i] < 0 || threadIds[i] > VDIF_MAX_THREAD_ID)
		{
			fprintf(stderr, "Error: illegal thread Id: %d ; needs to be within 0..%d, inclusive.\n", threadIds[i], VDIF_MAX_THREAD_ID);
			
			return -2;
		}
		vm->chanIndex[threadIds[i]] = i;
	}

	vm->goodMask = (1ULL << nThread) - 1;	/* nThread 1s as LSBs and 0s above that */

	return 0;
}

void printvdifmux(const struct vdif_mux *vm)
{
	if(vm)
	{
		int i;

		printf("vdif_mux:\n");
		printf("  inputFrameSize = %d\n", vm->inputFrameSize);
		printf("  inputDataSize = %d\n", vm->inputDataSize);
		printf("  outputFrameSize = %d\n", vm->outputFrameSize);
		printf("  outputDataSize = %d\n", vm->outputDataSize);
		printf("  inputFramesPerSecond = %d\n", vm->inputFramesPerSecond);
		printf("  frameGranularity = %d\n", vm->frameGranularity);
		printf("  nBit = %d\n", vm->nBit);
		printf("  nSort = %d\n", vm->nSort);
		printf("  nGap = %d\n", vm->nGap);
		printf("  nThread = %d\n", vm->nThread);
		printf("  nOutputChan = %d\n", vm->nOutputChan);
		printf("  goodMask = 0x%" PRIx64 "\n", vm->goodMask);
		printf("  flags = 0x%02x\n", vm->flags);
		printf("  thread to channel map:\n");
		for(i = 0; i <= VDIF_MAX_THREAD_ID; ++i)
		{
			if(vm->chanIndex[i] != MAGIC_BAD_THREAD)
			{
				printf("    %d -> %d\n", i, vm->chanIndex[i]);
			}
		}
	}
	else
	{
		printf("vdif_mux is NULL\n");
	}
}

/* Params are:
 *
 * dest:
 *	pointer to output (multiplexed, single-thread) VDIF data.
 * destSize:
 *	the size of the output (dest) array.
 * src:
 *	pointer to input (multi-thread) VDIF data.
 * srcSize:
 *	length of the input (src) array.
 * vm:
 *      vdif_mux structure initialized by call to above function
 * startOutputFrameNumber:
 *      if >= 0, fix the beginning of the dest buffer grid to be this.
 *      Set to -1 unless you really know what frame number to expect.
 * stats:
 *	statistics and information about the processing
 *
 * Will stop when one of three conditions occurs:
 * 1. dest array size is exceeded
 * 2. src array is exhausted
 * 3. a gap longer than nGap frames is encountered
 *
 * Returns:
 *  < 0 on error
 *  Number of processed bytes from source on success
 *
 * The output data is to be stored in dest.  Statistics and some details of the produced data are stored in the stats.
 * Output data is uniform in time.  All initially missing data is replaced with valid VDIF packets with the invalid bit set.
 *
 * see ../utils/vmux.c for example usage of this function
 */
int vdifmux(unsigned char *dest, int destSize, const unsigned char *src, int srcSize, const struct vdif_mux *vm, int64_t startOutputFrameNumber, struct vdif_mux_statistics *stats)
{
	int threadId;
	int nValidFrame = 0;			/* counts number of valid input frames found so far */
	int nSkip = 0;				/* counts number of bytes skipped (not what we are looking for) */
	int nFill = 0;				/* counts number of bytes skipped that were fill pattern */
	int nDup = 0;				/* number of duplicate frames found */
	int nInvalidFrame = 0;			/* counts number of VDIF frames skipped because of invalid bits */
	int64_t startFrameNumber;		/* = seconds*inputFramesPerSecond + frameNumber */

	int i;					/* index into src */
	int f;
	int N;					/* max value to allow i to be */
	int highestDestIndex = 0;
	int maxDestIndex;
	int maxSrcIndex;
	int bytesProcessed = 0;			/* the program return value */
	int nEnd = 0;				/* number of frames processed after the end of the buffer first reached */
	int nGoodOutput = 0;
	int nBadOutput = 0;
	int nWrongThread = 0;
	int seconds, frameNum;
	vdif_header outputHeader;
	int epoch = -1;
	int highestSortedDestIndex = -1;
	int vhUnset = 1;

	N = srcSize - vm->inputFrameSize;

	if(vm->flags & VDIF_MUX_FLAG_GOTOEND)
	{
		maxSrcIndex = srcSize - vm->inputFrameSize;
	}
	else
	{
		maxSrcIndex = srcSize - vm->nSort*vm->inputFrameSize;
	}

	if(vm->flags & VDIF_MUX_FLAG_RESPECTGRANULARITY)
	{
		if(startOutputFrameNumber > 0)
		{
			int m;

			m = startOutputFrameNumber % vm->frameGranularity;
			if(m != 0)
			{
				startOutputFrameNumber += (vm->frameGranularity - m);
			}
		}
	}

	maxDestIndex = destSize/vm->outputFrameSize - 1;

	startFrameNumber = startOutputFrameNumber;

	/* clear mask of presence */
	for(i = 0; i <= maxDestIndex; ++i)
	{
		uint64_t *p = (uint64_t *)(dest + vm->outputFrameSize*i);
		p[3] = 0;
	}

	/* Stage 1: find good data and put in output array. */
	for(i = 0; i <= N;)
	{
		const unsigned char *cur = src + i;
		const vdif_header *vh = (vdif_header *)cur;
		int64_t frameNumber;
		int destIndex;		/* frame index into destination array */
		int chanId;

		if( (vm->flags & VDIF_MUX_FLAG_ENABLEVALIDITY) && (getVDIFFrameInvalid(vh) > 0) )
		{
			i += vm->inputFrameSize;
			++nInvalidFrame;

			continue;
		}

		if(*((uint32_t *)(cur+vm->inputFrameSize-4)) == FILL_PATTERN)
		{
			/* Fill pattern at end of frame or invalid bit is set */
			i += vm->inputFrameSize;
			nFill += vm->inputFrameSize;

			continue;
		}
		if(*((uint32_t *)cur) == FILL_PATTERN)
		{
			/* Fill pattern at beginning of frame */
			i += 8;
			nFill += 8;

			continue;
		}
		if(getVDIFFrameBytes(vh) != vm->inputFrameSize ||
		   getVDIFNumChannels(vh) != 1 ||
		   getVDIFBitsPerSample(vh) != vm->nBit)
		{
			i += 4;
			nSkip += 4;

			continue;
		}

		/* If we are here, it looks like we have a VDIF frame to work with */
		threadId = getVDIFThreadID(vh);
		chanId = vm->chanIndex[threadId];
		if(chanId == MAGIC_BAD_THREAD)
		{
			/* Not one of the threads we are looking for */
			i += vm->inputFrameSize;
			++nWrongThread;

			continue;
		}

		frameNumber = (int64_t)(getVDIFFrameEpochSecOffset(vh)) * vm->inputFramesPerSecond + getVDIFFrameNumber(vh);
		
		if(startFrameNumber < 0)	/* we haven't seen data yet */
		{
			startFrameNumber = frameNumber - vm->nSort;
			startFrameNumber -= (startFrameNumber % vm->frameGranularity);	/* to ensure first frame starts on integer ns */
		}

		if(vhUnset)
		{
			memcpy(&outputHeader, vh, 16);
			memset(((char *)&outputHeader) + 16, 0, 16);

			/* use this first good frame to generate the prototype VDIF header for the output */
			setVDIFNumChannels(&outputHeader, vm->nOutputChan);
			setVDIFThreadID(&outputHeader, 0);
			setVDIFFrameBytes(&outputHeader, vm->outputFrameSize);
			setVDIFFrameInvalid(&outputHeader, 0);
			epoch = getVDIFEpoch(&outputHeader);

			vhUnset = 0;
		}

		destIndex = frameNumber - startFrameNumber;

		if(destIndex < 0)
		{
			/* no choice but to discard this data */
			i += vm->inputFrameSize;
			nSkip += vm->inputFrameSize;

			continue;
		}
		if(i > maxSrcIndex)
		{
			if(highestSortedDestIndex < 0)
			{
				highestSortedDestIndex = highestDestIndex;
			}
		}
		if(destIndex > maxDestIndex)
		{
			/* start the shut-down procedure */
			if(bytesProcessed == 0)
			{
				bytesProcessed = i;
			}
			i += vm->inputFrameSize;
			++nEnd;
			if(nEnd >= vm->nSort)
			{
				break;
			}
		}
		else /* here we have a usable packet */
		{
			uint64_t *p = (uint64_t *)(dest + vm->outputFrameSize*destIndex);
			
			if(destIndex > highestDestIndex + vm->nGap)
			{
				if(nValidFrame > vm->nSort || startOutputFrameNumber >= 0)
				{
					/* if we are out of the probationary nSort period, start the shut-down procedure */
					if(bytesProcessed == 0)
					{
						bytesProcessed = i;
					}
					++nEnd;
					if(nEnd >= vm->nSort)
					{
						break;
					}
				}
				else
				{
					/* otherwise we take this opportunity to reset the startFrameNumber and clear data moved up to now */
					
					startFrameNumber = frameNumber - vm->nSort;
					startFrameNumber -= (startFrameNumber % vm->frameGranularity);	/* to ensure first frame starts on integer ns */

					/* clear mask of presence */
					for(destIndex = 0; destIndex < highestDestIndex; ++destIndex)
					{
						p = (uint64_t *)(dest + vm->outputFrameSize*destIndex);
						p[3] = 0;
					}
					highestDestIndex = 0;

					/* at this point we're starting over, so there are no valid frames. */
					nSkip += nValidFrame*vm->inputFrameSize;
					nValidFrame = 0;	

					destIndex = frameNumber - startFrameNumber;
					p = (uint64_t *)(dest + vm->outputFrameSize*destIndex);
				}
			}

			/* set mask indicating valid data in place */
			if(p[3] & (1 << chanId))
			{
				++nDup;
			}
			else
			{
				/* NOTE!  We're using just a bit of the dest buffer to store pointers to the original data payloads */
				const unsigned char **threadBuffers = (const unsigned char **)(dest + vm->outputFrameSize*destIndex + VDIF_HEADER_BYTES);

				p[3] |= (1 << chanId);
				threadBuffers[chanId] = cur + VDIF_HEADER_BYTES;	/* store pointer to data for later corner turning */
				
				++nValidFrame;

				if(destIndex > highestDestIndex)
				{
					highestDestIndex = destIndex;
				}
				
				/* Once we have nSort good frames, we know the earliest frame acceptable, so now scrunch forward... */
				if(nValidFrame == vm->nSort && startOutputFrameNumber < 0)
				{
					int firstUsed;

					for(firstUsed = 0; firstUsed <= highestDestIndex; ++firstUsed)
					{
						p = (uint64_t *)(dest + vm->outputFrameSize*firstUsed);

						if(p[3] > 0)
						{
							break;
						}
					}

					if(firstUsed > 0 && firstUsed <= highestDestIndex)
					{
						/* Ensure frame granularity conditions remain met */
						firstUsed -= (firstUsed % vm->frameGranularity);

						/* slide data forward */
						for(f = firstUsed; f <= highestDestIndex; ++f)
						{
							int e = f-firstUsed;
							uint64_t *q;

							p = (uint64_t *)(dest + vm->outputFrameSize*e);
							q = (uint64_t *)(dest + vm->outputFrameSize*f);
							p[3] = q[3];

							if(p[3] != 0)
							{
								const unsigned char * const *threadBuffers2;

								threadBuffers  = (const unsigned char **)(dest + vm->outputFrameSize*e + VDIF_HEADER_BYTES);
								threadBuffers2 = (const unsigned char **)(dest + vm->outputFrameSize*f + VDIF_HEADER_BYTES);
								memcpy(threadBuffers, threadBuffers2, vm->nThread*sizeof(const unsigned char *));
							}
						}

						/* zero any remaining masks */
						for(f = highestDestIndex+1-firstUsed; f <= highestDestIndex ; ++f)
						{
							uint64_t *q;

							q = (uint64_t *)(dest + vm->outputFrameSize*f);
							q[3] = 0;
						}

						/* change a few other indexes */
						highestDestIndex -= firstUsed;
						highestSortedDestIndex -= firstUsed;
						startFrameNumber += firstUsed;
					}
				}
			}
			i += vm->inputFrameSize;
		}
	}

	if(bytesProcessed == 0)
	{
		bytesProcessed = i;
	}

	/* If there were fewer than nSort frames read and start output frame number not specified, scrunch output to front */
	if(nValidFrame < vm->nSort && startOutputFrameNumber < 0)
	{
		int firstUsed;
		uint64_t *p;

		for(firstUsed = 0; firstUsed <= highestDestIndex; ++firstUsed)
		{
			p = (uint64_t *)(dest + vm->outputFrameSize*firstUsed);

			if(p[3] == vm->goodMask)
			{
				break;
			}
		}
		if(firstUsed > 0 && firstUsed <= highestDestIndex)
		{
			/* Ensure frame granularity conditions remain met */
			firstUsed -= (firstUsed % vm->frameGranularity);

			/* slide data forward */
			for(f = firstUsed; f <= highestDestIndex; ++f)
			{
				int e = f-firstUsed;
				uint64_t *q;

				p = (uint64_t *)(dest + vm->outputFrameSize*e);
				q = (uint64_t *)(dest + vm->outputFrameSize*f);
				p[3] = q[3];

				if(p[3] != 0)
				{
					const unsigned char **threadBuffers;
					const unsigned char **threadBuffers2;

					threadBuffers  = (const unsigned char **)(dest + vm->outputFrameSize*e + VDIF_HEADER_BYTES);
					threadBuffers2 = (const unsigned char **)(dest + vm->outputFrameSize*f + VDIF_HEADER_BYTES);
					memcpy(threadBuffers, threadBuffers2, vm->nThread*sizeof(const unsigned char *));
				}
			}

			/* zero any remaining masks */
			for(f = highestDestIndex+1-firstUsed; f <= highestDestIndex ; ++f)
			{
				uint64_t *q;

				q = (uint64_t *)(dest + vm->outputFrameSize*f);
				q[3] = 0;
			}

			/* change a few other indexes */
			highestDestIndex -= firstUsed;
			highestSortedDestIndex -= firstUsed;
			startFrameNumber += firstUsed;
		}
	}

	/* Here the source dried up, but we want to be able to reconstruct a complete stream later, so back up, a bit to look for most recent incomplete frame consistent with nSort */
	if(highestSortedDestIndex >= 0)
	{
		for(f = highestDestIndex; f > highestSortedDestIndex; --f)
		{
			const uint64_t *p = (const uint64_t *)(dest + vm->outputFrameSize*f);
			uint64_t mask = p[3];

			if(mask != vm->goodMask)
			{
				const unsigned char * const *threadBuffers = (const unsigned char **)(dest + vm->outputFrameSize*f + VDIF_HEADER_BYTES);
				int t;
				
				highestDestIndex = f-1;
				for(t = 0; t < vm->nThread; ++t)
				{
					if(mask & (1<<t))
					{
						int d;

						d = threadBuffers[t] - src - VDIF_HEADER_BYTES;	/* this is number of bytes into input stream */
						if(d < bytesProcessed)
						{
							bytesProcessed = d;
						}
						--nValidFrame;
					}
				}
			}
		}
	}
	else
	{
		int minDestIndex = highestDestIndex - vm->nSort/vm->nThread;

		if(minDestIndex >= 0) for(f = highestDestIndex; f >= minDestIndex; --f)
		{
			const uint64_t *p = (const uint64_t *)(dest + vm->outputFrameSize*f);
			uint64_t mask = p[3];

			if(mask != vm->goodMask)
			{
				const unsigned char * const *threadBuffers = (const unsigned char **)(dest + vm->outputFrameSize*f + VDIF_HEADER_BYTES);
				int t;
				
				highestDestIndex = f-1;
				for(t = 0; t < vm->nThread; ++t)
				{
					if(mask & (1<<t))
					{
						int d;

						d = threadBuffers[t] - src - VDIF_HEADER_BYTES;	/* this is number of bytes into input stream */
						if(d < bytesProcessed)
						{
							bytesProcessed = d;
						}
						--nValidFrame;
					}
				}
			}
		}
	}

	/* If source did not dry up and dest got within nGap of filling, set highestDestIndex to produce (invalid) data through the end of the dest buffer for continuity */
	if(i < N && highestDestIndex < maxDestIndex && highestDestIndex > maxDestIndex - vm->nGap)
	{
		highestDestIndex = maxDestIndex;
	}

	/* Stage 2: do the corner turning and header population */

	seconds = startFrameNumber/vm->inputFramesPerSecond;
	frameNum = startFrameNumber%vm->inputFramesPerSecond;

	for(f = 0; f <= highestDestIndex; ++f)
	{
		unsigned char *frame = dest + vm->outputFrameSize*f;	/* points to rearrangement destination */
		const uint64_t *p = (const uint64_t *)frame;
		uint64_t mask;

		mask = p[3];

		/* generate header for output frame */
		memcpy(frame, (const char *)&outputHeader, VDIF_HEADER_BYTES);
		setVDIFFrameEpochSecOffset((vdif_header *)frame, seconds);
		setVDIFFrameNumber((vdif_header *)frame, frameNum);

		if(mask == vm->goodMask)
		{
			const unsigned char * const *threadBuffers = (const unsigned char * const *)(frame + VDIF_HEADER_BYTES);

			/* Note: The following function only works because all of the corner turners make a copy of the
			 * thread pointers before beginning */
			vm->cornerTurner(frame + VDIF_HEADER_BYTES, threadBuffers, vm->outputDataSize);

			++nGoodOutput;
		}
		else
		{
			/* Set invalid bit */
			setVDIFFrameInvalid((vdif_header *)frame, 1);

			++nBadOutput;
		}

		++frameNum;
		if(frameNum >= vm->inputFramesPerSecond)
		{
			++seconds;
			frameNum -= vm->inputFramesPerSecond;
		}
	}

	if(stats)
	{
		stats->nValidFrame += nValidFrame;
		stats->nInvalidFrame += nInvalidFrame;
		stats->nDiscardedFrame += (nValidFrame - vm->nThread*nGoodOutput);
		stats->nWrongThread += nWrongThread;
		stats->nDuplicateFrame += nDup;
		stats->nSkippedByte += nSkip;
		stats->nFillByte += nFill;
		stats->bytesProcessed += bytesProcessed;
		stats->nGoodFrame += nGoodOutput;

		stats->srcSize = srcSize;
		stats->srcUsed = bytesProcessed;
		stats->destSize = destSize;
		stats->destUsed = (nGoodOutput + nBadOutput)*vm->outputFrameSize;
		stats->inputFrameSize = vm->inputFrameSize;
		stats->outputFrameSize = vm->outputFrameSize;
		stats->outputFrameGranularity = vm->frameGranularity;
		stats->outputFramesPerSecond = vm->inputFramesPerSecond;
		stats->nOutputFrame = nGoodOutput + nBadOutput;
		stats->epoch = epoch;
		stats->startFrameNumber = startFrameNumber;
		
		++stats->nCall;
	}

	return bytesProcessed;
}

void printvdifmuxstatistics(const struct vdif_mux_statistics *stats)
{
	if(stats)
	{
		printf("VDIF multiplexer statistics:\n");
		printf("  Number of calls to vdifmux         = %d\n", stats->nCall);
		printf("  Number of valid input frames       = %lld\n", stats->nValidFrame);
		printf("  Number of invalid input frames     = %lld\n", stats->nInvalidFrame);
		printf("  Number of duplicate frames         = %lld\n", stats->nDuplicateFrame);
		printf("  Number of discarded frames         = %lld\n", stats->nDiscardedFrame);
		printf("  Number of wrong-thread frames      = %lld\n", stats->nWrongThread);
		printf("  Number of skipped interloper bytes = %lld\n", stats->nSkippedByte);
		printf("  Number of fill pattern bytes       = %lld\n", stats->nFillByte);
		printf("  Total number of bytes processed    = %lld\n", stats->bytesProcessed);
		printf("  Total number of good output frames = %lld\n", stats->nGoodFrame);
		printf("Properties of output data from recent call:\n");
		printf("  Input frame size                   = %d\n", stats->inputFrameSize);
		printf("  Output frame size                  = %d\n", stats->outputFrameSize);
		printf("  Number of output frames            = %d\n", stats->nOutputFrame);
		printf("  Epoch                              = %d\n", stats->epoch);
		printf("  Start output frame number          = %" PRId64 "\n", stats->startFrameNumber);
		printf("  Output frame granularity           = %d\n", stats->outputFrameGranularity);
		printf("  Output frames per second           = %d\n", stats->outputFramesPerSecond);
		printf("  %d/%d src bytes consumed\n", stats->srcUsed, stats->srcSize);
		printf("  %d/%d dest bytes generated\n", stats->destUsed, stats->destSize);
	}
	else
	{
		fprintf(stderr, "Weird: printvdifmuxstatistics called with null pointer\n");
	}
}

void resetvdifmuxstatistics(struct vdif_mux_statistics *stats)
{
	if(stats)
	{
		memset(stats, 0, sizeof(struct vdif_mux_statistics));
	}
}

static int testCornerTurn(const unsigned char *outputBuffer, const unsigned char * const *threadData, int outputBytes, int nt, int b)
{
	int nError = 0;
	int nSample = 8*outputBytes/b;	/* total samples in output stream */
	int s;
	int ut;	/* lowest power of 2 >= nt */
	unsigned int bitmask;

	for(ut = 1; ut < nt; ut *= 2);

	bitmask = (1 << b) - 1;

	for(s = 0; s < nSample; ++s)
	{
		int t;	/* thread */
		unsigned int outputvalue, inputvalue;
		int outputsample, outputshift, inputsample, inputshift;

		t = s % ut;
		if(t >= nt)
		{
			/* ignore padded data */
			continue;
		}

		outputsample = s*b / 8;
		outputshift = (s*b) % 8;
		outputvalue = (outputBuffer[outputsample] >> outputshift) & bitmask;

		inputsample = s*b/ut / 8;
		inputshift = (s*b/ut) % 8;
		inputshift /= b;
		inputshift *= b;
		inputvalue = (threadData[t][inputsample] >> inputshift) & bitmask;

		if(outputvalue != inputvalue)
		{
			++nError;
		}
	}

	return nError;
}

void testvdifcornerturners(int outputBytes, int nTest)
{
	const char devRandom[] = "/dev/urandom";
	const int bits[] = {1, 2, 4, 8, 0};
	const int maxThreads = 16;
	int bi;
	int t;
	unsigned char *threadData[maxThreads];
	unsigned char *outputBuffer;

	for(t = 0; t < maxThreads; ++t)
	{
		FILE *in;
		int nRead;
		threadData[t] = (unsigned char *)malloc(outputBytes);
		
		/* fill with random values */
		in = fopen(devRandom, "r");
		if(!in)
		{
			fprintf(stderr, "Error: cannot open %s\n", devRandom);

			return;
		}

		nRead = fread(threadData[t], 1, outputBytes, in);
		if(nRead <= 0)
		{
			fprintf(stderr, "Error: cannot read from %s\n", devRandom);
			fclose(in);

			return;
		}

		fclose(in);
	}
	outputBuffer = (unsigned char *)malloc(outputBytes);

	for(bi = 0; bits[bi]; ++bi)
	{
		int b = bits[bi];
		int nt;
		for(nt = -maxThreads; nt <= maxThreads; ++nt)
		{
			int i;
			void (*cornerTurner)(unsigned char *, const unsigned char * const *, int);
			clock_t t0, t1;
			int nError;
			
			cornerTurner = getCornerTurner(nt, b);

			if(!cornerTurner)
			{
				continue;
			}
			printf("%d bits  %d threads...  ", b, nt);
			fflush(stdout);

			t0 = clock();
			for(i = 0; i < nTest; ++i)
			{
				cornerTurner(outputBuffer, (const unsigned char * const*)threadData, outputBytes);
			}
			t1 = clock();
			if(t1 > t0)
			{
				printf("Took %d microseconds -> %0.0f Mbps", (int)(t1-t0), (8.0*nTest*outputBytes/(t1-t0)));
			}
			else
			{
				printf("Weird; took 0 time.");
			}

			nError = testCornerTurn(outputBuffer, (const unsigned char * const*)threadData, outputBytes, abs(nt), b);
			printf("   %d samples of %d were wrong.\n", nError, 8*outputBytes/b);
		}
	}

	free(outputBuffer);
	for(t = 0; t < maxThreads; ++t)
	{
		free(threadData[t]);
	}
}
