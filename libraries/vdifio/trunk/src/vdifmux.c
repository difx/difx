/***************************************************************************
 *   Copyright (C) 2013-2018 Walter Brisken                                *
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

static void set_nOutputChan(struct vdif_mux *vm)
{
	int nt;

	nt = vm->nThread / vm->fanoutFactor;
	for(vm->nOutputChan = 1; vm->nOutputChan < nt; vm->nOutputChan *= 2) ;
	vm->nOutputChan *= vm->inputChannelsPerThread;
}



/* Params are:
 *
 * inputFrameSize:
 *	length of single-thread VDIF data packet
 * inputFramesPerSecond:
 *	Number of frames per second per thread of input to expect
 * bitsPerSample:
 *	number of bits per sample (for complex, number of bits per component)
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
int configurevdifmux(struct vdif_mux *vm, int inputFrameSize, int inputFramesPerSecond, int bitsPerSample, int nThread, const int *threadIds, int nSort, int nGap, int flags)
{
	int i;
	int nBit;	/* bits to corner turn together */

	if(nThread > sizeof(vm->goodMask)*8)
	{
		fprintf(stderr, "Error: configurevdifmux: cannot run vdifmux on more than %d threads; %d requested.\n", (int)sizeof(vm->goodMask)*8, nThread);

		return -3;
	}

	if(flags & VDIF_MUX_FLAG_COMPLEX)
	{
		vm->complexFactor = 2;
	}
	else
	{
		vm->complexFactor = 1;
	}

	nBit = bitsPerSample * vm->complexFactor;
	vm->cornerTurner = getCornerTurner(nThread, nBit);
	if(vm->cornerTurner == 0)
	{
		fprintf(stderr, "No corner turner implemented for %d threads and %d bits\n", nThread, nBit);
		
		return -1;
	}

	vm->inputFrameSize = inputFrameSize;
	vm->inputFramesPerSecond = inputFramesPerSecond;
	vm->inputChannelsPerThread = 1;	/* Use setvdifmuxinputchannels() if this is not desired */
	vm->frameGranularity = inputFramesPerSecond/gcd(inputFramesPerSecond, 1000000000);
	vm->bitsPerSample = bitsPerSample;
	vm->nSort = nSort;
	vm->nGap = nGap;
	nThread = abs(nThread);	/* in case a special corner turner was requested */
	vm->nThread = nThread;
	vm->flags = flags;

	if(vm->nGap < vm->nSort)
	{
		vm->nGap = vm->nSort;
	}

	/* by default, and in most cases, don't merge multiple threads into a single channel.  Can be overridden with a call to setvdifmuxfanoutfactor */
	vm->fanoutFactor = 1;


	set_nOutputChan(vm);

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

int setvdifmuxinputchannels(struct vdif_mux *vm, int inputChannelsPerThread)
{
	int nBit;	/* total bits to be corner turned together */

	if(!vm)
	{
		fprintf(stderr, "Error: setvdifmuxinputchannels called with null vdif_mux structure\n");

		return -1;
	}

	if(inputChannelsPerThread <= 0)
	{
		fprintf(stderr, "Error: setvdifmuxinputchannels called with %d channels per thread\n", inputChannelsPerThread);

		return -2;
	}

	if(inputChannelsPerThread > 1 && vm->fanoutFactor > 1)
	{
		fprintf(stderr, "Error: setvdifmuxinputchannels: cannot have both inputChannelsPerThread and FanoutFactor > 1\n");
		
		return -3;
	}

	vm->inputChannelsPerThread = inputChannelsPerThread;

	set_nOutputChan(vm);

	nBit = vm->bitsPerSample*vm->inputChannelsPerThread*vm->complexFactor;
	vm->cornerTurner = getCornerTurner(vm->nThread, nBit);
	if(vm->cornerTurner == 0)
	{
		fprintf(stderr, "No corner turner implemented for %d threads and %d bits\n", vm->nThread, nBit);
		
		return -1;
	}

	return 0;
}

/* Put in for DBBC3: allows data from multiple VDIF threads to be multiplexed into a single channel */
int setvdifmuxfanoutfactor(struct vdif_mux *vm, int fanoutFactor)
{
	if(!vm)
	{
		fprintf(stderr, "Error: setvdifmuxfanoutfactor called with null vdif_mux structure\n");

		return -1;
	}

	if(fanoutFactor < 1)
	{
		fprintf(stderr, "Error: setvdifmuxfanoutfactor given out of range input value: %d.\n", fanoutFactor);

		return -2;
	}

	if(vm->inputChannelsPerThread > 1 && fanoutFactor > 1)
	{
		fprintf(stderr, "Error: setvdifmuxfanoutfactor: cannot have both inputChannelsPerThread=%d and FanoutFactor=%d > 1\n", vm->inputChannelsPerThread, fanoutFactor);
		
		return -3;
	}

	if(vm->nThread % fanoutFactor != 0)
	{
		fprintf(stderr, "Error: setvidfmuxfanoutfactor: fanoutFactor=%d does not divide nThread=%d\n", fanoutFactor, vm->nThread);

		return -4;
	}

	vm->fanoutFactor = fanoutFactor;

	set_nOutputChan(vm);

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
		printf("  inputChannelsPerThread = %d\n", vm->inputChannelsPerThread);
		printf("  bitsPerSample = %d\n", vm->bitsPerSample);
		printf("  complexFactor = %d\n", vm->complexFactor);
		printf("  nSort = %d\n", vm->nSort);
		printf("  nGap = %d\n", vm->nGap);
		printf("  nThread = %d\n", vm->nThread);
		printf("  nOutputChan = %d\n", vm->nOutputChan);
		printf("  fanoutFactor = %d\n", vm->fanoutFactor);
		printf("  goodMask = 0x%" PRIx64 "\n", vm->goodMask);
		printf("  flags = 0x%02x\n", vm->flags);
		printf("  thread to channel map:\n");
		if(vm->inputChannelsPerThread == 1)
		{
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
			for(i = 0; i <= VDIF_MAX_THREAD_ID; ++i)
			{
				if(vm->chanIndex[i] != MAGIC_BAD_THREAD)
				{
					printf("    %d -> %d-%d\n", i, 
						vm->chanIndex[i]*vm->inputChannelsPerThread, 
						(vm->chanIndex[i]+1)*vm->inputChannelsPerThread-1);
				}
			}
		}
	}
	else
	{
		printf("vdif_mux is NULL\n");
	}
}

/* goes through array of VDIF data of size srcSize and returns the lowest frame number (seconds*fps + frame) encountered in the first vm->nSort valid frames */
static int64_t getLowestFrameNumber(const unsigned char *src, int srcSize, const struct vdif_mux *vm)
{
	int i, N;
	int64_t lowest = -1;
	int nFrame = 0;

	N = srcSize - vm->inputFrameSize;

	for(i = 0; i <= N;)
	{
		const unsigned char *cur;
		const vdif_header *vh;
		int64_t frameNumber;

		cur = src + i;
		vh = (const vdif_header *)cur;
		if(*((uint32_t *)(cur+vm->inputFrameSize-4)) == FILL_PATTERN)
		{
			/* Fill pattern at end of frame or invalid bit is set */
			i += vm->inputFrameSize;

			continue;
		}
		if(*((uint32_t *)cur) == FILL_PATTERN)
		{
			/* Fill pattern at beginning of frame */
			i += 8;

			continue;
		}
		if(getVDIFFrameBytes(vh) != vm->inputFrameSize ||
		   getVDIFNumChannels(vh) != vm->inputChannelsPerThread ||
		   getVDIFBitsPerSample(vh) != vm->bitsPerSample)
		{
			/* here assume mismatch of key parameters is due to not being synced to the stream */
			i += 4;

			continue;
		}
		if( (vm->flags & VDIF_MUX_FLAG_ENABLEVALIDITY) && (getVDIFFrameInvalid(vh) > 0) )
		{
			/* invalid bit is set and respected */
			i += vm->inputFrameSize;

			continue;
		}

		frameNumber = (int64_t)(getVDIFFrameEpochSecOffset(vh)) * vm->inputFramesPerSecond + getVDIFFrameNumber(vh);

		if(lowest < 0 || frameNumber < lowest)
		{
			lowest = frameNumber;
		}
		++nFrame;
		i += vm->inputFrameSize;
		if(nFrame >= vm->nSort)
		{
			break;
		}
	}

	return lowest;
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
	int nDiscard = 0;			/* counts number of VDIF frames dropped because of ordering */
	int64_t startFrameNumber;		/* = seconds*inputFramesPerSecond + frameNumber */

	int i;					/* index into src */
	int f;
	int N;					/* max value to allow i to be */
	int highestDestIndex = 0;
	int maxDestIndex;
	int maxSrcIndex;
	int bytesProcessed = -1;		/* the program return value */
	int nEnd = 0;				/* number of frames processed after the end of the buffer first reached */
	int nGoodOutput = 0;
	int nBadOutput = 0;
	int nPartialOutput = 0;
	int nWrongThread = 0;
	int seconds, frameNum;
	union
	{
		vdif_header generic;
		vdif_edv4_header edv4;
	} outputHeader;
	int epoch = -1;
	int highestSortedDestIndex = -1;
	int vhUnset = 1;
	int nSort;	/* try using the supplied one, but if buffers are too small, shorten nSort accordingly */
	int framesIn = 0;

	nSort = vm->nSort;

	N = srcSize - vm->inputFrameSize;


	/* Adjust sort size if needed based on buffer sizes */
	if(3*srcSize <= 4*nSort*vm->inputFrameSize)
	{
		nSort = (3*srcSize)/(4*vm->inputFrameSize);
	}

	if(vm->flags & VDIF_MUX_FLAG_GOTOEND)
	{
		maxSrcIndex = srcSize - vm->inputFrameSize;
	}
	else
	{
		maxSrcIndex = srcSize - nSort*vm->inputFrameSize;
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

	if(startFrameNumber < 0)
	{
		startFrameNumber = getLowestFrameNumber(src, srcSize, vm);
		if(startFrameNumber < 0)
		{
			return -3;
		}
	}

	/* clear mask of presence */
	for(i = 0; i <= maxDestIndex; ++i)
	{
		uint64_t *p = (uint64_t *)(dest + vm->outputFrameSize*i);
		p[3] = 0;
	}

	/* Stage 1: find good data and put in output array. */
	for(i = 0; i <= N;)
	{
		const unsigned char *cur;
		const vdif_header *vh;
		int64_t frameNumber;
		int destIndex;		/* frame index into destination array */
		int chanId;

		cur = src + i;
		vh = (const vdif_header *)cur;
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
		   getVDIFNumChannels(vh) != vm->inputChannelsPerThread ||
		   getVDIFBitsPerSample(vh) != vm->bitsPerSample)
		{
			/* here assume mismatch of key parameters is due to not being synced to the stream */
			i += 4;
			nSkip += 4;

			continue;
		}
		if( (vm->flags & VDIF_MUX_FLAG_ENABLEVALIDITY) && (getVDIFFrameInvalid(vh) > 0) )
		{
			/* invalid bit is set and respected */
			i += vm->inputFrameSize;
			++nInvalidFrame;

			continue;
		}


		/* If we are here, it looks like we have a valid VDIF frame to work with */
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
		
		if(vhUnset)
		{
			memcpy(&outputHeader, vh, 16);
			if(vm->flags & VDIF_MUX_FLAG_PROPAGATEVALIDITY)
			{
				/* FIXME: handle heirarchical multiplexing */

				outputHeader.edv4.dummy = 0;
				outputHeader.edv4.masklength = vm->nThread/vm->fanoutFactor;
				outputHeader.edv4.eversion = 4;
				outputHeader.edv4.syncword = 0xACABFEED;
				outputHeader.edv4.validitymask = 0;
			}
			else
			{
				memset(((char *)&outputHeader) + 16, 0, 16);
			}

			/* use this first good frame to generate the prototype VDIF header for the output */
			setVDIFNumChannels(&outputHeader.generic, vm->nOutputChan);
			setVDIFThreadID(&outputHeader.generic, 0);
			setVDIFFrameBytes(&outputHeader.generic, vm->outputFrameSize);
			setVDIFFrameInvalid(&outputHeader.generic, 0);
			epoch = getVDIFEpoch(&outputHeader.generic);

			vhUnset = 0;
		}

		++framesIn;

		destIndex = frameNumber - startFrameNumber;

		if(destIndex < 0)
		{
			/* no choice but to discard this data which is to far in the past */
			i += vm->inputFrameSize;
			++nDiscard;

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
			if(framesIn < nSort)
			{
				/* no choice but to discard this data which is too far in the future */
				i += vm->inputFrameSize;
				++nDiscard;

				continue;
			}
			else
			{
				/* start the shut-down procedure */
				if(bytesProcessed == -1)
				{
					// fprintf(stderr, "src/vdifmux.c bytesProcessed = 0, shut-down procedure, setting it to %d; frameNumber=%lld startFrameNumber=%lld destIndex=%d maxDestIndex=%d : destSize=%d outFrameSize=%d\n", i, frameNumber, startFrameNumber, destIndex, maxDestIndex, destSize, vm->outputFrameSize);
					bytesProcessed = i;
				}
				i += vm->inputFrameSize;
				++nEnd;
				if(nEnd >= nSort)
				{
					break;
				}
			}
		}
		else /* here we have a usable packet */
		{
			uint64_t *p = (uint64_t *)(dest + vm->outputFrameSize*destIndex);
			
			if(destIndex > highestDestIndex + vm->nGap && vm->nGap > 0)
			{
				if(nValidFrame > nSort) // || startOutputFrameNumber >= 0)
				{


	// FIXME: revisit this logic
					/* if we are out of the probationary nSort period, start the shut-down procedure */
					if(bytesProcessed == -1)
					{
						bytesProcessed = i;
					}
					++nEnd;
					if(nEnd >= nSort)
					{
						break;
					}
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
			}
			i += vm->inputFrameSize;
		}
	}

	if(bytesProcessed == -1)
	{
		bytesProcessed = i;
	}

	/* Here the source dried up, but we want to be able to reconstruct a complete stream later, so back up, a bit to look for most recent incomplete frame consistent with nSort */
	if(highestSortedDestIndex >= 0)
	{
		for(f = highestDestIndex; f > highestSortedDestIndex; --f)
		{
			const uint64_t *p = (const uint64_t *)(dest + vm->outputFrameSize*f);
			uint64_t mask = p[3];

			if(vm->flags & VDIF_MUX_FLAG_PROPAGATEVALIDITY)
			{
				if(mask == 0)
				{
					highestDestIndex = f-1;
				}
			}
			else
			{
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
	}
	else
	{
		int minDestIndex = highestDestIndex - nSort/vm->nThread;

		if(minDestIndex >= 0) for(f = highestDestIndex; f >= minDestIndex; --f)
		{
			const uint64_t *p = (const uint64_t *)(dest + vm->outputFrameSize*f);
			uint64_t mask = p[3];

			if(vm->flags & VDIF_MUX_FLAG_PROPAGATEVALIDITY)
			{
				if(mask == 0)
				{
					highestDestIndex = f-1;
				}
			}
			else
			{
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
	}

	/* If source did not dry up and dest got within nGap of filling, set highestDestIndex to produce (invalid) data through the end of the dest buffer for continuity */
	if(i < N && highestDestIndex < maxDestIndex && (highestDestIndex > maxDestIndex - vm->nGap || vm->nGap == 0))
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

		if(vm->flags & VDIF_MUX_FLAG_PROPAGATEVALIDITY)
		{
			if(mask != 0)
			{
				const unsigned char **threadBuffers = (const unsigned char **)(frame + VDIF_HEADER_BYTES);
				vdif_edv4_header *edv4 = (vdif_edv4_header *)frame;
				if(vm->fanoutFactor > 1)
				{
					int k;

					edv4->validitymask = 0;
					for(i = k = 0; i < vm->nThread; i += vm->fanoutFactor, ++k)
					{
						int m;
						int j;
						
						m = 1;
						for(j = 0; j < vm->fanoutFactor; ++j)
						{
							m &= (mask >> (i+j));
						}
						edv4->validitymask |= (m << k);
					}
				}
				else
				{
					edv4->validitymask = mask;
				}

				if(mask != vm->goodMask)
				{
					int64_t i;
					/* point to random data rather than nowhere for invalid frames */

					for(i = 0; i < vm->nThread; ++i)
					{
						if( (mask & (1LL << i)) == 0)
						{
							threadBuffers[i] = src;
						}
					}

				}

				/* Note: The following function only works because all of the corner turners make a copy of the
				 * thread pointers before beginning */
				vm->cornerTurner(frame + VDIF_HEADER_BYTES, threadBuffers, vm->outputDataSize);

				if(mask == vm->goodMask)
				{
					++nGoodOutput;
				}
				else
				{
					++nPartialOutput;
				}
			}
			else
			{
				/* Set invalid bit */
				setVDIFFrameInvalid((vdif_header *)frame, 1);

				++nBadOutput;
			}
		}
		else
		{
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
		stats->nDiscardedFrame += nDiscard;
		stats->nWrongThread += nWrongThread;
		stats->nDuplicateFrame += nDup;
		stats->nSkippedByte += nSkip;
		stats->nFillByte += nFill;
		stats->bytesProcessed += bytesProcessed;
		stats->nGoodFrame += nGoodOutput;
		stats->nPartialFrame += nPartialOutput;
		stats->nFillerFrame += nBadOutput;

		stats->srcSize = srcSize;
		stats->srcUsed = bytesProcessed;
		stats->destSize = destSize;
		stats->destUsed = (nGoodOutput + nBadOutput + nPartialOutput)*vm->outputFrameSize;
		stats->inputFrameSize = vm->inputFrameSize;
		stats->outputFrameSize = vm->outputFrameSize;
		stats->outputFrameGranularity = vm->frameGranularity;
		stats->outputFramesPerSecond = vm->inputFramesPerSecond;
		stats->nOutputFrame = nGoodOutput + nBadOutput + nPartialOutput;
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
		printf("  Total number of bytes output       = %lld\n", stats->outputFrameSize*(stats->nGoodFrame+stats->nPartialFrame+stats->nFillerFrame));
		printf("  Total number of good output frames = %lld\n", stats->nGoodFrame);
		printf("  Total number of partial out frames = %lld\n", stats->nPartialFrame);
		printf("  Total number of filler out frames  = %lld\n", stats->nFillerFrame);
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
		fprintf(stderr, "Weird: printvdifmuxstatistics called with null pointer.\n");
	}
}

void resetvdifmuxstatistics(struct vdif_mux_statistics *stats)
{
	if(stats)
	{
		memset(stats, 0, sizeof(struct vdif_mux_statistics));
	}
}

