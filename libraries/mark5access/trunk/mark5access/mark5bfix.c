/***************************************************************************
 *   Copyright (C) 2013-2014 by Walter Brisken                             *
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
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "mark5bfix.h"

/* The main function in this file, mark5bfix, takes a block of Mark5B data and rewrites it with
 * any spurious inserted data removed and missing packets replaced by properly time-stamped
 * packets containing fill pattern
 */

#define MARK5B_FRAME_SIZE	10016

#ifdef WORDS_BIGENDIAN
#define MARK5_FILL_WORD		0x44332211UL
#define MARK5B_SYNC_WORD	0xEDDEADABUL
#else
#define MARK5_FILL_WORD		0x11223344UL
#define MARK5B_SYNC_WORD	0xABADDEEDUL
#endif

/* given a pointer to a Mark5B frame, calculate the time word for a frame dt seconds away in time */
static inline void gendifferentialtime(unsigned char timebytes[4], const unsigned char *cur, int dt)
{
	int newday;
	int newsec;
	
	newday = (cur[11] >> 4)*100 + (cur[11] & 0x0F)*10 + (cur[10] >> 4);
	newsec = (cur[10] & 0x0F)*10000 + (cur[9] >> 4)*1000 + (cur[9] & 0x0F)*100 + (cur[8] >> 4)*10 + (cur[8] & 0x0F);

	newsec += dt;
	if(newsec < 0)
	{
		newsec += 86400;
		--newday;
		if(newday < 0)
		{
			newday += 1000;
		}
	}
	else if(newsec >= 86400)
	{
		newsec -= 86400;
		++newday;
		if(newday >= 1000)
		{
			newday -= 1000;
		}
	}

	timebytes[0] = newsec % 10;
	newsec /= 10;
	timebytes[0] += (newsec % 10) << 4;
	newsec /= 10;
	timebytes[1] = newsec % 10;
	newsec /= 10;
	timebytes[1] += (newsec % 10) << 4;
	newsec /= 10;
	timebytes[2] = newsec % 10;
	timebytes[2] += (newday % 10) << 4;
	newday /= 10;
	timebytes[3] = newday % 10;
	newday /= 10;
	timebytes[3] += (newday %10) << 4;
}

int mark5bfix(unsigned char *dest, int destSize, const unsigned char *src, int srcSize, int framesPerSecond, int startOutputFrameNumber, struct mark5b_fix_statistics *stats)
{
	int N = srcSize - MARK5B_FRAME_SIZE;
	int nSkip = 0;		/* skipped bytes looking for frame sync */
	int nFill = 0;		/* detected or inferred fill bytes */
	int nInvalidFrame = 0;	/* number of packets with invalid (TVG) bit set */
	int nValidFrame = 0;	/* number of good frames written */
	int nLostPacket = 0;	/* logically a subset of nMissed */
	int frameInSecond;
	int second = 0;		/* second number, since beginning of call */
	int lastFrameInSecond = 1 << 24;	/* set to flag setting of startOutputFrame */
	int startOutputFrame = -1;
	int nDestFrames;	
	int lastGoodFrameNumber = 0;
	int i;

	nDestFrames = destSize / MARK5B_FRAME_SIZE;

	if(startOutputFrameNumber >= 0)
	{
		startOutputFrame = startOutputFrameNumber % framesPerSecond;
		lastGoodFrameNumber = startOutputFrame - 1;	/* make sure to fill in invalid status */
		lastFrameInSecond = startOutputFrame - 1;
	}

	for(i = 0; i <= N; )
	{
		const unsigned char *cur = src + i;
		int missed;
		int frameNumber;

		/* Look for sync word */
		if(*((uint32_t *)cur) != MARK5B_SYNC_WORD)
		{
			i += 4;
			nSkip += 4;

			continue;
		}

		/* Look for unexpected sync word */
		if(*((uint32_t *)(cur + 5008)) == MARK5B_SYNC_WORD)
		{
			i += 5008;
			++nLostPacket;

			continue;
		}

		/* Verify next sync word is coming */
		if(i <= N-MARK5B_FRAME_SIZE && *((uint32_t *)(cur + MARK5B_FRAME_SIZE)) != MARK5B_SYNC_WORD)
		{
			i += 5008;
			++nLostPacket;

			continue;
		}
		
		if(*((uint32_t *)(cur + 16)) == MARK5_FILL_WORD)
		{
			/* fill pattern at beginning of data */
			i += MARK5B_FRAME_SIZE;
			nFill += MARK5B_FRAME_SIZE;

			continue;
		}
		if(cur[5] & 0x80)	/* invalid bit set */
		{
			++nInvalidFrame;
			i += MARK5B_FRAME_SIZE;

			continue;
		}

		frameInSecond = cur[4] + (cur[5] * 256);
		if(frameInSecond < lastFrameInSecond)
		{
			if(lastFrameInSecond == (1 << 24))
			{
				/* first frame found */
				startOutputFrame = frameInSecond;

				if(framesPerSecond == 25600 && startOutputFrame % 2 == 1)
				{
					++startOutputFrame;
					lastGoodFrameNumber = startOutputFrame - 2;	/* make sure to fill in invalid status */
				}
				else
				{
					lastGoodFrameNumber = startOutputFrame - 1;	/* make sure to fill in invalid status */
				}
			}
			else if(frameInSecond < 30)
			{
				/* must have been a seconds increment recently */
				++second;

				lastFrameInSecond -= framesPerSecond;
			}
		}

		frameNumber = frameInSecond + second*framesPerSecond;

		if(frameNumber < startOutputFrame)
		{
			i += MARK5B_FRAME_SIZE;

			continue;
		}
	
		if(frameNumber - startOutputFrame >= nDestFrames)
		{
			break;
		}

		missed = (frameInSecond - lastFrameInSecond - 1);

		lastFrameInSecond = frameInSecond;

		if(missed > 0)
		{
			int m;
			int dt = 0;
			unsigned char timebytes[4];

			/* 1 or more frames was not found to be good, so mark it/them with an invalid bit */
			for(m = frameNumber - missed; m < frameNumber; ++m)
			{
				unsigned char *bytes = dest + (m-startOutputFrame)*MARK5B_FRAME_SIZE;
				uint32_t *words = (uint32_t *)bytes;
				int d, f;
				
				/* copy frame header data to make the frame legal */

				/* first the sync word */
				words[0] = MARK5B_SYNC_WORD;

				/* then frame in second */
				f = (m + framesPerSecond) % framesPerSecond;
				bytes[4] = f & 0xFF;
				bytes[5] = f >> 8;

				/* then time code */
				d = (m / framesPerSecond) - (frameNumber / framesPerSecond);

				if(d != 0)
				{
					/* This should be the rare case where a second jump happened at the time of the missing data */

					if(dt != d)
					{
						gendifferentialtime(timebytes, cur, d);
						dt = d;
					}

					memcpy(bytes + 8, timebytes, 4);
				}
				else
				{
					/* The common, simple case where missing data happens within a second */
					memcpy(bytes + 8, cur + 8, 8);
				}

				/* to mark a frame invalid, set fill pattern word at first and last word of the payload */
				words[4] = MARK5_FILL_WORD;
				words[2503] = MARK5_FILL_WORD;

				++nLostPacket;
			}
		}

		memcpy(dest + (frameNumber - startOutputFrame)*MARK5B_FRAME_SIZE, cur, MARK5B_FRAME_SIZE);
		++nValidFrame;

		lastGoodFrameNumber = frameNumber;

		i += MARK5B_FRAME_SIZE;
	}

	if(lastGoodFrameNumber < startOutputFrame)
	{
		/* no good data found */

		return -1;
	}
	else if(i <= N && lastGoodFrameNumber-startOutputFrame < nDestFrames - 1)
	{
		/* here we didn't run out of src data but ended with missing data */

		int m;
		int dt = 0;
		unsigned char timebytes[4];

		/* 1 or more frames was not found to be good, so mark it/them with an invalid bit */
		for(m = lastGoodFrameNumber + 1; m < nDestFrames; ++m)
		{
			const unsigned char *cur = dest + (lastGoodFrameNumber - startOutputFrame)*MARK5B_FRAME_SIZE;
			unsigned char *bytes = dest + (m - startOutputFrame)*MARK5B_FRAME_SIZE;
			uint32_t *words = (uint32_t *)bytes;
			int d, f;
			
			/* copy frame header data to make the frame legal */

			/* first the sync word */
			words[0] = MARK5B_SYNC_WORD;

			/* then frame in second */
			f = (m + framesPerSecond) % framesPerSecond;
			bytes[4] = f & 0xFF;
			bytes[5] = f >> 8;

			/* then time code */
			d = (m / framesPerSecond) - (lastGoodFrameNumber / framesPerSecond);

			if(d != 0)
			{
				/* This should be the rare case where a second jump happened at the time of the missing data */

				if(dt != d)
				{
					gendifferentialtime(timebytes, cur, d);
					dt = d;
				}

				memcpy(bytes + 8, timebytes, 4);
			}
			else
			{
				/* The common, simple case where missing data happens within a second */
				memcpy(bytes + 8, cur + 8, 8);
			}

			/* to mark a frame invalid, set fill pattern word at first and last word of the payload */
			words[4] = MARK5_FILL_WORD;
			words[2503] = MARK5_FILL_WORD;

			++nLostPacket;
		}

		lastGoodFrameNumber = nDestFrames - 1 + startOutputFrame;
	}

	if(stats)
	{
		stats->nValidFrame += nValidFrame;
		stats->nInvalidFrame += nInvalidFrame;
		stats->nSkippedByte += nSkip;
		stats->nFillByte += nFill;
		stats->nLostPacket += nLostPacket;
		stats->dataProcessed += i;

		stats->srcSize = srcSize;
		stats->srcUsed = i;
		stats->destSize = destSize;
		stats->destUsed = (lastGoodFrameNumber - startOutputFrame + 1)*MARK5B_FRAME_SIZE;
		stats->startFrameNumber = startOutputFrame;

		stats->startFrameSeconds = (dest[10] & 0x0F)*10000 + (dest[9] >> 4)*1000 + (dest[9] & 0x0F)*100 + (dest[8] >> 4)*10 + (dest[8] & 0x0F);
		stats->startFrameNanoseconds = ((long long)startOutputFrame*1000000000LL)/framesPerSecond;

		++stats->nCall;
	}

	return i;
}

void printmark5bfixstatistics(const struct mark5b_fix_statistics *stats)
{
	fprintmark5bfixstatistics(stdout, stats);
}

void fprintmark5bfixstatistics(FILE *out, const struct mark5b_fix_statistics *stats)
{
	if(stats)
	{
		fprintf(out, "Mark5B fixer statistics:\n");
		fprintf(out, "  Number of calls to fixmark5b       = %d\n", stats->nCall);
		fprintf(out, "  Number of valid input frames       = %Ld\n", stats->nValidFrame);
		fprintf(out, "  Number of invalid input frames     = %Ld\n", stats->nInvalidFrame);
		fprintf(out, "  Number of skipped interloper bytes = %Ld\n", stats->nSkippedByte);
		fprintf(out, "  Number of fill pattern bytes       = %Ld\n", stats->nFillByte);
		fprintf(out, "  Number of lost packets             = %Ld\n", stats->nLostPacket);
		fprintf(out, "  Number of bytes processed          = %Ld\n", stats->dataProcessed);
		fprintf(out, "Properties of output data from recent call:\n");
		fprintf(out, "  Start output frame number          = %d\n", stats->startFrameNumber);
		fprintf(out, "  Start output frame seconds         = %d\n", stats->startFrameSeconds);
		fprintf(out, "  Start output frame nanoseconds     = %d\n", stats->startFrameNanoseconds);
		fprintf(out, "  %d/%d src bytes consumed\n", stats->srcUsed, stats->srcSize);
		fprintf(out, "  %d/%d dest bytes generated\n", stats->destUsed, stats->destSize);
	}
	else
	{
		fprintf(out, "Weird: printmark5bfixstatistics called with null pointer\n");
	}
}

void resetmark5bfixstatistics(struct mark5b_fix_statistics *stats)
{
	if(stats)
	{
		memset(stats, 0, sizeof(struct mark5b_fix_statistics));
	}
}
