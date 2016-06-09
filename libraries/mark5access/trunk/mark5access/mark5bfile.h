/***************************************************************************
 *   Copyright (C) 2013-2016 Walter Brisken                                *
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

#ifndef __Mark5BFile_H__
#define __Mark5BFile_H__

#ifdef __cplusplus
extern "C" {
#endif

#define MARK5B_SUMMARY_FILE_LENGTH	256

struct mark5b_file_summary
{
	char fileName[MARK5B_SUMMARY_FILE_LENGTH];
	long long fileSize;	/* [bytes] */
	int nBit;		/* defaults to 2 */
	int nChannel;		/* 0 if not known */
	int framesPerSecond;	/* set to 0 if not known; default = 25600 */
	int startDay;		/* [MJD] or [MJD%1000] from earliest valid frame */
	int startSecond;	/* from earliest valid frame */
	int startFrame;		/* from earliest valid frame */
	int endDay;		/* from last frame */
	int endSecond;		/* from last frame */
	int endFrame;		/* from last frame */
	int firstFrameOffset;	/* bytes to get to first valid frame */
};

void resetmark5bfilesummary(struct mark5b_file_summary *sum);

void printmark5bfilesummary(const struct mark5b_file_summary *sum);

void snprintmark5bfilesummary(char *str, int maxLength, const struct mark5b_file_summary *sum);

static inline int mark5bfilesummarygetstartmjd(const struct mark5b_file_summary *sum)
{
	return sum->startDay;
}

static inline int mark5bfilesummarygetstartsecond(const struct mark5b_file_summary *sum)
{
	return sum->startSecond;
}

static inline int mark5bfilesummarygetstartns(const struct mark5b_file_summary *sum)
{
	return (int)((long long)(sum->startFrame)*1000000000LL/sum->framesPerSecond);
}

static inline void mark5bfilesummarysettotalbandwidth(struct mark5b_file_summary *sum, int bandwidthMHz)
{
	sum->framesPerSecond = bandwidthMHz*25*sum->nBit;
}

static inline void mark5bfilesummarysetbits(struct mark5b_file_summary *sum, int nBit)
{
	sum->nBit = nBit;
}

/* returns Mbps */
static inline int mark5bfilesummarygetbitrate(const struct mark5b_file_summary *sum)
{
	return (sum->framesPerSecond*2)/25;
}

int determinemark5bframeoffset(const unsigned char *buffer, int bufferSize);

int summarizemark5bfile(struct mark5b_file_summary *sum, const char *fileName);

void mark5bfilesummaryfixmjd(struct mark5b_file_summary *sum, int mjd);

void mark5bfilesummaryfixmjdtoday(struct mark5b_file_summary *sum);

#ifdef __cplusplus
}
#endif

#endif

