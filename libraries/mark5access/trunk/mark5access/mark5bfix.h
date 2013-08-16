/***************************************************************************
 *   Copyright (C) 2013 by Walter Brisken                                  *
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

#ifndef __MARK5B_FIX__
#define __MARK5B_FIX__

#ifdef __cplusplus
extern "C" {
#endif

struct mark5b_fix_statistics
{
	long long nValidFrame;
	long long nInvalidFrame;
	long long nSkippedByte;
	long long nFillByte;
	long long nLostPacket;
	long long dataProcessed;

	int srcSize;
	int srcUsed;
	int destSize;
	int destUsed;
	int startFrameNumber;
	int startFrameSeconds;
	int startFrameNanoseconds;

	int nCall;
};

int mark5bfix(unsigned char *dest, int destSize, const unsigned char *src, int srcSize, int framesPerSecond, int startOutputFrameNumber, struct mark5b_fix_statistics *stats);

void printmark5bfixstatistics(const struct mark5b_fix_statistics *stats);

void resetmark5bfixstatistics(struct mark5b_fix_statistics *stats);

#ifdef __cplusplus
}
#endif

#endif
