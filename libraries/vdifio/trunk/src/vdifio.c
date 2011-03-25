/***************************************************************************
 *   Copyright (C) 2009-2010 by Adam Deller / Walter Brisken               *
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
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include "vdifio.h"

int ymd2doy(int yr, int mo, int day)
{
        int monstart1[] = {0,31,59,90,120,151,181,212,243,273,304,334};
        int monstart2[] = {0,31,60,91,121,152,182,213,244,274,305,335};
        int L2;

        L2 = yr/4-(yr+7)/4-yr/100+(yr+99)/100+yr/400-(yr+399)/400;
        if(L2 == -1)
        {
                return day + monstart2[mo-1];
        }
        else
        {
                return day + monstart1[mo-1];
        }
}

int ymd2mjd(int yr, int mo, int day)
{
        int doy;
        int yr1 = yr - 1;

        doy = ymd2doy(yr, mo, day);

        return doy-678576+365*yr1+yr1/4-yr1/100+yr1/400;
}

int parse_vdif_header(char * rawheader, vdif_header * parsedheader)
{
	printf("Not yet implemented");
	return -1;
}

int getVDIFThreadID(char * rawheader)
{
	unsigned int headerword = ((unsigned int*)rawheader)[3];
	return (int)((headerword >> 16) & 0x3FF);
}

void setVDIFThreadID(char * rawheader, int threadid)
{
	unsigned int headerword = ((unsigned int*)rawheader)[3];
	headerword &= 0xFC00FFFF;
	headerword |= (threadid & 0x3FF) << 16;
        ((unsigned int*)rawheader)[3] = headerword;
}

int getVDIFFrameBytes(char * rawheader)
{
        unsigned int headerword = ((unsigned int*)rawheader)[2];
	return (int)(headerword & 0xFFFFFF)*8;
}

void setVDIFFrameBytes(char * rawheader, int bytes)
{
	unsigned int headerword = ((unsigned int*)rawheader)[2];
	headerword &= 0xFF000000;
	headerword |= (bytes & 0xFFFFFF) / 8;
	((unsigned int*)rawheader)[2] = headerword;
}

int getVDIFStationID(char * rawheader)
{
	unsigned int headerword = ((unsigned int*)rawheader)[3];
	return (int)(headerword & 0xFFFF);
}

int getVDIFBitsPerSample(char * rawheader)
{
	unsigned int headerword = ((unsigned int*)rawheader)[3];
	return (int)(((headerword >> 26) & 0x1F) + 1);
}

int getVDIFNumChannels(char * rawheader)
{
	int numchans, i;
	unsigned int headerword = ((unsigned int*)rawheader)[2];
	unsigned int logchans = ((headerword >> 24) & 0x1F);

	numchans = 1;
	for(i=0;i<logchans;i++)
	{
		numchans *= 2;
	}
	return numchans;
}

void setVDIFNumChannels(char * rawheader, int numchannels)
{
	unsigned int logchans = 0;
	while(numchannels > 1)
	{
		numchannels /= 2;
		logchans++;
	}
	unsigned int headerword = ((unsigned int*)rawheader)[2];
	headerword &= 0x83FFFFFF;
	headerword |= logchans << 24;
	((unsigned int*)rawheader)[2] = headerword;
}

int getVDIFFrameMJD(char * rawheader)
{
        unsigned int headerword = ((unsigned int*)rawheader)[1];
        int epoch = (int)((headerword >> 24) & 0x3F);
	int mjd = ymd2mjd(2000 + epoch/2, (epoch%2)*6+1, 1);
	headerword = ((int*)rawheader)[0];
	int seconds = (int)(headerword & 0x3FFFFFFF);
	return mjd + seconds/86400;
}

int getVDIFFrameSecond(char * rawheader)
{
        unsigned int headerword = ((unsigned int*)rawheader)[0];
        return (int)((headerword & 0x3FFFFFFF)%86400);
}

int getVDIFFrameNumber(char * rawheader)
{
        unsigned int headerword = ((unsigned int*)rawheader)[1];
        return (int)(headerword & 0xFFFFFF);
}

int getVDIFFrameInvalid(char * rawheader)
{
	unsigned int headerword = ((unsigned int*)rawheader)[0];
	return (int)((headerword >> 31) & 0x01);
}

void setVDIFFrameMJD(char * rawheader, int framemjd)
{
	unsigned int headerword = ((unsigned int*)rawheader)[1];
	int epoch = (int)((headerword >> 24) & 0x3F);
	int emjd = ymd2mjd(2000 + epoch/2, (epoch%2)*6+1, 1);
	headerword = ((int*)rawheader)[0];
	int seconds = (int)(headerword & 0x3FFFFFFF);
	int mjd = emjd + seconds/86400;
	if(emjd == framemjd) return; //its already right
	headerword += (framemjd-mjd)*86400;
	*((unsigned int*)rawheader) = headerword;
}

void setVDIFFrameSecond(char * rawheader, int framesecond)
{
	unsigned int headerword = ((unsigned int*)rawheader)[0];
	headerword -= (headerword & 0x3FFFFFFF)%86400;
	headerword += framesecond;
	*((unsigned int*)rawheader) = headerword;
}

void setVDIFFrameNumber(char * rawheader, int framenumber)
{
	unsigned int headerword = ((unsigned int*)rawheader)[1];
	headerword -= headerword & 0xFFFFFF;
	headerword += framenumber;
	*(&(((unsigned int*)rawheader)[1])) = headerword;
}

void setVDIFFrameInvalid(char * rawheader, unsigned int invalid)
{
	unsigned int headerword = ((unsigned int*)rawheader)[0];
	headerword &= 0x7FFFFFFF; //clear the invalid bit
	headerword |= (invalid << 31);
	*((unsigned int*)rawheader) = headerword;
}

