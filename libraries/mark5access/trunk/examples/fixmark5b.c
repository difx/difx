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

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../mark5access/mark5bfix.h"


int main(int argc, char **argv)
{
	FILE *in, *out;
	int srcBufferSize;
	int destBufferSize;
	int framesPerSecond;
	unsigned char *src, *dest;
	int leftover = 0;
	struct mark5b_fix_statistics stats;
	int startFrame = -1;

	resetmark5bfixstatistics(&stats);

	if(argc < 5)
	{
		printf("Usage: fixmark5b <m5b file> <frames per second> <read size> <output file> [<start frame>]\n");

		return 0;
	}

	in = fopen(argv[1], "r");
	if(!in)
	{
		printf("Error: cannot open %s\n", argv[1]);
	}

	out = fopen(argv[4], "w");
	if(!out)
	{
		printf("Error: cannot open %s\n", argv[4]);
	}

	if(argc > 5)
	{
		startFrame = atoi(argv[5]);
	}

	framesPerSecond = atoi(argv[2]);

	srcBufferSize = atoi(argv[3]);
	destBufferSize = (srcBufferSize * 8) / 10;
	
	src = (unsigned char *)malloc(srcBufferSize);
	dest = (unsigned char *)malloc(destBufferSize);
	
	for(;;)
	{
		int v;

		v = fread(src + leftover, 1, srcBufferSize - leftover, in);
		if(v + leftover <= 0)
		{
			break;
		}

		v = mark5bfix(dest, destBufferSize, src, leftover + v, framesPerSecond, startFrame, &stats);
		if(v < 10016)
		{
			break;
		}

		printf("\n");
		printmark5bfixstatistics(&stats);

		v = fwrite(dest, 1, stats.destUsed, out);

		leftover = stats.srcSize - stats.srcUsed;

		if(leftover > 0)
		{
			memmove(src, src+stats.srcSize-leftover, leftover);
		}

		startFrame = stats.startFrameNumber + stats.destUsed/10016;
	}

	free(src);
	free(dest);

	fclose(out);
	fclose(in);

	return 0;
}
