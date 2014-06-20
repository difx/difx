/***************************************************************************
 *   Copyright (C) 2014 by Walter Brisken                                  *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id: stripVDIF.c 2006 2010-03-04 16:43:04Z AdamDeller $
 * $HeadURL:  $
 * $LastChangedRevision: 2006 $
 * $Author: AdamDeller $
 * $LastChangedDate: 2010-03-04 09:43:04 -0700 (Thu, 04 Mar 2010) $
 *
 *==========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "vdifio.h"

const char program[] = "printVDIFgaps";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20140620";

static void usage()
{
	fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	fprintf(stderr, "A program to look for missing VDIF packets\n");
	fprintf(stderr, "\nUsage: %s <VDIF input file> <framesize> <framespersec> <nthread>\n", program);
	fprintf(stderr, "\n<VDIF input file> is the name of the VDIF file to read\n");
	fprintf(stderr, "\n<framesize> VDIF frame size, including header (5032 for VLBA)\n");
	fprintf(stderr, "\n<framespersec> is number of frames per thread per second\n");
	fprintf(stderr, "\n<nthread> is the number of threads to expect\n\n");
}

int main(int argc, char **argv)
{
	const int MaxFrameSize = 16*MAX_VDIF_FRAME_BYTES; /* read this much at a time */
	char buffer[MaxFrameSize];
	int leftover = 0;
	int framesize;
	FILE *input;
	long long framesread = 0;
	int nthread, framespersec;
	const vdif_header *header;
	int nSkip = 0;
	int force = 1;
	int lastframe = -1;
	int threadcount = 0;
	int ngap = 0;
	int ntoofew = 0;
	int ntoomany = 0;
	int startsec = 0;

	if(argc != 5)
	{
		usage();

		return EXIT_FAILURE;
	}

	if(strcmp(argv[1], "-") == 0)
	{
		input = stdin;
	}
	else
	{
		input = fopen(argv[1], "r");
		if(!input)
		{
			fprintf(stderr, "Cannot open input file %s\n", argv[1]);

			exit(EXIT_FAILURE);
		}
	}

	framesize = atoi(argv[2]);

	framespersec = atoi(argv[3]);

	nthread = atoi(argv[4]);

	for(;;)
	{
		int index, fill, readbytes;

		index = 0;

  		readbytes = fread(buffer+leftover, 1, MaxFrameSize-leftover, input); //read the VDIF header
		if(readbytes <= 0)
		{
			break;
		}
		fill = readbytes + leftover;
		for(;;)
		{
			if(fill-index < framesize)
			{
				/* need more data */
				leftover = fill-index;
				memmove(buffer, buffer+index, leftover);
				break;
			}
			header = (const vdif_header *)(buffer + index);
			if(force == 0)	/* if not forced, look for a match */
			{
				if(getVDIFFrameBytes(header) != framesize)
				{
					++index;
					++nSkip;
					continue;
				}
				if(header->eversion == 0 && (header->extended1 != 0 || header->extended2 != 0 || header->extended3 != 0 || header->extended1 != 0))
				{
					++index;
					++nSkip;
					continue;
				}
				if(header->eversion == 1 || header->eversion == 3)
				{
					const uint32_t *ui;

					ui = (const uint32_t *)(buffer + index);
					if(ui[5] != 0xACABFEED)
					{
						continue;
					}
				}

				if(nSkip > 0)
				{
					printf("Skipped %d interloper bytes\n", nSkip);
					nSkip = 0;
				}
			}

			if(header->frame != lastframe)
			{
				if(lastframe >= 0)
				{
					if((header->frame + framespersec - lastframe) % framespersec != 1)
					{
						++ngap;
						printf("frame number gap: jump from %d to %d\n", lastframe, header->frame);
						fflush(stdout);
					}
					else if(threadcount < nthread)
					{
						++ntoofew;
						printf("too few threads: %d < %d on frame %d\n", threadcount, nthread, header->frame);
						fflush(stdout);
					}
					else if(threadcount > nthread)
					{
						++ntoomany;
						printf("too many threads: %d > %d on frame %d\n", threadcount, nthread, header->frame);
						fflush(stdout);
					}
				}

				if(lastframe < 0)
				{
					startsec = header->seconds;
				}
				if(lastframe < 0 || lastframe > header->frame)
				{
					printf("second = %d  frames read = %d  ngap = %d  ntoofew = %d  ntoomany = %d  dur = %d sec\n", header->seconds, framesread, ngap, ntoofew, ntoomany, header->seconds - startsec);
					fflush(stdout);
				}

				threadcount = 0;

				lastframe = header->frame;
			}

			++threadcount;
			
			index += framesize;
			++framesread;
		}
	}

	printf("Read %lld frames\n", framesread);
	
	if(input != stdin)
	{
  		fclose(input);
	}

	return EXIT_SUCCESS;
}
