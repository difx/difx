/***************************************************************************
 *   Copyright (C) 2019 by Walter Brisken                                  *
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
 * $Id: printVDIFheader.c 8498 2018-09-21 18:47:05Z WalterBrisken $
 * $HeadURL:  $
 * $LastChangedRevision: 8498 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2018-09-21 12:47:05 -0600 (Fri, 21 Sep 2018) $
 *
 *==========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "vdifio.h"
#include "config.h"

const char program[] = "splitVDIFbygap";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20191215";

static void printVersion()
{
	fprintf(stderr, "%s ver. %s  %s  %s\n", program, version, author, verdate);
}

static void usage()
{
	fprintf(stderr, "\n");
	printVersion();
	fprintf(stderr, "\n");
	fprintf(stderr, "Split a VDIF file into multiple chunks when the time difference between packets is too big.\n\n");
	fprintf(stderr, "\nUsage: %s <VDIF input file> <output file prefix> [ <frame size> [ <max gap> ] ]\n", program);
	fprintf(stderr, "\n<VDIF input file> is the name of the VDIF file to read (- for stdin)\n");
	fprintf(stderr, "\n<output file prefix> is the beginning of the output filename.  The MJD and Seconds get added to this.\n");
	fprintf(stderr, "\n<frame size> VDIF frame size, including header (5032 for VLBA; default)\n");
	fprintf(stderr, "\n<max gap> is the maximum time (integer seconds; default is 1) between two consecutive frames before creating a new file.\n");
}

int main(int argc, char **argv)
{
	const char *inputFile = 0;
	const char *outputPrefix = 0;
	int maxGap = 1;
	int frameSize = 5032;
	FILE *in = 0;
	FILE *out = 0;
	int lastSecond = -99999;
	char *buffer;
	int verbose = 1;
	int nRead = 0;
	int nDropped = 0;
	const vdif_header *vh;

	if(argc < 3)
	{
		usage();

		return EXIT_SUCCESS;
	}

	inputFile = argv[1];
	outputPrefix = argv[2];
	if(argc > 3)
	{
		maxGap = atoi(argv[3]);
	}
	if(argc > 4)
	{
		frameSize = atoi(argv[4]);
	}

	if(strcmp(inputFile, "-") == 0)
	{
		in = stdin;
	}
	else
	{
		in = fopen(inputFile, "r");
		if(!in)
		{
			fprintf(stderr, "Cannot open %s for read\n", inputFile);
			
			return EXIT_FAILURE;
		}
	}

	buffer = (char *)malloc(frameSize);
	if(!buffer)
	{
		fclose(in);

		fprintf(stderr, "Error: could not allocate %d bytes\n", frameSize);

		return EXIT_FAILURE;
	}
	vh = (vdif_header *)buffer;

	for(;;)
	{
		size_t n;
		int second;

		n = fread(buffer, frameSize, 1, in);
		if(n != 1)
		{
			break;
		}

		if(getVDIFFrameBytes(vh) != frameSize)
		{
			fprintf(stderr, "Frame size mismatch.\n");

			break;
		}

		++nRead;

		second = getVDIFFrameSecond(vh);
		if(second != lastSecond)
		{
			if(second - lastSecond + 1 > maxGap)
			{
				char filename[128];

				if(verbose > 0)
				{
					printf("Time: MJD %d  Sec %d\n", getVDIFFrameMJD(vh), second);
				}

				if(out)
				{
					fclose(out);
				}

				snprintf(filename, 128, "%s_%d_%05d.vdif", outputPrefix, getVDIFFrameMJD(vh), second);
				out = fopen(filename, "w");
				if(!out)
				{
					fprintf(stderr, "Could not open %s for write\n", filename);

					break;
				}
				if(verbose >= 0)
				{
					printf("Opened %s for write\n", filename);
				}
			}
			
			lastSecond = second;
		}
		else if(second < lastSecond)
		{
			++nDropped;

			continue;
		}

		n = fwrite(buffer, frameSize, 1, out);
		if(n != 1)
		{
			fprintf(stderr, "Write failed\n");

			break;
		}
	}

	free(buffer);
	if(strcmp(inputFile, "-") != 0)
	{
		fclose(in);
	}
	if(out)
	{
		fclose(out);
	}

	if(verbose > 0)
	{
		printf("%d frames read\n", nRead);
		printf("%d frames dropped\n", nDropped);
	}

	return EXIT_SUCCESS;
}
