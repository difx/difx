/***************************************************************************
 *   Copyright (C) 2014-2015 by Walter Brisken, Adam Deller                *
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
#include <stdint.h>
#include "vdifio.h"
#include "vdif_mark6.h"

const char program[] = "printVDIFheader";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.2";
const char verdate[] = "20150606";

static void usage()
{
	fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	fprintf(stderr, "A program to dump some basic info about VDIF packets to the screen\n");
	fprintf(stderr, "\nUsage: %s <VDIF input file> <framesize> [<prtlev>]\n", program);
	fprintf(stderr, "\n<VDIF input file> is the name of the VDIF file to read\n");
	fprintf(stderr, "\n<framesize> VDIF frame size, including header (5032 for VLBA, 8224 for R2DBE)\n");
	fprintf(stderr, "\n<prtlev> is output type: hex short long\n\n");
	fprintf(stderr, "In normal operation this program searches for valid VDIF frames.\n");
	fprintf(stderr, "The heuristics used to identify valid frames are somewhat weak as\n");
	fprintf(stderr, "VDIF has no formal sync word.  Some data can fool this program.\n\n");
	fprintf(stderr, "If the data are known to contain only entire, valid VDIF frames of\n");
	fprintf(stderr, "of constant length (equal to that provided), <prtlev> can be set to\n");
	fprintf(stderr, "one of the following: forcehex forceshort forcelong.  If one of\n");
	fprintf(stderr, "these is used, the frame finding heuristics are bypassed.\n\n");
}

int main(int argc, char **argv)
{
	const int MaxFrameSize = 16*MAX_VDIF_FRAME_BYTES; /* read this much at a time */
	char buffer[MaxFrameSize];
	enum VDIFHeaderPrintLevel lev = VDIFHeaderPrintLevelShort;
	int leftover = 0;
	int framesize;
	FILE *input;
	long long framesread = 0;
	const vdif_header *header;
	int nSkip = 0;
	int force = 0;
	int n;	/* count read loops */
	int isMark6 = 0;

	if(argc < 3 || argc > 4)
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
	if(argc == 4)
	{
		if(strcmp(argv[3], "force") == 0)
		{
			force = 1;
		}
		else if(strcmp(argv[3], "hex") == 0)
		{
			lev = VDIFHeaderPrintLevelHex;
		}
		else if(strcmp(argv[3], "short") == 0)
		{
			lev = VDIFHeaderPrintLevelShort;
		}
		else if(strcmp(argv[3], "long") == 0)
		{
			lev = VDIFHeaderPrintLevelLong;
		}
		else if(strcmp(argv[3], "forcehex") == 0)
		{
			lev = VDIFHeaderPrintLevelHex;
			force = 1;
		}
		else if(strcmp(argv[3], "forceshort") == 0)
		{
			lev = VDIFHeaderPrintLevelShort;
			force = 1;
		}
		else if(strcmp(argv[3], "forcelong") == 0)
		{
			lev = VDIFHeaderPrintLevelLong;
			force = 1;
		}
		else
		{
			fprintf(stderr, "Print level must be one of hex, short or long.\n");
			
			exit(EXIT_FAILURE);
		}
	}

	for(n = 0;; ++n)
	{
		int index, fill, readbytes;
		uint32_t firstword;

		index = 0;

  		readbytes = fread(buffer+leftover, 1, MaxFrameSize-leftover, input); //read the VDIF header
		if(readbytes <= 0)
		{
			break;
		}
		if(n == 0)
		{
			firstword = *((uint32_t *)buffer);
			if(firstword == MARK6_START_OF_FILE)
			{
				printf("This looks like a Mark6 data file.  I'll skip the first 28 bytes.\n");
				index += 28;	// the first header is larger than the inter-chunk headers
				isMark6 = 1;
			}
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

			if(lev == VDIFHeaderPrintLevelShort && framesread % 24 == 0)
			{
				printf("FrameNum ");
				printVDIFHeader(header, VDIFHeaderPrintLevelColumns);
			}
			if(lev == VDIFHeaderPrintLevelLong)
			{
				printf("Frame %lld ", framesread);
			}
			else
			{
				printf("%8lld ", framesread);
			}
			printVDIFHeader(header, lev);
			
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
