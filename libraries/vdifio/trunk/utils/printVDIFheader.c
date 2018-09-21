/***************************************************************************
 *   Copyright (C) 2014-2018 by Walter Brisken, Adam Deller                *
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
 * $Id$
 * $HeadURL:  $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "vdifio.h"
#include "config.h"

#ifdef HAVE_MARK6SG
#include <mark6sg/mark6gather.h>
#include "mark6gather_vdif.h"
#endif

const char program[] = "printVDIFheader";
const char author[]  = "Walter Brisken <wbrisken@lbo.us>";
const char version[] = "0.7";
const char verdate[] = "20180921";

static void printVersion()
{
	fprintf(stderr, "%s ver. %s  %s  %s\n", program, version, author, verdate);
}

static void usage()
{
	fprintf(stderr, "\n");
	printVersion();
	fprintf(stderr, "\n");
	fprintf(stderr, "A program to dump some basic info about VDIF packets to the screen\n");
	fprintf(stderr, "\nUsage: %s [options] <VDIF input file> [<framesize> [<prtlev> [<offset>] ] ]\n", program);
	fprintf(stderr, "\n[options] can include:\n");
	fprintf(stderr, "  -h or --help    print help information\n");
	fprintf(stderr, "  --version       print version information\n");
	fprintf(stderr, "\n<VDIF input file> is the name of the VDIF file to read (- for stdin)\n");
	fprintf(stderr, "\n<framesize> VDIF frame size, including header (5032 for VLBA, 8224 for R2DBE)\n");
	fprintf(stderr, "\n<prtlev> is output type: none hex short long\n");
	fprintf(stderr, "\n<offset> is a number of bytes to skip at start of file\n\n");
	fprintf(stderr, "In normal operation this program searches for valid VDIF frames.\n");
	fprintf(stderr, "The heuristics used to identify valid frames are somewhat weak as\n");
	fprintf(stderr, "VDIF has no formal sync word.  Some data can fool this program.\n\n");
	fprintf(stderr, "If the data are known to contain only entire, valid VDIF frames of\n");
	fprintf(stderr, "of constant length (equal to that provided), <prtlev> can be set to\n");
	fprintf(stderr, "one of the following: forcehex forceshort forcelong.  If one of\n");
	fprintf(stderr, "these is used, the frame finding heuristics are bypassed.\n");
	fprintf(stderr, "If <framesize> is not provided, or if it is set to 0, the frame size\n");
	fprintf(stderr, "will be determined by the first frame, _even if it is invalid_!\n\n");
#ifdef HAVE_MARK6SG
	fprintf(stderr, "This can be run on Mark6 data directly.\n\n");
#endif
}

int main(int argc, char **argv)
{
	const int MaxFrameSize = 16*MAX_VDIF_FRAME_BYTES; /* read this much at a time */
	char buffer[MaxFrameSize];
	enum VDIFHeaderPrintLevel lev = VDIFHeaderPrintLevelShort;
	int leftover = 0;
	int framesize = 0;
	FILE *input;
	long long framesread = 0;
	const vdif_header *header;
	int nSkip = 0;
	int force = 0;
	int n;	/* count read loops */
#ifdef HAVE_MARK6SG
	int isMark6 = 0;
	int mk6Version = 0;
	int mk6PacketSize = 0;
#endif
	int mk6BlockHeaderSize = 0;
	int framesPerMark6Block = 0;
	int printHeader = 1;		/* if 1, print normal output, otherwise just errors */
	int lastSecond = -1;
	int nFrame = 0;


	if(argc < 2 || argc > 5)
	{
		fprintf(stderr, "\nPlease run with --help to get help information\n\n");

		return EXIT_FAILURE;
	}

	if(strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0)
	{
		usage();

		return EXIT_SUCCESS;
	}
	
	if(strcmp(argv[1], "--version") == 0)
	{
		printVersion();

		return EXIT_SUCCESS;
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

	if(argc > 2)
	{
		framesize = atoi(argv[2]);
		if(framesize > 0)
		{
			if(framesize < VDIF_HEADER_BYTES || framesize > MaxFrameSize)
			{
				fprintf(stderr, "Error: provided framesize is %d bytes, which is outside the range [%d, %d]\n", framesize, VDIF_HEADER_BYTES , MaxFrameSize);

				exit(EXIT_FAILURE);
			}
			if(framesize % 8 != 0)
			{
				fprintf(stderr, "Error: provided framesize must be divisible by 8\n");

				exit(EXIT_FAILURE);
			}
		}
	}
	if(argc > 3)
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
		else if(strcmp(argv[3], "none") == 0)
		{
			printHeader = 0;	// just print errors
		}
		else
		{
			fprintf(stderr, "Print level must be one of hex, short or long.\n");
			
			exit(EXIT_FAILURE);
		}
	}
	if(argc > 4)
	{
		long int jumpBytes;

		jumpBytes = atol(argv[4]);
		fprintf(stderr, "Jumping %ld bytes into file\n", jumpBytes);
		fseek(input, jumpBytes, SEEK_SET);
	}

	if(framesize <= 0)
	{
		int readbytes;

		readbytes = fread(buffer, 1, VDIF_HEADER_BYTES, input); //read the VDIF header
		if(readbytes != VDIF_HEADER_BYTES)
		{
			fprintf(stderr, "Cannot read %d bytes to decode first frame header\n", VDIF_HEADER_BYTES);

			exit(EXIT_FAILURE);
		}
		leftover = VDIF_HEADER_BYTES;
		framesize = getVDIFFrameBytes((const vdif_header *)buffer);
		if(framesize < VDIF_HEADER_BYTES || framesize > MaxFrameSize)
		{
			fprintf(stderr, "Error: first frame indicates framesize is %d bytes, which is outside the range [%d, %d]\n", framesize, VDIF_HEADER_BYTES , MaxFrameSize);

			exit(EXIT_FAILURE);
		}
		printf("Set framesize to be %d bytes, based on first frame found\n", framesize);
	}

	for(n = 0;; ++n)
	{
		int index, fill, readbytes;
		int fr;

		index = 0;
		fr = 0;

  		readbytes = fread(buffer+leftover, 1, MaxFrameSize-leftover, input);
		if(readbytes <= 0)
		{
			break;
		}
#ifdef HAVE_MARK6SG
		if(n == 0)
		{
			const Mark6Header *m6h;
			m6h = (const Mark6Header *)buffer;
			if(m6h->sync_word == MARK6_SYNC)
			{
				int headerSize = 0;

				headerSize = sizeof(Mark6Header);

				mk6BlockHeaderSize = mark6BlockHeaderSize(m6h->version);

				if(mk6BlockHeaderSize > 0)
				{
					printf("This looks like a Mark6 data file (version=%d).  I'll skip the first %d bytes.\n", m6h->version, headerSize);
					printMark6Header(m6h);
					index += headerSize;	// the first header is larger than the inter-chunk headers
					isMark6 = 1;
					framesPerMark6Block = (m6h->block_size - mk6BlockHeaderSize)/m6h->packet_size;
					mk6Version = m6h->version;
					mk6PacketSize = m6h->packet_size;
				}
			}
		}
#endif
		fill = readbytes + leftover;
		for(;;)
		{
			if(fill-index < framesize + mk6BlockHeaderSize)
			{
				/* need more data */
				leftover = fill-index;
				memmove(buffer, buffer+index, leftover);
				break;
			}

#ifdef HAVE_MARK6SG
			if(isMark6)
			{
				if(fr == framesPerMark6Block)
				{
					fr = 0;
					if(mk6Version > 1)
					{
						int32_t *blockSize = (int32_t *)(buffer+index+4);
						framesPerMark6Block = (*blockSize - mk6BlockHeaderSize)/mk6PacketSize;
					}
					/* skip over the block headers to prevent warnings */
					index += mk6BlockHeaderSize;
				}
			}
#endif

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
					static int first = 1;

					if(first)
					{
						first = 0;
						fprintf(stderr, "Error: non-compliant VDIF data: this data has EDV set to 0 but the extended header is not identically 0\n");
					}
				}
				if(header->eversion == 1 || header->eversion == 3)
				{
					const uint32_t *ui;

					ui = (const uint32_t *)(buffer + index);
					if(ui[5] != 0xACABFEED)
					{
						++index;
						++nSkip;
						continue;
					}
				}

				if(nSkip > 0)
				{
					printf("Skipped %d interloper bytes\n", nSkip);
					nSkip = 0;
				}
			}

			if(lastSecond != header->seconds)
			{
				if(lastSecond >= 0)
				{
					printf("Second %d had %d frames\n", lastSecond, nFrame);
				}
				else
				{
					printf("First second = %d\n", header->seconds);
				}
				nFrame = 0;
				lastSecond = header->seconds;
			}
			++nFrame;

			if(printHeader)
			{
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
			}
			
			index += framesize;
			++framesread;
			++fr;
		}
	}

	printf("Read %lld frames\n", framesread);
	
	if(input != stdin)
	{
  		fclose(input);
	}

	return EXIT_SUCCESS;
}
