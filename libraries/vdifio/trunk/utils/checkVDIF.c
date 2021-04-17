/***************************************************************************
 *   Copyright (C) 2021 by Walter Brisken                                  *
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
 * $Id: printVDIFheader.c 9371 2019-12-15 22:46:41Z WalterBrisken $
 * $HeadURL:  $
 * $LastChangedRevision: 9371 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2019-12-15 15:46:41 -0700 (Sun, 15 Dec 2019) $
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

const char program[] = "checkvdif";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "2021047";

static void printVersion()
{
	fprintf(stderr, "%s ver. %s  %s  %s\n", program, version, author, verdate);
}

static void usage()
{
	fprintf(stderr, "\n");
	printVersion();
	fprintf(stderr, "\n");
	fprintf(stderr, "A program to verify integrity of a VDIF file\n");
	fprintf(stderr, "\nUsage: %s [options] <VDIF input file>\n", program);
	fprintf(stderr, "\n[options] can include:\n\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h              print help information and quit\n\n");
	fprintf(stderr, "  --version       print version information\n\n");
	fprintf(stderr, "  --short\n");
	fprintf(stderr, "  -s              perform a short test\n\n");
	fprintf(stderr, "  --full\n");
	fprintf(stderr, "  -F              perform a test on the full file\n\n");
	fprintf(stderr, "  --jump [bytes]\n");
	fprintf(stderr, "  -j [bytes]      jump [bytes] into file before starting\n\n");
	fprintf(stderr, "  --bits [bits]\n");
	fprintf(stderr, "  -b [bits]       fail if any frame has other than [bits] quantization\n\n");
	fprintf(stderr, "  --framesize [bytes]\n");
	fprintf(stderr, "  -f [bytes]      fail if any frame has other than [bytes] per frame\n\n");
	fprintf(stderr, "<VDIF input file> is the name of the VDIF file to read (- for stdin)\n\n");
#ifdef HAVE_MARK6SG
	fprintf(stderr, "This can be run on Mark6 data directly.\n\n");
#endif
}

int main(int argc, char **argv)
{
	const int MaxFrameSize = 16*MAX_VDIF_FRAME_BYTES; /* read this much at a time */
	char buffer[MaxFrameSize];
	int framesToProcess = 102400;	/* by default, look at this many frames before declaring success */
	long int jumpBytes = 0;
	int leftover = 0;
	int framesize = 0;
	FILE *input;
	long long framesread = 0;
	const vdif_header *header;
	int nSkip = 0;
	int force = 1;			/* expect a perfect grid of data */
	int n;	/* count read loops */
#ifdef HAVE_MARK6SG
	int isMark6 = 0;
	int mk6Version = 0;
	int mk6PacketSize = 0;
#endif
	int mk6BlockHeaderSize = 0;
	int framesPerMark6Block = 0;
	int lastSecond = -1;
	int lastEpoch = -1;
	int nBit = -1;
	int a;
	const char *filename = 0;
	int returnValue = EXIT_SUCCESS;

	for(a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-") == 0)
			{
				filename = argv[a];
			}
			else if(strcmp(argv[a], "-h") == 0 || strcmp(argv[a], "--help") == 0)
			{
				usage();

				return EXIT_SUCCESS;
			}
			else if(strcmp(argv[a], "--version") == 0)
			{
				printVersion();

				return EXIT_SUCCESS;
			}
			else if(strcmp(argv[a], "-s") == 0 || strcmp(argv[a], "--short") == 0)
			{
				framesToProcess = 1024;
			}
			else if(strcmp(argv[a], "-F") == 0 || strcmp(argv[a], "--full") == 0)
			{
				framesToProcess = -1;
			}
			else if(a < argc - 1)
			{
				if(strcmp(argv[a], "-j") == 0 || strcmp(argv[a], "--jump") == 0)
				{
					++a;
					jumpBytes = atoll(argv[a]);
				}
				else if(strcmp(argv[a], "-f") == 0 || strcmp(argv[a], "--framesize") == 0)
				{
					++a;
					framesize = atoi(argv[a]);
				}
				else if(strcmp(argv[a], "-b") == 0 || strcmp(argv[a], "--bits") == 0)
				{
					++a;
					nBit = atoi(argv[a]);
				}
				else
				{
					fprintf(stderr, "Unknown command line parameter '%s'\n", argv[a]);

					return EXIT_FAILURE;
				}
			}
			else
			{
				fprintf(stderr, "Unknown command line parameter '%s'\n", argv[a]);

				return EXIT_FAILURE;
			}
		}
		else if(filename == 0)
		{
			filename = argv[a];
		}
		else
		{
			fprintf(stderr, "Error: cannot provide more than one input\n");

			return EXIT_FAILURE;
		}
	}

	if(filename == 0)
	{
		fprintf(stderr, "Error: no input files provided\n");

		return EXIT_FAILURE;
	}

	if(strcmp(filename, "-") == 0)
	{
		filename = "<stdin>";
		input = stdin;
	}
	else
	{
		input = fopen(filename, "r");
		if(!input)
		{
			fprintf(stderr, "Cannot open for read %s\n", filename);

			exit(EXIT_FAILURE);
		}
	}

	if(jumpBytes > 0)
	{
		fseek(input, jumpBytes, SEEK_SET);
	}

	if(framesize <= 0)
	{
		int readbytes;

		readbytes = fread(buffer, 1, VDIF_HEADER_BYTES, input); //read the VDIF header
		if(readbytes != VDIF_HEADER_BYTES)
		{
			fprintf(stderr, "File %s: Cannot read %d bytes to decode first frame header\n", filename, VDIF_HEADER_BYTES);

			returnValue = 18;

			goto bail;
		}
		leftover = VDIF_HEADER_BYTES;
		framesize = getVDIFFrameBytes((const vdif_header *)buffer);
		if(framesize < VDIF_HEADER_BYTES || framesize > MaxFrameSize)
		{
			fprintf(stderr, "File %s: First frame indicates framesize is %d bytes, which is outside the range [%d, %d]\n", filename, framesize, VDIF_HEADER_BYTES, MaxFrameSize);

			returnValue = 19;

			goto bail;
		}
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
						fprintf(stderr, "File %s Error: non-compliant VDIF data: this data has EDV set to 0 but the extended header is not identically 0\n", filename);
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

			if(nBit > 0 && getVDIFBitsPerSample(header) != nBit)
			{
				fprintf(stderr, "File %s Bits mismatch: %d %d\n", filename, getVDIFBitsPerSample(header), nBit);
				
				returnValue = 23;

				goto bail;
			}

			if(getVDIFFrameBytes(header) != framesize)
			{
				fprintf(stderr, "File %s Frame size mismatch: %d %d\n", filename, getVDIFFrameBytes(header), framesize);

				returnValue = 22;

				goto bail;
			}

			if(lastEpoch != header->epoch)
			{
				if(lastEpoch >= 0)
				{
					fprintf(stderr, "File %s Multiple epochs detected: %d %d\n", filename, lastEpoch, header->epoch);
					
					returnValue = 21;

					goto bail;
				}
				else
				{
					lastEpoch = header->epoch;
				}
			}

			if(lastSecond != header->seconds)
			{
				if(lastSecond >= 0)
				{
					if(header->seconds < lastSecond - 1)
					{
						fprintf(stderr, "File %s Time skew detected: %d %d\n", filename, lastSecond, header->seconds);

						returnValue = 20;

						goto bail;
					}
				}
				lastSecond = header->seconds;
			}

			index += framesize;
			++framesread;
			++fr;

			if(framesToProcess > 0 && framesread >= framesToProcess)
			{
				break;
			}
		}
		if(framesToProcess > 0 && framesread >= framesToProcess)
		{
			break;
		}
	}

bail:

	if(returnValue == EXIT_SUCCESS)
	{
		fprintf(stderr, "File %s appears good.\n", filename);
	}
	
	if(input != stdin)
	{
  		fclose(input);
	}

	return returnValue;
}
