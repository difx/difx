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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <vdifio.h>

const char program[] = "vdif2to8";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20131206";


static void convert2to8(unsigned char *dest, const unsigned char *src, int bytes)
{
	/* choose levels such that the ratio of high to low is as close to 3.3359 
	 * as possible to best maintain amplitude scaling.  127.5 is the center of
	 * the scale (equates to 0).  118.5/35.5 is pretty close to optimal.
	 */
	const unsigned char levels[4] = {9, 92, 163, 246};
	static int first = 1;
	static unsigned char lut2to8[256][4];	/* mapping from input 8 bits (4 samples) to output 4 8-bit samples */
	int i, o;

	if(first)
	{
		/* assemble look up table */

		for(i = 0; i < 256; ++i)
		{
			int j;

			for(j = 0; j < 4; ++j)
			{
				int k;

				k = (i >> (2*j)) & 0x3;
				lut2to8[i][j] = levels[k];
			}
		}
	}

	o = 0;
	for(i = 0; i < bytes; ++i)
	{
		dest[o++] = lut2to8[src[i]][0];
		dest[o++] = lut2to8[src[i]][1];
		dest[o++] = lut2to8[src[i]][2];
		dest[o++] = lut2to8[src[i]][3];
	}
}

int vdif2to8(FILE *out, FILE *in, int inputFrameBytes)
{
	const int inputBufferSize = 1000000;
	const int nBitIn = 2;
	const int nBitOut = 8;
	unsigned char *inputBuffer;
	unsigned char *outputBuffer;
	int arrayEnd = 0;
	int index = 0;
	int outputFrameBytes;
	long long nSkip = 0;

	void (*convert)(unsigned char *, const unsigned char *, int);

	convert = convert2to8;

	outputFrameBytes = (inputFrameBytes - VDIF_HEADER_BYTES)*nBitOut/nBitIn + VDIF_HEADER_BYTES;

	outputBuffer = malloc(outputFrameBytes);
	if(!outputBuffer)
	{
		fprintf(stderr, "Cannot allocate %d bytes\n", inputBufferSize);

		return -1;
	}

	inputBuffer = malloc(inputBufferSize);
	if(!inputBuffer)
	{
		fprintf(stderr, "Cannot allocate %d bytes\n", inputBufferSize);

		free(outputBuffer);

		return -2;
	}

	for(;;)
	{
		int nLeft;
		size_t v;
		vdif_header *vh;

		nLeft = arrayEnd - index;
		if(nLeft < inputFrameBytes)
		{
			if(nLeft > 0)
			{
				memmove(inputBuffer, inputBuffer+index, nLeft);
			}
			
			v = fread(inputBuffer+nLeft, 1, inputBufferSize-nLeft, in);
			arrayEnd = nLeft + v;
			if(arrayEnd < inputFrameBytes)
			{
				break;
			}
			index = 0;
		}

		vh = (vdif_header *)(inputBuffer + index);

		if(getVDIFFrameBytes(vh) != inputFrameBytes || getVDIFBitsPerSample(vh) != nBitIn || vh->legacymode)
		{
			++index;
			++nSkip;

			continue;
		}

		/* presume that if we got here that inputBuffer+index points to start of a valid frame */

		/* adjust header and stash it */
		setVDIFBitsPerSample(vh, nBitOut);
		setVDIFFrameBytes(vh, outputFrameBytes);
		memcpy(outputBuffer, inputBuffer+index, VDIF_HEADER_BYTES);

		/* convert binary data */
		convert(outputBuffer+VDIF_HEADER_BYTES, inputBuffer+index+VDIF_HEADER_BYTES, inputFrameBytes-VDIF_HEADER_BYTES);

		/* write modified frame to disk */
		v = fwrite(outputBuffer, 1, outputFrameBytes, out);

		if(v != outputFrameBytes)
		{
			fprintf(stderr, "Error: short write: %d/%d bytes.  Stopping.\n", (int)v, outputFrameBytes);
			fprintf(stderr, "Is the disk full?\n");

			break;
		}

		index += inputFrameBytes;
	}

	free(inputBuffer);
	free(outputBuffer);

	printf("Number of skipped bytes = %lld\n", nSkip);

	return 0;
}

int main(int argc, char **argv)
{
	const char *inFile;
	const char *outFile;
	int inputFrameBytes;
	FILE *in, *out;

	if(argc != 4)
	{
		fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
		fprintf(stderr, "Usage: %s <inputFile> <inputFrameBytes> <outputFile>\n", argv[0]);
		fprintf(stderr, "\nA program to take a VDIF file containing 2-bit samples and\n"
		                "convert it to 8-bit samples.\n\n");
		fprintf(stderr, "<inputFile> is the input 2-bit VDIF file, or - for stdin\n\n");
		fprintf(stderr, "<inputFrameBytes> is the size of one thread's data frame, including\n    header (for RDBE VDIF data this is 5032)\n\n");
		fprintf(stderr, "<outputFile> is the name of the output, 8-bit VDIF file,\n    or - for stdout\n\n");

		return 0;
	}

	inFile = argv[1];
	inputFrameBytes = atoi(argv[2]);
	outFile = argv[3];

	if(strcmp(inFile, "-") == 0)
	{
		in = stdin;
	}
	else
	{
		in = fopen(inFile, "r");

		if(!in)
		{
			fprintf(stderr, "Can't open %s for read.\n", inFile);

			return EXIT_FAILURE;
		}
	}

	if(strcmp(outFile, "-") != 0)
	{
		out = fopen(outFile, "w");
		if(!out)
		{
			fprintf(stderr, "Can't open %s for write.\n", outFile);
			fclose(in);

			return EXIT_FAILURE;
		}
	}
	else
	{
		out = stdout;
	}

	vdif2to8(out, in, inputFrameBytes);

	if(in != stdin)
	{
		fclose(in);
	}

	if(out != stdout)
	{
		fclose(out);
	}

	return 0;
}
