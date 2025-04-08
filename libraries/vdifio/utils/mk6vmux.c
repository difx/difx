/***************************************************************************
 *   Copyright (C) 2018-2024 by Walter Brisken                             *
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <vdifio.h>
#include <mark6sg/mark6gather.h>
#include "mark6gather_vdif.h"

const char program[] = "mk6vmux";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.2";
const char verdate[] = "20180905";

const int defaultChunkSize = 2000000;

int main(int argc, char **argv)
{
	unsigned char *src;
	unsigned char *dest;
	FILE *out;
	int n, rv;
	int threads[32];
	int nThread;
	int inputframesize;
	int nGap = 100;
	int nSort = 20;
	struct vdif_mux_statistics stats;
	int leftover;
	int srcChunkSize = defaultChunkSize*5/4;
	int destChunkSize = defaultChunkSize;
	int framesPerSecond;
	long long nextFrame = -1;
	const char *inFile;
	const char *outFile;
	off_t offset = 0;
	int bitsPerSample = 0;
	int nChanPerThread;
	const vdif_header *vh;
	struct vdif_mux vm;
	int flags = 0;
	Mark6Gatherer *G;

	if(argc < 6)
	{
		fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
		fprintf(stderr, "Usage: %s <inputFile> <inputFrameSize> <framesPerSecond> <threadList>\n    <outputFile> [<offset> [ [<chunkSize>] ] }\n", argv[0]);
		fprintf(stderr, "\nA program to take a multi-thread VDIF file and multiplex into\n"
				"a multi-channel, single thread file.  <thread list> should be\n"
				"comma-separated without space.  Setting <input file> to - will take\n"
				"take input from stdin.  Likewise setting output file to - will\n"
				"send output to stdout.  <offset> can be set to seek into the file.\n\n");
		fprintf(stderr, "<inputFile> is the input multi-thread VDIF file, or - for stdin\n\n");
		fprintf(stderr, "<inputFrameSize> is the size of one thread's data frame, including\n    header (for RDBE VDIF data this is 5032)\n\n");
		fprintf(stderr, "<framesPerSecond> is the number of frames per second in the input\n    file for each thread (and is thus the number of output frames per\n    second as well)\n\n");
		fprintf(stderr, "<threadList> is a comma-separated list of integers in range 0 to 1023;\n    the order of the numbers is significant and dictates the order of\n    channels in the output data\n\n");
		fprintf(stderr, "<outputFile> is the name of the output, single-thread VDIF file,\n    or - for stdout\n\n");
		fprintf(stderr, "<offset> is an optional offset into the input file (in bytes)\n\n");
		fprintf(stderr, "<chunkSize> is (roughly) how many bytes to operate on at a time\n    [default=%d]\n\n", defaultChunkSize);
		fprintf(stderr, "Note: as of version 0.5 this program supports multi-channel multi-thread input data\n\n");

		return 0;
	}

	inFile = argv[1];
	inputframesize = atoi(argv[2]);
	framesPerSecond = atoi(argv[3]);
	outFile = argv[5];


	for(n = nThread = 0; nThread < 32; ++nThread)
	{
		int c, p, i;
		if(argv[4][n] == ',')
		{
			++n;
		}
		c = sscanf(argv[4]+n, "%d%n", &(threads[nThread]), &p);
		n += p;	

		if(c != 1)
		{
			break;
		}

		if(threads[nThread] < 0 || threads[nThread] > 1023)
		{
			fprintf(stderr, "ThreadId with value %d found.  Must be in range 0..1023.\n", threads[nThread]);

			return EXIT_FAILURE;
		}

		if(nThread > 0)
		{
			for(i = 0; i < nThread; ++i)
			{
				if(threads[i] == threads[nThread])
				{
					printf("Error! threadId %d listed more than once!\n", threads[nThread]);

					return EXIT_FAILURE;
				}
			}
		}
	}
	if(nThread == 0)
	{
		fprintf(stderr, "No threads parsable from list: %s\n", argv[4]);

		return EXIT_FAILURE;
	}

	if(argc > 6)
	{
		offset = atoll(argv[6]);
	}
	if(argc > 7)
	{
		destChunkSize = atoi(argv[7]);
		srcChunkSize = destChunkSize*5/4;
		destChunkSize -= destChunkSize % 8;
		srcChunkSize -= srcChunkSize % 8;
	}


	G = openMark6GathererFromTemplate(inFile);

	if(!G)
	{
		fprintf(stderr, "Can't open %s for read.\n", inFile);

		return EXIT_FAILURE;
	}

	if(offset > 0)
	{
		int r;
		
		r = seekMark6Gather(G, offset);

		if(r != 0)
		{
			fprintf(stderr, "Error encountered in seek to position %lld\n", (long long)offset);
			closeMark6Gatherer(G);

			return EXIT_FAILURE;
		}
	}

	if(strcmp(outFile, "-") != 0)
	{
		out = fopen(outFile, "w");
		if(!out)
		{
			fprintf(stderr, "Can't open %s for write.\n", outFile);
			closeMark6Gatherer(G);

			return EXIT_FAILURE;
		}
	}
	else
	{
		out = stdout;
	}

	srcChunkSize = srcChunkSize - (srcChunkSize % G->packetSize);

	src = (unsigned char *)malloc(srcChunkSize);
	dest = (unsigned char *)malloc(destChunkSize);

	/* read just enough of the stream to peek at a frame header */

	n = mark6Gather(G, src, G->packetSize);

	if(n < G->packetSize)
	{
		fprintf(stderr, "Error reading first header.  Only %d of %d bytes were read\n", n, VDIF_HEADER_BYTES);

		return EXIT_FAILURE;
	}
	leftover = n;

	vh = (const vdif_header *)src;

	bitsPerSample = getVDIFBitsPerSample(vh);
	fprintf(stderr, "Got %d bits per sample from the first frame header\n", bitsPerSample);

	if(getVDIFComplex(vh) != 0)
	{
		flags |= VDIF_MUX_FLAG_COMPLEX;
		printf("Looks like complex sampled data.  Will take this into consideration.\n");
	}

	rv = configurevdifmux(&vm, inputframesize, framesPerSecond, bitsPerSample, nThread, threads, nSort, nGap, flags);
	if(rv < 0)
	{
		fprintf(stderr, "Error configuring vdifmux: %d\n", rv);

		return EXIT_FAILURE;
	}

	nChanPerThread = getVDIFNumChannels(vh);
	fprintf(stderr, "Got %d chans per thread from the first frame header\n", nChanPerThread);
	if(nChanPerThread != 1)
	{
		fprintf(stderr, "Setting nChanPerThread to %d based on first frame header\n", nChanPerThread);
		rv = setvdifmuxinputchannels(&vm, nChanPerThread);
		if(rv < 0)
		{
			fprintf(stderr, "Error adjusting vdifmux for %d channels per thread\n", nChanPerThread);

			return EXIT_FAILURE;
		}
	}

	if(strcmp(outFile, "-") != 0)
	{
		printvdifmux(&vm);
	}
	
	resetvdifmuxstatistics(&stats);
	
	for(;;)
	{
		int V;

		n = mark6Gather(G, src+leftover, srcChunkSize-leftover);
		if(n < 1)
		{
			if(leftover < inputframesize)
			{
				break;
			}
			else
			{
				n = 0;
			}
		}

		if(n < srcChunkSize-leftover)
		{
			nSort = -nSort;
		}
		V = vdifmux(dest, destChunkSize, src, n+leftover, &vm, nextFrame, &stats);

		if(V < 0)
		{
			break;
		}

		if(stats.startFrameNumber < 0)
		{
			if(stats.srcUsed > 0)
			{
				/* bytes were consumed, but no useful output was generated */

				continue;
			}
			else
			{
				/* here no bytes were consumed.  This is probably not a good thing */

				fprintf(stderr, "Weird: %d/%d bytes were consumed.  Stopping.\n", stats.srcUsed, stats.srcSize);

				break;
			}
		}

		/* if we encountered fill pattern at the seam between two chunkSizes we will need to write some dummy frames */
		if(nextFrame >= 0 && nextFrame != stats.startFrameNumber)
		{
			int nJump = (int)(stats.startFrameNumber - nextFrame);
			int j;

			printf("JUMP %d\n", nJump);

			/* borrow one output frame of src memory... */
			memcpy(src, dest, VDIF_HEADER_BYTES);
			for(j = 0; j < nJump; ++j)
			{
				setVDIFFrameSecond((vdif_header *)src, (nextFrame+j)/framesPerSecond);
				setVDIFFrameNumber((vdif_header *)src, (nextFrame+j)%framesPerSecond);
				setVDIFFrameInvalid((vdif_header *)src, 1);
				fwrite(src, 1, stats.outputFrameSize, out);
			}
		}

		fwrite(dest, 1, stats.destUsed, out);

		leftover = stats.srcSize - stats.srcUsed;

		if(leftover > 0)
		{
			memmove(src, src+stats.srcUsed, leftover);
		}

		nextFrame = stats.startFrameNumber + stats.nOutputFrame;
		
		if(nSort < 0)
		{
			break;
		}
	}

	closeMark6Gatherer(G);
	
	if(out != stdout)
	{
		printvdifmuxstatistics(&stats);
		fclose(out);
	}

	free(src);
	free(dest);

	return 0;
}
