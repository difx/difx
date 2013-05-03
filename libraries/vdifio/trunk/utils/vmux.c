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

const char program[] = "vmux";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20130406";

const int defaultChunkSize = 2000000;

int main(int argc, char **argv)
{
	unsigned char *src;
	unsigned char *dest;
	FILE *in, *out;
	int n;
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

	if(argc < 6)
	{
		fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
		fprintf(stderr, "Usage: %s <input file> <input frame size> <frames per second> <thread list> <output file> [<offset> [<chunkSize size>] ]\n", argv[0]);
		fprintf(stderr, "\nA program to take a multi-thread VDIF file and multiplex into\n"
				"a multi-channel, single thread file.  <thread list> should be\n"
				"comma-separated without space.  Setting <input file> to - will take\n"
				"take input from stdin.  Likewise setting output file to - will\n"
				"send output to stdout.  <offset> can be set to seek into the file.\n\n");

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
		destChunkSize -= destChunkSize % 4;
		srcChunkSize -= srcChunkSize % 4;
	}




	if(strcmp(inFile, "-") == 0)
	{
		if(offset != 0)
		{
			fprintf(stderr, "Error: cannot set offset when reading from stdin.\n");

			return EXIT_FAILURE;
		}

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
		else if(offset > 0)
		{
			int r;
			
			r = fseeko(in, offset, SEEK_SET);

			if(r != 0)
			{
				fprintf(stderr, "Error encountered in seek to position %Ld\n", (long long)offset);
				fclose(in);

				return EXIT_FAILURE;
			}
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

	src = (unsigned char *)malloc(srcChunkSize);
	dest = (unsigned char *)malloc(destChunkSize);

	leftover = 0;
	
	resetvdifmuxstatistics(&stats);
	
	for(;;)
	{
		int V;

		n = fread(src+leftover, 1, srcChunkSize-leftover, in);
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
		V = vdifmux(dest, destChunkSize, src, n+leftover, inputframesize, framesPerSecond, 2, nThread, threads, nSort, nGap, -1, &stats);

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

	if(in != stdin)
	{
		fclose(in);
	}
	
	if(out != stdout)
	{
		printvdifmuxstatistics(&stats);
		fclose(out);
	}

	free(src);
	free(dest);

	return 0;
}
