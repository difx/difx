/***************************************************************************
 *   Copyright (C) 2013-2016 by Walter Brisken                             *
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
#include <signal.h>
#include <vdifio.h>

const char program[] = "vmux";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.7";
const char verdate[] = "20160624";

const int defaultChunkSize = 2000000;

int die = 0;

void sigpipeHandler(int i)
{
	die = 1;
}

void usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	fprintf(stderr, "Usage: %s [options] <inputFile> <inputFrameSize> <framesPerSecond>\n   <threadList> <outputFile> [<offset> [<chunkSize>] ]\n", pgm);
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
	fprintf(stderr, "Options can include:\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h        Print this help info and quit\n\n");
	fprintf(stderr, "  --verbose\n");
	fprintf(stderr, "  -v        Increase verbosity\n\n");
	fprintf(stderr, "  --quiet\n");
	fprintf(stderr, "  -q        Decrease verbosity\n\n");
	fprintf(stderr, "  --noEDV4\n");
	fprintf(stderr, "  -n        Don't make use of EDV4 (per-thread validity) in output\n\n");
	fprintf(stderr, "  --EDV4\n");
	fprintf(stderr, "  -e        Use of EDV4 (per-thread validity) in output [default]\n\n");
	fprintf(stderr, "  --fanout <f>\n");
	fprintf(stderr, "  -f <f>    Set fanout factor to <f> (used for some DBBC3 data) [default = 1]\n\n");
	fprintf(stderr, "Note: as of version 0.5 this program supports multi-channel multi-thread input data\n\n");
}

int main(int argc, char **argv)
{
	unsigned char *src;
	unsigned char *dest;
	FILE *in, *out;
	int verbose = 1;
	int n, rv;
	int threads[32];
	int nThread;
	int inputframesize = 0;
	int nGap = 100;
	int nSort = 20;
	struct vdif_mux_statistics stats;
	int leftover;
	int srcChunkSize = defaultChunkSize*5/4;
	int destChunkSize = defaultChunkSize;
	int hasChunkSize = 0;
	int framesPerSecond = 0;
	long long nextFrame = -1;
	const char *inFile = 0;
	const char *outFile = 0;
	const char *threadString = 0;
	off_t offset = 0;
	int hasOffset = 0;
	int bitsPerSample = 0;
	int nChanPerThread;
	int fanoutFactor = 1;
	const vdif_header *vh;
	struct vdif_mux vm;
	int flags = VDIF_MUX_FLAG_PROPAGATEVALIDITY | VDIF_MUX_FLAG_RESPECTGRANULARITY;
	int a;

	signal(SIGPIPE, &sigpipeHandler);

	if(argc <= 1)
	{
		usage(argv[0]);

		return 0;
	}

	for(a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-' && strlen(argv[a]) > 1) 
		{
			if(strcmp(argv[a], "-h") == 0 || strcmp(argv[a], "--help") == 0)
			{
				usage(argv[0]);

				return EXIT_SUCCESS;
			}
			else if(strcmp(argv[a], "-v") == 0 || strcmp(argv[a], "--verbose") == 0)
			{
				++verbose;
			}
			else if(strcmp(argv[a], "-q") == 0 || strcmp(argv[a], "--quiet") == 0)
			{
				--verbose;
			}
			else if(strcmp(argv[a], "-n") == 0 || strcmp(argv[a], "--noEDV4") == 0)
			{
				flags &= ~VDIF_MUX_FLAG_PROPAGATEVALIDITY;
			}
			else if(strcmp(argv[a], "-e") == 0 || strcmp(argv[a], "--EDV4") == 0)
			{
				flags |= VDIF_MUX_FLAG_PROPAGATEVALIDITY;
			}
			else if(a < argc - 1 && (strcmp(argv[a], "-f") == 0 || strcmp(argv[a], "--fanout") == 0))
			{
				++a;
				fanoutFactor = atoi(argv[a]);
				if(fanoutFactor < 1)
				{
					fprintf(stderr, "Error: fanout factor must be positive integer.  Was '%s'\n", argv[a]);

					return EXIT_FAILURE;
				}
			}
			else
			{
				fprintf(stderr, "Error: argument %d unknown option '%s'\n", a, argv[a]);

				return EXIT_FAILURE;
			}
		}
		else if(inFile == 0)
		{
			inFile = argv[a];
			if(verbose > 2)
			{
				fprintf(stderr, "Arg %d: Input file = %s\n", a, inFile);
			}
		}
		else if(inputframesize == 0)
		{
			inputframesize = atoi(argv[a]);
			if(inputframesize <= 0)
			{
				fprintf(stderr, "Error: Argument %d, '%s', should be a positive integer (input frame size)\n", a, argv[a]);

				return EXIT_FAILURE;
			}
			if(verbose > 2)
			{
				fprintf(stderr, "Arg %d: Input frame size = %d\n", a, inputframesize);
			}
		}
		else if(framesPerSecond == 0)
		{
			framesPerSecond = atoi(argv[a]);
			if(framesPerSecond <= 0)
			{
				fprintf(stderr, "Error: Argument %d, '%s', should be a positive integer (frames per second)\n", a, argv[a]);

				return EXIT_FAILURE;
			}
			if(verbose > 2)
			{
				fprintf(stderr, "Arg %d: Frames per second = %d\n", a, framesPerSecond);
			}
		}
		else if(threadString == 0)
		{
			threadString = argv[a];
			if(verbose > 2)
			{
				fprintf(stderr, "Arg %d: Thread string = %s\n", a, threadString);
			}
		}
		else if(outFile == 0)
		{
			outFile = argv[a];
			if(verbose > 2)
			{
				fprintf(stderr, "Arg %d: Output file = %s\n", a, outFile);
			}
		}
		else if(hasOffset == 0)
		{
			hasOffset = 1;
			offset = atoll(argv[a]);
			if(verbose > 2)
			{
				fprintf(stderr, "Arg %d: Offset = %lld\n", a, (long long)offset);
			}
		}
		else if(hasChunkSize == 0)
		{
			hasChunkSize = 1;
			destChunkSize = atoi(argv[a]);
			srcChunkSize = destChunkSize*5/4;
			destChunkSize -= destChunkSize % 8;
			srcChunkSize -= srcChunkSize % 8;
			if(verbose > 2)
			{
				fprintf(stderr, "Arg %d: Chunk size = %d\n", a, destChunkSize);
			}
		}
		else
		{
			fprintf(stderr, "Unexpected argument %d, '%s'\n", a, argv[a]);

			return EXIT_FAILURE;
		}
	}

	if(outFile == 0)
	{
		fprintf(stderr, "Incomplete command line.  Run with --help for usage information.\n");

		return EXIT_FAILURE;
	}

	for(n = nThread = 0; nThread < 32; ++nThread)
	{
		int c, p, i;
		if(threadString[n] == ',')
		{
			++n;
		}
		c = sscanf(threadString+n, "%d%n", &(threads[nThread]), &p);
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
					fprintf(stderr, "Error! threadId %d listed more than once!\n", threads[nThread]);

					return EXIT_FAILURE;
				}
			}
		}
	}
	if(nThread == 0)
	{
		fprintf(stderr, "No threads parsable from list: %s\n", threadString);

		return EXIT_FAILURE;
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
				fprintf(stderr, "Error encountered in seek to position %lld\n", (long long)offset);
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

	/* read just enough of the stream to peek at a frame header */
	n = fread(src, 1, VDIF_HEADER_BYTES, in);
	if(n != VDIF_HEADER_BYTES)
	{
		fprintf(stderr, "Error reading first header.  Only %d of %d bytes were read\n", n, VDIF_HEADER_BYTES);

		return EXIT_FAILURE;
	}
	leftover = VDIF_HEADER_BYTES;

	vh = (const vdif_header *)src;

	if(bitsPerSample <= 0)
	{
		bitsPerSample = getVDIFBitsPerSample(vh);
		fprintf(stderr, "Got %d bits per sample from the first frame header\n", bitsPerSample);
	}

	if(getVDIFComplex(vh) != 0)
	{
		flags |= VDIF_MUX_FLAG_COMPLEX;
		fprintf(stderr, "Looks like complex sampled data.  Will take this into consideration.\n");
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

	if(fanoutFactor > 1)
	{
		fprintf(stderr, "Note: setting multiplex fanout to %d -- this is a new, untested feature\n", fanoutFactor);

		rv = setvdifmuxfanoutfactor(&vm, fanoutFactor);
		if(rv < 0)
		{
			fprintf(stderr, "Error adjusting vdifmux for fanout factor %d\n", fanoutFactor);

			return EXIT_FAILURE;
		}
	}

	if(verbose > 0 && strcmp(outFile, "-") != 0)
	{
		printvdifmux(&vm);
	}
	
	resetvdifmuxstatistics(&stats);
	
	while(!die)
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
		V = vdifmux(dest, destChunkSize, src, n+leftover, &vm, nextFrame, &stats);

		if(V < 0)
		{
			break;
		}

		if(verbose > 2 && out != stdout)
		{
			printvdifmuxstatistics(&stats);
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

			fprintf(stderr, "JUMP %d\n", nJump);

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
	
	if(verbose > 0 && out != stdout)
	{
		printvdifmuxstatistics(&stats);
		fclose(out);
	}

	free(src);
	free(dest);

	return 0;
}
