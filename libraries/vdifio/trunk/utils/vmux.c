/***************************************************************************
 *   Copyright (C) 2013-2021 by Walter Brisken                             *
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
const char version[] = "0.13";
const char verdate[] = "20210628";

const int defaultChunkSize = 10000000;
const int defaultNGap = 100;
const int defaultNSort = 5;

const double srcRatio = 2.0;

int die = 0;

void sigpipeHandler(int i)
{
	die = 1;
}

void sigtermHandler(int i)
{
	fprintf(stderr, "vmux: terminating early due to TERM signal\n");

	die = 1;
}

void sigintHandler(int i)
{
	fprintf(stderr, "vmux: terminating early due to INT signal\n");

	die = 1;
}

static void printVersion()
{
	fprintf(stderr, "%s ver. %s  %s  %s\n", program, version, author, verdate);
}

void usage(const char *pgm)
{
	fprintf(stderr, "\n");
	printVersion();
	fprintf(stderr, "\n");
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
	fprintf(stderr, "  --version Print version info and quit\n\n");
	fprintf(stderr, "  --verbose\n");
	fprintf(stderr, "  -v        Increase verbosity\n\n");
	fprintf(stderr, "  --quiet\n");
	fprintf(stderr, "  -q        Decrease verbosity\n\n");
	fprintf(stderr, "  --noEDV4\n");
	fprintf(stderr, "  -n        Don't make use of EDV4 (per-thread validity) in output\n\n");
	fprintf(stderr, "  --EDV4\n");
	fprintf(stderr, "  -e        Use EDV4 (per-thread validity) in output [default]\n\n");
	fprintf(stderr, "  --align\n");
	fprintf(stderr, "  -a        Pre-align multithreaded VDIF threads in problematic recordings (note: do not set 'sort').\n\n");
	fprintf(stderr, "  --fanout <f>\n");
	fprintf(stderr, "  -f <f>    Set fanout factor to <f> (used for some DBBC3 data) [default = 1]\n\n");
	fprintf(stderr, "  --gap <g>\n");
	fprintf(stderr, "  -g <g>    Set the max gap in frames to <g> [default = %d]\n\n", defaultNGap);
	fprintf(stderr, "  --sort <s>\n");
	fprintf(stderr, "  -s <s>    Set the max sort horizon in frames to <s> [default = %d]\n\n", defaultNSort);
	fprintf(stderr, "  --zero\n");
	fprintf(stderr, "  -z        Set nGap and nSort to 0 (same as '-g 0 -s 0')\n\n");
	fprintf(stderr, "  --1bit\n");
	fprintf(stderr, "  -1        Force to operate on 1-bit data streams\n\n");
	fprintf(stderr, "Note: as of version 0.5 this program supports multi-channel multi-thread input data.\n\n");
}

void setSignals()
{
	struct sigaction sigPipe;
	struct sigaction sigTerm;
	struct sigaction sigInt;

	sigPipe.sa_handler = sigpipeHandler;
	sigemptyset(&sigPipe.sa_mask);
	sigPipe.sa_flags = 0;
	sigaction(SIGPIPE, &sigPipe, 0);

	sigTerm.sa_handler = sigtermHandler;
	sigemptyset(&sigTerm.sa_mask);
	sigTerm.sa_flags = 0;
	sigaction(SIGTERM, &sigTerm, 0);
	
	sigInt.sa_handler  = sigintHandler;
	sigemptyset(&sigInt.sa_mask);
	sigInt.sa_flags = 0;
	sigaction(SIGINT, &sigInt, 0);
}

int main(int argc, char **argv)
{
	const int MaxThreads=128;
	unsigned char *src;
	unsigned char *dest;
	FILE *in = 0;
	FILE *out;
	struct vdif_file_reader reader;
	struct vdif_file_reader_stats readerstats;
	int useStdin = 0;
	int verbose = 1;
	int prealign = 0;
	int n, rv;
	int threads[MaxThreads];
	int nThread;
	int inputframesize = 0;
	int nGap = defaultNGap;
	int nSort = defaultNSort;
	struct vdif_mux_statistics stats;
	struct vdif_file_summary summary;
	int leftover;
	int srcChunkSize = (int)(defaultChunkSize*srcRatio);
	int destChunkSize = defaultChunkSize;
	int hasChunkSize = 0;
	int framesPerSecond = 0;
	int64_t nextFrame = -1;
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
			else if(strcmp(argv[a], "--version") == 0)
			{
				printVersion();

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
			else if(strcmp(argv[a], "-z") == 0 || strcmp(argv[a], "--zero") == 0)
			{
				nGap = 0;
				nSort = 0;
			}
			else if(strcmp(argv[a], "-n") == 0 || strcmp(argv[a], "--noEDV4") == 0)
			{
				flags &= ~VDIF_MUX_FLAG_PROPAGATEVALIDITY;
			}
			else if(strcmp(argv[a], "-e") == 0 || strcmp(argv[a], "--EDV4") == 0)
			{
				flags |= VDIF_MUX_FLAG_PROPAGATEVALIDITY;
			}
			else if(strcmp(argv[a], "-a") == 0 || strcmp(argv[a], "--align") == 0)
			{
				prealign = 1;
			}
			else if(strcmp(argv[a], "-1") == 0 || strcmp(argv[a], "--1bit") == 0)
			{
				bitsPerSample = 1;
			}
			else if(a < argc - 1 && (strcmp(argv[a], "-g") == 0 || strcmp(argv[a], "--gap") == 0))
			{
				++a;
				nGap = atoi(argv[a]);
				if(nGap < 0)
				{
					fprintf(stderr, "Error: gap must be positive integer or zero.  Was '%s'\n", argv[a]);

					return EXIT_FAILURE;
				}
			}
			else if(a < argc - 1 && (strcmp(argv[a], "-s") == 0 || strcmp(argv[a], "--sort") == 0))
			{
				++a;
				nSort = atoi(argv[a]);
				if(nSort < 0)
				{
					fprintf(stderr, "Error: sort must be positive integer or zero.  Was '%s'\n", argv[a]);

					return EXIT_FAILURE;
				}
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
			srcChunkSize = destChunkSize*srcRatio;
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

	for(n = nThread = 0; nThread < MaxThreads; ++nThread)
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

		useStdin = 1;
		prealign = 0;
		in = stdin;
	}
	else if(!prealign)
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
	else if(prealign)
	{
		summarizevdiffile(&summary, inFile, 0);
		if(summary.framesPerSecond <= 0)
		{
			summary.framesPerSecond = framesPerSecond;
		}

		n = vdifreaderOpen(&summary, &reader);
		if(n < 0)
		{
			fprintf(stderr, "Can't open %s for read.\n", inFile);

			return EXIT_FAILURE;
		}
		else if(offset > 0)
		{
			vdifreaderSeek(&reader, offset);
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
	if(!src || !dest)
	{
		fprintf(stderr, "Allocation failed! Reduce the chunk size and try again.\n");

		return EXIT_FAILURE;
	}

	setSignals();

	/* read just enough of the stream to peek at a frame header */
	if(useStdin)
	{
		n = fread(src, 1, VDIF_HEADER_BYTES, in);
	}
	else if(!prealign)
	{
		n = fread(src, 1, VDIF_HEADER_BYTES, in);
	}
	else if(prealign)
	{
		n = vdifreaderRead(&reader, src, VDIF_HEADER_BYTES);
	}
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

		if(useStdin || !prealign)
		{
			n = fread(src+leftover, 1, srcChunkSize-leftover, in);
		}
		else if(!prealign)
		{
			n = fread(src+leftover, 1, srcChunkSize-leftover, in);
		}
		else
		{
			n = vdifreaderRead(&reader, src+leftover, srcChunkSize-leftover);
		}

		if(n < 1)
		{
			if(leftover < inputframesize)
			{
				if(verbose > 1)
				{
					fprintf(stderr, "Stopping because leftover=%d < inputframesize=%d\n", leftover, inputframesize);
				}
				break;
			}
			else
			{
				n = 0;
			}
		}

		V = vdifmux(dest, destChunkSize, src, n+leftover, &vm, nextFrame, &stats);
		if(verbose > 2)
		{
			fprintf(stderr, "vdifmux(destSize=%d, srcSize=%d, startFrame=%Ld) -> %d\n", destChunkSize, n+leftover, (long long)nextFrame, V);
		}
		if(V < 0)
		{
			if(stats.nCall == 0)
			{
				if(V == -3)
				{
					fprintf(stderr, "No valid frames (matching expected parameters) were found in the first %d bytes, so there will be no output.  Perhaps use printVDIFheader to determine correct frame parameters.\n", n);
				}
				else
				{
					fprintf(stderr, "No output was provided because the input parameters didn't make sense.  vdifmux() returned %d on the first call.  Perhaps nSort, nGap or chunkSize can be changed to make this work.\n", V);
				}
			}
			fprintf(stderr, "vdifmux() error, returned %d\n", V);
			break;
		}

		if(n < srcChunkSize-leftover && V == 0)
		{
			/* causing end of cycle because no data left and no bytes processed */
			nSort = -1;
		}

		if(verbose > 2 && out != stdout)
		{
			printvdifmuxstatistics(&stats);
			if(!useStdin && nThread > 1 && prealign)
			{
				int j;
				vdifreaderStats(&reader, &readerstats);
				fprintf(stderr, "VDIF reader statistics:\n");
				for(j = 0; j < readerstats.nThread; j++)
				{
					fprintf(stderr, "  Thread %2d relative offset = %d frames\n", summary.threadIds[j], (int)(readerstats.threadOffsets[j]));
				}
				fprintf(stderr, "  Largest offset            = %d frames\n", (int)(readerstats.maxOffset));
			}
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
			int64_t nJump = (int64_t)(stats.startFrameNumber - nextFrame);
			int64_t j;

			fprintf(stderr, "JUMP %zd\n", nJump);

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

		if(stats.srcUsed <= 0)
		{
				fprintf(stderr, "Weird: %d/%d bytes were consumed. Maybe input was all fill-pattern.\n", stats.srcUsed, stats.srcSize);
				leftover = stats.srcSize % inputframesize;
		}
		else
		{
			leftover = stats.srcSize - stats.srcUsed;
		}

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

	if(prealign)
	{
		vdifreaderClose(&reader);
	}
	else if(in != stdin)
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
