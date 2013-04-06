#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <vdifio.h>

#define DEF_CHUNK	2000000

int main(int argc, char **argv)
{
	unsigned char *src;
	unsigned char *dest;
	FILE *in, *out;
	int n;
	int threads[32];
	int nThread = 0;
	int inputframesize;
	int nGap = 8;
	int nSort = 4;
	struct vdif_mux_statistics stats;
	int leftover;
	int chunk = DEF_CHUNK;
	int framesPerSecond;
	long long nextFrame = -1;
	int nWrite;

	if(argc < 6)
	{
		fprintf(stderr, "Usage: %s <input filename> <input frame size> <frames per second> <thread list> <output file> [<chunk size>]\n", argv[0]);
		fprintf(stderr, "\nA program to take a multi-thread VDIF file and multiplex into\n"
				"a multi-channel, single thread file.  <thread list> should be\n"
				"comma-separated without space.  Setting outout file to - will\n"
				"send output to stdout.\n\n");

		return 0;
	}

	inputframesize = atoi(argv[2]);

	framesPerSecond = atoi(argv[3]);

	for(n = 0; nThread < 32; ++nThread)
	{
		int c, p;
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

	}

	if(argc > 6)
	{
		chunk = atoi(argv[6]);
	}

	in = fopen(argv[1], "r");

	if(!in)
	{
		fprintf(stderr, "Can't open %s\n", argv[1]);

		return 0;
	}

	src = malloc(chunk);
	dest = malloc(chunk);
	if(strcmp(argv[5], "-") != 0)
	{
		out = fopen(argv[5], "w");
	}
	else
	{
		out = stdout;
	}

	leftover = 0;
	
	resetvdifmuxstatistics(&stats);
	
	nWrite = 0;

	for(;;)
	{
		n = fread(src+leftover, 1, chunk-leftover, in);
		if(n < 1)
		{
			break;
		}

		vdifmux(dest, chunk, src, n+leftover, inputframesize, framesPerSecond, 2, nThread, threads, nSort, nGap, -1, &stats);

		/* if we encountered fill pattern at the seam between two chunks we will need to write some dummy frames */
		if(nextFrame >= 0 && nextFrame != stats.startFrameNumber)
		{
			int nJump = (int)(stats.startFrameNumber - nextFrame);
			int j;

			/* borrow one output frame of src memory... */
			memcpy(src, dest, VDIF_HEADER_BYTES);
			for(j = 0; j < nJump; ++j)
			{
				setVDIFFrameSecond((vdif_header *)src, (nextFrame+j)/framesPerSecond);
				setVDIFFrameNumber((vdif_header *)src, (nextFrame+j)%framesPerSecond);
				setVDIFFrameInvalid((vdif_header *)src, 1);
				fwrite(src, 1, stats.outputFrameSize, out);
				nWrite += stats.outputFrameSize;
			}
		}

		fwrite(dest, 1, stats.destUsed, out);
		nWrite += stats.destUsed;

		leftover = stats.srcSize - stats.srcUsed;

		if(leftover > 0)
		{
			memmove(src, src+stats.srcUsed, leftover);
		}

		nextFrame = stats.startFrameNumber + stats.nOutputFrame;
	}

	fclose(in);
	
	if(strcmp(argv[4], "-") != 0)
	{
		printvdifmuxstatistics(&stats);
		fclose(out);
	}

	free(src);
	free(dest);

	return 0;
}
