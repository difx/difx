#include "config.h"
#include <stdio.h>
#include <stdlib.h>

#define FRAMESIZE	10016

int main(int argc, char **argv)
{
	unsigned char frame[FRAMESIZE];
	FILE *in;
	int i, v;
	int frameNum, lastFrameNum = 1000000000;

	if(argc > 1)
	{
		in = fopen(argv[1], "r");
		if(!in)
		{
			printf("Cannot open %s\n", argv[1]);

			return EXIT_FAILURE;
		}
	}
	else
	{
		printf("Usage: %s <filename> [<offset>]\n", argv[0]);

		return EXIT_FAILURE;
	}
	if(argc > 2)
	{
		long offset;
		
		offset = atol(argv[2]);
		v = fseek(in, offset, SEEK_SET);
		if(v < 0)
		{
			fprintf(stderr, "Error seeking %ld bytes into %s\n", offset, argv[1]);
			fclose(in);

			return EXIT_FAILURE;
		}
	}

	for(i = 0; ; i++)
	{
		v = fread(frame, FRAMESIZE, 1, in);
		if(!v)
		{
			break;
		}

		frameNum = frame[4]+256L*(frame[5] & 0x7F);

		printf("%02X%02X%02X%02X %c %5d  ", frame[3], frame[2], frame[1], frame[0], (frame[5] & 0x80 ? '*' : ' '), frameNum);
		printf("%02X%02X%02X%02X %02X%02X%02X%02X", frame[11], frame[10], frame[9], frame[8], frame[15], frame[14], frame[13], frame[12]);
		if(frameNum > lastFrameNum && frameNum != lastFrameNum+1)
		{
			printf("  skipped %d frames", frameNum-lastFrameNum-1);
		}
		printf("\n");

		lastFrameNum = frameNum;
	}
	fclose(in);

	return EXIT_SUCCESS;
}
