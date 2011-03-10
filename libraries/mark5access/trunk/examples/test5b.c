#include <stdio.h>
#include <stdlib.h>

#define FRAMESIZE	10016

int main(int argc, char **argv)
{
	unsigned char frame[FRAMESIZE];
	FILE *in;
	int i, v;
	long long offset;

	if(argc > 1)
	{
		in = fopen(argv[1], "r");
		if(!in)
		{
			printf("Cannot open %s\n", argv[1]);
		}
	}
	else
	{
		printf("Usage: %s <filename> [<offset>]\n", argv[0]);

		exit(0);
	}
	if(argc > 2)
	{
		offset = atoi(argv[2]);
	}

	for(i = 0; ; i++)
	{
		v = fread(frame, FRAMESIZE, 1, in);
		if(!v)
		{
			break;
		}

		printf("%02X%02X%02X%02X %5d  ", frame[3], frame[2], frame[1], frame[0], frame[4]+256L*frame[5]);
		printf("%02X%02X%02X%02X %02X%02X%02X%02X\n", frame[11], frame[10], frame[9], frame[8], frame[15], frame[14], frame[13], frame[12]);
	}
	fclose(in);

	return 0;
}
