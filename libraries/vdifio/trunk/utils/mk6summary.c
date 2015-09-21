#include <stdio.h>
#include <stdlib.h>
#include <vdifmark6.h>

void summarize(const char *fileName)
{
	FILE *in;
	Mark6Header H;
	char *buffer;
	char *data;
	int i;
	Mark6BlockHeader_ver2 *h;
	int s;

	in = fopen(fileName, "r");
	if(in == 0)
	{
		fprintf(stderr, "Cannot open %s for read\n", fileName);

		return;
	}

	fread(&H, sizeof(H), 1, in);

	printf("File: %s\n", fileName);
	printMark6Header(&H);

	if(H.version < 2)
	{
		return;
	}

	s = sizeof(Mark6BlockHeader_ver2);
	buffer = (char *)malloc(H.block_size);
	h = (Mark6BlockHeader_ver2 *)buffer;
	data = buffer + s;

	for(i = 0; ; ++i)
	{
		int v, j, n;
		int nSize;

		nSize = 0;
		v = fread(buffer, s, 1, in);
		if(v < 1)
		{
			break;
		}
		n = (h->wb_size-s)/H.packet_size;
		v = fread(buffer+s, 1, h->wb_size-s, in);
		if(v < s)
		{
			printf("Early EOF: only %d bytes read.  %d bytes expected.\n", v, h->wb_size-s);
		}
		for(j = 0; j < n; ++j)
		{
			vdif_header *V;

			V = (vdif_header *)(data + j*H.packet_size);
			if(getVDIFFrameBytes(V) == H.packet_size)
			{
				++nSize;
			}
		}
		printf("%d %d %d/%d\n", i, h->blocknum, nSize, n);
	}

	free(buffer);
}

int main(int argc, char **argv)
{
	int a;

	if(argc < 2)
	{
		printf("Usage: %s <mk6file>\n", argv[0]);

		exit(EXIT_SUCCESS);
	}

	for(a = 1; a < argc; ++a)
	{
		summarize(argv[a]);
	}

	return 0;
}
