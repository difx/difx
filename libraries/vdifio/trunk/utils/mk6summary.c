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
	vdif_header *v1, *v2;

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

	v1 = (vdif_header *)data;

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
			v2 = (vdif_header *)(data + j*H.packet_size);
			if(getVDIFFrameBytes(v2) == H.packet_size)
			{
				++nSize;
			}
		}
		printf("%d %d %d/%d  %d:%05d:%d - %d:%05d:%d\n", i, h->blocknum, nSize, n,
			getVDIFFrameEpochSecOffset(v1), getVDIFFrameNumber(v1), getVDIFThreadID(v1),
			getVDIFFrameEpochSecOffset(v2), getVDIFFrameNumber(v2), getVDIFThreadID(v2));
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
