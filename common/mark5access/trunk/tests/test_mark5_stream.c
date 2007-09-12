#include <stdlib.h>
#include "mark5access/mark5_stream.h"

int main(int argc, char **argv)
{
	struct mark5_stream *vs;
	int bits = 1;
	int fanout = 2;
	long long offset = 0;

	if(argc < 2)
	{
		printf("Usage : %s <infile> [<offset>]\n", argv[0]);
		return 0;
	}

	if(argc > 2)
	{
		offset=atoll(argv[2]);
	}

	vs = mark5_stream_file_open(argv[1], offset, bits, fanout);

	if(!vs)
	{
		printf("problem opening %s\n", argv[1]);
		return 0;
	}

	mark5_stream_print(vs);

	mark5_stream_close(vs);

	return 0;
}

