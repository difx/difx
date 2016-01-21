#include <stdio.h>
#include <stdlib.h>


unsigned int calculateMark5DirSignature(const unsigned char *data, int size)
{
	unsigned int signature;

	signature = 1;

	if(size > 0)
	{
		for(int j = 0; j < size/4; ++j)
		{
			unsigned int x = ((unsigned int *)data)[j] + 1;
			signature = signature ^ x;
		}

		/* prevent a zero signature */
		if(signature == 0)
		{
			signature = 0x55555555;
		}
	}

	return signature;
}

int main(int argc, char **argv)
{
	const int MaxSize = 10000000;
	unsigned char *data;
	unsigned int size;
	FILE *in;

	if(argc != 2)
	{
		fprintf(stderr, "Need filename\n");

		return EXIT_FAILURE;
	}

	in = fopen(argv[1], "r");
	if(!in)
	{
		fprintf(stderr, "File not found\n");
	}

	data = (unsigned char *)malloc(MaxSize);

	size = fread(data, 1, MaxSize, in);
	
	printf("Dir size = %u\n", size);
	printf("Sig = %u\n", calculateMark5DirSignature(data+128, size-128));

	free(data);
	fclose(in);

	return EXIT_SUCCESS;
}
