#include <stdio.h>
#include <stdlib.h>
#include <vdifio.h>

#define NSRC	10000000

int main(int argc, char **argv)
{
	unsigned char *src;
	unsigned char *dest;
	FILE *in, *out;
	int n;
	int threads[] = { 0, 640, 256, 896 };
	int nThread = sizeof(threads)/sizeof(int);
	int inputframesize = 5032;
	int nGap = 100;
	int nSort = 5;

	if(argc < 2)
	{
		printf("Give filename\n");

		return 0;
	}

	in = fopen(argv[1], "r");

	if(!in)
	{
		printf("Can't open %s\n", argv[1]);

		return 0;
	}

	src = malloc(NSRC);
	dest = malloc(2*NSRC);	// conservative

	n = fread(src, 1, NSRC, in);

	printf("%d bytes read\n", n);

	fclose(in);

	n = vdifmux(dest, n/(nThread*inputframesize), src, n, inputframesize, 12800, 2, nThread, threads, nSort, nGap);

	out = fopen("vdif.out", "w");
	fwrite(dest, 1, NSRC, out);
	fclose(out);

	return 0;
}
