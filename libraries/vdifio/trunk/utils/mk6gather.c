#include <stdio.h>
#include <stdlib.h>
#include "vdifmark6.h"

int main(int argc, char **argv)
{
	const int GatherSize = 10000000;
	Mark6Gatherer *G;
	char *buf;
	FILE *out;
	int i;

	if(argc != 2)
	{
		printf("Must provide template for files\n");

		exit(EXIT_FAILURE);
	}

	printf("Opening all files matching %s\n", argv[1]);
	G = openMark6GathererFromTemplate(argv[1]);

	seekMark6Gather(G, getMark6GathererFileSize(G)/2);

	out = fopen("gather.out", "w");

	printMark6Gatherer(G);

	buf = (char *)malloc(GatherSize);
	for(i = 0;; ++i)
	{
		int n;

		n = mark6Gather(G, buf, GatherSize);
		if(n <= 0)
		{
			break;
		}
		printf("%d  %d/%d\n", i, n, GatherSize);
		fwrite(buf, 1, n, out);
	}
	free(buf);

	fclose(out);

	closeMark6Gatherer(G);

	return 0;
}
