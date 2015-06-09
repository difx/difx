#include <stdio.h>
#include <stdlib.h>
#include "vdifmark6.h"

int main(int argc, char **argv)
{
	int a;
	Mark6File F;
	Mark6Descriptor *D;

	for(a = 1; a < argc; ++a)
	{
		int v;
		ssize_t ss;

		v = openMark6File(&F, argv[a]);
		if(v < 0)
		{
			printf("Open of %s failed with return value %d\n", argv[a], v);

			continue;
		}

		printMark6File(&F);

		ss = Mark6FileReadBlock(&F);
		if(ss <= 0)
		{
			printf("Read failed! ss=%d\n", (int)ss);
		}
		else
		{
			printMark6File(&F);
		}

		v = closeMark6File(&F);
		printf("Closed with return value %d\n", v);
	}

	printf("\n\n");

	D = openMark6(argc-1, argv+1);
	printMark6(D);

	/* do a gather operation and write to /tmp/tmp.vdif */
	{
		FILE *out;		

		out = fopen("/tmp/tmp.vdif", "w");
		if(!out)
		{
			fprintf(stderr, "Error, cannot open /tmp/tmp.vdif for write\n");
		}
		else
		{
			int i;
			char *buffer;
			const int bufsize = 40000000;

			buffer = (char *)malloc(bufsize);

			for(i = 0; i < 20; ++i)
			{
				ssize_t s;

				s = readMark6(D, buffer, bufsize);
				
				printf("Read number %d --> %d/%d bytes\n", i, (int)s, bufsize);

				if(s > 0)
				{
					fwrite(buffer, 1, s, out);
				}

				if(s < bufsize)
				{
					break;
				}
			}

			fclose(out);
			free(buffer);
		}
	}

	closeMark6(D);

	return 0;
}
