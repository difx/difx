#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int usage()
{
	printf("phxsel <mode> infile outfile\n");
	printf("<mode> is:\n");
	printf("  s    -- select\n");
	printf("  e    -- expand\n");

	return 0;
}

int main(int argc, char **argv)
{
	char mode;	/* 's'elect or 'e'xpand */
	int in, out;
	unsigned char inbuf[1024], outbuf[2048];
	int n, m, i, j;

	if(argc != 4)
	{
		return usage();
	}

	mode = argv[1][0];
	if(mode != 's' && mode != 'e')
	{
		return usage();
	}

	in = open64(argv[2], O_RDONLY | O_LARGEFILE);
	if(in <= 0)
	{
		printf("Cannot open %s for read\n", argv[2]);
		return 0;
	}
	out = open64(argv[3], O_WRONLY | O_CREAT | O_LARGEFILE, S_IRUSR | S_IWUSR);
	if(!out)
	{
		close(in);
		printf("Cannot open %s for write\n", argv[3]);
		return 0;
	}

	for(j = 0;; j++)
	{
		n = read(in, inbuf, 1024);
		if(n == 0)
		{
			break;
		}

		if(mode == 's')
		{
			m = n/2;
			for(i = 0; i < n; i+=2)
			{
				outbuf[i/2] = (inbuf[i] & 0x0F) |
					    ( (inbuf[i+1] & 0x0F) << 4);
			}
		}
		else
		{
			m = n*2;
			for(i = 0; i < n; i++)
			{
				outbuf[2*i]   = (inbuf[i] & 0x0F) * 0x11;
				outbuf[2*i+1] = ((inbuf[i] & 0xF0) >> 4) * 0x11;
			}
		}
		write(out, outbuf, m);
	}

	printf("%d kiB input converted\n", j);

	close(in);
	close(out);

	return 0;
}
