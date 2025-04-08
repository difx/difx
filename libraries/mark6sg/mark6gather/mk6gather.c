/***************************************************************************
 *   Copyright (C) 2015-2023 by Walter Brisken                             *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "mark6gather.h"

const char program[] = "mk6gather";
const char version[] = "1.6";
const char verdate[] = "20230623";

const char defaultOutfile[] = "gather.out";

void usage(const char *prog)
{
	fprintf(stderr, "%s ver. %s  %s\n\n", program, version, verdate);
	fprintf(stderr, "Usage: %s [options] <fileset template>\n\n", prog);
	fprintf(stderr, "options can include:\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h             print this help info and quit.\n\n");
	fprintf(stderr, "  --verbose\n");
	fprintf(stderr, "  -v             be more verbose.  -v -v for even more.\n\n");
	fprintf(stderr, "  --output <outfile>\n");
	fprintf(stderr, "  -o <outfile>   send output to <outfile> [default: %s].\n\n", defaultOutfile);
	fprintf(stderr, "  --bytes <bytes>\n");
	fprintf(stderr, "  -b <bytes>     stop process after <bytes> have been copied\n\n");
	fprintf(stderr, "  --skip <bytes>\n");
	fprintf(stderr, "  -s <bytes>     skip <bytes> at start of file\n\n");
	fprintf(stderr, "  --append\n");
	fprintf(stderr, "  -a             append to existing file; this continues a previous gather\n\n");
	fprintf(stderr, "<fileset template> is a glob expression selecting Mark6 files to.\n");
	fprintf(stderr, "read.  This is usually the name of the scan given to the recorder,\n");
	fprintf(stderr, "e.g., BB407A_LA_No0001\n\n");
	fprintf(stderr, "<outfile> can be set to - to indicate stdout.\n\n");
}

int main(int argc, char **argv)
{
	const int GatherSize = 10000000;
	Mark6Gatherer *G;
	char *buf;
	FILE *out;
	int i;
	int a;
	const char *outfile = defaultOutfile;
	const char *fileset = 0;
	int verbose = 0;
	int doAppend = 0;
	long long int skipBytes = 0;
	long long int maxBytes = 0;
	long long int bytesWritten = 0;

	if(argc < 2)
	{
		usage(argv[0]);

		exit(EXIT_FAILURE);
	}

	for(a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-h") == 0 || strcmp(argv[a], "--help") == 0)
			{
				usage(argv[0]);

				exit(EXIT_SUCCESS);
			}
			else if(strcmp(argv[a], "-v") == 0 || strcmp(argv[a], "--verbose") == 0)
			{
				++verbose;
			}
			else if(strcmp(argv[a], "-a") == 0 || strcmp(argv[a], "--append") == 0)
			{
				doAppend = 1;
			}
			else if(a < argc-1)
			{
				if(strcmp(argv[a], "-o") == 0 || strcmp(argv[a], "--outfile") == 0)
				{
					outfile = argv[a+1];
				}
				else if(strcmp(argv[a], "-b") == 0 || strcmp(argv[a], "--bytes") == 0)
				{
					maxBytes = atoll(argv[a+1]);
				}
				else if(strcmp(argv[a], "-s") == 0 || strcmp(argv[a], "--skip") == 0)
				{
					skipBytes = atoll(argv[a+1]);
				}
				else
				{
					fprintf(stderr, "Error: unknown command line parameter %s\n", argv[a]);

					exit(EXIT_FAILURE);
				}
				++a;
			}
			else
			{
				fprintf(stderr, "Error: unknown command line parameter %s\n", argv[a]);

				exit(EXIT_FAILURE);
			}
		}
		else if(fileset == 0)
		{
			fileset = argv[a];
		}
		else
		{
			fprintf(stderr, "Error: exactly one fileset should be provided.  Not sure what to do with argument: %s.\n", argv[a]);

			exit(EXIT_FAILURE);
		}
	}

	if(doAppend && skipBytes > 0)
	{
		fprintf(stderr, "Error: --skip cannot be used with --append\n");

		exit(EXIT_FAILURE);
	}

	if(doAppend && strcmp(outfile, "-") == 0)
	{
		fprintf(stderr, "Error: --append cannot be used when redirecting output to stdout\n");

		exit(EXIT_FAILURE);
	}

	if(verbose > 1 && strcmp(outfile, "-") == 0)
	{
		fprintf(stderr, "When streaming to stdout, verbosity level > 1 is not allowed.  Reducing verbosity.\n");
		verbose = 1;
	}

	if(fileset == 0)
	{
		fprintf(stderr, "Error: no fileset was provided\n");

		exit(EXIT_FAILURE);
	}

	if(verbose > 0)
	{
		fprintf(stderr, "Opening all files matching %s\n", fileset);
	}
	G = openMark6GathererFromTemplate(fileset);

	if(verbose > 0)
	{
		fprintf(stderr, "Gathered file size %Ld\n", (long long int)(getMark6GathererFileSize(G)));
	}

	if(strcmp(outfile, "-") == 0)
	{
		out = stdout;
	}
	else
	{
		if(doAppend)
		{
			struct stat st;
			int ok;

			ok = stat(outfile, &st);
			if(ok == 0) /* file already exists */
			{
				off_t size;
				int chunkSize;	/* excludes header -- just pure VDIF data */
				const Mark6File *m6f;

				m6f = G->mk6Files;
				
				chunkSize = m6f->maxBlockSize - ((m6f->maxBlockSize-m6f->blockHeaderSize) % m6f->packetSize) - m6f->blockHeaderSize;

				size = st.st_size;

				/* truncate file if not multiple of Mark6 block size */
				if(size % chunkSize != 0)
				{
					int rv;

fprintf(stderr, "Truncating existing file from %Ld to %Ld bytes\n", (long long int)(size), (long long int)(size - (size % chunkSize)));
					size -= (size % chunkSize);

					rv = truncate(outfile, size);
					if(rv != 0)
					{
						fprintf(stderr, "Error: call to truncate(%s, %Ld) resulted in: %s\n", outfile, (long long)size, strerror(errno));

						exit(EXIT_FAILURE);
					}
				}
				skipBytes = size;
				out = fopen(outfile, "a");
			}
			else	/* new file */
			{
				out = fopen(outfile, "w");
			}
		}
		else
		{
			out = fopen(outfile, "w");
		}

		if(!out)
		{
			fprintf(stderr, "Error: cannot open %s for write\n", outfile);

			exit(EXIT_FAILURE);
		}
	}

	if(skipBytes > 0)
	{
		if(verbose > 0)
		{
			fprintf(stderr, "Skipping %Ld bytes into file\n", skipBytes);
		}
		seekMark6Gather(G, skipBytes);
	}

	if(verbose > 1)
	{
		printMark6Gatherer(G);
	}

	buf = (char *)malloc(GatherSize);
	for(i = 0;; ++i)
	{
		int n;

		n = mark6Gather(G, buf, GatherSize);
		if(n <= 0)
		{
			if(n != 0)
			{
				fprintf(stderr, "Error: mark6Gather() returned %d (i = %d)\n", n, i);
			}
			break;
		}

		if(maxBytes > 0 && bytesWritten + n > maxBytes)
		{
			n = maxBytes - bytesWritten;
		}

		if(verbose > 0)
		{
			fprintf(stderr, "%d  %d/%d  %Ld/%Ld\n", i, n, GatherSize, bytesWritten + n, maxBytes);
		}
		fwrite(buf, 1, n, out);

		bytesWritten += n;

		if(maxBytes > 0 && bytesWritten >= maxBytes)
		{
			break;
		}
	}
	free(buf);

	if(out != stdout)
	{
		fclose(out);
	}

	closeMark6Gatherer(G);

	return 0;
}
