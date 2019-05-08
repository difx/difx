/***************************************************************************
 *   Copyright (C) 2015-2019 by Walter Brisken                             *
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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: mk6gather.c 7491 2016-10-30 22:21:45Z WalterBrisken $
// $HeadURL: $
// $LastChangedRevision: 7491 $
// $Author: WalterBrisken $
// $LastChangedDate: 2016-10-30 17:21:45 -0500 (Sun, 30 Oct 2016) $
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mark6gather.h"

const char program[] = "mk6gather";
const char version[] = "1.1";
const char verdate[] = "20190508";

const char defaultOutfile[] = "gather.out";

const void usage(const char *prog)
{
	printf("%s ver. %s  %s\n\n", program, version, verdate);
	printf("Usage: %s [options] <fileset template>\n\n", prog);
	printf("options can include:\n");
	printf("  --help\n");
	printf("  -h             print this help info and quit.\n\n");
	printf("  --verbose\n");
	printf("  -v             be more verbose.  -v -v for even more.\n\n");
	printf("  --output <outfile>\n");
	printf("  -o <outfile>   send output to <outfile> [default: %s].\n\n", defaultOutfile);
	printf("<fileset template> is a glob expression selecting Mark6 files to.\n");
	printf("read.  This is usually the name of the scan given to the recorder,\n");
	printf("e.g., BB407A_LA_No0001\n\n");
	printf("<outfile> can be set to - to indicate stdout.\n\n");
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
			else if(a < argc-1)
			{
				if(strcmp(argv[a], "-o") == 0 || strcmp(argv[a], "--outfile") == 0)
				{
					outfile = argv[a+1];
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

	if(verbose > 1 && strcmp(outfile, "-") == 0)
	{
		fprintf(stderr, "When streaming to stdout, verbosity level > 1 is not allowe.  Reducing verbosity.\n");
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
		fprintf(stderr, "Gathered file size %lld\n", getMark6GathererFileSize(G));
	}

//	seekMark6Gather(G, getMark6GathererFileSize(G)/2);

	if(strcmp(outfile, "-") == 0)
	{
		out = stdout;
	}
	else
	{
		out = fopen(outfile, "w");
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
		if(verbose > 0)
		{
			fprintf(stderr, "%d  %d/%d\n", i, n, GatherSize);
		}
		fwrite(buf, 1, n, out);
	}
	free(buf);

	if(out != stdout)
	{
		fclose(out);
	}

	closeMark6Gatherer(G);

	return 0;
}
