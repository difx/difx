/***************************************************************************
 *   Copyright (C) 2012 by Walter Brisken                                  *
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
// $Id$
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/examples/m5pcal.c $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "findvdif";
const char author[]  = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "20120418";

const size_t bufferLength = 2000000;

static void usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("Usage: %s [options] <infile> [<framesize>]\n\n", program);
	printf("  <infile> is the name of the input file\n\n");
	printf("  <framesize> is the full VDIF frame size to search for.\n");
	printf("      If not provided, legal frame sizes up to 9000 bytes will be tried.\n\n");
	printf("Options can include:\n\n");
	printf("  --verbose\n");
	printf("  -v           Be more verbose in operation\n\n");
	printf("  --quiet\n");
	printf("  -q           Be quieter\n\n");
	printf("  --help\n");
	printf("  -h           Print this help info and quit\n\n");
}


int main(int argc, char **argv)
{
	int verbose = 0;
	const char *inFile = 0;
	int framesize = 0;
	int i;
	size_t n;
	unsigned char *buffer;
	FILE *in;
	struct mark5_format *mf;

	for(i = 1; i < argc; i++)
	{
		if(argv[i][0] == '-')
		{
			if(strcmp(argv[i], "--verbose") == 0 ||
				strcmp(argv[i], "-v") == 0)
			{
				verbose++;
			}
			else if(strcmp(argv[i], "--quiet") == 0 ||
				strcmp(argv[i], "-q") == 0)
			{
				verbose--;
			}
			else if(strcmp(argv[i], "--help") == 0 ||
				strcmp(argv[i], "-h") == 0)
			{
				usage(argv[0]);

				return EXIT_SUCCESS;
			}
			else
			{
				fprintf(stderr, "I'm not sure what to do with command line argument '%s'\n", argv[i]);

				return EXIT_FAILURE;
			}

		}
		else if(inFile == 0)
		{
			inFile = argv[i];
		}
		else if(framesize == 0)
		{
			framesize = atoi(argv[i]);
		}
		else
		{
			fprintf(stderr, "I'm not sure what to do with command line argument '%s'\n",
				argv[i]);


			return EXIT_FAILURE;
		}

	}

	if(!inFile)
	{
		usage(argv[0]);

		return EXIT_FAILURE;
	}

	in = fopen(inFile, "r");
	if(!in)
	{
		fprintf(stderr, "Error: cannot open %s for read\n", inFile);

		return EXIT_FAILURE;
	}

	buffer = (unsigned char *)malloc(bufferLength);

	n = fread(buffer, 1, bufferLength, in);

	fclose(in);

	if(n < 100)
	{
		printf("File size is too small (%d)\n", (int)n);

	}
	else if(n < framesize+32)
	{
		printf("Not enough data loaded (%d) to search for frames of lenth %d\n", (int)n, framesize);
	}
	else
	{
		size_t offset;

		i = find_vdif_frame(buffer, n, &offset, &framesize);

		if(i < 0)
		{
			printf("No VDIF frames found\n");
		}
		else
		{
			printf("VDIF found: offset = %d bytes, framesize = %d bytes\n", (int)offset, framesize);
		}
	}

	printf("Testing memory stream:\n");
	mf = new_mark5_format_from_stream(new_mark5_stream_memory(buffer, bufferLength));
	print_mark5_format(mf);
	delete_mark5_format(mf);

	free(buffer);

	printf("Testing file stream:\n");
	mf = new_mark5_format_from_stream(new_mark5_stream_file(inFile, 0));
	print_mark5_format(mf);
	delete_mark5_format(mf);


	return EXIT_SUCCESS;
}

