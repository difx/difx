/***************************************************************************
 *   Copyright (C) 2013-2015 by Walter Brisken                             *
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
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <vdifio.h>

const char program[] = "vsum";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.3";
const char verdate[] = "20150318";

static void usage(const char *pgm)
{
	printf("%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("A utility to summarize the contents of VDIF data files\n\n");
	printf("Usage: %s [<options>] <file1> [<file2> [ ... ] ]\n\n", pgm);
	printf("  <fileX> is the name of a VDIF data file\n\n");
	printf("  <options> can include:\n");
	printf("    -h or --help      print this usage information and quit\n");
	printf("    -s or --shortsum  print a short summary, also usable for input to vex2difx\n");
	printf("\n");
}

int main(int argc, char **argv)
{
	if(argc < 2)
	{
		usage(argv[0]);

		exit(EXIT_FAILURE);
	}
	else
	{
		struct vdif_file_summary sum;
		int a, r;
		int shortsum = 0;

		for(a = 1; a < argc; ++a)
		{
			if(strcmp(argv[a], "-s") == 0 ||
			   strcmp(argv[a], "--shortsum") == 0)
			{
				shortsum = 1;
			}
			else if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage(argv[0]);

				exit(EXIT_SUCCESS);
			}
			else
			{
				r = summarizevdiffile(&sum, argv[a], 0);

				if(r < 0)
				{
					fprintf(stderr, "File %s VDIF summary failed with return value %d\n\n", argv[a], r);
				}
				else if(shortsum)
				{
					double mjd1, mjd2;
					char filename[1000];

					mjd1 = vdiffilesummarygetstartmjd(&sum) + (sum.startSecond % 86400)/86400.0;
					mjd2 = mjd1 + (sum.endSecond - sum.startSecond + 1)/86400.0;

					if(argv[a][0] != '/')
					{
						char path[1000];
						getcwd(path, 1000);
						snprintf(filename, 1000, "%s/%s", path, argv[a]);
					}
					else
					{
						snprintf(filename, 1000, "%s", argv[a]);
					}
					printf("%s %14.8f %14.8f\n", filename, mjd1, mjd2);
				}
				else
				{
					printvdiffilesummary(&sum);
				}
			}
		}
	}

	return 0;
}
