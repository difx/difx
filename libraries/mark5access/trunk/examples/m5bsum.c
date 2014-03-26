/***************************************************************************
 *   Copyright (C) 2013-2014 by Walter Brisken                             *
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

#include "config.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "../mark5access/mark5bfile.h"

const char program[] = "m5bsum";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20130817";

static void usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("A program to summarize contents of Mark5B files\n\n");
	printf("Usage : %s [<options>] <file1> [<file2> ...]\n\n", pgm);
	printf("  <fileX> is the name of the input file\n\n");
	printf("  <options> can include:\n");
	printf("    -h or --help      print this usage information and quit\n");
	printf("    -f or --fixmjd    use today's date to resolve MJD ambiguity\n");
	printf("    -s or --shortsum  print a short summary, also usable for input to vex2difx\n");
	printf("\n");
}

int main(int argc, char **argv)
{
	const int PathSize = 1000;
	int fixday = 0;
	int shortsum = 0;

	if(argc < 2)
	{
		usage(argv[0]);

		exit(EXIT_FAILURE);
	}
	else
	{
		struct mark5b_file_summary sum;
		int a, r;

		for(a = 1; a < argc; ++a)
		{
			if(strcmp(argv[a], "-f") == 0 ||
			   strcmp(argv[a], "--fixmjd") == 0)
			{
				fixday = 1;
			}
			else if(strcmp(argv[a], "-s") == 0 ||
			   strcmp(argv[a], "--shortsum") == 0)
			{
				fixday = 1;
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
				r = summarizemark5bfile(&sum, argv[a]);

				if(r < 0)
				{
					printf("File %s Mark5B summary failed with return value %d\n\n", argv[a], r);
				}
				else
				{
					if(fixday)
					{
						mark5bfilesummaryfixmjdtoday(&sum);
					}
					if(shortsum)
					{
						double mjd1, mjd2;
						char filename[PathSize];

						mjd1 = sum.startDay + sum.startSecond/86400.0 + sum.startFrame/(86400.0*sum.framesPerSecond);
						mjd2 = sum.endDay + sum.endSecond/86400.0 + sum.endFrame/(86400.0*sum.framesPerSecond);

						if(argv[a][0] != '/')
						{
							char path[PathSize];
							char *rv;
							
							rv = getcwd(path, PathSize);
							if(!rv)
							{
								fprintf(stderr, "Weird: getcwd returned null!  Cannot continue.\n");

								exit(EXIT_FAILURE);
							}
							snprintf(filename, PathSize, "%s/%s", path, argv[a]);
						}
						else
						{
							snprintf(filename, PathSize, "%s", argv[a]);
						}
						printf("%s %14.8f %14.8f\n", filename, mjd1, mjd2);
					}
					else
					{
						printmark5bfilesummary(&sum);
					}
				}
			}
		}
	}

	return 0;
}
