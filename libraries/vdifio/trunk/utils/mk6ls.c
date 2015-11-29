/***************************************************************************
 *   Copyright (C) 2015 by Walter Brisken                                  *
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
#include <vdifmark6.h>

const char program[] = "mk6ls";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.2";
const char verdate[] = "20151129";

void usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s  %s  %s\n\n", program, version, author, verdate);
	fprintf(stderr, "Usage: %s [options]\n\n", pgm);
	fprintf(stderr, "Options can include:\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h        Print this help info and quit\n\n");
	fprintf(stderr, "  --short\n");
	fprintf(stderr, "  -s        Print short form output [default]\n\n");
	fprintf(stderr, "  --long\n");
	fprintf(stderr, "  -l        Print long form output\n\n");
	fprintf(stderr, "  --full\n");
	fprintf(stderr, "  -f        Print full information for each file\n\n");
}

int main(int argc, char **argv)
{
	char **fileList;
	int n;
	int a;
	int longPrint = 0;
	int fullPrint = 0;

	for(a = 1; a < argc; ++a)
	{
		if(strcmp(argv[a], "-l") == 0 || strcmp(argv[a], "--long") == 0)
		{
			longPrint = 1;
			fullPrint = 0;
		}
		else if(strcmp(argv[a], "-f") == 0 || strcmp(argv[a], "--full") == 0)
		{
			longPrint = 0;
			fullPrint = 1;
		}
		else if(strcmp(argv[a], "-s") == 0 || strcmp(argv[a], "--short") == 0)
		{
			longPrint = 0;
			fullPrint = 0;
		}
		else if(strcmp(argv[a], "-h") == 0 || strcmp(argv[a], "--help") == 0)
		{
			usage(argv[0]);

			return EXIT_SUCCESS;
		}
	}

	n = getMark6FileList(&fileList);

	if(n == 0)
	{
		printf("No Mark6 files found in %s\n", getMark6Root());
	}
	else
	{
		int i;

		for(i = 0; i < n; ++i)
		{
			if(fullPrint)
			{
				Mark6Gatherer *G;
				long long int size;
				int f;

				G = openMark6GathererFromTemplate(fileList[i]);
				if(G)
				{
					size = getMark6GathererFileSize(G);
					printf("%s   %d  %lld  %s\n", fileList[i], G->nFile, size, isMark6GatherComplete(G) ? "(complete)" : "(incomplete)");
					for(f = 0; f < G->nFile; ++f)
					{
						printMark6File(&(G->mk6Files[f]));
					}
					closeMark6Gatherer(G);
				}
				else
				{
					printf("%s  (invalid Mark6 file)\n", fileList[i]);
				}
			}
			else if(longPrint)
			{
				Mark6Gatherer *G;
				long long int size;

				G = openMark6GathererFromTemplate(fileList[i]);
				if(G)
				{
					size = getMark6GathererFileSize(G);
					printf("%s   %d  %lld  %s\n", fileList[i], G->nFile, size, isMark6GatherComplete(G) ? "(complete)" : "(incomplete)");
					closeMark6Gatherer(G);
				}
				else
				{
					printf("%s  (invalid Mark6 file)\n", fileList[i]);
				}
			}
			else
			{
				printf("%s\n", fileList[i]);
			}
		}
	
		for(i = 0; i < n; ++i)
		{
			free(fileList[i]);
		}
		free(fileList);
	}


	return 0;
}
