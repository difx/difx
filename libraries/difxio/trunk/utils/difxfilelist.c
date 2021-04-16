/***************************************************************************
 *   Copyright (C) 2021 by Walter Brisken                                  *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "difx_input.h"

const char program[] = "difxfilelist";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20210416";

void printHeader()
{
	printf("# Col 1: Datastream index\n");
	printf("# Col 2: Station name\n");
	printf("# Col 3: File name (with full path)\n");
	printf("# Col 4: Data format\n");
	printf("# Col 5: Quantization bits\n");
	printf("# Col 6: Data frame size\n");
	printf("# Col 7: Number of recorded base band channelss\n");
}

void usage()
{
	printf("%s  ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("Usage : %s [options] <inputfilebase1> [ <inputfilebase2> [...] ]\n\n", program);
	printf("options can include:\n");
	printf("--help\n");
	printf("-h         print help information and quit\n\n");
	printf("--header\n");
	printf("-H         print column headers\n\n");
	printf("<inputfilebaseN> is the base name of a difx fileset.\n\n");
	printf("All normal program output goes to stdout.\n\n");
	printf("This program reads through one or more difx datasets and\n");
	printf("prints a table of files to be read and the expected data format.\n\n");
	printf("The columns in the output (space separated) are:\n");
	printHeader();
	printf("\n");
}

int processInputFile(const char *filebase)
{
	DifxInput *D;
	int d;

	D = loadDifxInput(filebase);

	if(!D)
	{
		fprintf(stderr, "Error: cannot open DiFX fileset %s\n", filebase);

		return -1;
	}

	D = updateDifxInput(D, 0);
	if(!D)
	{
		fprintf(stderr, "Error: updateDifxInput failed for DiFX fileset %s\n", filebase);
		
		return -2;
	}

	DifxInputSortAntennas(D, 0);

	for(d = 0; d < D->nDatastream; ++d)
	{
		const DifxDatastream *ds;
		const char *name;
		int a;
		int f;

		ds = D->datastream + d;
		a = ds->antennaId;
		name = D->antenna[a].name;

		if(ds->dataSource != DataSourceFile)
		{
			continue;
		}

		for(f = 0; f < ds->nFile; ++f)
		{
			printf("%2d %s %s %s %d %d %d\n", d, name, ds->file[f], ds->dataFormat, ds->quantBits, ds->dataFrameSize, ds->nRecBand);
		}
	}

	deleteDifxInput(D);

	return 0;
}

int main(int argc, char **argv)
{
	int a;

	for(a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage();

				exit(EXIT_SUCCESS);
			}
			if(strcmp(argv[a], "-H") == 0 ||
			   strcmp(argv[a], "--header") == 0)
			{
				printHeader();
			}
			else
			{
				fprintf(stderr, "Unknown option %s\n", argv[a]);

				exit(EXIT_FAILURE);
			}
		}
		else
		{
			processInputFile(argv[a]);
		}
	}

	return EXIT_SUCCESS;
}
