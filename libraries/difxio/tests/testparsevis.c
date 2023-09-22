/***************************************************************************
 *   Copyright (C) 2007-2011 by Walter Brisken                             *
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
#include <stdlib.h>
#include "difxio/parsevis.h"

const char program[] = "difxfringe";
const char version[] = "0.1";
const char verdate[] = "20111129";
const char author[]  = "Walter Brisken";

int usage(const char *pgm)
{
	fprintf(stderr, "%s ver. %s   %s %s\n\n", program, version, author, verdate);
	fprintf(stderr, "usage : %s <difx file> <nchan> [<baseline>]\n\n", pgm);

	return 0;
}

int fringe(const char *filename, int nchan, int baseline, const char *pol)
{
	const char outFilename[] = "vis.out";
	DifxVisRecord *vis;
	int i, k;
	int v;

	FILE *out;

	/* suppress warnings */
	baseline = 0;
	pol = 0;

	out = fopen(outFilename, "w");
	if(!out)
	{
		fprintf(stderr, "Error opening %s for write\n", outFilename);
	}

	vis = newDifxVisRecord(filename, nchan);

	if(!vis)
	{
		fprintf(stderr, "fringe : vis = 0\n");
		fclose(out);

		return -1;
	}

	for(i = 0; i < 3000; i++)
	{
		printf("i = %d\n", i);
		v = DifxVisRecordgetnext(vis);
		if(v < 0)
		{
			break;
		}
		printDifxParameters(vis->params);
		printf("%f %f # QQQ\n", creal(vis->visdata[8]), cimag(vis->visdata[8]));
		for(k = 0; k < nchan; k++)
		{
			fprintf(out, "%d %d %f %f\n", i, k, creal(vis->visdata[k]), cimag(vis->visdata[k]) );
		}
	}

	fclose(out);

	deleteDifxVisRecord(vis);

	return 0;
}

int main(int argc, char **argv)
{
	int nchan;
	int baseline = 0;
	const char *pol = 0;

	if(argc < 3)
	{
		return usage(argv[0]);
	}

	nchan = atoi(argv[2]);
	if(nchan < 1 || nchan > 1<<21)
	{
		fprintf(stderr, "nchan out of range\n");

		return EXIT_FAILURE;
	}

	if(argc > 3)
	{
		baseline = atoi(argv[3]);
		if(baseline < 0)
		{
			fprintf(stderr, "baseline out of range\n");

			return EXIT_FAILURE;
		}	
	}

	if(argc > 4)
	{
		pol = argv[4];
	}

	fringe(argv[1], nchan, baseline, pol);

	return EXIT_SUCCESS;
}
