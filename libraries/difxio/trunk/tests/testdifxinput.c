/***************************************************************************
 *   Copyright (C) 2008-2018 by Walter Brisken                             *
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
#include "difxio/difx_input.h"

const char program[] = "testdifxinput";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "1.4";
const char verdate[] = "20180906";

void usage()
{
	printf("%s  ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("Usage : %s [options] <inputfilebase1> [ <inputfilebase2> [...] ]\n\n", program);
	printf("options can include:\n");
	printf("--verbose\n");
	printf("-v         be a bit more verbose\n\n");
	printf("--help\n");
	printf("-h         print help information and quit\n\n");
	printf("--union\n");
	printf("-u         merge even incompatible frequency setups\n\n");
	printf("--eop-strict\n");
	printf("           don't allow merging of jobs with different EOP days\n\n");
	printf("--eop-loose\n");
	printf("           drop EOPs to prevent incompatibility\n\n");
	printf("--eop-relaxed\n");
	printf("           allow different EOPs per file as long as they re consistent (default)\n\n");
	printf("<inputfilebaseN> is the base name of a difx fileset.\n\n");
}

int main(int argc, char **argv)
{
	DifxInput *D = 0;
	int a, n;
	int verbose = 0;
	int nJob = 0;
	DifxMergeOptions mergeOptions;

	resetDifxMergeOptions(&mergeOptions);
	resetDifxInputCompatibilityStatistics();
	mergeOptions.eopMergeMode = EOPMergeModeRelaxed;
	
	for(a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-')
		{
			if(strcmp(argv[a], "-v") == 0 ||
			   strcmp(argv[a], "--verbose") == 0)
			{
				++verbose;
				continue;
			}
			else if(strcmp(argv[a], "-h") == 0 ||
			   strcmp(argv[a], "--help") == 0)
			{
				usage();

				exit(EXIT_SUCCESS);
			}
			else if(strcmp(argv[a], "-u") == 0 ||
			   strcmp(argv[a], "--union") == 0)
			{
				mergeOptions.freqMergeMode = FreqMergeModeUnion;

				continue;
			}
			else if(strcmp(argv[a], "--eop-strict") == 0)
			{
				mergeOptions.eopMergeMode = EOPMergeModeStrict;

				continue;
			}
			else if(strcmp(argv[a], "--eop-loose") == 0)
			{
				mergeOptions.eopMergeMode = EOPMergeModeLoose;

				continue;
			}
			else if(strcmp(argv[a], "--eop-relaxed") == 0)
			{
				mergeOptions.eopMergeMode = EOPMergeModeRelaxed;
			}
			else
			{
				fprintf(stderr, "Unknown option %s\n", argv[a]);

				exit(EXIT_FAILURE);
			}
		}
		else if(D == 0)
		{
			D = loadDifxInput(argv[a]);
		}
		else
		{
			DifxInput *D1, *D2;

			D1 = D;
			D2 = loadDifxInput(argv[a]);
			if(D2)
			{
				if(areDifxInputsCompatible(D1, D2, &mergeOptions))
				{
					D = mergeDifxInputs(D1, D2, &mergeOptions);
					deleteDifxInput(D1);
					deleteDifxInput(D2);
				}
				else
				{
					printf("cannot merge job %s\n", argv[a]);
					deleteDifxInput(D1);
					deleteDifxInput(D2);
					D = 0;
				}
			}
			else
			{
				deleteDifxInput(D);
				D = 0;
			}
		}
		if(!D)
		{
			fprintf(stderr, "File %s -> D == 0.  Quitting\n", argv[a]);

			return EXIT_FAILURE;
		}
		else
		{
			++nJob;
		}
	}

	if(nJob == 0)
	{
		printf("Nothing to do!  Quitting.  Run with -h for help information\n");

		return EXIT_SUCCESS;
	}

	D = updateDifxInput(D, &mergeOptions);
	if(!D)
	{
		fprintf(stderr, "Update failed: D == 0.  Quitting\n");
		
		return EXIT_FAILURE;
	}

	strcpy(D->job->inputFile, "input.test");
	strcpy(D->job->calcFile, "calc.test");
	strcpy(D->job->threadsFile, "threads.test");
	strcpy(D->job->imFile, "im.test");
	strcpy(D->job->outputFile, "output.test");

	printDifxInput(D);

	writeDifxCalc(D);
	writeDifxInput(D);
	writeDifxIM(D);

	deleteDifxInput(D);

	printf("\n");
	n = printDifxInputCompatibilityStatistics(verbose);
	if(n > 0)
	{
		printf("\n");
	}

	printf("It seems %d job(s) tested successfully.\n", nJob);

	return EXIT_SUCCESS;
}
