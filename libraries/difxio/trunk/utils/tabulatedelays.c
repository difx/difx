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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id: testdifxinput.c 5285 2013-05-08 07:14:47Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
 * $LastChangedRevision: 5285 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2013-05-08 01:14:47 -0600 (Wed, 08 May 2013) $
 *
 *==========================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "difx_input.h"

const char program[] = "calculatedelays";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20150622";

void usage()
{
	printf("%s  ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("Usage : %s [options] <inputfilebase1> [ <inputfilebase2> [...] ]\n\n", program);
	printf("options can include:\n");
	printf("--help\n");
	printf("-h         print help information and quit\n\n");
	printf("<inputfilebaseN> is the base name of a difx fileset.\n\n");
	printf("All normal program output goes to stdout.\n\n");
	printf("This program reads through one or more difx datasets and\n");
	printf("evaluates delay polynomials in the .im files on a regular\n");
	printf("time grid (every 24 seconds).  Delays and rates are both\n");
	printf("calculated.  Output should be self explanatory.\n\n");
}

/* Use Cramer's rule to evaluate polynomial */
double evaluatePoly(const double *p, int n, double x)
{
	double y;
	int i;

	if(n == 1)
	{
		return p[0];
	}

	y = p[n-1];

	for(i = n-2; i >= 0; i--)
	{
		y = x*y + p[i];
	}

	return y;
}

double evaluatePolyDeriv(const double *p, int n, double x)
{
	double y;
	int i;

	if(n == 1)
	{
		return 0;
	}

	if(n == 2)
	{
		return p[1];
	}

	y = (n-1)*p[n-1];

	for(i = n-2; i >= 1; i--)
	{
		y = x*y + i*p[i];
	}

	return y;
}

int main(int argc, char **argv)
{
	DifxInput *D = 0;
	int a, s;
	int mergable, compatible;
	int nJob = 0;
	
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
			else
			{
				fprintf(stderr, "Unknown option %s\n", argv[a]);

				exit(EXIT_FAILURE);
			}
		}
		else if(D == 0)
		{
			D = loadDifxInput(argv[a]);
			if(D)
			{
				D->eopMergeMode = EOPMergeModeRelaxed;
			}
		}
		else
		{
			DifxInput *D1, *D2;

			D1 = D;
			D2 = loadDifxInput(argv[a]);
			if(D2)
			{
				D2->eopMergeMode = EOPMergeModeRelaxed;
				mergable = areDifxInputsMergable(D1, D2);
				compatible = areDifxInputsCompatible(D1, D2);
				if(mergable && compatible)
				{
					D = mergeDifxInputs(D1, D2, 0);
					deleteDifxInput(D1);
					deleteDifxInput(D2);
				}
				else
				{
					fprintf(stderr, "cannot merge job %s: mergable=%d compatible=%d\n", argv[a], mergable, compatible);
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
		fprintf(stderr, "Nothing to do!  Quitting.  Run with -h for help information\n");

		return EXIT_SUCCESS;
	}

	D = updateDifxInput(D);
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

	printf("# produced by program %s ver. %s\n\n", program, version);
	printf("# Columns are:\n");
	printf("# 1. mjd [day]\n");
	for(a = 0; a < D->nAntenna; ++a)
	{
		printf("# %d. Antenna %d (%s) delay [us]\n", 2+2*a, a, D->antenna[a].name);
		printf("# %d. Antenna %d (%s) rate [us/s]\n", 3+2*a, a, D->antenna[a].name);
	}

	for(s = 0; s < D->nScan; ++s)
	{
		const DifxScan *ds;
		int refAnt;	/* points to a valid antenna in this poly */
		int p, i;

		ds = D->scan + s;

		printf("\n# scan %d of %d: source = %s\n", s+1, D->nScan, D->source[ds->phsCentreSrcs[0]].name);

		for(refAnt = 0; refAnt < D->nAntenna; ++refAnt)
		{
			if(ds->im[refAnt])
			{
				break;
			}
		}
		if(refAnt >= D->nAntenna)
		{
			/* Huh, no delays for any antennas...? */

			printf("#   No delays\n");

			break;
		}

		for(p = 0; p < ds->nPoly; ++p)
		{
			const int N = (p == ds->nPoly-1) ? 6 : 5;

			for(i = 0; i < N; ++i)
			{
				printf("%14.8f", ds->im[refAnt][0][p].mjd + (ds->im[refAnt][0][p].sec + i*24)/86400.0);

				for(a = 0; a < D->nAntenna; ++a)
				{
					double d, r;

					if(ds->im[a] == 0)
					{
						/* print zeros in cases where there is no data */
						d = r = 0.0;
					}
					else
					{
						d = evaluatePoly(ds->im[a][0][p].delay, ds->im[a][0][p].order+1, 24*i);
						r = evaluatePolyDeriv(ds->im[a][0][p].delay, ds->im[a][0][p].order+1, 24*i);
					}

					/* print to picosecond and picosecond/sec precision */
					printf("   %12.6f %9.6f", d, r);
				}

				printf("\n");
			}
		}
	}

	deleteDifxInput(D);

	printf("\nIt seems %d job(s) tested successfully.\n", nJob);

	return EXIT_SUCCESS;
}
