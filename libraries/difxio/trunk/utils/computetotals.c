/***************************************************************************
 *   Copyright (C) 2015-2017 by Walter Brisken                             *
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

const char program[] = "computetotals";
const char author[]  = "Walter Brisken <wbrisken@lbo.us>";
const char version[] = "0.1";
const char verdate[] = "20170413";

void usage()
{
	printf("%s  ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("Usage : %s [options] <residualdelayfile> <inputfilebase1> [ <inputfilebase2> [...] ]\n\n", program);
	printf("options can include:\n");
	printf("--help\n");
	printf("-h         print help information and quit\n\n");
	printf("<residualdelayfile> is the base name of file produced by convertdelay.\n\n");
	printf("<inputfilebaseN> is the base name of a difx fileset.\n\n");
	printf("All normal program output goes to stdout.\n\n");
	printf("This program reads through one or more difx datasets and\n");
	printf("evaluates delay polynomials in the .im files at times in the\n");
	printf("residualdelafile and adds the model.\n\n");
}

int main(int argc, char **argv)
{
	DifxInput *D = 0;
	int a;
	DifxMergeOptions mergeOptions;
	const char *residualDelayFile = 0;
	FILE *in;

	resetDifxMergeOptions(&mergeOptions);
	mergeOptions.eopMergeMode = EOPMergeModeRelaxed;
	mergeOptions.freqMergeMode = FreqMergeModeUnion;
	
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
		else if(residualDelayFile == 0)
		{
			residualDelayFile = argv[a];
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
					D = mergeDifxInputs(D1, D2, 0);
					deleteDifxInput(D1);
					deleteDifxInput(D2);
				}
				else
				{
					fprintf(stderr, "cannot merge job %s\n", argv[a]);
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
	}

	if(!D)
	{
		fprintf(stderr, "Nothing to do!  Quitting.  Run with -h for help information\n");

		return EXIT_SUCCESS;
	}

	D = updateDifxInput(D, 0);
	if(!D)
	{
		fprintf(stderr, "Update failed: D == 0.  Quitting\n");
		
		return EXIT_FAILURE;
	}

	DifxInputSortAntennas(D, 0);

	in = fopen(residualDelayFile, "r");
	if(!in)
	{
		fprintf(stderr, "Cannot open %s for read\n", residualDelayFile);

		return EXIT_FAILURE;
	}

	printf("# produced by program %s ver. %s\n\n", program, version);
	printf("# Columns are:\n");
	printf("# 1. mjd [day]\n");
	for(a = 0; a < D->nAntenna; ++a)
	{
		printf("# %d. Antenna %d (%s) model delay [us]\n", 3+3*a, a, D->antenna[a].name);
		printf("# %d. Antenna %d (%s) residual delay [us]\n", 3+3*a, a, D->antenna[a].name);
		printf("# %d. Antenna %d (%s) total delay [us]\n", 3+3*a, a, D->antenna[a].name);
	}

	for(;;)
	{
		const int MaxLineLen = 512;
		char line[MaxLineLen];
		char mjdResidStr[20];
		long double mjdResid, resid[20];
		char *rv;
		int n, antennaId;
		long double delay;
		int intMjd;
		double sec;
		
		line[MaxLineLen-1] = 0;
		rv = fgets(line, MaxLineLen-1, in);

		if(!rv)
		{
			break;
		}

		if(strlen(line) < 10 || line[0] == '#')
		{
			continue;
		}

		n = sscanf(line, "%s%Lf%Lf%Lf%Lf%Lf%Lf%Lf%Lf%Lf%Lf%Lf%Lf%Lf%Lf%Lf%Lf%Lf%Lf%Lf%Lf",
			mjdResidStr,
			resid + 0,  resid + 1,  resid + 2,  resid + 3,  resid + 4,
			resid + 5,  resid + 6,  resid + 7,  resid + 8,  resid + 9,
			resid + 10, resid + 11, resid + 12, resid + 13, resid + 14,
			resid + 15, resid + 16, resid + 17, resid + 18, resid + 19);

		mjdResid = atoll(mjdResidStr);

		intMjd = mjdResid;
		sec = (mjdResid - intMjd)*86400.0L;

		if(n != D->nAntenna + 1)
		{
			continue;
		}

		printf("%s", mjdResidStr);

		for(antennaId = 0; antennaId < D->nAntenna; ++antennaId)
		{
			int ok;

			ok = evaluateDifxInputDelayRate(&delay, 0, D, intMjd, sec, antennaId, 0);
			if(ok)
			{
				printf(" %15.10Lf", delay);
				printf(" %15.10Lf", resid[antennaId]*1000000.0);
				printf(" %15.10Lf", delay + resid[antennaId]*1000000.0);
			}
		}
	}

	fclose(in);

	deleteDifxInput(D);

	return EXIT_SUCCESS;
}
