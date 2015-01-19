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
 * $Id: $
 * $HeadURL: $
 * $LastChangedRevision: $
 * $Author: $
 * $LastChangedDate: $
 *
 *==========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <complex.h>
#include <time.h>
#include "difx_input.h"

#define MAX_ANTENNAS 40
#define C_LIGHT	299792458.0

const char program[] = "calcderiv";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const int version = 1;	/* for this program, must be an integer */
const char verdate[] = "20150115";

void usage()
{
	printf("%s  ver. %d  %s  %s\n\n", program, version, author, verdate);
	printf("Usage : %s [options]  <inputfilebase1> [ <inputfilebase2> [ ... ] ]\n\n", program);
	printf("options can include:\n");
	printf("--verbose\n");
	printf("-v         be a bit more verbose\n\n");
	printf("--help\n");
	printf("-h         print help information and quit\n\n");
	printf("<inputfilebaseN> is the base name of a difx fileset.\n\n");
}

int computeLMDerivatives(DifxInput *D, double deltaLM, int verbose)
{
	const int CommandLength = 1024;
	const int NumCalcRuns = 9;
	const int ops[9][2] = 
	{
		{  0,  0 },
		{  1,  0 },
		{ -1,  0 },
		{  0,  1 },
		{  0, -1 },
		{  1,  1 },
		{  1, -1 },
		{ -1,  1 },
		{ -1, -1 }
	};

	DifxScan *scan;
	int scanId;
	char command[CommandLength];
	DifxInput *DD[NumCalcRuns];
	int i;

	for(i = 0; i < NumCalcRuns; ++i)
	{
		int s;

		/* nudge the sources */
		for(s = 0; s < D->nSource; ++s)
		{
			DifxSource *source;

			source = D->source + s;
			source->ra += ops[i][0]*deltaLM*cos(source->dec);
			source->dec += ops[i][1]*deltaLM;
		}

		/* write new .calc file */
		writeDifxCalc(D);

		/* run calc11 */
		snprintf(command, CommandLength, "rm %s", D->job->imFile);
		system(command);
		snprintf(command, CommandLength, "difxcalc %s", D->job->calcFile);
		system(command);
		/* FIXME: vex2difx should put calc version info in .calc file */
		/* FIXME: option for calc 9 */

		/* make copes for inspection */
		snprintf(command, CommandLength, "cp %s %s.%02d\n", D->job->calcFile, D->job->calcFile, i);
		system(command);
		snprintf(command, CommandLength, "cp %s %s.%02d\n", D->job->imFile, D->job->imFile, i);
		system(command);

		/* load the updated version */
		DD[i] = loadDifxInput(D->job->inputFile);
		if(!DD[i])
		{
			fprintf(stderr, "Error: cannot open fileset %s number %d\n", D->job->inputFile, i);
			
			return -1;
		}
		DD[i] = updateDifxInput(DD[i]);
		if(!DD[i])
		{
			fprintf(stderr, "Update failed for fileset %s number %d\n", D->job->inputFile, i);
			
			return -2;
		}

		/* undo the motion */
		for(s = 0; s < D->nSource; ++s)
		{
			DifxSource *source;

			source = D->source + s;
			source->dec -= ops[i][1]*deltaLM;
			source->ra -= ops[i][0]*deltaLM*cos(source->dec);
		}
	}

	/* Now take the new info and compute the derivatives */
	for(scanId = 0; scanId < D->nScan; ++scanId)
	{
		int a, c, p;

		scan = D->scan + scanId;
		scan->imLM = newDifxPolyModelLMExtensionArray(scan->nAntenna, scan->nPhaseCentres + 1, scan->nPoly);

		for(a = 0; a < scan->nAntenna; ++a)
		{
			for(c = 0; c <= scan->nPhaseCentres; ++c)
			{
				for(p = 0; p < scan->nPoly; ++p)
				{
					double *d[NumCalcRuns];

					scan->imLM[a][c][p].delta = deltaLM;

					for(i = 0; i < NumCalcRuns; ++i)
					{
						d[i] = DD[i]->scan[scanId].im[a][c][p].delay;
					}
					for(i = 0; i <= D->job->polyOrder; ++i)
					{
						scan->imLM[a][c][p].dDelay_dl[i] = (d[1][i]-d[2][i])/(2.0*deltaLM);
						scan->imLM[a][c][p].dDelay_dm[i] = (d[3][i]-d[4][i])/(2.0*deltaLM);

						scan->imLM[a][c][p].d2Delay_dldl[i] = (d[1][i]+d[2][i]-2.0*d[0][i])/(deltaLM*deltaLM);
						scan->imLM[a][c][p].d2Delay_dmdm[i] = (d[3][i]+d[4][i]-2.0*d[0][i])/(deltaLM*deltaLM);

						scan->imLM[a][c][p].d2Delay_dldm[i] = (d[5][i]-d[6][i]-d[7][i]+d[8][i])/(deltaLM*deltaLM);
					}
				}
			}
		}
	}

	/* Write the updated .im file */
	writeDifxIM(D);

	/* Clean up intermediate data */
	for(i = 0; i < NumCalcRuns; ++i)
	{
		deleteDifxInput(DD[i]);
	}

	return 0;
}

int computeXYZDerivatives(DifxInput *D, double deltaXYZ, int verbose)
{
	const int CommandLength = 1024;
	const int NumCalcRuns = 19;
	const int ops[19][3] = 
	{
		{  0,  0,  0 },
		{  1,  0,  0 },
		{ -1,  0,  0 },
		{  0,  1,  0 },
		{  0, -1,  0 },
		{  0,  0,  1 },
		{  0,  0, -1 },
		{  1,  1,  0 },
		{  1, -1,  0 },
		{ -1,  1,  0 },
		{ -1, -1,  0 },
		{  1,  0,  1 },
		{  1,  0, -1 },
		{ -1,  0,  1 },
		{ -1,  0, -1 },
		{  0,  1,  1 },
		{  0,  1, -1 },
		{  0, -1,  1 },
		{  0, -1, -1 }
	};

	DifxScan *scan;
	int scanId;
	char command[CommandLength];
	DifxInput *DD[NumCalcRuns];
	int i;

	for(i = 0; i < NumCalcRuns; ++i)
	{
		int s;

		/* nudge the spacecraft */
		for(s = 0; s < D->nSpacecraft; ++s)
		{
			DifxSpacecraft *sc;
			int p;

			sc = D->spacecraft + s;

			for(p = 0; p < sc->nPoint; ++p)
			{
				sc->pos[p].X += ops[i][0]*deltaXYZ;
				sc->pos[p].Y += ops[i][1]*deltaXYZ;
				sc->pos[p].Z += ops[i][2]*deltaXYZ;
			}
		}

		/* write new .calc file */
		writeDifxCalc(D);

		/* run calc11 */
		snprintf(command, CommandLength, "rm %s", D->job->imFile);
		system(command);
		snprintf(command, CommandLength, "difxcalc %s", D->job->calcFile);
		system(command);
		/* FIXME: vex2difx should put calc version info in .calc file */
		/* FIXME: option for calc 9 */

		/* make copes for inspection */
		snprintf(command, CommandLength, "cp %s %s.%02d\n", D->job->calcFile, D->job->calcFile, i);
		system(command);
		snprintf(command, CommandLength, "cp %s %s.%02d\n", D->job->imFile, D->job->imFile, i);
		system(command);

		/* load the updated version */
		DD[i] = loadDifxInput(D->job->inputFile);
		if(!DD[i])
		{
			fprintf(stderr, "Error: cannot open fileset %s number %d\n", D->job->inputFile, i);
			
			return -1;
		}
		DD[i] = updateDifxInput(DD[i]);
		if(!DD[i])
		{
			fprintf(stderr, "Update failed for fileset %s number %d\n", D->job->inputFile, i);
			
			return -2;
		}

		/* undo the motion */
		for(s = 0; s < D->nSpacecraft; ++s)
		{
			DifxSpacecraft *sc;
			int p;

			sc = D->spacecraft + s;

			for(p = 0; p < sc->nPoint; ++p)
			{
				sc->pos[p].X -= ops[i][0]*deltaXYZ;
				sc->pos[p].Y -= ops[i][1]*deltaXYZ;
				sc->pos[p].Z -= ops[i][2]*deltaXYZ;
			}
		}
	}

	/* Now take the new info and compute the derivatives */
	for(scanId = 0; scanId < D->nScan; ++scanId)
	{
		int a, c, p;

		scan = D->scan + scanId;
		scan->imXYZ = newDifxPolyModelXYZExtensionArray(scan->nAntenna, scan->nPhaseCentres + 1, scan->nPoly);

		for(a = 0; a < scan->nAntenna; ++a)
		{
			for(c = 0; c <= scan->nPhaseCentres; ++c)
			{
				for(p = 0; p < scan->nPoly; ++p)
				{
					double *d[NumCalcRuns];

					scan->imXYZ[a][c][p].delta = deltaXYZ;

					for(i = 0; i < NumCalcRuns; ++i)
					{
						d[i] = DD[i]->scan[scanId].im[a][c][p].delay;
					}
					for(i = 0; i <= D->job->polyOrder; ++i)
					{
						scan->imXYZ[a][c][p].dDelay_dX[i] = (d[1][i]-d[2][i])/(2.0*deltaXYZ);
						scan->imXYZ[a][c][p].dDelay_dY[i] = (d[3][i]-d[4][i])/(2.0*deltaXYZ);
						scan->imXYZ[a][c][p].dDelay_dZ[i] = (d[5][i]-d[6][i])/(2.0*deltaXYZ);

						scan->imXYZ[a][c][p].d2Delay_dXdX[i] = (d[1][i]+d[2][i]-2.0*d[0][i])/(deltaXYZ*deltaXYZ);
						scan->imXYZ[a][c][p].d2Delay_dYdY[i] = (d[3][i]+d[4][i]-2.0*d[0][i])/(deltaXYZ*deltaXYZ);
						scan->imXYZ[a][c][p].d2Delay_dZdZ[i] = (d[5][i]+d[6][i]-2.0*d[0][i])/(deltaXYZ*deltaXYZ);

						scan->imXYZ[a][c][p].d2Delay_dXdY[i] = (d[7][i] -d[8][i] -d[9][i] +d[10][i])/(deltaXYZ*deltaXYZ);
						scan->imXYZ[a][c][p].d2Delay_dXdZ[i] = (d[11][i]-d[12][i]-d[13][i]+d[14][i])/(deltaXYZ*deltaXYZ);
						scan->imXYZ[a][c][p].d2Delay_dYdZ[i] = (d[15][i]-d[16][i]-d[17][i]+d[18][i])/(deltaXYZ*deltaXYZ);
					}
				}
			}
		}
	}

	/* Write the updated .im file */
	writeDifxIM(D);

	/* Clean up intermediate data */
	for(i = 0; i < NumCalcRuns; ++i)
	{
		deleteDifxInput(DD[i]);
	}

	return 0;
}

int run(const char *fileBase, int verbose, double deltaLM, double deltaXYZ)
{
	const int CommandLength = 1024;
	DifxInput *D;
	DifxSource *source;
	DifxScan *scan;
	int sourceId, scanId, pc;
	int nLM = 0;
	int nXYZ = 0;
	char command[CommandLength];
	int rv;

	/* 0. Run calc9 to get starting point */
	snprintf(command, CommandLength, "calcif2 -f %s", fileBase);
	printf("Executing command: %s\n", command);
	system(command);

	/* 1. load fileset */
	D = loadDifxInput(fileBase);
	if(!D)
	{
		fprintf(stderr, "Error: cannot open fileset named %s\n", fileBase);
		
		return -1;
	}
	D = updateDifxInput(D);
	if(!D)
	{
		fprintf(stderr, "Update failed for fileset named %s: D == 0.  Quitting\n", fileBase);
		
		return -2;
	}

	/* 2. Test for type.  Spacecraft table implies XYZ, otherwise LM */
	for(scanId = 0; scanId < D->nScan; ++scanId)
	{
		scan = D->scan + scanId;
		for(pc = 0; pc < scan->nPhaseCentres; ++pc)
		{
			sourceId = D->scan->phsCentreSrcs[pc];
			if(sourceId < 0 || sourceId > D->nSource)
			{
				fprintf(stderr, "Error: source number %d out of range for scan %d of fileset named %s\n", sourceId, scanId, fileBase);
				deleteDifxInput(D);

				return -3;
			}
			source = D->source + sourceId;
			if(source->spacecraftId >= 0)
			{
				++nXYZ;
			}
			else
			{
				++nLM;
			}
		}
	}
	if(nXYZ == 0 && nLM == 0)
	{
		fprintf(stderr, "No sources found for fileset named %s\n", fileBase);
		deleteDifxInput(D);

		return -4;
	}
	if(nXYZ > 0 && nLM > 0)
	{
		fprintf(stderr, "Error: both XYZ and LM sources found in fileset named %s.  Cannot cope.\n", fileBase);
		deleteDifxInput(D);

		return -5;
	}

	/* 3. back up original .calc file and save copy of original .im file */
	snprintf(command, CommandLength, "cp %s %s.bak", D->job->calcFile, D->job->calcFile);
	system(command);
	snprintf(command, CommandLength, "cp %s %s.save", D->job->imFile, D->job->imFile);
	system(command);

	/* 4. Do the calculations */
	if(nLM > 0)
	{
		rv = computeLMDerivatives(D, deltaLM, verbose);
	}
	else
	{
		rv = computeXYZDerivatives(D, deltaXYZ, verbose);
	}

	/* 5. Restore the .calc file */
	snprintf(command, CommandLength, "mv %s.bak %s", D->job->calcFile, D->job->calcFile);
	system(command);

	/* 6. Write the updated model file */
	if(rv == 0)
	{
		snprintf(D->job->calcServer, DIFXIO_HOSTNAME_LENGTH, program);
		D->job->calcVersion = version;
		D->job->calcProgram = 0;
		writeDifxIM(D);
	}

	/* 7. Clean things up */
	deleteDifxInput(D);
	
	return 0;
}
		

int main(int argc, char **argv)
{
	int a;
	int verbose = 0;
	double deltaLM = 10.0/206265.0;		/* (rad) about 10 arcseconds */
	double deltaXYZ = 100.0;		/* (m) */

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
			else
			{
				fprintf(stderr, "Unknown option %s\n", argv[a]);

				exit(EXIT_FAILURE);
			}
		}
		else
		{
			printf("Running on file %s\n", argv[a]);
			run(argv[a], verbose, deltaLM, deltaXYZ);
		}
	}

	return EXIT_SUCCESS;
}
