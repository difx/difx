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
 * $Id: difxcalculator.c 1419 2009-08-29 21:37:08Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxio/branches/difx-1.5/utils/difxcalculator.c $
 * $LastChangedRevision: 1419 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2009-08-29 15:37:08 -0600 (Sat, 29 Aug 2009) $
 *
 *==========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <complex.h>
#include <time.h>
#include "difx_input.h"
#include "delayderivatives.h"

#define MAX_ANTENNAS 40
#define C_LIGHT	299792458.0

const char program[] = "fitdelay";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20150114";

void usage()
{
	printf("%s  ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("Usage : %s [options] <residdelayfile> <inputfilebase1> [ <inputfilebase2> [ ... ] ]\n\n", program);
	printf("options can include:\n");
	printf("--verbose\n");
	printf("-v         be a bit more verbose\n\n");
	printf("--help\n");
	printf("-h         print help information and quit\n\n");
	printf("<residdelayfile> is the name of a file containing residual delays (format TBD).\n\n");
	printf("<inputfilebaseN> is the base name of a difx fileset.\n\n");
}

/* goes to DifxInput object and interpolates the partial derivatives in time, filling the DelayDerivatives structure in the end */
double getDelay(const DifxInput *D, int antennaId, int scanId, int pc, int mjd, double sec, DelayDerivatives *dd)
{
	int p;
	DifxPolyModel *im;
	DifxPolyModelXYZExtension *imXYZ;
	double dt, dt2, dt3, dt4;
	double rv;

	im = D->scan[scanId].im[antennaId][pc];
	imXYZ = D->scan[scanId].imXYZ[antennaId][pc];

	for(p = 0; p < D->scan[scanId].nPoly; ++p)
	{
		if(im[p].mjd == mjd && im[p].sec <= sec && im[p].sec+im[p].validDuration >= sec)
		{
			break;
		}
	}

	if(p == D->scan[scanId].nPoly)
	{
		return -1e9;
	}

	dt = sec - im[p].sec;
	dt2 = dt*dt;
	dt3 = dt*dt2;
	dt4 = dt2*dt2;

	dd->modelDelay = (im[p].delay[0] + im[p].delay[1]*dt + im[p].delay[2]*dt2 + im[p].delay[3]*dt3 + im[p].delay[4]*dt4)*0.000001;

	/* And derivatives over space, interpolated in time */
	dd->modelDelayDeriv[0] = (imXYZ[p].dDelay_dX[0] + imXYZ[p].dDelay_dX[1]*dt + imXYZ[p].dDelay_dX[2]*dt2 + imXYZ[p].dDelay_dX[3]*dt3 + imXYZ[p].dDelay_dX[4]*dt4)*0.000001;
	dd->modelDelayDeriv[1] = (imXYZ[p].dDelay_dY[0] + imXYZ[p].dDelay_dY[1]*dt + imXYZ[p].dDelay_dY[2]*dt2 + imXYZ[p].dDelay_dY[3]*dt3 + imXYZ[p].dDelay_dY[4]*dt4)*0.000001;
	dd->modelDelayDeriv[2] = (imXYZ[p].dDelay_dZ[0] + imXYZ[p].dDelay_dZ[1]*dt + imXYZ[p].dDelay_dZ[2]*dt2 + imXYZ[p].dDelay_dZ[3]*dt3 + imXYZ[p].dDelay_dZ[4]*dt4)*0.000001;

	/* And second derivatives over space, interpolated in time */
	dd->modelDelayDeriv2[0][0] = (imXYZ[p].d2Delay_dXdX[0] + imXYZ[p].d2Delay_dXdX[1]*dt + imXYZ[p].d2Delay_dXdX[2]*dt2 + imXYZ[p].d2Delay_dXdX[3]*dt3 + imXYZ[p].d2Delay_dXdX[4]*dt4)*0.000001;
	dd->modelDelayDeriv2[1][1] = (imXYZ[p].d2Delay_dYdY[0] + imXYZ[p].d2Delay_dYdY[1]*dt + imXYZ[p].d2Delay_dYdY[2]*dt2 + imXYZ[p].d2Delay_dYdY[3]*dt3 + imXYZ[p].d2Delay_dYdY[4]*dt4)*0.000001;
	dd->modelDelayDeriv2[2][2] = (imXYZ[p].d2Delay_dZdZ[0] + imXYZ[p].d2Delay_dZdZ[1]*dt + imXYZ[p].d2Delay_dZdZ[2]*dt2 + imXYZ[p].d2Delay_dZdZ[3]*dt3 + imXYZ[p].d2Delay_dZdZ[4]*dt4)*0.000001;
	dd->modelDelayDeriv2[1][0] = dd->modelDelayDeriv2[0][1] = (imXYZ[p].d2Delay_dXdY[0] + imXYZ[p].d2Delay_dXdY[1]*dt + imXYZ[p].d2Delay_dXdY[2]*dt2 + imXYZ[p].d2Delay_dXdY[3]*dt3 + imXYZ[p].d2Delay_dXdY[4]*dt4)*0.000001;
	dd->modelDelayDeriv2[2][0] = dd->modelDelayDeriv2[0][2] = (imXYZ[p].d2Delay_dXdZ[0] + imXYZ[p].d2Delay_dXdZ[1]*dt + imXYZ[p].d2Delay_dXdZ[2]*dt2 + imXYZ[p].d2Delay_dXdZ[3]*dt3 + imXYZ[p].d2Delay_dXdZ[4]*dt4)*0.000001;
	dd->modelDelayDeriv2[2][1] = dd->modelDelayDeriv2[1][2] = (imXYZ[p].d2Delay_dYdZ[0] + imXYZ[p].d2Delay_dYdZ[1]*dt + imXYZ[p].d2Delay_dYdZ[2]*dt2 + imXYZ[p].d2Delay_dYdZ[3]*dt3 + imXYZ[p].d2Delay_dYdZ[4]*dt4)*0.000001;
	
//	printf("Delays for ant %d\n", antennaId);
//	printDelayDerivatives(dd);

	return rv;
}

/* return value in units of s^2 */
double misfit(int nAnt, const double srcOffset[3], const DelayDerivatives *dd, const double *residDelay, double M1[3], double M2[3][3])
{
	int a1, a2, i, j;
	double T[MAX_ANTENNAS];
	double T1[MAX_ANTENNAS][3];
	double T2[MAX_ANTENNAS][3][3];
	double M = 0;
	const double *R = residDelay;

	for(i = 0; i < 3; ++i)
	{
		M1[i] = 0.0;
		for(j = 0; j < 3; ++j)
		{
			M2[i][j] = 0;
		}
	}


	for(a1 = 0; a1 < nAnt; ++a1)
	{
		T[a1] = calcDelay(dd+a1, srcOffset);
		T1[a1][0] = calcDelay1(dd+a1, srcOffset, 0);
		T1[a1][1] = calcDelay1(dd+a1, srcOffset, 1);
		T1[a1][2] = calcDelay1(dd+a1, srcOffset, 2);
		T2[a1][0][0] = calcDelay2(dd+a1, srcOffset, 0, 0);
		T2[a1][1][1] = calcDelay2(dd+a1, srcOffset, 1, 1);
		T2[a1][2][2] = calcDelay2(dd+a1, srcOffset, 2, 2);
		T2[a1][0][1] = T2[a1][1][0] = calcDelay2(dd+a1, srcOffset, 0, 1);
		T2[a1][0][2] = T2[a1][2][0] = calcDelay2(dd+a1, srcOffset, 0, 2);
		T2[a1][1][2] = T2[a1][2][1] = calcDelay2(dd+a1, srcOffset, 1, 2);
	}


	for(a2 = 1; a2 < nAnt; ++a2)
	{
		for(a1 = 0; a1 < a2; ++a1)
		{
			double g;
			double h[3];

			g = (T[a1] - T[a2]) - (R[a1] - R[a2]);	/* FIXME: the middle sign is in question */
			h[0] = T1[a1][0] - T1[a2][0];
			h[1] = T1[a1][1] - T1[a2][1];
			h[2] = T1[a1][2] - T1[a2][2];

			M += g*g;
			M1[0] += 2.0*g*h[0];
			M1[1] += 2.0*g*h[1];
			M1[2] += 2.0*g*h[2];
			M2[0][0] += 2.0*(g*(T2[a1][0][0] - T2[a2][0][0]) + h[0]*h[0]);
			M2[1][1] += 2.0*(g*(T2[a1][1][1] - T2[a2][1][1]) + h[1]*h[1]);
			M2[2][2] += 2.0*(g*(T2[a1][2][2] - T2[a2][2][2]) + h[2]*h[2]);
			M2[0][1] += 2.0*(g*(T2[a1][0][1] - T2[a2][0][1]) + h[0]*h[1]);
			M2[0][2] += 2.0*(g*(T2[a1][0][2] - T2[a2][0][2]) + h[0]*h[2]);
			M2[1][2] += 2.0*(g*(T2[a1][1][2] - T2[a2][1][2]) + h[1]*h[2]);
		}
	}

	/* symmetries */
	M2[1][0] = M2[0][1];
	M2[2][0] = M2[0][2];
	M2[2][1] = M2[1][2];

	return M;
}

void invert3x3(double A[3][3], double result[3][3])
{
	double determinant =     A[0][0]*(A[1][1]*A[2][2]-A[2][1]*A[1][2])
        	                -A[0][1]*(A[1][0]*A[2][2]-A[1][2]*A[2][0])
                	        +A[0][2]*(A[1][0]*A[2][1]-A[1][1]*A[2][0]);
	double invdet = 1/determinant;
	result[0][0] =  (A[1][1]*A[2][2]-A[2][1]*A[1][2])*invdet;
	result[1][0] = -(A[0][1]*A[2][2]-A[0][2]*A[2][1])*invdet;
	result[2][0] =  (A[0][1]*A[1][2]-A[0][2]*A[1][1])*invdet;
	result[0][1] = -(A[1][0]*A[2][2]-A[1][2]*A[2][0])*invdet;
	result[1][1] =  (A[0][0]*A[2][2]-A[0][2]*A[2][0])*invdet;
	result[2][1] = -(A[0][0]*A[1][2]-A[1][0]*A[0][2])*invdet;
	result[0][2] =  (A[1][0]*A[2][1]-A[2][0]*A[1][1])*invdet;
	result[1][2] = -(A[0][0]*A[2][1]-A[2][0]*A[0][1])*invdet;
	result[2][2] =  (A[0][0]*A[1][1]-A[1][0]*A[0][1])*invdet;
}

/* residDelay in seconds */
void solve1(const DifxInput *D, double mjd, const double *residDelay)
{
	DifxSource *source;
	DifxScan *scan;
	DifxSpacecraft *sc;
	int sourceId;
	sixVector interpolatedInitialPosition;

	DelayDerivatives *dd;
	double antPos[MAX_ANTENNAS][3];
	double sourcePos[3];
	double sourceOffset[3];	/* solve for this, then add to the above to get the final answer */
	int a, i;
	int mjdDay;
	double mjdSec;

	mjdDay = mjd;
	mjdSec = (mjd - mjdDay)*86400.0;

	dd = (DelayDerivatives *)calloc(D->nAntenna, sizeof(DelayDerivatives));

	for(a = 0; a < D->nAntenna; ++a)
	{
		antPos[a][0] = D->antenna[a].X/C_LIGHT;
		antPos[a][1] = D->antenna[a].Y/C_LIGHT;
		antPos[a][2] = D->antenna[a].Z/C_LIGHT;
		/* FIXME: first 0 below should refer to actual scan number */
		getDelay(D, a, 0, 0, mjdDay, mjdSec, dd + a);
	}

	scan = D->scan + 0;	/* Only support one scan per job, at least now */
	if(scan->imXYZ == 0)
	{
		fprintf(stderr, "Error: .im file needs the XYZ Extension data!\n");
		free(dd);

		return;
	}
	sourceId = scan->phsCentreSrcs[0];
	source = D->source + sourceId;
	if(source->spacecraftId < 0)
	{
		printf("Error: source %s is not a spacecraft!\n", source->name);
		free(dd);

		return;
	}
	sc = D->spacecraft + source->spacecraftId;
	
	/* use ephemeris as starting point */
	evaluateDifxSpacecraft(sc, mjdDay, mjdSec/86400.0, &interpolatedInitialPosition);
	sourcePos[0] = interpolatedInitialPosition.X/C_LIGHT;
	sourcePos[1] = interpolatedInitialPosition.Y/C_LIGHT;
	sourcePos[2] = interpolatedInitialPosition.Z/C_LIGHT;
	for(i = 0; i < 3; ++i)
	{
		sourceOffset[i] = 0.0;
	}

	printf("Starting position at mjd %14.8f for SC = %10.8f %10.8f %10.5f (sec) = %5.3f %5.3f %5.3f (m)\n",
		mjd, sourcePos[0], sourcePos[1], sourcePos[2],
		sourcePos[0]*C_LIGHT, sourcePos[1]*C_LIGHT, sourcePos[2]*C_LIGHT);

	for(i = 0; i < 5; ++i)
	{
		double M;
		double M1[3];
		double M2[3][3];
		double M2inverse[3][3];
		double score;

		M = misfit(D->nAntenna, sourceOffset, dd, residDelay, M1, M2);
		
		invert3x3(M2, M2inverse);

//		printf("M = %e M1 = %e,%e,%e\n", M, M1[0], M1[1], M1[2]);
		
		score = sqrt(2.0*M/(D->nAntenna*(D->nAntenna-1)))*C_LIGHT;

		printf("%d  Offset = %5.3f %5.3f %5.3f (m) -> score = %5.3f (m)\n",
			i,
			sourceOffset[0], 
			sourceOffset[1], 
			sourceOffset[2], 
			score);

		sourceOffset[0] -= M2inverse[0][0]*M1[0] + M2inverse[0][1]*M1[1] + M2inverse[0][2]*M1[2];
		sourceOffset[1] -= M2inverse[1][0]*M1[0] + M2inverse[1][1]*M1[1] + M2inverse[1][2]*M1[2];
		sourceOffset[2] -= M2inverse[2][0]*M1[0] + M2inverse[2][1]*M1[1] + M2inverse[2][2]*M1[2];


	}
	printf("Ending position at mjd %14.8f for SC = %10.8f %10.8f %10.5f (sec) = %5.3f %5.3f %5.3f (m)\n",
		mjd, sourcePos[0]+sourceOffset[0]/C_LIGHT, sourcePos[1]+sourceOffset[1]/C_LIGHT, sourcePos[2]+sourceOffset[2]/C_LIGHT,
		sourcePos[0]*C_LIGHT+sourceOffset[0], sourcePos[1]*C_LIGHT+sourceOffset[1], sourcePos[2]*C_LIGHT+sourceOffset[2]);
	printf("\n");

	free(dd);
}

void testsolve(const DifxInput *D)
{
/*
      69      16:00:22.0      00:00:01.5          1         -9.649785E-07
      70      16:00:22.0      00:00:01.5          2          4.650725E-07
      71      16:00:22.0      00:00:01.5          3          2.207024E-06
      72      16:00:22.0      00:00:01.5          4         -2.259683E-07
      73      16:00:22.0      00:00:01.5          5          2.110347E-07
      74      16:00:22.0      00:00:01.5          6         -5.328458E-06
      75      16:00:22.0      00:00:01.5          7          1.267747E-06
      76      16:00:22.0      00:00:01.5          8         -9.634513E-07
      77      16:00:22.0      00:00:01.5          9          0.000000E+00
      78      16:00:22.0      00:00:01.5         10          2.853595E-06

     169      16:00:39.5      00:00:01.5          1         -9.665882E-07
     170      16:00:39.5      00:00:01.5          2          4.662891E-07
     171      16:00:39.5      00:00:01.5          3          2.219105E-06
     172      16:00:39.5      00:00:01.5          4         -2.290494E-07
     173      16:00:39.5      00:00:01.5          5          2.117527E-07
     174      16:00:39.5      00:00:01.5          6         -5.338962E-06
     175      16:00:39.5      00:00:01.5          7          1.273706E-06
     176      16:00:39.5      00:00:01.5          8         -9.663453E-07
     177      16:00:39.5      00:00:01.5          9          0.000000E+00
     178      16:00:39.5      00:00:01.5         10          2.761165E-06

     269      16:00:57.0      00:00:01.5          1         -9.681762E-07
     270      16:00:57.0      00:00:01.5          2          4.674806E-07
     271      16:00:57.0      00:00:01.5          3          2.231192E-06
     272      16:00:57.0      00:00:01.5          4         -2.281470E-07
     273      16:00:57.0      00:00:01.5          5          2.124021E-07
     274      16:00:57.0      00:00:01.5          6         -5.349444E-06
     275      16:00:57.0      00:00:01.5          7          1.279670E-06
     276      16:00:57.0      00:00:01.5          8         -9.692022E-07
     277      16:00:57.0      00:00:01.5          9          0.000000E+00
     278      16:00:57.0      00:00:01.5         10          2.885403E-06

*/
	double delays1[10] = { -9.649785e-07, 4.650725e-07, 2.207024e-06, -2.259683e-07, 2.110347e-07, -5.328458e-06, 1.267747e-06, -9.634513e-07, 0.0, 2.853595e-06 };
	double mjd1 = 56999.666921;
	double delays2[10] = { -9.665882E-07, 4.662891E-07, 2.219105E-06, -2.290494E-07, 2.117527E-07, -5.338962E-06, 1.273706E-06, -9.663453E-07, 0.0, 2.761165E-06 };
	double mjd2 = 56999.667124;
	double delays3[10] = { -9.681762E-07, 4.674806E-07, 2.231192E-06, -2.281470E-07, 2.124021E-07, -5.349444E-06, 1.279670E-06, -9.692022E-07, 0.0, 2.885403E-06 };
	double mjd3 = 56999.667326;

	solve1(D, mjd1, delays1);
	solve1(D, mjd2, delays2);
	solve1(D, mjd3, delays3);
}

int main(int argc, char **argv)
{
	DifxInput *D = 0;
	int a;
	int verbose = 0;
	int mergable, compatible;
	int nJob = 0;
	const char *delayFilename = 0;
	
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
		else if(delayFilename == 0)
		{
			delayFilename = argv[a];

			continue;
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
				mergable = areDifxInputsMergable(D1, D2);
				compatible = areDifxInputsCompatible(D1, D2);
				if(mergable && compatible)
				{
					D = mergeDifxInputs(D1, D2, verbose);
					deleteDifxInput(D1);
					deleteDifxInput(D2);
				}
				else
				{
					printf("cannot merge job %s: mergable=%d compatible=%d\n", argv[a], mergable, compatible);
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

	D = updateDifxInput(D);
	if(!D)
	{
		fprintf(stderr, "Update failed: D == 0.  Quitting\n");
		
		return EXIT_FAILURE;
	}

	//printDifxInput(D);

	printf("Delay filename to read in: %s\n", delayFilename);
	testsolve(D);

	deleteDifxInput(D);

	return 0;
}
