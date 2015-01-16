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

#define MAX_ANTENNAS 40
#define C_LIGHT	299792458.0

const char program[] = "fitdelay";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20150114";

typedef struct
{
	/* all quantities in seconds */
	double modelDelay;
	double modelDelayDeriv[3];
	double modelDelayDeriv2[3][3];
} DelayDerivatives;

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

void printDelayDerivatives(const DelayDerivatives *dd)
{
	printf("  T = %10.8e\n", dd->modelDelay);
	printf("  dT/dX =  %10.8e\n", dd->modelDelayDeriv[0]);
	printf("  dT/dY =  %10.8e\n", dd->modelDelayDeriv[1]);
	printf("  dT/dZ =  %10.8e\n", dd->modelDelayDeriv[2]);
	printf("  d2T/dXdX =  %10.8e\n", dd->modelDelayDeriv2[0][0]);
	printf("  d2T/dXdY =  %10.8e\n", dd->modelDelayDeriv2[0][1]);
	printf("  d2T/dXdZ =  %10.8e\n", dd->modelDelayDeriv2[0][2]);
	printf("  d2T/dYdY =  %10.8e\n", dd->modelDelayDeriv2[1][1]);
	printf("  d2T/dYdZ =  %10.8e\n", dd->modelDelayDeriv2[1][2]);
	printf("  d2T/dZdZ =  %10.8e\n", dd->modelDelayDeriv2[2][2]);
}

double interpolate(double x0, double y0, double x1, double y1, double x)
{
	if(x1 < x0)
	{
		x1 += 1.0;
	}
	if(x < x0)
	{
		x += 1.0;
	}
	return y0 + (y1-y0)*(x-x0)/(x1-x0);
}

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
	
	printf("Delays for ant %d\n", antennaId);
	printDelayDerivatives(dd);

	return rv;
}

double getU(const DifxInput *D, int antennaId, int scanId, int pc, int mjd, double sec)
{
	int p;
	DifxPolyModel *im;
	double dt;
	double rv;

	im = D->scan[scanId].im[antennaId][pc];

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

	rv = im[p].u[0] + im[p].u[1]*dt + im[p].u[2]*dt*dt + im[p].u[3]*dt*dt*dt;
	rv = rv/C_LIGHT;	/* convert m to sec */

	return rv;
}

double getV(const DifxInput *D, int antennaId, int scanId, int pc, int mjd, double sec)
{
	int p;
	DifxPolyModel *im;
	double dt;
	double rv;

	im = D->scan[scanId].im[antennaId][pc];

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

	rv = im[p].v[0] + im[p].v[1]*dt + im[p].v[2]*dt*dt + im[p].v[3]*dt*dt*dt;
	rv = rv/C_LIGHT;	/* convert m to sec */

	return rv;
}

double misfit(int nAnt, const double srcOffset[3], const DelayDerivatives *dd, const double *residDelay)
{
	int a1, a2;
	double rv = 0.0;

	for(a2 = 1; a2 < nAnt; ++a2)
	{
		for(a1 = 0; a1 < a2; ++a1)
		{
			
		}
	}

	return rv;
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

		printf("Ant %d : pos = %f %f %f  model = %10.8f  resid = %10.8f\n", a+1, antPos[a][0], antPos[a][1], antPos[a][2], dd[a].modelDelay, residDelay[a]);
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


	for(i = 0; i < 100; ++i)
	{
		double score;

		score = misfit(D->nAntenna, sourceOffset, dd, residDelay);

		printf("Offset = %5.3f %5.3f %5.3f (m) -> score = %5.3f (m)\n",
			sourceOffset[0]*C_LIGHT, 
			sourceOffset[1]*C_LIGHT, 
			sourceOffset[2]*C_LIGHT, 
			score);
	}

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
*/
	double delays[10] = { -9.649785e-07, 4.650725e-07, 2.207024e-06, -2.259683e-07, 2.110347e-07, -5.328458e-06, 1.267747e-06, -9.634513e-07, 0.0, 2.853595e-06 };
	double mjd = 56999.666921;
	solve1(D, mjd, delays);
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
