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

double getDelay(const DifxInput *D, int antennaId, int scanId, int pc, int mjd, double sec)
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

	rv = im[p].delay[0] + im[p].delay[1]*dt + im[p].delay[2]*dt*dt + im[p].delay[3]*dt*dt*dt;
	rv -= im[p].dry[0] + im[p].dry[1]*dt + im[p].dry[2]*dt*dt + im[p].dry[3]*dt*dt*dt;
	rv -= im[p].wet[0] + im[p].wet[1]*dt + im[p].wet[2]*dt*dt + im[p].wet[3]*dt*dt*dt;
	rv = rv*0.000001;	/* convert us to sec */

	//printf("ANT %d  mjd %d sec %f -> %10.6f   im.sec = %d  dt=%f d[0]=%10.8e d[1]=%10.8e -> rv = %10.8e\n", antennaId, mjd, sec, mjd+sec/86400.0, im[p].sec, dt, im[p].delay[0], im[p].delay[1], rv);

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

double misfit(int nAnt, const double srcPos[3], const double *antPos[3], const double *modelDelay, const double *residDelay)
{
	int a1, a2;
	double rv = 0.0;

	for(a2 = 1; a2 < nAnt; ++a2)
	{
		for(a1 = 0; a1 < a2; ++a1)
		{
			
		}
	}
}

/* residDelay in seconds */
void solve1(const DifxInput *D, double mjd, const double *residDelay)
{
	double antPos[MAX_ANTENNAS][3];
	double sourcePos[3];
	double modelDelay[MAX_ANTENNAS];
	double U[MAX_ANTENNAS];
	double V[MAX_ANTENNAS];
	int a, i;
	int mjdDay;
	double mjdSec;

	mjdDay = mjd;
	mjdSec = (mjd - mjdDay)*86400.0;

	for(a = 0; a < D->nAntenna; ++a)
	{
		antPos[a][0] = D->antenna[a].X/C_LIGHT;
		antPos[a][1] = D->antenna[a].Y/C_LIGHT;
		antPos[a][2] = D->antenna[a].Z/C_LIGHT;
		/* FIXME: first 0 below should refer to actual scan number */
		modelDelay[a] = getDelay(D, a, 0, 0, mjdDay, mjdSec);

		printf("Ant %d : pos = %f %f %f  model = %10.8f  resid = %10.8f\n", a+1, antPos[a][0], antPos[a][1], antPos[a][2], modelDelay[a], residDelay[a]);
	}

	for(i = 0; i < 3; ++i)
	{
		sourcePos[i] = antPos[8][i]*20.0/C_LIGHT;	/* FIXME! now just using 20x PT as starting source position */
	}

	for(i = 0; i < 100; ++i)
	{
		
	}

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
