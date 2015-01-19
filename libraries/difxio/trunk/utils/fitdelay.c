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
const char verdate[] = "20150119";

void usage()
{
	printf("%s  ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("Usage : %s [options] <residdelayfile> <inputfilebase1> [ <inputfilebase2> [ ... ] ]\n\n", program);
	printf("options can include:\n");
	printf("--verbose\n");
	printf("-v         be a bit more verbose\n\n");
	printf("--help\n");
	printf("-h         print help information and quit\n\n");
	printf("<residdelayfile> is the name of a file containing residual delays.\n\n");
	printf("<inputfilebaseN> is the base name of a difx fileset.\n\n");
	printf("The <residdelayfile> is in free-form text format with:\n");
	printf("  Comments start with #\n");
	printf("  First number on a row is MJD + fractional MJD\n");
	printf("  Then one number per antenna containing delay in units of seconds\n");
	printf("  This file can be make by the associated python program \"convertdelay\"\n");
	printf("  with input from AIPS PRTAB on a FRING SN table with BOX = 1,2,4,12\n\n");
}

/* Use Cramer's rule to evaluate polynomial */
double evaluatePoly(const double *coef, int n, double t)
{
	double v;
	int i;

	if(n == 1)
	{
		return coef[0];
	}

	v = coef[n-1];

	for(i = n-2; i >= 0; i--)
	{
		v = t*v + coef[i];
	}

	return v;
}

/* goes to DifxInput object and interpolates the partial derivatives in time, filling the DelayLMDerivatives structure in the end */
void getDelayLM(const DifxInput *D, int antennaId, int scanId, int pc, int mjd, double sec, DelayLMDerivatives *dd)
{
	int p;
	DifxPolyModel *im;
	DifxPolyModelLMExtension *imLM;
	double dt;

	im = D->scan[scanId].im[antennaId][pc];
	imLM = D->scan[scanId].imLM[antennaId][pc];

	for(p = 0; p < D->scan[scanId].nPoly; ++p)
	{
		if(im[p].mjd == mjd && im[p].sec <= sec && im[p].sec+im[p].validDuration >= sec)
		{
			break;
		}
	}

	if(p == D->scan[scanId].nPoly)
	{
		/* time not found in the scan */

		return;
	}

	dt = sec - im[p].sec;

	/* 0.000001 converts from us to s */
	dd->delay             = 0.000001*evaluatePoly(im[p].delay,          D->job->polyOrder+1, dt);
	dd->delayDeriv[0]     = 0.000001*evaluatePoly(imLM[p].dDelay_dl,    D->job->polyOrder+1, dt);
	dd->delayDeriv[1]     = 0.000001*evaluatePoly(imLM[p].dDelay_dm,    D->job->polyOrder+1, dt);
	dd->delayDeriv2[0][0] = 0.000001*evaluatePoly(imLM[p].d2Delay_dldl, D->job->polyOrder+1, dt);
	dd->delayDeriv2[1][1] = 0.000001*evaluatePoly(imLM[p].d2Delay_dmdm, D->job->polyOrder+1, dt);
	dd->delayDeriv2[0][1] = 0.000001*evaluatePoly(imLM[p].d2Delay_dldm, D->job->polyOrder+1, dt);
	dd->delayDeriv2[1][0] = dd->delayDeriv2[0][1];	/* by symmetry */
}

/* goes to DifxInput object and interpolates the partial derivatives in time, filling the DelayXYZDerivatives structure in the end */
void getDelayXYZ(const DifxInput *D, int antennaId, int scanId, int pc, int mjd, double sec, DelayXYZDerivatives *dd)
{
	int p;
	DifxPolyModel *im;
	DifxPolyModelXYZExtension *imXYZ;
	double dt;

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
		/* time not found in the scan */

		return;
	}

	dt = sec - im[p].sec;

	/* 0.000001 converts from us to s */
	dd->delay             = 0.000001*evaluatePoly(im[p].delay,           D->job->polyOrder+1, dt);
	dd->delayDeriv[0]     = 0.000001*evaluatePoly(imXYZ[p].dDelay_dX,    D->job->polyOrder+1, dt);
	dd->delayDeriv[1]     = 0.000001*evaluatePoly(imXYZ[p].dDelay_dY,    D->job->polyOrder+1, dt);
	dd->delayDeriv[1]     = 0.000001*evaluatePoly(imXYZ[p].dDelay_dZ,    D->job->polyOrder+1, dt);
	dd->delayDeriv2[0][0] = 0.000001*evaluatePoly(imXYZ[p].d2Delay_dXdX, D->job->polyOrder+1, dt);
	dd->delayDeriv2[1][1] = 0.000001*evaluatePoly(imXYZ[p].d2Delay_dYdY, D->job->polyOrder+1, dt);
	dd->delayDeriv2[2][2] = 0.000001*evaluatePoly(imXYZ[p].d2Delay_dZdZ, D->job->polyOrder+1, dt);
	dd->delayDeriv2[0][1] = 0.000001*evaluatePoly(imXYZ[p].d2Delay_dXdY, D->job->polyOrder+1, dt);
	dd->delayDeriv2[0][2] = 0.000001*evaluatePoly(imXYZ[p].d2Delay_dXdZ, D->job->polyOrder+1, dt);
	dd->delayDeriv2[1][2] = 0.000001*evaluatePoly(imXYZ[p].d2Delay_dYdZ, D->job->polyOrder+1, dt);
	dd->delayDeriv2[1][0] = dd->delayDeriv2[0][1];	/* by symmetry */
	dd->delayDeriv2[2][0] = dd->delayDeriv2[0][2];	/* by symmetry */
	dd->delayDeriv2[2][1] = dd->delayDeriv2[1][2];	/* by symmetry */
}

/* return value in units of s^2 */
double misfitLM(int nAnt, const double sourceOffset[2], const DelayLMDerivatives *dd, const double *residDelay, double M1[2], double M2[2][2])
{
	int a1, a2, i, j;
	double T[MAX_ANTENNAS];
	double T1[MAX_ANTENNAS][2];
	double T2[MAX_ANTENNAS][2][2];
	double M = 0;
	const double *R = residDelay;

	for(i = 0; i < 2; ++i)
	{
		M1[i] = 0.0;
		for(j = 0; j < 2; ++j)
		{
			M2[i][j] = 0;
		}
	}

	for(a1 = 0; a1 < nAnt; ++a1)
	{
		T[a1] = calcDelayLM(dd+a1, sourceOffset);
		T1[a1][0] = calcDelayLM1(dd+a1, sourceOffset, 0);
		T1[a1][1] = calcDelayLM1(dd+a1, sourceOffset, 1);
		T2[a1][0][0] = calcDelayLM2(dd+a1, sourceOffset, 0, 0);
		T2[a1][1][1] = calcDelayLM2(dd+a1, sourceOffset, 1, 1);
		T2[a1][0][1] = T2[a1][1][0] = calcDelayLM2(dd+a1, sourceOffset, 0, 1);
	}

	for(a2 = 1; a2 < nAnt; ++a2)
	{
		for(a1 = 0; a1 < a2; ++a1)
		{
			/* g and h are common subexpressions */
			double g;
			double h[2];

			g = (T[a1] - T[a2]) - (R[a1] - R[a2]);
			h[0] = T1[a1][0] - T1[a2][0];
			h[1] = T1[a1][1] - T1[a2][1];

			M += g*g;
			M1[0] += 2.0*g*h[0];
			M1[1] += 2.0*g*h[1];
			M2[0][0] += 2.0*(g*(T2[a1][0][0] - T2[a2][0][0]) + h[0]*h[0]);
			M2[1][1] += 2.0*(g*(T2[a1][1][1] - T2[a2][1][1]) + h[1]*h[1]);
			M2[0][1] += 2.0*(g*(T2[a1][0][1] - T2[a2][0][1]) + h[0]*h[1]);
		}
	}

	/* symmetries */
	M2[1][0] = M2[0][1];

	return M;
}

/* return value in units of s^2 */
double misfitXYZ(int nAnt, const double sourceOffset[3], const DelayXYZDerivatives *dd, const double *residDelay, double M1[3], double M2[3][3])
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
		T[a1] = calcDelayXYZ(dd+a1, sourceOffset);
		T1[a1][0] = calcDelayXYZ1(dd+a1, sourceOffset, 0);
		T1[a1][1] = calcDelayXYZ1(dd+a1, sourceOffset, 1);
		T1[a1][2] = calcDelayXYZ1(dd+a1, sourceOffset, 2);
		T2[a1][0][0] = calcDelayXYZ2(dd+a1, sourceOffset, 0, 0);
		T2[a1][1][1] = calcDelayXYZ2(dd+a1, sourceOffset, 1, 1);
		T2[a1][2][2] = calcDelayXYZ2(dd+a1, sourceOffset, 2, 2);
		T2[a1][0][1] = T2[a1][1][0] = calcDelayXYZ2(dd+a1, sourceOffset, 0, 1);
		T2[a1][0][2] = T2[a1][2][0] = calcDelayXYZ2(dd+a1, sourceOffset, 0, 2);
		T2[a1][1][2] = T2[a1][2][1] = calcDelayXYZ2(dd+a1, sourceOffset, 1, 2);
	}

	for(a2 = 1; a2 < nAnt; ++a2)
	{
		for(a1 = 0; a1 < a2; ++a1)
		{
			/* g and h are common subexpressions */
			double g;
			double h[3];

			/* FIXME: handle antenna flagging better */
			if(a1 == 3 || a1 == 4 || a2 == 3 || 2 == 4)
			{
				continue;
			}

			g = (T[a1] - T[a2]) - (R[a1] - R[a2]);
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

void invert2x2(double A[2][2], double result[2][2])
{
	double determinant = A[0][0]*A[1][1] -A[0][1]*A[1][0];
	double invdet = 1.0/determinant;
	result[0][0] =  A[1][1]*invdet;
	result[1][0] = -A[1][0]*invdet;
	result[0][1] = -A[0][1]*invdet;
	result[1][1] =  A[0][0]*invdet;
}

void invert3x3(double A[3][3], double result[3][3])
{
	double determinant = A[0][0]*(A[1][1]*A[2][2]-A[2][1]*A[1][2])
                            -A[0][1]*(A[1][0]*A[2][2]-A[1][2]*A[2][0])
                            +A[0][2]*(A[1][0]*A[2][1]-A[1][1]*A[2][0]);
	double invdet = 1.0/determinant;
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
void solve1(const DifxInput *D, double mjd, const double *residDelay, const double *clockNS)
{
	DifxSource *source;
	DifxScan *scan;
	int scanId, sourceId;

	double correctedResidDelay[MAX_ANTENNAS];	/* (s) */
	int a, i;
	int mjdDay;
	double mjdSec;

	for(scanId = 0; scanId < D->nScan; ++scanId)
	{
		if(mjd >= D->scan[scanId].mjdStart && mjd <= D->scan[scanId].mjdEnd)
		{
			break;
		}
	}

	if(scanId >= D->nScan)
	{
		fprintf(stderr, "Warning: mjd %14.8f not in a scan.  Skipping.\n", mjd);

		return;
	}

	scan = D->scan + scanId;

	mjdDay = mjd;
	mjdSec = (mjd - mjdDay)*86400.0;
	sourceId = scan->phsCentreSrcs[0];
	source = D->source + sourceId;

	/* Apply clock offsets */
	for(a = 0; a < D->nAntenna; ++a)
	{
		correctedResidDelay[a] = residDelay[a] - clockNS[a]*1.0e-9;
	}

	if(scan->imLM != 0)
	{
		fprintf(stderr, "Error: .im uses LM Extension but that is not yet supported here\n");

		return;

		DelayLMDerivatives *dd;
		double sourcePos[2];	/* (rad) [0] is RA, [1] is Dec */
		double sourceOffset[2];	/* (rad) solve for this, then add to the initial source position to get the final answer */
		double score;		/* (rad) metric to be minimized */

		/* compute delay model partial derivatives */
		dd = (DelayLMDerivatives *)calloc(D->nAntenna, sizeof(DelayLMDerivatives));
		for(a = 0; a < D->nAntenna; ++a)
		{
			getDelayLM(D, a, scanId, 0, mjdDay, mjdSec, dd + a);
		}

		/* use RA and Dec as starting point */
		sourcePos[0] = source->ra;
		sourcePos[1] = source->dec;
		for(i = 0; i < 2; ++i)
		{
			sourceOffset[i] = 0.0;
		}

		for(i = 0; i < 5; ++i)	/* FIXME: use real convergence test.  But this converges _really_ fast so 5 iterations is overkill */
		{
			double M;
			double M1[2];
			double M2[2][2];
			double M2inverse[2][2];

			/* calculate fit metric and its first and second partial derivatives w.r.t. source position at sourceOffset from nominal position */
			M = misfitLM(D->nAntenna, sourceOffset, dd, correctedResidDelay, M1, M2);
			
			invert2x2(M2, M2inverse);

			score = sqrt(2.0*M/(D->nAntenna*(D->nAntenna-1)))*C_LIGHT;

			sourceOffset[0] -= M2inverse[0][0]*M1[0] + M2inverse[0][1]*M1[1];
			sourceOffset[1] -= M2inverse[1][0]*M1[0] + M2inverse[1][1]*M1[1];
		}
		printf("mjd = %14.8f  score = %f  offset = %5.3f %5.3f rad  pos = %5.3f %5.3f rad\n",
			mjd, score,
			sourceOffset[0], sourceOffset[1],
			sourcePos[0]+sourceOffset[0]/cos(sourcePos[1]), sourcePos[1]+sourceOffset[1]);

		free(dd);
	}
	else if(scan->imXYZ != 0)
	{
		DelayXYZDerivatives *dd;
		double sourcePos[3];	/* (m) */
		double sourceOffset[3];	/* (m) solve for this, then add to the above to get the final answer */
		DifxSpacecraft *sc;
		sixVector interpolatedInitialPosition;
		double score;		/* (m) metric to be minimized */

		if(source->spacecraftId < 0)
		{
			fprintf(stderr, "Error: source %s is not a spacecraft!\n", source->name);

			return;
		}
		sc = D->spacecraft + source->spacecraftId;
		
		/* compute delay model partial derivatives */
		dd = (DelayXYZDerivatives *)calloc(D->nAntenna, sizeof(DelayXYZDerivatives));
		for(a = 0; a < D->nAntenna; ++a)
		{
			getDelayXYZ(D, a, scanId, 0, mjdDay, mjdSec, dd + a);
		}

		/* use ephemeris as starting point */
		evaluateDifxSpacecraft(sc, mjdDay, mjdSec/86400.0, &interpolatedInitialPosition);
		sourcePos[0] = interpolatedInitialPosition.X;
		sourcePos[1] = interpolatedInitialPosition.Y;
		sourcePos[2] = interpolatedInitialPosition.Z;
		for(i = 0; i < 3; ++i)
		{
			sourceOffset[i] = 0.0;
		}

		for(i = 0; i < 5; ++i)	/* FIXME: use real convergence test.  But this converges _really_ fast so 5 iterations is overkill */
		{
			double M;
			double M1[3];
			double M2[3][3];
			double M2inverse[3][3];

			/* calculate fit metric and its first and second partial derivatives w.r.t. source position at sourceOffset from nominal position */
			M = misfitXYZ(D->nAntenna, sourceOffset, dd, correctedResidDelay, M1, M2);
			
			invert3x3(M2, M2inverse);

			score = sqrt(2.0*M/(D->nAntenna*(D->nAntenna-1)))*C_LIGHT;

			sourceOffset[0] -= M2inverse[0][0]*M1[0] + M2inverse[0][1]*M1[1] + M2inverse[0][2]*M1[2];
			sourceOffset[1] -= M2inverse[1][0]*M1[0] + M2inverse[1][1]*M1[1] + M2inverse[1][2]*M1[2];
			sourceOffset[2] -= M2inverse[2][0]*M1[0] + M2inverse[2][1]*M1[1] + M2inverse[2][2]*M1[2];
		}
		printf("mjd = %14.8f  score = %f  offset = %5.3f %5.3f %5.3f m  pos = %5.3f %5.3f %5.3f m\n",
			mjd, score,
			sourceOffset[0], sourceOffset[1], sourceOffset[2],
			sourcePos[0]+sourceOffset[0], sourcePos[1]+sourceOffset[1], sourcePos[2]+sourceOffset[2]);

		free(dd);
	}
	else
	{
		fprintf(stderr, "Error: .im file needs either LM or XYZ Extension data! (scanId = %d)\n", scanId);

		return;
	}
}

void testsolve(const DifxInput *D)
{
	double clockNS [10] = {16, 90, -93, 0, 33, 38, -19, -5.6, 0, -45 };

	double delays1[10] = { -9.649785e-07, 4.650725e-07, 2.207024e-06, -2.259683e-07, 2.110347e-07, -5.328458e-06, 1.267747e-06, -9.634513e-07, 0.0, 2.853595e-06 };
	double mjd1 = 56999.666921;
	double delays2[10] = { -9.665882E-07, 4.662891E-07, 2.219105E-06, -2.290494E-07, 2.117527E-07, -5.338962E-06, 1.273706E-06, -9.663453E-07, 0.0, 2.761165E-06 };
	double mjd2 = 56999.667124;
	double delays3[10] = { -9.681762E-07, 4.674806E-07, 2.231192E-06, -2.281470E-07, 2.124021E-07, -5.349444E-06, 1.279670E-06, -9.692022E-07, 0.0, 2.885403E-06 };
	double mjd3 = 56999.667326;

	solve1(D, mjd1, delays1, clockNS);
	solve1(D, mjd2, delays2, clockNS);
	solve1(D, mjd3, delays3, clockNS);
}

/* The delay file here is in a free format text format with:
 * Comments starting with #
 * First number on a row is MJD + fractional MJD
 * Then one number per antenna containing delay in units of seconds
 * This file can be make by the associated python program "convertdelay"
 * with input from AIPS PRTAB on a FRING SN table with BOX = 1,2,4,12 
 */
void runfile(const DifxInput *D, const char *delayFile)
{
	/* FIXME: handle != 10 antennas */
	/* FIXME: hard coded clock offsets here */
	const int LineLength = 2048;
	char line[LineLength];
	double clockNS[10] = {16, 90, -93, 0, 33, 38, -19, -5.6, 0, -45 };	/* (ns) Same sign sense as AIPS FRING produces */
	FILE *in;
	double mjd, delays[10];		/* (s) Same sign sense as AIPS FRING produces */
	
	in = fopen(delayFile, "r");
	if(!in)
	{
		fprintf(stderr, "Cannot open %s\n", delayFile);
		
		return;
	}

	for(;;)
	{
		int n;

		fgets(line, LineLength, in);
		if(feof(in))
		{
			break;
		}
		if(line[0] == '#')
		{
			continue;
		}
		n = sscanf(line, "%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf", &mjd, delays+0, delays+1, delays+2, delays+3, delays+4, delays+5, delays+6, delays+7, delays+8, delays+9);
		if(n != 11)
		{
			break;
		}
		solve1(D, mjd, delays, clockNS);
	}

	fclose(in);
}

int main(int argc, char **argv)
{
	DifxInput *D = 0;
	int a;
	int verbose = 0;
	int mergable, compatible;
	int nJob = 0;
	const char *delayFilename = 0;

	printf("# File written by %s ver. %s\n", program, version);
	
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
			printf("# Loading DiFX fileset: %s\n", argv[a]);
			D = loadDifxInput(argv[a]);
		}
		else
		{
			DifxInput *D1, *D2;

			printf("# Loading DiFX fileset: %s\n", argv[a]);
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

	printf("# Delay filename to read in: %s\n", delayFilename);
	runfile(D, delayFilename);

	deleteDifxInput(D);

	return 0;
}
