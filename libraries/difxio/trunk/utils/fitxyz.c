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
#include "config.h"
#include "difx_input.h"
#include "delayderivatives.h"

#define MAX_POINTS	10000
#define MAX_COEF	32

#if HAVE_GSL
#include <gsl/gsl_multifit.h>


const char program[] = "fitxyz";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20150119";


/* fit N-term polynomial v(t) to C[0] + C[1]*t + ... + C[N-1]*t^{N-1) */
/* n is number of (t, v) values */
/* coef is output poly coefs */
int fit(const double *t, const double *v, int n, double *coef, int N)
{
	int i, j;
	double chisq;
	gsl_matrix *X, *cov;
	gsl_vector *y, *w, *c;
	gsl_multifit_linear_workspace *work;

	X = gsl_matrix_alloc(n, N);
	y = gsl_vector_alloc(n);
	w = gsl_vector_alloc(n);
	work = gsl_multifit_linear_alloc(n, N);

	c = gsl_vector_alloc(N);
	cov = gsl_matrix_alloc(N, N);

	for (i = 0; i < n; i++)
	{
		double tn;
		
		tn = 1.0;
		for(j = 0; j < N; ++j)
		{
			gsl_matrix_set(X, i, j, tn);
			tn *= t[i];
		}

		gsl_vector_set(y, i, v[i]);
		gsl_vector_set(w, i, 1.0);	/* uniform weight for now */
	}

	gsl_multifit_wlinear(X, w, y, c, cov, &chisq, work);

	for(j = 0; j < N; ++j)
	{
		coef[j] = gsl_vector_get(c, j);
	}

	gsl_matrix_free(X);
	gsl_vector_free(y);
	gsl_vector_free(w);
	gsl_vector_free(c);
	gsl_matrix_free(cov);
	gsl_multifit_linear_free(work);

	return 0;
}
#endif

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

int main(int argc, char **argv)
{
#if HAVE_GSL
	const int MaxLineLength = 1024;
	FILE *in;

	int mjd0 = -1;		/* reference day */
	double t[MAX_POINTS];	/* day (mjd) */
	double s[MAX_POINTS];	/* score */
	double x[MAX_POINTS];
	double y[MAX_POINTS];
	double z[MAX_POINTS];
	int nPoint;
	int i;
	char line[MaxLineLength];
	double coef[3][MAX_COEF];	/* first index over X, Y, Z */
	const char *inputFile;
	int nCoef;

	if(argc != 3)
	{
		printf("Usage:  %s <file> <nCoef>\n\n", program);
		printf("Input file should be output from fitdelay\n");

		return EXIT_FAILURE;
	}

	inputFile = argv[1];
	nCoef = atoi(argv[2]);
	if(nCoef < 1 || nCoef > MAX_COEF)
	{
		fprintf(stderr, "nCoef must be in range [1, %d]\n", MAX_COEF);

		return EXIT_FAILURE;
	}

	in = fopen(inputFile, "r");
	if(!in)
	{
		fprintf(stderr, "Cannot open %s for read\n", inputFile);

		return EXIT_FAILURE;
	}

	for(nPoint = 0; nPoint < MAX_POINTS; ++nPoint)
	{
		int c;
		double mjd;

		fgets(line, MaxLineLength, in);
		if(feof(in))
		{
			break;
		}
		c = sscanf(line, "%*s%*s%lf%*s%*s%lf%*s%*s%lf%lf%lf", &mjd, s+nPoint, x+nPoint, y+nPoint, z+nPoint);
		if(c != 5)
		{
			fprintf(stderr, "Warning: line %d not parsed\n", nPoint+1);
		}
		if(mjd0 < 0)
		{
			mjd0 = mjd;
		}
		t[nPoint] = mjd - mjd0;
	}
	if(nPoint == MAX_POINTS)
	{
		fprintf(stderr, "Warning: truncating read at %d points\n", MAX_POINTS);
	}

	fclose(in);

	fit(t, x, nPoint, coef[0], nCoef);
	fit(t, y, nPoint, coef[1], nCoef);
	fit(t, z, nPoint, coef[2], nCoef);

	printf("# File produced by %s\n", program);
	printf("# Input file: %s\n", inputFile);
	printf("# Coefficients in days since mjd %d are:\n", mjd0);
	for(i = 0; i < nCoef; ++i)
	{
		printf("# d^%d:  %f  %f  %f\n", i, coef[0][i], coef[1][i], coef[2][i]);
	}

	printf("#\n");
	printf("# Column 1: Time (mjd)\n");
	printf("# Column 2: X data (m)\n");
	printf("# Column 3: Y data (m)\n");
	printf("# Column 4: Z data (m)\n");
	printf("# Column 5: X fit (m)\n");
	printf("# Column 6: Y fit (m)\n");
	printf("# Column 7: Z fit (m)\n");

	for(i = 0; i < nPoint; ++i)
	{
		printf("%10.8f  %6.4f %6.4f %6.4f  %6.4f %6.4f %6.4f\n",
			mjd0 + t[i],
			x[i], y[i], z[i],
			evaluatePoly(coef[0], nCoef, t[i]),
			evaluatePoly(coef[1], nCoef, t[i]),
			evaluatePoly(coef[2], nCoef, t[i]));
	}

	return EXIT_SUCCESS;
#else
	fprintf(stderr, "Error: this program was not compiled with GSL and will not run\n");

	return EXIT_FAILURE;
#endif
}
