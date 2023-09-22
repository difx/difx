/***************************************************************************
 *   Copyright (C) 2008-2021 by Walter Brisken & Adam Deller               *
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
/*
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: poly.c 5101 2013-01-07 22:30:19Z WalterBrisken $
// $HeadURL: $
// $LastChangedRevision: 5101 $
// $Author: WalterBrisken $
// $LastChangedDate: 2013-01-07 15:30:19 -0700 (Mon, 07 Jan 2013) $
//
//============================================================================
         */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_multifit.h>
#include "poly.h"

#define MAX_MODEL_ORDER 12

/* Implement a specific variant of Neville's algorithm for equally spaced
 * data.  The expansion is about x=0 and the input data points are 
 * expected to be at x = d*i for i = [0, n) 
 */
void computePoly(double *p, int n, double d)
{
	long double C[MAX_MODEL_ORDER+1][MAX_MODEL_ORDER+1];
	int g, h, i;

	for(h = 0; h < n; h++)
	{
		C[h][0] = p[h];
	}

	for(g = 1; g < n; g++)
	{
		for(h = 0; h < n-g; h++)
		{
			C[h][g] = 0.0;
			for(i = g-1; i >= 0; i--)
			{
				C[h][i+1] += (C[h+1][i] - C[h][i])/(g*d);
				C[h][i]    = (C[h][i]*(h+g) - C[h+1][i]*h)/g;
			}
		}
	}

	for(i = 0; i < n; i++)
	{
		p[i] = C[0][i];
	}
}


/* This routine takes generally more data points than there are parameters to fit (i.e., oversamp > 1)
   and does a least squares fit to that data.  The two end points are constrained; that is, the
   polynomial is forced to go through these points.

   The fitting process is done in the Berenstein polynomial basis where the first and last terms
   (corresponding to (1-t)^(n-1) and t^(n-1) ) are uniquely defined by the first and last data points.
   Once the Berenstein coefficients are known a binomial transform turns them into coefficients for
   t^k .

   Note that to improve numerical precision a constant value corresponding to the initial value is
   subtracted during the firring process and added back at the very end.
*/
void fitpoly_(double *p, const double *q, int n, int oversamp, double d)
{
	static int binomial[MAX_MODEL_ORDER+1][MAX_MODEL_ORDER+1];
	static int first = 1;
	gsl_multifit_linear_workspace * work;
	gsl_matrix *X, *cov;
	gsl_vector *y, *w, *c;
	int nData = (n-1)*oversamp-1;
	int nParam = n-2;
	double C, qn;
	double delta;
	double chisq;
	double dfac;
	int i, j, k;

/*         printf("1: %d %d %f\n", n, oversamp, d);
           printf("2: %f %f %f %f %f %f\n", q[0], q[1], q[2], q[3], q[4], q[5]);     */

	if(first)
	{
		first = 0;
		for(j = 0; j <= MAX_MODEL_ORDER; ++j)
		{
			for(i = 0; i <= MAX_MODEL_ORDER; ++i)
			{
				if(i == 0)
				{
					binomial[j][i] = 1;
				}
				else
				{
					binomial[j][i] = 0;
				}
			}
		}
		for(j = 1; j <= MAX_MODEL_ORDER; ++j)
		{
			for(i = 1; i <= j; ++i)
			{
				binomial[j][i] = binomial[j-1][i-1] + binomial[j-1][i];
			}
		}
	}

	delta = d*(nData+1);
/*         printf("3: %f %f %d\n", delta, d, nData);     */

	X = gsl_matrix_alloc(nData, nParam);
	y = gsl_vector_alloc(nData);
	w = gsl_vector_alloc(nData);
/*         printf("4: %f %f %d\n", delta, d, nData);     */

	cov = gsl_matrix_alloc(nParam, nParam);
	c = gsl_vector_alloc(nParam);

	C = q[0];
	qn = q[nData+1] - C;
	
	for(i = 1; i <= nData; ++i)
	{
		double tau = i/(nData+1.0);
		double taun = 1.0;

		/* In here the Berenstein polynomial coefficients are implicitly set */
		for(j = 1; j < n-1; ++j)
		{
			double b = 1.0;
			for(k = 0; k < j; ++k)
			{
				b *= tau;
			}
			for(k = j; k < n - 1; ++k)
			{
				b *= (1.0 - tau);
			}
			gsl_matrix_set(X, i-1, j-1, b*binomial[n-1][j]);
		}

		for(k = 1; k < n; ++k)
		{
			taun *= tau;
		}

		gsl_vector_set(y, i-1, q[i] - C - qn*taun);
		gsl_vector_set(w, i-1, 1.0);
	}

	work = gsl_multifit_linear_alloc(nData, nParam);
	gsl_multifit_wlinear(X, w, y, c, cov, &chisq, work);
	gsl_multifit_linear_free(work);

	/* Collect coefficients of (1-t)^(n-k)*t^k */
	p[0] = 0.0;
	for(i = 1; i < n-1; ++i)
	{
		p[i] = gsl_vector_get(c, i-1);
	}
	p[n-1] = qn;

	/* apply binomial transform */
	for(j = 1; j < n; ++j)
	{
		for(i = n-1; i >= j; --i)
		{
			p[i] -= p[i-1];
		}
	}
	for(i = 0; i < n; ++i)
	{
		p[i] *= binomial[n-1][i];
	}

	/* put back the DC bias removed early in the process */
	p[0] += C;

	/* scale into correct domain coordinates */
	dfac = 1.0;
	for(i = 1; i < n; ++i)
	{
		dfac /= delta;
		p[i] *= dfac;
	}

	gsl_matrix_free(X);
	gsl_vector_free(y);
	gsl_vector_free(w);
	gsl_matrix_free(cov);
	gsl_vector_free(c);
}

double computePoly2(double *p, const double *q, int n, int oversamp, double d, int interpolationType)
{
	double r = 0.0;
	int i;

	switch(interpolationType)
	{
	case 0:	
		for(i = 0; i < n; ++i)
		{
			p[i] = q[i*oversamp];
		}
		/* use the computePoly function above */
		computePoly(p, n, d*oversamp);
		break;
	case 1:
		/* Look at http://www.gnu.org/software/gsl/manual/html_node/Fitting-Examples.html */

		fitpoly_(p, q, n, oversamp, d);

		break;
	default:
		fprintf(stderr, "Error: computePoly2: interpolationType=%d doesn't exist\n", interpolationType);
		r = -1.0;
	}

	/* determine greatest interpolation error */
	for(i = 0; i <= (n-1)*oversamp; ++i)
	{
		double v = evaluatePoly(p, n, d*i);
		double e = fabs(v-q[i]);
		if(e > r)
		{
			r = e;
		}
	}

	return r;
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

#if 0
int main(int argc, char **argv)
/*
int mane(int argc, char **argv)
    */
{
	double p[10], d[10], dd[10];
	int i, n=0;
	double delta = 1.0;

	for(i = 1; i < argc; i++)
	{
		p[i-1] = atof(argv[i]);
		n++;
	}

	for(i = 0; i < n; i++)
	{
		d[i] = evaluatePoly(p, n, i*delta);
		dd[i] = evaluatePolyDeriv(p, n, i*delta);
	}

	for(i = 0; i < n; i++)
	{
		printf("%d %f %f %f\n", i, p[i], d[i], dd[i]);
	}

	printf("\n");

	computePoly(d, n, delta);

	for(i = 0; i < n; i++)
	{
		printf("%d %f %f %e\n", i, p[i], d[i], p[i]-d[i]);
	}

	return 0;
}
#endif
