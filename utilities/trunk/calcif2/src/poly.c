/***************************************************************************
 *   Copyright (C) 2008-2012 by Walter Brisken & Adam Deller               *
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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <difxio.h>	/* only needed for MAX_MODEL_ORDER */
#include "poly.h"

#ifndef MAX_MODEL_ORDER
#define MAX_MODEL_ORDER 10
#endif

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

double computePoly2(double *p, const double *q, int n, int oversamp, double d, int interpolationType)
{
	double r = 0.0;
	int i;

	switch(interpolationType)
	{
	case 0:	
	case 1:
		for(i = 0; i < n; ++i)
		{
			p[i] = q[i*oversamp];
		}
		/* use the computePoly function above */
		computePoly(p, n, d*oversamp);
		break;
	case 2:
		/* Look at http://www.gnu.org/software/gsl/manual/html_node/Fitting-Examples.html */
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
