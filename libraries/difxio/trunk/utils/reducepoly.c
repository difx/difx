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
#include <gsl/gsl_multifit.h>
#include "difx_input.h"

const char program[] = "reducepoly";
const char author[]  = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.1";
const char verdate[] = "20150708";

void usage()
{
	printf("%s  ver. %s  %s  %s\n\n", program, version, author, verdate);
	printf("A program to reduce the polynomial order of the delay model\n\n");
	printf("Usage : %s [options] <inputfilebase1> [ <inputfilebase2> [...] ]\n\n", program);
	printf("options can include:\n");
	printf("--help\n");
	printf("-h         print help information and quit\n\n");
	printf("-2\n");
	printf("-3\n");
	printf("-4\n");
	printf("-5         reduce polynomial to 2, 3, 4, or 5 terms\n\n");
	printf("Note: This overwrites the original .im file(s)\n\n");
}

enum Item
{
	ItemDelay = 0,
	ItemAz,
	ItemEl,
	ItemDry,
	ItemWet,
	ItemUVW
};

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
void fitPoly(double *p, const double *q, int n, int nq, double d)
{
	static int binomial[MAX_MODEL_ORDER+1][MAX_MODEL_ORDER+1];
	static int first = 1;
	gsl_multifit_linear_workspace * work;
	gsl_matrix *X, *cov;
	gsl_vector *y, *w, *c;
	int nData = nq-2;
	int nParam = n-2;
	double C, qn;
	double delta;
	double chisq;
	double dfac;
	int i, j, k;

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

	X = gsl_matrix_alloc(nData, nParam);
	y = gsl_vector_alloc(nData);
	w = gsl_vector_alloc(nData);

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

void reducePoly(DifxInput *D, int nPoly)
{
	int s;
	
	for(s = 0; s < D->nScan; ++s)
	{
		DifxScan *ds;
		int refAnt;	/* points to a valid antenna in this poly */
		int p, i;

		ds = D->scan + s;

		printf("# scan %d of %d: source = %s\n", s+1, D->nScan, D->source[ds->phsCentreSrcs[0]].name);

		if(!ds->im)
		{
			printf("#   No IM table for this scan\n");

			continue;
		}

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

			printf("#   No delays for this scan!\n");

			continue;
		}

		for(p = 0; p < ds->nPoly; ++p)
		{
			int a;

			for(a = 0; a < D->nAntenna; ++a)
			{
				if(ds->im[a])
				{
					int pc;

					for(pc = 0; pc <= ds->nPhaseCentres; ++pc)
					{
						double data[8];
						double poly[8];
						double dt;
						int nData;
						int start;

						dt = (double)(ds->im[a][pc][p].validDuration)/ds->im[a][pc][p].order;
						nData = ds->im[a][pc][p].order+1;
						start = 1;

						for(i = 0; i < nData; ++i)
						{
							data[i+start] = evaluatePoly(ds->im[a][pc][p].delay, ds->im[a][pc][p].order+1, dt*i);
						}
						/* commented out until a better fitting routine can be found 
						if(p > 0)
						{
							start = 0;
							++nData;
							data[0] = evaluatePoly(ds->im[a][pc][p-1].delay, ds->im[a][pc][p-1].order+1, ds->im[a][0][p-1].validDuration-dt);
						}
						if(p < ds->nPoly-1)
						{
							data[start+nData] = evaluatePoly(ds->im[a][pc][p+1].delay, ds->im[a][pc][p+1].order+1, dt);
							++nData;
						}
						*/
						if(nPoly == 2)
						{
							poly[0] = data[start];
							poly[1] = (data[start+nData-1]-data[start])/(dt*(nData-1));
						}
						else
						{
							fitPoly(poly, data+start, nPoly, nData, dt);
						}
						for(i = 0; i < nPoly; ++i)
						{
							ds->im[a][pc][p].delay[i] = poly[i];
						}
						for(; i <= ds->im[a][pc][p].order; ++i)
						{
							ds->im[a][pc][p].delay[i] = 0.0;
						}
					}
				}
			}
		}
	}
	writeDifxIM(D);
}

int main(int argc, char **argv)
{
	int a;
	int nPoly = 4;	/* order+1 of the destination polynomial */
	
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
			else if(strcmp(argv[a], "-2") == 0)
			{
				nPoly = 2;
			}
			else if(strcmp(argv[a], "-3") == 0)
			{
				nPoly = 3;
			}
			else if(strcmp(argv[a], "-4") == 0)
			{
				nPoly = 4;
			}
			else if(strcmp(argv[a], "-5") == 0)
			{
				nPoly = 5;
			}
			else
			{
				fprintf(stderr, "Unknown option %s\n", argv[a]);

				exit(EXIT_FAILURE);
			}
		}
		else 
		{
			DifxInput *D;
			
			D = loadDifxInput(argv[a]);
			D = updateDifxInput(D, 0);
			if(!D)
			{
				fprintf(stderr, "Update failed: D == 0.  Quitting\n");
				
				return EXIT_FAILURE;
			}

			reducePoly(D, nPoly);

			deleteDifxInput(D);
		}
	}

	return EXIT_SUCCESS;
}
