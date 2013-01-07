/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken & Adam Deller               *
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

#ifndef __POLY_H__
#define __POLY_H__

/* Implement a specific variant of Neville's algorithm for equally spaced
 * data.  The expansion is about x=0 and the input data points are 
 * expected to be at x = d*i for i = [0, n) 
 */
void computePoly(double *p, int n, double d);

double computePoly2(double *p, const double *q, int n, int oversamp, double d, int interpolationType);

void fitPoly(double *p, const double *q, int n, int oversamp, double d);

/* Use Cramer's rule to evaluate polynomial */
double evaluatePoly(const double *p, int n, double x);

double evaluatePolyDeriv(const double *p, int n, double x);

#endif
