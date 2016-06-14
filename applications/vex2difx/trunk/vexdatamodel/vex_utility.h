/***************************************************************************
 *   Copyright (C) 2009-2016 by Walter Brisken                             *
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
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/vexdatamodel/util.h $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#ifndef __VEX_UTILITY_H__
#define __VEX_UTILITY_H__

#include <algorithm>

// To capitalize a string
#define Upper(s) transform(s.begin(), s.end(), s.begin(), (int(*)(int))toupper)

// To uncapitalize a string
#define Lower(s) transform(s.begin(), s.end(), s.begin(), (int(*)(int))tolower)

extern "C" {
int fvex_double(char **field, char **units, double *d);
int fvex_ra(char **field, double *ra);
int fvex_dec(char **field, double *dec);
}

int checkCRLF(const char *filename);

//FIXME: add DOYtoMJD from vexload also to timeutils?

//FIXME: move to timeutils?
/* round to nearest second */
double roundSeconds(double mjd);

/* check if an integer is a power of 2 */
bool isPowerOf2(int n);

int nextPowerOf2(int x);

/* Modified from http://www-graphics.stanford.edu/~seander/bithacks.html */
int intlog2(unsigned int v);

char swapPolarizationCode(char pol);

#endif
