/***************************************************************************
 *   Copyright (C) 2008 by Walter Brisken                                  *
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
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#ifndef __DIFX_WRITE_H__
#define __DIFX_WRITE_H__

#ifdef __cplusplus
extern "C" {
#endif


#include <stdio.h>
#include "difxio/parsedifx.h"
#include "difxio/difx_input.h"

double truncSeconds(double mjd);

double roundSeconds(double mjd);

int writeDifxLine(FILE *out, const char *key, const char *value);

int writeDifxLine1(FILE *out, const char *key, int i1, const char *value);

int writeDifxLine2(FILE *out, const char *key, int i1, int i2, const char *value);

int writeDifxLine3(FILE *out, const char *key, int i1, int i2, int i3, const char *value);


int writeDifxLineBoolean(FILE *out, const char *key, int value);

int writeDifxLineBoolean1(FILE *out, const char *key, int i1, int value);


int writeDifxLineInt(FILE *out, const char *key, int value);

int writeDifxLineInt1(FILE *out, const char *key, int i1, int value);

int writeDifxLineInt2(FILE *out, const char *key, int i1, int i2, int value);

int writeDifxLineHex(FILE *out, const char *key, unsigned long value);


int writeDifxLineDouble(FILE *out, const char *key, const char *format, double value);

int writeDifxLineDouble1(FILE *out, const char *key, int i1, const char *format, double value);

int writeDifxLineDouble2(FILE *out, const char *key, int i1, int i2, const char *format, double value);


int writeDifxLineArray(FILE *out, const char *key, const double *array, int n);

int writeDifxLineArray1(FILE *out, const char *key, int i1, const double *array, int n);

int writeDifxLineArray2(FILE *out, const char *key, int i1, int i2, const double *array, int n);

int writeDifxLineStringArray(FILE *out, const char *key, const DifxStringArray *sa);

int writeDifxLineStringArray1(FILE *out, const char *key, int i1, const DifxStringArray *sa);

int writeDifxDateLines(FILE *out, double mjd);

#ifdef __cplusplus
}
#endif


#endif
