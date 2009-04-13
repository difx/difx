/***************************************************************************
 *   Copyright (C) 2007 by Walter Brisken                                  *
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

#ifndef __PARSE_VIS_H__
#define __PARSE_VIS_H__


#include <stdio.h>
#include "parsedifx.h"

#ifdef __cplusplus
#include <complex>
typedef std::complex<float> cplx32f;
#else
#include <complex.h>
typedef float complex cplx32f;
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef struct
{
	FILE *infile;			/* file pointer */
	DifxParameters *params;		/* structure containing text params */
	int nchan;			/* number of channels to expect */
	int visnum;			/* counter of number of vis */
	cplx32f *visdata;		/* pointer to nchan complex values (2x float) */
} DifxVisRecord;

/* open difx file and return pointer to vis structure */
DifxVisRecord *newDifxVisRecord(const char *filename, int nchan);

/* close difx file and delete any allocated resources */
void deleteDifxVisRecord(DifxVisRecord *vis);

/* load next vis record  return -1 if eof */
int DifxVisRecordgetnext(DifxVisRecord *vis);

/* find next occurance of given parameters */
int DifxVisRecordfindnext(DifxVisRecord *vis, int baseline, int freqid,
	const char *pol);

#ifdef __cplusplus
}
#endif


#endif
