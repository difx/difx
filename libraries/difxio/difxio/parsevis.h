/***************************************************************************
 *   Copyright (C) 2007-2010 by Walter Brisken & Adam Deller               *
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


#define VISRECORD_SYNC_WORD_DIFX1	(('B' << 24) + ('A' << 16) + ('S' << 8) + 'E')
#define VISRECORD_SYNC_WORD_DIFX2	0xFF00FF00


typedef struct
{
	FILE *infile;			/* file pointer */
	DifxParameters *params;		/* structure containing text params */
	int nchan;			/* number of channels to expect */
	int visnum;			/* counter of number of vis */
	int sync;			/* space to store the sync value */
        int headerversion;		/* 0=old style, 1=new binary style */
	int baseline;			/* The baseline number (256*A1 + A2, 1 indexed) */
	int mjd;			/* The MJD integer day */
	double seconds;			/* The seconds offset from mjd */
	int configindex;		/* The index to the configuration table */
	int sourceindex;		/* The index to the source table */
	int freqindex;			/* The index to the freq table */
	char polpair[3];		/* The polarisation pair */
	int pulsarbin;			/* The pulsar bin */
	double dataweight;		/* The fractional data weight */
	double uvw[3];			/* The u,v,w values in metres */
	cplx32f *visdata;		/* pointer to nchan complex values (2x float) */
} DifxVisRecord;

/* open difx file and return pointer to vis structure */
DifxVisRecord *newDifxVisRecord(const char *filename, int nchan);

/* close difx file and delete any allocated resources */
void deleteDifxVisRecord(DifxVisRecord *vis);

/* load next vis record  return -1 if eof */
int DifxVisRecordgetnext(DifxVisRecord *vis);

/* find next occurrence of given parameters */
int DifxVisRecordfindnext(DifxVisRecord *vis, int baseline, int freqid,
	const char *pol);

#ifdef __cplusplus
}
#endif


#endif
