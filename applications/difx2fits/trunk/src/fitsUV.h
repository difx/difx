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
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#ifndef __FITS_UV_H__
#define __FITS_UV_H__

#include <complex.h>
#include <math.h>
#include <glob.h>
#include <sys/types.h>
#include "difxio/parsedifx.h"
#include "difx2fits.h"

struct __attribute__((packed)) UVrow
{
	float U, V, W;
	double jd, utc;
	int32_t baseline, filter;
	int32_t sourceId1, freqId1;	/* 1-based FITS indices */
	float intTime;
	/* FIXME -- no pulsar gate id! */
	float data[0];	/* this takes no room in the "sizeof" operation */
};

/* Information useful for tracking properies of visibility records */
typedef struct
{
	glob_t globbuf;
	int curFile, nFile;
	const DifxInput *D;
	DifxParameters *dp;
	FILE *in;
	double U, V, W;
	double mjd, utc;
	double *mjdLastRecord;		/* indexed by zero-based antenna id */
	float tInt;
	int baseline;
	int *antennaIdRemap;		/* to convert baseline number */
	int jobId;
	int configId;
	int sourceId;
	int scanId;
	int freqId;			/* DiFX configId or FITS freqId */
	int bandId;			/* FITS IF index, 0-based */
	int polId;			/* FITS polarization index, 0-based */
	int nPol, nFreq;
	int polStart;			/* start of polarization FITS axis */
	float *spectrum;		/* input visibility spectrum */
	float recweight;
	int nData;
	int nComplex;
	struct UVrow *record;
	float *weight;
	float *data;
	int changed;
	int first;
	double *scale;			/* scale factor, indexed by 0-based antennaId */
	int flagTransition;
	int keepAC;			/* 0 prevents auto-correlations from being preserved */
	int keepXC;			/* 0 prevents cross-correlations from being preserved */
	int pulsarBin;
	int phaseCentre;
	int nRec;			/* number of records read from file */
	int maxRec;			/* maximum number of records to read from file */
	char sideband;			/* sideband, as correlated */
} DifxVis;

DifxVis *newDifxVis(const DifxInput *D, int jobId, int pulsarBin, int phaseCentre, double scaleFactor);
void deleteDifxVis(DifxVis *dv);
int DifxVisNextFile(DifxVis *dv);
int DifxVisNewUVData(DifxVis *dv, int verbose, int skipextraautocorrs);
int DifxVisCollectRandomParams(const DifxVis *dv);


#endif
