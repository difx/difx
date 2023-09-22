/***************************************************************************
 *   Copyright (C) 2008-2017 by Walter Brisken                             *
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
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"

#warning "FIXME: implement RAOBS and DECOBS columns"

const DifxInput *DifxInput2FitsSU(const DifxInput *D, struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	char bandFormDouble[8];
	char bandFormFloat[8];

	struct fitsBinTableColumn columns[] =
	{
		{"SOURCE_ID", "1J", "source id number", 0},
		{"SOURCE", "16A", "source name", 0},
		{"QUAL", "1J", "source qualifier", 0},
		{"CALCODE", "4A", "calibrator code", 0},
		{"FREQID", "1J", "freq id number in frequency tbl", 0},
		{"IFLUX", bandFormFloat, "ipol flux density at ref freq", "JY"},
		{"QFLUX", bandFormFloat, "qpol flux density at ref freq", "JY"},
		{"UFLUX", bandFormFloat, "upol flux density at ref freq", "JY"},
		{"VFLUX", bandFormFloat, "vpol flux density at ref freq", "JY"},
		{"ALPHA", bandFormFloat, "spectral index", 0},
		{"FREQOFF", bandFormFloat, "freq. offset from ref freq.","HZ"},
		{"RAEPO", "1D", "Right Ascension at EQUINOX", "DEGREES"},
		{"DECEPO", "1D", "Declination at EQUINOX", "DEGREES"},
		{"EQUINOX", "8A", "Mean equinox"},
		{"RAAPP", "1D", "apparent RA at 0 UTC ref day", "DEGREES"},
		{"DECAPP", "1D", "apparent Dec at 0 UTC ref day", "DEGREES"},
		{"SYSVEL", bandFormDouble, "systemic velocity at ref pixel", "M/SEC"},
		{"VELTYP", "8A", "velocity type", 0},
		{"VELDEF", "8A", "velocity def: radio, optical", 0},
		{"RESTFREQ", bandFormDouble, "line rest frequency", "HZ"},
		{"PMRA", "1D", "proper motion in RA", "DEG/DAY"},
		{"PMDEC", "1D", "proper motion in Dec", "DEG/DAY"},
		{"PARALLAX", "1E", "parallax of source", "ARCSEC"},
		{"EPOCH", "1D", "Epoch of EQUINOX", "YEARS"},
 		{"RAOBS", "1D", "Pointing RA at EQUINOX", "DEGREES"},
 		{"DECOBS", "1D", "Pointing Dec at EQUINOX", "DEGREES"}
	};

	int nColumn;
	int nRowBytes;
	int nBand;
	int b;
	char *fitsbuf;
	char *p_fitsbuf;
	float fluxI[array_MAX_BANDS];
	float fluxQ[array_MAX_BANDS];
	float fluxU[array_MAX_BANDS];
	float fluxV[array_MAX_BANDS];
	float alpha[array_MAX_BANDS];
	float freqOffset[array_MAX_BANDS];
	char equinox[8];
	double sysVel[array_MAX_BANDS];
	double restFreq[array_MAX_BANDS];
	char velType[8];
	char velDef[8];
	int *fitsSource;
	int *fitsFreqSet;
	int i, nFitsSource;
	int sourceId;

	if(D == 0)
	{
		return 0;
	}

	strcpypad(equinox, "J2000", 8);

	nBand = D->nIF;
	sprintf(bandFormFloat, "%1dE", nBand);
	sprintf(bandFormDouble, "%1dD", nBand); 

	nColumn = NELEMENTS(columns);
	nRowBytes = FitsBinTableSize(columns, nColumn);

	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "SOURCE");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteEnd(out);

	for(i = 0; i < array_MAX_BANDS; ++i)
	{
		freqOffset[i] = 0.0;
	}
	
	/* no knowledge of these from inputs */
	for(b = 0; b < nBand; ++b)
	{
		sysVel[b] = 0.0;
		fluxI[b] = 0.0;
		fluxQ[b] = 0.0;
		fluxU[b] = 0.0;
		fluxV[b] = 0.0;
		alpha[b] = 0.0;
		restFreq[b] = p_fits_keys->ref_freq;
	}

	strcpypad(velType, "GEOCENTR", 8);
	strcpypad(velDef, "OPTICAL", 8);

	fitsbuf = (char *)calloc(nRowBytes, 1);
	if(fitsbuf == 0)
	{
		return 0;
	}
	
	fitsSource = (int *)malloc(D->nSource*D->nFreqSet*sizeof(int));
	fitsFreqSet = (int *)malloc(D->nSource*D->nFreqSet*sizeof(int));
	for(sourceId = 0; sourceId < D->nSource*D->nFreqSet; ++sourceId)
	{
		fitsSource[sourceId] = -1;
	}
	nFitsSource = -1;
	for(sourceId = 0; sourceId < D->nSource; ++sourceId)
	{
		int freqSetId;

		for(freqSetId = 0; freqSetId < D->nFreqSet; ++freqSetId)
		{
			i = D->source[sourceId].fitsSourceIds[freqSetId];
			if(i < 0)
			{
				continue;
			}
			if(fitsSource[i] < 0)
			{
				fitsSource[i] = sourceId;
				fitsFreqSet[i] = freqSetId;
				if(i > nFitsSource)
				{
					nFitsSource = i;
				}
			}
		}
	}
	++nFitsSource;

	for(i = 0; i < nFitsSource; ++i)
	{
		int freqSetId;
		const DifxSource *source;
		double muRA;			/* proper motion */
		double muDec;
		float parallax;
		double epoch;
		double RAEpoch, decEpoch;	/* position of Epoch */
		double RAApp, decApp;		/* apparent position */
		double RAObs, decObs;		/* pointing center of antennas */
		char sourceName[16];
		int32_t sourceId1, freqId1;	/* 1-based indices for FITS file */
		int qual;
		int pointingCentreSrc;
		char calCode[4];
		
		sourceId = fitsSource[i];
		if(sourceId < 0)
		{
			fprintf(stderr, "Developer error: sourceId = -1\n");
			continue;
		}

		source = D->source + sourceId;
		freqSetId = fitsFreqSet[i];
		if(freqSetId < 0 || freqSetId >= D->nFreqSet)
		{
			fprintf(stderr, "Developer error: freqSetId out of range = %d; nFreqSet = %d\n", freqSetId, D->nFreqSet);
			continue;
		}
		
		/* Note: if any of these 4 nubmers are changed, expect AIPS to not handle these correctly */
		muRA = 0.0;
		muDec = 0.0;
		parallax = 0.0;
		epoch = 2000.0;

		sourceId1 = i + 1;	/* FITS sourceId1 is 1-based */
		qual = source->qual;
		strcpypad(calCode, source->calCode, 4);
		RAEpoch = source->ra * 180.0 / M_PI;
		decEpoch = source->dec * 180.0 / M_PI;
		RAApp = RAEpoch;
		decApp = decEpoch;
		pointingCentreSrc = DifxInputGetPointingCentreSource(D, sourceId);
		if(pointingCentreSrc >= 0)
		{
			RAObs = D->source[pointingCentreSrc].ra * 180.0 / M_PI;
			decObs = D->source[pointingCentreSrc].dec * 180.0 / M_PI;
		}
		else
		{
			RAObs = RAEpoch;
			decObs = decEpoch;
		}
		freqId1 = freqSetId + 1;  /* FITS 1-based */
		strcpypad(sourceName, source->name, 16);

		p_fitsbuf = fitsbuf;

		FITS_WRITE_ITEM (sourceId1, p_fitsbuf);
		FITS_WRITE_ITEM (sourceName, p_fitsbuf);
		FITS_WRITE_ITEM (qual, p_fitsbuf);
		FITS_WRITE_ITEM (calCode, p_fitsbuf);
		FITS_WRITE_ITEM (freqId1, p_fitsbuf);
		FITS_WRITE_ARRAY(fluxI, p_fitsbuf, nBand);
		FITS_WRITE_ARRAY(fluxQ, p_fitsbuf, nBand);
		FITS_WRITE_ARRAY(fluxU, p_fitsbuf, nBand);
		FITS_WRITE_ARRAY(fluxV, p_fitsbuf, nBand);
		FITS_WRITE_ARRAY(alpha, p_fitsbuf, nBand);
		FITS_WRITE_ARRAY(freqOffset, p_fitsbuf, nBand);
		FITS_WRITE_ITEM (RAEpoch, p_fitsbuf);
		FITS_WRITE_ITEM (decEpoch, p_fitsbuf);
		FITS_WRITE_ARRAY(equinox, p_fitsbuf, 8);
		FITS_WRITE_ITEM (RAApp, p_fitsbuf);
		FITS_WRITE_ITEM (decApp, p_fitsbuf);
		FITS_WRITE_ARRAY(sysVel, p_fitsbuf, nBand);
		FITS_WRITE_ITEM (velType, p_fitsbuf);
		FITS_WRITE_ITEM (velDef, p_fitsbuf);
		FITS_WRITE_ARRAY(restFreq, p_fitsbuf, nBand);
		FITS_WRITE_ITEM (muRA, p_fitsbuf);
		FITS_WRITE_ITEM (muDec, p_fitsbuf);
		FITS_WRITE_ITEM (parallax, p_fitsbuf);
		FITS_WRITE_ITEM (epoch, p_fitsbuf);
		FITS_WRITE_ITEM (RAObs, p_fitsbuf);
		FITS_WRITE_ITEM (decObs, p_fitsbuf);

		testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "SU");

#ifndef WORDS_BIGENDIAN
		FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
		fitsWriteBinRow(out, fitsbuf);
	}

	free(fitsbuf);
	free(fitsSource);
	free(fitsFreqSet);

	return D;
}	
