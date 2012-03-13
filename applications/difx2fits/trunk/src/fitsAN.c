/***************************************************************************
 *   Copyright (C) 2008-2012 by Walter Brisken                             *
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

const DifxInput *DifxInput2FitsAN(const DifxInput *D, struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	/*  define the antenna characteristic FITS table columns */
	char bandFormFloat[8];
	struct fitsBinTableColumn columns[] =
	{
		{"TIME", "1D", "time of center of interval", "DAYS"},
		{"TIME_INTERVAL", "1E", "row interval", "DAYS"},
		{"ANNAME", "8A", "station name", 0},
		{"ANTENNA_NO", "1J", "antenna number", 0},
		{"ARRAY", "1J", "array id number", 0},
		{"FREQID", "1J", "frequency id number", 0},
		{"NO_LEVELS", "1J", "number of digitizer levels", 0},
		{"POLTYA", "1A", "feed A poln. code", 0},
		{"POLAA", bandFormFloat, "feed A position angle", "DEGREES"},
		{"POLCALA", bandFormFloat, "feed A poln. cal. parameter", 0},
		{"POLTYB", "1A", "feed B poln. code", 0},
		{"POLAB", bandFormFloat, "feed B position angle", "DEGREES"},
		{"POLCALB", bandFormFloat, "feed B poln. cal. parameter", 0}
	};

	int nColumn;
	int nBand;
	int nRowBytes;
	char *fitsbuf;
	double start, stop;
	char *p_fitsbuf;
	int bandId;
	int configId;
	double time;
	float timeInt;
	char polTypeA;
	float polAA[array_MAX_BANDS];
	float polCalA[array_MAX_BANDS];
	char polTypeB;
	float polAB[array_MAX_BANDS];
	float polCalB[array_MAX_BANDS];
	/* 1-based indices for FITS file */
	int32_t arrayId1;

	if(D == 0)
	{
		return 0;
	}

	nColumn = NELEMENTS(columns);
	nBand = D->nIF;
	sprintf(bandFormFloat, "%1dE", nBand);

	nRowBytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS format */
	fitsbuf = (char *)calloc (nRowBytes, 1);
	if(fitsbuf == 0)
	{
		return 0;
	}
	
	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "ANTENNA");

	arrayWriteKeys (p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteInteger(out, "NOPCAL", 0, "");
	fitsWriteString(out, "POLTYPE", "APPROX", "");
	fitsWriteEnd(out);

	start = D->mjdStart - (int)D->mjdStart;
	stop  = D->mjdStop  - (int)D->mjdStart; 

	arrayId1 = 1;
	polTypeA = 'R';
	polTypeB = 'L';
	time = 0.5 * (stop + start);
	timeInt = stop - start;
	for(bandId = 0; bandId < nBand; ++bandId)
	{
		polAA[bandId] = 0.0;
		polCalA[bandId] = 0.0;
		polAB[bandId] = 0.0;
		polCalB[bandId] = 0.0;
	}

	for(configId = 0; configId < D->nConfig; ++configId)
	{
		int antennaId;
		int32_t freqId1 = 0;
		
		if(D->config[configId].fitsFreqId < freqId1)
		{
			continue;	/* already got this freqid */
		}
		freqId1 = D->config[configId].fitsFreqId + 1; /* FITS fqId starts at 1 */
		for(antennaId = 0; antennaId < D->nAntenna; ++antennaId)
		{
			const int maxDatastreams = 8;
			char antName[DIFXIO_NAME_LENGTH];
			int n;
			int dsIds[maxDatastreams];
			int32_t nLevel;
			int32_t antId1;

			n = DifxInputGetDatastreamIdsByAntennaId(dsIds, D, antennaId, maxDatastreams);
			if(n < 1)
			{
				/* This should never happen, but if it does assign the global quantBits */
				nLevel = 1 << (D->quantBits);
				printf("\nWarning: DifxInput2FitsAN: antennaId=%d seems to have no associated datastreams!\n\n                            ", antennaId);
			}
			else
			{
				nLevel = 1 << (D->datastream[dsIds[0]].quantBits);
			}
			/* FIXME: if n > 1, verify all datastreams have same quantization */

			p_fitsbuf = fitsbuf;
			antId1 = antennaId + 1;	  /* FITS antId1 starts at 1 */
			strcpypad(antName, D->antenna[antennaId].name, 8);

			FITS_WRITE_ITEM (time, p_fitsbuf);
			FITS_WRITE_ITEM (timeInt, p_fitsbuf);
			FITS_WRITE_ARRAY(antName, p_fitsbuf, 8);
			FITS_WRITE_ITEM (antId1, p_fitsbuf);
			FITS_WRITE_ITEM (arrayId1, p_fitsbuf);
			FITS_WRITE_ITEM (freqId1, p_fitsbuf);
			FITS_WRITE_ITEM (nLevel, p_fitsbuf);
			FITS_WRITE_ITEM (polTypeA, p_fitsbuf);
			FITS_WRITE_ARRAY(polAA, p_fitsbuf, nBand);
			FITS_WRITE_ARRAY(polCalA, p_fitsbuf, nBand);
			FITS_WRITE_ITEM (polTypeB, p_fitsbuf);
			FITS_WRITE_ARRAY(polAB, p_fitsbuf, nBand);
			FITS_WRITE_ARRAY(polCalB, p_fitsbuf, nBand);

			testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "AN");

#ifndef WORDS_BIGENDIAN
			FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
			fitsWriteBinRow(out, fitsbuf);
		}
	}

	/* clean up and return */
	free(fitsbuf);

	return D;
}
