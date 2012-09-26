/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken                             *
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

const DifxInput *DifxInput2FitsFR(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	char bandFormDouble[8];
	char bandFormFloat[8];
	char bandFormInt[8];

	struct fitsBinTableColumn columns[] =
	{
		{"FREQID", "1J", "FREQID number in uv data", 0},
		{"BANDFREQ", bandFormDouble, "frequency offset", "HZ"},
		{"CH_WIDTH", bandFormFloat, "spectral channel bandwidth", "HZ"},
		{"TOTAL_BANDWIDTH", bandFormFloat, "total bw of a BAND", "HZ"},
		{"SIDEBAND", bandFormInt, "sideband of each BAND", 0},
		{"BB_CHAN", bandFormInt, "baseband channel number (1-16)", 0}
	};

	int nRowBytes;
	int nColumn;
	char *fitsbuf, *p_fitsbuf;
	double bandFreq[array_MAX_BANDS];
	float chanBW[array_MAX_BANDS];
	float bandBW[array_MAX_BANDS];
	int32_t netSide[array_MAX_BANDS];
	int32_t bbChan[array_MAX_BANDS];
	int configId;
	int nBand;
	int i;
	int32_t freqId1;	/* 1-based index for FITS file */
	const DifxConfig *config;
	const DifxIF *IF;

	if(D == 0)
	{
		return 0;
	}

	nBand = p_fits_keys->no_band;

	sprintf(bandFormDouble, "%1dD", nBand);  
	sprintf(bandFormFloat, "%1dE", nBand);  
	sprintf(bandFormInt, "%1dJ", nBand);  

	nColumn = NELEMENTS(columns);

	/* determine size of FITS record to be written */
	nRowBytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS format */
	fitsbuf = (char *)calloc(nRowBytes, 1);
	if(fitsbuf == 0)
	{
		return 0;
	}
	
	/* spew out the table header */
	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "FREQUENCY");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 2, "");
	fitsWriteEnd(out);

	freqId1 = 0;

	for(configId = 0; configId < D->nConfig; ++configId)
	{
		config = D->config + configId;

		/* only write one row per unique frequency ID */
		if(config->fitsFreqId < freqId1)
		{
			continue;
		}
		freqId1 = config->fitsFreqId + 1;

		for(i = 0; i < nBand; ++i)
		{
			IF = config->IF + i;
			bandFreq[i] = (IF->freq - D->refFreq)*1.0e6;
			chanBW[i] = (IF->bw*D->specAvg/D->nOutChan)*1.0e6;
			bandBW[i] = chanBW[i]*D->nOutChan;
			netSide[i] = ( IF->sideband == 'U' ? 1 : -1 );
			bbChan[i] = 0;	/* vistigial */
			/* correct for skipping some channels */
			bandFreq[i] += netSide[i]*IF->bw*D->startChan*1.0e6/(double)(D->nInChan);
			//printf("Writing IF with freq %f, chanBW %f, bandBW %f\n", bandFreq[i], chanBW[i], bandBW[i]);
		}
		
		/* pointer to the buffer for FITS records */
		p_fitsbuf = fitsbuf;

		FITS_WRITE_ITEM (freqId1, p_fitsbuf);         /* FREQ_ID */
		FITS_WRITE_ARRAY(bandFreq, p_fitsbuf, nBand); /* BANDFREQ */
		FITS_WRITE_ARRAY(chanBW, p_fitsbuf, nBand);   /* CH_WIDTH */
		FITS_WRITE_ARRAY(bandBW, p_fitsbuf, nBand);   /* BANDWIDTH */
		FITS_WRITE_ARRAY(netSide, p_fitsbuf, nBand);  /* SIDEBAND */
		FITS_WRITE_ARRAY(bbChan, p_fitsbuf, nBand);   /* BB_CHAN */

		testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "FR");

#ifndef WORDS_BIGENDIAN
		FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
		fitsWriteBinRow(out, fitsbuf);
	}

	/* free buffer and return */
	free(fitsbuf);

	return D;
}
