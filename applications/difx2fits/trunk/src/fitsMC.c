/***************************************************************************
 *   Copyright (C) 2008-2017 by Walter Brisken & Adam Deller               *
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

const DifxInput *DifxInput2FitsMC(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, int phaseCentre)
{
	char bandFormFloat[8];

	struct fitsBinTableColumn columns[] =
	{
		{"TIME", "1D", "Time of center of interval", "DAYS"},
		{"SOURCE_ID", "1J", "source id from sources tbl", 0},
		{"ANTENNA_NO", "1J", "antenna id from antennas tbl", 0},
		{"ARRAY", "1J", "array id number", 0},
		{"FREQID", "1J", "freq id from frequency tbl", 0},
		{"ATMOS", "1D", "atmospheric group delay", "SECONDS"},
		{"DATMOS", "1D", "atmospheric group delay rate", "SEC/SEC"},
		{"GDELAY", "1D", "CALC geometric delay", "SECONDS"},
		{"GRATE", "1D", "CALC geometric delay rate", "SEC/SEC"},
		{"CLOCK_1", "1D", "electronic delay", "SECONDS"},
		{"DCLOCK_1", "1D", "electronic delay rate", "SEC/SEC"},
		{"LO_OFFSET_1", bandFormFloat, "station lo_offset for polar. 1", "HZ"},
		{"DLO_OFFSET_1", bandFormFloat, "station lo_offset rate for polar. 1", "HZ/SEC"},
		{"DISP_1", "1E", "dispersive delay", "SECONDS"},
		{"DDISP_1", "1E", "dispersive delay rate", "SEC/SEC"},
		{"CLOCK_2", "1D", "electronic delay", "SECONDS"},
		{"DCLOCK_2", "1D", "electronic delay rate", "SEC/SEC"},
		{"LO_OFFSET_2", bandFormFloat, "station lo_offset for polar. 2", "HZ"},
		{"DLO_OFFSET_2", bandFormFloat, "station lo_offset rate for polar. 2", "HZ/SEC"},
		{"DISP_2", "1E", "dispersive delay for polar 2", "SECONDS"},
		{"DDISP_2", "1E", "dispersive delay rate for polar 2", "SEC/SEC"}
	};

	int nColumn;
 	int nRowBytes;
	char *p_fitsbuf, *fitsbuf;
	int nBand, nPol;
	int b, j, s, p, np, a;
	float LOOffset[array_MAX_BANDS];
	float LORate[array_MAX_BANDS];
	float dispDelay;
	float dispDelayRate;
	const DifxConfig *config;
	const DifxScan *scan;
	const DifxPolyModel *P;
	double time;
	double delay, delayRate;
	double atmosDelay, atmosRate;
	double clock, clockRate, c1, c2;
	int configId, dsId, antId;
	int *skip;
	int skipped=0;
	int printed=0;
	/* 1-based indices for FITS file */
	int32_t antId1, arrayId1, sourceId1, freqId1;
	int polyDuration = 0;

	if(D == 0)
	{
		return 0;
	}

	nBand = p_fits_keys->no_band;
	sprintf (bandFormFloat, "%1dE", nBand);  
  
	nPol = D->nPol;
	if(nPol == 2)
	{
		nColumn = NELEMENTS(columns);
	}
	else	/* don't populate last 6 columns if not full polar */
	{
		nColumn = NELEMENTS(columns) - 6;
	}
	nRowBytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS order */
	fitsbuf = (char *)calloc(nRowBytes, 1);
	if(fitsbuf == 0)
	{
		return 0;
	}
  
	for(a = 0; a < D->nAntenna; ++a)
	{
		if(D->scan->im[a])
		{
			polyDuration = D->scan->im[a][0][0].validDuration;
			break;
		}
	}

	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "MODEL_COMPS");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "NO_POL", nPol, "");
	fitsWriteInteger(out, "FFT_SIZE", D->nInChan*2, "");
	fitsWriteInteger(out, "OVERSAMP", 0, "");
	fitsWriteInteger(out, "ZERO_PAD", 0, "");
	fitsWriteInteger(out, "FFT_TWID", 1, "Version of FFT twiddle table used");
	fitsWriteString(out, "TAPER_FN", taperFunctionNames[D->job->taperFunction], "");
	fitsWriteFloat(out, "DELTAT", polyDuration/86400.0, "DAYS");
	fitsWriteInteger(out, "TABREV", 1, "");
	
	fitsWriteEnd(out);

	arrayId1 = 1;

	/* some values that are always zero */
	for(b = 0; b < nBand; ++b)
	{
		LOOffset[b] = 0.0;
		LORate[b] = 0.0;
	}

	dispDelay = 0.0;
	dispDelayRate = 0.0;

	skip = (int *)calloc(D->nAntenna, sizeof(int));

	for(s = 0; s < D->nScan; ++s)
	{
		scan = D->scan + s;
		configId = scan->configId;
		if(configId < 0)
		{
			continue;
		}
		if(phaseCentre >= scan->nPhaseCentres)
		{
			continue;
		}
		config = D->config + configId;
		freqId1 = config->freqSetId + 1;
		sourceId1 = D->source[scan->phsCentreSrcs[phaseCentre]].fitsSourceIds[config->freqSetId] + 1;

		if(scan->im)
		{
			np = scan->nPoly;
		}
		else
		{
			fprintf(stderr, "No IM table available for scan %d; aborting MC file creation\n", s);
			continue;
		}

		for(p = 0; p < np; ++p)
		{
			/* loop over original .input file antenna list */
			for(a = 0; a < config->nAntenna; ++a)
			{
				DifxAntenna *da;

				dsId = config->ant2dsId[a];
				if(dsId < 0 || dsId >= D->nDatastream)
				{
					continue;
				}
				/* convert to D->antenna[] index ... */
				antId = D->datastream[dsId].antennaId;

				if(antId < 0 || antId >= scan->nAntenna)
				{
					continue;
				}

				da = D->antenna + antId;	/* pointer to DifxAntenna structure */

				/* ... and to FITS antennaId */
				antId1 = antId + 1;

				if(scan->im[antId] == 0)
				{
					if(skip[antId] == 0)
					{
						printf("\n    Polynomial model error : skipping antId %d = %s", 
						antId, da->name);
						++skip[antId];
						++printed;
						++skipped;
					}
					continue;
				}

				P = scan->im[antId][phaseCentre] + p;

				time = P->mjd - (int)(D->mjdStart) + P->sec/86400.0;

				/* in general, convert from (us) to (sec) */
				atmosDelay = (P->dry[0] + P->wet[0])*1.0e-6;
				atmosRate  = (P->dry[1] + P->wet[1])*1.0e-6;

				/* here correct the sign of delay, and remove atmospheric
				* portion of it. */
				delay     = -P->delay[0]*1.0e-6 - atmosDelay;
				delayRate = -P->delay[1]*1.0e-6 - atmosRate;

				c1 = evaluateDifxAntennaClock(da, P->mjd + P->sec/86400.0);
				c2 = evaluateDifxAntennaClock(da, P->mjd + (P->sec + P->validDuration)/86400.0);
				clockRate = ((c2-c1)/P->validDuration)*1.0e-6;
				clock     = c1*1.0e-6;

				p_fitsbuf = fitsbuf;

				FITS_WRITE_ITEM (time, p_fitsbuf);
				FITS_WRITE_ITEM (sourceId1, p_fitsbuf);
				FITS_WRITE_ITEM (antId1, p_fitsbuf);
				FITS_WRITE_ITEM (arrayId1, p_fitsbuf);
				FITS_WRITE_ITEM (freqId1, p_fitsbuf);
				FITS_WRITE_ITEM (atmosDelay, p_fitsbuf);
				FITS_WRITE_ITEM (atmosRate, p_fitsbuf);
				FITS_WRITE_ITEM (delay, p_fitsbuf);
				FITS_WRITE_ITEM (delayRate, p_fitsbuf);

				for(j = 0; j < nPol; ++j)
				{
					FITS_WRITE_ITEM (clock, p_fitsbuf);
					FITS_WRITE_ITEM (clockRate, p_fitsbuf);
					FITS_WRITE_ARRAY(LOOffset, p_fitsbuf, nBand);
					FITS_WRITE_ARRAY(LORate, p_fitsbuf, nBand);
					FITS_WRITE_ITEM (dispDelay, p_fitsbuf);
					FITS_WRITE_ITEM (dispDelayRate, p_fitsbuf);
				} /* Polar loop */

				testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "MC");

				#ifndef WORDS_BIGENDIAN
				FitsBinRowByteSwap(columns, nColumn, fitsbuf);
				#endif
				fitsWriteBinRow(out, fitsbuf);
			} /* Antenna loop */
		} /* Intervals in scan loop */
	} /* Scan loop */

	if(printed)
	{
		printf("\n                            ");
	}
  
	/* release buffer space */
	free(fitsbuf);
	free(skip);

	return D;
}
