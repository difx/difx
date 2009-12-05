/***************************************************************************
 *   Copyright (C) 2008, 2009 by Walter Brisken                            *
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
#include "other.h"


/* ant D.O.Y. dur(days) nRecChan (tsys, bandName)[nRecChan] */
static int parseTsys(const char *line, char *antName, 
	double *time, float *timeInt, float tSys[])
{
	int p;
	int n, i, nRecChan;

	n = sscanf(line, "%s%lf%f%d%n", antName, time, timeInt, &nRecChan, &p);
	if(n != 4)
	{
		return -1;
	}

	for(i = 0; i < nRecChan; i++)
	{
		line += p;
		n = sscanf(line, "%f%*s%n", tSys + i, &p);
		if(n != 1)
		{
			return -2;
		}
	}
	
	return nRecChan;
}

const DifxInput *DifxInput2FitsTS(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out,
	int phasecentre)
{
	char bandFormFloat[4];
	
	/*  define the flag FITS table columns */
	struct fitsBinTableColumn columns[] =
	{
		{"TIME", "1D", "time of center of interval", "DAYS"},
		{"TIME_INTERVAL", "1E", "time span of datum", "DAYS"},
		{"SOURCE_ID", "1J", "source id number from source tbl", 0},
		{"ANTENNA_NO", "1J", "antenna id from array geom. tbl", 0},
		{"ARRAY", "1J", "????", 0},
		{"FREQID", "1J", "freq id from frequency tbl", 0},
		{"TSYS_1", bandFormFloat, "system temperature", "K"},
		{"TANT_1", bandFormFloat, "antenna temperature", "K"},
		{"TSYS_2", bandFormFloat, "system temperature", "K"},
		{"TANT_2", bandFormFloat, "antenna temperature", "K"}
	};

	int nColumn;
	int nRowBytes;
	char *fitsbuf, *p_fitsbuf;
	int refDay;
	char line[1000];
	char antName[20];
	float tSysRecChan[array_MAX_BANDS];
	float tSys[2][array_MAX_BANDS];
	float tAnt[2][array_MAX_BANDS];
	int nBand;
	int configId, sourceId, scanId;
	int i, j, nPol=0;
	int bandId, polId, antId;
	int nRecChan;
	int v;
	double f;
	double time, mjd;
	float timeInt;
	const DifxScan *scan;
	FILE *in;
	char *rv;
	/* The following are 1-based indices for writing to FITS */
	int32_t sourceId1, freqId1, arrayId1, antId1;

	/* a portable way to set NaNs that is compatible with FITS */
	union
	{
		int32_t i32;
		float f;
	} nan;
	nan.i32 = -1;
	
	if(D == 0)
	{
		return D;
	}

	in = fopen("tsys", "r");
	if(!in)
	{
		return D;
	}

	nBand = D->nIF;
	nPol = D->nPol;

	sprintf(bandFormFloat, "%dE", nBand);

	mjd2dayno((int)(D->mjdStart), &refDay);

	/* get the maximum dimensions possibly needed */
	f = D->mjdStart - (int)(D->mjdStart);

	if(nPol == 2)
	{
		nColumn = NELEMENTS(columns);
	}
	else
	{
		nColumn = NELEMENTS(columns) - 2;
	}
	
	nRowBytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS format */
	fitsbuf = (char *)calloc(nRowBytes, 1);
	if(fitsbuf == 0)
	{
		return 0;
	}

	fitsWriteBinTable(out, nColumn, columns, nRowBytes, 
		"SYSTEM_TEMPERATURE");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "NO_POL", nPol, "");
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteEnd(out);

	freqId1 = 1;
	arrayId1 = 1;
	for(i = 0; i < 16; i++)
	{
		tAnt[0][i] = nan.f;	/* set to NaN */
		tAnt[1][i] = nan.f;	/* set to NaN */
	}
	
	for(;;)
	{
		rv = fgets(line, 999, in);
		if(!rv)
		{
			break;
		}
			
		/* ignore possible comment lines */
		if(line[0] == '#')
		{
			continue;
		}
		else 
		{
			nRecChan = parseTsys(line, antName, &time, 
				&timeInt, tSysRecChan);

			/* discard records for unused antennas */
			antId = DifxInputGetAntennaId(D, antName);
			if(antId < 0)
			{
				continue;
			}

			/* discard records outside time range */
			time -= refDay;
			mjd = time + (int)(D->mjdStart);
			scanId = DifxInputGetScanIdByAntennaId(D, mjd, antId);
			if(scanId < 0)
			{
				continue;
			}
			scan = D->scan + scanId;

			if(phasecentre >= scan->nPhaseCentres)
			{
				printf("Skipping scan %d as the requested phase centre was not used\n", scanId);
				continue;
			}

			sourceId = scan->phsCentreSrcs[phasecentre];
			if(sourceId < 0 || mjd < D->mjdStart || 
				mjd > D->mjdStop)
			{
				continue;
			}

			configId = scan->configId;
			freqId1 = D->config[configId].freqId + 1;

			for(j = 0; j < 2; j++)
			{
				for(i = 0; i < 16; i++)
				{
					tSys[j][i] = 0;
				}
			}

			/* Take the recorder channel order data and populate
			 * into [polId][bandId] order
			 */
			for(i = 0; i < nRecChan; i++)
			{
				v = DifxConfigRecChan2IFPol(D, configId,
					antId, i, &bandId, &polId);
				if(v < 0)
				{
					continue;
				}
				if(bandId < 0 || polId < 0)
				{
					fprintf(stderr, "Error: derived "
						"bandId and polId (%d,%d) are "
						"not legit.  From "
						"recChan=%d.\n", 
						bandId, polId, i);
				}
				if(tSysRecChan[i] < 990.0)
				{
					tSys[polId][bandId] = tSysRecChan[i];
				}
				else
				{
					tSys[polId][bandId] = nan.f;
				}
			}

			/* 1-based values for FITS */
			antId1 = antId + 1;
			sourceId1 = D->source[sourceId].fitsSourceId + 1;
		
			p_fitsbuf = fitsbuf;
		
			FITS_WRITE_ITEM (time, p_fitsbuf);
			FITS_WRITE_ITEM (timeInt, p_fitsbuf);
			FITS_WRITE_ITEM (sourceId1, p_fitsbuf);
			FITS_WRITE_ITEM (antId1, p_fitsbuf);
			FITS_WRITE_ITEM (arrayId1, p_fitsbuf);
			FITS_WRITE_ITEM (freqId1, p_fitsbuf);

			for(i = 0; i < nPol; i++)
			{
				FITS_WRITE_ARRAY(tSys[i], p_fitsbuf, nBand);
				FITS_WRITE_ARRAY(tAnt[i], p_fitsbuf, nBand);
			}

			testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "TS");

#ifndef WORDS_BIGENDIAN
			FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
			fitsWriteBinRow(out, fitsbuf);
		}
	}

	/* close the file, free memory, and return */
	fclose(in);
	free(fitsbuf);

	return D;
}
