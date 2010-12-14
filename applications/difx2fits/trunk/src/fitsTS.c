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

static float tAnt[2][array_MAX_BANDS];

void nanify(float X[2][array_MAX_BANDS])
{
	int i;

	/* a portable way to set NaNs that is compatible with FITS */
	union
	{
		int32_t i32;
		float f;
	} nan;
	nan.i32 = -1;
	
	for(i = 0; i < array_MAX_BANDS; i++)
	{
		X[0][i] = nan.f;
		X[1][i] = nan.f;
	}
}

/* ant D.O.Y. dur(days) nRecBand (tsys, bandName)[nRecBand] */
static int parseTsys(const char *line, char *antName, 
	double *time, float *timeInt, float tSys[])
{
	int p;
	int n, i, nRecBand;

	n = sscanf(line, "%s%lf%f%d%n", antName, time, timeInt, &nRecBand, &p);
	if(n != 4)
	{
		return -1;
	}

	for(i = 0; i < nRecBand; i++)
	{
		line += p;
		n = sscanf(line, "%f%*s%n", tSys + i, &p);
		if(n != 1)
		{
			return -2;
		}
	}
	
	return nRecBand;
}

typedef struct
{
	double pOn, wOn, pOff, wOff;
} SwitchedPower;

static int readSwitchedPower(const char *line, double *mjd1, double *mjd2, SwitchedPower *sp, int max)
{
	int i, n, p;

	n = sscanf(line, "%lf%lf%n", mjd1, mjd2, &p);
	if(n != 2)
	{
		return -1;
	}
	line += p;

	for(i = 0; i < max; i++)
	{
		n = sscanf(line, "%lf%lf%lf%lf%n", &sp[i].pOn, &sp[i].wOn, &sp[i].pOff, &sp[i].wOff, &p);
		line += p;
		if(n <= 0)
		{
			break;
		}
		if(n != 4)
		{
			return -1;
		}
	}

	return i;
}

static void clearSwitchedPower(SwitchedPower *sp, int n)
{
	int i;

	for(i = 0; i < n; i++)
	{
		sp[i].pOn = sp[i].pOff = sp[i].wOn = sp[i].wOff = 0.0;
	}
}

static void accumulateSwitchedPower(SwitchedPower *average, const SwitchedPower *meas, int n)
{
	double w;
	int i;

	for(i = 0; i < n; i++)
	{
		w = 1.0/(meas[i].wOn*meas[i].wOn);
		average[i].pOn += w*meas[i].pOn;
		average[i].wOn += w;

		w = 1.0/(meas[i].wOff*meas[i].wOff);
		average[i].pOff += w*meas[i].pOff;
		average[i].wOff += w;
	}
}

static double unscaledTsys(const SwitchedPower *sp)
{
	double a, b;

	a = sp->pOn/sp->wOn;
	b = sp->pOff/sp->wOff;

	return 0.5*(a+b)/(a-b);
}

int getDifxTsys(const DifxInput *D, int datastreamId, double avgSeconds, int phasecentre, 
	int nRowBytes, char *fitsbuf, int nColumn, const struct fitsBinTableColumn *columns,
	struct fitsPrivate *out)
{
	const int MaxFilenameLength=256;
	const int MaxLineLength=1000;
	char line[MaxLineLength];
	char filename[MaxFilenameLength];
	char *p_fitsbuf;
	FILE *in;
	SwitchedPower measurement[array_MAX_BANDS*2];
	SwitchedPower average[array_MAX_BANDS*2];
//	double pCal[];
	double mjd1, mjd2, mjd;
	double accumStart = -1, accumEnd = -1;
	int antId;
	int configId = -1;
	int scanId = -1;
	int i, v, n;
	int nBand = -1;;
	char *rv;
	int currentScanId = -1;
	int currentConfigId = -1;
	int nAccum = 0;
	int doDump = 0;
	double windowDuration = 0;	/* in days */
	double dumpWindow = 0;		/* mjd of end of window */
	DifxScan *scan;

	v = snprintf(filename, MaxFilenameLength, 
		"%s/SWITCHEDPOWER_%d", D->outputFile, datastreamId);
	if(v >= MaxFilenameLength)
	{
		fprintf(stderr, "Developer error: getDifxTsys filename length wanted to be %d bytes long, not %d\n",
			v, MaxFilenameLength);
		
		return -1;
	}
	in = fopen(filename, "r");
	if(!in)
	{
		return -1;
	}

	antId = D->datastream[datastreamId].antennaId;

	for(;;)
	{
		rv = fgets(line, MaxLineLength, in);
		if(!rv)
		{
			scanId = -1;
			configId = -1;
			mjd = -1;
			n = -1;
		}
		else
		{
			int s1, s2;

			n = readSwitchedPower(line, &mjd1, &mjd2, measurement, array_MAX_BANDS*2);
			mjd = 0.5*(mjd1+mjd2);
			s1 = DifxInputGetScanIdByAntennaId(D, mjd1, antId);
			s2 = DifxInputGetScanIdByAntennaId(D, mjd2, antId);
			if(s1 != s2)
			{
				scanId = -1;
			}
			else
			{
				scanId = s1;
			}
		}

		if(scanId != currentScanId)
		{
			if(scanId >= 0)
			{
				double s, e;
				int nWindow;

				doDump = 1;
				scan = D->scan + scanId;
				configId = scan->configId;
				s = mjd1;	/* usually this will be > scan->mjdStart */
				e = scan->mjdEnd;

				nWindow = (int)((e - s)/(avgSeconds/86400.0) + 0.5);

				if(nWindow < 1)
				{
					nWindow = 1;
				}
				windowDuration = (e - s)/nWindow;
				dumpWindow = s + windowDuration;
			}
		}
		else if(mjd > dumpWindow && dumpWindow > 0.0)
		{
			doDump = 1;

			while(mjd > dumpWindow)
			{
				dumpWindow += windowDuration;
			}
		}
		else if(scanId >= 0 && n != nBand)
		{
			fprintf(stderr, "Developer error: getDifxTsys: ds=%d scanId=%d n=%d "
				"nBand=%d mjd1=%12.6f mjd2=%12.6f\n",
				datastreamId, scanId, n, nBand, mjd1, mjd2);
			exit(0);
		}

		if(configId != currentConfigId)
		{
			// Get updated pcal value
		}

		if(doDump)
		{
			scan = D->scan + currentScanId;

			if(nAccum > 0 && currentScanId >= 0 && currentConfigId >= 0 && 
			   phasecentre < scan->nPhaseCentres && 
			   scan->phsCentreSrcs[phasecentre] >= 0)
			{
				// write Tsys row to file
				int freqId, bandId, polId, sourceId;
				int32_t freqId1, antId1, sourceId1, arrayId1;
				float tSys[2][array_MAX_BANDS];
				double time;
				float timeInt;

				time = (accumStart + accumEnd)*0.5 - (int)(D->mjdStart);
				timeInt = accumEnd - accumStart;

				sourceId = scan->phsCentreSrcs[phasecentre];

				/* 1-based values for FITS */
				sourceId1 = D->source[sourceId].fitsSourceIds[currentConfigId] + 1;
				antId1 = antId + 1;
				arrayId1 = 1;
				freqId1 = D->config[currentConfigId].fitsFreqId + 1;

				nanify(tSys);
				
				for(i = 0; i < nBand; i++)
				{
					v = DifxConfigRecBand2FreqPol(D, configId,
						antId, i, &freqId, &polId);
					if(v < 0)
					{
						continue;
					}
					if(freqId < 0 || polId < 0 || freqId >= D->nFreq)
					{
						fprintf(stderr, "Developer error: derived freqId and "
							"polId (%d,%d) are not legit.  From antId=%d, recBand=%d.\n", 
							antId, freqId, polId, i);

						fclose(in);

						return 0;
					}
					bandId = D->config[configId].freqId2IF[freqId];
					if(bandId < 0)
					{
						/* This Freq is not entering this FITS file */
						continue;
					}
					tSys[polId][bandId] = unscaledTsys(average + i);
				}
			
				p_fitsbuf = fitsbuf;
			
				FITS_WRITE_ITEM (time, p_fitsbuf);
				FITS_WRITE_ITEM (timeInt, p_fitsbuf);
				FITS_WRITE_ITEM (sourceId1, p_fitsbuf);
				FITS_WRITE_ITEM (antId1, p_fitsbuf);
				FITS_WRITE_ITEM (arrayId1, p_fitsbuf);
				FITS_WRITE_ITEM (freqId1, p_fitsbuf);

				for(i = 0; i < D->nPol; i++)
				{
					FITS_WRITE_ARRAY(tSys[i], p_fitsbuf, D->nIF);
					FITS_WRITE_ARRAY(tAnt[i], p_fitsbuf, D->nIF);
				}

				testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "TS");

#ifndef WORDS_BIGENDIAN
				FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
				fitsWriteBinRow(out, fitsbuf);
			}

			nAccum = 0;
			doDump = 0;
			clearSwitchedPower(average, array_MAX_BANDS*2);
		}

		if(mjd < 0)
		{
			break;
		}

		nBand = n;
		if(mjd > 0 && scanId > 0 && nBand > 0)
		{
			accumulateSwitchedPower(average, measurement, nBand);
			if(nAccum == 0)
			{
				accumStart = mjd1;
			}
			accumEnd = mjd2;
			nAccum++;
		}

		currentScanId = scanId;
		currentConfigId = configId;
	}

	fclose(in);

	return antId;
}

const DifxInput *DifxInput2FitsTS(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out,
	int phasecentre)
{
	const int MaxLineLength=1000;

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
	char line[MaxLineLength+1];
	char antName[DIFXIO_NAME_LENGTH];
	float tSysRecBand[2*array_MAX_BANDS];
	float tSys[2][array_MAX_BANDS];
	int nBand;
	int configId, sourceId, scanId;
	int i, nPol=0;
	int freqId, bandId, polId, antId;
	int nRecBand;
	int v, d;
	double f;
	double time, mjd;
	float timeInt;
	const DifxScan *scan;
	FILE *in;
	char *rv;
	int *hasDifxTsys;	/* flag per antenna specifying whether or not tsys was supplied from difx */
	/* The following are 1-based indices for writing to FITS */
	int32_t sourceId1, freqId1, arrayId1, antId1;

	if(D == 0)
	{
		return D;
	}

	nanify(tAnt);

	hasDifxTsys = (int *)calloc(D->nAntenna, sizeof(int));

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
	
	in = fopen("tsys", "r");

	for(d = 0; d < D->nDatastream; d++)
	{
		antId = getDifxTsys(D, d, 20, phasecentre, nRowBytes, fitsbuf, nColumn, columns, out);
		if(antId >= 0)
		{
			hasDifxTsys[antId] = 1;
		}
	}

	if(in) for(;;)
	{
		rv = fgets(line, MaxLineLength, in);
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
			nRecBand = parseTsys(line, antName, &time, 
				&timeInt, tSysRecBand);

			/* discard records for unused antennas */
			antId = DifxInputGetAntennaId(D, antName);
			if(antId < 0)
			{
				continue;
			}

			if(hasDifxTsys[antId])
			{
				/* tsys already provided */
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
			freqId1 = D->config[configId].fitsFreqId + 1;
			
			nanify(tSys);

			/* Take the recorder channel order data and populate
			 * into [polId][bandId] order
			 */
			for(i = 0; i < nRecBand; i++)
			{
				v = DifxConfigRecBand2FreqPol(D, configId,
					antId, i, &freqId, &polId);
				if(v < 0)
				{
					continue;
				}
				if(freqId < 0 || polId < 0 || freqId >= D->nFreq)
				{
					fprintf(stderr, "Developer error: derived freqId and "
						"polId (%d,%d) are not legit.  From recBand=%d.\n", 
						freqId, polId, i);

					fclose(in);
					free(hasDifxTsys);
					free(fitsbuf);

					return 0;
				}
				bandId = D->config[configId].freqId2IF[freqId];
				if(bandId < 0)
				{
					/* This Freq is not entering this FITS file */
					continue;
				}
				if(tSysRecBand[i] < 990.0)
				{
					tSys[polId][bandId] = tSysRecBand[i];
				}
			}

			/* 1-based values for FITS */
			antId1 = antId + 1;
			sourceId1 = D->source[sourceId].fitsSourceIds[configId] + 1;
		
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
	if(in)
	{
		fclose(in);
	}
	free(hasDifxTsys);
	free(fitsbuf);

	return D;
}
