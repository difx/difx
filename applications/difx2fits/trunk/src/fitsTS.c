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
#include <string.h>
#include "config.h"
#include "difx2fits.h"
#include "other.h"


typedef struct
{
	char antenna[8];
	char receiver[8];
	float freq, tcalR, tcalL;
} TCal;

typedef struct
{
	double pOn;	/* power in the on state */
	double wOn;	/* weight for the on state = 1/sigma^2 */
	double pOff;	/* power in the off state */
	double wOff;	/* weight for the on state = 1/sigma^2 */
} SwitchedPower;


/* This is somewhat ugly.  Perhaps move these internal to some function? */
TCal *tCals = 0;
int nTcal;


int loadTcals()
{
	const int MaxLineLength = 100;
	char line[MaxLineLength];
	const char *tcalFilename;
	char *rv;
	FILE *in;
	int i;
	int nAlloc = 0;

	tcalFilename = getenv("TCAL_FILE");
	if(tcalFilename == 0)
	{
		return -1;
	}
	in = fopen(tcalFilename, "r");
	if(!in)
	{
		return -2;
	}

	nAlloc = 4000;
	tCals = (TCal *)malloc(nAlloc*sizeof(TCal));

	for(i = nTcal = 0; ; i++)
	{
		rv = fgets(line, MaxLineLength, in);
		if(!rv)
		{
			break;
		}
		if(line[0] == '#')
		{
			continue;
		}
		if(nTcal >= nAlloc)
		{
			nAlloc += 1000;
			tCals = (TCal *)realloc(tCals, nAlloc*sizeof(TCal));
		}
		sscanf(line, "%7s%7s%f%f%f", tCals[nTcal].antenna, tCals[nTcal].receiver,
			&tCals[nTcal].freq, &tCals[nTcal].tcalR, &tCals[nTcal].tcalL);
		nTcal++;
	}

	fclose(in);

	return 0;
}

float getTcalValue(const char *antname, float freq, char pol)
{
	int i;
	int besti;
	int secondbesti;
	float bestf, w1, w2, df;

	if(nTcal == 0)
	{
		return 1.0;
	}

	besti = -1;
	bestf = 1e9;
	df = 1e9;
	for(i = 0; i < nTcal; i++)
	{
		df = fabs(freq - tCals[i].freq);
		if(strcasecmp(antname, tCals[i].antenna) == 0 && df < bestf)
		{
			bestf = df;
			besti = i;
		}
	}
	
	if(besti < 0)
	{
		return 1.0;
	}

	bestf = tCals[besti].freq;

	secondbesti = -1;
	if(besti > 0 && bestf > freq && 
	   strcmp(tCals[besti].antenna, tCals[besti-1].antenna) == 0 &&
	   strcmp(tCals[besti].receiver, tCals[besti-1].receiver) == 0)
	{
		secondbesti = besti - 1;
	}
	else if(besti < nTcal-1 && bestf < freq && 
	        strcmp(tCals[besti].antenna, tCals[besti+1].antenna) == 0 &&
	        strcmp(tCals[besti].receiver, tCals[besti+1].receiver) == 0)
	{
		secondbesti = besti + 1;
	}

	if(secondbesti >= 0)
	{
		w1 = fabs( (tCals[secondbesti].freq - freq)/(tCals[secondbesti].freq - bestf) );
		w2 = fabs( (tCals[besti].freq - freq)/(tCals[secondbesti].freq - bestf) );
	}
	else
	{
		secondbesti = besti;
		w1 = 1.0;
		w2 = 0.0;
	}

	if(pol == 'r' || pol == 'R')
	{
		return w1*tCals[besti].tcalR + w2*tCals[secondbesti].tcalR;
	}
	else if(pol == 'l' || pol == 'L')
	{
		return w1*tCals[besti].tcalL + w2*tCals[secondbesti].tcalL;
	}
	else
	{
		return 1.0;
	}
}

static void nanify(float X[2][array_MAX_BANDS])
{
	int i;

	/* Note: This is a particular NaN variant the FITS-IDI format/convention 
	 * wants, namely 0xFFFFFFFF */
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

	if(a > b)
	{
		return 0.5*(a+b)/(a-b);
	}
	else
	{
		return -1.0;
	}
}

static int populateDifxTSys(float tSys[][array_MAX_BANDS], const DifxInput *D, int configId, int antId,
	const SwitchedPower *average, int nRecBand)
{
	int i, p, r, v;
	int freqId, polId;
	const DifxConfig *dc;
	double freq, tCal;

	dc = D->config + configId;

	for(i = 0; i < D->nIF; i++)
	{
		for(p = 0; p < dc->IF[i].nPol; p++)
		{
			/* search for a compatible record band.  This should allow zoom bands to work. */
			for(r = 0; r < nRecBand; r++)
			{
				v = DifxConfigRecBand2FreqPol(D, configId, antId, r, &freqId, &polId);

				if(v < 0)
				{
					continue;
				}
			
				if(polId < 0 || freqId < 0 || freqId >= D->nFreq)
				{
					fprintf(stderr, "Developer error: derived freqId and polId (%d,%d) are not legit.  From recBand=%d.\n", 
						freqId, polId, r);

					exit(0);
				}

				if(polId == p && isDifxIFInsideDifxFreq(dc->IF + i, D->freq + freqId))
				{
					break;
				}
			}
			if(r < nRecBand) /* match found */
			{
				freq = dc->IF[i].freq;
				if(dc->IF[i].sideband == 'L')
				{
					freq -= dc->IF[i].bw*0.5;
				}
				else
				{
					freq += dc->IF[i].bw*0.5;
				}
				/* Note: could do better by considering full band Tsys variations */
				tCal = getTcalValue(D->antenna[antId].name, freq, D->config[configId].pol[polId]);
				if(tCal > 0.0)
				{
					tSys[p][i] = tCal*unscaledTsys(average + r);
				}
			}
		}
	}

	return 0;
}

static int getDifxTsys(const DifxInput *D, int jobId, int antId, int origDsId, 
	double avgSeconds, int phasecentre, int nRowBytes, char *fitsbuf, 
	int nColumn, const struct fitsBinTableColumn *columns,
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
	double mjd1, mjd2, mjd;
	double accumStart = -1, accumEnd = -1;
	int configId = -1;
	int scanId = -1;
	int i, v, n;
	int nRecBand = -1;
	char *rv;
	int currentScanId = -1;
	int currentConfigId = -1;
	int nAccum = 0;
	int doDump = 0;
	double windowDuration = 0;	/* in days */
	double dumpWindow = 0;		/* mjd of end of window */
	DifxScan *scan;
	float tSys[2][array_MAX_BANDS];
	float tAnt[2][array_MAX_BANDS];
	int sourceId;
	int32_t freqId1, antId1, sourceId1, arrayId1;
	double time;
	float timeInt;

	if(tCals == 0)
	{
		loadTcals();
	}

	v = snprintf(filename, MaxFilenameLength, "%s/SWITCHEDPOWER_%d", D->job[jobId].outputFile, origDsId);

	if(v >= MaxFilenameLength)
	{
		fprintf(stderr, "Developer error: getDifxTsys jobId=%d antId=%d origDsId=%d filename length wanted to be %d bytes long, not %d\n",
			jobId, antId, origDsId, v, MaxFilenameLength);
		
		return -1;
	}
	in = fopen(filename, "r");
	if(!in)
	{
		return -1;
	}

	nanify(tAnt);

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
			if(currentScanId >= 0)
			{
				doDump = 1;
			}
			if(scanId >= 0)
			{
				double s, e;
				int nWindow;

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
		else if(scanId >= 0 && n != nRecBand)
		{
			fprintf(stderr, "Developer error: getDifxTsys: antId=%d origDsId=%d scanId=%d n=%d nRecBand=%d mjd1=%12.6f mjd2=%12.6f\n",
				antId, origDsId, scanId, n, nRecBand, mjd1, mjd2);

			exit(0);
		}

		if(doDump)
		{
			scan = D->scan + currentScanId;

			if(nAccum > 0 && currentScanId >= 0 && currentConfigId >= 0 && 
			   phasecentre < scan->nPhaseCentres && 
			   scan->phsCentreSrcs[phasecentre] >= 0)
			{
				// write Tsys row to file
				time = (accumStart + accumEnd)*0.5 - (int)(D->mjdStart);
				timeInt = accumEnd - accumStart;

				sourceId = scan->phsCentreSrcs[phasecentre];

				/* 1-based values for FITS */
				sourceId1 = D->source[sourceId].fitsSourceIds[currentConfigId] + 1;
				antId1 = antId + 1;
				arrayId1 = 1;
				freqId1 = D->config[currentConfigId].fitsFreqId + 1;

				nanify(tSys);
				populateDifxTSys(tSys, D, currentConfigId, antId, average, nRecBand);

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

		nRecBand = n;
		if(mjd > 0 && scanId >= 0 && nRecBand > 0 && !isAntennaFlagged(D->job + jobId, mjd, antId))
		{
			accumulateSwitchedPower(average, measurement, nRecBand);
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

	return 1;
}

static int populateTSys(float tSys[][array_MAX_BANDS], 
	const DifxInput *D, int configId, int antId, const float tSysRecBand[], int nRecBand)
{
	int i, p, r, v;
	int freqId, polId;
	const DifxConfig *dc;

	dc = D->config + configId;

	for(i = 0; i < D->nIF; i++)
	{
		for(p = 0; p < dc->IF[i].nPol; p++)
		{
			/* search for a compatible record band.  This should allow zoom bands to work. */
			for(r = 0; r < nRecBand; r++)
			{
				v = DifxConfigRecBand2FreqPol(D, configId, antId, r, &freqId, &polId);
			
				if(v < 0)
				{
					continue;
				}
			
				if(polId < 0 || freqId < 0 || freqId >= D->nFreq)
				{
					fprintf(stderr, "Developer error: derived freqId and polId (%d,%d) are not legit.  From recBand=%d.\n", 
						freqId, polId, r);

					exit(0);
				}

				if(polId == p && isDifxIFInsideDifxFreq(dc->IF + i, D->freq + freqId))
				{
					break;
				}
			}
			if(r < nRecBand) /* match found */
			{
				tSys[p][i] = tSysRecBand[r];
			}
		}
	}

	return 0;
}

const DifxInput *DifxInput2FitsTS(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out,
	int phasecentre, double DifxTsysAvgSeconds)
{
	const int MaxLineLength=1000;
	const int MaxDatastreamsPerAntenna=8;

	char bandFormFloat[8];
	int origDsIds[MaxDatastreamsPerAntenna];
	
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
	float tAnt[2][array_MAX_BANDS];
	int nBand;
	int configId, sourceId, scanId;
	int i, nPol=0;
	int antId, jobId;
	int nRecBand;
	int v, n;
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

	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "SYSTEM_TEMPERATURE");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "NO_POL", nPol, "");
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteEnd(out);

	freqId1 = 1;
	arrayId1 = 1;
	
	in = fopen("tsys", "r");

	if(DifxTsysAvgSeconds > 0.0)
	{
		for(antId = 0; antId < D->nAntenna; antId++)
		{
			for(jobId = 0; jobId < D->nJob; jobId++)
			{
				n = DifxInputGetOriginalDatastreamIdsByAntennaIdJobId(origDsIds, D, antId, jobId, MaxDatastreamsPerAntenna);
				if(n > 1)
				{
					fprintf(stderr, "\nWarning: > 1 datastream for antennaId=%d.  This has not been tested.\n", antId);
				}
				for(i = 0; i < n; i++)
				{
					v = getDifxTsys(D, jobId, antId, origDsIds[i], DifxTsysAvgSeconds, phasecentre, nRowBytes, fitsbuf, nColumn, columns, out);
					if(v >= 0)
					{
						hasDifxTsys[antId]++;
					}
				}
			}
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
			if(sourceId < 0)
			{
				continue;
			}

			configId = scan->configId;
			freqId1 = D->config[configId].fitsFreqId + 1;
			
			nanify(tSys);
			populateTSys(tSys, D, configId, antId, tSysRecBand, nRecBand);

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
		in = 0;
	}
	free(hasDifxTsys);
	free(fitsbuf);
	if(tCals)
	{
		free(tCals);
		tCals = 0;
	}

	return D;
}
