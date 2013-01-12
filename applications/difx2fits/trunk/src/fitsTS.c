/***************************************************************************
 *   Copyright (C) 2008-2013 by Walter Brisken & Adam Deller               *
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
#include <glob.h>
#include <difxio/difx_tcal.h>
#include "config.h"
#include "difx2fits.h"
#include "other.h"
#include "util.h"


typedef struct
{
	double pOn;	/* power in the on state */
	double wOn;	/* weight for the on state = 1/sigma^2 */
	double pOff;	/* power in the off state */
	double wOff;	/* weight for the on state = 1/sigma^2 */
} SwitchedPower;


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
	
	for(i = 0; i < array_MAX_BANDS; ++i)
	{
		X[0][i] = nan.f;
		X[1][i] = nan.f;
	}
}

/* ant D.O.Y. dur(days) nRecBand (tsys, bandName)[nRecBand] */
static int parseTsys(const char *line, char *antName, double *time, float *timeInt, float tSys[])
{
	static char tsmAntName[10] = "XX";
	int p, n, i, nRecBand;
	char firstWord[100];

	/* skip comments of various types */
	if(line[0] == '#' || line[0] == '!' || line[0] == '*' || line[0] == '/')
	{
		return -4;
	}
	
	n = sscanf(line, "%99s", firstWord);
	if(n != 1)
	{
		return -3;
	}
	p = atoi(firstWord);

	/* Do different things depending on what the first word in the line is */
	if(strcmp(firstWord, "TSYS") == 0)
	{
		for(i = 0; firstWord[i]; ++i)
		{
			if(firstWord[i] == '=')
			{
				break;
			}
		}
		if(!firstWord[i])	/* no = found */
		{
			return -5;
		}
		n = sscanf(line+i, "%*s%s", tsmAntName);
		
		return -6;
	}
	else if(p > 0 && p <= 366 && strlen(firstWord) <= 3)	/* Assume it is a TSM Tsys record */
	{
		char timestr[20];
		int factor = 24;
		int j;
		char *s;

		/* get time */
		n = sscanf(line, "%*d%19s%n", timestr, &p);
		if(n != 1)
		{
			return -7;
		}
		*time = p;
		s = timestr;
		for(i = 0; i < 3; ++i)
		{
			int last = 0;

			for(j = 0; s[j] != 0 && s[j] != ':'; ++j) {}

			if(s[j] == 0 || s[j+1] == 0)
			{
				last = 1;
			}

			s[j] = 0;
			*time += atof(s)/factor;
			factor *= 60;

			if(last)
			{
				break;
			}

			s += j+1;
		}
		for(i = 0; i < nRecBand; ++i)
		{
			line += p;
			n = sscanf(line, "%f%n", tSys + i, &p);
			if(n != 1)
			{
				return -2;
			}
		}
	}
	else if(strlen(firstWord) < 10 && firstWord[0] >= 'A' && firstWord[0] <= 'Z')	/* Assume it is a difx-style record */
	{
		n = sscanf(line, "%31s%lf%f%d%n", antName, time, timeInt, &nRecBand, &p);
		if(n != 4)
		{
			return -1;
		}

		for(i = 0; i < nRecBand; ++i)
		{
			line += p;
			n = sscanf(line, "%f%*s%n", tSys + i, &p);
			if(n != 1)
			{
				return -2;
			}
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

	for(i = 0; i < max; ++i)
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

	for(i = 0; i < n; ++i)
	{
		sp[i].pOn = sp[i].pOff = sp[i].wOn = sp[i].wOff = 0.0;
	}
}

static void accumulateSwitchedPower(SwitchedPower *average, const SwitchedPower *meas, int n)
{
	double w;
	int i;

	for(i = 0; i < n; ++i)
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

static int populateDifxTSys(float tSys[][array_MAX_BANDS], const DifxInput *D, int configId, int antId, const SwitchedPower *average, int nRecBand, DifxTcal *T)
{
	int i, p, r, v;
	int freqId, polId;
	const DifxConfig *dc;
	double freq, tCal;

	dc = D->config + configId;

	for(i = 0; i < D->nIF; ++i)
	{
		for(p = 0; p < dc->IF[i].nPol; ++p)
		{
			/* search for a compatible record band.  This should allow zoom bands to work. */
			for(r = 0; r < nRecBand; ++r)
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

					exit(EXIT_FAILURE);
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
				tCal = getDifxTcal(T, D->mjdStart, D->antenna[antId].name, "", D->config[configId].pol[polId], freq);
				if(tCal > 0.0)
				{
					tSys[p][i] = tCal*unscaledTsys(average + r);
					if(tSys[p][i] < 0.0)
					{
						tSys[p][i] = 999.0;
					}
				}
			}
		}
	}

	return 0;
}

static int getDifxTsys(const DifxInput *D, int jobId, int antId, int origDsId, double avgSeconds, int phaseCentre, int nRowBytes, char *fitsbuf, int nColumn, const struct fitsBinTableColumn *columns, struct fitsPrivate *out, DifxTcal *T)
{
	const int MaxLineLength=1000;
	char line[MaxLineLength];
	const char *fileName;
	char globPattern[DIFXIO_FILENAME_LENGTH];
	char *p_fitsbuf;
	FILE *in = 0;
	SwitchedPower measurement[array_MAX_BANDS*2];
	SwitchedPower average[array_MAX_BANDS*2];
	double mjd1, mjd2, mjd;
	double mjdLast = 0.0;		/* to verify time ordering of records */
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
	glob_t globBuffer;
	int nFile, curFile;

	clearSwitchedPower(average, array_MAX_BANDS*2);

	v = snprintf(globPattern, DIFXIO_FILENAME_LENGTH, "%s/SWITCHEDPOWER*_%d", D->job[jobId].outputFile, origDsId);

	if(v >= DIFXIO_FILENAME_LENGTH)
	{
		fprintf(stderr, "Developer error: getDifxTsys jobId=%d antId=%d origDsId=%d filename length wanted to be %d bytes long, not %d\n",
			jobId, antId, origDsId, v, DIFXIO_FILENAME_LENGTH);
		
		return -1;
	}

	v = glob2(__FUNCTION__, globPattern, 0, 0, &globBuffer);
	
	nFile = globBuffer.gl_pathc;
	if(nFile < 1)
	{
		return -1;
	}
	curFile = -1;

	nanify(tAnt);

	for(;;)
	{
		/* First: if no open file, try the next one */
		if(!in)
		{
			++curFile;
			if(curFile < nFile)
			{
				fileName = globBuffer.gl_pathv[curFile];
				
				in = fopen(fileName, "r");
				if(!in)
				{
					fprintf(stderr, "Warning: switched power file %s\n", fileName);
					fprintf(stderr, "  cannot be opened for read and is being skipped!\n");

					continue;
				}
			}
			else
			{
				fileName = 0;
			}
		}

		/* Second: if there is an open file, try reading a line */
		if(in)
		{
			rv = fgets(line, MaxLineLength, in);
		}
		else
		{
			rv = 0;
		}

		/* Third: if no data was read, either go to top and try next file or prepare for the end */
		if(!rv)
		{
			if(in)
			{
				fclose(in);
				in = 0;
			}

			if(curFile < nFile)
			{
				continue;
			}
			else	/* set things up to end the loop */
			{
				scanId = -1;
				configId = -1;
				mjd = -1;	/* causes exit at the end */
				n = -1;
			}
		}
		else	/* Here data was read: deal with parsing it */
		{
			int s1, s2;

			n = readSwitchedPower(line, &mjd1, &mjd2, measurement, array_MAX_BANDS*2);
			mjd = 0.5*(mjd1+mjd2);

			if(mjd < mjdLast)
			{
				/* probably only occurs after a new file is opened that may overlap in time */

				continue;
			}
			else
			{
				mjdLast = mjd;
			}

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

		if(scanId != currentScanId)	/* When a scan ends, trigger dump of data */
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
		else if(mjd > dumpWindow && dumpWindow > 0.0)	/* when a dump interval ends, trigger dump of data */
		{
			doDump = 1;

			while(mjd > dumpWindow)
			{
				dumpWindow += windowDuration;
			}
		}
		else if(scanId >= 0 && n != nRecBand)	/* bail if the wrong number of measurements were found */
		{
			fprintf(stderr, "Developer error: getDifxTsys: antId=%d origDsId=%d scanId=%d n=%d nRecBand=%d mjd1=%12.6f mjd2=%12.6f\n",
				antId, origDsId, scanId, n, nRecBand, mjd1, mjd2);

			exit(EXIT_FAILURE);
		}

		if(doDump)	/* here is where data goes to the file */
		{
			scan = D->scan + currentScanId;

			if(nAccum > 0 && currentScanId >= 0 && currentConfigId >= 0 && phaseCentre < scan->nPhaseCentres && scan->phsCentreSrcs[phaseCentre] >= 0)
			{
				// write Tsys row to file
				time = (accumStart + accumEnd)*0.5 - (int)(D->mjdStart);
				timeInt = accumEnd - accumStart;

				sourceId = scan->phsCentreSrcs[phaseCentre];

				/* 1-based values for FITS */
				sourceId1 = D->source[sourceId].fitsSourceIds[currentConfigId] + 1;
				antId1 = antId + 1;
				arrayId1 = 1;
				freqId1 = D->config[currentConfigId].fitsFreqId + 1;

				nanify(tSys);
				populateDifxTSys(tSys, D, currentConfigId, antId, average, nRecBand, T);

				p_fitsbuf = fitsbuf;
			
				FITS_WRITE_ITEM (time, p_fitsbuf);
				FITS_WRITE_ITEM (timeInt, p_fitsbuf);
				FITS_WRITE_ITEM (sourceId1, p_fitsbuf);
				FITS_WRITE_ITEM (antId1, p_fitsbuf);
				FITS_WRITE_ITEM (arrayId1, p_fitsbuf);
				FITS_WRITE_ITEM (freqId1, p_fitsbuf);

				for(i = 0; i < D->nPol; ++i)
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
			++nAccum;
		}

		currentScanId = scanId;
		currentConfigId = configId;
	}

	globfree(&globBuffer);

	return 1;
}

static int populateTSys(float tSys[][array_MAX_BANDS], const DifxInput *D, int configId, int antId, const float tSysRecBand[], int nRecBand)
{
	int i, p, r, v;
	int freqId, polId;
	const DifxConfig *dc;

	dc = D->config + configId;

	for(i = 0; i < D->nIF; ++i)
	{
		for(p = 0; p < dc->IF[i].nPol; ++p)
		{
			/* search for a compatible record band.  This should allow zoom bands to work. */
			for(r = 0; r < nRecBand; ++r)
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

					exit(EXIT_FAILURE);
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

const DifxInput *DifxInput2FitsTS(const DifxInput *D, struct fits_keywords *p_fits_keys, struct fitsPrivate *out, int phaseCentre, double DifxTsysAvgSeconds)
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
	double time, mjd;
	float timeInt;
	const DifxScan *scan;
	FILE *in;
	char *rv;
	int *hasDifxTsys;	/* flag per antenna specifying whether or not tsys was supplied from difx */
	/* The following are 1-based indices for writing to FITS */
	int32_t sourceId1, freqId1, arrayId1, antId1;
	DifxTcal *T;
	const char *tcalFilename;
	int year, month, day;

	if(D == 0)
	{
		return D;
	}

	T = newDifxTcal();

	/* first test for VLBA case */
	if( (tcalFilename = getenv("TCAL_PATH")) != 0)
	{
		v = setDifxTcalVLBA(T, tcalFilename);
		if(v < 0)
		{
			fprintf(stderr, "Error initializing VLBA Tcal values\n");

			exit(EXIT_FAILURE);
		}
	}
	else if( (tcalFilename = getenv("TCAL_FILE")) != 0)
	{
		v = setDifxTcalDIFX(T, tcalFilename);
		if(v < 0)
		{
			fprintf(stderr, "Error initializing VLBA Tcal values\n");

			exit(EXIT_FAILURE);
		}
	}

	nanify(tAnt);

	hasDifxTsys = (int *)calloc(D->nAntenna, sizeof(int));
	if(hasDifxTsys == 0)
	{
		fprintf(stderr, "Error: DifxInput2FitsTS: Cannot allocate %d integers for hasDifxTsys\n", D->nAntenna);

		exit(EXIT_FAILURE);
	}

	nBand = D->nIF;
	nPol = D->nPol;

	sprintf(bandFormFloat, "%dE", nBand);

	mjd2dayno((int)(D->mjdStart), &refDay);
	mjd2date((int)(D->mjdStart), &year, &month, &day);

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
		fprintf(stderr, "Error: DifxInput2FitsTS: Cannot allocate %d bytes for fitsbuf\n", nRowBytes);

		free(hasDifxTsys);

		exit(EXIT_FAILURE);
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
		for(antId = 0; antId < D->nAntenna; ++antId)
		{
			for(jobId = 0; jobId < D->nJob; ++jobId)
			{
				n = DifxInputGetOriginalDatastreamIdsByAntennaIdJobId(origDsIds, D, antId, jobId, MaxDatastreamsPerAntenna);
				if(n > 1)
				{
					fprintf(stderr, "\nWarning: > 1 datastream for antennaId=%d.  This has not been tested.\n", antId);
				}
				for(i = 0; i < n; ++i)
				{
					v = getDifxTsys(D, jobId, antId, origDsIds[i], DifxTsysAvgSeconds, phaseCentre, nRowBytes, fitsbuf, nColumn, columns, out, T);
					if(v >= 0)
					{
						++hasDifxTsys[antId];
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
			nRecBand = parseTsys(line, antName, &time, &timeInt, tSysRecBand);
			if(nRecBand == -1)
			{
				continue;
			}

			if(time > 50000.0)	/* must be an MJD */
			{
				mjd = time;
				time = mjd - (int)(D->mjdStart);
			}
			else			/* must be day of year */
			{
				time -= refDay;
				if(time < -300)	/* must be new years crossing */
				{
					time += DaysThisYear(year);
				}
				else if(time > 300) /* must be partial project after new year */
				{
					time -= DaysLastYear(year);
				}
				mjd = time + (int)(D->mjdStart);
			}

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
			scanId = DifxInputGetScanIdByAntennaId(D, mjd, antId);
			if(scanId < 0)
			{
				continue;
			}
			scan = D->scan + scanId;

			if(phaseCentre >= scan->nPhaseCentres)
			{
				printf("Skipping scan %d as the requested phase centre was not used\n", scanId);
				continue;
			}

			sourceId = scan->phsCentreSrcs[phaseCentre];
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

			for(i = 0; i < nPol; ++i)
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

	deleteDifxTcal(T);

	return D;
}
