/***************************************************************************
 *   Copyright (C) 2008-2013 by Walter Brisken                             *
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
#include <strings.h>
#include "config.h"
#include "difx2fits.h"
#include "other.h"
#include "util.h"


typedef struct
{
	int bandMask[array_MAX_BANDS];
	int polId;
	int nBand;
	/* names ending in 1 are 1-based indices for FITS */
	int32_t sourceId1;
	int32_t arrayId1;
	int32_t baselineId1[2];
	int32_t freqId1;
	float timeRange[2];
	int32_t chanRange1[2];
	char reason[64];
	int severity;
} FlagDatum;


static int parseFlag(char *line, char *antName, float timeRange[2], char *reason, int *recBand)
{
	int l;
	int n;

	n = sscanf(line, "%s%f%f%d%n", antName, timeRange+0, timeRange+1, recBand, &l);

	if(n < 4)
	{
		return 0;
	}
	
	copyQuotedString(reason, line+l, 40);
	
	return 1;
}


static void writeFLrow(struct fitsPrivate *out, char *fitsbuf, int nRowBytes, const struct fitsBinTableColumn *columns, int nColumn, const FlagDatum *FL)
{
	char *p_fitsbuf;
	int polMask[4];

	p_fitsbuf = fitsbuf;

	/* Derive polarization Mask */

	if(FL->polId < 0)	/* Flag all pols */
	{
		polMask[0] = 1;
		polMask[1] = 1;
	}
	else if(FL->polId == 0)
	{
		polMask[0] = 1;
		polMask[1] = 0;
	}
	else
	{
		polMask[0] = 0;
		polMask[1] = 1;
	}

	/* Cross pol terms are always affected */
	polMask[2] = polMask[3] = 1;

	FITS_WRITE_ITEM (FL->sourceId1, p_fitsbuf);	  /* SOURCE ID */
	FITS_WRITE_ITEM (FL->arrayId1, p_fitsbuf);	  /* ARRAY ID */
	FITS_WRITE_ITEM (FL->baselineId1, p_fitsbuf);	  /* ANTENNA IDS */
	FITS_WRITE_ITEM (FL->freqId1, p_fitsbuf);	  /* FREQ ID */
	FITS_WRITE_ITEM (FL->timeRange, p_fitsbuf);	  /* TIME RANAGE */
	FITS_WRITE_ARRAY(FL->bandMask, p_fitsbuf, FL->nBand); /* BANDS */
	FITS_WRITE_ITEM (FL->chanRange1, p_fitsbuf);	  /* CHANNELS */
	FITS_WRITE_ITEM (polMask, p_fitsbuf);		  /* POLARIZATIONS */
	FITS_WRITE_ARRAY(FL->reason, p_fitsbuf, 40);	  /* REASON */
	FITS_WRITE_ITEM (FL->severity, p_fitsbuf);	  /* SEVERITY */

	testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "FL");
	
#ifndef WORDS_BIGENDIAN	
	FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
	fitsWriteBinRow(out, fitsbuf);
}

int processFlagFile(const DifxInput *D, const char *antennaName, const char *flagFile, struct fitsPrivate *out, char *fitsbuf, int nRowBytes, int nColumn, const struct fitsBinTableColumn *columns, FlagDatum *FL, int refDay, int year)
{
	FILE *in;
	int nRec = 0;
	int i;
	double start, stop;
#warning "FIXME: only one configId supported here"
	int configId = 0;

	in = fopen("flag", "r");
	if(!in)
	{
		return 0;
	}

	start = D->mjdStart - (int)D->mjdStart;
	stop  = D->mjdStop  - (int)D->mjdStart;

	/* Write flags from file "flag" */
	for(;;)
	{
		const int MaxLineLength=1000;
		const DifxConfig *dc;
		char *rv;
		char antName[DIFXIO_NAME_LENGTH];
		char line[MaxLineLength+1];
		int recBand;
		int v;
		
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
		else if(parseFlag(line, antName, FL->timeRange, FL->reason, &recBand))
		{
			int antennaId;
			int freqId;

			if(antennaName && strcasecmp(antennaName, antName) != 0)
			{
				/* not a matching antenna */

				continue;
			}
			antennaId = DifxInputGetAntennaId(D, antName);
			if(antennaId < 0)
			{
				continue;
			}

			if(FL->timeRange[0] > 50000.0)	/* must be MJD */
			{
				FL->timeRange[0] -= (int)(D->mjdStart);
				FL->timeRange[1] -= (int)(D->mjdStart);
			}
			else	/* must be day of year */
			{
				FL->timeRange[0] -= refDay;
				FL->timeRange[1] -= refDay;
				if(FL->timeRange[0] < -300.0)	/* must be new years crossing */
				{
					FL->timeRange[0] += DaysThisYear(year);
				}
				else if(FL->timeRange[0] > 300.0)/* must be partial project after new year */
				{
					FL->timeRange[0] -= DaysLastYear(year);
				}
				if(FL->timeRange[1] < -300.0)	/* must be new years crossing */
				{
					FL->timeRange[1] += DaysThisYear(year);
				}
				else if(FL->timeRange[1] > 300.0)/* must be partial project after new year */
				{
					FL->timeRange[1] -= DaysLastYear(year);
				}
			}
			
			if(strncmp(FL->reason, "recorder", 8) == 0)
			{
				/* No need to propagate flags for recorder not recording */

				continue;
			}

			/* Tune up the flag time range */
			if(FL->timeRange[0] > stop || FL->timeRange[1] < start)
			{
				continue;
			}
			if(FL->timeRange[0] < start)
			{
				FL->timeRange[0] = start;
			}
			if(FL->timeRange[1] > stop)
			{
				FL->timeRange[1] = stop;
			}
			if(strcmp(FL->reason, "observing system idle") == 0)
			{
				/* Observation ended at this site.  Flag remainder of it */

				FL->timeRange[1] = 1.0;
			}
			
			/* Set antenna of flag.  ALL flags are associated with exactly 1 antenna. */
			FL->baselineId1[0] = antennaId + 1;

			dc = D->config + configId;

			/* convert the recorder channel number into FITS
			 * useful values: the IF index (bandId) and the
			 * polarization index (polId).  Both are zero-based
			 * numbers, with -1 implying "all values"
			 */
			v = DifxConfigRecBand2FreqPol(D, configId, antennaId, recBand, &freqId, &FL->polId);
			if(v < 0)
			{
				continue;
			}

			/* Then cycle through all IFs to see if that IF is flagged or not */
			for(i = 0; i < D->nIF; ++i)
			{
				if(recBand < 0 || isDifxIFInsideDifxFreq(dc->IF + i, D->freq + freqId))
				{
					FL->bandMask[i] = 1;
				}
				else
				{
					FL->bandMask[i] = 0;
				}
			}
			writeFLrow(out, fitsbuf, nRowBytes, columns, nColumn, FL);
		}
	}
	fclose(in);

	return nRec;
}

const DifxInput *DifxInput2FitsFL(const DifxInput *D, struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	char bandFormInt[8];

	/*  define the flag FITS table columns */
	struct fitsBinTableColumn columns[] =
	{
		{"SOURCE_ID", "1J", "source id number from source tbl", 0},
		{"ARRAY", "1J", "????", 0},
		{"ANTS", "2J", "antenna id from antennas tbl", 0},
		{"FREQID", "1J", "freq id from frequency tbl", 0},
		{"TIMERANG", "2E", "time flag condition begins, ends", "DAYS"},
		{"BANDS", bandFormInt, "true if the baseband is bad", 0},
		{"CHANS", "2J", "channel range to be flagged", 0},
		{"PFLAGS", "4J", "flag array for polarization", 0},
		{"REASON", "40A", "reason for data to be flagged bad", 0},
		{"SEVERITY", "1J", "severity code", 0}
	};

	int nColumn;
	int nRowBytes;
	char *fitsbuf;
	double start, stop;
	int refDay;
	int antId, i;
	int year, month, day;
	FlagDatum FL;

#warning "FIXME: only one configId supported here"
	int configId = 0;

	if(D == 0)
	{
		return D;
	}

	FL.nBand = p_fits_keys->no_band;	/* same as D->nIF, as set in populateFitsKeywords */
	sprintf(bandFormInt, "%dJ", FL.nBand);
	
	nColumn = NELEMENTS(columns);
	nRowBytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS format */
	fitsbuf = (char *)calloc(nRowBytes, 1);
	if(fitsbuf == 0)
	{
		fprintf(stderr, "Error: DifxInput2FitsFL: Cannot allocate %d bytes for fitsbuf\n", nRowBytes);

		return 0;
	}

	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "FLAG");
	arrayWriteKeys (p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 2, "");
	fitsWriteEnd(out);

	start = D->mjdStart - (int)D->mjdStart;
	stop  = D->mjdStop  - (int)D->mjdStart;

	mjd2dayno((int)(D->mjdStart), &refDay);
	mjd2date((int)(D->mjdStart), &year, &month, &day);
	
	/* some constant values */
	FL.sourceId1 = 0;
	FL.freqId1 = 0;
	FL.arrayId1 = 0;
	FL.severity = -1;
	FL.chanRange1[0] = 0;
	FL.chanRange1[1] = 0;
	FL.baselineId1[1] = 0;

	/* First: look for individual station flag files */
	for(antId = 0; antId < D->nAntenna; ++antId)
	{
		char flagFile[DIFXIO_FILENAME_LENGTH];
		int v;
	
		v = snprintf(flagFile, DIFXIO_FILENAME_LENGTH, "%s%s.%s.flag", D->job->obsCode, D->job->obsSession, D->antenna[antId].name);
		if(v >= DIFXIO_FILENAME_LENGTH)
		{
			fprintf(stderr, "Developer error: DifxInput2FitsFL: DIFXIO_FILENAME_LENGTH=%d is too small.  Wants to be %d.\n", DIFXIO_FILENAME_LENGTH, v+1);

			exit(0);
		}

		v = globcase(__FUNCTION__, "*.*.flag", flagFile);
		if(v == 0)
		{
			/* no files match */

			continue;
		}

		v = processFlagFile(D, D->antenna[antId].name, flagFile, out, fitsbuf, nRowBytes, nColumn, columns, &FL, refDay, year);
	}

	/* Second: look for a multi-station flag file.  Unlike for tsys, pcal, and weather, flags will be applied from both
	 * the antenna-specific flag file and "flag".
	 */
	processFlagFile(D, 0, "flag", out, fitsbuf, nRowBytes, nColumn, columns, &FL, refDay, year);

	/* Finally: make flags for bandId/polIds that were not observed */
	FL.timeRange[0] = start;
	FL.timeRange[1] = stop;
	strcpy(FL.reason, "This Band/Pol not observed");

	/* Assumption here is all antennas observed the same frequencies and pols */
	/* If that assumption fails, no real harm is done as the vis records would contain all zeros */

	for(i = 0; i < D->nIF; ++i)
	{
		FL.bandMask[i] = 0;
	}
	for(i = 0; i < D->nIF; ++i)
	{
		const DifxConfig *dc;

		dc = D->config + configId;
			
		if(dc->IF[i].nPol < dc->nPol)	/* Aha, a pol is missing.  Flag it */
		{
			int antennaId;

			FL.bandMask[i] = 1;
			
			if(dc->IF[i].nPol > 0 && dc->IF[i].pol[0] == dc->pol[0])
			{
				FL.polId = 1;
			}
			else if(dc->IF[i].nPol > 0 && dc->IF[i].pol[0] == dc->pol[1])
			{
				FL.polId = 0;
			}
			else
			{
				/* Should not happen! */
				fprintf(stderr, "\nDeveloper error: flagging all pols of IF=%d\n", i);
				FL.polId = -1;
			}

			for(antennaId = 0; antennaId < D->nAntenna; antennaId++)
			{
				FL.baselineId1[0] = antennaId + 1;
				writeFLrow(out, fitsbuf, nRowBytes, columns, nColumn, &FL);
			}
			
			FL.bandMask[i] = 0;
		}
	}

	/* free memory, and return */
	free(fitsbuf);

	return D;
}
