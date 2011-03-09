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
#include <strings.h>
#include "config.h"
#include "difx2fits.h"
#include "other.h"


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


static int parseFlag(char *line, int refDay, char *antName, float timeRange[2], 
	char *reason, int *recBand)
{
	int l;
	int n;

	n = sscanf(line, "%s%f%f%d%n", antName, timeRange+0, timeRange+1,
		recBand, &l);

	if(n < 4)
	{
		return 0;
	}

	timeRange[0] -= refDay;
	timeRange[1] -= refDay;
	
	copyQuotedString(reason, line+l, 40);
	
	return 1;
}


static void writeFLrow(struct fitsPrivate *out, char *fitsbuf, int nRowBytes,
	struct fitsBinTableColumn *columns, int nColumn, const FlagDatum *FL)
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

const DifxInput *DifxInput2FitsFL(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	const int MaxLineLength=1000;

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
	char line[MaxLineLength+1];
	char antName[DIFXIO_NAME_LENGTH];
	int refDay;
	int i, v;
	int recBand;
#warning "FIXME: only one configId supported here"
	int configId = 0;
	int antennaId;
	int freqId;
	FILE *in;
	FlagDatum FL;
	const DifxConfig *dc;
	char *rv;
#if 0
	int p, d, c;
	const DifxDatastream *ds;
	char polName;
	int hasData[2][array_MAX_BANDS];
#endif	
	if(D==0)
	{
		return D;
	}

	FL.nBand = p_fits_keys->no_band;	/* same as D->nIF, as set in populateFitsKeywords */
	sprintf(bandFormInt, "%dJ", FL.nBand);
	
	in = fopen("flag", "r");
	
	if(!in)
	{
		return D;
	}

	nColumn = NELEMENTS(columns);
	nRowBytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS format */
	fitsbuf = (char *)calloc(nRowBytes, 1);
	if(fitsbuf == 0)
	{
		fclose(in);
		
		return 0;
	}

	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "FLAG");
	arrayWriteKeys (p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 2, "");
	fitsWriteEnd(out);

	start = D->mjdStart - (int)D->mjdStart;
	stop  = D->mjdStop  - (int)D->mjdStart;

	mjd2dayno((int)(D->mjdStart), &refDay);
	
	/* some constant values */
	FL.sourceId1 = 0;
	FL.freqId1 = 0;
	FL.arrayId1 = 0;
	FL.severity = -1;
	FL.chanRange1[0] = 0;
	FL.chanRange1[1] = 0;
	FL.baselineId1[1] = 0;
	
	/* Write flags from file "flag" */
	for(;;)
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
		else if(parseFlag(line, refDay, antName, FL.timeRange, FL.reason, &recBand))
		{
			if(strncmp(FL.reason, "recorder", 8) == 0)
			{
				/* No need to propagate flags for recorder not recording */

				continue;
			}

			/* Tune up the flag time range */
			if(FL.timeRange[0] > stop || FL.timeRange[1] < start)
			{
				continue;
			}
			if(FL.timeRange[0] < start)
			{
				FL.timeRange[0] = start;
			}
			if(FL.timeRange[1] > stop)
			{
				FL.timeRange[1] = stop;
			}
			if(strcmp(FL.reason, "observing system idle") == 0)
			{
				/* Observation ended at this site.  Flag remainder of it */

				FL.timeRange[1] = 1.0;
			}
			
			/* Set antenna of flag.  ALL flags are associated with exactly 1 antenna. */
			antennaId = DifxInputGetAntennaId(D, antName);
			if(antennaId < 0)
			{
				continue;
			}
			FL.baselineId1[0] = antennaId + 1;

			dc = D->config + configId;

			/* convert the recorder channel number into FITS
			 * useful values: the IF index (bandId) and the
			 * polarization index (polId).  Both are zero-based
			 * numbers, with -1 implying "all values"
			 */
			v = DifxConfigRecBand2FreqPol(D, configId, antennaId, recBand, &freqId, &FL.polId);
			if(v < 0)
			{
				continue;
			}

			/* Then cycle through all IFs to see if that IF is flagged or not */
			for(i = 0; i < D->nIF; i++)
			{
				if(recBand < 0 || isDifxIFInsideDifxFreq(dc->IF + i, D->freq + freqId))
				{
					FL.bandMask[i] = 1;
				}
				else
				{
					FL.bandMask[i] = 0;
				}
			}
			writeFLrow(out, fitsbuf, nRowBytes, columns, nColumn, &FL);
		}
	}

	/* Make flags for bandId/polIds that were not observed */
	FL.timeRange[0] = start;
	FL.timeRange[1] = stop;
	strcpy(FL.reason, "This Band/Pol not observed");

	/* Assumption here is all antennas observed the same frequencies and pols */
	/* If that assumption fails, no real harm is done as the vis records would contain all zeros */

	dc = D->config + configId;
	for(i = 0; i < D->nIF; i++)
	{
		FL.bandMask[i] = 0;
	}
	for(i = 0; i < D->nIF; i++)
	{
		if(dc->IF[i].nPol < dc->nPol)	/* Aha, a pol is missing.  Flag it */
		{
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

#if 0
	FL.freqId1 = 0;
	for(configId = 0; configId < D->nConfig; configId++)
	{
	    dc = D->config + configId;

	    /* want to loop only over unique freqIds */
	    if(dc->fitsFreqId < FL.freqId1)
	    {
	    	continue;       /* this freqId1 done already */
	    }
	    FL.freqId1 = dc->fitsFreqId + 1;
	    for(d = 0; d < dc->nDatastream; d++)
	    {
		if(dc->datastreamId[d] < 0)
		{
			continue;
		}

#warning "FIXME: here we assume 1:1 mapping between antennaId and datastreamId"
		ds = D->datastream + dc->datastreamId[d];
		FL.baselineId1[0] = ds->antennaId + 1;

		/* populate a "presense" matrix. */
		for(p = 0; p < dc->nPol; p++)
		{
			for(i = 0; i < dc->nIF; i++)
			{
				hasData[p][i] = 0;
			}
		}
		for(c = 0; c < ds->nRecBand; c++)
		{
			polName = ds->recBandPolName[c];
			if(ds->recBandFreqId[c] < 0 || ds->recBandFreqId[c] >= ds->nRecFreq)
			{
				fprintf(stderr, "Error: recBandFreqId[%d] is %d, nRecFreq is %d\n",
				        c, ds->recBandFreqId[c], ds->nRecFreq);
				continue;
			}
			freqId = ds->recFreqId[ds->recBandFreqId[c]];
			i = dc->freqId2IF[freqId];
			if(polName == dc->pol[0])
			{
				p = 0;
			}
			else if(polName == dc->pol[1])
			{
				p = 1;
			}
			else
			{
				/* no polarization I heard of! */
				continue;
			}
			hasData[p][i] = 1;
		}
		
		/* if not present, write a flag */
		for(p = 0; p < dc->nPol; p++)
		{
			for(i = 0; i < dc->nIF; i++)
			{
				if(hasData[p][i] == 0)
				{
					FL.bandId = i;
					FL.polId = p;
					writeFLrow(out, fitsbuf, nRowBytes, columns, nColumn, &FL);
				}
			}
		}
	    }
	}
#endif

	/* close the file, free memory, and return */
	fclose(in);
	free(fitsbuf);

	return D;
}
