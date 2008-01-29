#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"
#include "other.h"


typedef struct
{
	int bandId;
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
	char *reason, int *recChan)
{
	int l;
	int n;

	n = sscanf(line, "%s%f%f%d%n", antName, timeRange+0, timeRange+1,
		recChan, &l);

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
	int polMask[4], bandMask[array_MAX_BANDS];
	int i;

	p_fitsbuf = fitsbuf;

	/* Derive band flag Mask */
	if(FL->bandId < 0)
	{
		for(i = 0; i < FL->nBand; i++)
		{
			bandMask[i] = 1;
		}
	}
	else
	{
		for(i = 0; i < FL->nBand; i++)
		{
			bandMask[i] = 0;
		}
		bandMask[FL->bandId] = 1;
	}
	
	/* Derive polarization Mask */
	polMask[2] = polMask[3] = 1;
	if(FL->polId < 0)
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

	FITS_WRITE_ITEM (FL->sourceId1, p_fitsbuf);	  /* SOURCE ID */
	FITS_WRITE_ITEM (FL->arrayId1, p_fitsbuf);	  /* ARRAY ID */
	FITS_WRITE_ITEM (FL->baselineId1, p_fitsbuf);	  /* ANTENNA IDS */
	FITS_WRITE_ITEM (FL->freqId1, p_fitsbuf);	  /* FREQ ID */
	FITS_WRITE_ITEM (FL->timeRange, p_fitsbuf);	  /* TIME RANAGE */
	FITS_WRITE_ARRAY(bandMask, p_fitsbuf, FL->nBand); /* BANDS */
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
	char bandFormInt[4];

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
	char line[1000];
	char antName[10];
	int refDay;
	int i, c, d, p, v;
	int hasData[2][array_MAX_BANDS];
	int recChan;
	int configId = 0;	/* currently only support 1 config */
	int antId;
	char polName;
	int freqNum;
	FILE *in;
	FlagDatum FL;
	DifxConfig *dc;
	DifxDSEntry *ds;
	
	FL.nBand = p_fits_keys->no_band;
	sprintf(bandFormInt, "%dJ", FL.nBand);
	
	in = fopen("flag", "r");
	
	if(!in || D == 0)
	{
		return D;
	}

	nColumn = NELEMENTS(columns);
	nRowBytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS format */
	fitsbuf = (char *)calloc(nRowBytes, 1);
	if(fitsbuf == 0)
	{
		return 0;
	}

	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "FLAG");
	arrayWriteKeys (p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 2, "");
	fitsWriteEnd(out);

	start = D->mjdStart - (int)D->mjdStart;
	stop = start + D->duration/86400.0;

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
		fgets(line, 999, in);
		if(feof(in))
		{
			break;
		}
			
		/* ignore possible comment lines */
		if(line[0] == '#')
		{
			continue;
		}
		else if(parseFlag(line, refDay, antName, FL.timeRange, 
			FL.reason, &recChan))
		{
			if(strncmp(FL.reason, "recorder", 8) == 0)
			{
				continue;
			}

			antId = DifxInputGetAntennaId(D, antName);
			if(antId < 0)
			{
				continue;
			}

			/* convert the recorder channel number into FITS
			 * useful values -- the IF index (bandId) and the
			 * polarization index (polId).  Both are zero-based
			 * numbers, with -1 implying "all values"
			 */
			v = DifxConfigRecChan2IFPol(D, configId,
				antId, recChan, &FL.bandId, &FL.polId);
			if(v < 0)
			{
				fprintf(stderr, "Flag row ignored: %s \n",
					line);
				continue;
			}

			if(strcmp(FL.reason, "observing system idle") == 0)
			{
				FL.timeRange[1] = 1.0;
			}
			
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

			FL.baselineId1[0] = antId + 1;

			writeFLrow(out, fitsbuf, nRowBytes, 
				columns, nColumn, &FL);
		}
	}

	/* Make flags for bandId/polIds that were not observed */
	FL.timeRange[0] = start;
	FL.timeRange[1] = stop;
	strcpy(FL.reason, "This Band/Pol not observed");

	FL.freqId1 = 0;
	for(configId = 0; configId < D->nConfig; configId++)
	{
	    /* want to loop only over unique freqIds */
	    if(D->config[configId].freqId < FL.freqId1)
	    {
	    	continue;       /* this freqId1 done already */
	    }
	    FL.freqId1 = D->config[configId].freqId + 1;
	    dc = D->config + configId;
	    for(d = 0; dc->indexDS[d] >= 0; d++)
	    {
		ds = D->dsentry + dc->indexDS[d];
		FL.baselineId1[0] = ds->antId + 1;

		/* populate a "presense" matrix. */
		for(p = 0; p < dc->nPol; p++)
		{
			for(i = 0; i < dc->nIF; i++)
			{
				hasData[p][i] = 0;
			}
		}
		for(c = 0; c < ds->nRecChan; c++)
		{
			polName = ds->RCpolName[c];
			freqNum = ds->RCfreqId[c];
			i = dc->freqId2IF[freqNum];
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
					writeFLrow(out, fitsbuf, nRowBytes,
						columns, nColumn, &FL);
				}
			}
		}
	    }
	}

	/* close the file, free memory, and return */
	fclose(in);
	free(fitsbuf);

	return D;
}
