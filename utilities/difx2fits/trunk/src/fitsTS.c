#include <math.h>
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
	float tsys;

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
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
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
	float tSysRecChan[16], tSys[2][16], tAnt[16];
	int nBand;
	int configId = 0;	/* only support one config now */
	int i, j, nPol=0;
	int bandId, polId, antId;
	int nRecChan;
	int v;
	double f;
	double time, mjd, mjdStop;
	float timeInt;
	FILE *in;
	/* The following are 1-based indices for writing to FITS */
	int32_t sourceId1, freqId1, arrayId1, antId1;
	
	nBand = D->nIF;
	nPol = D->nPol;

	sprintf(bandFormFloat, "%dE", nBand);

	if(D == 0)
	{
		return D;
	}

	mjd2dayno((int)(D->mjdStart), &refDay);

	/* get the maximum dimensions possibly needed */
	f = D->mjdStart - (int)(D->mjdStart);

	in = fopen("tsys", "r");

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

	mjdStop = D->mjdStart + D->duration/86400.0;

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
		((unsigned int *)tAnt)[i] = -1;	/* set to NaN */
	}
	
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
		else 
		{
			nRecChan = parseTsys(line, antName, &time, 
				&timeInt, tSysRecChan);

			/* discard records outside time range */
			time -= refDay;
			mjd = time + (int)(D->mjdStart);
			if(mjd < D->mjdStart || mjd > mjdStop)
			{
				continue;
			}
		
			/* discard records for unused antennas */
			antId = DifxInputGetAntennaId(D, antName);
			if(antId < 0)
			{
				continue;
			}

			sourceId1 = DifxInputGetSourceId(D, mjd) + 1;

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
			v = 0;
			for(i = 0; i < nRecChan && v >= 0; i++)
			{
				v = DifxConfigRecChan2IFPol(D, configId,
					antId, i, &bandId, &polId);
				if(bandId < 0 || polId < 0)
				{
					fprintf(stderr, "Error: derived "
						"bandId and polId (%d,%d) are "
						"not legit.  From "
						"recChan=%d.\n", 
						bandId, polId, i);
				}
				tSys[polId][bandId] = tSysRecChan[i];
			}
			if(v < 0)
			{
				continue;
			}

			antId1 = antId + 1;	/* 1-based value for FITS */
		
			p_fitsbuf = fitsbuf;
		
			FITS_WRITE_ITEM (time, p_fitsbuf);
			FITS_WRITE_ITEM (timeInt, p_fitsbuf);
			FITS_WRITE_ITEM (sourceId1, p_fitsbuf);
			FITS_WRITE_ITEM (antId1, p_fitsbuf);
			FITS_WRITE_ITEM (arrayId1, p_fitsbuf);
			FITS_WRITE_ITEM (freqId1, p_fitsbuf);

			for(i = 0; i < nPol; i++)
			{
				FITS_WRITE_ARRAY(tSys, p_fitsbuf, nBand);
				FITS_WRITE_ARRAY(tAnt, p_fitsbuf, nBand);
			}

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
