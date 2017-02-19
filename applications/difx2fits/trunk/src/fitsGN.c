/***************************************************************************
 *   Copyright (C) 2008-2017 by Walter Brisken                             *
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
#include <ctype.h>
#include <strings.h>
#include <glob.h>
#include "config.h"
#include "difx2fits.h"
#include "util.h"
#include "other.h"


#define MAXENTRIES		8000UL
#define MAXTOKEN		512
#define MAXTAB			6
#define N_VLBA_BANDS		12
#define ANTENNA_NAME_LENGTH	4

typedef struct
{
	float mjd1, mjd2;
	int band;
	char antName[ANTENNA_NAME_LENGTH];
	int nFreq, nPoly, nDPFU, nTime;
	float freq[2];
	float poly[MAXTAB];
	float DPFU[2];
	float time[8];
	float sxFlag;	/* 1 if it is an sx mode, otherwise 0 */
} GainRow;
	
static const float bandEdges[N_VLBA_BANDS+1] = 
{
	0, 	/* 90cm P  */
	450,	/* 50cm    */
	900, 	/* 21cm L  */
	1550, 	/* 18cm L  Different from above due to two separate gain curve files */
	2000,	/* 13cm S  */
	4000,	/* 6cm  C  */
	6000,	/* 4cm  X  */
	10000,	/* 2cm  U  */
	18000, 	/* 1cm  K  */
	26000, 	/*      Ka  Not yet existing */
	40000,	/* 7mm  Q  */
	70000,	/* 3mm  W  */
	100000	/* end of list marker */
};

/* function that just renames matching antennas in the table to render them useless */
static void nullifyGainRows(GainRow *G, int nRow, const char *antenna)
{
	int r;

	for(r = 0; r < nRow; ++r)
	{
		if(strcasecmp(antenna, G[r].antName) == 0)
		{
			strcpy(G[r].antName, "-");
		}
	}
}

/* freq in MHz, t in days since ref day */
/* if sxFlag is set, only look for 4cmsx or 13cmsx */
static int getGainRow(GainRow *G, int nRow, const char *antName, double freq, double mjd, int sxFlag)
{
	int i, r;
	int bestr = -1;
	int band = -1;
	int eband, dband;
	double efreq, dfreq;

	efreq = 1e11;
	eband = N_VLBA_BANDS;

	for(i = 0; i < N_VLBA_BANDS; ++i)
	{
		if(freq > bandEdges[i] && freq < bandEdges[i+1])
		{
			band = i;
		}
	}
	if(band < 0)
	{
		return -1;
	}

	for(r = 0; r < nRow; ++r)
	{
		if(strcmp(antName, G[r].antName) != 0)
		{
			continue;
		}
		if(fabs(sxFlag - G[r].sxFlag) > 0.01)
		{
			continue;
		}
		if(mjd <= G[r].mjd1 || mjd > G[r].mjd2)
		{
			continue;
		}
		dband = abs(band-G[r].band);
		if(dband > 1)
		{
			continue;
		}
		dfreq = fabs(band-G[r].freq[0]);
		if(dband < eband || (dband == eband && dfreq < efreq))
		{
			bestr = r;
			eband = dband;
			efreq = dfreq;
		}
	}

	return bestr;
}

static int isHSAAntenna(const char *token)
{
	const char antennas[] = " AR BR EB FD GB HN KP LA MK NL OV PT SC Y ";
	char matcher[8];

	if(strlen(token) >= ANTENNA_NAME_LENGTH)
	{
		return 0;
	}
	sprintf(matcher, " %s ", token);
	if(strstr(antennas, matcher) != 0)
	{
		return 1;
	}
	else
	{	
		return 0;
	}
}

static int isVLITEAntenna(const char *token)
{
	if(strlen(token) != 2)
	{
		return 0;
	}
	if(token[0] != 'V')
	{
		return 0;
	}
	if(token[1] < '0' || token[1] > '9')
	{
		return 0;
	}

	return 1;
}
				
static int getNoQuote(char firstchar, FILE *in, char *token)
{
	int c, i = 1;
	
	token[0] = firstchar;
	
	for(;;)
	{
		c = fgetc(in);
		if(c == EOF)
		{
			token[i] = 0;

			return EOF;
		}
		else if(isalnum(c) || c == '.' || c == '-' || c == '+')
		{
			token[i] = c;
			++i;
			if(i >= MAXTOKEN)
			{
				token[i] = 0;

				return EOF;
			}
		}
		else
		{
			token[i] = 0;

			return c;
		}
	}
}

static int getQuote(FILE *in, char *token)
{
	int c, i = 0;

	for(;;)
	{
		c = fgetc(in);
		if(c == EOF)
		{
			token[i] = 0;

			return EOF;
		}
		else if(c == '\'')
		{
			token[i] = 0;

			return 0;
		}
		else if(c == '\n')
		{
			token[i] = 0;
			fprintf(stderr, "Warning: Gain table parsing: EOL found in quotes: %s\n", token);

			return 0;
		}
		else
		{
			token[i] = c;
			++i;
			if(i >= MAXTOKEN)
			{
				token[i] = 0;

				return EOF;
			}
		}
	}
}

static int parseGN(const char *filename, int row, GainRow *G)
{
	FILE *in;
	int c = 0;
	char token[MAXTOKEN+1] = "";
	int *ctr = 0;
	int max = 0;
	float *val = 0;
	int action = 0;	/* State variable : 0= set LHS, 1= set RHS */
	int v;

	if(row < 0)
	{
		return row;
	}
	
	in = fopen(filename, "r");
	if(!in)
	{
		return row;
	}
	
	for(;;)
	{
		if(!c)
		{
			c = fgetc(in);
		}
		if(c == EOF)
		{
			break;
		}
		else if(c == '=')
		{
			if(strcasecmp(token, "DPFU") == 0)
			{
				val = G[row].DPFU;
				ctr = &G[row].nDPFU;
				max = 2;
			}
			else if(strcasecmp(token, "FREQ") == 0)
			{
				val = G[row].freq;
				ctr = &G[row].nFreq;
				max = 2;
			}
			else if(strcasecmp(token, "POLY") == 0)
			{
				val = G[row].poly;
				ctr = &G[row].nPoly;
				max = MAXTAB;
			}
			else if(strcasecmp(token, "TIMERANG") == 0)
			{
				val = G[row].time;
				ctr = &G[row].nTime;
				max = 8;
			}
			else if(strcasecmp(token, "BAND") == 0)
			{
				val = &(G[row].sxFlag);
				ctr = 0;
				max = 1;
			}
			else
			{
				val = 0;
				ctr = 0;
				max = 0;
			}
			if(ctr)
			{
				*ctr = 1;
			}
			action = 1;
			c = 0;
		}
		else if(c == '!') /* handle comment */
		{
			const char *rv;

			/* get rest of line */
			rv = fgets(token, MAXTOKEN, in);
			token[0] = 0;
			action = 0;
			c = 0;
			if(!rv)
			{
				fprintf(stderr, "Warning: parseGN: How odd, end of input after comment character!\n");
			
				break;
			}
		}
		else if(c == ',')
		{
			if(ctr)
			{
				++(*ctr);
			}
			action = 1;
			c = 0;
		}
		if(c == '/')
		{
			++row;
			action = 0;
			if(row >= MAXENTRIES)
			{
				fprintf(stderr, "Error: parseGN: too many rows in file %s\n", filename);
				fclose(in);

				return -1;
			}
			c = 0;
		}
		else if(c > ' ')
		{
			if(c == '\'')
			{
				c = getQuote(in, token);
			}
			else
			{
				c = getNoQuote(c, in, token);
			}

			if(c == EOF)
			{
				continue;
			}
			
			if(action == 1)
			{
				if(val && ctr)
				{
					if((*ctr) > max)
					{
						fprintf(stderr, "Too many values %d>%d\n", *ctr, max);
					}
					else
					{
						val[(*ctr)-1] = atof(token);
					}
				}
				else if(val)	/* must be sxFlag variable */
				{
					if(strcasestr(token, "sx") != 0)
					{
						val[0] = 1.0;
					}
					else
					{
						val[0] = 0.0;
					}
				}
				action = 0;
			}
			else
			{
				if(isHSAAntenna(token) || isVLITEAntenna(token))
				{
					v = snprintf(G[row].antName, ANTENNA_NAME_LENGTH, "%s", token);
					if(v >= ANTENNA_NAME_LENGTH)
					{
						fprintf(stderr, "Developer error: antenna name wanted %d characters while only %d are allowed\n", v, ANTENNA_NAME_LENGTH-1);
					}
				}

			}
			
		}
		else
		{
			c = 0;
		}
	}

	fclose(in);

	return row;
}

static void GainRowsSetTimeBand(GainRow *G, int nRow)
{
	int i, j;
	double freq;

	for(i = 0; i < nRow; ++i)
	{
		if(G[i].antName[0] == '-')
		{
			continue;
		}
		if(G[i].nPoly != 0 && G[i].nFreq != 0 && 
		   G[i].nTime != 0 && G[i].nDPFU != 0)
		{
			G[i].mjd1 = ymd2mjd(G[i].time[0], 
					    G[i].time[1], 
					    G[i].time[2]) + 
					    G[i].time[3]/24.0;
			G[i].mjd2 = ymd2mjd(G[i].time[4], 
					    G[i].time[5], 
					    G[i].time[6]) +
					    G[i].time[7]/24.0;
		}
		for(j = 0; j < N_VLBA_BANDS; ++j)
		{
			freq = G[i].freq[0];
			if(freq > bandEdges[j] && freq < bandEdges[j+1])
			{
				G[i].band = j;
			}
		}
	}
}

int loadGainCurves(const DifxInput *D, GainRow *G)
{
	char *path;
	int nRow = 0;
	int v;
	unsigned int f;
	int antId;

	/* In this function we first load up all the gain files foind in $GAIN_CURVE_PATH.
	 * Then we look for local gain files called <exper>.<ant>.gain .  If found, the
	 * already loaded data is invalidated and the per-antenna file is loaded.
	 */

	path = getenv("GAIN_CURVE_PATH");
	if(path == 0)
	{
		printf("\n    GAIN_CURVE_PATH not set; only using local gain files.\n");
		printf("                            ");
	}
	else
	{
		char pattern[DIFXIO_FILENAME_LENGTH];
		glob_t files;
		
		memset((char *)&files, 0, sizeof(glob_t));
		v = snprintf(pattern, DIFXIO_FILENAME_LENGTH, "%s/*", path);
		if(v >= DIFXIO_FILENAME_LENGTH)
		{
			fprintf(stderr, "\n    loadGainCurves: glob pattern too long.\n");
			printf("                            ");

			return -2;
		}
		glob(pattern, 0, 0, &files);
		if(files.gl_pathc == 0)
		{
			printf("\n    No files found in $GAIN_CURVE_PATH\n");
			printf("                            ");
		}
		else
		{
			for(f = 0; f < files.gl_pathc; ++f)
			{
				nRow = parseGN(files.gl_pathv[f], nRow, G);
			}
		}

		globfree(&files);
	}

	for(antId = 0; antId < D->nAntenna; ++antId)
	{
		char gainFile[DIFXIO_FILENAME_LENGTH];

		v = snprintf(gainFile, DIFXIO_FILENAME_LENGTH, "%s%s.%s.gain", D->job->obsCode, D->job->obsSession, D->antenna[antId].name);
		if(v >= DIFXIO_FILENAME_LENGTH)
		{
			fprintf(stderr, "\n\nDeveloper error: DifxInput2FitsGN: DIFXIO_FILENAME_LENGTH=%d is too small.  Wants to be %d.\n", DIFXIO_FILENAME_LENGTH, v+1);

			exit(EXIT_FAILURE);
		}

		v = globcase(__FUNCTION__, "*.*.gain", gainFile);
		if(v == 0)
		{
			/* no matching file */

			continue;
		}
	
		nullifyGainRows(G, nRow, D->antenna[antId].name);

		nRow = parseGN(gainFile, nRow, G);
	}

	if(nRow > 0)
	{
		GainRowsSetTimeBand(G, nRow);
	}

	return nRow;
}

const DifxInput *DifxInput2FitsGN(const DifxInput *D, struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	GainRow *G;
	int nRow;
	char bandFormInt[8];
	char bandFormFloat[8];
	char tabFormFloat[8];
	
	/*  define the flag FITS table columns */
	struct fitsBinTableColumn columns[] =
	{
		{"ANTENNA_NO", "1J", "antenna id from array geom. tbl", 0},
		{"ARRAY", "1J", "????", 0},
		{"FREQID", "1J", "freq id from frequency tbl", 0}, 
		{"TYPE_1", bandFormInt, "gain curve type", 0},
		{"NTERM_1", bandFormInt, "number of terms", 0},
		{"X_TYP_1", bandFormInt, "abscissa type of plot", 0},
		{"Y_TYP_1", bandFormInt, "second axis of 3d plot", 0},
		{"X_VAL_1", bandFormFloat, "For tabulated curves", 0},
		{"Y_VAL_1", tabFormFloat, "For tabulated curves", 0},
		{"GAIN_1", tabFormFloat, "Gain curve", 0},
		{"SENS_1", bandFormFloat, "Sensitivity", "K/JY"},
		{"TYPE_2", bandFormInt, "gain curve type", 0},
		{"NTERM_2", bandFormInt, "number of terms", 0},
		{"X_TYP_2", bandFormInt, "abscissa type of plot", 0},
		{"Y_TYP_2", bandFormInt, "second axis of 3d plot", 0},
		{"X_VAL_2", bandFormFloat, "For tabulated curves", 0},
		{"Y_VAL_2", tabFormFloat, "For tabulated curves", 0},
		{"GAIN_2", tabFormFloat, "Gain curve", 0},
		{"SENS_2", bandFormFloat, "Sensitivity", "K/JY"}
	};
	
	int nColumn;
	int nRowBytes;
	char *fitsbuf, *p_fitsbuf;
	int antId;
	int r, i, j, p, nBand, nPol;
	const char *antName;
	float xVal[array_MAX_BANDS];
	float yVal[MAXTAB*array_MAX_BANDS];
	int32_t curveType [array_MAX_BANDS];
	int32_t xType[array_MAX_BANDS];
	int32_t yType[array_MAX_BANDS];
	int32_t nTerm[array_MAX_BANDS];
	float gain[MAXTAB*array_MAX_BANDS];
	float sens[2][array_MAX_BANDS];
	int bad;
	double mjd;
	double freq;
	int messages = 0;
	/* 1-based indices for FITS file */
	int32_t antId1, freqId1, arrayId1;

	/* Note: This is a particular NaN variant the FITS-IDI format/convention 
	 * wants, namely 0xFFFFFFFF */
	union
	{
		int32_t i32;
		float f;
	} nan;
	nan.i32 = -1;

	if(!D)
	{
		return D;
	}

	G = calloc(MAXENTRIES, sizeof(GainRow));
	if(!G)
	{
		fprintf(stderr, "Error: DifxInput2FitsGN: could not allocate G (%lu bytes)\n", MAXENTRIES*sizeof(GainRow));

		exit(EXIT_FAILURE);
	}
	
	nPol = D->nPol;
	nBand = D->nIF;
	sprintf(bandFormInt, "%dJ", nBand);
	sprintf(bandFormFloat, "%dE", nBand);
	sprintf(tabFormFloat, "%dE", MAXTAB*nBand);

	if(nPol == 2)
	{
		nColumn = NELEMENTS(columns);
	}
	else
	{
		nColumn = NELEMENTS(columns) - 8;
	}
	nRowBytes = FitsBinTableSize(columns, nColumn);

	nRow = loadGainCurves(D, G);
	if(nRow < 0)
	{
		/* gain file not found.  Not necessarily a problem. */
		free(G);

		return D;
	}

	/* calloc space for storing table in FITS format */
	if((fitsbuf = (char *)calloc(nRowBytes, 1)) == 0)
	{
		fprintf(stderr, "Error: DifxInput2FitsGN: could not allocate fitsbuf (%d bytes)\n", nRowBytes);
		free(G);

		exit(EXIT_FAILURE);
	}

	/* spew out the table header */
	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "GAIN_CURVE");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "NO_POL", nPol, "");
	fitsWriteInteger(out, "NO_TABS", MAXTAB, "");
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteEnd(out);

	for(i = 0; i < array_MAX_BANDS; ++i)
	{
		curveType[i] = 2;
		xType[i] = 0;
		yType[i] = 2;
		xVal[i] = nan.f;	/* NaN */
	}
	for(i = 0; i < array_MAX_BANDS*MAXTAB; ++i)
	{
		yVal[i] = nan.f;	/* NaN */
	}
	arrayId1 = 1;
	mjd = 0.5*(D->mjdStart + D->mjdStop);

	for(antId = 0; antId < D->nAntenna; ++antId)
	{
		int freqSetId;

		freqId1 = 0;
		antName = D->antenna[antId].name;
		antId1 = antId + 1;
		bad = 0;

		for(freqSetId = 0; freqSetId < D->nFreqSet; ++freqSetId)
		{
			const DifxFreqSet *dfs;
			int sxFlag;

			dfs = D->freqSet + freqSetId;

			freqId1 = freqSetId + 1;
			
			sxFlag = isDifxFreqSetSX(dfs);
			
			for(i = 0; i < dfs->nIF; ++i)
			{
				freq = dfs->IF[i].freq;	/* MHz */
				r = getGainRow(G, nRow, antName, freq, mjd, sxFlag);
				if(r < 0)
				{
					if(messages == 0)
					{
						printf("\n");
					}
					printf("    No gain curve for station '%s'\n", antName);
					++messages;
					bad = 1;
					break;
				}
				nTerm[i] = G[r].nPoly;
				for(j = 0; j < MAXTAB; ++j)
				{
					gain[i*MAXTAB + j] = (j < nTerm[i]) ?  G[r].poly[j] : 0.0;
				}
				for(p = 0; p < nPol; ++p)
				{
					sens[p][i] = G[r].DPFU[p];
				}
			}
			if(bad == 1)
			{
				continue;
			}
			
			p_fitsbuf = fitsbuf;
			
			FITS_WRITE_ITEM (antId1, p_fitsbuf);
			FITS_WRITE_ITEM (arrayId1, p_fitsbuf);
			FITS_WRITE_ITEM (freqId1, p_fitsbuf);

			for(p = 0; p < nPol; ++p)
			{
				FITS_WRITE_ARRAY(curveType, p_fitsbuf, nBand);
				FITS_WRITE_ARRAY(nTerm, p_fitsbuf, nBand);
				FITS_WRITE_ARRAY(xType, p_fitsbuf, nBand);
				FITS_WRITE_ARRAY(yType, p_fitsbuf, nBand);
				FITS_WRITE_ARRAY(xVal, p_fitsbuf, nBand);
				FITS_WRITE_ARRAY(yVal, p_fitsbuf, nBand*MAXTAB);
				FITS_WRITE_ARRAY(gain, p_fitsbuf, nBand*MAXTAB);
				FITS_WRITE_ARRAY(sens[p], p_fitsbuf, nBand);
			}

			testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "GN");

#ifndef WORDS_BIGENDIAN
			FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
			fitsWriteBinRow(out, fitsbuf);
		}
	}

	/* free allocated memory */
	free(fitsbuf);
	free(G);

	if(messages > 0)
	{
		printf("                            ");
	}

	return D;
}
