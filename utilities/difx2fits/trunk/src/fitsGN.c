#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <ctype.h>
#include <strings.h>
#include <glob.h>
#include "config.h"
#include "difx2fits.h"
#include "other.h"


#define MAXENTRIES	5000
#define MAXTOKEN	512
#define MAXTAB		6
#define MAXIFS		16
#define NBANDS		11

typedef struct
{
	float mjd1, mjd2;
	int band;
	char antName[4];
	int nfreq, npoly, ndpfu, ntime;
	float freq[2];
	float poly[MAXTAB];
	float dpfu[2];
	float time[8];
} GainRow;
	
static const float bandEdges[NBANDS+1] = 
{
	0, 	/* 90cm P  */
	450,	/* 50cm    */
	900, 	/* 20cm L  */
	2000,	/* 13cm S  */
	4000,	/* 6cm  C  */
	6000,	/* 4cm  X  */
	10000,	/* 2cm  U  */
	18000, 	/* 1cm  K  */
	26000, 	/* 9mm  Ka */
	40000,	/* 7mm  Q  */
	70000,	/* 3mm  W  */
	100000
};

/* freq in MHz, t in days since ref day */
static int getGainRow(GainRow *G, int nRow, const char *antName,
	double freq, float mjd)
{
	int i, r;
	int bestr = -1;
	int band = -1;
	int eband, dband;
	double efreq, dfreq;

	efreq = 1e11;
	eband = NBANDS;

	for(i = 0; i < NBANDS; i++)
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

	for(r = 0; r < nRow; r++)
	{
		if(mjd <= G[r].mjd1 || mjd > G[r].mjd2)
		{
			continue;
		}
		if(strcmp(antName, G[r].antName) != 0)
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

static int testAntenna(const char *token, char *value)
{
	const char antennas[] = " AR BR EB FD GB HN KP LA MK NL OV PT SC Y ";
	char matcher[8];

	if(strlen(token) > 4)
	{
		return -1;
	}
	sprintf(matcher, " %s ", token);
	if(strstr(antennas, matcher) != 0)
	{
		strcpy(value, token);
	}
	
	return 0;
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
			i++;
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
			fprintf(stderr, "Warning -- EOL found in quotes: %s\n",
				token);
			return 0;
		}
		else
		{
			token[i] = c;
			i++;
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
				val = G[row].dpfu;
				ctr = &G[row].ndpfu;
				max = 2;
			}
			else if(strcasecmp(token, "FREQ") == 0)
			{
				val = G[row].freq;
				ctr = &G[row].nfreq;
				max = 2;
			}
			else if(strcasecmp(token, "POLY") == 0)
			{
				val = G[row].poly;
				ctr = &G[row].npoly;
				max = MAXTAB;
			}
			else if(strcasecmp(token, "TIMERANG") == 0)
			{
				val = G[row].time;
				ctr = &G[row].ntime;
				max = 8;
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
			fgets(token, 999, in);
			token[0] = 0;
			action = 0;
			c = 0;
		}
		else if(c == ',')
		{
			if(ctr)
			{
				(*ctr)++;
			}
			action = 1;
			c = 0;
		}
		if(c == '/')
		{
			row++;
			action = 0;
			if(row >= MAXENTRIES)
			{
				fprintf(stderr, "ERROR -- too many rows\n");
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
						fprintf(stderr, 
					"Too many values %d>%d\n", *ctr, max);
					}
					else
					{
						val[(*ctr)-1] = atof(token);
					}
				}
				action = 0;
			}
			else
			{
				testAntenna(token, G[row].antName);
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

	for(i = 0; i < nRow; i++)
	{
		if(G[i].npoly != 0 && G[i].nfreq != 0 && 
		   G[i].ntime != 0 && G[i].ndpfu != 0)
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
		for(j = 0; j < NBANDS; j++)
		{
			freq = G[i].freq[0];
			if(freq > bandEdges[j] && freq < bandEdges[j+1])
			{
				G[i].band = j;
			}
		}
	}
}

int loadGainCurves(GainRow *G)
{
	char *path;
	char pattern[256];
	glob_t files;
	int nRow = 0;
	unsigned int f;
	
	path = getenv("GAIN_CURVE_PATH");
	if(path == 0)
	{
		printf("\n    GAIN_CURVE_PATH not set -- skipping GN table.\n");
		printf("                            ");
		return -1;
	}
	
	memset((char *)&files, sizeof(glob_t), 0);
	sprintf(pattern, "%s/*", path);
	glob(pattern, 0, 0, &files);
	if(files.gl_pathc == 0)
	{
		printf("\n    No files found in $GAIN_CURVE_PATH\n");
		printf("                            ");
		return -2;
	}
	
	for(f = 0; f < files.gl_pathc; f++)
	{
		nRow = parseGN(files.gl_pathv[f], nRow, G);
	}

	if(nRow > 0)
	{
		GainRowsSetTimeBand(G, nRow);
	}

	return nRow;
}

const DifxInput *DifxInput2FitsGN(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	GainRow *G;
	int nRow;
	char bandFormInt[4];
	char bandFormFloat[4];
	char tabFormFloat[4];
	
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
	int n_row_bytes;
	char *fitsbuf, *p_fitsbuf;
	int c, r, a, i, j, p, no_band, no_pol;
	const char *antName;
	float NaNs[MAXTAB*MAXIFS];
	int Twos[MAXIFS], Zeros[MAXIFS];
	int nTerm[MAXIFS];
	float gain[MAXTAB*MAXIFS];
	float sens[2][MAXIFS];
	int antId, freqId, arrayId;
	int bad;
	int mjd;
	double freq;
	int messages = 0;

	G = calloc(MAXENTRIES, sizeof(GainRow));
	if(!G || !D)
	{
		free(fitsbuf);
		return D;
	}
	
	no_pol = D->nPol;
	no_band = D->nIF;
	sprintf(bandFormInt, "%dJ", no_band);
	sprintf(bandFormFloat, "%dE", no_band);
	sprintf(tabFormFloat, "%dE", MAXTAB*no_band);
	
	if(no_pol == 2)
	{
		nColumn = NELEMENTS(columns);
	}
	else
	{
		nColumn = NELEMENTS(columns) - 8;
	}
	n_row_bytes = FitsBinTableSize(columns, nColumn);

	nRow = loadGainCurves(G);
	if(nRow < 0)
	{
		free(G);
		return D;
	}

	/* calloc space for storing table in FITS format */
	if((fitsbuf = (char *)calloc(n_row_bytes, 1)) == 0)
	{
		free(G);
		return 0;
	}

	/* spew out the table header */
	fitsWriteBinTable(out, nColumn, columns, n_row_bytes, "GAIN_CURVE");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "NO_POL", no_pol, "");
	fitsWriteInteger(out, "NO_TABS", MAXTAB, "");
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteEnd(out);

	for(i = 0; i < MAXTAB*MAXIFS; i++)
	{
		((int *)NaNs)[i] = -1;
	}
	for(i = 0; i < MAXIFS; i++)
	{
		Twos[i] = 2;
		Zeros[i] = 0;
	}
	arrayId = 1;
	freqId = 0;
	mjd = D->mjdStart + 0.5*D->duration/86400.0;

	for(a = 0; a < D->nAntenna; a++)
	{
		antName = D->antenna[a].name;
		antId = a+1;
		bad = 0;

		for(c = 0; c < D->nConfig; c++)
		{
			if(D->config[c].freqId < freqId)
			{
				continue;	/* this freqId done already */
			}
			freqId = D->config[c].freqId + 1;
			for(i = 0; i < no_band; i++)
			{
				freq = D->config[c].IF[i].freq;	/* MHz */
				r = getGainRow(G, nRow, antName, freq, mjd);
				if(r < 0)
				{
					if(messages == 0)
					{
						printf("\n");
					}
					fprintf(stderr, "    No gain curve for %s\n",
						antName);
					messages++;
					bad = 1;
					break;
				}
				nTerm[i] = G[r].npoly;
				for(j = 0; j < MAXTAB; j++)
				{
					gain[i*MAXTAB + j] = 
						(j < nTerm[i]) ? G[r].poly[j] : 0.0;
				}
				for(p = 0; p < no_pol; p++)
				{
					sens[p][i] = G[r].dpfu[p];
				}
			}
			if(bad == 1)
			{
				continue;
			}
			
			p_fitsbuf = fitsbuf;
			
			/* ANTENNA_NO */
			bcopy((char *)&antId, p_fitsbuf, sizeof(antId));
			p_fitsbuf += sizeof(antId);

			/* ARRAY */
			bcopy((char *)&arrayId, p_fitsbuf, sizeof(arrayId));
			p_fitsbuf += sizeof(arrayId);

			/* FREQ_ID */
			bcopy((char *)&freqId, p_fitsbuf, sizeof(freqId));
			p_fitsbuf += sizeof(freqId);

			for(p = 0; p < no_pol; p++)
			{
				/* TYPE */
				bcopy((char *)Twos, p_fitsbuf, no_band*sizeof(float));
				p_fitsbuf += no_band*sizeof(float);

				/* NTERM */
				bcopy((char *)nTerm, p_fitsbuf, no_band*sizeof(int));
				p_fitsbuf += no_band*sizeof(int);

				/* X_TYP */
				bcopy((char *)Zeros, p_fitsbuf, no_band*sizeof(float));
				p_fitsbuf += no_band*sizeof(float);

				/* Y_TYP */
				bcopy((char *)Twos, p_fitsbuf, no_band*sizeof(float));
				p_fitsbuf += no_band*sizeof(float);

				/* X_VAL */
				bcopy((char *)NaNs, p_fitsbuf, no_band*sizeof(float));
				p_fitsbuf += no_band*sizeof(float);

				/* Y_VAL */
				bcopy((char *)NaNs, p_fitsbuf, 
					MAXTAB*no_band*sizeof(float));
				p_fitsbuf += MAXTAB*no_band*sizeof(float);

				/* GAIN */
				bcopy((char *)gain, p_fitsbuf, 
					MAXTAB*no_band*sizeof(float));
				p_fitsbuf += MAXTAB*no_band*sizeof(float);

				/* SENSITIVITY */
				bcopy((char *)(sens[p]), p_fitsbuf, 
					no_band*sizeof(float));
				p_fitsbuf += no_band*sizeof(float);
			}

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
