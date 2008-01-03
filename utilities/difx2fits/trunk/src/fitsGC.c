#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <ctype.h>
#include <strings.h>
#include "difx2fits.h"
#include "other.h"



#define MAXENTRIES	5000
#define MAXTOKEN	512
#define MAXTAB		6

typedef struct
{
	double t1, t2;
	char ant[4];
	int nfreq, npoly, ndpfu, ntime;
	float freq[2];
	float poly[6];
	float dpfu[2];
	float time[8];
} GainRow;

/* freq in MHz, t in days since ref day */
static int getGainRow(GainRow *G, int nRows, double freq, double t)
{
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
	if(strstr(matcher, antennas) != 0)
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

static int parseGC(const char *filename, int row, GainRow *G)
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
				max = 6;
			}
			else if(strcasecmp(token, "TIME") == 0)
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
				*ctr++;
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

			printf("TOKEN: <%s> (%d)\n", token, c);

			if(c == EOF)
			{
				continue;
			}
			
			if(action == 1)
			{
				if(val && ctr)
				{
					if(*ctr > max)
					{
						fprintf(stderr, 
							"Too many values\n");
					}
					else
					{
						val[*ctr-1] = atof(token);
						*ctr++;
					}
				}
				action = 0;
			}
			else
			{
				testAntenna(token, G[row].ant);
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

static int GainRowsSetTime(GainRow *G, int nRow, int refday)
{
	int i;

	for(i = 0; i < nRow; i++)
	{
		if(G[i].npoly == 0 || G[i].nfreq == 0 || 
		   G[i].ntime == 0 || G[i].ndpfu == 0)
		{
			G[i].t1 = G[i].time[0] -refday +
				  G[i].time[1]/24.0 +
				  G[i].time[2]/1440.0 +
				  G[i].time[3]/86400.0;
			G[i].t2 = G[i].time[4] -refday +
				  G[i].time[5]/24.0 +
				  G[i].time[6]/1440.0 +
				  G[i].time[7]/86400.0;
		}
	}
}

const DifxInput *DifxInput2FitsGC(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	GainRow *G;
	int nRow;
	char bandFormInt[4];
	char bandFormFloat[6];
	char tabFormFloat[6];
	
	/*  define the flag FITS table columns */
	struct fitsBinTableColumn columns[] =
	{
		{"ANTENNA_NO", "1J", "antenna id from array geom. tbl", ""},
		{"ARRAY", "1J", "????", ""},
		{"FREQID", "1J", "freq id from frequency tbl", ""}, 
		{"TYPE_1", bandFormInt, "gain curve type", ""},
		{"NTERM_1", bandFormInt, "number of terms", ""},
		{"X_TYP_1", bandFormInt, "abscissa type of plot", ""},
		{"Y_TYP_1", bandFormInt, "second axis of 3d plot", ""},
		{"X_VAL_1", bandFormFloat, "For tabulated curves", ""},
		{"Y_VAL_1", tabFormFloat, "For tabulated curves", ""},
		{"GAIN_1", tabFormFloat, "Gain curve", ""},
		{"SENS_1", bandFormFloat, "Sensitivity", ""},
		{"TYPE_2", bandFormInt, "gain curve type", ""},
		{"NTERM_2", bandFormInt, "number of terms", ""},
		{"X_TYP_2", bandFormInt, "abscissa type of plot", ""},
		{"Y_TYP_2", bandFormInt, "second axis of 3d plot", ""},
		{"X_VAL_2", bandFormFloat, "For tabulated curves", ""},
		{"Y_VAL_2", tabFormFloat, "For tabulated curves", ""},
		{"GAIN_2", tabFormFloat, "Gain curve", ""},
		{"SENS_2", bandFormFloat, "Sensitivity", ""}
	};
	
	int nColumn;
	int n_row_bytes;
	char *fitsbuf, *p_fitsbuf;
	int refday;
	int i, no_band;

	no_band = p_fits_keys->no_band;
	sprintf(bandFormInt, "%dJ", no_band);
	sprintf(bandFormFloat, "%dE", no_band);
	sprintf(tabFormFloat, "%dE", MAXTAB*no_band);
	
	nColumn = NELEMENTS(columns);
	n_row_bytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS format */
	if ((fitsbuf = (char *)calloc (n_row_bytes, 1)) == 0)
	{
		return 0;
	}
	
	G = calloc(MAXENTRIES, sizeof(GainRow));
	if(!G || !D)
	{
		free(fitsbuf);
		return D;
	}

	nRow = parseGC("/home/jansky3/vlbaops/TCAL/vlba_gains.key", nRow, G);
	nRow = parseGC("/home/jansky3/vlbaops/TCAL/gain.ar", nRow, G);
	nRow = parseGC("/home/jansky3/vlbaops/TCAL/gain.eb", nRow, G);
	nRow = parseGC("/home/jansky3/vlbaops/TCAL/gain.gb", nRow, G);
	nRow = parseGC("/home/jansky3/vlbaops/TCAL/gain.y" , nRow, G);
	if(nRow < 0)
	{
		free(G);
		return D;
	}

	mjd2dayno((int)(D->mjdStart), &refday);
	GainRowsSetTime(G, nRow, refday);

	

	free(fitsbuf);
	free(G);
}
