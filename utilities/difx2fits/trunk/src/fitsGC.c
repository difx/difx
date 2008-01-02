#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "difx2fits.h"
#include "byteorder.h"
#include "other.h"



#define MAXENTRIES	5000
#define MAXTOKEN	512

typedef struct
{
	char ant[4];
	int nfreq, npoly, ndpfu, ntime;
	float freq[2];
	float poly[6];
	float dpfu[2];
	float time[8];
} GainRow;

static int testAntenna(const char *token, char *value)
{
	const char antennas[] = 
		" AR BR EB FD GB HN KP LA MK NL OV PT SC Y Y1 Y27 ";
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


const DifxInput *DifxInput2FitsGC(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	GainRow *G;
	int nRow;

	G = calloc(MAXENTRIES, sizeof(GainRow));
	if(!G || !D)
	{
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

	free(G);
}
