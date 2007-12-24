#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "difx2fits.h"
#include "byteorder.h"
#include "other.h"


static void cpstr(char *dest, const char *src)
{
	dest[0] = 0;

	if(*src == 0)
	{
		return;
	}
	while(*src != '\'')
	{
		src++;
		if(!*src)
		{
			return;
		}
	}
	src++;
	while(*src != '\'')
	{
		*dest = *src;
		dest++;
		if(!*src)
		{
			*dest = 0;
			return;
		}
		src++;
	}
	*dest = 0;
}

static int parseflag(char *line, int refday, char *antname, float timerange[2], 
	char *reason)
{
	char *s;
	int d1, h1, m1, s1, d2, h2, m2, s2;

	s = strstr(line, "ant_name");
	if(!s) 
	{
		return 0;
	}
	cpstr(antname, s+9);
	
	s = strstr(line, "reason");
	if(!s) 
	{
		return 0;
	}
	cpstr(reason, s+7);
	
	s = strstr(line, "timerang=");
	if(!s)
	{
		return 0;
	}
	if(sscanf(s+9, "%d,%d,%d,%d,%d,%d,%d,%d", 
		&d1, &h1, &m1, &s1,
		&d2, &h2, &m2, &s2) != 8)
	{
		return 0;
	}
	timerange[0] = d1-refday + h1/24.0 + m1/1440.0 + s1/86400.0;
	timerange[1] = d2-refday + h2/24.0 + m2/1440.0 + s2/86400.0;

	return 1;
}

const int setant(const DifxInput *D, int ants[2], const char *antname)
{
	int a;
	
	ants[1] = 0;

	for(a = 0; a < D->nAntenna; a++)
	{
		if(strcmp(D->antenna[a].name, antname) == 0)
		{
			ants[0] = a;
			return 0;
		}
	}

	return -1;
}

const int setbandpol(const DifxInput *D, int bands[2], int pflags[4],
	const char *reason)
{
	int i, bbc;
	
	bands[0] = 1;
	bands[1] = D->nFreq;
	for(i = 0; i < 4; i++)
	{
		pflags[i] = 1;
	}

	if(strncmp("channel", reason, 7) == 0)
	{
		bbc = reason[8] - '0';
	}

	return 0;
}

const DifxInput *DifxInput2FitsFG(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, 
	FILE *calfile)
{
	/* define the columns in the FITS "FLAG" Table */
	struct __attribute__((packed)) FITS_flag
	{
		int source_id;
		int array;
		int ants[2];
		int freqid;
		float timerange[2];	/* days since ref day */
		int bands[2];
		int chans[2];
		int pflags[4];
		char reason[40];
		int severity;
	} fg_row;

	/*  define the flag FITS table columns */
	struct fitsBinTableColumn columns[] =
	{
		{"SOURCE_ID", "1J", "source id number from source tbl"},
		{"ARRAY", "1J", "????"},
		{"ANTS", "2J", "antenna id from antennas tbl"},
		{"FREQID", "1J", "freq id from frequency tbl"},
		{"TIMERANG", "2E", "time flag condition begins, ends", "DAYS"},
		{"BANDS", "2J", "band range to be flagged"},
		{"CHANS", "2J", "channel range to be flagged"},
		{"PFLAGS", "4J", "flag array for polarization"},
		{"REASON", "40A", "reason for data to be flagged bad"},
		{"SEVERITY", "1J", "severity code"}
	};

	int nColumn;
	int n_row_bytes, irow;
	char *fitsbuf;
	double start, stop;
	int swap;
	char line[1000];
	char reason[64], antname[10];
	int refday;
	int i;

	if(!calfile || D == 0)
	{
		return D;
	}

	swap = (byteorder() == BO_LITTLE_ENDIAN);

	nColumn = NELEMENTS(columns);
	
	n_row_bytes = FitsBinTableSize(columns, NELEMENTS(columns));

	/* malloc space for storing table in FITS format */
	if ((fitsbuf = (char *)malloc (n_row_bytes)) == 0)
	{
		return 0;
	}

	fitsWriteBinTable(out, nColumn, columns, n_row_bytes, "FLAG");

	arrayWriteKeys (p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteEnd(out);

	start = D->mjdStart - (int)D->mjdStart;
	stop = start + D->duration/86400.0;

	mjd2dayno((int)(D->mjdStart), &refday);
	
	for(;;)
	{
		fgets(line, 999, calfile);
		if(feof(calfile))
		{
			return;
		}
			
		if(strncmp(line+1, " ----- ", 7) == 0 &&
		   strncmp(line+8, "Edit data", 9) != 0)
		{
			return;
		}
		if(line[0] == '!')
		{
			continue;
		}
		else if(parseflag(line, refday, antname, fg_row.timerange, 
			reason))
		{
			if(strncmp(reason, "recorder", 8) == 0)
			{
				continue;
			}
			if(strcmp(reason, "observing system idle") == 0)
			{
				fg_row.timerange[1] = 1.0;
			}
			
			if(fg_row.timerange[0] > stop ||
			   fg_row.timerange[1] < start)
			{
				continue;
			}
			if(fg_row.timerange[0] < start)
			{
				fg_row.timerange[0] = start;
			}
			if(fg_row.timerange[1] > stop)
			{
				fg_row.timerange[1] = stop;
			}

			fg_row.source_id = 0;
			fg_row.array = 0;
			fg_row.freqid = 0;
			fg_row.chans[0] = 0;
			fg_row.chans[1] = 0;
			fg_row.severity = -1;
	
			if(setant(D, fg_row.ants,  antname) < 0)
			{
				continue;
			}
			if(setbandpol(D, fg_row.bands, fg_row.pflags, 
				reason) < 0)
			{
				continue;
			}
			for(i = 0; i < 40; i++)
			{
				fg_row.reason[i] = 0;
			}
			strncpy(fg_row.reason, reason, 40);
			
			if(swap)
			{
				FitsBinRowByteSwap(columns, 
					NELEMENTS(columns), &fg_row);
			}
			fitsWriteBinRow(out, (char *)&fg_row);
		}
	}

	return D;
}
