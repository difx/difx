#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"
#include "other.h"

static int getShape(const char *filename, double t1, double t2,
	int *no_pol, int *no_band)
{
	FILE *in;
	char line[1000];
	double t;
	int np, nb, n;

	in = fopen(filename, "r");
	if(in == 0)
	{
		return -1;
	}

	for(;;)
	{
		fgets(line, 999, in);
		if(feof(in))
		{
			break;
		}
		if(line[0] == '#')
		{
			continue;
		}
		n = sscanf(line, "%*s%lf%*f%d%d", &t, &np, &nb);
		if(n != 3)
		{
			continue;
		}
		if(t >= t1 && t <= t2)
		{
			if(np > *no_pol)
			{
				*no_pol = np;
			}
			if(nb > *no_band)
			{
				*no_band = nb;
			}
		}
	}
	fclose(in);

	return 1;
}

/* ant D.O.Y. dur(days) nPol nBand (tsys)[nBand*nPol] (band)[nBand] */
static int parseTsys(const char *line, int no_pol, int no_band, char *antenna, 
	double *t, float *dt, float ty[][16])
{
	int p, n, i, j, np, nb;
	float A;

	n = sscanf(line, "%s%lf%f%d%d%n", antenna, t, dt, &np, &nb, &p);
	if(n != 5)
	{
		return -1;
	}
	if(np > no_pol || nb > no_band)
	{
		return -2;
	}

	for(i = 0; i < 16; i++)
	{
		ty[0][i] = ty[1][i] = 0.0;
	}

	for(i = 0; i < np; i++)
	{
		for(j = 0; j < nb; j++)
		{
			line += p;
			n = sscanf(line, "%f%n", &A, &p);
			if(n != 1)
			{
				return -3;
			}
			ty[i][j] = A;
		}
	}
	
	return 2;
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
	int n_row_bytes;
	char *fitsbuf, *p_fitsbuf;
	int refday;
	char line[1000];
	char antenna[20];
	float ty[2][16], tant[16];
	int no_band;
	int sourceId, freqId, arrayId, antId;
	int i, no_pol=0;
	int v;
	double f;
	double t, mjd, mjdStop;
	float dt;
	FILE *in;
	
	no_band = p_fits_keys->no_band;
	no_pol = 0;

	sprintf(bandFormFloat, "%dE", no_band);

	if(D == 0)
	{
		return D;
	}

	mjd2dayno((int)(D->mjdStart), &refday);

	/* get the maximum dimensions possibly needed */
	f = D->mjdStart - (int)(D->mjdStart);
	v = getShape("tsys", refday+f, refday+f+D->duration/86400.0,
		&no_pol, &no_band);

	if(v < 0)
	{
		return D;
	}

	in = fopen("tsys", "r");

	if(no_pol == 2)
	{
		nColumn = NELEMENTS(columns);
	}
	else
	{
		nColumn = NELEMENTS(columns) - 2;
	}
	
	n_row_bytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS format */
	if((fitsbuf = (char *)calloc(n_row_bytes, 1)) == 0)
	{
		return 0;
	}

	mjdStop = D->mjdStart + D->duration/86400.0;

	fitsWriteBinTable(out, nColumn, columns, n_row_bytes, 
		"SYSTEM_TEMPERATURE");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "NO_POL", no_pol, "");
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteEnd(out);

	freqId = 1;
	arrayId = 1;
	for(i = 0; i < 16; i++)
	{
		((unsigned int *)tant)[i] = -1;	/* set to NaN */
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
			parseTsys(line, no_pol, no_band, antenna, 
				&t, &dt, ty);

			/* discard records outside time range */
			t -= refday;
			mjd = t + (int)(D->mjdStart);
			if(mjd < D->mjdStart || mjd > mjdStop)
			{
				continue;
			}
		
			/* discard records for unused antennas */
			antId = DifxInputGetAntennaId(D, antenna) + 1;
			if(antId <= 0)
			{
				continue;
			}

			sourceId = DifxInputGetSourceId(D, mjd) + 1;
		
			p_fitsbuf = fitsbuf;
		
			/* TIME */
			bcopy((char *)&t, p_fitsbuf, sizeof(t));
			p_fitsbuf += sizeof(t);
		
			/* TIME INTERVAL */
			bcopy((char *)&dt, p_fitsbuf, sizeof(dt));
			p_fitsbuf += sizeof(dt);
		
			/* SOURCE_ID */
			bcopy((char *)&sourceId, p_fitsbuf, sizeof(sourceId));
			p_fitsbuf += sizeof(sourceId);

			/* ANTENNA */
			bcopy((char *)&antId, p_fitsbuf, sizeof(antId));
			p_fitsbuf += sizeof(antId);

			/* ARRAY */
			bcopy((char *)&arrayId, p_fitsbuf, sizeof(arrayId));
			p_fitsbuf += sizeof(arrayId);

			/* FREQ_ID */
			bcopy((char *)&freqId, p_fitsbuf, sizeof(freqId));
			p_fitsbuf += sizeof(freqId);

			for(i = 0; i < no_pol; i++)
			{
				/* TSYS */
				bcopy((char *)(ty[i]), p_fitsbuf, 
					no_band*sizeof(float));
				p_fitsbuf += no_band*sizeof(float);

				/* TANT */
				bcopy((char *)tant, p_fitsbuf, 
					no_band*sizeof(float));
				p_fitsbuf += no_band*sizeof(float);
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
