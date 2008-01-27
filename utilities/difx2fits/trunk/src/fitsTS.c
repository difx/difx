#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"
#include "other.h"


/* ant D.O.Y. dur(days) nRecChan (tsys, bandName)[nRecChan] */
static int parseTsys(const char *line, char *antenna, 
	double *t, float *dt, float ty[])
{
	int p, n, i, nRecChan;
	float A;

	n = sscanf(line, "%s%lf%f%d%n", antenna, t, dt, &nRecChan, &p);
	if(n != 4)
	{
		return -1;
	}

	for(i = 0; i < nRecChan; i++)
	{
		line += p;
		n = sscanf(line, "%f%*s%n", &A, &p);
		if(n != 2)
		{
			return -2;
		}
		ty[i] = A;
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
	int n_row_bytes;
	char *fitsbuf, *p_fitsbuf;
	int refday;
	char line[1000];
	char antenna[20];
	float tyRecChan[16], ty[2][16], tant[16];
	int no_band;
	int sourceId, freqId, arrayId, antId;
	int configId = 0;	/* only support one config now */
	int i, j, no_pol=0;
	int bandId, polId;
	int nRecChan;
	int v;
	double f;
	double t, mjd, mjdStop;
	float dt;
	FILE *in;
	
	no_band = D->nIF;
	no_pol = D->nPol;

	sprintf(bandFormFloat, "%dE", no_band);

	if(D == 0)
	{
		return D;
	}

	mjd2dayno((int)(D->mjdStart), &refday);

	/* get the maximum dimensions possibly needed */
	f = D->mjdStart - (int)(D->mjdStart);

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
			nRecChan = parseTsys(line, antenna, &t, &dt, tyRecChan);

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

			for(j = 0; j < 2; j++)
			{
				for(i = 0; i < 16; i++)
				{
					ty[j][i] = 0;
				}
			}

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
			}
			if(v < 0)
			{
				continue;
			}
		
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
