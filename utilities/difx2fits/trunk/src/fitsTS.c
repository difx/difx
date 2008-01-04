#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"
#include "other.h"

static int parseTsys(const char *line, int no_band, char *antenna, 
	double *t, float *dt, float *ty1, float *ty2)
{
	int p, n, i;

	n = sscanf(line, "%s%lf%f%n", antenna, t, dt, &p);
	if(n != 3)
	{
		return 0;
	}
	for(i = 0; i < no_band; i++)
	{
		line += p;
		n = sscanf(line, "%f%n", ty1+i, &p);
		if(n != 1)
		{
			return 0;
		}
	}
	for(i = 0; i < no_band; i++)
	{
		line += p;
		n = sscanf(line, "%f%n", ty2+i, &p);
		if(n != 1)
		{
			return 1;
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
	float ty1[16], ty2[16], tant[16];
	int no_band;
	int sourceId, freqId, arrayId, antId;
	int i, np, nPol=0;
	double t, mjd, mjdStop;
	float dt;
	FILE *in;
	
	no_band = p_fits_keys->no_band;
	sprintf(bandFormFloat, "%dE", no_band);

	in = fopen("tsys", "r");
	
	if(!in || D == 0)
	{
		return D;
	}

	/* learn number of polarizations */
	while(nPol <= 0)
	{
		fgets(line, 999, in);
		if(feof(in))
		{
			fclose(in);
			return D;
		}
		if(line[0] == '#')
		{
			continue;
		}

		nPol = parseTsys(line, no_band, antenna, &t, &dt, ty1, ty2);
	}
	fseek(in, 0, SEEK_SET);

	if(nPol == 2)
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

	mjd2dayno((int)(D->mjdStart), &refday);
	mjdStop = D->mjdStart + D->duration/86400.0;

	fitsWriteBinTable(out, nColumn, columns, n_row_bytes, 
		"SYSTEM_TEMPERATURE");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "NO_POL", nPol, "");
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteEnd(out);

	freqId = 1;
	arrayId = 0;
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
			np = parseTsys(line, no_band, antenna, 
				&t, &dt, ty1, ty2);

			/* discard records outside time range */
			t -= refday;
			mjd = t + (int)(D->mjdStart);
			if(mjd < D->mjdStart || mjd > mjdStop)
			{
				continue;
			}
		
			if(np != nPol)
			{
				fprintf(stderr, "TSYS Pol mismatch %d != %d\n", 
					np, nPol);
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

			/* ARRAY */
			bcopy((char *)&arrayId, p_fitsbuf, sizeof(arrayId));
			p_fitsbuf += sizeof(arrayId);

			/* ANTENNA */
			bcopy((char *)&antId, p_fitsbuf, sizeof(antId));
			p_fitsbuf += sizeof(antId);

			/* FREQ_ID */
			bcopy((char *)&freqId, p_fitsbuf, sizeof(freqId));
			p_fitsbuf += sizeof(freqId);

			/* TSYS_1 */
			bcopy((char *)ty1, p_fitsbuf, no_band*sizeof(float));
			p_fitsbuf += no_band*sizeof(float);

			/* TANT_1 */
			bcopy((char *)tant, p_fitsbuf, no_band*sizeof(float));
			p_fitsbuf += no_band*sizeof(float);

			if(np > 1)
			{
				/* TSYS_2 */
				bcopy((char *)ty2, p_fitsbuf, 
					no_band*sizeof(float));
				p_fitsbuf += no_band*sizeof(float);

				/* TANT_2 */
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
