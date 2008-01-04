#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"
#include "other.h"

typedef struct 
{
	double time;
	float temp, pres, dewpt, wspeed, wdir;
} WRrow;

static int parseWeather(const char *line, WRrow *wr, char *antName)
{
	int n;

	n = sscanf(line, "%s%lf%f%f%f%f%f", antName, 
		&wr->time, &wr->temp, &wr->pres, &wr->dewpt,
		&wr->wspeed, &wr->wdir);
			
	if(n != 7)
	{
		return 0;
	}

	return 1;
}

const DifxInput *DifxInput2FitsWR(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	/*  define the flag FITS table columns */
	struct fitsBinTableColumn columns[] =
	{
		{"TIME", "1D", "time of measurement", "DAYS"},
		{"TIME_INTERVAL", "1E", "time span over which data applies", "DAYS"},
		{"ANTENNA_NO", "1J", "antenna id from antennas tbl", 0},
		{"TEMPERATURE", "1E", "ambient temperature", "CENTIGRADE"},
		{"PRESSURE", "1E", "atmospheric pressure", "MILLIBARS"},
		{"DEWPOINT", "1E", "dewpoint", "CENTIGRADE"},
		{"WIND_VELOCITY", "1E", "wind velocity", "M/SEC"},
		{"WIND_DIRECTION", "1E", "wind direction", "DEGREES"},
		{"WVR_H2O", "0E", "", ""},
		{"IONOS_ELECTRON", "0E", "", ""}
	};

	WRrow wr;
	int i;
	int nColumn;
	int n_row_bytes;
	char *fitsbuf, *p_fitsbuf;
	char antName[64];
	char line[1000];
	double mjd, mjdStop;
	int antId, refday;
	double t;
	float dt;
	FILE *in;
	
	in = fopen("weather", "r");
	
	if(!in || D == 0)
	{
		return D;
	}

	nColumn = NELEMENTS(columns);
	
	n_row_bytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS format */
	if((fitsbuf = (char *)calloc(n_row_bytes, 1)) == 0)
	{
		return 0;
	}

	mjd2dayno((int)(D->mjdStart), &refday);
	mjdStop = D->mjdStart + D->duration/86400.0;

	/* calloc space for storing table in FITS format */
	if((fitsbuf = (char *)calloc(n_row_bytes, 1)) == 0)
	{
		return 0;
	}

	fitsWriteBinTable(out, nColumn, columns, n_row_bytes, "WEATHER");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteString(out, "MAPFUNC", " ", "");
	fitsWriteString(out, "WVR_TYPE", " ", "");
	fitsWriteString(out, "ION_TYPE", " ", "");
	fitsWriteEnd(out);
	
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
			/* take out * from line */
			for(i = 0; line[i]; i++)
			{
				if(line[i] == '*')
				{
					line[i] = ' ';
				}
			}
			if(parseWeather(line, &wr, antName) == 0)
			{
				continue;
			}
			
			t = wr.time - refday;
			dt = 0.0;
			
			antId = DifxInputGetAntennaId(D, antName) + 1;
			if(antId <= 0)
			{
				continue;
			}
			
			mjd = t + (int)(D->mjdStart);
			if(mjd < D->mjdStart || mjd > mjdStop)
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
			
			/* ANTENNA_NO */
			bcopy((char *)&antId, p_fitsbuf, sizeof(antId));
			p_fitsbuf += sizeof(antId);

			/* TEMPERATURE */
			bcopy((char *)&wr.temp, p_fitsbuf, sizeof(wr.temp));
			p_fitsbuf += sizeof(wr.temp);

			/* PRESSURE */
			bcopy((char *)&wr.pres, p_fitsbuf, sizeof(wr.pres));
			p_fitsbuf += sizeof(wr.pres);

			/* DEW POINT */
			bcopy((char *)&wr.dewpt, p_fitsbuf, sizeof(wr.dewpt));
			p_fitsbuf += sizeof(wr.dewpt);

			/* WIND SPEED */
			bcopy((char *)&wr.wspeed, p_fitsbuf, sizeof(wr.wspeed));
			p_fitsbuf += sizeof(wr.wspeed);

			/* WIND DIRECTION */
			bcopy((char *)&wr.wdir, p_fitsbuf, sizeof(wr.wdir));
			p_fitsbuf += sizeof(wr.wdir);

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
