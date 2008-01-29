#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"

const DifxInput *DifxInput2FitsAN(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	/*  define the antenna characteristic FITS table columns */
	char bandFormFloat[4];
	struct fitsBinTableColumn columns[] =
	{
		{"TIME", "1D", "time of center of interval", "DAYS"},
		{"TIME_INTERVAL", "1E", "row interval", "DAYS"},
		{"ANNAME", "8A", "station name", 0},
		{"ANTENNA_NO", "1J", "antenna number", 0},
		{"ARRAY", "1J", "array id number", 0},
		{"FREQID", "1J", "frequency id number", 0},
		{"NO_LEVELS", "1J", "number of digitizer levels", 0},
		{"POLTYA", "1A", "feed A poln. code", 0},
		{"POLAA", bandFormFloat, "feed A position angle", "DEGREES"},
		{"POLCALA", bandFormFloat, "feed A poln. cal. parameter", 0},
		{"POLTYB", "1A", "feed B poln. code", 0},
		{"POLAB", bandFormFloat, "feed B position angle", "DEGREES"},
		{"POLCALB", bandFormFloat, "feed B poln. cal. parameter", 0}
	};

	int nColumn;
	int nBand;
	int nRowBytes;
	char *fitsbuf;
	double start, stop;
	char *p_fitsbuf;
	int a, b, c;
	char antName[8];
	double time;
	float timeInt;
	int antId, arrayId, freqId, nLevel;
	char polTypeA;
	float polAA[array_MAX_BANDS];
	float polCalA[array_MAX_BANDS];
	char polTypeB;
	float polAB[array_MAX_BANDS];
	float polCalB[array_MAX_BANDS];

	if(D == 0)
	{
		return 0;
	}

	nColumn = NELEMENTS(columns);
	nBand = D->nIF;
	sprintf(bandFormFloat, "%1dE", nBand);

	nRowBytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS format */
	fitsbuf = (char *)calloc (nRowBytes, 1);
	if(fitsbuf == 0)
	{
		return 0;
	}
	
	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "ANTENNA");

	arrayWriteKeys (p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteInteger(out, "NOPCAL", 0, "");
	fitsWriteString(out, "POLTYPE", "APPROX", "");
	fitsWriteEnd(out);

	start = D->mjdStart - (int)D->mjdStart;
	stop = start + D->duration/86400.0;

	arrayId = 1;
	freqId = 0;
	nLevel = 1 << (D->quantBits);
	polTypeA = 'R';
	polTypeB = 'L';
	time = 0.5 * (stop + start);
	timeInt = stop - start;
	for(b = 0; b < nBand; b++)
	{
		polAA[b] = 0.0;
		polCalA[b] = 0.0;
		polAB[b] = 0.0;
		polCalB[b] = 0.0;
	}

	for(c = 0; c < D->nConfig; c++)
	{
		if(D->config[c].freqId < freqId)
		{
			continue;	/* already got this freqid */
		}
		freqId = D->config[c].freqId + 1; /* FITS fqId starts at 1 */
		for(a = 0; a < D->nAntenna; a++)
		{
			p_fitsbuf = fitsbuf;
			antId = a + 1;	  /* FITS antId starts at 1 */
			strcpypad(antName, D->antenna[a].name, 8);

			FITS_WRITE_ITEM (time, p_fitsbuf);
			FITS_WRITE_ITEM (timeInt, p_fitsbuf);
			FITS_WRITE_ARRAY(antName, p_fitsbuf, 8);
			FITS_WRITE_ITEM (antId, p_fitsbuf);
			FITS_WRITE_ITEM (arrayId, p_fitsbuf);
			FITS_WRITE_ITEM (freqId, p_fitsbuf);
			FITS_WRITE_ITEM (nLevel, p_fitsbuf);
			FITS_WRITE_ITEM (polTypeA, p_fitsbuf);
			FITS_WRITE_ARRAY(polAA, p_fitsbuf, nBand);
			FITS_WRITE_ARRAY(polCalA, p_fitsbuf, nBand);
			FITS_WRITE_ITEM (polTypeB, p_fitsbuf);
			FITS_WRITE_ARRAY(polAB, p_fitsbuf, nBand);
			FITS_WRITE_ARRAY(polCalB, p_fitsbuf, nBand);

#ifndef WORDS_BIGENDIAN
			FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
			fitsWriteBinRow(out, fitsbuf);
		}
	}

	/* clean up and return */
	free (fitsbuf);

	return D;
}
