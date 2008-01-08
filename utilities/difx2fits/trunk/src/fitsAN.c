#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"

const DifxInput *DifxInput2FitsAN(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	/* define the columns in the FITS "ANTENNA" characteristics Table */
	struct FITS_antenna
	{
		double time;
		float time_int;
		char anname[8];
		int ant_no;
		int array_id;
		int freq_id;
		int no_levels;
		char poltya;
		float polaa[array_MAX_BANDS];
		float polcala[array_MAX_BANDS];
		char poltyb;
		float polab[array_MAX_BANDS];
		float polcalb[array_MAX_BANDS];
	} ac_row;

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
	int no_bands;
	int n_row_bytes, irow;
	char *fitsbuf;
	double start, stop;
	char *p_fitsbuf;
	int i, c;

	if(D == 0)
	{
		return 0;
	}

	nColumn = NELEMENTS(columns);
	no_bands = D->nIF;
	sprintf(bandFormFloat, "%1dE", no_bands);

	n_row_bytes = FitsBinTableSize(columns, NELEMENTS(columns));

	/* calloc space for storing table in FITS format */
	if ((fitsbuf = (char *)calloc (n_row_bytes, 1)) == 0)
	{
		return 0;
	}
	
	fitsWriteBinTable(out, nColumn, columns, n_row_bytes, "ANTENNA");

	arrayWriteKeys (p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteInteger(out, "NOPCAL", 0, "");
	fitsWriteString(out, "POLTYPE", "APPROX", "");
	fitsWriteEnd(out);

	start = D->mjdStart - (int)D->mjdStart;
	stop = start + D->duration/86400.0;

	ac_row.array_id = 1;
	ac_row.freq_id = 0;
	ac_row.no_levels = 1 << (D->quantBits);
	ac_row.poltya = 'R';
	ac_row.poltyb = 'L';
	ac_row.time = 0.5 * (stop + start);
	ac_row.time_int = stop - start;
	for(i=0; i<no_bands; i++)
	{
		ac_row.polaa[i] = 0.0;
		ac_row.polcala[i] = 0.0;
		ac_row.polab[i] = 0.0;
		ac_row.polcalb[i] = 0.0;
	}

	for(c = 0; c < D->nConfig; c++)
	{
		if(D->config[c].freqId < ac_row.freq_id)
		{
			continue;	/* already got this freqid */
		}
		ac_row.freq_id = D->config[c].freqId + 1;
		for(irow = 0; irow < D->nAntenna; irow++)
		{
			p_fitsbuf = fitsbuf;
			ac_row.ant_no = irow + 1;

			/* TIME */
			bcopy ((char *)&ac_row.time, p_fitsbuf, sizeof(ac_row.time));
			p_fitsbuf += sizeof(ac_row.time);

			/* TIME_INT */
			bcopy ((char *)&ac_row.time_int, p_fitsbuf, sizeof(ac_row.time_int));
			p_fitsbuf += sizeof(ac_row.time_int);

			/* ANNAME */
			strcpypad(p_fitsbuf, D->antenna[irow].name, 8);
			p_fitsbuf += 8;

			/* ANT_NO */
			bcopy ((char *)&ac_row.ant_no, p_fitsbuf, sizeof(ac_row.ant_no));
			p_fitsbuf += sizeof(ac_row.ant_no);

			/* ARRAY_ID */
			bcopy ((char *)&ac_row.array_id, p_fitsbuf, sizeof(ac_row.array_id));
			p_fitsbuf += sizeof(ac_row.array_id);

			/* FREQ_ID */
			bcopy ((char *)&ac_row.freq_id, p_fitsbuf, sizeof(ac_row.freq_id));
			p_fitsbuf += sizeof(ac_row.freq_id);

			/* NO_LEVELS */
			bcopy ((char *)&ac_row.no_levels, p_fitsbuf, sizeof(ac_row.no_levels));
			p_fitsbuf += sizeof(ac_row.no_levels);

			/* POLTYA */
			bcopy ((char *)&ac_row.poltya, p_fitsbuf, sizeof(ac_row.poltya));
			p_fitsbuf += sizeof(ac_row.poltya);

			/* POLAA */
			bcopy((char *)&ac_row.polaa[i], p_fitsbuf, no_bands*sizeof(ac_row.polaa[i]));
			p_fitsbuf += no_bands*sizeof(ac_row.polaa[i]);

			/* POLCALA */
			bcopy((char *)&ac_row.polcala[i], p_fitsbuf, no_bands*sizeof(ac_row.polcala[i]));
			p_fitsbuf += no_bands*sizeof(ac_row.polcala[i]);

			/* POLTYB */
			bcopy((char *)&ac_row.poltyb, p_fitsbuf, sizeof(ac_row.poltyb));
			p_fitsbuf += sizeof(ac_row.poltyb);

			/* POLAB */
			bcopy((char *)&ac_row.polab[i], p_fitsbuf, no_bands*sizeof(ac_row.polab[i]));
			p_fitsbuf += no_bands*sizeof(ac_row.polab[i]);

			/* POLCALB */
			bcopy((char *)&ac_row.polcalb[i], p_fitsbuf, no_bands*sizeof(ac_row.polcalb[i]));
			p_fitsbuf += no_bands*sizeof(ac_row.polcalb[i]);

#ifndef WORDS_BIGENDIAN
			FitsBinRowByteSwap(columns, NELEMENTS(columns), fitsbuf);
#endif
			fitsWriteBinRow(out, fitsbuf);
		}
	}

	/* clean up and return */
	free (fitsbuf);

	return D;
}
