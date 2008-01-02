#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "difx2fits.h"
#include "byteorder.h"

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
		{"ANNAME", "8A", "station name"},
		{"ANTENNA_NO", "1J", "antenna number"},
		{"ARRAY", "1J", "array id number"},
		{"FREQID", "1J", "frequency id number"},
		{"NO_LEVELS", "1J", "number of digitizer levels"},
		{"POLTYA", "1A", "feed A poln. code"},
		{"POLAA", bandFormFloat, "feed A position angle", "DEGREES"},
		{"POLCALA", bandFormFloat, "feed A poln. cal. parameter"},
		{"POLTYB", "1A", "feed B poln. code"},
		{"POLAB", bandFormFloat, "feed B position angle", "DEGREES"},
		{"POLCALB", bandFormFloat, "feed B poln. cal. parameter"}
	};

	int nColumn;
	int no_bands;
	int n_row_bytes, irow;
	char *fitsbuf;
	double start, stop;
	int swap;

	if(D == 0)
	{
		return 0;
	}

	swap = (byteorder() == BO_LITTLE_ENDIAN);

	nColumn = NELEMENTS(columns);
	no_bands = D->config[0].nIF;
	sprintf(bandFormFloat,  "%1dE", no_bands);

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
	ac_row.freq_id = 1;	/* Why set to fqid 1? */
	ac_row.no_levels = 1 << (D->config[0].quantBits);
	ac_row.poltya = 'R';
	ac_row.poltyb = 'L';
	ac_row.time = 0.5 * (stop + start);
	ac_row.time_int = stop - start;

	for (irow = 0; irow < D->nAntenna; irow++)
	{
		char *p_fitsbuf = fitsbuf;
		int i;

		/*
		* Load the AN table buffer.
		* Order must agree with the FITS header.
		*/
		ac_row.ant_no = irow + 1;

		/* time */
		bcopy ((char *)&ac_row.time, p_fitsbuf, sizeof (ac_row.time));
		p_fitsbuf += sizeof (ac_row.time);

		/* time_int */
		bcopy ((char *)&ac_row.time_int, p_fitsbuf, 
			sizeof (ac_row.time_int));
		p_fitsbuf += sizeof (ac_row.time_int);

		/* anname */
		strcpypad(p_fitsbuf, D->antenna[irow].name, 8);
		p_fitsbuf += 8;

		/* ant_no */
		bcopy ((char *)&ac_row.ant_no, p_fitsbuf, 
			sizeof (ac_row.ant_no));
		p_fitsbuf += sizeof (ac_row.ant_no);

		/* array_id */
		bcopy ((char *)&ac_row.array_id, p_fitsbuf, 
			sizeof (ac_row.array_id));
		p_fitsbuf += sizeof (ac_row.array_id);

		/* freq_id */
		bcopy ((char *)&ac_row.freq_id, p_fitsbuf, 
			sizeof (ac_row.freq_id));
		p_fitsbuf += sizeof (ac_row.freq_id);

		/* no_levels */
		bcopy ((char *)&ac_row.no_levels, p_fitsbuf, 
			sizeof (ac_row.no_levels));
		p_fitsbuf += sizeof (ac_row.no_levels);

		/* poltya */
		bcopy ((char *)&ac_row.poltya, p_fitsbuf, 
			sizeof (ac_row.poltya));
		p_fitsbuf += sizeof (ac_row.poltya);

		/* polaa */
		for(i=0; i<no_bands; i++, p_fitsbuf += sizeof(ac_row.polaa[i]))
		{
			ac_row.polaa[i] = 0.0;
			bcopy ((char *)&ac_row.polaa[i], p_fitsbuf,
			sizeof (ac_row.polaa[i]));
		}

		/* polcala */
		for (i = 0; i < no_bands; i++, p_fitsbuf += sizeof (ac_row.polcala[i]))
		{
			ac_row.polcala[i] = 0.0;
			bcopy ((char *)&ac_row.polcala[i], p_fitsbuf,
			sizeof (ac_row.polcala[i]));
		}

		/* poltyb */
		bcopy ((char *)&ac_row.poltyb, p_fitsbuf, sizeof (ac_row.poltyb));
		p_fitsbuf += sizeof (ac_row.poltyb);

		/* polab */
		for (i = 0; i < no_bands; i++, p_fitsbuf += sizeof (ac_row.polab[i]))
		{
			ac_row.polab[i] = 0.0;
			bcopy ((char *)&ac_row.polab[i], p_fitsbuf,
			sizeof (ac_row.polab[i]));
		}

		/* polcalb */
		for (i = 0; i < no_bands; i++, p_fitsbuf += sizeof (ac_row.polcalb[i]))
		{
			ac_row.polcalb[i] = 0.0;
			bcopy ((char *)&ac_row.polcalb[i], p_fitsbuf,
			sizeof (ac_row.polcalb[i]));
		}

		if(swap)
		{
			FitsBinRowByteSwap(columns, NELEMENTS(columns),
				fitsbuf);
		}
		fitsWriteBinRow(out, fitsbuf);
	}

	/* clean up and return */
	free (fitsbuf);

	return D;
}
