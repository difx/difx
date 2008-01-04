#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"

const DifxInput *DifxInput2FitsSO(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	/*  structure that defines the columns in the FITS "Source Table" */
	struct FITS_source
	{
		int             src_id;
		char            source[16];
		int             qual;
		char            calcode[4];
		int             freq_id;
		float           iflux;
		float           qflux;
		float           uflux;
		float           vflux;
		float           alpha;
		double          freqoff[array_MAX_BANDS];
		double          raepo;
		double          decepo;
		double          epoch;
		double          raapp;
		double          decapp;
		double          sysvel[array_MAX_BANDS];
		char            veltyp[8];
		char            veldef[8];
		double          restfreq[array_MAX_BANDS];
		double          pmra;
		double          pmdec;
		float           parallax;
	} src_row;

	char bandFormDouble[4];
	char bandFormFloat[4];
	struct fitsBinTableColumn columns[] =
	{
		{"ID_NO.", "1J", "source id number", 0},
		{"SOURCE", "16A", "source name", 0},
		{"QUAL", "1J", "source qualifier", 0},
		{"CALCODE", "4A", "calibrator code", 0},
		{"FREQID", "1J", "freq id number in frequency tbl", 0},
		{"IFLUX", bandFormFloat, "ipol flux density at ref freq", "JY"},
		{"QFLUX", bandFormFloat, "qpol flux density at ref freq", "JY"},
		{"UFLUX", bandFormFloat, "upol flux density at ref freq", "JY"},
		{"VFLUX", bandFormFloat, "vpol flux density at ref freq", "JY"},
		{"ALPHA", bandFormFloat, "spectral index", 0},
		{"FREQOFF", bandFormDouble, "freq. offset from ref freq.","HZ"},
		{"RAEPO", "1D", "Right Ascension at EPOCH", "DEGREES"},
		{"DECEPO", "1D", "Declination at EPOCH", "DEGREES"},
		{"EPOCH", "1D", "epoch 1950.0B or J2000", "YEARS"},
		{"RAAPP", "1D", "apparent RA at 0 IAT ref day", "DEGREES"},
		{"DECAPP", "1D", "apparent Dec at 0 IAT ref day", "DEGREES"},
		{"SYSVEL", bandFormDouble, "systemic velocity at ref pixel", 
			"M/SEC"},
		{"VELTYP", "8A", "velocity type", 0},
		{"VELDEF", "8A", "velocity def: radio, optical", 0},
		{"RESTFREQ", bandFormDouble, "line rest frequency", "HZ"},
		{"PMRA", "1D", "proper motion in RA", "DEG/DAY"},
		{"PMDEC", "1D", "proper motion in Dec", "DEG/DAY"},
		{"PARALLAX", "1E", "parallax of source", "ARCSEC"}
	};

	int nColumn;
	int n_row_bytes;
	int no_bands;
	int irow, i;
	char *fitsbuf;
	
	if(D == 0)
	{
		return 0;
	}

	nColumn = NELEMENTS(columns);
	no_bands = D->config[0].nIF;

	sprintf(bandFormFloat, "%1dE", no_bands);
	sprintf(bandFormDouble, "%1dD", no_bands); 

	n_row_bytes = FitsBinTableSize(columns, NELEMENTS(columns));
	
	fitsWriteBinTable(out, nColumn, columns, n_row_bytes, "SOURCE");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteEnd(out);
	
	src_row.iflux = src_row.qflux = src_row.uflux = 0.0;
	src_row.vflux = src_row.alpha = 0.0;

	src_row.parallax = 0.0;
	strcpypad(src_row.veltyp, "GEOCENTR", 8);
	strcpypad(src_row.veldef, "OPTICAL", 8);
	src_row.pmra = src_row.pmdec = 0.0;
	src_row.epoch = 2000.0;

	if((fitsbuf = (char *)calloc(n_row_bytes, 1)) == 0)
	{
		return 0;
	}

	for (irow = 0; irow < D->nSource; irow++)
	{
		char *p_fitsbuf = fitsbuf;

		src_row.src_id = irow + 1;
		src_row.qual = D->source[irow].qual;
		strcpypad(src_row.calcode, D->source[irow].calcode, 4);
		src_row.raepo = D->source[irow].ra * 180.0 / M_PI;
		src_row.decepo = D->source[irow].dec * 180.0 / M_PI;
		src_row.raapp = src_row.raepo;
		src_row.decapp = src_row.decepo;
		src_row.freq_id = D->source[irow].configId + 1;

		/* load the SO table buffer */

		/* src_id */
		bcopy ((char *) &(src_row.src_id), p_fitsbuf, 
			sizeof (src_row.src_id));
		p_fitsbuf += sizeof (src_row.src_id);

		/* source */
		strcpypad(p_fitsbuf, D->source[irow].name, 16);
		p_fitsbuf += 16;

		/* qual */
		bcopy ((char *) &(src_row.qual), p_fitsbuf, sizeof (src_row.qual));
		p_fitsbuf += sizeof (src_row.qual);

		/* calcode */
		strcpypad(p_fitsbuf, src_row.calcode, sizeof (src_row.calcode));
		p_fitsbuf += sizeof (src_row.calcode);

		/* freq_id */
		bcopy ((char *) &(src_row.freq_id), p_fitsbuf, sizeof (src_row.freq_id));
		p_fitsbuf += sizeof (src_row.freq_id);

		for (i = 0; i < no_bands; i++)
		{
			/* iflux */
			bcopy ((char *) &(src_row.iflux), p_fitsbuf,
			sizeof (src_row.iflux));
			p_fitsbuf += sizeof (src_row.iflux);

			/* qflux */
			bcopy ((char *) &(src_row.qflux), p_fitsbuf,
			sizeof (src_row.qflux));
			p_fitsbuf += sizeof (src_row.qflux);

			/* uflux */
			bcopy ((char *) &(src_row.uflux), p_fitsbuf,
			sizeof (src_row.uflux));
			p_fitsbuf += sizeof (src_row.uflux);

			/* vflux */
			bcopy ((char *) &(src_row.vflux), p_fitsbuf,
			sizeof (src_row.vflux));
			p_fitsbuf += sizeof (src_row.vflux);

			/* alpha */
			bcopy ((char *) &(src_row.alpha), p_fitsbuf,
			sizeof (src_row.alpha));
			p_fitsbuf += sizeof (src_row.alpha);

			/* freqoff */
			src_row.freqoff[i] = 0.0;
			bcopy ((char *) &(src_row.freqoff[i]), p_fitsbuf,
			sizeof (src_row.freqoff[i]));
			p_fitsbuf += sizeof (src_row.freqoff[i]);
		}

		/* raepo */
		bcopy ((char *) &(src_row.raepo), p_fitsbuf, 
			sizeof (src_row.raepo));
		p_fitsbuf += sizeof (src_row.raepo);

		/* decepo */
		bcopy ((char *) &(src_row.decepo), p_fitsbuf, 
			sizeof (src_row.decepo));
		p_fitsbuf += sizeof (src_row.decepo);

		/* epoch */
		bcopy ((char *) &(src_row.epoch), p_fitsbuf, 
			sizeof (src_row.epoch));
		p_fitsbuf += sizeof (src_row.epoch);

		/* raapp */
		bcopy ((char *) &(src_row.raapp), p_fitsbuf, 
			sizeof (src_row.raapp));
		p_fitsbuf += sizeof (src_row.raapp);

		/* decapp */
		bcopy ((char *) &(src_row.decapp), p_fitsbuf, 
			sizeof (src_row.decapp));
		p_fitsbuf += sizeof (src_row.decapp);

		/* sysvel */
		for (i=0; i<no_bands; i++, p_fitsbuf+=sizeof(src_row.sysvel[i]))
		{
			src_row.sysvel[i] = 0.0;
			bcopy ((char *) &(src_row.sysvel[i]), p_fitsbuf,
				sizeof (src_row.sysvel[i]));
		}

		/* veltyp */
		strcpypad(p_fitsbuf, src_row.veltyp, sizeof (src_row.veltyp));
		p_fitsbuf += sizeof (src_row.veltyp);

		/* veldef */
		strcpypad(p_fitsbuf, src_row.veldef, sizeof (src_row.veldef));
		p_fitsbuf += sizeof (src_row.veldef);

		/* restfreq */
		for (i = 0; i < no_bands; i++, p_fitsbuf += 
			sizeof (src_row.restfreq[i]))
		{
			src_row.restfreq[i] = p_fits_keys->ref_freq;
			bcopy ((char *) &(src_row.restfreq[i]), p_fitsbuf,
			sizeof (src_row.restfreq[i]));
		}

		/* pmra */
		bcopy ((char *) &(src_row.pmra), p_fitsbuf, 
			sizeof (src_row.pmra));
		p_fitsbuf += sizeof (src_row.pmra);

		/* pmdec */
		bcopy ((char *) &(src_row.pmdec), p_fitsbuf, 
			sizeof (src_row.pmdec));
		p_fitsbuf += sizeof (src_row.pmdec);

		/* parallax */
		bcopy ((char *) &(src_row.parallax), p_fitsbuf,
		sizeof (src_row.parallax));
		p_fitsbuf += sizeof (src_row.parallax);

#ifndef WORDS_BIGENDIAN
		FitsBinRowByteSwap(columns, NELEMENTS(columns), fitsbuf);
#endif
		fitsWriteBinRow (out, fitsbuf);
	}
	free (fitsbuf);
	

	return D;
}	
