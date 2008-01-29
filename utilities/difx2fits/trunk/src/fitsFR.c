#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"

const DifxInput *DifxInput2FitsFR(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	char bandFormDouble[4];
	char bandFormFloat[4];
	char bandFormInt[4];

	struct fitsBinTableColumn columns[] =
	{
		{"FREQID", "1J", "FREQID number in uv data", 0},
		{"BANDFREQ", bandFormDouble, "frequency offset", "HZ"},
		{"CH_WIDTH", bandFormFloat, "spectral channel bandwidth", "HZ"},
		{"TOTAL_BANDWIDTH", bandFormFloat, "total bw of a BAND", "HZ"},
		{"SIDEBAND", bandFormInt, "sideband of each BAND", 0},
		{"BB_CHAN", bandFormInt, "baseband channel number (1-16)", 0}
	};

	int nRowBytes;
	int nColumn;
	char *fitsbuf, *p_fitsbuf;
	double bandFreq[array_MAX_BANDS];
	float chanBW[array_MAX_BANDS];
	float bandBW[array_MAX_BANDS];
	int32_t netSide[array_MAX_BANDS];
	int32_t bbChan[array_MAX_BANDS];
	int row;
	int nBand;
	int i;
	int freqId;


	if(D == 0)
	{
		return 0;
	}

	nBand = p_fits_keys->no_band;

	sprintf(bandFormDouble, "%1dD", nBand);  
	sprintf(bandFormFloat, "%1dE", nBand);  
	sprintf(bandFormInt, "%1dJ", nBand);  

	nColumn = NELEMENTS(columns);

	/* determine size of FITS record to be written */
	nRowBytes = FitsBinTableSize(columns, nColumn);

	/* calloc space for storing table in FITS format */
	if((fitsbuf = (char *)calloc(nRowBytes, 1)) == 0)
	{
		return 0;
	}
	
	/* spew out the table header */
	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "FREQUENCY");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 2, "");
	fitsWriteEnd(out);

	freqId = 0;

	for(row = 0; row < D->nConfig; row++)
	{
		/* only write one row per unique frequency ID */
		if(D->config[row].freqId < freqId)
		{
			continue;
		}
		freqId = D->config[row].freqId + 1;

		for(i = 0; i < nBand; i++)
		{
			bandFreq[i] = (D->config[row].IF[i].freq -
				D->refFreq) * 1.0e6;
			chanBW[i] = (D->config[row].IF[i].bw /
				D->nOutChan) * 1.0e6;
			bandBW[i] = D->config[row].IF[i].bw * 1.0e6;
			netSide[i] = (D->config[row].IF[i].sideband 
				== 'U' ? 1 : -1);
			bbChan[i] = 0;	/* vistigial */
		}
		
		/* pointer to the buffer for FITS records */
		p_fitsbuf = fitsbuf;

		FITS_WRITE_ITEM (freqId, p_fitsbuf);          /* FREQ_ID */
		FITS_WRITE_ARRAY(bandFreq, p_fitsbuf, nBand); /* BANDFREQ */
		FITS_WRITE_ARRAY(chanBW, p_fitsbuf, nBand);   /* CH_WIDTH */
		FITS_WRITE_ARRAY(bandBW, p_fitsbuf, nBand);   /* BANDWIDTH */
		FITS_WRITE_ARRAY(netSide, p_fitsbuf, nBand);  /* SIDEBAND */
		FITS_WRITE_ARRAY(bbChan, p_fitsbuf, nBand);   /* BB_CHAN */

#ifndef WORDS_BIGENDIAN
		FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
		fitsWriteBinRow(out, fitsbuf);
	}

	/* free buffer and return */
	free(fitsbuf);

	return D;
}
