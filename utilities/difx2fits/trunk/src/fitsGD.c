#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "config.h"
#include "difx2fits.h"

const DifxInput *DifxInput2FitsGD(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	struct fitsBinTableColumn columns[] =
	{
		{"FIXME",   "16A", "fixme", 0}
	};

	int nColumn;
	int nRowBytes;
	char *fitsbuf;
	char *p_fitsbuf;
	int p;
	
	if(D == 0)
	{
		return 0;
	}

	if(D->nPulsar == 0 || !D->pulsar)
	{
		return D;
	}

	printf("WARNING -- Pulsar table not ready yet\n");

	nColumn = NELEMENTS(columns);
	nRowBytes = FitsBinTableSize(columns, nColumn);

	fitsWriteBinTable(out, nColumn, columns, nRowBytes, "GATEDUTY");
	arrayWriteKeys(p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteEnd(out);

	fitsbuf = (char *)calloc(nRowBytes, 1);
	if(fitsbuf == 0)
	{
		return 0;
	}
	
#if 0
	for(p = 0; p < D->nPulsar; p++)
	{

		testFitsBufBytes(p_fitsbuf - fitsbuf, nRowBytes, "GD");

#ifndef WORDS_BIGENDIAN
		FitsBinRowByteSwap(columns, nColumn, fitsbuf);
#endif
		fitsWriteBinRow(out, fitsbuf);
	}

#endif
	free(fitsbuf);

	return D;
}	
