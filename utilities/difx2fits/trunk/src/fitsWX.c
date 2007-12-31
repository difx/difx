#include <math.h>
#include <stdlib.h>
#include <sys/types.h>
#include <strings.h>
#include "difx2fits.h"
#include "byteorder.h"
#include "other.h"


const DifxInput *DifxInput2FitsPC(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out)
{
	/*  define the flag FITS table columns */
	struct fitsBinTableColumn columns[] =
	{
	};

	int nColumn;
	int n_row_bytes, irow;
	char *fitsbuf, *p_fitsbuf;
	int swap;
	char line[1000];
	int no_band;
	FILE *in;
	
	no_band = p_fits_keys->no_band;
	
	in = fopen("weather", "r");
	
	if(!in || D == 0)
	{
		return D;
	}

	swap = (byteorder() == BO_LITTLE_ENDIAN);

	nColumn = NELEMENTS(columns);
	
	n_row_bytes = FitsBinTableSize(columns, nColumn);

	/* malloc space for storing table in FITS format */
	if ((fitsbuf = (char *)malloc (n_row_bytes)) == 0)
	{
		return 0;
	}

	fitsWriteBinTable(out, nColumn, columns, n_row_bytes, "WEATHER");

	arrayWriteKeys (p_fits_keys, out);
	fitsWriteInteger(out, "TABREV", 1, "");
	fitsWriteEnd(out);

	
	for(;;)
	{
		fgets(line, 999, in);
		if(feof(in))
		{
			return;
		}
			
		/* ignore possible comment lines */
		if(line[0] == '#')
		{
			continue;
		}
		else 
		{
			if(swap)
			{
				FitsBinRowByteSwap(columns, nColumn, &fitsbuf);
			}
			fitsWriteBinRow(out, fitsbuf);
		}
	}

	/* close the file, free memory, and return */
	fclose(in);
	free(fitsbuf);

	return D;
}
