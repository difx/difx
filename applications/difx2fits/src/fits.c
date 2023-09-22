/* Stolen from arrayFits.c */

#include "fits.h"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>

/* forward declarations */
static int fitsWriteZeroes (struct fitsPrivate *);
static int fitsWriteEndTable (struct fitsPrivate *);
static int fitsItemSize(const char *);

/*******************************************************************************
*/
int fitsReadOpen		/* open FITS file for reading only */
    (
    struct fitsPrivate *pFile,	/* pointer to FITS file control struct */
    const char filename[]		/* file to open */
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 */
{
    strcpy (pFile->filename, filename);

    if ((pFile->fp = fopen (pFile->filename, "r")) == 0)
	return -1;
	
    pFile->bytes_written = 0LL;
    pFile->rows_written = 0; 

    return 0;
}

/*******************************************************************************
*/
int fitsWriteOpen		/* create FITS file and open it for writing */
    (
    struct fitsPrivate *pFile,	/* pointer to FITS file control struct */
    const char *filename		/* file to oepn */
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 */
{
    /* open stream */
    if ((pFile->fp = fopen (filename, "w")) == 0)
        { 
	printf("fitsWriteOpen: fopen() error");  
        return -1;
        }

    strcpy (pFile->filename, filename);
    pFile->bytes_written = 0LL;
    pFile->rows_written = 0; 

    return 0;
}

/*******************************************************************************
*/
int fitsWriteClose		/* close FITS file */
    (
    struct fitsPrivate *pFile	/* pointer to FITS file control struct */
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 *
 * Zero pad the file
 */
{
    int status;

    /* zero pad FITS file to next 2880 byte boundary */
    if (fitsWriteZeroes (pFile) == -1)
        return -1;

    status = fclose(pFile->fp);

    return (status);
}

/*******************************************************************************
*/
int fitsWriteTable
    (
    struct fitsPrivate *pFile,	/* pointer to FITS file control struct */ 
    int num_col,		/* size of columns[] */
    struct fitsTableColumn columns[],
    int row_bytes,		/* byte count for 
				   future fitsWriteBinRow() calls */
    const char extname[]
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 */
{
    char keyword[9];
    int i;
    static char separator[] =
	"------------------------------------------------";

    pFile->row_bytes = row_bytes;

    /* finish prior extension/matrix */
    if (fitsWriteZeroes (pFile) == -1)
	return -1;

    if (fitsWriteString (pFile, "XTENSION", "TABLE",
			 "FITS ASCII Table Extension") == -1)
	return -1;

    if (fitsWriteInteger (pFile, "BITPIX", 8, "") == -1)
	return -1;

    if (fitsWriteInteger (pFile, "NAXIS", 2, "") == -1)
	return -1;

    if (fitsWriteInteger (pFile, "NAXIS1", row_bytes, "") == -1)
	return -1;

    fgetpos(pFile->fp, &pFile->naxis2_offset);

    if (fitsWriteInteger (pFile, "NAXIS2", 0,
			  "[number of rows is initially zero]") == -1)
	return -1;

    if (fitsWriteInteger (pFile, "PCOUNT", 0, "") == -1)
	return -1;

    if (fitsWriteInteger (pFile, "GCOUNT", 1, "") == -1)
	return -1;

    if (fitsWriteString (pFile, "EXTNAME", extname, "") == -1)
	return -1;

    if (fitsWriteInteger (pFile, "TFIELDS", num_col, "") == -1)
	return -1;

    if (fitsWriteComment (pFile, "", separator) == -1)
	return -1;

    /* write contents of given columns[] */
    for (i = 0; i < num_col; i++)
	{
	if (i > 0 && fitsWriteComment (pFile, "", "") == -1)
	    return -1;

	sprintf (keyword, "TTYPE%-3d", i + 1);
	if (fitsWriteString (pFile, keyword,
			     columns[i].p_name, columns[i].p_comment) == -1)
	    return -1;

	sprintf (keyword, "TFORM%-3d", i + 1);
	if (fitsWriteString (pFile, keyword, 
			     columns[i].p_format, "") == -1)
	    return -1;

	if (columns[i].p_units != (char *)NULL)
	    {
	    sprintf (keyword, "TUNIT%-3d", i + 1);
	    if (fitsWriteString (pFile, keyword, 
				 columns[i].p_units, "") == -1)
		return -1;
	    }
	}

    if (fitsWriteComment (pFile, "", separator) == -1)
	return -1;

    pFile->rows_written = 0;
    return -1;
}

/*******************************************************************************
*/
int fitsWriteBinTable
    (
    struct fitsPrivate *pFile,	/* pointer to FITS file control struct */
    int num_col,		/* size of columns[] */
    const struct fitsBinTableColumn columns[],
    int row_bytes,		/* byte count for 
				   future fitsWriteBinRow() calls */
    const char extname[]
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 */
{
    int i;
    char keyword[9];

    if (row_bytes > 0)
	{
	pFile->row_bytes = row_bytes;
	if(row_bytes != fitsRowSize(columns, num_col))
	    {
	    printf(" %s FITS header size %d different from declared %d\n",
	       extname, fitsRowSize(columns, num_col), row_bytes);
	    }
	}
    else pFile->row_bytes = fitsRowSize(columns, num_col);

    /* finish prior extension/matrix */
    if (fitsWriteZeroes (pFile) == -1)
	return -1;

    if (fitsWriteString (pFile, "XTENSION", "BINTABLE",
		     "FITS Binary Table Extension") == -1)
	return -1;

    if (fitsWriteInteger (pFile, "BITPIX", 8, "") == -1)
	return -1;

    if (fitsWriteInteger (pFile, "NAXIS", 2, "") == -1)
	return -1;

    if (fitsWriteInteger (pFile, "NAXIS1", row_bytes, "") == -1)
	return -1;

    fgetpos(pFile->fp, &pFile->naxis2_offset);

    if (fitsWriteInteger (pFile, "NAXIS2", 0, 
			  "[number of rows is initially zero]") == -1)
	return -1;

    if (fitsWriteInteger (pFile, "PCOUNT", 0, "") == -1)
	return -1;

    if (fitsWriteInteger (pFile, "GCOUNT", 1, "") == -1)
	return -1;

    if (fitsWriteInteger (pFile, "TFIELDS", num_col, "") == -1)
	return -1;

    if (fitsWriteString (pFile, "EXTNAME", extname, "") == -1)
	return -1;

    if (fitsWriteInteger (pFile, "EXTVER", 1, "") == -1)
	return -1;

    /* write contents of given columns[] */
    for (i = 0; i < num_col; i++)
	{
	sprintf (keyword, "TTYPE%-3d", i + 1);
	if (fitsWriteString (pFile, keyword,
			     columns[i].p_name, columns[i].p_comment) == -1)
	    return -1;

	sprintf (keyword, "TFORM%-3d", i + 1);
	if (fitsWriteString (pFile, keyword,
			     columns[i].p_repeat_type, "") == -1)
	    return -1;

	if (columns[i].p_units != (char *)NULL)
	    {
	    sprintf (keyword, "TUNIT%-3d", i + 1);
	    if (fitsWriteString (pFile, keyword, 
				 columns[i].p_units, "") == -1)
		return -1;
	    }
	}

    pFile->rows_written = 0;
    return 0;
}

/*******************************************************************************
*/
int fitsWriteBinData
    (
    struct fitsPrivate *pFile,	/* pointer to FITS file control struct */
    const char  *p_data,
    int    dataSize
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 *
 * Writes data to the binary portion of the FITS table.  This function
 * writes the individual data fields (as opposed to the whole row).
 *
 * NOTE:  It is up to the caller to bump rows_written.
 */
{
    if (fitsWriteData (pFile, dataSize, p_data) == -1)
	return -1; 

    return 0;
}

/*******************************************************************************
*/
int fitsWriteBinRow
    (
    struct fitsPrivate *pFile,	/* pointer to FITS file control struct */
    const char *row
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 */
{
    if (fitsWriteData (pFile, pFile->row_bytes, row) == -1)
	return -1; 

    pFile->rows_written++;
    return 0;
}

/*******************************************************************************
*/
int fitsWriteComment
    (
    struct fitsPrivate *pFile,	/* pointer to FITS file control struct */
    const char key[],
    const char comment[]
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 */
{
    char card[81];

    /* left justify given strings in output fields, blank fill to right */
    sprintf (card, "%-8s%-72s", key, comment);
    return (fitsWriteData (pFile, 80, card));
}

/*******************************************************************************
*/
int fitsWriteFloat
    (
    struct fitsPrivate *pFile,	/* pointer to FITS file control struct */
    const char key[],
    double value,
    const char comment[]
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 */
{
    char card[81];

    sprintf (card, "%-8s= %25.17E / %-42s", key, value, comment);
    return (fitsWriteData (pFile, 80, card));
}

/*******************************************************************************
*/
int fitsWriteInteger
    (
    struct fitsPrivate *pFile,	/* pointer to FITS file control struct */
    const char key[],
    int value,
    const char comment[]
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 */
{
    char card[81];

    sprintf (card, "%-8s= %20d / %-47s", key, value, comment);
    return (fitsWriteData (pFile, 80, card));
}

/*******************************************************************************
*/
int fitsWriteLogical
    (
    struct fitsPrivate *pFile,	/* pointer to FITS file control struct */
    const char key[],
    int value,
    const char comment[]
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 */
{
    char card[81];

    sprintf (card, "%-8s= %19s%c / %-47s", key, "", value ? 'T' : 'F', comment);
    return (fitsWriteData (pFile, 80, card));
}

/*******************************************************************************
*/
int fitsWriteString
    (
    struct fitsPrivate *pFile,	/* pointer to FITS file control struct */
    const char key[],
    const char value[],
    const char comment[]
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 */
{
	char string[81];
	char format[30];
	char card[81];
	int i;

	sprintf(string, "%-8s= '%-8s'", key, value);
	i = strlen(string);
	if(i < 30)
	{
		i = 30;
	}
	sprintf(format, "%%-30s / %%%ds", i-77);
	sprintf(card, format, string, comment);

	return fitsWriteData(pFile, 80, card);
}

/*******************************************************************************
*/
int fitsWriteEnd
    (
    struct fitsPrivate *pFile	/* pointer to FITS file control struct */
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 */
{
    fitsWriteComment (pFile, "END", "");
    while ((pFile->bytes_written % 2880LL) != 0)
	if (fitsWriteComment (pFile, "", "") == -1)
	    return -1;
    return 0;
}

/*******************************************************************************
*/
static int fitsWriteZeroes
    (
    struct fitsPrivate *pFile	/* pointer to FITS file control struct */
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 *
 * Zero pad the FITS file out to next 2880 byte boundary.
 */
{
    int pad = 2880 - pFile->bytes_written % 2880LL;

    /* terminate table if one is still in progress */
    if (pFile->rows_written != 0
     && fitsWriteEndTable (pFile) != 0)
	return -1;
  
    if (pad != 0 && pad != 2880)
	{
	char zero[2880];
	memset(zero, 0 , pad);
	if (fitsWriteData (pFile, pad, zero) == -1)
	    return -1;
	}

    return 0;
}

/*******************************************************************************
*/
static int fitsWriteEndTable
    (
    struct fitsPrivate *pFile	/* pointer to FITS file control struct */
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 *
 * Update the file's NAXIS2 field to reflect the number of records written to 
 * the file.
 */
{
    fpos_t offset;

    fgetpos(pFile->fp, &offset);

    /* move read/write position to NAXIS2 field */
    if (fsetpos(pFile->fp, &pFile->naxis2_offset) != 0)
	return -1;

    /* write NAXIS2 field */
    if (fitsWriteInteger (pFile, "NAXIS2", pFile->rows_written, "") != 0)
	return -1;

    /* decrement bytes written count because fitsWriteInteger() bumped it 
       and this is re-write not append */
    pFile->bytes_written -= 80LL;

    /* reset read/write position to file end */
    if (fsetpos(pFile->fp, &offset) != 0)
	return -1;

    pFile->rows_written = 0;
    return 0;
}

/*******************************************************************************
*/
int fitsWriteData
    (
    struct fitsPrivate *pFile,	/* pointer to FITS file control struct */
    unsigned int size,		/* number of bytes to write */
    const char *pData		/* buffer to copy from */
    )
/*
 * RETURNS OK = 0 | ERROR = -1
 *
 * Write to a buffered file stream.
 */
{
    if (fwrite (pData, sizeof (char), size, pFile->fp) != size)
	{
	printf("fitsWriteData: write() error");
	}

    pFile->bytes_written += size;
    return 0;
}

/* calculate row size given fitsBinTableColumns */
int fitsRowSize(const struct fitsBinTableColumn *pCol, int ncol)
    {
    int i, n;
    for(n=i=0; i<ncol; i++) n += fitsItemSize(pCol[i].p_repeat_type);
    return n;
    }

/* calculate item size given repeat and type */
static int fitsItemSize(const char *pRT)
    {
    int rpt;
    const char *pC = pRT;
    sscanf(pC, "%d", &rpt);
    while(isdigit(*pC)) pC++;
    return rpt*fitsTypeSize(*pC);
    }

/* size of the various data types */
int fitsTypeSize(char C)
    {
    switch (C)
      {
      case 'D': return 8;
      case 'E': case 'J': return 4;
      case 'I': return 2;
      case 'A': case 'L': return 1;
      default: return 0;
      }
    }

int FitsBinTableSize(const struct fitsBinTableColumn *columns, int nColumns)
{
	int s = 0;
	int i, n;
	char c;
	
	for(i = 0; i < nColumns; i++)
	{
		sscanf(columns[i].p_repeat_type, "%d%c", &n, &c);
		s += n*fitsTypeSize(c);
	}

	return s;
}

int FitsBinRowByteSwap(const struct fitsBinTableColumn *columns, int nColumns,
	void *data)
{
	char *cdata;
	int temp;
	int t, n, b, q;
	char c;

	cdata = (char *)data;
	for(n = 0; n < nColumns; n++)
	{
		sscanf(columns[n].p_repeat_type, "%d%c", &q, &c);
		if(q <= 0)
		{
			continue;
		}
		t = fitsTypeSize(c);
		for(b = 0; b < q; b++)
		{
			switch(t)
			{
			case 1:
				break;	/* no swapping */
			case 2:
				temp = *cdata;
				*cdata = *(cdata+1);
				*(cdata+1) = temp;
				break;
			case 4:
				temp = *cdata;
				*cdata = *(cdata+3);
				*(cdata+3) = temp;
				temp = *(cdata+1);
				*(cdata+1) = *(cdata+2);
				*(cdata+2) = temp;
				break;
			case 8:
				temp = *cdata;
				*cdata = *(cdata+7);
				*(cdata+7) = temp;
				temp = *(cdata+1);
				*(cdata+1) = *(cdata+6);
				*(cdata+6) = temp;
				temp = *(cdata+2);
				*(cdata+2) = *(cdata+5);
				*(cdata+5) = temp;
				temp = *(cdata+3);
				*(cdata+3) = *(cdata+4);
				*(cdata+4) = temp;
				break;
			default:
				return 0;
			}
			cdata += t;
		}
	}	
	return 1;
}

/*******************************************************************************
*/
int mjd2fits
    (
    int date,		/* MJD date */
    char *pFitsStr	/* on return this strings contains fits format date */
    )
/*
 * RETURN OK = 0 | ERROR = -1
 *
 * This function converts the given Modified Julian Date to a string in 
 * "FITS" format, e.g. 94Feb28 yields "28/02/94".
 */
{
    int year;
    int month;
    int day;

    if (mjd2date (date, &year, &month, &day) == -1)
	return -1;
    
    sprintf(pFitsStr, "%04d-%02d-%02d", year, month, day);

    return 0;
}


void fprintFitsKeywords(FILE *out, const struct fits_keywords *keys)
{
	fprintf(out, "FITS keywords [%p]\n", keys);
	if(keys)
	{
		fprintf(out, "  Obs code = %s\n", keys->obscode);
		fprintf(out, "  Num Stokes = %d\n", keys->no_stkd);
		fprintf(out, "  First Stokes = %d\n", keys->stk_1);
		fprintf(out, "  Num bands = %d (either single or dual pol)\n", keys->no_band);
		fprintf(out, "  Num chans = %d (per band)\n", keys->no_chan);
		fprintf(out, "  Ref freq = %3.1f Hz\n", keys->ref_freq);
		fprintf(out, "  Chan BW = %3.1f Hz\n", keys->chan_bw);
		fprintf(out, "  Ref pixel = %f\n", keys->ref_pixel);
		fprintf(out, "  Ref date = MJD %d\n", keys->ref_date);
		fprintf(out, "  Num pol = %d (maximum in any band)\n", keys->no_pol);
	}
}

void printFitsKeywords(const struct fits_keywords *keys)
{
	fprintFitsKeywords(stdout, keys);
}

void arrayWriteKeys
    (
    struct fits_keywords *p_fits_keys,	/* uv FITS table keywords */
    struct fitsPrivate *p_fitsfile	/* active FITS file pointer */
    )
/*
 * Write FITS table header keywords in current table.
 */
{
    char dateStr[20];

    fitsWriteString  (p_fitsfile, "OBSCODE", p_fits_keys->obscode, "");
    mjd2fits (p_fits_keys->ref_date, dateStr);
    fitsWriteString  (p_fitsfile, "RDATE",   dateStr, "");
    fitsWriteInteger (p_fitsfile, "NO_STKD", p_fits_keys->no_stkd, "");
    fitsWriteInteger (p_fitsfile, "STK_1",   p_fits_keys->stk_1, "");
    fitsWriteInteger (p_fitsfile, "NO_BAND", p_fits_keys->no_band, "");
    fitsWriteInteger (p_fitsfile, "NO_CHAN", p_fits_keys->no_chan, "");
    fitsWriteFloat   (p_fitsfile, "REF_FREQ",p_fits_keys->ref_freq, "");
    fitsWriteFloat   (p_fitsfile, "CHAN_BW", p_fits_keys->chan_bw,  "");
    fitsWriteFloat   (p_fitsfile, "REF_PIXL",p_fits_keys->ref_pixel, "");
}

void strcpypad(char *dest, const char *src, int n)
{
	int i, b=0;

	for(i = 0; i < n; i++)
	{
		if(src[i] == 0)
		{
			b = 1;
		}
		if(b == 1)
		{
			dest[i] = 0;
		}
		else
		{
			dest[i] = src[i];
		}
	}
}

void testFitsBufBytes(int dPointer, int nRowBytes, const char *tableName)
{
	/* Do a sanity check.  This is mainly for developers */
	if(dPointer != nRowBytes)
	{
		fprintf(stderr, "\n%s table : p_fitsbuf - fitsbuf != "
			"nRowBytes : %d %d\n", tableName,
			dPointer, nRowBytes);
		fprintf(stderr, "This program has a bug!\n");
		exit(EXIT_FAILURE);
	}
}
