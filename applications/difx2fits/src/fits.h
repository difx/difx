/* stolen from VLBA's fits.h */

#ifndef __FITS_H__
#define __FITS_H__

#include <stdio.h>
#include <math.h>
#include <string.h>
#include "other.h"

#ifndef M_PI
#define M_PI           3.14159265358979323846
#endif

#define FITS_WRITE_ITEM(item, buffer)  { memcpy(buffer, (char *)(&(item)), sizeof(item)); buffer += sizeof(item); }

#define FITS_WRITE_ARRAY(array, buffer, n)  { memcpy(buffer, (char *)((array)), n*sizeof((array)[0])); buffer += n*sizeof((array)[0]); }

// Codes for STK_1 from AIPS Memo 114r section 2.6 table 6
#define FITS_STOKES_UNDEFINED 0
#define FITS_STOKES_I  1
#define FITS_STOKES_Q  2
#define FITS_STOKES_U  3
#define FITS_STOKES_V  4
#define FITS_STOKES_RR -1
#define FITS_STOKES_LL -2
#define FITS_STOKES_RL -3
#define FITS_STOKES_LR -4
#define FITS_STOKES_XX -5
#define FITS_STOKES_YY -6
#define FITS_STOKES_XY -7
#define FITS_STOKES_YX -8
#define FITS_STOKES_AN_TABLE -9 // outside FITS-IDI spec; indicates that correct pols are in AN table (esp. when H,V)

#define FITS_OBS_CODE_LENGTH	24

/* Structs for describing FITS data types: */

struct fitsBinTableColumn
{
	char *p_name;
	char *p_repeat_type;
	char *p_comment;
	char *p_units;
};

struct fitsTableColumn
{
	char *p_name;
	char *p_format;
	char *p_comment;
	char *p_units;
};

struct fitsMatrixAxis
{
	int naxis;
	char *p_ctype;
	double crpix;
	double crval;
	double cdelt;
	double crota;
	char *p_comment;
};

struct fitsMatrixPixels
{
	int bitpix;
	float bzero;
	float bscale;
	int blank;
};

/* private data structure for the FITS package */
struct fitsPrivate
{
	FILE *fp;
	char filename[256];
	long long bytes_written;
	int row_bytes;
	fpos_t naxis2_offset;
	int rows_written;
};

struct fits_keywords
{
	char obscode[FITS_OBS_CODE_LENGTH];	/* observation code */
	int no_stkd;		/* number of polarizations in data */
	int stk_1;		/* first Stokes parameter in data */
	int no_band;		/* number of 'bands' in data */
	int no_chan;		/* number of spectral channels in data */
	double ref_freq;	/* reference frequency */
	float chan_bw;		/* freq. channel bandwidth */
	float ref_pixel;	/* reference freq at which pixel */
	int ref_date;		/* reference date in MJD (0hrs) */
	int no_pol;		/* number polarizations */
};

int fitsReadOpen (struct fitsPrivate *, const char *);
int fitsWriteBinRow (struct fitsPrivate *, const char *);
int fitsWriteBinTable (struct fitsPrivate *, int, const struct fitsBinTableColumn *, int, const char *, int);
int fitsWriteClose (struct fitsPrivate *);
int fitsWriteComment (struct fitsPrivate *, const char *, const char *); 
int fitsWriteData (struct fitsPrivate *, unsigned int, const char *); 
int fitsWriteDouble (struct fitsPrivate *, char *, double, char *);
int fitsWriteEnd (struct fitsPrivate *);
int fitsWriteFloat (struct fitsPrivate *, const char *, double, const char *); 
int fitsWriteInteger (struct fitsPrivate *, const char *, int, const char *); 
int fitsWriteLogical (struct fitsPrivate *, const char *, int, const char *); 
int fitsWriteOpen (struct fitsPrivate *, const char *);
int fitsWriteString (struct fitsPrivate *, const char *, const char *, const char *); 
int fitsWriteTable (struct fitsPrivate *, int, struct fitsTableColumn *, int, const char *);
int fitsRowSize(const struct fitsBinTableColumn *, int);
int FitsBinTableSize(const struct fitsBinTableColumn *columns, int nColumns);
int FitsBinRowByteSwap(const struct fitsBinTableColumn *columns, int nColumns, void *data);
int fitsTypeSize(char);
int mjd2fits(int, char *);
void fprintFitsKeywords(FILE *out, const struct fits_keywords *keys);
void printFitsKeywords(const struct fits_keywords *keys);
void arrayWriteKeys(struct fits_keywords *p_fits_keys, struct fitsPrivate *p_fitsfile);
void strcpypad(char *dest, const char *src, int n);
void testFitsBufBytes(int dPointer, int nRowBytes, const char *tableName);

#endif
