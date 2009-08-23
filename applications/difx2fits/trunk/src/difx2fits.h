#ifndef __DIFX2FITS_H__
#define __DIFX2FITS_H__

#include <difxio/difx_input.h>
#include "fits.h"

#define array_MAX_BANDS 32
#define array_MAX_TONES	64
#define MAX_INPUT_FILES 4096


struct CommandLineOptions
{
	char *fitsFile;
	char *baseFile[MAX_INPUT_FILES];
	int nBaseFile;
	int writemodel;
	int pretend;
	double scale;
	int verbose;
	/* some overrides */
	int specAvg;
	int doalldifx;
	float nOutChan;
	float startChan;
	int keepOrder;
	int dontCombine;
	int overrideVersion;
	double sniffTime;
	int pulsarBin;
	double jobMatrixDeltaT;	/* seconds */
};

const DifxInput *DifxInput2FitsHeader(const DifxInput *D,
	struct fitsPrivate *out);

const DifxInput *DifxInput2FitsAG(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsSU(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsAN(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsFR(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsML(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsCT(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsMC(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsUV(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out, 
	struct CommandLineOptions *opts);

const DifxInput *DifxInput2FitsFL(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsTS(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsPH(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsWR(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsGN(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsSO(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsGD(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

const DifxInput *DifxInput2FitsGM(const DifxInput *D,
	struct fits_keywords *p_fits_keys, struct fitsPrivate *out);

#endif
