#ifndef __DIFX2FITS_H__
#define __DIFX2FITS_H__

#include <difxio/difx_input.h>
#include "fits.h"

#define array_MAX_BANDS 32
#define array_MAX_TONES	64

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
	double scale, int verbose, double sniffTime, int pulsarBin);

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
