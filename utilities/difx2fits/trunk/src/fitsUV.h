#ifndef __FITS_UV_H__
#define __FITS_UV_H__

#include <complex.h>
#include <math.h>
#include "difxio/parsedifx.h"
#include "difx2fits.h"

struct __attribute__((packed)) UVrow
{
	float U, V, W;
	double jd, iat;
	int32_t baseline, filter, sourceId, freqId;
	float intTime;
	/* FIXME -- no pulsar gate id! */
	float data[0];	/* this takes no room in the "sizeof" operation */
};

typedef struct
{
	glob_t globbuf;
	int curFile, nFile;
	const DifxInput *D;
	DifxParameters *dp;
	FILE *in;
	struct fitsPrivate *out;
	double U, V, W;
	double mjd;
	float tInt;
	int baseline;
	int configId;
	int sourceId;
	int freqId;			/* DiFX configId or AIPS freqId */
	int IFnum;			/* which BB channel */
	int polId;
	int nChan, nPol, nFreq;
	int maxChan, maxPol;
	int polStart;			/* start of polarization FITS axis */
	float *spectrum;		/* visibility spectrum */
	int nData;
	int nComplex;
	struct UVrow *record;
	float *weight;
	float *data;
} DifxVis;

DifxVis *newDifxVis(const DifxInput *D, const char *filebase, 
	struct fitsPrivate *out);
void deleteDifxVis(DifxVis *dv);
int DifxVisNextFile(DifxVis *dv);
int DifxVisNewUVData(DifxVis *dv);


#endif
