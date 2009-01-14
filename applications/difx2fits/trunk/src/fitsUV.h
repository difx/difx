#ifndef __FITS_UV_H__
#define __FITS_UV_H__

#include <complex.h>
#include <math.h>
#include <glob.h>
#include <sys/types.h>
#include "difxio/parsedifx.h"
#include "difx2fits.h"

struct __attribute__((packed)) UVrow
{
	float U, V, W;
	double jd, iat;
	int32_t baseline, filter;
	int32_t sourceId1, freqId1;	/* 1-based FITS indices */
	float intTime;
	/* FIXME -- no pulsar gate id! */
	float data[0];	/* this takes no room in the "sizeof" operation */
};

/* Information useful for tracking properies of visibility records */
typedef struct
{
	glob_t globbuf;
	int curFile, nFile;
	const DifxInput *D;
	DifxParameters *dp;
	FILE *in;
	double U, V, W;
	double mjd;
	float tInt;
	int baseline;
	int *antennaIdRemap;		/* to convert baseline number */
	int jobId;
	int configId;
	int sourceId;
	int scanId;
	int freqId;			/* DiFX configId or FITS freqId */
	int bandId;			/* FITS IF index, 0-based */
	int polId;			/* FITS polarization index, 0-based */
	int pulsarBin;
	int nPol, nFreq;
	int polStart;			/* start of polarization FITS axis */
	float *spectrum;		/* input visibility spectrum */
	float recweight;
	int nData;
	int nComplex;
	struct UVrow *record;
	float *weight;
	float *data;
	int changed;
	int first;
	double scale;
	int flagTransition;
} DifxVis;

DifxVis *newDifxVis(const DifxInput *D, int jobId);
void deleteDifxVis(DifxVis *dv);
int DifxVisNextFile(DifxVis *dv);
int DifxVisNewUVData(DifxVis *dv, int verbose, int pulsarBin);
int DifxVisCollectRandomParams(const DifxVis *dv);


#endif
