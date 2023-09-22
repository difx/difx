#ifndef __DELAYCAL_H__
#define __DELAYCAL_H__

#include <complex.h>
#include <math.h>
#include "difxio/difx_input.h"

/* to speed up application */
typedef struct
{
	double lastMJD;	/* [day] */
	double weight0;
	double weight1;
	int index0;	/* index to data[] below */
	int index1;	/* index to data[] below */
} DelayCalCache;

void resetDelayCalCache(DelayCalCache *dcc);

void printDelayCalCache(const DelayCalCache *dcc);


typedef struct
{
	double mjd;
	double **delay;	/* [us]  indexed by [antennaID][bbc] */
	double **phase;	/* [rad] indexed by [antennaID][bbc] */
	/* FIXME: add double **rate; */
} DelaySolution;

typedef struct
{
	int nAntenna;
	int nBBC;
	int nSlot;
	int nPol;
	char pols[4];
	DelaySolution *data;	/* index by time slot */
} DelayCal;

DelayCal *loadDelayCal(const char *filename, const DifxInput *D);

void deleteDelayCal(DelayCal *dc);

void printDelayCal(const DelayCal *dc);

int applyDelayCal(float complex *spectrum, int nChan, double deltaFreq, int freqId, int antId1, char pol1, int antId2, char pol2, double mjd, const DelayCal *dc, DelayCalCache *dcc);

#endif
