#ifndef __BANDPASS_H__
#define __BANDPASS_H__

#include <complex.h>
#include <math.h>
#include "difxio/difx_input.h"

/***

bandpass.c/h implement routines to read in a bandpass file and apply it 
to the visibilities prior to creating the .FITS file.

The functionality is fairly primitive and not well tested as of initial 
deployment.  To try it:

1. Run difx2fits with the --bandpass option on a single strong calibrator
source.  A file ending in .bandpass will be created.  This file contains
baseline-based bandpasses.  This filetype is not ready to be applied.

2. Run bp2antenna on the .bandpass file.  This runs least-squares fitting
to compute antenna-based bandpasses.  Redirect the output to a file.

3. When running difx2fits, use the --applybandpass option, followed by
the filename containing the antenna-based bandpasses.

Use at your own risk, at least until it is better qualified!

***/

typedef struct
{
	char antennaName[MAX_ANTENNA_SITE_NAME_LENGTH];
	int nChan;
	double freq;	/* [MHz] low end of band, always positive */
	double bw;	/* [MHz] bandwidth, always positive */
	char pol;
	float complex *gain;	/* complex gain to apply ; reciprocal of value in file */
} BandpassData;

typedef struct
{
	char obsCode[DIFXIO_OBSCODE_LENGTH];
	const DifxInput *D;
	int nPol;	/* number of observed polarizations; from DifxConfig */
	char pol[2];	/* the polarizations; from DifxConfig */
	int nFreq;
	int nAnt;
	int nData;
	BandpassData *data;
	BandpassData ****bp;	/* [pol][freqId][antId]; points to an element of data[] */
} Bandpass;

Bandpass *loadBandpass(const char *fileName, const DifxInput *D);

void deleteBandpass(Bandpass *B);

int applyBandpass(float complex *spectrum, int nChan, int freqId, int antId1, char pol1, int antId2, char pol2, const Bandpass *B);


#endif
