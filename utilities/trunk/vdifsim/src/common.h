#ifndef __COMMON_H__
#define __COMMON_H__

#include <complex.h>
#include <glib.h>
#include <fftw3.h>
#include <difxio/difx_input.h>
#include "options.h"
#include "configuration.h"

/* structures associated with common signal components */

typedef struct
{
	int sampRate;		/* [Hz] */
	int nSample;		/* total number of samples */
	int index0;		/* Index to samples[subbandId][]  pointing to mjd,sec */
	const DifxFreq *freq;	/* Information about each subband */
	double complex *spectrum;
	double *spectralShaping;
	double *samples;	/* [subbandId][sample Index] ; stores 1 second of data, plus buffer at start and end */
	fftw_plan fftPlan;
	int freqId;
	char pol;
	int nUnderflow;		/* number of copy requests for data earlier than array */
	int nOverflow;		/* number of copy requests for data later than array */
} CommonSubband;

typedef struct
{
	const DifxInput *D;
	int mjd;		/* Geocentric MJD */
	int sec;		/* Geocentric second of the day */
	int nSubband;		/* Number of sub-bands (intermediate frequencies) */
	CommonSubband *subband;	/* SubBand */
	GRand *random;		/* Glib's random number structure */
	double fluxDensity;	/* continuum source flux density */
	const DifxSource *source;
	int ephemeris;		/* non-zero if ephemeris-driven */
} CommonSignal;

CommonSignal *newCommonSignal(const DifxInput *D, const CommandLineOptions *opts, const Configuration *config);

void deleteCommonSignal(CommonSignal *C);

void generateCommonSignal(CommonSignal *C, int mjd, int sec);

const CommonSubband *getCommonSubband(const CommonSignal *C, int freqId, char pol);

int copySamples(const CommonSubband *cs, double *dest, int startSample, int nSamp);

void printCommonSignal(const CommonSignal *C);

/* Move to utility file? */
void rand_gauss_values(GRand *R, double *array, double rms, int N);

void add_rand_gauss_values(GRand *R, double *array, double rms, int N);

#endif
