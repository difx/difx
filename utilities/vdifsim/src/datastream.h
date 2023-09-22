#ifndef __DATASTREAM_H__
#define __DATASTREAM_H__

#include <stdio.h>
#include <complex.h>
#include <glib.h>
#include <fftw3.h>
#include <difxio/difx_input.h>
#include "options.h"
#include "common.h"
#include "configuration.h"

/* major steps
 *
 * 1. select samples from CommonSignal -> samps
 * 2. additive noise
 * 3. fringe rotate -> frSamps
 * 4. FFT -> spec
 * 5. frac sample correction -> spec
 * 6. apply filter
 * 7. iFFT -> samps
 * 8. put chunk into 1 second buffer
 * 9. AGC -> samps
 * 10. write VDIF
 */

typedef struct
{
	int threadId;			/* value comes from index of DifxFreq */
	const CommonSubband *cs;
	const DifxFreq *df;		/* pointer to details on the spectrum */
	int nChunk;			/* number of sub-second chunks in one second */
	int nSamp;			/* number of real samples to work on at one time */
	int nSample1sec;		/* size of 1 second array */
	double *samps;			/* actual sample value */
	double complex *frSamps;	/* complex valued time series storing result of fringe rotation */
	double complex *spec;		/* spectral representation of data */
	double *samples1sec;		/* full 1 second of output data */
	fftw_plan c2cPlan;		/* for step 4 */
	fftw_plan ifftPlan;		/* for step 7 */
	char pol;
} DatastreamSubband;

typedef struct
{
	const char *antennaName;
	const DifxDatastream *dd;	/* all of the details related to the datastream */
	const DifxAntenna *da;		/* antenna-specific info, e.g., clock model */
	const DifxPolyModel *im;	/* delay model */
	int datastreamId;		/* from DifxInput */
	int framesPerSecond;		/* VDIF frames per second per subband */
	int samplesPerFrame;		/* VDIF samples per frame */
	int dataBytes;			/* data bytes per VDIF frame, excluding header */
	int bits;			/* bits per (real) sample */
	double bw;			/* [Hz] bandwidth per subband */
	const char *filename;
	FILE *out;
	int nSubband;
	DatastreamSubband *subband;
	double SEFD;			/* [Jy] amount of additive noise */
	GRand *random;
	double *filter;			/* channelization filter */
	const AntennaParameters *parameters;	/* from configuration file; null if no config is set */
} Datastream;

Datastream *newDatastream(const DifxInput *D, int dsId, const CommonSignal *C, const CommandLineOptions *opts, const Configuration *config);

Datastream **newDatastreams(const DifxInput *D, const CommonSignal *C, const CommandLineOptions *opts, const Configuration *config);

void deleteDatastream(Datastream *d);

void deleteDatastreams(Datastream **ds);

void printDatastream(const Datastream *d);

void writeVDIF(const Datastream *d, const CommonSignal *C);

void datastreamProcess(const DifxInput *D, const CommonSignal *C, Datastream *d);

#endif
