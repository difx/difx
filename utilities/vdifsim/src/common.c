#include <string.h>
#include <math.h>
#include <glib.h>
#include <complex.h>
#include <fftw3.h>
#include "common.h"

static int hasBand(const CommonSignal *C, int freqId, char pol)
{
	int s;

	if(C->nSubband == 0)
	{
		return 0;
	}
	for(s = 0; s < C->nSubband; ++s)
	{
		const CommonSubband *S;

		S = C->subband + s;

		if(S->freqId == freqId && S->pol == pol)
		{
			return 1;
		}
	}

	return 0;
}

const CommonSubband *getCommonSubband(const CommonSignal *C, int freqId, char pol)
{
	int s;

	for(s = 0; s < C->nSubband; ++s)
	{
		const CommonSubband *S;

		S = C->subband + s;

		if(S->freqId == freqId && S->pol == pol)
		{
			return S;
		}
	}

	return 0;
}

static inline double sinc(double x)
{
	return x == 0 ? 1.0 : sin(x)/x;
}

void shapeCommonSubband(CommonSubband *cs, const Configuration *config)
{
	double f0, f1, df;	/* [MHz] */
	int N;
	int i, f;

/* Note: accumulate the array in power units, then take a final sqrt at the end */

	if(!cs->spectralShaping)
	{
		cs->spectralShaping = (double *)malloc((cs->nSample/2) * sizeof(double));
	}

	N = cs->nSample/2;
	f0 = cs->freq->freq;
	df = cs->freq->bw / N;
	f1 = cs->freq->freq + cs->freq->bw;

/* 1. start with any broad-band signal, or blank the shaping */
	if(config->fluxDensity)
	{
		if(config->specIndex != 0.0 && config->specIndexFreq > 0.0)
		{
			for(i = 0; i <= N; ++i)
			{
				double freq;
				freq = f0 + i*df;

				cs->spectralShaping[i] = config->fluxDensity * pow(freq/config->specIndexFreq, config->specIndex);
			}
		}
		else
		{
			for(i = 0; i <= cs->nSample/2; ++i)
			{
				cs->spectralShaping[i] = config->fluxDensity;
			}
		}

	}
	else
	{
		memset(cs->spectralShaping, 0, (cs->nSample/2) * sizeof(double));
	}

/* 2. add other features */
	for(f = 0; f < config->nFeature; ++f)
	{
		const Feature *F;
		double delta;	/* [MHz] distance of feature center to either end of the subband, or zero if in subband */
		double d0, d1;

		F = config->features + f;

		if(f0 < F->freq && F->freq < f1)
		{
			delta = 0.0;
		}
		else
		{
			d0 = fabs(F->freq - f0);
			d1 = fabs(F->freq - f1);
			delta = d0 < d1 ? d0 : d1;
		}
		
		switch(F->type)
		{
		case Feature_Gaussian:
			if(delta < 3*F->width)
			{
				double sigma;	/* [MHz] */

				sigma = F->width/sqrt(8.0*log(2));  /* See https://brainder.org/2011/08/20/gaussian-kernels-convert-fwhm-to-sigma/ */
				for(i = 0; i <= cs->nSample/2; ++i)
				{
					double x;

					x = (f0 + i*df - F->freq)/sigma;
					cs->spectralShaping[i] += F->fluxDensity*exp(-x*x/2);
				}
			}
			break;
		case Feature_Sinc:
			if(delta < 100*F->width)
			{
				for(i = 0; i <= cs->nSample/2; ++i)
				{
					double x;

					x = (f0 + i*df - F->freq)/F->width;
					if(x < 100)
					{
						double s;

						s = sinc(2.0*M_PI*x);
						cs->spectralShaping[i] += F->fluxDensity*s*s;
					}
				}
			}
			break;
		case Feature_Triangle:
			if(delta < F->width/2.0)
			{
				int i0, i1;

				i0 = (F->freq - F->width/2.0 - f0)/df;
				if(i0 < 0)
				{
					i0 = 0;
				}
				i1 = (F->freq + F->width/2.0 - f0)/df;
				if(i1 > N)
				{
					i1 = N;
				}
				for(i = i0; i <= i1; ++i)
				{
					double x;

					x = 1.0-fabs((f0 + i*df - F->freq)/(F->width/2.0));
					cs->spectralShaping[i] += F->fluxDensity * x;
				}
			}
			break;
		case Feature_Box:
			if(delta < F->width/2.0)
			{
				int i0, i1;

				i0 = (F->freq - F->width/2.0 - f0)/df;
				if(i0 < 0)
				{
					i0 = 0;
				}
				i1 = (F->freq + F->width/2.0 - f0)/df;
				if(i1 > N)
				{
					i1 = N;
				}
				for(i = i0; i <= i1; ++i)
				{
					cs->spectralShaping[i] += F->fluxDensity;
				}
			}
			break;
		case Feature_Tone:
			if(delta == 0.0)
			{
			}
			break;
		default:
			break;
		}
	}

	for(i = 0; i <= cs->nSample/2; ++i)
	{
		if(cs->spectralShaping[i] > 0)
		{
			cs->spectralShaping[i] = sqrt(cs->spectralShaping[i]);
		}
		else
		{
			cs->spectralShaping[i] = 0;
		}
	}
}

CommonSignal *newCommonSignal(const DifxInput *D, const CommandLineOptions *opts, const Configuration *config)
{
	CommonSignal *C;
	int dsId;

	C = (CommonSignal *)calloc(1, sizeof(CommonSignal));
	C->D = D;
	C->fluxDensity = opts->fluxDensity;
	if(opts->randSeed)
	{
		C->random = g_rand_new_with_seed(opts->randSeed);
	}
	else
	{
		C->random = g_rand_new();
	}
	C->subband = (CommonSubband *)calloc(2*D->nFreq, sizeof(CommonSubband));	/* may be more storage than needed */

	/* loop over all datastreams.  In each case add to the list of subbands as un-used ones are identified */

	for(dsId = 0; dsId < D->nDatastream; ++dsId)
	{
		DifxDatastream *ds;
		int b;

		ds = D->datastream + dsId;
		for(b = 0; b < ds->nRecBand; ++b)
		{
			int freqId;

			freqId = ds->recFreqId[ds->recBandFreqId[b]];

			if(!hasBand(C, freqId, ds->recBandPolName[b]))
			{
				CommonSubband *cs;

				cs = C->subband + C->nSubband;

				cs->freqId = freqId;
				cs->freq = D->freq + freqId;
				cs->pol = ds->recBandPolName[b];
				cs->sampRate = 2 * (int)(cs->freq->bw*1000000.0 + 0.5);
				cs->nSample = (int)(cs->sampRate * 1.024);		/* array to be 1024ms long */
				cs->index0 = (int)(cs->sampRate * 0.001);		/* zero point of array is 1ms into array */
				cs->spectrum = (double complex *)fftw_malloc((cs->nSample+2)*sizeof(double));
				cs->samples = (double *)fftw_malloc(cs->nSample*sizeof(double));
				cs->fftPlan = fftw_plan_dft_c2r_1d(cs->nSample, cs->spectrum, cs->samples, FFTW_ESTIMATE | FFTW_DESTROY_INPUT);

				++C->nSubband;

				if(config)
				{
					shapeCommonSubband(cs, config);
				}
			}
		}
	}

	C->source = D->source + D->scan->pointingCentreSrc;
	C->ephemeris = (C->source->spacecraftId >= 0) ? 1 : 0;

	return C;
}

void deleteCommonSignal(CommonSignal *C)
{
	if(C)
	{
		if(C->nSubband > 0)
		{
			int s;
			
			for(s = 0; s < C->nSubband; ++s)
			{
				CommonSubband *cs;

				cs = C->subband + s;

				fftw_destroy_plan(cs->fftPlan);
				cs->fftPlan = 0;

				if(cs->spectrum)
				{
					fftw_free(cs->spectrum);
					cs->spectrum = 0;
				}
				if(cs->samples)
				{
					fftw_free(cs->samples);
					cs->samples = 0;
				}
				if(cs->spectralShaping)
				{
					free(cs->spectralShaping);
					cs->spectralShaping = 0;
				}
			}

			free(C->subband);
			C->subband = 0;
		}
		
		g_rand_free(C->random);

		free(C);
	}
}

void rand_gauss_values(GRand *R, double *array, double rms, int N)
{
	int i;

	if(N % 2 == 1)
	{
		fprintf(stderr, "rand_gauss_values : N must be even!  Was %d\n", N);

		exit(0);
	}

	for(i = 0; i < N; i += 2)
	{
		double fac, rsq, v1, v2;

		do
		{
			v1 = g_rand_double_range(R, -1.0, 1.0);
			v2 = g_rand_double_range(R, -1.0, 1.0);
			rsq = v1*v1 + v2*v2;
		} while(rsq >= 1.0 || rsq == 0.0);
		
		fac = sqrt(-2.0*log(rsq)/rsq)*rms;
		array[i] = v1 * fac;
		array[i+1] = v2 * fac;
	}
}

void add_rand_gauss_values(GRand *R, double *array, double rms, int N)
{
	int i;

	if(N % 2 == 1)
	{
		fprintf(stderr, "rand_gauss_values : N must be even!  Was %d\n", N);

		exit(0);
	}

	for(i = 0; i < N; i += 2)
	{
		double fac, rsq, v1, v2;

		do
		{
			v1 = g_rand_double_range(R, -1.0, 1.0);
			v2 = g_rand_double_range(R, -1.0, 1.0);
			rsq = v1*v1 + v2*v2;
		} while(rsq >= 1.0 || rsq == 0.0);
		
		fac = sqrt(-2.0*log(rsq)/rsq)*rms;
		array[i] += v1 * fac;
		array[i+1] += v2 * fac;
	}
}

void generateCommonSignal(CommonSignal *C, int mjd, int sec)
{
	int s;

	C->mjd = mjd;
	C->sec = sec;

	for(s = 0; s < C->nSubband; ++s)
	{
		CommonSubband *cs;

		cs = C->subband + s;

		if(cs->spectralShaping)
		{
			int i;

/* 1a. Generate the data in the frequency domain */
			/* scaling is such that after the following FFT a broadband source will have RMS of about 1 Jy */
			rand_gauss_values(C->random, (double *)(cs->spectrum), 1.0/sqrt(cs->nSample), cs->nSample+2);

/* 2a. Here add spectral shaping (if desired) */
			for(i = 0; i <= cs->nSample/2; ++i)
			{
				cs->spectrum[i] *= cs->spectralShaping[i];
			}
		}
		else	/* No shaping provided, use broad-band flux density */
		{
/* 1b. Generate the data in the frequency domain */
			/* scaling is such that after the following FFT a broadband source will have RMS of about sqrt(C->fluxDensity) Jy */
			rand_gauss_values(C->random, (double *)(cs->spectrum), sqrt(C->fluxDensity/cs->nSample), cs->nSample+2);
		}

/* 3. Transform data into the time domain */
		fftw_execute(cs->fftPlan);
	}
}

int copySamples(const CommonSubband *cs, double *dest, int startSample, int nSamp)
{
	const void *src;
	int N;
	int index;

	index = startSample + cs->index0;
	src = cs->samples + index;
	N = nSamp;
	if(index < 0)
	{
		memset(dest, 0, -index*sizeof(double));
		src -= index;
		dest -= index;
		N += index;
fprintf(stderr, "[U:%d]", index);
		//++cs->nUnderflow;
	}
	if(index + N > cs->nSample)
	{
fprintf(stderr, "[O:%d]", index+N-cs->nSample);
		N = cs->nSample - index;
		memset(dest + N, 0, (nSamp-N)*sizeof(double));
		//++css->nOverflow;
	}
	if(N > 0)
	{
		memcpy(dest, src, N*sizeof(double));
	}
	else
	{
		N = 0;
	}

	return N;
}

void printCommonSignal(const CommonSignal *C)
{
	printf("CommonSignal [%p]\n", C);
	if(C)
	{
		int s;

		printf("  mjd = %d\n", C->mjd);
		printf("  sec = %d\n", C->sec);
		printf("  nSubband = %d\n", C->nSubband);
		for(s = 0; s < C->nSubband; ++s)
		{
			CommonSubband *cs;

			cs = C->subband + s;

			printf("    Subband %d:\n", s);
			printf("      freqId = %d\n", cs->freqId);
			printf("      pol = %c\n", cs->pol);
			printf("      freq = %f MHz\n", cs->freq->freq);
			printf("      sampRate = %d /sec\n", cs->sampRate);
			printf("      nSample = %d\n", cs->nSample);
			printf("      index0 = %d\n", cs->index0);
		}
	}
}
