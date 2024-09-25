#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <errno.h>
#include <complex.h>
#include <math.h>
#include <glib.h>
#include <vdifio.h>
#include <stdint.h>
#include "datastream.h"
#include "model.h"

Datastream *newDatastream(const DifxInput *D, int dsId, const CommonSignal *C, const CommandLineOptions *opts, const Configuration *config)
{
	Datastream *d;
	const DifxDatastream *dd;
	const DifxFreq *df;		/* to use as a reference */
	int v, s;
	struct stat st;
	double f;			/* FFT gain factor */
	int ft;				/* filter transition length in channels */
	int i;

	dd = D->datastream + dsId;
	df = D->freq + dd->recFreqId[0];

	v = stat(dd->file[0], &st);
	if(v == 0 || errno != ENOENT)
	{
		if(opts->force)
		{
			fprintf(stderr, "Warning: %s already exists.  Will be overwriten.\n", dd->file[0]);
		}
		else
		{
			fprintf(stderr, "Error: %s already exists.  Won't overwrite.\n", dd->file[0]);

			exit(0);
		}
	}

	d = (Datastream *)calloc(1, sizeof(Datastream));
	
	printf("Opening %s for write\n", dd->file[0]);
	d->out = fopen(dd->file[0], "w");

	if(!d->out)
	{
		fprintf(stderr, "Error: cannot open %s for write.\n", dd->file[0]);

		exit(0);
	}
		
	d->datastreamId = dsId;
	d->antennaName = D->antenna[dd->antennaId].name;
	if(config)
	{
		d->parameters = getAntennaParametersConst(config, d->antennaName);
	}
	d->da = D->antenna + dd->antennaId;
	d->dd = dd;
	d->filename = dd->file[0];
	d->bw = df->bw * 1000000.0;
	d->bits = dd->quantBits;
	d->dataBytes = dd->dataFrameSize - 32;
	d->samplesPerFrame = 8*d->dataBytes/d->bits;
	d->framesPerSecond = (int)(d->bw*2/d->samplesPerFrame + 0.5);
	d->SEFD = opts->SEFD;
	if(d->parameters)
	{
		if(d->parameters->SEFD > 0.0)
		{
			d->SEFD = d->parameters->SEFD;
		}
	}
	if(opts->randSeed)
	{
		d->random = g_rand_new_with_seed(opts->randSeed + dsId + 1);
	}
	else
	{
		d->random = g_rand_new();
	}

	/* allocate one extra array element to make logic below easier */
	d->filter = (double *)fftw_malloc((D->nInChan + 1)*sizeof(complex));
	f = sqrt(2)/D->nInChan;
	for(i = 0; i <= D->nInChan; ++i)
	{
		d->filter[i] = f;
	}
	if(dd->dataSampling == SamplingComplexDSB)
	{
		/* For double-sideband, use smaller edge transitions and reduce gain near band center */
		ft = D->nInChan * opts->filterTransition/2.0;
		for(i = 0; i < ft; ++i)
		{
			double taper;

			taper = 0.5*(1.0-cos(M_PI*i/ft));
			d->filter[i] *= taper;
			d->filter[D->nInChan - i] *= taper;

			taper = 0.25*(3.0-cos(M_PI*i/ft));
			d->filter[i + D->nInChan/2] *= taper;
			if(i != 0)
			{
				d->filter[D->nInChan/2 - i] *= taper;
			}
		}
	}
	else
	{
		ft = D->nInChan * opts->filterTransition;
		for(i = 0; i < ft; ++i)
		{
			double taper;

			taper = 0.5*(1.0-cos(M_PI*i/ft));
			d->filter[i] *= taper;
			d->filter[D->nInChan-i] *= taper;
		}
	}

	if(opts->debug)
	{
		if(dsId == 0)
		{
			FILE *o;
			o = fopen("filter.txt", "w");
			for(i = 0; i <= D->nInChan; ++i)
			{
				fprintf(o, "%d %e\n", i, d->filter[i]);
			}
			fclose(o);
		}
	}

	d->im = D->scan->im[dd->antennaId][0];	/* only look at first phase center */
	d->nSubband = dd->nRecBand;
	d->subband = (DatastreamSubband *)calloc(d->nSubband, sizeof(DatastreamSubband));
	for(s = 0; s < d->nSubband; ++s)
	{
		DatastreamSubband *ds;
		int freqId;

		ds = d->subband + s;

		freqId = dd->recFreqId[dd->recBandFreqId[s]];
		ds->threadId = s;
		ds->pol = dd->recBandPolName[s];
		ds->df = D->freq + freqId;
		ds->cs = getCommonSubband(C, freqId, ds->pol);
		if(ds->cs == 0)
		{
			fprintf(stderr, "Error: getCommonSubband(_, %d, %c) failed\n", freqId, ds->pol);

			exit(0);
		}
		ds->nSample1sec = ds->cs->sampRate;
		ds->nSamp = 2*D->nInChan;
		ds->nChunk = ds->nSample1sec / ds->nSamp;
		ds->samps = (double *)fftw_malloc(ds->nSamp * sizeof(double));
		ds->frSamps = (double complex *)fftw_malloc((ds->nSamp + 1) * sizeof(double complex));
		ds->samples1sec = (double *)fftw_malloc(ds->nSample1sec * sizeof(double));
		ds->spec = (double complex *)fftw_malloc((ds->nSamp + 1) * sizeof(double complex));
		ds->c2cPlan = fftw_plan_dft_1d(ds->nSamp, ds->frSamps, ds->spec, FFTW_FORWARD, FFTW_ESTIMATE | FFTW_DESTROY_INPUT);
		if(dd->dataSampling == SamplingReal)
		{
			ds->ifftPlan = fftw_plan_dft_c2r_1d(ds->nSamp, ds->spec, ds->samps, FFTW_ESTIMATE | FFTW_DESTROY_INPUT);
		}
		else if(dd->dataSampling == SamplingComplex || dd->dataSampling == SamplingComplexDSB)
		{
			ds->ifftPlan = fftw_plan_dft_1d(ds->nSamp/2, ds->spec, (double complex *)(ds->samps), FFTW_BACKWARD, FFTW_ESTIMATE | FFTW_DESTROY_INPUT);
		}
		else
		{
			fprintf(stderr, "Sampling type %d is unsupported.\n", (int)(dd->dataSampling));

			exit(1);
		}
	}

	return d;
}

Datastream **newDatastreams(const DifxInput *D, const CommonSignal *C, const CommandLineOptions *opts, const Configuration *config)
{
	Datastream **ds;
	int dsId;

	ds = (Datastream **)malloc((D->nDatastream+1)*sizeof(Datastream *));

	for(dsId = 0; dsId < D->nDatastream; ++dsId)
	{
		ds[dsId] = newDatastream(D, dsId, C, opts, config);
	}

	ds[D->nDatastream] = 0;

	return ds;
}

void deleteDatastream(Datastream *d)
{
	if(d)
	{
		if(d->out)
		{
			printf("Closing %s\n", d->filename);
			fclose(d->out);
		}
		if(d->nSubband > 0)
		{
			int s;

			for(s = 0; s < d->nSubband; ++s)
			{
				DatastreamSubband *ds;

				ds = d->subband + s;

				fftw_destroy_plan(ds->c2cPlan);
				fftw_destroy_plan(ds->ifftPlan);
				fftw_free(ds->samps);
				fftw_free(ds->frSamps);
				fftw_free(ds->spec);
				fftw_free(ds->samples1sec);
			}

			free(d->subband);
		}

		g_rand_free(d->random);
		fftw_free(d->filter);

		free(d);
	}
}

void deleteDatastreams(Datastream **ds)
{
	if(ds)
	{
		int dsId;

		for(dsId = 0; ds[dsId]; ++dsId)
		{
			deleteDatastream(ds[dsId]);
		}

		free(ds);
	}
}

void printDatastream(const Datastream *d)
{
	printf("Datastream %d [%p]\n", d->datastreamId, d);
	if(d)
	{
		int s;

		printf("  antenna = %s\n", d->antennaName);
		printf("  framesPerSecond = %d\n", d->framesPerSecond);
		printf("  samplesPerFrame = %d\n", d->samplesPerFrame);
		printf("  dataBytes = %d\n", d->dataBytes);
		printf("  bits = %d\n", d->bits);
		printf("  bandwidth = %f Hz\n", d->bw);
		printf("  output filename = %s\n", d->filename);
		printf("  nSubband = %d\n", d->nSubband);
		for(s = 0; s < d->nSubband; ++s)
		{
			const DatastreamSubband *ds;
		
			ds = d->subband + s;

			printf("    Subband %d:\n", s);
			printf("      pol = %c\n", ds->pol);
			printf("      threadId = %d\n", ds->threadId);
			printf("      nChunk = %d\n", ds->nChunk);
			printf("      nSamp = %d\n", ds->nSamp);
			printf("      nSample1sec = %d\n", ds->nSample1sec);
		}
	}
}

/* Move to utility source file? */
/* scale array[] so the RMS value is 1 */
void normalize(double *array, int N)
{
	double ss, f;
	int i;

	ss = 0.0;
	for(i = 0; i < N; ++i)
	{
		ss += array[i]*array[i];
	}
	if(ss > 0.0)
	{
		f = sqrt(N/ss);
	}
	else
	{
		f = 0.0;
	}
	for(i = 0; i < N; ++i)
	{
		array[i] *= f;
	}
}

/* Move to vdifio? */
/* for optimal results, src[] should be zero mean */
void encode1bit(unsigned char *dest, double *src, int N)
{
	int i;
	unsigned char a;
	int s;
	
	s = 0;
	a = 0;
	for(i = 0; i < N; ++i)
	{
		unsigned char b;

		b = src[i] < 0.0 ? 0 : 1;

		a |= (b << s);
		if(s == 7)
		{
			*dest = a;
			++dest;
			a = 0;
			s = 0;
		}
		else
		{
			++s;
		}
	}
}

/* Move to vdifio? */
/* For optimal results, src[] should be zero mean with RMS=1.0 */
void encode2bit(unsigned char *dest, double *src, int N)
{
	const double thresh = 0.96;	/* for optimal SNR */
	int i;
	unsigned char a;
	int s;
	
	s = 0;
	a = 0;
	for(i = 0; i < N; ++i)
	{
		unsigned char b;

		if(src[i] < -thresh) 
		{
			b = 0;
		}
		else if(src[i] < 0.0)
		{
			b = 1;
		}
		else if(src[i] < thresh)
		{
			b = 2;
		}
		else
		{
			b = 3;
		}

		a |= (b << s);

		if(s == 6)
		{
			*dest = a;
			++dest;
			a = 0;
			s = 0;
		}
		else
		{
			s += 2;
		}
	}
}

/* Move to vdifio? */
/* For optimal results, src[] should be zero mean with RMS=1.0 */
void encode4bit(unsigned char *dest, double *src, int N)
{
	const double v0 = 0.3356;	/* see https://library.nrao.edu/public/memos/vlba/up/VLBASU_52.pdf */
	int i;
	unsigned char a;
	int s;

	a = 0;
	s = 0;
	for(i = 0; i < N; ++i)
	{
		unsigned char b;
		int q;

		q = src[i] / v0 + 8;
		if(q < 1)
		{
			b = 0;
		}
		else if(q >= 15)
		{
			b = 15;
		}
		else
		{
			b = q;
		}

		a |= (b << s);

		if(s == 4)
		{
			*dest = a;
			++dest;
			a = 0;
			s = 0;
		}
		else
		{
			s += 4;
		}
	}
}

/* Move to vdifio? */
/* For optimal results, src[] should be zero mean with RMS=1.0 */
void encode8bit(unsigned char *dest, double *src, int N)
{
	const double v0 = 0.3356;	/* see https://library.nrao.edu/public/memos/vlba/up/VLBASU_52.pdf */
	int i;

	for(i = 0; i < N; ++i)
	{
		unsigned char b;
		int q;

		q = src[i] / v0 + 128;
		if(q < 1)
		{
			b = 0;
		}
		else if(q >= 255)
		{
			b = 255;
		}
		else
		{
			b = q;
		}

		dest[i] = b;
	}
}

/* For optimal results, src[] should be zero mean with RMS=1.0 */
void encode16bit(unsigned char *dest, double *src, int N)
{
	const double v0 = 0.01;		/* value not necessarily optimal, but with this many bits the sweet spot is large */
	int i;
	uint16_t *d;

	d = (uint16_t *)dest;

	for(i = 0; i < N; ++i)
	{
		int q;

		q = src[i] / v0 + 32768;
		if(q < 1)
		{
			d[i] = 0;
		}
		else if(q >= 65535)
		{
			d[i] = 65535;
		}
		else
		{
			d[i] = q;
		}
	}
}

void writeVDIF(const Datastream *d, const CommonSignal *C)
{
	vdif_header *vh;
	int frameLength;
	unsigned char *data;
	int f;

	frameLength = d->dataBytes + VDIF_HEADER_BYTES;

	vh = (vdif_header *)calloc(1, frameLength);
	data = (unsigned char *)(vh) + VDIF_HEADER_BYTES;
	setVDIFEpochMJD(vh, C->mjd);
	setVDIFFrameMJD(vh, C->mjd);
	setVDIFFrameSecond(vh, C->sec);
	vh->framelength8 = frameLength/8;
	vh->nbits = d->bits - 1;

	if(d->dd->dataSampling != SamplingReal)
	{
		vh->iscomplex = 1;
	}

	for(f = 0; f < d->framesPerSecond; ++f)
	{
		int start;
		int s;

		vh->frame = f;
		start = f * d->samplesPerFrame;

		for(s = 0; s < d->nSubband; ++s)
		{
			const DatastreamSubband *ds;

			ds = d->subband + s;
			vh->threadid = ds->threadId;
			vh->invalid = 0;

			switch(d->bits)
			{
			case 1:
				encode1bit(data, ds->samples1sec + start, d->samplesPerFrame);
				break;
			case 2:
				encode2bit(data, ds->samples1sec + start, d->samplesPerFrame);
				break;
			case 4:
				encode4bit(data, ds->samples1sec + start, d->samplesPerFrame);
				break;
			case 8:
				encode8bit(data, ds->samples1sec + start, d->samplesPerFrame);
				break;
			case 16:
				encode16bit(data, ds->samples1sec + start, d->samplesPerFrame);
				break;
			default:
				fprintf(stderr, "Error: no encoder for other than 2 bits yet!\n");
				exit(0);
			}

			if(d->parameters)
			{
				double v;

				v = g_rand_double_range(d->random, 0.0, 1.0);
				if(v < d->parameters->droppedPacketRate)
				{
					continue;
				}
				else if(v > 1.0-d->parameters->invalidPacketRate)
				{
					vh->invalid = 1;
				}
				
			}
			fwrite(vh, frameLength, 1, d->out);
		}
	}

	fflush(d->out);

	free(vh);
}

/* Note: To handle lower sideband, fringe rotate as if SSLO was at freq minus bandwidth and then swap sign of alternate samples (real) or conjugate (complex) */
void datastreamProcess(const DifxInput *D, const CommonSignal *C, Datastream *d)
{
	int s;

/* loop over subband */
	for(s = 0; s < d->nSubband; ++s)
	{
		DatastreamSubband *ds;
		const CommonSubband *cs;
		int c;
		double freq_MHz;		/* [MHz] frequency in MHz */
		int int_freq_MHz;
		double frac_freq_MHz;
		double *pulseCalSamples;

		ds = d->subband + s;
		cs = ds->cs;
		//freq_MHz = ds->df->freq + 0.5*ds->df->bw;
		freq_MHz = ds->df->freq;
		if(d->dd->dataSampling == SamplingComplexDSB)
		{
			/* In Double-Sideband mode, the sampled band is centered on the specified frequency */
			/* The total bandwidth (sum of both sidebands) is given by specifed bandwidth */

			/* FIXME: are the += and -= correct here? */
			if(ds->df->sideband == 'L')
			{
				freq_MHz -= ds->df->bw / 2.0;
			}
			else
			{
				freq_MHz += ds->df->bw / 2.0;
			}
		}
		else if(ds->df->sideband == 'L')
		{
			freq_MHz -= ds->df->bw;
		}
		int_freq_MHz = (int)freq_MHz;
		frac_freq_MHz = freq_MHz - int_freq_MHz;

		if(d->parameters && d->parameters->pulseCalInterval > 0)
		{
			const epsilon = 0.001;	/* [MHz] don't use tones closer to band edge than this */
			double cosFactor, sinFactor;
			int tone1, tone2;	/* index of first and last tone to use */

			/* These two values scale the cos and sin components of the tone */
			/* In case of lower sideband complex, sinFactor has opposite sign */
			cosFactor = 2.0*sqrt(d->SEFD*d->parameters->pulseCalFrac);
			sinFactor = (ds->df->sideband == 'U') ? cosFactor : -cosFactor;

			tone1 = (freq_MHz+epsilon)/d->parameters->pulseCalInterval + 1;
			tone2 = (freq_MHz+ds->df->bw-epsilon)/d->parameters->pulseCalInterval - 1;

			pulseCalSamples = (double *)calloc(ds->nSamp, sizeof(double));

			if(tone2 >= tone1)
			{
				if(d->dd->dataSampling == SamplingReal)
				{
					int t;

					for(t = tone1; t <= tone2; ++t)
					{
						double toneFreq;	/* [MHz] apparent tone frequency within band */
						double sampleFactor, delayFactor;
						int i;
						
						toneFreq = t * d->parameters->pulseCalInterval - freq_MHz;
						sampleFactor = 2.0*M_PI*toneFreq/(2.0*ds->df->bw);
						delayFactor = 2.0*M_PI*toneFreq*d->parameters->pulseCalDelay;
						if(ds->df->sideband == 'U')
						{
							delayFactor = -delayFactor;	/* this choice of sign is consistent with m5pcal */
						}

						for(i = 0; i < ds->nSamp; ++i)
						{
							pulseCalSamples[i] += cosFactor*cos(delayFactor + i*sampleFactor);
						}
					}
				}
				else
				{
					int t;

					for(t = tone1; t <= tone2; ++t)
					{
						double toneFreq;	/* [MHz] apparent tone frequency within band */
						double sampleFactor, delayFactor;
						int i;

						toneFreq = t * d->parameters->pulseCalInterval - freq_MHz;
						sampleFactor = 2.0*M_PI*toneFreq/(2.0*ds->df->bw);
						delayFactor = 2.0*M_PI*toneFreq*d->parameters->pulseCalDelay;
						if(ds->df->sideband == 'U')
						{
							delayFactor = -delayFactor;	/* this choice of sign is consistent with m5pcal */
						}

						for(i = 0; i < ds->nSamp; i += 2)
						{
							pulseCalSamples[i]   += cosFactor*cos(delayFactor + i*sampleFactor);
							pulseCalSamples[i+1] += sinFactor*sin(delayFactor + i*sampleFactor);
						}
					}
				}
			}
		}

/* Loop over sub-second time (chunk) */
		for(c = 0; c < ds->nChunk; ++c)
		{
			double t;		/* [s] time centroid of chunk */
			double delay_us;	/* [us] delay in microsec */
			int int_delay_us;
			double frac_delay_us;
			double rate_us_s;	/* [us/s] */
			double desiredSample;	/* desired start sample relative to "t0" index of CommonSignal */
			double fracSample;	/* actual minus desired */
			int actualSample;
			int startSample;
			int i;
			double delay_center_samples;
			double clockOffset_us;	/* [us] clock offset (error) */

/* 1. coarse delay -- sample selection */
			/* compute delay model at center of chunk */
			t = C->sec + (c + 0.5)/ds->nChunk;

			if(d->parameters)
			{
				clockOffset_us = d->parameters->clockOffset;
			}
			else
			{
				clockOffset_us = 0.0;
			}

			/* apply delay model to get appropriate time at the geocenter */
			delay_us = getDelay(d->im, D->scan->nPoly, C->mjd, t, 1.0/ds->nChunk) + evaluateDifxAntennaClock(d->da, C->mjd + t/86400.0) + clockOffset_us;
			t += delay_us*1e-6;
			
			delay_us = getDelay(d->im, D->scan->nPoly, C->mjd, t, 1.0/ds->nChunk) + evaluateDifxAntennaClock(d->da, C->mjd + t/86400.0) + clockOffset_us;
			int_delay_us = (int)delay_us;
			frac_delay_us = delay_us - int_delay_us;
			rate_us_s = getRate(d->im, D->scan->nPoly, C->mjd, t) + evaluateDifxAntennaClockRate(d->da, C->mjd + t/86400.0);

			startSample = c*ds->nSamp;
			desiredSample = startSample + delay_us*1e-6*ds->nSample1sec;
			actualSample = round(desiredSample);
			fracSample = desiredSample - actualSample;

			// calculate delay times frequency modulo 1
			delay_center_samples = int_freq_MHz*frac_delay_us + frac_freq_MHz*int_delay_us + frac_freq_MHz*frac_delay_us;
			delay_center_samples -= round(delay_center_samples);

			copySamples(cs, ds->samps, actualSample, ds->nSamp);

/* 2. additive noise */
			if(d->parameters && d->parameters->switchedPowerFrac > 0.0)
			{
/* 2a. if switched power is requested, add that at the same time */
				long long int index, index2;
				long long int q;
				long long int stop;
				int spState;
				
				index = c * ds->nSamp;
				stop = index + ds->nSamp;
				index2 = 0;

				q = d->parameters->switchedPowerFreq*2*index / ds->nSample1sec;
				spState = 1 - (q % 2);

				while(index2 < stop)
				{
					double s;

					// get next transition
					index2 = ceil(ds->nSample1sec*(q+1)/(2.0*d->parameters->switchedPowerFreq));
					if(index2 > stop)
					{
						index2 = stop;
					}

					s = sqrt(d->SEFD * (1.0 + spState*d->parameters->switchedPowerFrac));
					add_rand_gauss_values(d->random, ds->samps + (index % ds->nSamp), s, index2-index);
					++q;
					index = index2;
					spState = 1-spState;
				}
			}
			else
			{
				add_rand_gauss_values(d->random, ds->samps, sqrt(d->SEFD), ds->nSamp);
			}

/* 3. fringe rotation -- opposite sense as for correlator */
			for(i = 0; i < ds->nSamp; ++i)
			{
				double phi, ft;


				ft = delay_center_samples + (i - 0.5*ds->nSamp)/cs->sampRate * rate_us_s * freq_MHz;
				ft -= round(ft);

				phi = 2*M_PI*ft;
				ds->frSamps[i] = ds->samps[i] * (cos(phi) + I*sin(phi));
			}

/* 4. FFT; size given by fftspecres */
			fftw_execute(ds->c2cPlan);

/* 5. fine delay -- fractional sample correction */
/* 6. apply channel filter (which has FFT scaling compensation built in) */
			if(d->dd->dataSampling == SamplingComplexDSB)
			{
				/* FIXME: not sure this is correct... */
				int ns2, ns4;
				
				ns2 = ds->nSamp/2;
				ns4 = ds->nSamp/4;

				for(i = 0; i <= ns2; ++i)
				{
					double phi;

					phi = 2.0*M_PI*fracSample*(i-ns4)/ds->nSamp;
					ds->spec[i] *= d->filter[i] * (cos(phi) + I*sin(phi));
				}
			}
			else
			{
				for(i = 0; i <= ds->nSamp/2; ++i)
				{
					double phi;

					phi = 2.0*M_PI*fracSample*i/ds->nSamp;
					ds->spec[i] *= d->filter[i] * (cos(phi) + I*sin(phi));
				}
			}

/* 7. iFFT, C->R for real data or C->C for complex data */
			if(d->dd->dataSampling == SamplingComplexDSB)
			{
				/* FIXME: not sure this is correct... */
				int ns2, ns4;

				ns2 = ds->nSamp/2;
				ns4 = ds->nSamp/4;

				/* roll the frequency domain representation by BW/2 */
				for(i = 0; i <= ns2; ++i)
				{
					double complex t;

					t = ds->spec[i];
					ds->spec[i] = ds->spec[i+ns4];
					ds->spec[i+ns4] = t;
				}
			}
			fftw_execute(ds->ifftPlan);

/* 8. apply pulse cal if desired */
			if(d->parameters && d->parameters->pulseCalInterval > 0)
			{
				for(i = 0; i < ds->nSamp; ++i)
				{
					/* note that this is being done with real-valued arrays, but for complex data, both arrays are complex-valued */
					ds->samps[i] += pulseCalSamples[i];
				}
			}

/* 9. place results in 1-second duration array */
			memcpy(ds->samples1sec + startSample, ds->samps, ds->nSamp*sizeof(double));
		}

		if(d->parameters && d->parameters->pulseCalInterval > 0)
		{
			free(pulseCalSamples);
		}

/* 10. "AGC" -- normalize array prior to quantization */
		normalize(ds->samples1sec, ds->nSample1sec);

/* 11. if LSB, flip sign of alternate samples */
		if(ds->df->sideband == 'L')
		{
			int i;

			for(i = 1; i < ds->nSample1sec; i+=2)
			{
				ds->samples1sec[i] = -ds->samples1sec[i];
			}
		}
	}
/* 12. quantize and create VDIF */
	writeVDIF(d, C);
}
