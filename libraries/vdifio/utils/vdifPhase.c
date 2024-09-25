#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <stdint.h>
#include <complex.h>
#include <math.h>
#include <fftw3.h>
#include "vdifio.h"

const char program[] = "vdifPhase";
const char author[] = "Walter Brisken <wbrisken@nrao.edu>";
const char version[] = "0.2.1";
const char verdate[] = "20240430";

const int defaultBits = 2;


volatile int die = 0;

struct sigaction old_sigint_action;

void siginthand(int j)
{
	fprintf(stderr, "\nBeing killed.  Partial results will be saved.\n\n");
	die = 1;

	sigaction(SIGINT, &old_sigint_action, 0);
}

void usage()
{
	printf("\n%s ver. %s  %s  %s\n\n", program, version, verdate, author);
	printf("Usage: %s <fileName> <frameSize> <bandwidth> <thread> <frames> [<nBit> [<nominalFreq>] ]\n\n", program);
	printf("<fileName> is name of file to read\n\n");
	printf("<frameSize> is the full VDIF frame size in bytes\n\n");
	printf("<bandwidth> is the channel bandwidth in MHz\n\n");
	printf("<thread> is the VDIF thread ID to process\n\n");
	printf("<frames> is the number of consecutive frames to bundle prior to FFT\n\n");
	printf("<nBit> is the number of bits per sample (default is %d)\n\n", defaultBits);
	printf("<nominalFreq> [Hz] is the nominal signal frequency, if known.\n\n");
	printf("This program goes through a VDIF file and looks for <frames> consecutive\n");
	printf("Frames (without gaps or invalid) of specified <thread> and looks for the\n");
	printf("strongest spectral feature.  It interpolates to get a frequency, amplitude\n");
	printf("and phase for that feature.  These values go to stdout with the following\n");
	printf("columns:\n");
	printf("  1. Time (seconds) since start of VDIF stream\n");
	printf("  2. Interpolated channel number containing feature\n");
	printf("  3. Frequency (MHz; from LO side of channel) of the feature\n");
	printf("  4. Power of feature, relative to noise\n");
	printf("  5. Phase (radians) of feature\n\n");
}


/*
 * The following function interpolates a spectrum with a mathematically correct
 * formulation assuming the underlying signal is a pure tone.  The argumemt
 * should be a pointer to an element of an array that is a local peak.
 * Both neighboring elements must be accessible.  The return value
 * will be in the range [-0.5, 0.5], represting the array index, relative
 * to the provided pointer corresponding to the actual frequency.
 */
double interpolateSpectrumPeak(const double complex *s)
{
	double dota, dotc;
	double b, r;

	dota = - creal(s[0]) * creal(s[-1]) - cimag(s[0]) * cimag(s[-1]);
	dotc = - creal(s[0]) * creal(s[1]) - cimag(s[0]) * cimag(s[1]);
	b = cabs(s[0]);
	if(dotc > dota)
	{
		r = cabs(s[1])/b;

		return r/(1.0+r);
	}
	else
	{
		r = cabs(s[-1])/b;

		return -r/(1.0+r);
	}
}

double cnorm(double complex v)
{
	return creal(v)*creal(v) + cimag(v)*cimag(v);
}

/* t is some time [sec], to pass through to a print statement */
void processSpectrum(const double complex *spectrum, int nChan, int bw, double t, double *psd, double deltaPhi, int f)
{
	const int edge = 4;
	static double *power = 0;
	int c;
	double max_power = 0;
	int max_chan = 0;
	double dc;
	double freq;	/* [MHz] interpolated frequency of peak */
	double phase;	/* [rad] */
	double totalPower = 0.0;

	if(power == 0)
	{
		power = (double *)malloc(nChan*sizeof(double));
	}

	for(c = 0; c < nChan; ++c)
	{
		power[c] = creal(spectrum[c] * ~spectrum[c])/(2*nChan);
		psd[c] += power[c];
		totalPower += power[c];
	}
	/* exclude DC from total power */
	totalPower -= power[0];

	for(c = edge; c < nChan-edge ; ++c)
	{
		if(power[c] > max_power)
		{
			max_power = power[c];
			max_chan = c;
		}
	}

	dc = interpolateSpectrumPeak(spectrum + max_chan);

	if(dc > 0.0)
	{
		double complex v;
		double r;

		r = cnorm(spectrum[max_chan+1])/cnorm(spectrum[max_chan]);
		v = spectrum[max_chan] + r*spectrum[max_chan+1];
		phase = carg(v);
	}
	else if(dc < 0.0)
	{
		double complex v;
		double r;

		r = cnorm(spectrum[max_chan-1])/cnorm(spectrum[max_chan]);
		v = spectrum[max_chan] + r*spectrum[max_chan-1];
		phase = carg(v);
	}
	else
	{
		phase = carg(spectrum[max_chan]);
	}

	if(dc != 0.0)
	{
		double f;

		f = M_PI*dc/sin(M_PI*dc);
		max_power *= f*f;	/* correct peak value for sinc^2() roll-off */
	}

	freq = bw*(max_chan+dc)/nChan;

	phase = (phase - deltaPhi)/(2.0*M_PI) + 12;
	phase -= (int)phase;
	phase *= (2.0*M_PI);


	printf("%8.6f %f %f %f %f %d  %d %f %f %f %f  %f %f  %f %f  %f %f  %f\n", t, max_chan+dc, freq, max_power, phase, f % 8, 
		max_chan, dc, 
		cabs(spectrum[max_chan-1]), cabs(spectrum[max_chan]), cabs(spectrum[max_chan+1]),
		creal(spectrum[max_chan-1]), cimag(spectrum[max_chan-1]),
		creal(spectrum[max_chan]), cimag(spectrum[max_chan]),
		creal(spectrum[max_chan+1]), cimag(spectrum[max_chan+1]),
		totalPower);
}

/******* Some VDIF routines that should be made available in the library... *******/
/* (adapted from mark5access) */

#define OPTIMAL_2BIT_HIGH 3.3359

static const float HiMag = OPTIMAL_2BIT_HIGH;
static const float FourBit1sigma = 2.95;

static float lut1bit[256][8];
static float lut2bit[256][4];
static float lut4bit[256][2];
static float lut8bit[256];

static void initluts()
{
	/* Warning: these are different than for VLBA/Mark4/Mark5B! */
	const float lut2level[2] = {-1.0, 1.0};
	const float lut4level[4] = {-HiMag, -1.0, 1.0, HiMag};
	const float lut16level[16] = {-8/FourBit1sigma,-7/FourBit1sigma,-6/FourBit1sigma,-5/FourBit1sigma,-4/FourBit1sigma,
				      -3/FourBit1sigma,-2/FourBit1sigma,-1/FourBit1sigma,0,1/FourBit1sigma,2/FourBit1sigma,
				      3/FourBit1sigma,4/FourBit1sigma,5/FourBit1sigma,6/FourBit1sigma,7/FourBit1sigma};
	int b, i, l;
	
	for(b = 0; b < 256; b++)
	{
		/* lut1bit */
		for(i = 0; i < 8; i++)
		{
			l = (b>>i) & 0x01;
			lut1bit[b][i] =  lut2level[l];
		}

		/* lut2bit */
		for(i = 0; i < 4; i++)
		{
			l = (b >> (2*i)) & 0x03;
			lut2bit[b][i] = lut4level[l];
		}

		/* lut4bit */
		for(i = 0; i < 2; i++)
		{
			l = (b >> (4*i)) & 0x0F;
			lut4bit[b][i] = lut16level[l];
		}

		/* lut8bit */
		lut8bit[b] = (b-128)/3.3;	/* This scaling mimics 2-bit data if 8 bit RMS==~10 */
	}
}

static void vdif_decode_1bit_double(const unsigned char *src, int n, double *dest)
{
	int o, i;

	for(o = i = 0; o < n; o++)
	{
		const float *fp;
		
		fp = lut1bit[src[i]];
		i++;

		dest[o] = fp[0];
		o++;
		dest[o] = fp[1];
		o++;
		dest[o] = fp[2];
		o++;
		dest[o] = fp[3];
		o++;
		dest[o] = fp[4];
		o++;
		dest[o] = fp[5];
		o++;
		dest[o] = fp[6];
		o++;
		dest[o] = fp[7];
	}
}

static void vdif_decode_2bit_double(const unsigned char *src, int n, double *dest)
{
	int o, i;

	for(o = i = 0; o < n; o++)
	{
		const float *fp;
		
		fp = lut2bit[src[i]];
		i++;

		dest[o] = fp[0];
		o++;
		dest[o] = fp[1];
		o++;
		dest[o] = fp[2];
		o++;
		dest[o] = fp[3];
	}
}

static void vdif_decode_4bit_double(const unsigned char *src, int n, double *dest)
{
	int o, i;

	for(o = i = 0; o < n; o++)
	{
		const float *fp;
		
		fp = lut4bit[src[i]];
		i++;

		dest[o] = fp[0];
		o++;
		dest[o] = fp[1];
	}
}

static void vdif_decode_8bit_double(const unsigned char *src, int n, double *dest)
{
	int o;

	for(o = 0; o < n; o++)
	{
		dest[o] = lut8bit[src[o]];
	}
}

static void vdif_decode_16bit_double(const unsigned char *src, int n, double *dest)
{
	const uint16_t *src16;
	int o;

	src16 = (const uint16_t *)src;

	for(o = 0; o < n; o++)
	{
		dest[o] = (src16[o] - 32768)/8.0;
	}
}

/***************************************************************/

int run(const char *fileName, int frameSize, int bw, int thread, int frames, int nBit, int64_t nomFreq)
{
	FILE *in;
	FILE *out;
	int nRec = 0;			/* number of records (FFT blocks) processed */
	int nDiscard = 0;		/* number of VDIF frames discarded (incomplete bundles) */
	int nInvalid = 0;
	int nWrongThread = 0;
	int nWrongBits = 0;
	int nWrongSize = 0;
	int samplesPerFrame;
	int nSamp;			/* number of real samples going into FFT */
	int frameRate;			/* [frames per sec] */
	double *samples;		/* length = nSamp */
	double complex *spectrum;	/* length = nSamp/2 + 1 */
	double *psd;			/* power spectral density */
	fftw_plan plan = 0;
	char *readBuffer;
	const vdif_header *vh;
	const unsigned char *data;
	int f = 0;				/* next frame number (of frames) to fill */
	int lastSecond = -1;		/* from VDIF header */
	int lastFrame = -1;		/* from VDIF header */
	int nRead;
	int startSeconds = -1;
	void (*decode)(const unsigned char *src, int n, double *dest);
	int c;
	double dp;

	switch(nBit)
	{
	case 1:
		decode = vdif_decode_1bit_double;
		break;
	case 2:
		decode = vdif_decode_2bit_double;
		break;
	case 4:
		decode = vdif_decode_4bit_double;
		break;
	case 8:
		decode = vdif_decode_8bit_double;
		break;
	case 16:
		decode = vdif_decode_16bit_double;
		break;
	default:
		fprintf(stderr, "Error: %d bits per sample is not supported.\n", nBit);
		exit(EXIT_FAILURE);
	}

	samplesPerFrame = (frameSize-32)*8/nBit;
	nSamp = frames*samplesPerFrame;
	frameRate = 2000000LL*bw/samplesPerFrame;

	readBuffer = (char *)malloc(frameSize);
	vh = (const vdif_header *)readBuffer;
	data = (const unsigned char *)(readBuffer + 32);

	samples = (double *)fftw_malloc(nSamp * sizeof(double));
	spectrum = (double complex *)fftw_malloc((nSamp/2 + 1) * sizeof(double complex));
	psd = (double *)calloc(nSamp/2 + 1, sizeof(double));

	dp = (nomFreq % frameRate)/(double)frameRate;

	if(strcmp(fileName, "-") == 0)
	{
		in = stdin;
	}
	else
	{
		in = fopen(fileName, "r");
		if(!in)
		{
			fprintf(stderr, "Error: cannot open %s for read\n", fileName);

			exit(0);
		}
	}

	for(nRead = 0; !die; ++nRead)
	{
		size_t r;

		r = fread(readBuffer, frameSize, 1, in);
		if(r != 1)
		{
			break;
		}
		if(vh->invalid)
		{
			++nInvalid;
			continue;
		}
		if(vh->threadid != thread)
		{
			++nWrongThread;
			continue;
		}
		if(vh->nbits+1 != nBit)
		{
			++nWrongBits;
			continue;
		}
		if(vh->framelength8 * 8 != frameSize)
		{
			++nWrongSize;
			continue;
		}

		if(startSeconds < 0)
		{
			startSeconds = vh->seconds;
		}

		if(f > 0)
		{
			if(vh->frame == 0)
			{
				if(lastFrame != frameRate - 1 || lastSecond != vh->seconds-1)
				{
					nDiscard += f;
					f = 0;
				}
			}
			else
			{
				if(lastFrame != vh->frame - 1 || lastSecond != vh->seconds)
				{
					nDiscard += f;
					f = 0;
				}
			}
		}
		lastSecond = vh->seconds;
		lastFrame = vh->frame;

		decode(data, samplesPerFrame, samples + f*samplesPerFrame);

		++f;

		/* do FFT? */
		if(f == frames)
		{
			double t;
			double deltaPhi;

			if(plan == 0)
			{
				if(vh->iscomplex)
				{
					plan = fftw_plan_dft_1d(nSamp/2, (fftw_complex *)samples, spectrum, FFTW_FORWARD, FFTW_ESTIMATE);
				}
				else
				{
					plan = fftw_plan_dft_r2c_1d(nSamp, samples, spectrum, FFTW_ESTIMATE);
				}
			}

			fftw_execute(plan);

			t = vh->seconds - startSeconds + (vh->frame + 1 - 0.5*frames)/frameRate;

			deltaPhi = 2.0*M_PI*((nomFreq * vh->frame) % frameRate)/(double)frameRate;

			processSpectrum(spectrum, nSamp/2, bw, t, psd, deltaPhi, vh->frame);

			f = 0;

			++nRec;
		}
	}

	out = fopen("spec.vdifPhase", "w");
	for(c = 0; c < nSamp/2; ++c)
	{
		fprintf(out, "%d %f %f\n", c, 2.0*c*bw/nSamp, psd[c]/nRec);
	}
	fclose(out);


	/* print some summary information */
	fprintf(stderr, "Frame rate: %d\n", frameRate);
	fprintf(stderr, "Samples per frame: %d\n", samplesPerFrame);
	fprintf(stderr, "Phase granularity: %f turns = %f radians\n", dp, dp*2.0*M_PI);
	fprintf(stderr, "Frames read: %d\n", nRead);
	fprintf(stderr, "Good records created: %d\n", nRec);
	fprintf(stderr, "Frames marked invalid: %d\n", nInvalid);
	fprintf(stderr, "Frames with wrong thread: %d\n", nWrongThread);
	fprintf(stderr, "Frames with wrong bits: %d\n", nWrongBits);
	fprintf(stderr, "Frames of wrong size: %d\n", nWrongSize);
	fprintf(stderr, "Frames discarded (from incomplete bundling): %d\n", nDiscard);

	if(plan)
	{
		fftw_destroy_plan(plan);
	}
	fftw_free(spectrum);
	fftw_free(samples);
	free(psd);
	free(readBuffer);

	if(in != stdin)
	{
		fclose(in);
	}

	return nRec;
}



int main(int argc, char **argv)
{
	const char *fileName = 0;
	int frameSize = 0;
	int bw = 0;
	int thread = -1;
	int frames = 0;
	int nBit = -1;
	struct sigaction new_sigint_action;
	int a;
	int64_t nomFreq = -1;

	initluts();

	if(argc == 1)
	{
		usage();

		exit(EXIT_SUCCESS);
	}

	for(a = 1; a < argc; ++a)
	{
		if(argv[a][0] == '-' && strlen(argv[a]) > 1)
		{
			if(strcmp(argv[a], "-h") == 0 || strcmp(argv[a], "--help") == 0)
			{
				usage();

				exit(EXIT_SUCCESS);
			}
		}
		else if(fileName == 0)
		{
			fileName = argv[a];
		}
		else if(frameSize <= 0)
		{
			frameSize = atoi(argv[a]);
		}
		else if(bw <= 0)
		{
			bw = atoi(argv[a]);
		}
		else if(thread < 0)
		{
			thread = atoi(argv[a]);
		}
		else if(frames <= 0)
		{
			frames = atoi(argv[a]);
		}
		else if(nBit <= 0)
		{
			nBit = atoi(argv[a]);
		}
		else if(nomFreq < 0)
		{
			nomFreq = atoll(argv[a]);
		}
		else
		{
			fprintf(stderr, "Unexpected command line parameter: %s\n", argv[a]);

			exit(EXIT_FAILURE);
		}
	}

	if(nBit <= 0)
	{
		nBit = defaultBits;
	}

	if(nomFreq < 0)
	{
		nomFreq = 0;
	}

	if(frames <= 0)
	{
		fprintf(stderr, "Error: Incomplete command line.\n");

		exit(EXIT_FAILURE);
	}

	new_sigint_action.sa_handler = siginthand;
	sigemptyset(&new_sigint_action.sa_mask);
	new_sigint_action.sa_flags = 0;
	sigaction(SIGINT, &new_sigint_action, &old_sigint_action);

	run(fileName, frameSize, bw, thread, frames, nBit, nomFreq);

	return 0;
}
