/***************************************************************************
 *   Copyright (C) 2008-2017 by Walter Brisken, Jan Wagner & Chris Phillips *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: m5spec.c 6687 2015-06-03 02:14:10Z JanWagner $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
// $LastChangedRevision: 6687 $
// $Author: JanWagner $
// $LastChangedDate: 2015-06-03 11:14:10 +0900 (Wed, 03 Jun 2015) $
//
//============================================================================

// Useful thoughs at http://www.katjaas.nl/FFTwindow/FFTwindow&filtering.html

#define OUTPUT_BITS 2             // desired quantization in output VDIF file, options are 2, 8, or 32 (2-bit VDIF encoding, 8-bit linear encoding, or 32-bit float)
#define DEFAULT_IDFT_LEN 128      // default number of points to place accross extractable narrowband signal
#define STDDEV_MIN_SAMPLES 8192   // minimum number of output time domain samples to use in determining 'sigma' for 2-bit re-quantization
#define USE_C2C_IDFT     0        // 1 to use complex-to-complex inverse DFT, 0 to use complex-to-real inverse DFT (faster, less tested)
#define FFTW_FLAGS  FFTW_ESTIMATE // FFTW_ESTIMATE or FFTW_MEASURE or FFTW_PATIENT

#ifdef __GNUC__
	#define RESTRICT __restrict__
#else
	#define RESTRICT
#endif

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))
#define _FILE_OFFSET_BITS 64

#include "../mark5access/mark5_stream.h"

#include <assert.h>
#include <fcntl.h>
#include <math.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <complex.h>
#include <fftw3.h>
typedef float fftw_real;

const char program[] = "m5subband";
const char author[]  = "Jan Wagner";
const char version[] = "1.2";
const char verdate[] = "20170426";

static uint32_t m_VDIF_refep_MJDs[] =
{
    // http://www.csgnetwork.com/julianmodifdateconv.html
    // Base epoch nr 0x00 = 0:00 UT 01 Jan 2000 = 51544 = base +  0 months (1st January 2000)
    // Next epoch nr 0x01 = 0:00 UT 01 Jul 2000 = 51726 = base +  6 months (1st July    2000)
    // Next epoch nr 0x02 = 0:00 UT 01 Jan 2001 = 51910 = base + 12 months (1st January 2001)
    // ...
    // Matlab code, maybe also Octave, to generate this table:
    //    yy = 2000:2050; mm = [1 7];
    //    for y=yy, for m=mm, fprintf(1, '%d, ', mjuliandate(y,m,1)), end, end;
    51544, 51726, 51910, 52091, 52275, 52456, 52640, 52821, 53005, 53187, 53371, 53552, 53736,
    53917, 54101, 54282, 54466, 54648, 54832, 55013, 55197, 55378, 55562, 55743, 55927, 56109,
    56293, 56474, 56658, 56839, 57023, 57204, 57388, 57570, 57754, 57935, 58119, 58300, 58484,
    58665, 58849, 59031, 59215, 59396, 59580, 59761, 59945, 60126, 60310, 60492, 60676, 60857,
    61041, 61222, 61406, 61587, 61771, 61953, 62137, 62318, 62502, 62683, 62867, 63048, 63232,
    63414, 63598, 63779, 63963, 64144, 64328, 64509, 64693, 64875, 65059, 65240, 65424, 65605,
    65789, 65970, 66154, 66336, 66520, 66701, 66885, 67066, 67250, 67431, 67615, 67797, 67981,
    68162, 68346, 68527, 68711, 68892, 69076, 69258, 69442, 69623, 69807, 69988
};

enum WindowFunction { Cosine=0, Hann=1, Boxcar=2 };
static const char* WindowFunctionNames[3] = { "cosine", "Hann", "boxcar" };

typedef struct VDIFEncapsulator_tt {
	int fd;
	char* fmt;
	uint32_t hdr[8];
	size_t payloadbytes;
	uint32_t framenr;
	uint32_t framesec;
	uint32_t refepoch;
	double fps;
	int writepos;
} VDIFEncapsulator_t;

typedef struct FilterConfig_tt {
	int if_nr;
	int factor;
	int no_lead;
	int no_tail;
	int discard_incomplete_on_close;
	float start_MHz;
	float stop_MHz;
	int npoints; // points to place accross the start_MHz--stop_MHz range
	enum WindowFunction winfunc;
} FilterConfig_t;

int vdifencap_open(VDIFEncapsulator_t* t, int rate_Mbps);
int vdifencap_close(VDIFEncapsulator_t* t, int discard_partial);
int vdifencap_updateheader(VDIFEncapsulator_t* t);
int vdifencap_copyheader(const char* fn, VDIFEncapsulator_t* t);
int vdifencap_write(VDIFEncapsulator_t* t, const char* data, size_t len);
int vdifencap_MJDsec_to_RefEpSec(const uint32_t MJD, const uint32_t sec_of_day, uint32_t *ref_epoch, uint32_t* sec_of_epoch);

void generate_window_Hann(fftw_real *wf, int L);
void generate_window_cosine(fftw_real *wf, int L);
void generate_window_boxcar(fftw_real *wf, int L);

/////////////////////////////////////////////////////////////////////////////////////////////
// Signals (Ctrl-C)
/////////////////////////////////////////////////////////////////////////////////////////////

volatile int die = 0;

struct sigaction old_sigint_action;

void siginthand(int j)
{
	printf("\nBeing killed.  Partial results will be saved.\n\n");
	die = 1;

	sigaction(SIGINT, &old_sigint_action, 0);
}


/////////////////////////////////////////////////////////////////////////////////////////////
// Helper Functions
/////////////////////////////////////////////////////////////////////////////////////////////

static void usage()
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("A Mark5 time domain filter. Extracts a narrow subband from a wideband recording.\n\n");
	printf("Can use VLBA, Mark3/4, and Mark5B formats using the mark5access library.\n\n");
	printf("Usage : m5subband [--refmjd=<n>] [--wf=Hann|cos|box] [--npts=<n>] [--trunc]\n");
	printf("                  [--no-leading|--leading] [--no-tailing|--tailing]\n");
	printf("                  <infile> <dataformat> <outfile> <if_nr> <qf> <f0> <f1> [<offset>]\n\n");
	printf("Optional parameters:\n\n");
	printf("  --refmjd=<n> resolve ambiguity of 3-digit MJD of Mark5B (default: 57000)\n");
	printf("  --wf=<n> choose pre- and post-filtering window function (default: cos)\n");
	printf("  --npts=<n> to choose number of DFT points across extractable subband (default: %d)\n", DEFAULT_IDFT_LEN);
	printf("  --trunc to discard incomplete frame when output file is closed, zero-pad otherwise\n");
	printf("  --[no-]leading to discard/keep leading part of filter response, valid for qf>1\n");
	printf("  --[no-]tailing to discard/keep tailing part of filter response, valid for qf>1\n\n");
	printf("Arguments:\n\n");
	printf("  <infile> is the name of the input file\n\n");
	printf("  <dataformat> should be of the form: <FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n");
	printf("    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)\n\n");
	printf("  <outfile> is the output VDIF file for the extracted subband\n\n");
	printf("  <if_nr> is the IF to process (1 is the first recorded IF)\n\n");
	printf("  <qf> is the quality factor (1 default, >=2 to reduce spectral leakage)\n\n");
	printf("  <f0> is the low edge (in MHz) of the subband to filter out\n\n");
	printf("  <f1> is the high edge (in MHz) of the subband to filter out\n\n");
	printf("  <offset> is number of bytes into file to start decoding\n\n");
}

/** Return next power of 2 from n */
int next_pow2(int n)
{
	int e = ceilf(log2f((float)n));
	return 1 << e;
}

/* Return next even number from n */
int next_even(int n)
{
	return n + n%2;
}

/** Return 1 if number is a power of 2 */
int is_pow2(int n)
{
	return ((n & (n - 1)) == 0);
}

/** Return 1 if floating point nr is an integer */
int is_integer(float n)
{
	return (ceilf(n) == n);
}

/** Calculate standard deviation of data */
float stddev(const float* v, const size_t N)
{
	float mean = 0.0f, M2 = 0.0f;
	size_t i;
	for (i = 0; i < N; i++)
	{
		float delta, delta2;
		delta = v[i] - mean;
		mean += delta / (i + 1);
		delta2 = v[i] - mean;
		M2 += delta*delta2;
	}
	return sqrtf(M2 / (N-1));
}

/////////////////////////////////////////////////////////////////////////////////////////////
// Window-Overlap Helpers
/////////////////////////////////////////////////////////////////////////////////////////////

/** Read new data into 'raw' array and appends data from 'raw[if_nr]' to output 'out'
 * after first removing the oldest data still present in 'out'. The amount of new data
 * that is appended this way depends on 'factor', factor 1 : 100% new data (Ldft new samples),
 * factor 2 : 50% new data (retain recent Ldft/2 samples and append Ldft/2 new samples),
 * factor 4 : 25% new data (retain recent 3*Ldft/4 samples and append Ldft/4 new samples).
 * Finally applies the weights in vector 'w' to 'out' to produce a weighted 'wout' vector.
 */
#if defined __GNUC__ && !defined __clang__
__attribute__((optimize("unroll-loops")))
#endif
int windowed_mk5_read(struct mark5_stream *ms, const int Ldft, float **raw,
    const int if_nr, const int factor, float * RESTRICT out, const fftw_real * RESTRICT w, fftw_real * RESTRICT wout,
    const enum WindowFunction wtype)
{
	const int_fast32_t nunroll = 8;
	int_fast32_t n, k;

	// Read new data; default to zeroes if EOF
	int rc = mark5_stream_decode(ms, Ldft/factor, raw);
	if (rc < 0)
	{
		//printf("Hit input file EOF, padding with zero-valued samples\n");
		memset(raw[if_nr], 0x00, Ldft*sizeof(float));
	}

	// Append new data into output
	if (factor <= 1)
	{
		memcpy(out, raw[if_nr], Ldft*sizeof(float));
	}
	else
	{
		int nnew = Ldft/factor;
		int nold = Ldft - nnew;
		memmove(out, out + nnew, nold*sizeof(float));       // discard oldest samples
		memcpy(out + nold, raw[if_nr], nnew*sizeof(float)); // append new samples
	}

	// Window the data
	if (wtype == Boxcar)
	{
		memcpy(wout, out, Ldft*sizeof(float));
		return rc;
	}
	for (n = 0; (n + nunroll) < Ldft; n += nunroll)
	{
		for (k = 0; k < nunroll; k++)
		{
			wout[n+k] = w[n+k] * out[n+k];
		}
	}
	for (; n < Ldft; n++)
	{
		wout[n] = w[n] * out[n];
	}

	return rc;
}

/** Write a piece of data into a VDIF output file.
 * The intended use is for having 'nsamp == Lidft/factor' samples in 'in',
 * to write the most recent fully completed time domain data.
 */
void requantize_into_vdif(VDIFEncapsulator_t* vdif, const float * RESTRICT in, float sigma, unsigned char * RESTRICT tmp, const int nsamp)
{
	int_fast32_t n;

	// Store 32-bit / 8-bit / 2-bit
#if OUTPUT_BITS==32
	if (vdifencap_write(vdif, (const char*)in, nsamp*sizeof(fftw_real)) < 0) // VDIF format
	//if (write(fdout, out_td, Lidft*sizeof(fftw_real)) < 0) // headerless format
	{
		perror("write");
	}
#elif OUTPUT_BITS==8
	float scale = 8.0/sigma;
	for (n = 0; n < nsamp; n++)
	{
		tmp[n] = in[n] * scale;
	}
	if (vdifencap_write(vdif, (const char*)tmp, nsamp*sizeof(char)) < 0) // VDIF format
	//if (write(fdout, out_td, Lidft*sizeof(fftw_real)) < 0) // headerless format
	{
		perror("write");
	}
#elif OUTPUT_BITS==2
	// See https://science.nrao.edu/facilities/vlba/publications/memos/sci/index/sci09memo.pdf
	sigma *= 0.9816;
	assert((nsamp % 4) == 0);
	for (n = 0; n < nsamp; n += 4)
	{
		float v1, v2, v3, v4;
		unsigned char enc1, enc2, enc3, enc4;
		v1 = in[n+0];
		v2 = in[n+1];
		v3 = in[n+2];
		v4 = in[n+3];

		// 2-bit : 00 = -VHi, 10=-VLo, 01=+VLo, 11=+VHi for Mark5B
		// 2-bit : 00 = -VHi, 01=-VLo, 10=+VLo, 11=+VHi for VDIF
		if (v1 >= sigma) { enc1 = 0b11; }
		else if (v1 >= 0) { enc1 = 0b10; }
		else if (v1 >= -sigma) { enc1 = 0b01; }
		else { enc1 = 0b00; }

		if (v2 >= sigma) { enc2 = 0b11; }
		else if (v2 >= 0) { enc2 = 0b10; }
		else if (v2 >= -sigma) { enc2 = 0b01; }
		else { enc2 = 0b00; }

		if (v3 >= sigma) { enc3 = 0b11; }
		else if (v3 >= 0) { enc3 = 0b10; }
		else if (v3 >= -sigma) { enc3 = 0b01; }
		else { enc3 = 0b00; }

		if (v4 >= sigma) { enc4 = 0b11; }
		else if (v4 >= 0) { enc4 = 0b10; }
		else if (v4 >= -sigma) { enc4 = 0b01; }
		else { enc4 = 0b00; }

		tmp[n/4] = (enc4<<6) | (enc3<<4) | (enc2<<2) | enc1;
		//tmp[n/4] = (enc1<<6) | (enc2<<4) | (enc3<<2) | enc4;
	}
	if (vdifencap_write(vdif, (const char*)tmp, nsamp/4) < 0) // VDIF format
	//if (write(fdout, out_td, Lidft/4) < 0) // headerless format
	{
		perror("write");
	}
#else
	printf("Error: unsupported OUTPUT_BITS of %d\n", OUTPUT_BITS);
	exit(1);
#endif
}

#if defined __GNUC__ && !defined __clang__
__attribute__((optimize("unroll-loops")))
#endif
void window_and_accumulate_c2r(fftw_real * RESTRICT out, const fftwf_complex * RESTRICT in,
    const int Lidft, const fftw_real phasor_re, const fftw_real * RESTRICT w)
{
	const int_fast32_t nunroll = 8;
	int_fast32_t n, k;
	for (n = 0; (n + nunroll) < Lidft; n += nunroll)
	{
		for (k = 0; k < nunroll; k++)
		{
			out[n+k] += phasor_re * creal(in[n+k]) * w[n+k];
		}
	}
	for ( ; n < Lidft; n++)
	{
		out[n] += phasor_re * creal(in[n]) * w[n];
	}
}

#if defined __GNUC__ && !defined __clang__
__attribute__((optimize("unroll-loops")))
#endif
void window_and_accumulate_r2r(fftw_real * RESTRICT out, const fftw_real * RESTRICT in,
    const int Lidft, const fftw_real phasor_re, const fftw_real * RESTRICT w)
{
	const int_fast32_t nunroll = 8;
	int_fast32_t n, k;
	for (n = 0; (n + nunroll) < Lidft; n += nunroll)
	{
		for (k = 0; k < nunroll; k++)
		{
			out[n+k] += phasor_re * in[n+k] * w[n+k];
		}
	}
	for ( ; n < Lidft; n++)
	{
		out[n] += phasor_re * in[n] * w[n];
	}
}

/////////////////////////////////////////////////////////////////////////////////////////////
// Window Functions
/////////////////////////////////////////////////////////////////////////////////////////////

/** Generate a Hann window (double application pre-FFT then post-IDFT produces Hann-squared) */
void generate_window_Hann(fftw_real *wf, int L)
{
	int i;
	for (i = 0; i < L; i++)
	{
		float w = (2*M_PI*i) / (L - 1.0f);
		wf[i] = 0.5f - 0.5f * cosf(w);
	}
}

/** Generate a cosine window (double application pre-FFT then post-IDFT produces cosine-squared) */
void generate_window_cosine(fftw_real *wf, int L)
{
	int i;
	for (i = 0; i < L; i++)
	{
		float w = (M_PI/L) * (i - 0.5f*(L-1.0f));
		wf[i] = cosf(w);
	}
}

/** Generate a boxcar window (double application pre-FFT then post-IDFT produces unity) */
void generate_window_boxcar(fftw_real *wf, int L)
{
	int i;
	for (i = 0; i < L; i++)
	{
		wf[i] = 1.0f;
	}
}

/////////////////////////////////////////////////////////////////////////////////////////////
// Filtering Function
/////////////////////////////////////////////////////////////////////////////////////////////

int filterRealData(const char* infile, struct mark5_stream *ms, const int fdout, const FilterConfig_t* cfg)
{
	const int if_nr = cfg->if_nr;
	const int factor = cfg->factor;
	float start_MHz = cfg->start_MHz;
	float stop_MHz = cfg->stop_MHz;
	char fmtstring[64];

	float bw_in_MHz, bw_out_MHz, df_MHz, R_Mbps;
	float r, rfrac, rot_f_re;
	int start_bin, stop_bin;
	int Ldft, Lcopy, Lidft;
	int i, rc, report_interval;

	size_t niter = 0, nidft = 0, ntailing = 0;
	struct timeval t1, t2, tstart, tstop;

	VDIFEncapsulator_t vdif;
	const int nbit_out = OUTPUT_BITS;
	int mjd, sec;
	double nsec;

	float **raw;
	fftw_real *wf_analysis, *wf_resynthesis;
	fftw_real *in_raw, *dft_in, *out_td;
	fftwf_complex *dft_out, *idft_in, *idft_out;
	fftwf_plan plan_fwd;
#if USE_C2C_IDFT
	fftwf_plan plan_inv;
#else
	fftwf_plan plan_inv_c2r;
#endif
	fftw_real *sigma_data;
	int sigma_nsamples = 0, min_sigma_nsamples;
	float sigma = 1.0f;

	char *out_converter_tmp;

	if ((factor < 1) || (factor > 16))
	{
		printf("Error: quality factor (%d) must be between 1 and 16\n", factor);
		return -1;
	}

	// Bandwidths
	bw_in_MHz = floorf(ms->samprate * 0.5e-6);
	bw_out_MHz = fabsf(stop_MHz - start_MHz);

	// Determine transform sizes: fix IDFT length, pad it, then adjust DFT length
	Lcopy = cfg->npoints;                     // #bins to copy from DFT complex out into IDFT input
	//Lidft = next_pow2(2*(Lcopy-Lcopy%2));   // zero-pad Lcopy such that time-domain output data will be somewhat oversampled
	Lidft = next_even(2*(Lcopy-Lcopy%2));     // zero-pad Lcopy that time-domain output data will be closer to critically sampled
	Ldft  = 2.0*bw_in_MHz/(bw_out_MHz/Lcopy); // #bins total in large DFT

        // Determine DFT output region to extract (bins) and find actual MHz range
        // note: 'Ldft'-point r2c FFT produces spectrum in first 'Ldft/2+1' complex output bins
	df_MHz     = 2.0*bw_in_MHz / Ldft;
	start_bin  = (int)(start_MHz / df_MHz);
        stop_bin   = start_bin + Lcopy;
	start_MHz  = start_bin*df_MHz;
	stop_MHz   = stop_bin*df_MHz;
	bw_out_MHz = fabsf(stop_MHz - start_MHz);
	R_Mbps     = 2*bw_out_MHz*nbit_out;
	min_sigma_nsamples = MAX(8*Lidft*factor, STDDEV_MIN_SAMPLES);
	if (!is_integer(R_Mbps) || !is_pow2(R_Mbps))
	{
		printf("Error: output bandwidth (%.3f MHz) gives non-2^n rate (%.3f Mbps), not supported by DiFX!\n", bw_out_MHz, R_Mbps);
		return -1;
	}

	// Make sure DFT and IDFT lengths suitable for data overlapping
	assert( factor * (int)(Ldft/factor) == Ldft /* catch rounding errors */ );
	assert( factor * (int)(Lidft/factor) == Lidft /* catch rounding errors */ );
	if (factor * (int)(Ldft/factor) != Ldft)
	{
		printf("Error: Ldft=%d not evenly divisible by factor=%d!\n", Ldft, factor);
		return -1;
	}
	if (factor * (int)(Lidft/factor) != Lidft)
	{
		printf("Error: Lidft=%d not evenly divisible by factor=%d!\n", Lidft, factor);
		return -1;
	}

	// Coefficient for coherent phase connection between overlapped input segments
	r = ((float)start_bin)/((float)factor);
	rfrac = r - floorf(r);

	// Allocate data buffers and (I)DFT areas
	raw = (float **)malloc(ms->nchan*sizeof(float *));
	for (i = 0; i < ms->nchan; ++i)
	{
		raw[i] = malloc(sizeof(float)*Ldft); // or Ldft/factor
	}
	in_raw = malloc(sizeof(float)*Ldft);
	out_converter_tmp = malloc(sizeof(char)*2*Lidft);
	dft_in = fftwf_malloc(sizeof(fftw_real)*Ldft);
	dft_out = fftwf_malloc(sizeof(fftwf_complex)*(Ldft/2+1));
	idft_in = fftwf_malloc(sizeof(fftwf_complex)*Lidft+8);
	idft_out = fftwf_malloc(sizeof(fftwf_complex)*Lidft+8);
	out_td = fftwf_malloc(sizeof(fftw_real)*2*Lidft);
	sigma_data = fftwf_malloc(sizeof(fftw_real)*min_sigma_nsamples);
	memset(dft_in, 0x00, sizeof(fftw_real)*Lidft);
	memset(dft_out, 0x00, sizeof(fftwf_complex)*(Lidft/2+1));
	memset(out_td, 0x00, sizeof(fftw_real)*2*Lidft);
	memset(sigma_data, 0x00, sizeof(fftw_real)*min_sigma_nsamples);
	// TODO: use fftw_plan_many_dft for a notable speed improvement?

	// Window functions
	wf_analysis = fftwf_malloc(sizeof(fftw_real)*Ldft);
	wf_resynthesis = fftwf_malloc(sizeof(fftw_real)*Lidft);
	switch (cfg->winfunc)
	{
		case Cosine:
			generate_window_cosine(wf_analysis, Ldft);
			generate_window_cosine(wf_resynthesis, Lidft);
			break;
		case Hann:
			generate_window_Hann(wf_analysis, Ldft);
			generate_window_Hann(wf_resynthesis, Lidft);
			break;
		case Boxcar:
			generate_window_boxcar(wf_analysis, Ldft);
			generate_window_boxcar(wf_resynthesis, Lidft);
			break;
	}

	// Make sure we start at an integer second in the input file
	niter = 0;
	while (1)
	{
		mark5_stream_get_sample_time(ms, &mjd, &sec, &nsec);
		if (nsec == 0)
		{
			break;
		}
		rc = mark5_stream_decode(ms, 1, raw);
		if (rc < 0)
		{
			return -1;
		}
		niter++;
	}

	// Create output file and set correct time
	vdifencap_open(&vdif, R_Mbps);
	vdif.fd = fdout;
	vdifencap_MJDsec_to_RefEpSec(mjd, sec, &vdif.refepoch, &vdif.framesec);
	vdifencap_updateheader(&vdif);

	// Reporting
	report_interval = (0.050 * (2*bw_in_MHz*1e6) * factor) / Ldft;
	report_interval = MAX(report_interval, 100);
	printf("report_interval = %d\n", report_interval);
	snprintf(fmtstring, sizeof(fmtstring)-1, "VDIF_%zu-%.0f-1-%d", vdif.payloadbytes, R_Mbps, nbit_out);
	fmtstring[sizeof(fmtstring)-1] = '\0';
	printf("%-14s : first integer second (MJD %d sec %d) found after %zd samples.\n", "Input file", mjd, sec, niter);
	printf("%-14s : %s with %.1f frames/s at %.3f Mbps\n", "Output format", fmtstring, vdif.fps, R_Mbps);
	printf("%-14s : MJD %.3f is VDIF reference epoch %d\n", "Output time", mjd + sec/86400.0, vdif.refepoch);
	#if USE_C2C_IDFT
	printf("%-14s : %d-pt r2c DFT, take %d bins (%d...%d), %d-pt c2c IDFT, %s window, %.1f deg phase/IDFT\n", "Configuration",
		Ldft, Lcopy, start_bin, stop_bin, Lidft, WindowFunctionNames[cfg->winfunc], 360.0*rfrac
	);
	#else
	printf("%-14s : %d-pt r2c DFT, take %d bins (%d...%d), %d-pt c2r IDFT, %s window, %.1f deg phase/IDFT\n", "Configuration",
		Ldft, Lcopy, start_bin, stop_bin, Lcopy, WindowFunctionNames[cfg->winfunc], 360.0*rfrac
	);
	#endif
	printf("%-14s : start at effective %.3f MHz, stop at %.3f MHz, qf=%d\n", "Extraction", start_MHz, stop_MHz, factor);
	printf("%-14s : keep leading=%d, keep tailing=%d, keep incomplete last frame=%d\n", "Options",
		!cfg->no_lead, !cfg->no_tail, !cfg->discard_incomplete_on_close
	);
	printf("%-14s : FFTSpecRes=%.6e format=%s\n", "DiFX v2d", df_MHz, fmtstring);

	// Prepare (I)DFT plans
	printf("Preparing FFTW plans...\n");
	plan_fwd = fftwf_plan_dft_r2c_1d(Ldft, dft_in, dft_out, FFTW_FLAGS);
	#if USE_C2C_IDFT
	plan_inv = fftwf_plan_dft_1d(Lidft, idft_in, idft_out, FFTW_BACKWARD, FFTW_FLAGS);
	#else
	plan_inv_c2r = fftwf_plan_dft_c2r_1d(Lidft, idft_in, (fftw_real*)idft_out, FFTW_FLAGS);
	#endif

	// Process raw input data
	niter = 0;
	gettimeofday(&t1, NULL);
	tstart = t1;
	printf("Filtering...\n");
	while (!die)
	{
		// Append new data to 'in_raw' and window the entire Ldft-long sample vector into 'dft_in'
		rc = windowed_mk5_read(ms, Ldft, raw, if_nr, factor, in_raw, wf_analysis, dft_in, cfg->winfunc);
		if (rc < 0)
		{
			ntailing++;
			if (cfg->no_tail || (!cfg->no_tail && (ntailing >= factor)))
			{
				// Stop when tailing data (zero-pad after EOF) has gone through filtering process
				break;
			}
		}

		// Transform r2c Ldft reals from 'dft_in' into Ldft/2+1 complex in 'dft_out'
		fftwf_execute(plan_fwd);

		// Copy desired bin range into input of zero-padded IDFT
		memcpy(idft_in, dft_out + start_bin, sizeof(fftwf_complex) * (Lcopy + 1));

		// What to do with DC idft_in[0] and Nyquist idft_in[Lcopy] points?
		//idft_in[0] = creal(idft_in[0]); idft_in = creal(idft_in[Lcopy]); // retain the information
		idft_in[0] = 0; idft_in[Lcopy] = 0; // erase the information

		// Weight of current output
		rot_f_re = cos(2.0*M_PI*rfrac*((double)nidft)); // TODO: could use periodicity on w to improve numerical precision at large #nidft
		//printf(" i=%zd r=%.3f\n   ", nidft, rot_f_re);
		nidft++;

		// Inverse transform idft_in --> idft_out
		#if USE_C2C_IDFT
		fftwf_execute(plan_inv);     // Lidft complex (with zero-padding past Ldft/2+1) to Lidft complex
		#else
		fftwf_execute(plan_inv_c2r); // Lidft/2+1 complex to Lidft reals
		#endif

		// Window and add
		#if USE_C2C_IDFT
		window_and_accumulate_c2r(out_td, idft_out, Lidft, rot_f_re, wf_resynthesis);
		#else
		window_and_accumulate_r2r(out_td, (fftw_real*)idft_out, Lidft, rot_f_re, wf_resynthesis);
		#endif

		// Calculate standard deviation
		if ((sigma_nsamples < min_sigma_nsamples) && (!cfg->no_lead || (cfg->no_lead && (nidft >= factor))))
		{
			int nappend = MIN(min_sigma_nsamples-sigma_nsamples, Lidft/factor);
			memcpy(sigma_data + sigma_nsamples, out_td, sizeof(fftw_real) * nappend);
			sigma_nsamples += nappend;
			sigma = stddev(sigma_data, sigma_nsamples);
			if (sigma_nsamples >= min_sigma_nsamples)
			{
				printf("%-14s : %d-bit, stddev=%.2f from %d samples\n", "Quantizer", nbit_out, sigma, sigma_nsamples);
			}
		}

		// Store completed samples as 32/8/2-bit
		if (!cfg->no_lead || (cfg->no_lead && (nidft >= factor)))
		{
			requantize_into_vdif(&vdif, out_td, sigma, (unsigned char*)out_converter_tmp, Lidft/factor);
		}

		// Advance the output data overlap
		if (factor > 1)
		{
			int nnew = Lidft/factor;
			int nold = Lidft - nnew;
			memmove(out_td, out_td + nnew, nold*sizeof(fftwf_complex));
			memset(out_td + nold, 0x00, nnew*sizeof(fftwf_complex));
		}
		else
		{
			memset(out_td, 0x00, Lidft*sizeof(fftwf_complex));
		}

		// Status reports
		niter++;
		if ((niter % report_interval) == 0)
		{
			double dt;
			mark5_stream_get_sample_time(ms, &mjd, &sec, &nsec);
			gettimeofday(&t2, NULL);
			dt = (t2.tv_sec - t1.tv_sec) + 1e-6*(t2.tv_usec - t1.tv_usec);
			printf("input at %dd %.4fs : CPU %.2f Ms/s\n", mjd, nsec*1e-9+sec, 1e-6*report_interval*(Ldft/dt)/factor);
			t1 = t2;
		}

	}

	vdifencap_close(&vdif, cfg->discard_incomplete_on_close);

	gettimeofday(&tstop, NULL);
	if (1)
	{
		double dt = (tstop.tv_sec - tstart.tv_sec) + 1e-6*(tstop.tv_usec - tstart.tv_usec);
		printf("Finished in %.1f seconds, total throughput %.2f Ms/s.\n", dt, 1e-6*nidft*(Ldft/dt)/factor);
		printf("Use format %s to decode the output VDIF file.\n", fmtstring);
	}

	return 0;
}


/////////////////////////////////////////////////////////////////////////////////////////////
// VDIF Re-Encapsulation
/////////////////////////////////////////////////////////////////////////////////////////////

/** Create a new VDIF file */
int vdifencap_open(VDIFEncapsulator_t* t, int rate_Mbps)
{
	t->framenr = 0;
	t->framesec = 0;
	t->refepoch = 0;
	t->payloadbytes = 8000;
	t->writepos = 0;
	t->fps = (rate_Mbps*1e6/8.0) / t->payloadbytes;
	while ((floor(t->fps) != t->fps) || ((t->payloadbytes % 8) != 0))
	{
		t->payloadbytes++;
		t->fps = (rate_Mbps*1e6/8.0) / t->payloadbytes;
	}
	t->hdr[0] = 0; // word 0: [Invalid(1) | Legacy(1) | Seconds from ref epoch(30)]
	t->hdr[1] = 0; // word 1: [none(2)  | RefEp(6) | Data Frame#(24)]
	t->hdr[2] = (t->payloadbytes + 32)/8;   // word 2: [Version(3) | log2 Nch(5) | Framelen(24) in 8-byte units with 32-byte header included]
	t->hdr[3] = (OUTPUT_BITS-1)<<26;        // word 3: [Complex(1) | bit/sample-1 (5) | Thread ID(10) | Station ID(16)]
	t->hdr[4] = 0; // # words 4 to 8: extended user data
	t->hdr[5] = 0;
	t->hdr[6] = 0;
	t->hdr[7] = 0;
	return 0;
}

/** Copy all fields of a reference VDIF header */
int vdifencap_copyheader(const char* fn, VDIFEncapsulator_t* t)
{
	uint32_t hdr[8];
	int fd = open(fn, O_RDONLY);
	if (read(fd, hdr, 8*sizeof(uint32_t)) < 0)
	{
		perror("reading VDIF input file");
		return -1;
	}
	t->hdr[0] = hdr[0] & ((1ULL<<31)-1); // copy Seconds from rec epoch
	t->hdr[1] = hdr[1] & 0x3F000000;     // copy RefEpoch
	close(fd);
	return 0;
}

/** Update internal VDIF template header timestamp seconds and epoch and frame-nr */
int vdifencap_updateheader(VDIFEncapsulator_t* t)
{
	t->hdr[0] = t->framesec;
	t->hdr[1] = ((t->refepoch << 24) & 0x3F000000) | (t->framenr & 0x00FFFFFF);
	return 0;
}

/** Increment internal VDIF timestamp */
int vdifencap_inc_header(VDIFEncapsulator_t* t)
{
	t->framenr++;
	if (t->framenr >= t->fps)
	{
		t->framenr = 0;
		t->framesec++;
	}
	vdifencap_updateheader(t);
	return 0;
}

/** Append data into VDIF */
int vdifencap_write(VDIFEncapsulator_t* t, const char* data, size_t len)
{
	size_t i = 0, nremain, nfit, nwrite;
	ssize_t rc;
	while (i < len)
	{
		if (t->writepos == 0)
		{
			write(t->fd, t->hdr, sizeof(t->hdr));
			vdifencap_inc_header(t);
		}
		nremain = len - i;
		nfit = t->payloadbytes - t->writepos;
		nwrite = (nremain < nfit) ? nremain : nfit;
		rc = write(t->fd, (void*)(&data[i]), nwrite);
		if (rc < 0)
		{
			return rc;
		}
		i += nwrite;
		t->writepos += nwrite;
		if (t->writepos >= t->payloadbytes)
		{
			t->writepos = 0;
		}
	}
	return len;
}

/** Close and truncate; either discard last incomplete frame (discard_partial=1) or pad it with zeroes (discard_partial=0)  */
int vdifencap_close(VDIFEncapsulator_t* t, int discard_partial)
{
	if (t->writepos < t->payloadbytes)
	{
		off_t sz;
		sz = lseek(t->fd, -(t->writepos + sizeof(t->hdr)), SEEK_CUR);
		if (sz > 0)
		{
			if (discard_partial == 1)
			{
				ftruncate(t->fd, sz);
			}
			else
			{
				ftruncate(t->fd, sz + t->payloadbytes + sizeof(t->hdr));
			}
		}
	}
	close(t->fd);
	return 0;
}

int vdifencap_MJDsec_to_RefEpSec(const uint32_t MJD, const uint32_t sec_of_day, uint32_t *ref_epoch, uint32_t* sec_of_epoch)
{
	const size_t Nentries = sizeof(m_VDIF_refep_MJDs) / sizeof(uint32_t);
	uint32_t re;

	if ((MJD < 51544) || (ref_epoch == NULL) || (sec_of_epoch == NULL))
	{
		return -1;
	}

	// Reference epoch = number of 6-month periods passed since 01/01/2000
	re = (MJD - 51544) / (366/2);
	if (re > Nentries)
	{
		return -1;
	}
	*ref_epoch = re;

	// Second in this reference epoch
	*sec_of_epoch = (MJD - m_VDIF_refep_MJDs[re]) * 86400 + sec_of_day;

	return 0;
}


/////////////////////////////////////////////////////////////////////////////////////////////
// MAIN
/////////////////////////////////////////////////////////////////////////////////////////////

int main(int argc, char **argv)
{
	struct mark5_stream *ms;
	long long offset = 0;
	int fdout;
	char *infile, *format, *outfile;
	int refmjd = 57000;
	int retval;
	struct sigaction new_sigint_action;

	FilterConfig_t fcfg;
	fcfg.npoints = DEFAULT_IDFT_LEN;
	fcfg.winfunc = Cosine;
	fcfg.no_lead = 1;
	fcfg.no_tail = 1;
	fcfg.discard_incomplete_on_close = 0;

	// Optional parameters
	while ((argc > 1) && argv[1][0]=='-')
	{
		if (strncmp(argv[1], "--refmjd=", 9) == 0)
		{
			refmjd = atoi(argv[1] + 9);
		}
		else if (strncmp(argv[1], "--wf=", 5) == 0)
		{
			if (strcasecmp(argv[1]+5, "hann") == 0)
			{
				fcfg.winfunc = Hann;
			}
			else if (strcasecmp(argv[1]+5, "box") == 0)
			{
				fcfg.winfunc = Boxcar;
			}
			else if (strcasecmp(argv[1]+5, "cos") == 0)
			{
				fcfg.winfunc = Cosine;
			}
			else
			{
				printf("Warning: unknown --wf argument '%s'\n", argv[1]+5);
			}
		}
		else if (strncmp(argv[1], "--npts=", 7) == 0)
		{
			fcfg.npoints = atoi(argv[1]+7);
		}
		else if (strncmp(argv[1], "--trunc", 7) == 0)
		{
			fcfg.discard_incomplete_on_close = 1;
		}
		else if (strncmp(argv[1], "--no-leading", 12) == 0)
		{
			fcfg.no_lead = 1;
		}
		else if (strncmp(argv[1], "--leading", 9) == 0)
		{
			fcfg.no_lead = 0;
		}
		else if (strncmp(argv[1], "--no-tailing", 12) == 0)
		{
			fcfg.no_tail = 1;
		}
		else if (strncmp(argv[1], "--tailing", 9) == 0)
		{
			fcfg.no_tail = 0;
		}
		argc--;
		argv++;
	}

	// Required args
	if (argc < 8)
	{
		usage();
		return EXIT_FAILURE;
	}

	infile = argv[1];
	format = argv[2];
	outfile = argv[3];
	fcfg.if_nr = atoi(argv[4]) - 1;
	fcfg.factor = atoi(argv[5]);
	fcfg.start_MHz = atof(argv[6]);
	fcfg.stop_MHz = atof(argv[7]);

	// Optional args
	if (argc > 8)
	{
		offset = atoll(argv[8]);
	}

	ms = new_mark5_stream_absorb(
		new_mark5_stream_file(infile, offset),
		new_mark5_format_generic_from_string(format) );
	if (!ms)
	{
		printf("Error: problem opening %s\n", infile);

		return EXIT_FAILURE;
	}

	if (refmjd > 0)
	{
		//printf("Changing reference MJD to %d\n", refmjd);
		mark5_stream_fix_mjd(ms, refmjd);
	}

	mark5_stream_print(ms);

	// Check that args are reasonable
	if ((fcfg.start_MHz > ms->samprate/2.0e6) || (fcfg.stop_MHz > ms->samprate/2.0e6))
	{
		printf("Error: extraction range %.3f--%.3f MHz falls outside recorded bandwidth of %.3f MHz!",
			fcfg.start_MHz, fcfg.stop_MHz, ms->samprate/2.0e6);

		return EXIT_FAILURE;
	}
	if ((fcfg.if_nr < 0) || (fcfg.if_nr > (ms->nchan-1)))
	{
		printf("Error: IF number (%d) must be between %d and %d\n", fcfg.if_nr+1, 1, ms->nchan);

		return EXIT_FAILURE;
	}
	if ((fcfg.winfunc == Boxcar) && (fcfg.factor != 1))
	{
		printf("Warning: windowing disabled (boxcar selected) so setting qf to 1\n");
		fcfg.factor = 1;
	}
	if ((fcfg.factor < 0) || (fcfg.factor > 16))
	{
		fcfg.factor = 1;
	}
	if ((fcfg.npoints < 16) || (fcfg.npoints & 1))
	{
		printf("Error: number of points accross narrowband signal must be even and >=16\n");
		return EXIT_FAILURE;
	}

	fdout = open(outfile, O_CREAT|O_TRUNC|O_WRONLY, S_IWUSR|S_IRUSR|S_IRGRP|S_IROTH);

	new_sigint_action.sa_handler = siginthand;
	sigemptyset(&new_sigint_action.sa_mask);
	new_sigint_action.sa_flags = 0;
	sigaction(SIGINT, &new_sigint_action, &old_sigint_action);

	retval = filterRealData(infile, ms, fdout, &fcfg);

	return retval;
}

