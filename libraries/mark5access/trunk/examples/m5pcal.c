/***************************************************************************
 *   Copyright (C) 2010-2013 by Walter Brisken                             *
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
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include "config.h"
#include <complex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <fftw3.h>
#include <signal.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "m5pcal";
const char author[]  = "Walter Brisken";
const char version[] = "0.6";
const char verdate[] = "20130510";

int ChunkSize = 0;
const int MaxTones = 4096;
const int MaxFreqs = 64;

#define max(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a > _b ? _a : _b; })

int die = 0;

typedef void (*sighandler_t)(int);

sighandler_t oldsiginthand;

void siginthand(int j)
{
	printf("\nBeing killed.  Partial results will be saved.\n\n");
	die = 1;

	signal(SIGINT, oldsiginthand);
}

static void usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("An offline pulse cal extractor.  Can use VLBA, Mark3/4, and Mark5B formats using the\nmark5access library.\n\n");
	printf("Usage: %s [options] <infile> <dataformat> <freq1> [<freq2> ... ] <outfile>\n\n", program);
	printf("  <infile> is the name of the input file\n\n");
	printf("  <dataformat> should be of the form: <FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n");
	printf("    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)\n\n");
	printf("  <freq1> ... is/are the frequencies (MHz) relative to baseband of the first\n");
	printf("      tone to detect; there should be one specified per baseband channel (IF)\n\n");
	printf("  <outfile> is the name of the output file\n\n");
	printf("Options can include:\n\n");
	printf("  --verbose\n");
	printf("  -v           Be more verbose in operation\n\n");
	printf("  --quiet\n");
	printf("  -q           Be quieter\n\n");
	printf("  --help\n");
	printf("  -h           Print this help info and quit\n\n");
	printf("  --chunksize <number>\n");
	printf("  -c <number>  Use a fixed rather than automatic chunk size (6400 in version 0.5).\n\n");
	printf("  -n <number>  Integrate over <number> chunks of data [1000]\n\n");
	printf("  -N <number>  Number of outer loops to perform\n\n");
	printf("  --offset <number>\n");
	printf("  -o <number>  Jump <number> bytes into the file [0]\n\n");
	printf("  --interval <number>\n");
	printf("  -i <number>  Assume a pulse cal comb interval of <number> MHz [1]\n\n");
	printf("  --edge <number>\n");
	printf("  -e <number>  Don't use channels closer than <number> MHz to the edge in delay calc.\n\n");
}

static double calcDelay(int nTone, double *toneFreq_MHz, double *toneAmp, double *tonePhase, double bandCenter, double bandValid)
{
	int i, n=0;
	double d, sum=0.0;
	double ref;
	double window;
	double delay;

	if(nTone < 2)
	{
		return 0.0;
	}

	ref = tonePhase[nTone/2+1] - tonePhase[nTone/2];

	for(i = 1; i < nTone; ++i)
	{
		double f0 = toneFreq_MHz[i-1];
		double f1 = toneFreq_MHz[i];

		if( fabs(f0 - bandCenter) > bandValid ||
		    fabs(f1 - bandCenter) > bandValid )
		{
			continue;
		}

		d = tonePhase[i] - tonePhase[i-1];
		if(d - ref > M_PI)
		{
			d -= 2.0*M_PI;
		}
		if(ref - d > M_PI)
		{
			d += 2.0*M_PI;
		}
		sum += d/(f1 - f0);
		n++;
	}

	if(n < 1)
	{
		return 0;
	}

	window = fabs(500.0/(toneFreq_MHz[nTone/2+1] - toneFreq_MHz[nTone/2]));

	delay = 1000.0*(sum/n)/(2.0*M_PI);

	while(delay > window)
	{
		delay -= 2.0*window;
	}
	while(delay < -window)
	{
		delay += 2.0*window;
	}

	return delay;
}

static double mod2pi(double x)
{
	int n;

	n = (int)(x / (2.0*M_PI));

	return x - n*2.0*M_PI;
}

int gcd(int a, int b)
{
	if (a == 0 || b == 0)
	{
		return max(a,b);
	}
	while (1)
	{
		a = a % b;
		if (a == 0)
		{
			return b;
		}
		b = b % a;
		if (b == 0)
		{
			return a;
		}
	}
	return 1;
}

static int getTones(int freq_kHz, double complex *spectrum, int nChan, double bw_MHz, int interval_MHz, int ns, double *toneFreq_MHz, double *toneAmp, double *tonePhase)
{
	int nTone;
	int startTone;
	int f0_kHz, f1_kHz, df_kHz, f, flsb;
	int bw_kHz;
	int chan;
	complex double z;

	df_kHz = interval_MHz*1000;

	if (abs(freq_kHz) >= abs(df_kHz))
	{
		fprintf(stderr, "Error: tones offsets >= tone interval are not supported\n");

		return 0;
	}

	if(bw_MHz < 0.1)
	{
		fprintf(stderr, "Error: bandwidth below 125 kHz not supported\n");

		return 0;
	}

	bw_kHz = (int)(bw_MHz*1000.0+0.5);

	if(freq_kHz >= 0.0)
	{
		f1_kHz = freq_kHz + bw_kHz;

		/* don't allow a tone at 0 freq */
		startTone = (freq_kHz == 0) ? 1 : 0;

		for(nTone = 0; nTone < MaxTones; nTone++)
		{
			f = (startTone + nTone)*df_kHz + freq_kHz;
			if(f >= f1_kHz)
			{
				break;
			}
			chan = f*nChan/bw_kHz;
			//fprintf(stderr, "Tone %2d with f=%5d in chan %d/%d=%.4f MHz\n", nTone, f, chan, nChan, chan*bw_MHz/nChan);
			z = spectrum[chan];
			toneFreq_MHz[nTone] = f/1000.0;
			toneAmp[nTone] = sqrt(z*~z);
			tonePhase[nTone] = mod2pi(atan2(creal(z),cimag(z)) + 2.0*M_PI*(ns/1000000.0)*(f - freq_kHz));
			/* FIXME -- is the total sign correct above? */
		}
	}
	else
	{
		f0_kHz = -freq_kHz - bw_kHz;

		/* don't allow a tone at 0 freq */
		startTone = (abs(df_kHz) == abs(freq_kHz)) ? 1 : 0;

		for(nTone = 0; nTone < MaxTones; ++nTone)
		{
			f = (startTone - nTone)*df_kHz + freq_kHz;
			if(f <= f0_kHz)
			{
				break;
			}
                        flsb = (startTone + nTone + 1)*df_kHz + freq_kHz;
			chan = abs(flsb)*nChan/bw_kHz;
			//fprintf(stderr, "Tone %2d with f=%5d in chan %d/%d=%.4f MHz\n", nTone, f, chan, nChan, chan*bw_MHz/nChan);
			z = spectrum[chan];
			toneFreq_MHz[nTone] = f/1000.0;
			toneAmp[nTone] = sqrt(z*~z);
			tonePhase[nTone] = mod2pi(atan2(creal(z), cimag(z)) - 2.0*M_PI*(ns/1000000.0)*(f - freq_kHz));
			/* FIXME -- is the total sign correct above? */
		}
	}

	return nTone;
}

static int pcal(const char *inFile, const char *format, int nInt, int nFreq, const int *freq_kHz, const int interval_MHz, const char *outFile, const long long offset, double edge_MHz, const int verbose, const int nDelay)
{
	struct mark5_stream *ms;
	double bw_MHz;
	double **data;
	double complex **bins;
	long long total, unpacked;
	FILE *out;
	int i, j, k, status, N;
	int nTone;
	double toneAmp[MaxTones];
	double tonePhase[MaxTones];
	double toneFreq[MaxTones];
	int DFTlen;
	fftw_plan plan;
	int ns;
	double startSec, stopSec;

	ms = new_mark5_stream_absorb(
		new_mark5_stream_file(inFile, offset),
		new_mark5_format_generic_from_string(format) );

	if(!ms)
	{
		fprintf(stderr, "Error: problem opening %s\n", inFile);

		return EXIT_FAILURE;
	}

	bw_MHz = ms->samprate/2.0e6;
	ns = ms->ns;

	DFTlen = 0;
	for(i = 0; i < nFreq; ++i)
	{
		int DFTlen_curr;
		int foffset = abs(freq_kHz[i]);
		if (foffset == 0)
		{
			DFTlen_curr = 2 * bw_MHz/gcd(interval_MHz, bw_MHz);
		}
		else
		{
			DFTlen_curr = 2 * 1000*bw_MHz / gcd(foffset, 1000*bw_MHz);
		}
		if (DFTlen == 0 || DFTlen == DFTlen_curr)
		{
			DFTlen = DFTlen_curr;
			continue;
		}
		DFTlen = DFTlen*DFTlen_curr / gcd(DFTlen,DFTlen_curr); // LCM(a,b)=a*b/GCD(a,b)
	}
	if (ChunkSize <= 0)
	{
		fprintf(stderr, "Determined optimal DFT length to be %d points over %.2f MHz.\n", DFTlen, bw_MHz);
	}
	else
	{
		fprintf(stderr, "Using a DFT length of %d points (vs. auto-determined %d points) over %.2f MHz.\n", ChunkSize, DFTlen, bw_MHz);
		DFTlen = ChunkSize;
	}

	if(edge_MHz < 0.0)
	{
		edge_MHz = bw_MHz / 16.0;
		printf("Edge band not provided; taking 1/16 of the bandwidth = %f MHz\n", edge_MHz);
	}

	if(verbose > 0)
	{
		mark5_stream_print(ms);
	}

	total = unpacked = 0;

	out = fopen(outFile, "w");
	if(!out)
	{
		fprintf(stderr, "Error: cannot open %s for write\n", outFile);
		delete_mark5_stream(ms);

		return EXIT_FAILURE;
	}

	data = (double **)malloc(ms->nchan*sizeof(double *));
	for(i = 0; i < ms->nchan; ++i)
	{
		data[i] = (double *)malloc(DFTlen*sizeof(double));
	}

	bins = (double complex **)malloc(nFreq*sizeof(double *));
	for(i = 0; i < nFreq; ++i)
	{
		bins[i] = (double complex *)malloc(DFTlen*sizeof(double complex));
	}

	stopSec = ms->sec + ms->ns*1.0e-9;

	for(N = 0; N < nDelay; ++N)
	{
		startSec = stopSec;

		for(i = 0; i < nFreq; ++i)
		{
			memset(bins[i], 0, DFTlen*sizeof(double complex));
		}

		for(k = 0; k < nInt; ++k)
		{
			if(die)
			{
				break;
			}
			status = mark5_stream_decode_double(ms, DFTlen, data);
			
			if(status < 0)
			{
				break;
			}
			else
			{
				total += DFTlen;
				unpacked += status;
			}

			if(ms->consecutivefails > 5)
			{
				printf("Too many failures.  consecutive, total fails = %d %d\n", ms->consecutivefails, ms->nvalidatefail);
				break;
			}
			
			for(i = 0; i < nFreq; ++i)
			{
				for(j = 0; j < DFTlen; ++j)
				{
					bins[i][j] += data[i][j];
				}
			}
		}
		nInt = k;

		if(die)
		{
			break;
		}

		stopSec = ms->sec + ms->ns*1.0e-9 + total/(double)(ms->samprate);

		if(nInt < 1)
		{
			fprintf(stderr, "Error: no samples unpacked\n");
		}
		else
		{
			if(verbose >= -1)
			{
				printf("%Ld / %Ld samples unpacked\n", unpacked, total);
			}

			/* normalize */
			for(i = 0; i < nFreq; ++i)
			{
				for(j = 0; j < DFTlen; ++j)
				{
					bins[i][j] /= nInt;	/* FIXME: correct for FFT size? */
				}
			}

			/* FFT */
			for(i = 0; i < nFreq; ++i)
			{
				double sum = 0.0;
				double factor;

				plan = fftw_plan_dft_1d(DFTlen, bins[i], bins[i], FFTW_FORWARD, FFTW_ESTIMATE);
				fftw_execute(plan);
				fftw_destroy_plan(plan);

				for(j = 0; j < DFTlen/2; ++j)
				{
 				        sum += creal(bins[i][j]*~bins[i][j]);
				}
				factor = 1.0/sqrt(sum);
				for(j = 0; j < DFTlen; ++j)
				{
					bins[i][j] *= factor;
				}
			}

			/* write data out */

			for(i = 0; i < nFreq; ++i)
			{
				double bandCenter, bandValid;
				double f0, f1, delay;

				f0 = fabs(freq_kHz[i]/1000.0);
				f1 = fabs(freq_kHz[i]/1000.0 + bw_MHz);

				bandCenter = 0.5*(f0+f1);
				bandValid = 0.5*fabs(bw_MHz) - edge_MHz;

				nTone = getTones(freq_kHz[i], bins[i], DFTlen/2, bw_MHz, interval_MHz, ns, toneFreq, toneAmp, tonePhase);

				delay = calcDelay(nTone, toneFreq, toneAmp, tonePhase, bandCenter, bandValid);

				if(verbose >= -1)
				{
					printf("Sub-band %d = %f-%f MHz\n\n", i, f0, f1);
				}

				if(nTone < 1)
				{
					printf("  No tones in this band\n\n");
				}
				else
				{
					for(j = 0; j < nTone; ++j)
					{
						if(verbose >= 0)
						{
							printf("  Sample %3d  Tone %2d  Freq=%.3f MHz  Amp=%6.4f  Phase=%6.2f deg\n",
								N, j, toneFreq[j], toneAmp[j], tonePhase[j]*180.0/M_PI);
						}
						fprintf(out, "%d %f %.3f %d %6.4f %6.2f %f\n",
							N, 0.5*(startSec+stopSec), j, toneFreq[j], toneAmp[j], tonePhase[j]*180.0/M_PI, delay);
					}
					if(nTone > 1)
					{
						printf("  t1=%7.5f s  t2=%7.5f s  Freq=%5.3f MHz  Delay=%f ns\n", startSec, stopSec, bandCenter, delay);
					}
					if(verbose >= -1)
					{
						printf("\n");
					}
				}
			}
		}
		fflush(stdout);
	}

	/* Clean up */
	fclose(out);

	delete_mark5_stream(ms);
	for(i = 0; i < ms->nchan; ++i)
	{
		free(data[i]);
	}
	free(data);
	data = 0;

	for(i = 0; i < nFreq; ++i)
	{
		free(bins[i]);
	}
	free(bins);
	bins = 0;

	return 0;
}

int isNumber(const char *str)
{
	int i;
	int nDot=0, nDash=0;

	for(i = 0; str[i]; ++i)
	{
		if(isdigit(str[i]))
		{
			continue;
		}
		else if(str[i] == '.')
		{
			++nDot;
			if(nDot > 1)
			{
				return 0;
			}
		}
		else if(str[i] == '-')
		{
			++nDash;
			if(nDash > 1 || i != 0)
			{
				return 0;
			}
		}
		else
		{
			return 0;
		}
	}

	return 1;
}

int main(int argc, char **argv)
{
	int i;
	int verbose = 0;
	int interval_MHz = 1;
	int nFreq = 0;
	int freq_kHz[MaxFreqs];
	const char *inFile = 0;
	const char *outFile = 0;
	const char *format = 0;
	long long offset = 0LL;
	int nInt = 1000;
	int nDelay = 1;
	double edge_MHz = -1;
	double v;
	int retval;

	oldsiginthand = signal(SIGINT, siginthand);

	for(i = 1; i < argc; ++i)
	{
		if(isNumber(argv[i]))
		{
			if(nFreq >= MaxFreqs)
			{
				fprintf(stderr, "Warning: too many frequencies specified.  Stopping at %d\n", MaxFreqs);
			}
			else
			{
				v = atof(argv[i]);
				if(v >= 0)
				{
					freq_kHz[nFreq] = (int)(v*1000.0 + 0.5);
				}
				else
				{
					freq_kHz[nFreq] = (int)(v*1000.0 - 0.5);
				}
				fprintf(stderr, "freq_kHz[%d] = %d\n", nFreq, freq_kHz[nFreq]);
				nFreq++;
			}
		}
		else if(argv[i][0] == '-')
		{
			if(strcmp(argv[i], "--verbose") == 0 ||
				strcmp(argv[i], "-v") == 0)
			{
				++verbose;
			}
			else if(strcmp(argv[i], "--quiet") == 0 ||
				strcmp(argv[i], "-q") == 0)
			{
				--verbose;
			}
			else if(strcmp(argv[i], "--help") == 0 ||
				strcmp(argv[i], "-h") == 0)
			{
				usage(argv[0]);

				return EXIT_SUCCESS;
			}
			else if(i+1 < argc)
			{
				if(strcmp(argv[i], "-n") == 0)
				{
					++i;
					nInt = atol(argv[i]);
				}
				else if(strcmp(argv[i], "-N") == 0)
				{
					++i;
					nDelay = atol(argv[i]);
				}
				else if(strcmp(argv[i], "--chunksize") == 0 ||
					strcmp(argv[i], "-c") == 0)
				{
					++i;
					ChunkSize = atoi(argv[i]);
				}
				else if(strcmp(argv[i], "--offset") == 0 ||
					strcmp(argv[i], "-o") == 0)
				{
					++i;
					offset = atoll(argv[i]);
				}
				else if(strcmp(argv[i], "--interval") == 0 ||
					strcmp(argv[i], "-i") == 0)
				{
					++i;
					interval_MHz = atol(argv[i]);
				}
				else if(strcmp(argv[i], "--edge") == 0 ||
					strcmp(argv[i], "-e") == 0)
				{
					++i;
					edge_MHz = atof(argv[i]);
				}
				else
				{
					fprintf(stderr, "I'm not sure what to do with command line argument '%s'\n", argv[i]);

					return EXIT_FAILURE;
				}
			}
			else
			{
				fprintf(stderr, "I'm not sure what to do with command line argument '%s'\n", argv[i]);

				return EXIT_FAILURE;
			}

		}
		else if(inFile == 0)
		{
			inFile = argv[i];
		}
		else if(format == 0)
		{
			format = argv[i];
		}
		else if(outFile == 0)
		{
			outFile = argv[i];
		}
		else
		{
			fprintf(stderr, "I'm not sure what to do with command line argument '%s'\n", argv[i]);


			return EXIT_FAILURE;
		}

	}

	if(!outFile || nFreq == 0)
	{
		usage(argv[0]);

		return EXIT_FAILURE;
	}

	retval = pcal(inFile, format, nInt, nFreq, freq_kHz, interval_MHz, outFile, offset, edge_MHz, verbose, nDelay);

	return retval;
}

