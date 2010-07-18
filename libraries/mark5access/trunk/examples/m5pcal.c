/***************************************************************************
 *   Copyright (C) 2010 by Walter Brisken                                  *
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
// $Id: m5spec.c 1989 2010-02-26 17:37:16Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
// $LastChangedRevision: 1989 $
// $Author: WalterBrisken $
// $LastChangedDate: 2010-02-26 10:37:16 -0700 (Fri, 26 Feb 2010) $
//
//============================================================================

#include <complex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <fftw3.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "m5pcal";
const char author[]  = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "2010 Jul 17";

const int ChunkSize = 6400;
const int MaxTones = 64;
const int MaxFreqs = 64;

int usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("An offline pulse cal extractor.  Can use VLBA, Mark3/4, and Mark5B "
		"formats using the\nmark5access library.\n\n");
	printf("Usage: %s [options] <infile> <dataformat> <freq1> [<freq2> ... ] <outfile>\n\n", program);
	printf("  <infile> is the name of the input file\n\n");
	printf("  <dataformat> should be of the form: "
		"<FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n\n");
	printf("  <outfile> is the name of the output file\n\n");

	return 0;
}

static double calcDelay(int nTone, int *toneFreq_MHz, double *toneAmp, double *tonePhase)
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

	for(i = 1; i < nTone; i++)
	{
		d = tonePhase[i] - tonePhase[i-1];
		if(d - ref > M_PI)
		{
			d -= 2*M_PI;
		}
		if(ref - d > M_PI)
		{
			d += 2*M_PI;
		}
		sum += d/(toneFreq_MHz[i] - toneFreq_MHz[i-1]);
		n++;
	}

	window = fabs(500.0/(toneFreq_MHz[nTone/2+1] - toneFreq_MHz[nTone/2]));

	delay = 1000.0*(sum/n)/(2.0*M_PI);

	printf("delay = %f  window = %f\n", delay, window);

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

static int getTones(int freq_kHz, double complex *spectrum, int nChan, double bw_MHz, int interval_MHz, int *toneFreq_MHz, double *toneAmp, double *tonePhase)
{
	int nTone;
	int startTone;
	int f0_kHz, f1_kHz, df_kHz, f;
	int bw_kHz;
	int chan;
	complex double z;

	if(bw_MHz < 0.1)
	{
		fprintf(stderr, "Bandwidth below 125 kHz not supported\n");

		return 0;
	}

	bw_kHz = (int)(bw_MHz*1000.0+0.5);

	if(freq_kHz > 0.0)
	{
		df_kHz = interval_MHz*1000;
		startTone = freq_kHz / df_kHz + 1;
		f1_kHz = freq_kHz + bw_kHz;

		for(nTone = 0; nTone < MaxTones; nTone++)
		{
			f = (startTone + nTone)*df_kHz;
			if(f >= f1_kHz)
			{
				break;
			}
			chan = (f - freq_kHz)*nChan/bw_kHz;
			z = spectrum[chan];
			toneFreq_MHz[nTone] = f/1000.0;
			toneAmp[nTone] = sqrt(z*~z);
			tonePhase[nTone] = atan2(creal(z),cimag(z));
		}
	}
	else
	{
		df_kHz = interval_MHz*1000;
		startTone = -freq_kHz / df_kHz;
		if(startTone * df_kHz == -freq_kHz)
		{
			/* don't allow a tone at 0 freq */
			startTone -= 1;
		}
		f0_kHz = -freq_kHz - bw_kHz;

		for(nTone = 0; nTone < MaxTones; nTone++)
		{
			f = (startTone - nTone)*df_kHz;
			if(f <= f0_kHz)
			{
				break;
			}
			chan = (-freq_kHz - f)*nChan/bw_kHz;
			z = spectrum[chan];
			toneFreq_MHz[nTone] = f/1000.0;
			toneAmp[nTone] = sqrt(z*~z);
			tonePhase[nTone] = atan2(creal(z), cimag(z));
		}
	}

	return nTone;
}

int pcal(const char *inFile, const char *format, int nInt, int nFreq, const int *freq_kHz, int interval_MHz, const char *outFile, long long offset, int verbose)
{
	struct mark5_stream *ms;
	double bw_MHz;
	double **data;
	double complex **bins;
	long long total, unpacked;
	FILE *out;
	int i, j, k, status;
	int nTone;
	double toneAmp[MaxTones];
	double tonePhase[MaxTones];
	int toneFreq[MaxTones];
	fftw_plan plan;

	ms = new_mark5_stream(
		new_mark5_stream_file(inFile, offset),
		new_mark5_format_generic_from_string(format) );

	if(!ms)
	{
		printf("Error: problem opening %s\n", inFile);
		
		return 0;
	}

	bw_MHz = ms->samprate/2.0e6;
	
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

		return 0;
	}

	data = (double **)malloc(ms->nchan*sizeof(double *));
	for(i = 0; i < ms->nchan; i++)
	{
		data[i] = (double *)malloc(ChunkSize*sizeof(double));
	}

	bins = (double complex **)malloc(nFreq*sizeof(double *));
	for(i = 0; i < nFreq; i++)
	{
		bins[i] = (double complex *)calloc(ChunkSize, sizeof(double complex));
	}

	for(k = 0; k < nInt; k++)
	{
		status = mark5_stream_decode_double(ms, ChunkSize, data);
		
		if(status < 0)
		{
			break;
		}
		else
		{
			total += ChunkSize;
			unpacked += status;
		}

		if(ms->consecutivefails > 5)
		{
			printf("Too many failures.  consecutive, total fails = %d %d\n", ms->consecutivefails, ms->nvalidatefail);
			break;
		}
		
		for(i = 0; i < nFreq; i++)
		{
			for(j = 0; j < ChunkSize; j++)
			{
				bins[i][j] += data[i][j];
			}
		}
	}
	nInt = k;

	/* read stage cleanup */
	delete_mark5_stream(ms);
	for(i = 0; i < ms->nchan; i++)
	{
		free(data[i]);
	}
	free(data);
	data = 0;


	if(nInt < 1)
	{
		fclose(out);
		fprintf(stderr, "Error: no samples unpacked\n");
	}
	else
	{
		printf("%Ld / %Ld samples unpacked\n", unpacked, total);

		/* normalize */
		for(i = 0; i < nFreq; i++)
		{
			for(j = 0; j < ChunkSize; j++)
			{
				bins[i][j] /= nInt;	/* FIXME: correct for FFT size? */
			}
		}

		/* FFT */
		for(i = 0; i < nFreq; i++)
		{
			plan = fftw_plan_dft_1d(ChunkSize, bins[i], bins[i], FFTW_FORWARD, FFTW_ESTIMATE);
			fftw_execute(plan);
			fftw_destroy_plan(plan);
		}

		/* write data out */

		for(i = 0; i < nFreq; i++)
		{
			double f0, f1, delay;

			f0 = fabs(freq_kHz[i]/1000.0);
			f1 = fabs(freq_kHz[i]/1000.0 + bw_MHz);

			nTone = getTones(freq_kHz[i], bins[i], ChunkSize/2, bw_MHz, interval_MHz, 
				toneFreq, toneAmp, tonePhase);

			delay = calcDelay(nTone, toneFreq, toneAmp, tonePhase);

			printf("Sub-band %d = %f-%f MHz\n\n", i, f0, f1);

			if(nTone < 1)
			{
				printf("  No tones in this band\n\n");
			}
			else
			{
				if(verbose >= 0)
				{
					for(j = 0; j < nTone; j++)
					{
						printf("  Tone %2d  Freq=%d MHz  Amp=%6.4f  Phase=%+6.2f deg\n",
							j, toneFreq[j], toneAmp[j], tonePhase[j]*180/M_PI);
						fprintf(out, "%d %d %6.4f %+6.2f\n",
							j, toneFreq[j], toneAmp[j], tonePhase[j]*180/M_PI);
					}
				}
				if(nTone > 1)
				{
					printf("  Delay = %f ns\n", delay);
				}
				printf("\n");
			}
		}

		fclose(out);
	}

	/* Clean up */
	for(i = 0; i < nFreq; i++)
	{
		free(bins[i]);
	}
	free(bins);
	bins = 0;

	return nInt;
}

int isNumber(const char *str)
{
	int i;
	int nDot=0, nDash=0;

	for(i = 0; str[i]; i++)
	{
		if(isdigit(str[i]))
		{
			continue;
		}
		if(str[i] == '.')
		{
			nDot++;
			if(nDot > 1)
			{
				return 0;
			}
		}
		if(str[i] == '-')
		{
			nDash++;
			if(nDash > 1)
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
	double v;

	for(i = 1; i < argc; i++)
	{
		if(isNumber(argv[i]))
		{
			if(nFreq >= MaxFreqs)
			{
				fprintf(stderr, "Warning: too many frequencies specified.  Stopping at %d\n",
					MaxFreqs);
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
				nFreq++;
			}
		}
		else if(argv[i][0] == '-')
		{
			if(strcmp(argv[i], "--verbose") == 0 ||
				strcmp(argv[i], "-v") == 0)
			{
				verbose++;
			}
			if(strcmp(argv[i], "--quiet") == 0 ||
				strcmp(argv[i], "-q") == 0)
			{
				verbose--;
			}
			else if(strcmp(argv[i], "--help") == 0 ||
				strcmp(argv[i], "-h") == 0)
			{
				return usage(argv[0]);
			}
			else if(i+1 < argc)
			{
				if(strcmp(argv[i], "-n") == 0)
				{
					i++;
					nInt = atol(argv[i]);
				}
				else if(strcmp(argv[i], "--offset") == 0 ||
					strcmp(argv[i], "-o") == 0)
				{
					i++;
					offset = atoll(argv[i]);
				}
				else if(strcmp(argv[i], "--interval") == 0 ||
					strcmp(argv[i], "-i") == 0)
				{
					i++;
					interval_MHz = atol(argv[i]);
				}
				else
				{
					fprintf(stderr, "I'm not sure what to do with command line argument <%s>\n",
						argv[i]);

					return 0;
				}
			}
			else
			{
				fprintf(stderr, "I'm not sure what to do with command line argument <%s>\n",
					argv[i]);

				return 0;
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
			fprintf(stderr, "I'm not sure what to do with command line argument <%s>\n",
				argv[i]);


			return 0;
		}

	}

	if(!outFile || nFreq == 0)
	{
		return usage(argv[0]);
	}

	pcal(inFile, format, nInt, nFreq, freq_kHz, interval_MHz, outFile, offset, verbose);

	return 0;
}

