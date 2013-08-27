/***************************************************************************
 *   Copyright (C) 2010-2011 by Walter Brisken                             *
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
#include <stdio.h>
#include <complex.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <fftw3.h>
#include <math.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "zerocorr";
const char author[]  = "Walter Brisken";
const char version[] = "0.2";
const char verdate[] = "20110926";

const int MaxLineLen = 256;

int die = 0;

typedef void (*sighandler_t)(int);

void siginthand(int j)
{
	printf("\nBeing killed\n");
	die = 1;
}


typedef struct
{
	struct mark5_stream *ms;
	double **data;
	fftw_complex *zdata, *spec;
	fftw_plan plan;
	double deltaF;

	char *inputFile;
	char *dataFormat;
	long long offset;
	int subBand;
	int fftSize;
	int startChan;
	int nChan;
} DataStream;

typedef struct
{
	DataStream *ds1, *ds2;
	char *confFile;
	int nChan;
	fftw_complex *visibility;
	fftw_complex *lags;
	fftw_plan plan;
	double *ac1, *ac2;
	FILE *outVis;
	FILE *outLag;
	double deltaF, deltaT;

	char *visFile;
	char *lagFile;
	int nFFT;
} Baseline;

void deleteDataStream(DataStream *ds);
void deleteBaseline(Baseline *B);


void stripEOL(char *str)
{
	int i;
	int lastGood = 0;

	for(i = 0; str[i]; i++)
	{
		if(str[i] == '#')
		{
			break;
		}
		if(str[i] > ' ')
		{
			lastGood = i;
		}
	}

	str[lastGood+1] = 0;
}


DataStream *newDataStream(FILE *in)
{
	const int NItem = 7;
	DataStream *ds;
	char buffer[NItem][MaxLineLen+1];
	int i;
	char *v;

	ds = (DataStream *)calloc(1, sizeof(DataStream));

	for(i = 0; i < NItem; i++)
	{
		v = fgets(buffer[i], MaxLineLen, in);
		if(!v)
		{
			deleteDataStream(ds);
			
			return 0;
		}
		stripEOL(buffer[i]);
	}

	ds->inputFile = strdup(buffer[0]);
	ds->dataFormat = strdup(buffer[1]);
	ds->subBand = atoi(buffer[2]);
	ds->offset = atoll(buffer[3]);
	ds->fftSize = atoi(buffer[4]);
	ds->startChan = atoi(buffer[5]);
	ds->nChan = atoi(buffer[6]);
	if(ds->startChan < 0 || ds->startChan > ds->fftSize/2 ||
	   (ds->nChan > 0 && (ds->startChan + ds->nChan) > ds->fftSize/2) ||
	   (ds->nChan < 0 && (ds->startChan + ds->nChan) < -1))
	{
		printf("The start channel must be in 0 .. %d, inclusive, and\n"
		       "the number of channels to keep must be as well:\n"
		       "For file %s\n"
		       "you have %d < 0 or %d > %d with %d channels to keep\n",
			ds->fftSize/2, ds->inputFile, ds->startChan,
			ds->startChan, ds->fftSize/2, ds->nChan);

		deleteDataStream(ds);
		ds = 0;

		return 0;
	}

	ds->ms = new_mark5_stream_absorb(
		new_mark5_stream_file(ds->inputFile, ds->offset),
		new_mark5_format_generic_from_string(ds->dataFormat) );

	if(!ds->ms)
	{
		printf("problem opening %s\n", ds->inputFile);

		deleteDataStream(ds);
		ds = 0;
		
		return 0;
	}

	mark5_stream_print(ds->ms);

	ds->data = (double **)calloc(ds->ms->nchan, sizeof(double *));
	for(i = 0; i < ds->ms->nchan; i++)
	{
		ds->data[i] = (double *)calloc(ds->fftSize+2, sizeof(double));
	}
	ds->zdata = (fftw_complex *)calloc(ds->fftSize/2+2, sizeof(fftw_complex));
	ds->spec = (fftw_complex *)calloc(abs(ds->nChan), sizeof(fftw_complex));
	ds->plan = fftw_plan_dft_r2c_1d(ds->fftSize, ds->data[ds->subBand], ds->zdata, FFTW_ESTIMATE);

	ds->deltaF = (double)(ds->ms->samprate)/(double)(ds->fftSize);

	return ds;
}

void deleteDataStream(DataStream *ds)
{
	int i;

	if(ds)
	{
		if(ds->ms)
		{
			if(ds->data)
			{
				for(i = 0; i < ds->ms->nchan; i++)
				{
					if(ds->data[i])
					{
						free(ds->data[i]);
						ds->data[i] = 0;
					}
				}
				free(ds->data);
				ds->data = 0;
			}
			delete_mark5_stream(ds->ms);
			ds->ms = 0;
		}
		if(ds->zdata)
		{
			free(ds->zdata);
			ds->zdata = 0;
		}
		if(ds->spec)
		{
			free(ds->spec);
			ds->spec = 0;
		}
		if(ds->inputFile)
		{
			free(ds->inputFile);
			ds->inputFile = 0;
		}
		if(ds->dataFormat)
		{
			free(ds->dataFormat);
			ds->inputFile = 0;
		}
		if(ds->plan)
		{
			fftw_destroy_plan(ds->plan);
			ds->plan = 0;
		}
	}
}

int feedDataStream(DataStream *ds)
{
	int i, status;
	double scale;

	status = mark5_stream_decode_double(ds->ms, ds->fftSize, ds->data);

	if(status < 0)
	{
		return status;
	}

	fftw_execute(ds->plan);

	scale = 1.0/(ds->fftSize);

	if(ds->nChan > 0)
	{
		for(i = 0; i < ds->nChan; i++)
		{
			ds->spec[i] = ds->zdata[ds->startChan+i]*scale;
		}
	}
	else
	{
		for(i = 0; i < -ds->nChan; i++)
		{
			/* FIXME : I think this conjugation is needed! -WFB 20100806 */
			ds->spec[i] = ~ds->zdata[ds->startChan-i]*scale;
		}
	}

	return 0;
}

void printDataStream(const DataStream *ds)
{
	printf("  DataStream [%p]\n", ds);
	if(!ds)
	{
		return;
	}
	printf("    input file = %s\n", ds->inputFile);
	printf("    data format = %s\n", ds->dataFormat);
	printf("    file offset = %Ld\n", ds->offset);
	printf("    sub band to process = %d\n", ds->subBand);
	printf("    fft size = %d\n", ds->fftSize);
	printf("    start channel = %d\n", ds->startChan);
	printf("    number of channels to keep = %d\n", ds->nChan);
	printf("    deltaF = %f Hz\n", ds->deltaF);
}

Baseline *newBaseline(const char *confFile)
{
	Baseline *B;
	FILE *in;
	const int NItem = 3;
	char buffer[NItem][MaxLineLen+1];
	int i;
	char *v;

	in = fopen(confFile, "r");
	if(!in)
	{
		fprintf(stderr, "Cannot open conf file %s\n", confFile);
	}

	B = (Baseline *)calloc(1, sizeof(Baseline));
	B->confFile = strdup(confFile);
	B->ds1 = newDataStream(in);
	B->ds2 = newDataStream(in);

	if(!B->ds1 || !B->ds2)
	{
		deleteBaseline(B);

		fclose(in);

		return 0;
	}

	if(B->ds1->ms->sec != B->ds2->ms->sec ||
	   B->ds1->ms->ns  != B->ds2->ms->ns)
	{
		printf("\n\n*** WARNING *** Data stream times do not match ***\n\n");
	}
	
	for(i = 0; i < NItem; i++)
	{
		v = fgets(buffer[i], MaxLineLen, in);
		if(!v)
		{
			deleteBaseline(B);

			fclose(in);

			return 0;
		}
		stripEOL(buffer[i]);
	}

	fclose(in);

	if(abs(B->ds1->nChan) != abs(B->ds2->nChan))
	{
		fprintf(stderr, "Number of channels per datastream must match (%d %d)\n",
			B->ds1->nChan, B->ds2->nChan);

		deleteBaseline(B);

		return 0;
	}
	
	B->nChan = abs(B->ds1->nChan);
	B->visFile = strdup(buffer[0]);
	B->lagFile = strdup(buffer[1]);
	B->nFFT = atoi(buffer[2]);
	if(B->nFFT <= 0)
	{
		B->nFFT = 0x7FFFFFFF;	/* effectively no limit */
	}
	B->visibility = (fftw_complex *)calloc(B->nChan, sizeof(fftw_complex));
	B->lags = (fftw_complex *)calloc(B->nChan, sizeof(fftw_complex));
	B->ac1 = (double *)calloc(B->nChan, sizeof(double));
	B->ac2 = (double *)calloc(B->nChan, sizeof(double));

	B->outVis = fopen(B->visFile, "w");
	if(!B->outVis)
	{
		fprintf(stderr, "Cannot open %s for output\n", B->visFile);

		deleteBaseline(B);

		return 0;
	}

	B->outLag = fopen(B->lagFile, "w");
	if(!B->outLag)
	{
		fprintf(stderr, "Cannot open %s for output\n", B->lagFile);

		deleteBaseline(B);

		return 0;
	}

	B->plan = fftw_plan_dft_1d(B->nChan, B->visibility, B->lags, FFTW_BACKWARD, FFTW_ESTIMATE);

	/* FIXME: check that ds1 and ds2 have same */
	B->deltaF = B->ds1->deltaF;
	B->deltaT = 1.0/(B->nChan*B->ds1->deltaF);

	return B;
}

void deleteBaseline(Baseline *B)
{
	if(B)
	{
		if(B->confFile)
		{
			free(B->confFile);
			B->confFile = 0;
		}
		if(B->visFile)
		{
			free(B->visFile);
			B->visFile = 0;
		}
		if(B->lagFile)
		{
			free(B->lagFile);
			B->lagFile = 0;
		}
		if(B->visibility)
		{
			free(B->visibility);
			B->visibility = 0;
		}
		if(B->lags)
		{
			free(B->lags);
			B->lags = 0;
		}
		if(B->ac1)
		{
			free(B->ac1);
			B->ac1 = 0;
		}
		if(B->ac2)
		{
			free(B->ac2);
			B->ac2 = 0;
		}
		if(B->outVis)
		{
			fclose(B->outVis);
			B->outVis = 0;
		}
		if(B->outLag)
		{
			fclose(B->outLag);
			B->outLag = 0;
		}
		if(B->plan)
		{
			fftw_destroy_plan(B->plan);
			B->plan = 0;
		}

		free(B);
	}
}

void printBaseline(const Baseline *B)
{
	printf("Baseline [%p]\n", B);
	if(B == 0)
	{
		return;
	}
	printDataStream(B->ds1);
	printDataStream(B->ds2);
	printf("  conf file = %s\n", B->confFile);
	printf("  visibility output file = %s\n", B->visFile);
	printf("  lag function output file = %s\n", B->lagFile);
	printf("  nFFT = %d\n", B->nFFT);
	printf("  nChan = %d\n", B->nChan);
	printf("  deltaF = %f Hz\n", B->deltaF);
	printf("  deltaT = %e s\n", B->deltaT);
}

static void usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("A zero baseline cross correlator\n\n");
	printf("Usage: %s [ <options> ] <conf file>\n\n", pgm);
	printf("options can include:\n\n");
	printf("  --help\n");
	printf("  -h         Print this help information and quit\n\n");
	printf("  --verbose\n");
	printf("  -v         Increase the output verbosity\n\n");
	printf("The conf file should have 17 lines as follows:\n\n"
"For the first datastream:\n"
"   1  Input baseband data file name\n"
"   2  Input format (e.g., Mark5B-2048-16-2)\n"
"   3  Input sub-band to process (0-based index)\n"
"   4  Offset into the file (bytes)\n"
"   5  Size of FFT to perform over the original bandwidth\n"
"   6  First channel (spectral point) to correlate\n"
"   7  Number of channels to correlate (negative for LSB)\n"
"For the second datastream:\n"
"   8  Input baseband data file name\n"
"   9  Input format (e.g., Mark5B-2048-16-2)\n"
"  10  Input sub-band to process (0-based index)\n"
"  11  Offset into the file (bytes)\n"
"  12  Size of FFT to perform over the original bandwidth\n"
"  13  First channel to correlate\n"
"  14  Number of channels to correlate (negative for LSB)\n"
"Other general parameters:\n"
"  15  Name of output visibility file\n"
"  16  Name of output lag file\n"
"  17  Number of FFTs to process (if -1, run on entire input files)\n\n");
	printf("The visibility output file (specified in line 15 above) has 8 columns:\n"
"   1  Channel (spectral point) number\n"
"   2  Frequency relative to first spectral channel (Hz)\n"
"   3  Real value of the visibility\n"
"   4  Imaginary value of the visibility\n"
"   5  Amplitude\n"
"   6  Phase\n"
"   7  Autocorrelation of the first datastream (real only)\n"
"   8  Autocorrelation of the second datastream (real only)\n\n");
	printf("The lags output file (specified in line 16 above) has 7 columns:\n"
"   1  Channel (spectral point) number\n"
"   2  Time lag (sec)\n"
"   3  Real value of the lag function\n"
"   4  Imaginary value of the lag function\n"
"   5  Amplitude\n"
"   6  Phase\n"
"   7  Window function\n\n");
	printf("Control-C will stop this program after the next FFT is completed and\n"
"will write the partial results to the output files.\n\n");
}

static void report_datastream(DataStream *ds, int xf)
{
	int ll;

	printf("Stream %s\n", ds->ms->streamname);
	for(ll=0; ll < ds->nChan; ll++)
	{
		printf("%+4.1f%c", ds->data[ds->subBand][ll], ll%16==15 ? '\n' : ' ');
	}
	if(!xf)
	{
		return;
	}
	printf("RealXF %s\n", ds->ms->streamname);
	for(ll=0; ll < ds->fftSize/2 + 1; ll++)
	{
		printf( "%+4.1f%c", creal(ds->zdata[ll]), ll%8==7 ? '\n' : ' ');
	}
	printf("ImagXF %s\n", ds->ms->streamname);
	for(ll=0; ll < ds->fftSize/2 + 1; ll++)
	{
		printf( "%+4.1f%c", cimag(ds->zdata[ll]), ll%8==7 ? '\n' : ' ');
	}
}
static void report_baseline_data(Baseline *B, int xf)
{
	report_datastream(B->ds1, xf);
	report_datastream(B->ds2, xf);
}

static int zerocorr(const char *confFile, int verbose)
{
	Baseline *B;
	int n, j, v, index;
	sighandler_t oldsiginthand;
	double x, y, window, scale;

	oldsiginthand = signal(SIGINT, siginthand);

	B = newBaseline(confFile);
	if(!B)
	{
		return EXIT_FAILURE;
	}

	if(verbose > 0)
	{
		printBaseline(B);
	}

	for(n = 0; n < B->nFFT; n++)
	{
		if(verbose > 1 && n % 100 == 0)
		{
			printf("%d of %d FFTs complete\n", n, B->nFFT);
		}

		v = feedDataStream(B->ds1);
		if(v < 0)
		{
			break;
		}
		v = feedDataStream(B->ds2);
		if(v < 0)
		{
			break;
		}
		if(verbose > 2 && n % 1000 == 0)
			report_baseline_data(B, verbose > 3);

		for(j = 0; j < B->nChan; j++)
		{
			B->visibility[j] += B->ds1->spec[j]*~B->ds2->spec[j];
			B->ac1[j] += creal(B->ds1->spec[j]*~B->ds1->spec[j]);
			B->ac2[j] += creal(B->ds2->spec[j]*~B->ds2->spec[j]);
		}

		if(die)
		{
			fprintf(stderr, "\nStopping early at %d / %d\n", n+1, B->nFFT);
			
			break;
		}
	}

	if(n == 0)
	{
		fprintf(stderr, "No data correlated!\n");
	}
	else
	{
		printf("%d FFTs processed\n", n);

		scale = 1.0/(n);

		for(j = 0; j < B->nChan; j++)
		{
			x = creal(B->visibility[j])*scale;
			y = cimag(B->visibility[j])*scale;
			fprintf(B->outVis, "%d %e  %f %f %f %f  %f %f\n", j, j*B->deltaF, x, y, sqrt(x*x+y*y), atan2(y, x), B->ac1[j]/n, B->ac2[j]/n);
		}

		fftw_execute(B->plan);

		scale = 1.0/(n);

		for(j = -B->nChan/2+1; j < B->nChan/2; j++)
		{
			index = j >= 0 ? j : j+B->nChan;
			window = (B->nChan/2 - abs(j))/(float)(B->nChan/2);
			x = creal(B->lags[index])*scale;
			y = cimag(B->lags[index])*scale;	
			fprintf(B->outLag, "%d %e  %f %f %f %f  %f\n", j, j*B->deltaT, x, y, sqrt(x*x+y*y), atan2(y, x), window);
		}
	}

	deleteBaseline(B);

	signal(SIGINT, oldsiginthand);

	return EXIT_SUCCESS;
}

int main(int argc, char **argv)
{
	int a;
	int verbose = 0;
	const char *confFile = 0;
	int retval;

	if(argc <= 1)
	{
		usage(argv[0]);

		return EXIT_FAILURE;
	}

	for(a = 1; a < argc; a++)
	{
		if(strcmp(argv[a], "-h") == 0 ||
		   strcmp(argv[a], "--help") == 0)
		{
			usage(argv[0]);

			return EXIT_SUCCESS;
		}
		else if(strcmp(argv[a], "-v") == 0 ||
		   strcmp(argv[a], "--verbose") == 0)
		{
			verbose++;
		}
		else if(confFile == 0)
		{
			confFile = argv[a];
		}
		else
		{
			fprintf(stderr, "\nSorry, I don't know what to do with `%s'\n", argv[a]);
			fprintf(stderr, "Run with -h for usage instructions\n\n");

			return EXIT_FAILURE;
		}
	}

	if(confFile == 0)
	{
		fprintf(stderr, "\nError: no conf file provided on the command line.  Quitting.\n\n");

		retval = EXIT_FAILURE;
	}
	else
	{
		retval = zerocorr(confFile, verbose);
	}

	return retval;
}

