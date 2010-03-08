/***************************************************************************
 *   Copyright (C) 2008-2010 by Walter Brisken                             *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#include <complex.h>
#include <stdlib.h>
#include <complex.h>
#include <fftw3.h>
#include <math.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "m5spec";
const char author[]  = "Walter Brisken";
const char version[] = "1.0";
const char verdate[] = "2010 Mar 8";

int usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("A Mark5 spectrometer.  Can use VLBA, Mark3/4, and Mark5B "
		"formats using the\nmark5access library.\n\n");
	printf("Usage : %s <infile> <dataformat> <nchan> <nint> <outfile> [<offset>]\n\n", program);
	printf("  <infile> is the name of the input file\n\n");
	printf("  <dataformat> should be of the form: "
		"<FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n\n");
	printf("  <nchan> is the number of channels to make per IF\n\n");
	printf("  <nint> is the number of FFT frames to spectrometize\n\n");
	printf("  <outfile> is the name of the output file\n\n");
	printf("  <offset> is number of bytes into file to start decoding\n\n");

	return 0;
}

int spec(const char *filename, const char *formatname, int nchan, int nint,
	const char *outfile, long long offset)
{
	struct mark5_stream *ms;
	double **data, **spec;
	fftw_complex **zdata, **zx;
	int c, i, j, status;
	int chunk, nif;
	long long total, unpacked;
	FILE *out;
	fftw_plan *plan;
	double re, im;
	double f, sum;
	double x, y;

	chunk = 2*nchan;

	total = unpacked = 0;

	ms = new_mark5_stream(
		new_mark5_stream_file(filename, offset),
		new_mark5_format_generic_from_string(formatname) );

	if(!ms)
	{
		printf("problem opening %s\n", filename);
		return 0;
	}

	mark5_stream_print(ms);

	out = fopen(outfile, "w");
	if(!out)
	{
		fprintf(stderr, "Error -- cannot open %s for write\n", outfile);
		delete_mark5_stream(ms);

		return 0;
	}

	nif = ms->nchan;

	data = (double **)malloc(nif*sizeof(double *));
	spec = (double **)malloc(nif*sizeof(double *));
	zdata = (fftw_complex **)malloc(nif*sizeof(fftw_complex *));
	plan = (fftw_plan *)malloc(nif*sizeof(fftw_plan));
	zx = (fftw_complex **)malloc((nif/2)*sizeof(fftw_complex *));
	for(i = 0; i < nif; i++)
	{
		data[i] = (double *)malloc((chunk+2)*sizeof(double));
		spec[i] = (double *)calloc(nchan, sizeof(double));
		zdata[i] = (fftw_complex *)malloc((nchan+1)*sizeof(fftw_complex));
		plan[i] = fftw_plan_dft_r2c_1d(nchan*2, data[i],
			(fftw_complex *)zdata[i], FFTW_MEASURE);
	}
	for(i = 0; i < nif/2; i++)
	{
		zx[i] = (fftw_complex *)calloc(nchan, sizeof(fftw_complex));
	}

	for(j = 0; j < nint; j++)
	{
		status = mark5_stream_decode_double(ms, chunk, data);
		
		if(status < 0)
		{
			break;
		}
		else
		{
			total += chunk;
			unpacked += status;
		}

		if(ms->consecutivefails > 5)
		{
			break;
		}

		for(i = 0; i < nif; i++)
		{
			/* FFT */
			fftw_execute(plan[i]);
		}

		for(i = 0; i < nif; i++)
		{
			for(c = 0; c < nchan; c++)
			{
				re = creal(zdata[i][c]);
				im = cimag(zdata[i][c]);
				spec[i][c] += re*re + im*im;
			}
		}

		for(i = 0; i < nif/2; i++)
		{
			for(c = 0; c < nchan; c++)
			{
				zx[i][c] += zdata[2*i][c]*~zdata[2*i+1][c];
			}
		}
	}

	fprintf(stderr, "%Ld / %Ld samples unpacked\n", unpacked, total);

	/* normalize */
	sum = 0.0;
	for(c = 0; c < nchan; c++)
	{
		for(i = 0; i < nif; i++)
		{
			sum += spec[i][c];
		}
	}

	f = nif*nchan/sum;

	for(c = 0; c < nchan; c++)
	{
		fprintf(out, "%f ", (double)c*ms->samprate/(2.0e6*nchan));
		for(i = 0; i < nif; i++)
		{
			fprintf(out, " %f", f*spec[i][c]);
		}
		for(i = 0; i < nif/2; i++)
		{
			x = creal(zx[i][c])*f;
			y = cimag(zx[i][c])*f;
			fprintf(out, "  %f %f", sqrt(x*x+y*y), atan2(y, x));
		}
		fprintf(out, "\n");
	}

	fclose(out);

	for(i = 0; i < nif; i++)
	{
		free(data[i]);
		free(zdata[i]);
		free(spec[i]);
		fftw_destroy_plan(plan[i]);
	}
	for(i = 0; i < nif/2; i++)
	{
		free(zx[i]);
	}
	free(zx);
	free(data);
	free(zdata);
	free(spec);
	free(plan);

	delete_mark5_stream(ms);

	return 0;
}

int main(int argc, char **argv)
{
	long long offset = 0;
	int nchan, nint;

	if(argc == 2)
	{
		struct mark5_format *mf;
		int bufferlen = 1<<11;
		char *buffer;
		FILE *in;
		int r;

		buffer = malloc(bufferlen);
		
		in = fopen(argv[1], "r");
		r = fread(buffer, bufferlen, 1, in);
		if(r < 1)
		{
			fprintf(stderr, "Error, buffer read failed.\n");
		}
		
		else
		{
			mf = new_mark5_format_from_stream(
				new_mark5_stream_memory(buffer, bufferlen/2));

			print_mark5_format(mf);
			delete_mark5_format(mf);

			mf = new_mark5_format_from_stream(
				new_mark5_stream_memory(buffer, bufferlen/2));

			print_mark5_format(mf);
			delete_mark5_format(mf);
		}

		fclose(in);
		free(buffer);

		return 0;
	}

	else if(argc < 6)
	{
		return usage(argv[0]);
	}

	nchan = atol(argv[3]);
	nint  = atol(argv[4]);
	if(nint <= 0)
	{
		nint = 2000000000L;
	}

	if(argc > 6)
	{
		offset=atoll(argv[6]);
	}

	spec(argv[1], argv[2], nchan, nint, argv[5], offset);

	return 0;
}

