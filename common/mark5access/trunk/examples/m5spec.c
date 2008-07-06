/***************************************************************************
 *   Copyright (C) 2008 by Walter Brisken                                  *
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

#include <stdlib.h>
#include <complex.h>
#include <fftw3.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "m5spec";
const char author[]  = "Walter Brisken";
const char version[] = "0.1";
const char verdate[] = "2008 Jul 6";

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
	fftw_complex *zdata;
	int c, i, j, k, status;
	int chunk;
	long long total, unpacked;
	FILE *out;
	fftw_plan *plan;
	double re, im;
	double f, sum;

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

	data = (double **)malloc(ms->nchan*sizeof(double *));
	spec = (double **)malloc(ms->nchan*sizeof(double *));
	zdata = (fftw_complex *)malloc((nchan+1)*sizeof(fftw_complex));
	plan = (fftw_plan *)malloc(ms->nchan*sizeof(fftw_plan));
	for(i = 0; i < ms->nchan; i++)
	{
		data[i] = (double *)malloc((chunk+2)*sizeof(double));
		spec[i] = (double *)calloc(nchan,    sizeof(double));
		plan[i] = fftw_plan_dft_r2c_1d(nchan*2, data[i],
			(fftw_complex *)zdata, FFTW_MEASURE);
	}

	mark5_stream_print(ms);


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

		for(c = 0; c < ms->nchan; c++)
		{
			/* FFT */
			fftw_execute(plan[c]);

			for(i = 0; i < nchan; i++)
			{
				re = creal(zdata[i]);
				im = cimag(zdata[i]);
				spec[c][i] += re*re + im*im;
			}
		}
	}

	fprintf(stderr, "%Ld / %Ld samples unpacked\n", unpacked, total);

	out = fopen(outfile, "w");

	/* normalize */
	sum = 0.0;
	for(i = 0; i < nchan; i++)
	{
		for(j = 0; j < ms->nchan; j++)
		{
			sum += spec[j][i];
		}
	}

	f = ms->nchan*nchan/sum;

	for(i = 0; i < nchan; i++)
	{
		for(j = 0; j < ms->nchan; j++)
		{
			fprintf(out, "%f ", f*spec[j][i]);
		}
		fprintf(out, "\n");
	}

	fclose(out);

	for(i = 0; i < ms->nchan; i++)
	{
		free(data[i]);
		free(spec[i]);
		fftw_destroy_plan(plan[i]);
	}
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

		buffer = malloc(bufferlen);
		
		in = fopen(argv[1], "r");
		fread(buffer, bufferlen, 1, in);
		
		mf = new_mark5_format_from_stream(
			new_mark5_stream_memory(buffer, bufferlen/2));

		print_mark5_format(mf);
		delete_mark5_format(mf);

		mf = new_mark5_format_from_stream(
			new_mark5_stream_memory(buffer, bufferlen/2));

		print_mark5_format(mf);
		delete_mark5_format(mf);

		free(buffer);

		return 0;
	}

	else if(argc < 6)
	{
		return usage(argv[0]);
	}

	nchan = atol(argv[3]);
	nint  = atol(argv[4]);

	if(argc > 7)
	{
		offset=atoll(argv[6]);
	}

	spec(argv[1], argv[2], nchan, nint, argv[5], offset);

	return 0;
}

