/***************************************************************************
 *   Copyright (C) 2010-2020 by Walter Brisken and Chris Phillips          *
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
#include <math.h>
#include <signal.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "m5fold";
const char author[]  = "Walter Brisken";
const char version[] = "1.7";
const char verdate[] = "20200114";

const int ChunkSize = 10000;

volatile int die = 0;

struct sigaction old_sigint_action;

void siginthand(int j)
{
	fprintf(stderr, "\nBeing killed.  Partial results will be saved.\n\n");
	die = 1;

	sigaction(SIGINT, &old_sigint_action, 0);
}

static void usage(const char *pgm)
{
	fprintf(stderr, "\n");

	fprintf(stderr, "%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	fprintf(stderr, "A Mark5 power folder.  Can use VLBA, Mark3/4, Mark5B and VDIF " "formats using the\nmark5access library.\n\n");
	fprintf(stderr, "Usage: %s <infile> <dataformat> <nbin> <nint> <freq> <outfile> [<offset>]\n\n", program);
	fprintf(stderr, "  <infile> is the name of the input file\n\n");
	fprintf(stderr, "  <dataformat> should be of the form: <FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	fprintf(stderr, "    VLBA1_2-256-8-2\n");
	fprintf(stderr, "    MKIV1_4-128-2-1\n");
	fprintf(stderr, "    Mark5B-512-16-2\n");
	fprintf(stderr, "    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)\n\n");
	fprintf(stderr, "  alternatively for VDIF and CODIF, Mbps can be replaced by <FramesPerPeriod>m<AlignmentSeconds>, e.g.\n");
	fprintf(stderr, "    VDIF_1000-64000m1-1-2 (8000 frames per 1 second, x1000 bytes x 8 bits= 64 Mbps)\n");
	fprintf(stderr, "    CODIFC_5000-51200m27-8-1 (51200 frames every 27 seconds, x5000 bytes x 8 bits / 27  ~= 76 Mbps\n");
	fprintf(stderr, "    This allows you to specify rates that are not an integer Mbps value, such as 32/27 CODIF oversampling\n\n");
	fprintf(stderr, "  <nbin> is the number of bins per if across 1 period\n");
	fprintf(stderr, "         if negative, the conversion to true power is not performed\n\n");
	fprintf(stderr, "  <nint> is the number of %d sample chunks to work on\n\n", ChunkSize);
	fprintf(stderr, "  <freq> [Hz] -- the inverse of the period to be folded\n\n");
	fprintf(stderr, "  <outfile> is the name of the output file\n\n");
	fprintf(stderr, "  <offset> is number of bytes into file to start decoding\n\n");
	fprintf(stderr, "Example: look for the 80 Hz switched power:\n\n");
	fprintf(stderr, "  m5fold 2bit.data.vlba VLBA1_1-128-8-2 128 10000 80 switched_power.out\n\n");
	fprintf(stderr, "Output: A file with <nchan>+1 columns.  First column is time [s].\n");
	fprintf(stderr, "  Each remaining column is folded power for that baseband channel.\n");
	fprintf(stderr, "  If nbin is positive, the scaling is such that <v^2> = 1 yields a\n");
	fprintf(stderr, "  power reading of 1.0.  Optimal S/N occurs for power ~= 1.03\n\n");
	fprintf(stderr, "Note: This program is useless on 1-bit quantized data\n\n");
}

static int fold(const char *filename, const char *formatname, int nbin, int nint, double freq, const char *outfile, long long offset)
{
	struct mark5_stream *ms;
	double **bins=0;
	int **weight;
	int c, i, j, k, status;
	int nif, bin;
	long long total, unpacked;
	FILE *out;
	double R;
	long long sampnum;
	int docorrection = 1;
	int docomplex = 0;

	if(nbin < 0)
	{
		nbin = -nbin;
		docorrection = 0;
	}

	total = unpacked = 0;

	ms = new_mark5_stream(
		new_mark5_stream_file(filename, offset),
		new_mark5_format_generic_from_string(formatname) );

	if(!ms)
	{
		fprintf(stderr, "Error: problem opening %s\n", filename);

		return EXIT_FAILURE;
	}

	if(ms->nbit < 2)
	{
		fprintf(stderr, "Warning: 1-bit data supplied.  Results will be\n");
		fprintf(stderr, "useless.  Proceeding anyway!\n\n");
	}

	if(ms->nbit > 2)
	{
		fprintf(stderr, "More than 2 bits: power not being corrected!\n");
		docorrection = 0;
	}

	mark5_stream_print(ms);

	if(ms->iscomplex) 
	{
		fprintf(stderr, "Complex decode\n");
		docomplex = 1;
	}

	sampnum = (long long)((double)ms->ns*(double)ms->samprate*1.0e-9 + 0.5);

	out = fopen(outfile, "w");
	if(!out)
	{
		fprintf(stderr, "Error: cannot open %s for write\n", outfile);
		delete_mark5_stream(ms);

		return EXIT_FAILURE;
	}

	R = nbin*freq/ms->samprate;

	nif = ms->nchan;

	bins = (double **)malloc(nif*sizeof(double *));
	weight = (int **)malloc(nif*sizeof(int *));
	for(i = 0; i < nif; ++i)
	{
		bins[i] = (double *)calloc(nbin, sizeof(double));
		weight[i] = (int *)calloc(nbin, sizeof(int));
	}

	if(!docomplex) 
	{
		double **data;

		data = (double **)malloc(nif*sizeof(double *));
		for(i = 0; i < nif; ++i)
		{
			data[i] = (double *)malloc(ChunkSize*sizeof(double));
		}

		if(ms->ns < 0 || ms->ns > 1000000000)
		{
			fflush(stdout);
			fprintf(stderr, "\n***Warning*** The nano-seconds portion of the timestamp is nonsensable: %d; continuing anyway, but don't expect the time alignment to be meaningful.\n\n", ms->ns);

			sampnum = 0;
		}

		for(j = 0; j < nint; ++j)
		{
			if(die)
			{
				break;
			}

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
				fprintf(stderr, "Too many failures.  consecutive, total fails = %d %d\n", ms->consecutivefails, ms->nvalidatefail);

				break;
			}

			for(k = 0; k < ChunkSize; ++k)
			{
				if(data[0][k] != 0.0)
				{
					bin = ((long long)(sampnum*R)) % nbin;
					for(i = 0; i < nif; ++i)
					{
						bins[i][bin] += data[i][k]*data[i][k];
						++weight[i][bin];
					}
				}
				++sampnum;
			}
		}

		for(i = 0; i < nif; ++i)
		{
			free(data[i]);
		}
		free(data);
	} 
	else 
	{
		double complex **cdata=0;
		
		cdata = (complex double **)malloc(nif*sizeof(double complex*));
		for(i = 0; i < nif; ++i)
		{
			cdata[i] = (complex double *)malloc(ChunkSize*sizeof(complex double));
		}

		if(ms->ns < 0 || ms->ns > 1000000000)
		{
			fflush(stdout);
			fprintf(stderr, "\n***Warning*** The nano-seconds portion of the timestamp is nonsensable: %d; continuing anyway, but don't expect the time alignment to be meaningful.\n\n", ms->ns);

			sampnum = 0;
		}

		for(j = 0; j < nint; ++j)
		{
			if(die)
			{
				break;
			}

			status = mark5_stream_decode_double_complex(ms, ChunkSize, cdata);

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
				fprintf(stderr, "Too many failures.  consecutive, total fails = %d %d\n", ms->consecutivefails, ms->nvalidatefail);

				break;
			}

			for(k = 0; k < ChunkSize; ++k)
			{
				if(cdata[0][k] != 0.0)
				{
					bin = ((long long)(sampnum*R)) % nbin;
					for(i = 0; i < nif; ++i)
					{
						bins[i][bin] += cdata[i][k]*~cdata[i][k];
						++weight[i][bin];
					}
				}
				++sampnum;
			}
		}

		for(i = 0; i < nif; ++i)
		{
			free(cdata[i]);
		}
		free(cdata);
	}

	fprintf(stderr, "%lld / %lld samples unpacked\n", unpacked, total);

	/* normalize */
	for(k = 0; k < nbin; ++k)
	{
		for(i = 0; i < nif; ++i)
		{
			if(weight[i][k]) 
			{
				bins[i][k] /= weight[i][k];

			}
		}
	}

	/* convert the mean quantized voltage squared to nominal power */
	if(docorrection)
	{
		for(k = 0; k < nbin; ++k)
		{
			for(i = 0; i < nif; ++i)
			{
				bins[i][k] = correct_2bit_power(bins[i][k]);
			}
		}
	}

	for(c = 0; c < nbin; ++c)
	{
		fprintf(out, "%11.9f ", c/(freq*nbin));
		for(i = 0; i < nif; ++i)
		{
			fprintf(out, " %f", bins[i][c]);
		}
		fprintf(out, "\n");
	}

	fclose(out);

	for(i = 0; i < nif; ++i)
	{
		free(bins[i]);
		free(weight[i]);
	}
	free(bins);
	free(weight);

	delete_mark5_stream(ms);

	return EXIT_SUCCESS;
}

int main(int argc, char **argv)
{
	long long offset = 0;
	int nbin, nint;
	double freq;
	int retval;
	struct sigaction new_sigint_action;

	if(argc < 6)
	{
		usage(argv[0]);

		return EXIT_FAILURE;
	}

	nbin = atol(argv[3]);
	nint = atol(argv[4]);
	freq = atof(argv[5]);

	/* if supplied nint is non-sensical, assume whole file */
	if(nint <= 0)
	{
		nint = 2000000000L;
	}

	if(argc > 7)
	{
		offset = atoll(argv[7]);
	}

	new_sigint_action.sa_handler = siginthand;
	sigemptyset(&new_sigint_action.sa_mask);
	new_sigint_action.sa_flags = 0;
	sigaction(SIGINT, &new_sigint_action, &old_sigint_action);

	retval = fold(argv[1], argv[2], nbin, nint, freq, argv[6], offset);

	return retval;
}

