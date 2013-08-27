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
#include <complex.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <signal.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "m5fold";
const char author[]  = "Walter Brisken";
const char version[] = "1.5";
const char verdate[] = "20130320";

const int ChunkSize = 10000;

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
	printf("A Mark5 power folder.  Can use VLBA, Mark3/4, Mark5B and VDIF "
		"formats using the\nmark5access library.\n\n");
	printf("Usage: %s <infile> <dataformat> <nbin> <nint> <freq> <outfile> [<offset>]\n\n", program);
	printf("  <infile> is the name of the input file\n\n");
	printf("  <dataformat> should be of the form: "
		"<FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n");
	printf("    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)\n\n");
	printf("  <nbin> is the number of bins per if across 1 period\n");
	printf("         if negative, the conversion to true power is not performed\n\n");
	printf("  <nint> is the number of %d sample chunks to work on\n\n", ChunkSize);
	printf("  <freq> [Hz] -- the inverse of the period to be folded\n\n");
	printf("  <outfile> is the name of the output file\n\n");
	printf("  <offset> is number of bytes into file to start decoding\n\n");
	printf("Example: look for the 80 Hz switched power:\n\n");
	printf("  m5fold 2bit.data.vlba VLBA1_1-128-8-2 128 10000 80 switched_power.out\n\n");
	printf("Output: A file with <nchan>+1 columns.  First column is time [s].\n");
	printf("  Each remaining column is folded power for that baseband channel.\n");
	printf("  If nbin is positive, the scaling is such that <v^2> = 1 yields a\n");
	printf("  power reading of 1.0.  Optimal S/N occurs for power ~= 1.03\n\n");
	printf("Note: This program is useless on 1-bit quantized data\n\n");
}

static int fold(const char *filename, const char *formatname, int nbin, int nint,
	double freq, const char *outfile, long long offset)
{
	struct mark5_stream *ms;
	double **data=0, **bins=0;
	double complex **cdata=0;
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
		printf("Error: problem opening %s\n", filename);

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

	if (ms->complex_decode != 0) 
	  {
	    printf("Complex decode\n");
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

	if (!docomplex) 
	  {
	    data = (double **)malloc(nif*sizeof(double *));
	    bins = (double **)malloc(nif*sizeof(double *));
	    weight = (int **)malloc(nif*sizeof(int *));
	    for(i = 0; i < nif; i++)
	      {
		data[i] = (double *)malloc(ChunkSize*sizeof(double));
		bins[i] = (double *)calloc(nbin, sizeof(double));
		weight[i] = (int *)calloc(nbin, sizeof(int));
	      }
	    
	    if(ms->ns < 0 || ms->ns > 1000000000)
	      {
		fflush(stdout);
		fprintf(stderr, "\n***Warning*** The nano-seconds portion of the timestamp is nonsensable: %d; continuing anyway, but don't expect the time alignment to be meaningful.\n\n", ms->ns);
		
		sampnum = 0;
	      }
	    
	    for(j = 0; j < nint; j++)
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
		    printf("Too many failures.  consecutive, total fails = %d %d\n", ms->consecutivefails, ms->nvalidatefail);
			
		    break;
		  }

		for(k = 0; k < ChunkSize; k++)
		  {
		    if(data[0][k] != 0.0)
		      {
			bin = ((long long)(sampnum*R)) % nbin;
			for(i = 0; i < nif; i++)
			  {
			    bins[i][bin] += data[i][k]*data[i][k];
			    weight[i][bin]++;
			  }
		      }
		    sampnum++;
		  }
	      }
	  } 
	else 
	  {
	    cdata = (complex double **)malloc(nif*sizeof(double complex*));
	    bins = (double **)malloc(nif*sizeof(double *));
	    weight = (int **)malloc(nif*sizeof(double *));
	    for(i = 0; i < nif; i++)
	      {
		cdata[i] = (complex double *)malloc(ChunkSize*sizeof(complex double));
		bins[i] = (double *)calloc(nbin, sizeof(double));
		weight[i] = (int *)calloc(nbin, sizeof(int));
	      }
	    
	    if(ms->ns < 0 || ms->ns > 1000000000)
	      {
		fflush(stdout);
		fprintf(stderr, "\n***Warning*** The nano-seconds portion of the timestamp is nonsensable: %d; continuing anyway, but don't expect the time alignment to be meaningful.\n\n", ms->ns);
		
		sampnum = 0;
	      }
	    
	    for(j = 0; j < nint; j++)
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
		    printf("Too many failures.  consecutive, total fails = %d %d\n", ms->consecutivefails, ms->nvalidatefail);
			
		    break;
		  }

		for(k = 0; k < ChunkSize; k++)
		  {
		    if(cdata[0][k] != 0.0)
		      {
			bin = ((long long)(sampnum*R)) % nbin;
			for(i = 0; i < nif; i++)
			  {
			    bins[i][bin] += creal(cdata[i][k])*creal(cdata[i][k]) +
			                   cimag(cdata[i][k])*cimag(cdata[i][k]);
			    weight[i][bin]++;
			  }
		      }
		    sampnum++;
		  }
	      }
	  }

	fprintf(stderr, "%Ld / %Ld samples unpacked\n", unpacked, total);

	/* normalize */
	for(k = 0; k < nbin; k++)
	{
		for(i = 0; i < nif; i++)
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
		for(k = 0; k < nbin; k++)
		{
			for(i = 0; i < nif; i++)
			{
				bins[i][k] = correct_2bit_power(bins[i][k]);
			}
		}
	}

	for(c = 0; c < nbin; c++)
	{
		fprintf(out, "%11.9f ", c/(freq*nbin));
		for(i = 0; i < nif; i++)
		{
			fprintf(out, " %f", bins[i][c]);
		}
		fprintf(out, "\n");
	}

	fclose(out);

	for(i = 0; i < nif; i++)
	{
	  if (docomplex)
		free(cdata[i]);
	  else
		free(data[i]);
	  free(bins[i]);
	  free(weight[i]);
	}
	if (docomplex)
	  free(cdata);
	else
	  free(data);
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

	if(argc < 6)
	{
		usage(argv[0]);

		return EXIT_FAILURE;
	}

	oldsiginthand = signal(SIGINT, siginthand);

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

	retval = fold(argv[1], argv[2], nbin, nint, freq, argv[6], offset);

	return retval;
}

