/***************************************************************************
 *   Copyright (C) 2010-2017 by Walter Brisken, Chris Phillips             *
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
// $Id: m5fold.c 3935 2011-11-20 00:29:43Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/examples/m5fold.c $
// $LastChangedRevision: 3935 $
// $Author: WalterBrisken $
// $LastChangedDate: 2011-11-20 11:29:43 +1100 (Sun, 20 Nov 2011) $
//
//============================================================================

#include "complex.h"
#include <complex.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <signal.h>
#include <stdint.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "m5timeseries";
const char author[]  = "Chris Phillips";
const char version[] = "0.2";
const char verdate[] = "20170426";

const int ChunkSize = 10000;

volatile int die = 0;

struct sigaction old_sigint_action;

void siginthand(int j) {
  printf("\nBeing killed.  Partial results will be saved.\n\n");
  die = 1;

  sigaction(SIGINT, &old_sigint_action, 0);
}

static void usage(const char *pgm) {
  printf("\n");

  printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
  printf("A Mark5 power averager.  Can use VLBA, Mark3/4, Mark5B and VDIF"
	 "formats using the\nmark5access library.\n\n");
  printf("Usage: %s <infile> <dataformat> <tint> <time> <outfile> [<offset>]\n\n", program);
  printf("  <infile> is the name of the input file\n\n");
  printf("  <dataformat> should be of the form: "
	 "<FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
  printf("    VLBA1_2-256-8-2\n");
  printf("    MKIV1_4-128-2-1\n");
  printf("    Mark5B-512-16-2\n");
  printf("    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)\n\n");
  printf("  <tint> is the integration time, in millisec. Fractions allowed\n");
  printf("  <time> The number of samples, in seconds to process\n\n");
  printf("  <outfile> is the name of the output file\n\n");
  printf("  <offset> is number of bytes into file to start decoding\n\n");
  printf("Example: look for the 80 Hz switched power:\n\n");
  printf("  m5timeseries 2bit.data.vlba VLBA1_1-128-8-2 4 2 power.out\n\n");
  printf("Output: A file with <nchan>+2 columns.  First column is output line\n");
  printf("  number (starting at 0).  Second column is time [s].\n");
  printf("  Each remaining column is folded power for that baseband channel.\n");
  printf("  If nbin is positive, the scaling is such that <v^2> = 1 yields a\n");
  printf("  power reading of 1.0.  Optimal S/N occurs for power ~= 1.03\n\n");
  printf("Note: This program is useless on 1-bit quantized data\n\n");
}

int average_real(struct mark5_stream *ms, int *nused, uint64_t nint, uint64_t *navg,
		 double *avif, double **data) {
  int status;
  int nif = ms->nchan;

  if (*nused<0 || *nused>=ChunkSize) { // No data available
    status = mark5_stream_decode_double(ms, ChunkSize, data);
    if(status < 0) {
      return(status);
    } 
    if(ms->consecutivefails > 5) {
      printf("Too many failures.  consecutive, total fails = %d %d\n", ms->consecutivefails, ms->nvalidatefail);
      return(-1);
    }
    *nused = 0;
  } 

  int i;
  for (i=*nused; i<ChunkSize; i++) {
    int j;
    for (j=0; j<nif; j++) {
      avif[j] += data[j][i]*data[j][i];
    }
    (*nused)++;
    (*navg)++;

    if (*navg>=nint) return(0);
  }
  return(0);
}

int average_complex(struct mark5_stream *ms, int *nused, uint64_t nint, uint64_t *navg,
		    double *avif, complex double **cdata) {
  int status;
  int nif = ms->nchan;

  if (*nused<0 || *nused>=ChunkSize) { // No data available
    status = mark5_stream_decode_double_complex(ms, ChunkSize, cdata);
    if(status < 0) {
      return(status);
    } 
    if(ms->consecutivefails > 5) {
      printf("Too many failures.  consecutive, total fails = %d %d\n", ms->consecutivefails, ms->nvalidatefail);
      return(-1);
    }
    *nused = 0;
  } 

  int i;
  for (i=*nused; i<ChunkSize; i++) {
    int j;
    for (j=0; j<nif; j++) {
      avif[j] += creal(cdata[j][i])*creal(cdata[j][i]) +
	               cimag(cdata[j][i])*cimag(cdata[j][i]);
    }
    (*nused)++;
    (*navg)++;

    if (*navg>=nint) return(0);
  }
  return(0);
}

static int timeaverage(const char *filename, const char *formatname, double tint, double time,
		       const char *outfile, long long offset) {
  struct mark5_stream *ms;
  double **data=0, *avif;
  double complex **cdata=0;
  int i, status=0, nused;
  int nif;
  uint64_t totalsamples, nint, samplesDone, navg;
  FILE *out;
  int docomplex = 0;

  //total = unpacked = 0;

  ms = new_mark5_stream(new_mark5_stream_file(filename, offset),
			new_mark5_format_generic_from_string(formatname));

  if(!ms) {
    printf("Error: problem opening %s\n", filename);
    return EXIT_FAILURE;
  }

  mark5_stream_print(ms);

  if (ms->complex_decode != 0)  {
    printf("Complex decode\n");
    docomplex = 1;
  }

  out = fopen(outfile, "w");
  if(!out) {
    fprintf(stderr, "Error: cannot open %s for write\n", outfile);
    delete_mark5_stream(ms);
    return EXIT_FAILURE;
  }

  //R = nbin*freq/ms->samprate;

  nif = ms->nchan;
  totalsamples = (ms->samprate * time);  // Total number of samples to process
  nint  = (ms->samprate * tint/1000.0);  // Samples per itergration

  if (docomplex)  {
    cdata = (complex double **)malloc(nif*sizeof(double complex*));
    for(i = 0; i < nif; i++) {
      cdata[i] = (complex double *)malloc(ChunkSize*sizeof(complex double));
    }
  } else {
    data = (double**)malloc(nif*sizeof(double*));
    for(i = 0; i < nif; i++) {
      data[i] = (double *)malloc(ChunkSize*sizeof(double));
    }
  } 
  avif = (double*)calloc(nif,sizeof(double));
	    
  if (ms->ns < 0 || ms->ns > 1000000000) {
    fflush(stdout);
    fprintf(stderr, "\n***Warning*** The nano-seconds portion of the timestamp is nonsensable: %d; continuing anyway, but don't expect the time alignment to be meaningful.\n\n", ms->ns);
  }
	    
  int iInt = 0;
  samplesDone = 0;
  navg = 0;
  nused = -1;
  while (samplesDone<totalsamples) {
    if(die) break;

    if (docomplex) {
      status = average_complex(ms, &nused, nint, &navg, avif, cdata);
    } else {
      status = average_real(ms, &nused, nint, &navg, avif, data);
    }
    if (status<0) {
      break;
    }

    if (navg>=nint) {
      fprintf(out, "%4d %10.6f", iInt, iInt*nint/(double)ms->samprate);
      iInt++;
      int i;
      for(i=0; i<nif; i++) {
	avif[i] /= navg;
	fprintf(out, " %1f", avif[i]);
	avif[i] = 0;
      }
      fprintf(out, "\n");
      samplesDone += navg;
      navg = 0;
    }
  }
  fclose(out);

  for(i = 0; i < nif; i++) {
    if (docomplex)
      free(cdata[i]);
    else
      free(data[i]);
  }
  if (docomplex)
    free(cdata);
  else
    free(data);
  free(avif);
  delete_mark5_stream(ms);

  if(status >= 0)
    status = EXIT_SUCCESS;
  return status;
}

int main(int argc, char **argv) {
  long long offset = 0;
  double time, tint;
  int retval;
  struct sigaction new_sigint_action;

  if(argc < 6){
    usage(argv[0]);
    return EXIT_FAILURE;
  }

  tint = atof(argv[3]);
  time = atof(argv[4]);

  /* if supplied time is non-sensical, assume whole file */
  if (time <= 0) {
    time = 1e99;
  }

  if(argc > 6)	{
    offset = atoll(argv[6]);
  }

  new_sigint_action.sa_handler = siginthand;
  sigemptyset(&new_sigint_action.sa_mask);
  new_sigint_action.sa_flags = 0;
  sigaction(SIGINT, &new_sigint_action, &old_sigint_action);

  retval = timeaverage(argv[1], argv[2], tint, time, argv[5], offset);

  return retval;
}

