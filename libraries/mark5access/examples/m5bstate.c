//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: m5bstate.c 10490 2022-06-03 14:18:03Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/libraries/mark5access/examples/m5bstate.c $
// $LastChangedRevision: 10490 $
// $Author: WalterBrisken $
// $LastChangedDate: 2022-06-03 22:18:03 +0800 (五, 2022-06-03) $
//
//============================================================================


#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <math.h>
#include "../mark5access/mark5_stream.h"

const char program[] = "m5bstate";
const char author[]  = "Alessandra Bertarini"; /* with input from Chris Phillips, Walter Brisken, and likely others */
const char version[] = "1.5";
const char verdate[] = "2023 Jan 06";

volatile int die = 0;

struct sigaction old_sigint_action;

void siginthand(int j)
{
	printf("\nBeing killed.  Partial results will be saved.\n\n");
	die = 1;

	sigaction(SIGINT, &old_sigint_action, 0);
}

int usage(const char *pgm)
{
	printf("\n");

	printf("%s ver. %s   %s  %s\n\n", program, version, author, verdate);
	printf("A baseband data state counter.  Can use VLBA, Mark3/4, Mark5B and\n");
	printf("single-thread VDIF or CODIF formats using the mark5access library.\n");
	printf("Multi-thread VDIF can be handed using the vdifbstate wrapper.\n\n");
	printf("Usage : %s <infile> <dataformat> <nframes> [<offset> [ {E|O} ] ]\n\n", program);
	printf("  <infile> is the name of the input file\n\n");
	printf("  <dataformat> should be of the form: <FORMAT>-<Mbps>-<nchan>-<nbit>, e.g.:\n");
	printf("    VLBA1_2-256-8-2\n");
	printf("    MKIV1_4-128-2-1\n");
	printf("    Mark5B-512-16-2\n");
	printf("    VDIF_1000-64-1-2 (here 1000 is payload size in bytes)\n\n");
	printf("  alternatively for VDIF and CODIF, Mbps can be replaced by <FramesPerPeriod>m<AlignmentSeconds>, e.g.\n");
	printf("    VDIF_1000-8000m1-1-2 (8000 frames per 1 second, x1000 bytes x 8 bits= 64 Mbps)\n");
	printf("    CODIFC_5000-51200m27-8-1 (51200 frames every 27 seconds, x5000 bytes x 8 bits / 27 ~= 76 Mbps\n");
	printf("    This allows you to specify rates that are not an integer Mbps value, such as 32/27 CODIF oversampling\n\n");
	printf("  <nframes> is the number of frames to bstate-erize\n\n");
	printf("  <offset> is number of bytes into file to start decoding [default value is zero]\n\n");
	printf("  E: Putting an E after the offset parameter causes the program to consider even samples (starting with 0)\n");
	printf("  O: Putting an O after the offset parameter causes the program to consider odd samples (starting with 0)\n\n");

	return 0;
}

void process_realdata(struct mark5_stream *ms, int nframes, int nstates, int evenodd)
{
  int i, j, k, status, sum;
  long long total, unpacked;
  double x;
  int start, delta;

  int chunk = ms->framesamples;
  int nif = ms->nchan;

  double **data = (double **)malloc(nif*sizeof(double *));
  long **bstate = (long **)malloc(nif*sizeof(long *));
  /*Haystack gain's calculation*/
  double *gfact = (double *)malloc(nif*sizeof(double));
  int nBlank = 0;


/* a is required for Haystack gain calculation*/
  double a = 8 * (M_PI - 3) / (3 * M_PI * (4 - M_PI));

  switch(evenodd)
  {
  case 0x01:
    start = 0;
    delta = 2;
    break;
  case 0x02:
    start = 1;
    delta = 2;
    break;
  default:
    start = 0;
    delta = 1;
    break;
  }

  total = unpacked = 0;

  for(i = 0; i < nif; i++)
  {
    data[i] = (double *)malloc((chunk+2)*sizeof(double)); 
    bstate[i] = (long *)malloc((nstates)*sizeof(long)); 
  }

  /* initialize bstate variable to zeroes*/
  for(i = 0; i < nif; i++)
  {
    for(j = 0; j < nstates; j++)
    {
      bstate[i][j] = 0;
    }
  }


  for(j = 0; j < nframes; j++)
  {
  if(die)
  {
    break;
  }

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


  for(i = 0; i < nif; i++) 
  {

    for(k = start; k < chunk; k+=delta)
    {
      /*       printf("%lf\n", data[i][k]); */
      /* now start to count the states from data[i][k]*/

      if(data[i][k] == 0)
      {
        ++nBlank;
      }
      else if (ms->nbit == 1)
      {
	if (data[i][k] > 0) bstate[i][1]++;
	if (data[i][k] < 0) bstate[i][0]++;
      }
      else if (ms->nbit == 2)
      {
	if (data[i][k] > 0 && data[i][k] < 2.) bstate[i][2]++;
	if (data[i][k] > 2.) bstate[i][3]++;
	if (data[i][k] < 0 && data[i][k] > -2.) bstate[i][1]++;
	if (data[i][k] < -2.) bstate[i][0]++;
      }
      
    }
  } 

  }

  fprintf(stderr, "%lld / %lld samples unpacked\n", unpacked, total);
  if(evenodd != 0x03)
  {
    fprintf(stderr, "Only the %s samples were examined\n", evenodd == 0x01 ? "EVEN" : "ODD");
  }
  if(nBlank > 0)
  {
    fprintf(stderr, "%d samples were flagged as invalid and not counted\n", nBlank);
  }

  /* header of the output bstate table based on Haystack bstate output*/
  if (ms->nbit == 1)
  {
    printf("\nCh    -      +         -      +     gfact\n");
  }
  else if (ms->nbit == 2)
  {
    printf("\nCh    --      -     +     ++        --      -      +     ++     gfact\n");
  }
	  
  /* normalize */
  for(i = 0; i < nif; i++)
  {
    printf("%2d ", i);
    sum = 0.0;
    for(j = 0; j < nstates; j++)
    {
      sum += bstate[i][j];
    }
    for(j = 0; j < nstates; j++)
    {
      printf("%7ld ", bstate[i][j]);
    }
    printf("    ");
    for(j = 0; j < nstates; j++)
    {
      printf("%5.1f  ", (float)bstate[i][j]/sum * 100.);
    }
    /* Haystack gain correction calculation */
	      
    x = (double) (bstate[i][1] + bstate[i][2]) / sum;
    gfact[i] = sqrt (-4 / (M_PI * a) - log (1 - x*x)
		     + 2 * sqrt (pow (2 / (M_PI * a) + log (1 - x*x) / 2, 2)
				 - log (1-x*x)/a)) / 0.91;
    printf("%5.2lf", gfact[i]);  
    printf("\n");
  }


  for(i = 0; i < nif; i++)
  {
    free(data[i]);
    free(bstate[i]);
  }
  free(data);
  free(gfact);
  free(bstate);
}

void process_complexdata(struct mark5_stream *ms, int nframes, int nstates, int evenodd) {
  int i, j, k, status, sum;
  long long total, unpacked;
  double x;
  int start, delta;

  int chunk = ms->framesamples;
  int nif = ms->nchan;

  double complex **cdata = (double complex**)malloc(nif*sizeof(double complex*));
  long **bstate = (long **)malloc(nif*sizeof(long *));
  /*Haystack gain's calculation*/
  double *gfact = (double *)malloc(nif*sizeof(double));
 

/* a is required for Haystack gain calculation*/
  double a = 8 * (M_PI - 3) / (3 * M_PI * (4 - M_PI));

  switch(evenodd)
  {
  case 0x01:
    start = 0;
    delta = 2;
    break;
  case 0x02:
    start = 1;
    delta = 2;
    break;
  default:
    start = 0;
    delta = 1;
    break;
  }

  total = unpacked = 0;

  for(i = 0; i < nif; i++)
  {
    cdata[i] = (double complex*)malloc((chunk+2)*sizeof(double complex)); 
    bstate[i] = (long *)malloc((nstates*2)*sizeof(long)); 
  }

  /* initialize bstate variable to zeroes*/
  for(i = 0; i < nif; i++)
  {
    for(j = 0; j < nstates*2; j++)
    {
      bstate[i][j] = 0;
    }
  }


  for(j = 0; j < nframes; j++)
  {
    if(die)
      {
	break;
      }

    status = mark5_stream_decode_double_complex(ms, chunk, cdata);

    if(status < 0)
      {
	break;
      }
    else
      {
	total += chunk;
	unpacked += status;
      }


    for(i = 0; i < nif; i++) 
      {
	
	for(k = start; k < chunk; k+=delta)
	  {
	    /*       printf("%lf\n", data[i][k]); */
	    /* now start to count the states from data[i][k]*/

	    if (ms->nbit == 1)
	      {
		if (creal(cdata[i][k]) > 0) bstate[i][1]++;
		if (creal(cdata[i][k]) < 0) bstate[i][0]++;
		if (cimag(cdata[i][k]) > 0) bstate[i][3]++;
		if (cimag(cdata[i][k]) < 0) bstate[i][2]++;
	      }
	    else if (ms->nbit == 2)
	      {
		if (creal(cdata[i][k]) > 0 && creal(cdata[i][k]) < 2.) bstate[i][2]++;
		if (creal(cdata[i][k]) > 2.) bstate[i][3]++;
		if (creal(cdata[i][k]) < 0 && creal(cdata[i][k]) > -2.) bstate[i][1]++;
		if (creal(cdata[i][k]) < -2.) bstate[i][0]++;
		if (cimag(cdata[i][k]) > 0 && cimag(cdata[i][k]) < 2.) bstate[i][6]++;
		if (cimag(cdata[i][k]) > 2.) bstate[i][7]++;
		if (cimag(cdata[i][k]) < 0 && cimag(cdata[i][k]) > -2.) bstate[i][5]++;
		if (cimag(cdata[i][k]) < -2.) bstate[i][4]++;
	      }
	  }
      }
  }    

  fprintf(stderr, "%lld / %lld samples unpacked\n", unpacked, total);
  if(evenodd != 0x03)
  {
    fprintf(stderr, "Only the %s samples were examined\n", evenodd == 0x01 ? "EVEN" : "ODD");
  }

  /* header of the output bstate table based on Haystack bstate output*/
  if (ms->nbit == 1)
    {
      printf("\nCh    -      +         -      +    -      +         -      +     gfact\n");
    }
  else if (ms->nbit == 2)
    {
      printf("\nCh     --       -       +      ++      --       -       +      ++        --      -      +     ++     --      -      +     ++    gfact\n");
    }

  /* normalize */
  for(i = 0; i < nif; i++)
    {
      printf("%2d ", i);
      sum = 0.0;
      for(j = 0; j < nstates; j++)
	{
	  sum += bstate[i][j];
	}
      for(j = 0; j < nstates*2; j++)
	{
	  printf("%7ld ", bstate[i][j]);
	}
      printf("    ");
      for(j = 0; j < nstates*2; j++)
	{
	  printf("%5.1f  ", (float)bstate[i][j]/sum * 100.);
	}
      /* Haystack gain correction calculation */

      x = (double) (bstate[i][1] + bstate[i][2]) / sum;
      gfact[i] = sqrt (-4 / (M_PI * a) - log (1 - x*x)
		       + 2 * sqrt (pow (2 / (M_PI * a) + log (1 - x*x) / 2, 2)
				   - log (1-x*x)/a)) / 0.91;
      printf("%5.2lf", gfact[i]);  
      printf("\n");
    }


  for(i = 0; i < nif; i++)
    {
      free(cdata[i]);
      free(bstate[i]);
    }
  free(cdata);
  free(gfact);
  free(bstate);
}

void process_8bit_realdata(struct mark5_stream *ms, int nframes) {
  int i, j, k, status;
  long long total, unpacked;

  int chunk = ms->framesamples;
  int nif = ms->nchan;

  double **data = (double **)malloc(nif*sizeof(double *));
  double *sum = malloc(nif*sizeof(double));
  double *sumsqr = malloc(nif*sizeof(double));

  total = unpacked = 0;

  for(i = 0; i < nif; i++)
  {
    data[i] = (double *)malloc((chunk+2)*sizeof(double)); 
  }

  /* initialize stats variable to zeroes*/
  for(i = 0; i < nif; i++)
  {
      sum[i] = 0;
      sumsqr[i] = 0;
  }


  for(j = 0; j < nframes; j++)
  {
    if(die)
    {
      break;
    }

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


    for(i = 0; i < nif; i++) 
    {
      double thissum=0, thissumsqr=0;
      for(k = 0; k < chunk; k++)
      {
	thissum += data[i][k];
	thissumsqr += data[i][k]*data[i][k];
      }
      sum[i] += thissum;
      sumsqr[i] += thissumsqr;
    }
  }

  fprintf(stderr, "%lld / %lld samples unpacked\n", unpacked, total);

  /* header of the output bstate table based on Haystack bstate output*/

  printf("\nCh    RMS   Mean\n");
	  
  for(i = 0; i < nif; i++)
  {
    double mean = sum[i]/total;
    double stddev = sqrt(sumsqr[i]/total - mean*mean);
    printf("%2d  %.3f  %.4f\n", i, stddev, mean);
  }

  for(i = 0; i < nif; i++)
  {
    free(data[i]);
  }
  free(data);
  free(sum);
  free(sumsqr);
}

void process_8bit_complexdata(struct mark5_stream *ms, int nframes) {
  int i, j, k, status;
  long long total, unpacked;

  int chunk = ms->framesamples;
  int nif = ms->nchan;

  double complex **cdata = (double complex**)malloc(nif*sizeof(double *));
  double *sum = malloc(nif*2*sizeof(double));
  double *sumsqr = malloc(nif*2*sizeof(double));

  total = unpacked = 0;

  for(i = 0; i < nif; i++)
  {
    cdata[i] = (double complex*)malloc((chunk+2)*sizeof(double complex)); 
  }

  /* initialize stats variable to zeroes*/
  for(i = 0; i < nif*2; i++)
  {
      sum[i] = 0;
      sumsqr[i] = 0;
  }


  for(j = 0; j < nframes; j++)
  {
    if(die)
    {
      break;
    }

    status = mark5_stream_decode_double_complex(ms, chunk, cdata);

    if(status < 0)
    {
      break;
    }
    else
    {
      total += chunk;
      unpacked += status;
    }


    for(i = 0; i < nif; i++) 
    {
      double thissum_real=0, thissumsqr_real=0, thissum_imag=0, thissumsqr_imag=0;
      for(k = 0; k < chunk; k++)
      {
	thissum_real += creal(cdata[i][k]);
	thissum_imag += cimag(cdata[i][k]);
	thissumsqr_real += creal(cdata[i][k])*creal(cdata[i][k]);
	thissumsqr_imag += cimag(cdata[i][k])*cimag(cdata[i][k]);
      }
      sum[i*2] += thissum_real;
      sumsqr[i*2] += thissumsqr_real;
      sum[i*2+1] += thissum_imag;
      sumsqr[i*2+1] += thissumsqr_imag;
    }
  }

  fprintf(stderr, "%lld / %lld samples unpacked\n", unpacked, total);

  printf("\nCh    RMS   Mean\n");
	  
  for(i = 0; i < nif; i++)
  {
    double mean = sum[i*2]/total;
    double stddev = sqrt(sumsqr[i*2]/total - mean*mean);
    printf("%2d  %.3f  %.4f\n", i, stddev, mean);
  }

  for(i = 0; i < nif; i++)
  {
    free(cdata[i]);
  }
  free(cdata);
  free(sum);
  free(sumsqr);
}



double std_dev2(double a[], int n)
{
    int i;
    if(n == 0)
        return 0.0;
    double sum = 0;
    double sq_sum = 0;
    for(i = 0; i < n; ++i) {
       sum += a[i];
       sq_sum += a[i] * a[i];
    }
    double mean = sum / n;
    double variance = sq_sum / n - mean * mean;
    return sqrt(variance);
}


int bstate(const char *filename, const char *formatname, int nframes, long long offset, int evenodd)
{
	struct mark5_stream *ms;
	int nstates;
	int docomplex;

	ms = new_mark5_stream_absorb(
		new_mark5_stream_file(filename, offset),
		new_mark5_format_generic_from_string(formatname) );

	if(!ms)
	{
		printf("Error: problem opening %s\n", filename);

		return 0;
	}

	mark5_stream_print(ms);

	if(ms->iscomplex) 
	{
		printf("Complex decode\n");
		docomplex = 1;
		//chunk = nchan;
	}
	else
	{
		docomplex = 0;
		//chunk = 2*nchan;
	}

       /*53601 is the max no. of frames for Mark5B that would overflow bstate storage in worst case */
        if(nframes <= 0 || nframes > 53601)
        {
                printf("\nWARNING: nframes out of range, setting to 1000\n\n");
                nframes = 1000;
        }



        /* bstate 2nd dim. is either 2 for the 1bit: ++ -- or 4 for the 2 bits ++ + - -- */
	if(ms->nbit == 8 || ms->nbit == 16)
	{
	  if (docomplex) {
	    process_8bit_complexdata(ms, nframes);
	  } else {
	    process_8bit_realdata(ms, nframes);
	  }
	}
	else
	{
	  if(ms->nbit == 1) 
	  {
	    nstates = 2;
	  }
	  else if(ms->nbit == 2)
	  {
	    nstates = 4;
	  }
	  else 
	  {
	    printf("Error: unsupported bit sampling %d, must be either 1 or 2\n", ms->nbit);

	    return 0;
	  }

	  if(docomplex) {
	    process_complexdata(ms, nframes, nstates, evenodd);
	  } else {
	    process_realdata(ms, nframes, nstates, evenodd);
	  }
	}

	delete_mark5_stream(ms);

	return 0;
}

int main(int argc, char **argv)
{
	long long offset = 0;
	int evenodd = 0x03;	/* a bit field: 0x01 implies consider even samples, 0x02 for odd */
	int nframes;
	struct sigaction new_sigint_action;

	new_sigint_action.sa_handler = siginthand;
	sigemptyset(&new_sigint_action.sa_mask);
	new_sigint_action.sa_flags = 0;
	sigaction(SIGINT, &new_sigint_action, &old_sigint_action);

	if(argc == 2)
	{
		struct mark5_format *mf;
		int bufferlen = 1<<11;
		char *buffer;
		FILE *in;
		int r;

		if(strcmp(argv[1], "-h") == 0 ||
		   strcmp(argv[1], "--help") == 0)
		{
			return usage(argv[0]);
		}

		in = fopen(argv[1], "r");
		if(!in)
		{
			fprintf(stderr, "Error: cannot open file '%s'\n", argv[1]);
			return 0;
		}

		buffer = malloc(bufferlen);

		r = fread(buffer, 1, bufferlen, in);
		if(r < bufferlen)
		{
			fprintf(stderr, "Error: cannot read %d bytes from file\n", bufferlen);
			fprintf(stderr, "Error:   just read %d bytes from file\n", r);
			fclose(in);
			free(buffer);
			return -1;
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

	else if(argc < 4)
	{
		return usage(argv[0]);
	}

	nframes  = atol(argv[3]);

	if(argc > 4)
	{
		offset=atoll(argv[4]);
	}
	if(argc > 5)
	{
		if(strcasecmp(argv[5], "O") == 0)
		{
			evenodd = 0x02;
		}
		else if(strcasecmp(argv[5], "E") == 0)
		{
			evenodd = 0x01;
		}
		else
		{
			fprintf(stderr, "Error: the even/odd argument must be 'E' or 'O'.\n");
			return -1;
		}
	}

	bstate(argv[1], argv[2], nframes, offset, evenodd);

	return 0;
}

