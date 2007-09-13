#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <fftw3.h>
#include <difxio.h>

#define REAL 0
#define IMAG 1
#define MAX_FREQ 16

const char program[] = "fringetool";
const char version[] = "1.1";
const char verdate[] = "20070913";
const char author[]  = "Brisken, w/Whiteis, sharing the blame";

int usage(const char *pgm)
{
	fprintf(stderr, "%s ver. %s   %s %s\n\n", program, version,
		author, verdate);
	fprintf(stderr, "usage : %s <visibility file> <input file>\n\n", pgm);

	return 0;
}

int fringe(const char *v_filename, const char *i_filename)
{
        DifxVisRecord *vis; /* visibility record structure */
	DifxParameters *dp; /* input parameters structure */
	int i, j;
	int k;
	int v;
	int chan = 999;
	int nchan;
	int mjd;
	int sec;
	int nfreq;
	double bw[MAX_FREQ];
	double peak;
	double max_pk;
	double phase = 1000.0;
	double delay;
	fftw_complex *i_data, *o_data; /* FFT data */
	fftw_plan f_plan;              /* "the plan" */

	FILE *out;

	/* load the input parameter from file */
	dp = newDifxParametersfromfile(i_filename);
	/* search for #channels */
	i = DifxParametersfind(dp, 0, "NUM CHANNELS");
	nchan = atoi(DifxParametersvalue(dp, i));
	printf("%s = %d\n", dp->rows[i].key, nchan);
	/* use frequency entries to loop and find corresponding BW */
	i = DifxParametersfind(dp, 0, "FREQ ENTRIES");
	nfreq = atoi(DifxParametersvalue(dp, i));
	printf("%s = %d\n", dp->rows[i].key, nfreq);
	for (i = 0; i < nfreq; i++)
	  {
	    /* Look for key with one index, print if found */
	    j = DifxParametersfind1(dp, 0, "BW (MHZ) %d", i);
	    bw[i] = atoi(DifxParametersvalue(dp, j));
	    /*	    printf("%s = %f\n", dp->rows[j].key, bw[i]); */
	  }
	/* initialize visibility record parsing */
	vis = newDifxVisRecord(v_filename, nchan);
	if(!vis)
	{
		fprintf(stderr, "fringtool: cannot init visibility parsing\n");
		return -1;
	}

	/* allocate space for "the plan" */
	i_data = (fftw_complex *)fftw_malloc(sizeof(fftw_complex)*nchan);
	o_data = (fftw_complex *)fftw_malloc(sizeof(fftw_complex)*nchan);
	if (!i_data || !o_data)
	  {
	    printf("Cannot allocate necessary fftw data structures\n");
	    return (-1);
	  }
	
	/* Create a plan for this channel count */
	f_plan = fftw_plan_dft_1d(nchan,   /* #channels */ 
				  i_data,  /* input data */
				  o_data,  /* output data */
				  FFTW_FORWARD, /* Forward xform */
				  FFTW_ESTIMATE); 

	/* Open our output file */
	out = fopen("vis.out", "w");

	/* Loop for as long as necessary:
	 * - reading visibility records
	 * - taking the FFT
	 * - finding the peak amplitude/phase
         * - writing sanitized results to file
         */
	 
	fprintf(out, "# Time         BL   Pol  FQ     Delay  Phase      Amp\n");
	fprintf(out, "# (mjd)                         (nSec) (deg)\n");
	for(;;)
	{
	  int bl, fi;
	  int lag_bin;
	  const char *p;
	  v = DifxVisRecordgetnext(vis);
	  if(v < 0)
	    {
	      break;
	    }
	  /* Save MJD */
	  i = DifxParametersfind(vis->params, 0, "MJD");
	  mjd = atoi(DifxParametersvalue(vis->params, i));
	  i = DifxParametersfind(vis->params, 0, "SECONDS");
	  sec = atoi(DifxParametersvalue(vis->params, i));
	  /* Get baseline, frequency index and polarity */
	  i = DifxParametersfind(vis->params, 0, "BASELINE NUM");
	  bl = atoi(DifxParametersvalue(vis->params, i));
	  i = DifxParametersfind(vis->params, 0, "FREQ INDEX");
	  fi = atoi(DifxParametersvalue(vis->params, i));
	  i = DifxParametersfind(vis->params, 0, "POLARISATION PAIR");
	  p = DifxParametersvalue(vis->params, i);
#ifdef NOTNOW
	  bl = atoi(vis->params->rows[0].value); /* Baseline */
	  fi = atoi(vis->params->rows[5].value); /* Freq Idx */
	  p  = vis->params->rows[6].value;       /* polarity */
#endif
	  //	  printDifxParameters(vis->params);
	  for(k = 0; k < nchan; k++)
	    {
	      i_data[k][REAL] = creal(vis->visdata[k]); /*Save real */
	      i_data[k][IMAG] = cimag(vis->visdata[k]); /* Save imaginary */
	    }
	  fftw_execute(f_plan); /* do fft for this baseline, polarization, freq*/
	  /* Scan the output data, save the channel with max magnitude */
	  max_pk = 0.0;
	  for (k = 0; k < nchan; k++)
	    {
	      /* fprintf(out, "%f %f \n", o_data[k][REAL], o_data[k][IMAG]);*/
	      peak = sqrt((o_data[k][REAL]*o_data[k][REAL]) +
			       (o_data[k][IMAG]*o_data[k][IMAG]));
	      if (peak > max_pk)
		{
		  max_pk = peak;
		  phase = (180.0/M_PI)*atan2(o_data[k][IMAG],o_data[k][REAL]);
		  chan = k;
		}
	    }
	  /* channels are increasing + up until (nchan-1)/2 and decreasing - 
	   * thereafter, which puts channel 0 at the center of the range.
           * This is due to conversion into lag space, post FFT.
           */  
	  if (chan > (nchan-1)/2)
	    lag_bin = chan - nchan;
	  else
	    lag_bin = chan;
	  delay = (lag_bin/bw[fi])*1e3;
	  fprintf(out, "%12.6f  %4d  %2s  %2d  %10.4lf %+7.3lf %8.3lf\n", 
		  mjd + sec/86400.0, bl, p, fi, delay, phase,  max_pk);
	}

	fclose(out);

	/* deallocate resources */
	deleteDifxVisRecord(vis);
	deleteDifxParameters(dp);
	fftw_destroy_plan(f_plan);
	fftw_free(i_data);
	fftw_free(o_data);

	return 0;
}

int main(int argc, char **argv)
{
	char *v_fname; /* visibility file */
	char *i_fname; /* input file */

	if(argc < 3)
	{
		return usage(argv[0]);
	}

	v_fname = argv[1];
        i_fname = argv[2];

	fringe(v_fname, i_fname);

	return 0;
}
