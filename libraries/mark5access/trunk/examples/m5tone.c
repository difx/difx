/* This file: m5tone.c
 * 15/06/11
 *
 * Reads a mk5 file 
 * Forms spectrum with FFTW and reports the tone position.
 *
 * Compile and link on fxmanager (frontend) as oper with:
 *
gcc mark5tone.c -c \
    -I/cluster/difx/DiFX-trunk_64/svn/libraries/mark5access/trunk \
    -I/cluster/pgplot

gcc mark5tone.o -o mark5tone \
-L/cluster/difx/DiFX-trunk_64/svn/libraries/mark5access/trunk/mark5access/.libs \
-lmark5access -lfftw3 -lm -lc -lpng -lX11 -lgfortran \
-L/cluster/pgplot -lcpgplot -lpgplot


To execute:
dtrunk      (sets environment variable for ld to find libmark5access.so.0)
./m5tone

User guide on fxmanager:
/cluster/difx/DiFX-trunk_64/svn/libraries/mark5access/trunk/doc/UserGuide

*/


#include <stdio.h>
#include <stdlib.h>
#include <mark5access.h>
#include <complex.h>
#include <fftw3.h>
#include <math.h>
#include <cpgplot.h>

#define MJD2011JAN01 55562  /* MJD of Jan 1st, 2011 */
#define YEAR 2011
#define LEAPYEAR 0
#define AP 0.02             /* Length of an accumulation period / s */
#define STNAME "AP"         /* Station code for writing in PCAL table */
#define SCALEFACTOR 0.01    /* PCAL amplitude scale factor needed to 
                               keep fourfit happy */
#define NTONE 16            /* Number of tones per band */
#define TONEFREQ 229344     /* Top tone freq, others go down 1 MHz per tone */
/*#define TONECHAN 38912 */     /* Channel number in PFB chan 8 containint tone */
#define TONECHAN 16384      /* Channel number in PFB chan 8 containing tone */
#define CHAN 0

#define HEADER 1            /* Print to stdout the input file parameters */
#define PCAL 0              /* Output file in PCAL format for difx2mark4 */
#define WVRDATA 0           /* Output file in wvr.data format for fourfit.wvr */
#define BSTATE 1            /* Print state counts? */
#define SPECTRUM 1          /* Report accumulated spectrum of the PFB output? */
#define PLOT 1              /* Make pgplot of spectrum? */
#define COHERENCE 0         /* Report the vector average tone amp / scalar average amp? */
#define DEBUG 0

double getSomeData(const char *filename, int chan, int nsamp, int nloop);
void day2date(int doy, int *month, int *day);

int main(int argc, char **argv)
{
  //int nsamp = 131072;       /* Number of spectral points in FFT */
  int nsamp = 16384;
  int nloop = 488*3;       /* 488 spectra per s, 60 s */

  int chan = CHAN;

  if (argc < 2) {
    printf("Usage: mark5tone <fname of m5b file>\n");
    exit(1);
  }
  
  if (argc == 3)
  {
	chan =  atoi(argv[2]);
  }
	

  getSomeData(argv[1], chan, nsamp, nloop);

  return 0;
}


/* nsamp = number of samples to read from each channel 
           for each call to mark5_stream_decode
   nloop = number of times to: call mark5_stream_decode to get nsamp of data
                               process the data
*/
double getSomeData(const char *filename, int chan, int nsamp, int nloop)
{
  int offset;	/* start reading at the start of the file */
  float **data;	/* place to accumulate data */
  int l, i, j, n;
  char c;
  int endoffile = 0;


/* Declare a pointer to the mark5_stream structure that will 
 * be used throughout the example */

  struct mark5_stream *ms;


/* Declare fftw things */

  fftw_complex *in, *out;
  fftw_plan pf;


/* Declare bstate storage */

  long bstate[nsamp][4];


/* Declare spectrum storage */

  complex spectrum[nsamp];
  float freq[nsamp], amp[nsamp], phase[nsamp];
  char title[100];
  float maxamp;
  int maxfreq;
  double ampsum = 0;


/* Declare frame time storage */

  int mjd, sec;
  double ns, startmjd, APstartMJD, scanStartMJD;
  int h, m, s;


/* Declare storage for PCAL real, imag */

  FILE *fp, *fp2;
  char fname[100], fname2[100];
  complex pcal;


/* Declare storage for info for output wvr.data line */

  int year;
  int month;
  int day;
  int hour;
  int minute;
  double second;
  double t;


/* open a file stream with given filename
 * create a mark5b format structure
 * pass both to create an entire mark5_stream structure
 *
 * This is hardwired to open a 2048 Mbps stream with 16 channels
 * sampled at 2 bits with a decimation of 1.
 */



  ms = new_mark5_stream(
	new_mark5_stream_file(filename, offset),
	new_mark5_format_generic_from_string( "MKIV1_4-1024-8-2" ) ); 
//	new_mark5_format_generic_from_string( "Mark5B-2048-16-2" ) ); 

/* if successful, ms will point to the mark5_stream structure.
 * if not, all allocated resources will be freed and a null
 * pointer will be returned.
 */

  if (ms == 0) {
    fprintf(stderr, "Problem creating mark5_stream\n");
    return 0;
  }


/* optional : fix the mjd date ambiguity */

  mark5_stream_fix_mjd(ms, 55562);


/* optional : print to stdout the parameters of this structure */

  if (HEADER)
    mark5_stream_print(ms);


/* Allocate memory for output data array 
 * the number of channels to be read is stored as ms->nchan.
 */

  data = (float **)malloc(ms->nchan * sizeof(float **));
  for (j = 0; j < ms->nchan; j++) {
    data[j] = (float *)malloc(nsamp*sizeof(float));
  }

/* Loop over the calculation.  Decoding will continue from where the
 * previous decoding left off.  
 *
 * WARNING : depending on the format (and in this case, the fanout)
 * nsamp must be a multiple of 1, 2, 4, or 8
 */

  if (nsamp % ms->samplegranularity != 0) {
    fprintf(stderr, "WARNING -- decoding a nonstandard number "
		"of samples.  Expect bogus results\n");
  }
	 

/* Prepare fftw for transform with nsamp points*/

  in = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * nsamp);
  out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * nsamp);
  pf = fftw_plan_dft_1d(nsamp, in, out, FFTW_FORWARD, FFTW_ESTIMATE);


/* Initialize bstate storage to zero */

  for (i = 0; i < nsamp; i++)
    for (j = 0; j < 4; j++)
      bstate[i][j] = 0;


/* Initialize spectrum storage to zero */

  for (i = 0; i < nsamp; i++)
    spectrum[i] = 0.0 + 0.0 * I;


/* Initialize PCAL storage to zero */

  pcal = 0.0 + 0.0 * I;


/* Remember the start time of the first AP of the scan */

  mark5_stream_get_frame_time(ms, &mjd, &sec, &ns);
  APstartMJD = mjd + (sec + ns/1e9) / 86400.0;
  scanStartMJD = APstartMJD;



/* Prepare output file name in format 'doy-hhmmss_<ST>_PCAL' or 
 * 'doy-hhmmss_<ST>_wvr.data' */

  h = (int)(sec / 3600);
  m = (int)(sec - h * 3600) / 60;
  s = (int)(sec - h * 3600 - m * 60);
  if (PCAL)
    sprintf(fname, "%03d-%02d%02d%02d_AP_PCAL", mjd - MJD2011JAN01 + 1, h, m, s);
  if (WVRDATA)
    sprintf(fname, "%03d-%02d%02d%02d_AP_wvr.data", mjd - MJD2011JAN01 + 1, h, m, s);
  sprintf(fname2, "%s.inv", fname);


/* Open output file for PCAL */

  if (PCAL || WVRDATA) {
    fp = fopen(fname, "w");
    if (fp == NULL) {
      printf("Trouble opening PCAL output file %s\n", fname);
      exit(1);
    }
    fp2 = fopen(fname2, "w");
    if (fp == NULL) {
      printf("Trouble opening PCAL output file %s\n", fname2);
      exit(1);
    }
  }

/* Write header to output wvr.data and wvr.data.inv files */

  if (WVRDATA) {
    fprintf(fp, "# yyyy-MM-ddThh:mm:ss.ssssss Phase(degrees)\n");
    fprintf(fp2, "# yyyy-MM-ddThh:mm:ss.ssssss Phase(degrees)\n");
  }



/* Read our data */

  for (l = 0; l < nloop; l++) { 
/*  while (endoffile != 1) { */

/* Read the frame time at the start of this block of data */

    mark5_stream_get_frame_time(ms, &mjd, &sec, &ns);
    startmjd = mjd + (sec + ns/1e9) / 86400.0;


/* Read nsamp samples from each channel and fill into data */

    n = mark5_stream_decode(ms, nsamp, data);
    if (n == 0) {
      endoffile = 1;
      l = nloop;
    }


/* For channel 9 (Walter counts from zero, so j is 8) */

    j = chan;


/* For all samples */

    for (i = 0; i < nsamp; i++) {


/* Write the data from this channel to output */

//      printf("%f\n", data[j][i]); 

/* Increment the state counts
 * Values are: +3.3359: 11
 *                +1.0: 10
 *                -1.0: 01
 *             -3.3359: 00
 */

      if (data[j][i] < -3) bstate[j][0]++;
      if ((data[j][i] > -3) && (data[j][i] < 0)) bstate[j][1]++;
      if ((data[j][i] > 0) && (data[j][i] < 3)) bstate[j][2]++;
      if (data[j][i] > 3) bstate[j][3]++;

    }


/* Prepare for FFT */

/* Copy data from channel j into input array for fftw */

    for (i = 0; i < nsamp; i++) 
      in[i] = data[j][i] + 0.0 * I;


/* Copy test vector generator data into input array for fftw */
/*

//  for (i = 0; i < nsamp; i++)
 //   in[i] = 1.0 * cos((double)i /nsamp * 37700.0) + 0.0 * I;
*/


/* Transform to frequency domain */

    fftw_execute(pf);


/* Accumulate spectrum */

    for (i = 0; i < nsamp; i++)
      spectrum[i] += out[i];


/* Accumulate amplitude in channel TONECHAN (38912 or 26624)  */

    ampsum += cabs(out[TONECHAN]);


/* Accumulate PCAL re and im in channel 38912 */

    pcal += out[TONECHAN];


/* Report amp & phase in channel TONECHAN (38912 or 26624) containing tone */

    if (DEBUG)
      printf("%.9lf %f %lf %lf\n", startmjd - MJD2011JAN01 + 1, 
                               (float)l/64e6*nsamp, 
 	                       cabs(out[TONECHAN]),
                               carg(out[TONECHAN]) / M_PI * 180.0);


/* At end of each AP? */

    if (PCAL || WVRDATA) {
      if (fmod(ns/1e9, AP) < (double)nsamp / ms->samprate) {


/* Normalize pcal amplitude by number of points in spectrum 
   since fftw should but doesn't do this */

        pcal /= nsamp;


/* Normalize pcal amplitude also by SCALEFACTOR to get amplitudes 
   into range expected by fourfit */

        pcal *= SCALEFACTOR;


/* Write accumulated PCAL table row in format required by fourfit:

1) Antenna ID
2) decimal DOY
3) duration of the measurements (days)
4) cableCal (in picosecond)
5) no. of polarization (for geo: 1)
6) no. Band (number of sub-band with measurements: for geo 16)
7) No. of tone (no. of pulse cal tone detected per band per pol.
    (8 in X-band, 6 in S-band). 
8) no. State (no. of state counts measured per band
    per pol. -- should be zero)
9)no. recoded Channels (no. of recorded channels at time of measurement 
    (<= npol*nband).

Following these fields are two variable array of no. 
The first variable-length field is the 

Pulse cal data field consisting of nPol*nBand*nTone groups of four numbers:

1) recorder channel number (zero-based) corresponding to the measurement. 
2) tone sky frequency (MHz)
3) real and 
4) imaginary parts of the tone measured at the given sky frequency

The second variable-length field
is the state count data. For each band of each polarization, nState + 1
values are listed. The first number is the record channel number or -1 if
that polarization/band combination was not observed or monitored. The
remainder contain state counts.

*/
        if (PCAL) {
  	  fprintf(fp, "%s %011.7lf %9.7lf %4.2f %d %d %d %d %d",
	     STNAME,
             APstartMJD - MJD2011JAN01 + 1, 
             AP / 86400.0,
      	     0.0,
             1,
             16,
             32,
             0,
             16);

	  fprintf(fp2, "%s %011.7lf %9.7lf %4.2f %d %d %d %d %d",
	     STNAME,
             APstartMJD - MJD2011JAN01 + 1, 
             AP / 86400.0,
      	     0.0,
             1,
             16,
             32,
             0,
             16);

          for (i = 0; i < ms->nchan; i++)
            for (n = 0; n < 32; n++)
              fprintf(fp, "   %d %d %.5le %.5le",
	         0,
                 TONEFREQ - (i * 32 + n),
	         creal(pcal),
                 cimag(pcal));

          for (i = 0; i < ms->nchan; i++)
            for (n = 0; n < 32; n++)
              fprintf(fp2, "   %d %d %.5le %.5le",
	         0,
                 TONEFREQ - (i * 32 + n),
	         creal(pcal),
                 -1.0 * cimag(pcal));
	}

        if (WVRDATA) {

/* Write wvr.data file table row in format required by fourfit.wvr */

/* t = time in decimal DOY at the middle of the AP */

          t = APstartMJD - MJD2011JAN01 + 1;
          year = YEAR;
          day2date((int)t, &month, &day);
          hour = (int)((t - (int)t) * 24.0);
          minute = (int)(((t - (int)t) * 24.0 - hour)* 60.0);
          second = ((((t - (int)t) * 24.0 - hour) * 60.0 - minute) * 60.0);
          fprintf(fp, "%d-%02d-%02dT%02d:%02d:%06.3lf %6.2lf",
                  year,
                  month,
                  day,
                  hour,
                  minute,
                  second,
		  carg(pcal) / M_PI * 180.0);
          fprintf(fp2, "%d-%02d-%02dT%02d:%02d:%06.3lf %6.2lf",
                  year,
                  month,
                  day,
                  hour,
                  minute,
                  second,
		  -1.0 * carg(pcal) / M_PI * 180.0);
	}

        fprintf(fp, "\n");
        fprintf(fp2, "\n");
        fflush(fp);
        fflush(fp2);


/* Tell the user about our progress */

        printf("Wrote PCAL data for AP %.3lf s after scan start to file %s\n", 
               (APstartMJD - scanStartMJD) * 86400.0,
               fname);


/* Re-initialize things to get ready for the next AP */

        pcal = 0.0 + 0.0 * I;
        APstartMJD = startmjd;
      }
    }


/* Finished loop over all blocks of data */

  }


/* Report accumulated spectrum */

  if (SPECTRUM)
    for (i = 0; i < nsamp; i++) {
      freq[i] = i;
      amp[i] = cabs(spectrum[i] / sqrt(nsamp * nloop));
      phase[i] = carg(spectrum[i]);
      printf("%lf %lf %lf\n", freq[i], amp[i], phase[i]);
    }


/* Report coherence */

  if (COHERENCE)
    printf("Coherence = %lf\n", cabs(spectrum[TONECHAN]) / ampsum);


/* Report state counts */

  if (BSTATE) {
    printf("State counts:\n");
    printf(" 11: %5.2f \%\n", (float)bstate[j][0] / nsamp / nloop * 100.0);
    printf(" 10: %5.2f \%\n", (float)bstate[j][1] / nsamp / nloop * 100.0);
    printf(" 01: %5.2f \%\n", (float)bstate[j][2] / nsamp / nloop * 100.0);
    printf(" 00: %5.2f \%\n", (float)bstate[j][3] / nsamp / nloop * 100.0);
  }

/* Plot spectrum */
  if (PLOT) {
/* Open pgplot xwindow */

    if (cpgbeg(0, "/xwindow", 1, 2) < 1)
      printf("cpgplot could not open xwindow\n");
    else {

/* Set colour to white */

      cpgsci(1);                         

/* Set axis scaling */

      maxamp = 0;
      for (i = 1; i < nsamp; i++)
        if (amp[i] > maxamp) {
          maxamp = amp[i];
          maxfreq = i;
        }
    printf("Channel containing max = %d\n", maxfreq);
      cpgenv(0., nsamp/2, 0., maxamp * 1.2, 0, 0);

/* Plot lines */

      cpgline(nsamp/2, freq, amp);         

/* Label axes */

      sprintf(title, "PFB channel: %d  (counting from 1)", j + 1);
      cpglab("Frequency / channels", "Amplitude / arbitrary units", title);

/* Close plot window when finished */

      cpgclos();
    }
  }


/* Now clean up */

  for(j = 0; j < ms->nchan; j++) {
    free(data[j]);
  }
  free(data);


/* Cause file to be closed, memory freed */

  delete_mark5_stream(ms);


/* Release memory from fftw */

  fftw_destroy_plan(pf);
  fftw_free(in); 
  fftw_free(out);


/* Close output file */

  if (PCAL) {
    fclose(fp2);
  }
  return(0);
}


/* day2date.c
 * ALR 8/4/96
 *
 * converts day number to month & day in month
 * Inputs: day number in year
 *         whether year is leap year
 */

void day2date(int doy, int *month, int *day)
{
  int months[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
  int i;

/* enter correct number of days in Feb for leap year */

  months[1] += LEAPYEAR;

/* for each month from Jan upward,
 * subtract total number of days in month from day number in year,
 * until total <= 0
 */

  i = 0;
  while (doy > 0) doy -= months[i++];

/* back off one month, to make day number > 1 */

  i--;
  doy += months[i];

/* have got month in i (counting from zero), and day in month in day */

  *month = i + 1;
  *day = doy;

}

/* Example output wvr.data file format:

#2004-04-16 00:00:00 - 2004-04-16 23:59:59
# yyyy-MM-ddThh:mm:ss Phase(degrees)
2004-04-16T23:54:08 85.23
2004-04-16T23:54:14 84.39
2004-04-16T23:54:21 84.33

*/
