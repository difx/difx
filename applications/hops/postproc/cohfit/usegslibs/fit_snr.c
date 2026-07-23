/************************************************************************/
/*                                                                      */
/* Fits a theoretical curve of SNR versus segmentation time             */
/*                                                                      */
/*      Inputs:         codatum         structure of scan  data         */
/*                      npt             number of array elements        */
/*      Output:         codatum         struct. filled in with snr fit  */
/*                      return value    seglen of function peak         */
/*                                                                      */
/* This does a simple 3-pt parabolic interpolation in log-linear space  */
/* near at the highest SNR found; renamed time->peakslen for clarity.   */
/* The interface was changed to return a double for some clarity        */
/*                                                                      */
/* Created October 6 1995 by CJL/SSD                                    */
/* Hacked up April 26 2026 by GBC                                       */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "cohfit.h"

double fit_snr(cosumary *codatum, int npt)
{
    float loglen[MAX_NSEGLEN];
    int i,k;                        /* first: k is index of peak SNR */
    float peak,peakslen,logtime;
    float r,s,t,a,b,c,x,y,z;

    msg("", 1);
    msg("Doing the three-point SNR fit for maximum", 2);

    peak=-1.0;
    for(i=0;i<npt;i++) {
        loglen[i]=log10(codatum->seglen[i]);                                
        if ((codatum->snr[i])>peak) {
             peak=(codatum->snr[i]);
             k = i;
        }
    }
    a=loglen[k]; z= (codatum->snr[k]);
    if (k==0) {                     /* edge case: low end */
        b=loglen[k+1]; y = (codatum->snr[k+1]);
        c=loglen[k+2]; x = (codatum->snr[k+2]);
        codatum->snr_peak[0] = 0;
        codatum->snr_peak[1] = 1;
        codatum->snr_peak[2] = 2;
    } else if (k==(npt-1)) {        /* edge case: high end */
        b=loglen[k-1]; y = (codatum->snr[k-1]);
        c=loglen[k-2]; x = (codatum->snr[k-2]);
        codatum->snr_peak[0] = npt-3;
        codatum->snr_peak[1] = npt-2;
        codatum->snr_peak[2] = npt-1;
    } else if ((k>0)&&(k<(npt-1))) {
        b=loglen[k-1]; y = (codatum->snr[k-1]);
        c=loglen[k+1]; x = (codatum->snr[k+1]);
        codatum->snr_peak[0] = k-1;
        codatum->snr_peak[1] = k;
        codatum->snr_peak[2] = k+1;
    } else {
        msg("Developer error in fit_snr().", 3);
        return(-1);
    }
    peakslen = codatum->seglen[k];
    msg("3pt peak of %f near [0|%d,%d,%d|%d] seglen = %f", 2, peak,
        codatum->snr_peak[0], codatum->snr_peak[1], codatum->snr_peak[2],
        npt-1, peakslen);

                                    /* parabola is y=rx^2 + sx + t */
    r = (a*(x-y) +b*(z-x) + c*(y-z))/(a*a*(b-c)+b*b*(c-a)+c*c*(a-b));
    s = ((y-x) - r*(b*b-c*c))/(b-c);
    t = z - r*a*a - s*a;

                                    /* Peak is at x = -s/(2r) */
    logtime = -s/(2*r);
    peakslen = pow(10.0,logtime);
                                    /* Is this value reasonable? */
                                    /* Sanity checks.            */
                                    /* Look for concave up parabola, time */
                                    /* too small or too big.        */
    if ((r>=0.0) || (peakslen<1) || (peakslen>2*(codatum->seglen[npt-1]))) {
        if (r>=0.0) msg("SNR parabola points wrong way.", 2);
        if (peakslen<1) msg("SNR peak time is less than one second.", 2);
        if (peakslen>2*(codatum->seglen[npt-1]))
            msg("SNR peak exceeds max seglength (%d).", 2,
                codatum->seglen[npt-1]);
        return(-2);
    }
    msg("3pt parabolic fit at %f seglen", 2, peakslen);
    
    for(i=0;i<npt;i++)              /* evaluate the full parabola */
        codatum->fitsnr[i] = r*pow(loglen[i],2.0) + s*loglen[i] + t;
    codatum->didfits |= FITOPT_SNR_3PT;
    codatum->iteratio[FITOPT_NDX_3PT] = 1;
    codatum->redchisq[FITOPT_NDX_3PT] = 47.0;   /* flags this done */
    return(peakslen);
}      

/*
 * eof vim:nospell
 */
