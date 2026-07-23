/************************************************************************/
/*									*/
/* Given a grid of SNR by rate and delay, this routine finds the peak,	*/
/* then does a 2-dimensional parabolic interpolation to refine the peak	*/
/* location and height between grid points.  It also assesses the 	*/
/* probability of false detection for a single-baseline search, and	*/
/* checks the SNR statistics for consistency with theoretical		*/
/* expectations.  Based on what it finds, it assigns a numerical code	*/
/* for the quality of the fringes that were found.  It repeats the fit	*/
/* procedure for all baselines in the input structure array.		*/
/*									*/
/*	Inputs:		srchdata	Contains grid of SNR values for	*/
/*					each baseline			*/
/*									*/
/*	Output:		srchdata	datum fields filled in with fit	*/
/*					for each baseline		*/
/*			return value	0=success, else failure		*/
/*									*/
/* Created February 5 1996 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "search.h"

int
fit_peaks (struct srchsummary *srchdata)
    {
    int i, j, k, rcell, dcell;
    double max_snr, fp_rcell, fp_dcell, rpos, dpos, fit_snr, snr_ratio;
    double ratediff, ratestep, delaydiff, delaystep;
    struct srchsummary *sb;
    extern int parabola (double y0, double y1, double y2, double* x_max, double* y_max);
    extern char* fringename(fringesum *fsumm);

    for (i=0; i<MAX_BNO; i++)
	{
					/* Did we reach the end? */
	if (srchdata[i].datum == NULL) break;
					/* convenience ptr */
	sb = srchdata + i;
					/* Find peak grid point */
	max_snr = 0;
	for (j=0; j<sb->nrate; j++)
	    for (k=0; k<sb->ndelay; k++)
		{
		if (max_snr < sb->snr[j][k])
		    {
		    rcell = j;
		    dcell = k;
		    max_snr = sb->snr[j][k];
		    }
		}
	msg("Peak cell is %d,%d", 1, rcell, dcell);
					/* Interpolate in 2 directions */
	snr_ratio = 1.0;
	if ((rcell == 0) || (rcell == sb->nrate - 1))
	    {
					/* Fringes at window-edge */
	    fp_rcell = rcell;
	    sb->datum->quality = 'E';
	    }
	else if (parabola (sb->snr[rcell-1][dcell], 
			sb->snr[rcell][dcell],
			sb->snr[rcell+1][dcell], 
			&rpos, &fit_snr) != 0)
	    {
	    msg ("Fit failed for %s", 2, fringename (sb->datum));
	    continue;
	    }
	else 
	    {
	    fp_rcell = (double)rcell + rpos;
	    snr_ratio = fit_snr / max_snr;
	    }

	if ((dcell == 0) || (dcell == sb->ndelay - 1))
	    {
					/* Fringes at window-edge */
	    fp_dcell = dcell;
	    sb->datum->quality = 'E';
	    }
	else if (parabola (sb->snr[rcell][dcell-1], 
			sb->snr[rcell][dcell],
			sb->snr[rcell][dcell+1], 
			&dpos, &fit_snr) != 0)
	    {
	    msg ("Fit failed for %s", 2, fringename (sb->datum));
	    continue;
	    }
	else 
	    {
	    fp_dcell = (double)dcell + dpos;
	    snr_ratio *= fit_snr / max_snr;
	    }
					/* Fill in the datum values */
	ratediff = sb->max_rate - sb->min_rate;
	if (sb->nrate > 1) ratestep = ratediff / (double)(sb->nrate - 1);
	else ratestep = 0;
	sb->datum->delay_rate = sb->min_rate + (fp_rcell * ratestep);
	delaydiff = sb->max_delay - sb->min_delay;
	if (sb->ndelay > 1) delaystep = delaydiff / (double)(sb->ndelay - 1);
	else delaystep = 0;
	sb->datum->mbdelay = sb->min_delay + (fp_dcell * delaystep);
					/* SNR corrected for peak fitting */
	sb->datum->snr = max_snr * snr_ratio;
					/* Skip any fancy quality factor */
					/* stuff for now ... put in */
					/* something dumb */
	msg("Peak SNR is %g at %g,%g", 1,
	    sb->datum->snr, sb->datum->delay_rate, sb->datum->mbdelay);
	if (sb->datum->snr < 4.0) sb->datum->quality = '0';
	else if (sb->datum->quality == 'E')  ;
	else if (sb->datum->snr < 4.3) sb->datum->quality = '3';
	else if (sb->datum->snr < 4.4) sb->datum->quality = '4';
	else if (sb->datum->snr < 4.5) sb->datum->quality = '5';
	else if (sb->datum->snr < 4.6) sb->datum->quality = '6';
	else if (sb->datum->snr < 4.7) sb->datum->quality = '7';
	else if (sb->datum->snr < 4.8) sb->datum->quality = '8';
	else sb->datum->quality = '9';
	}
					/* Ignore error returns for now */
    return (0);
    }
