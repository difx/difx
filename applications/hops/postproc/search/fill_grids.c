/************************************************************************/
/*									*/
/* This routine manages the different residual rates and delays for	*/
/* a single scan as fed to the search program.  Since the data		*/
/* are sorted by rate and delay before we loop over the data, all we	*/
/* need to do here is make sure that the grid is regularly spaced.	*/
/* Then we put the SNR in the grid, and tabulate the axes of the grid.	*/
/*									*/
/*	Inputs:		srchdata	contains all data points for	*/
/*					this scan			*/
/*									*/
/*	Output:		srchdata	grids set up and filled for	*/
/*					all baselines			*/
/*									*/
/* Created February 1 1996 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "search.h"

#define VERY_BIG 1.0e99
#define REMLIMIT 0.10

int
fill_grids_orig (struct srchsummary *srchdata)
    {
    int i, j, nrate, ndelay, rcell, dcell, uneven;
    double rate, delay, min_rate, min_delay, max_rate, max_delay, snr;
    double lastrate, lastdelay, rdiff, ddiff, rem, remlimit;
    double rinc, dinc, fp_nrate, fp_ndelay, fp_rcell, fp_dcell;
    struct srchsummary *sb;
    char *rlp = getenv("HOPS_SEARCH_REMLIMIT");

    remlimit = rlp ? atof(rlp) : REMLIMIT;
    
					/* Loop over baselines */
    for (i=0; i<MAX_BNO; i++)
	{
					/* Did we reach the end? */
	if (srchdata[i].datum == NULL) break;
					/* convenience ptr */
	sb = srchdata + i;
					/* Data are sorted, so can just */
					/* loop through looking for smallest */
					/* nonzero incr. in rate and delay */
					/* to determine grid spacing */  
					/* Get grid dimensions from min/max */
	min_rate = max_rate = lastrate = sb->darray[0]->delay_rate;
	min_delay = max_delay = lastdelay = sb->darray[0]->mbdelay;
	rdiff = ddiff = 0.0;
	rinc = dinc = VERY_BIG;
	msg("Baseline %d has %d data", 2, i, sb->nd);
	for (j=0; j<sb->nd; j++)
	    {
	    rate = sb->darray[j]->delay_rate;
	    delay = sb->darray[j]->mbdelay;
					/* Check delay/rate incrs. & extrema */
					/* Pre-sorted data makes this easy, */
					/* but must still beware of missing */
					/* data points which may confuse a */
					/* quick'n'dirty look @ start & end */
	    rdiff = fabs (rate - lastrate);
	    ddiff = fabs (delay - lastdelay);
	    if ((rdiff > 0.0) && (rdiff < rinc))
		{rinc = rdiff; printf("rinc = %.9lf\n",rinc);}
	    if ((ddiff > 0.0) && (ddiff < dinc))
		{dinc = ddiff; printf("dinc = %.9lf\n",dinc);}
					/* Extrema */
	    if (rate < min_rate) min_rate = rate;
	    if (rate > max_rate) max_rate = rate;
	    if (delay < min_delay) min_delay = delay;
	    if (delay > max_delay) max_delay = delay;
					/* Set up for next loop */
	    lastrate = rate;
	    lastdelay = delay;
	    }
					/* See if we got sensible answers */
	if (min_rate == max_rate)
	    sb->nrate = 1;
	else
	    {
					/* Calculate number of rate cells */
	    printf ("max_rate=%lf  min_rate=%lf  ",max_rate,min_rate);
	    printf ("rinc = %lf\n", rinc);
	    fp_nrate = (max_rate - min_rate) / rinc + 1.0;
	    printf ("fp_nrate = %lf\n",fp_nrate);
	    if ((nrate = fp_nrate+0.5) > MAX_NRATE)
		{
		msg ("Too many rate cells, %d", 2, nrate);
		return (1);
		}
	    printf ("nrate = %d\n",nrate);
					/* Check that it was integer number */
	    rem = fabs (fp_nrate - (double)nrate);
	    printf ("rem (rate) = %lf\n",rem);
/*	    if (rem > remlimit)			This fails, needs replacing 
		{
		msg ("Rate spans non-integral number of rate increments", 2);
		return (1);
		} */
	    }
					/* Same for delay */
	if (min_delay == max_delay)
	    sb->ndelay = 1;
	else
	    {
					/* Calculate number of delay cells */
  	    printf ("max_delay=%lf  min_delay=%lf  ",max_delay,min_delay);
	    printf ("dinc = %lf\n",dinc);
	    fp_ndelay = (max_delay - min_delay) / dinc + 1.0;
	    printf ("fp_ndelay = %lf\n",fp_ndelay);
	    if ((ndelay = fp_ndelay+0.5) > MAX_NDELAY)
		{
		msg ("Too many delay cells, %d", 2, ndelay);
		return (1);
		}
 	    printf ("ndelay = %d\n",ndelay);

					/* Check that it was integer number */
 	    
	    rem = fabs (fp_ndelay - (double)ndelay);
	    printf ("rem (delay) = %lf\n",rem);
/*	    if (rem > remlimit)
		{
		msg ("Delay spans non-integral number of delay increments", 2);
		return (1);
		}
*/
	    }
					/* Record the rate/delay dimensions */
					/* of grid */
	sb->nrate = nrate;
	sb->ndelay = ndelay;
	sb->min_rate = min_rate;
	sb->max_rate = max_rate;
	sb->min_delay = min_delay;
	sb->max_delay = max_delay;
					/* Reset increments to be sure */
	rinc = (max_rate - min_rate) / ((double)nrate - 1.0);
	dinc = (max_delay - min_delay) / ((double)ndelay - 1.0);
					/* Ready to fill in the grid */
	uneven = FALSE;
	for (j=0; j<sb->nd; j++)
	    {
	    rate = sb->darray[j]->delay_rate;
	    delay = sb->darray[j]->mbdelay;
	    snr = sb->darray[j]->snr;
					/* Calculate grid coords, make */
					/* sure they are integer */
	    fp_rcell = (rate - min_rate) / rinc;
	    rcell = fp_rcell+0.5;
	    rem = fabs (fp_rcell - (double)rcell);
	    if (rem > remlimit) uneven = TRUE;

	    fp_dcell = (delay - min_delay) / dinc;
	    dcell = fp_dcell+0.5;
	    rem = fabs (fp_dcell - (double)dcell);
	    if (rem > remlimit) uneven = TRUE;
					/* Insert snr value */
	    sb->snr[rcell][dcell] = snr;
	    }
	if (uneven)
	    {
	    msg ("Uneven delay distribution", 2);
	    msg ("Set HOPS_SEARCH_REMLIMIT > %g if you are desperate",
		2, remlimit);
	    return (1);
	    }
	}

    return (0);
    }
