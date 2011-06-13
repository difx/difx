/************************************************************************/
/*									*/
/* In cases where looping is requested in "natural" data units, the	*/
/* looping parameters cannot be set until the data file has been read	*/
/* into memory.  This routine uses the data in memory to complete the	*/
/* processing of the user's looping request, placing the results into	*/
/* the loops structure.							*/
/*									*/
/*	Inputs:		fxp		Main data structure		*/
/*			loops		partially filled in		*/
/*									*/
/*	Output:		loops		All filled in			*/
/*			return value	0=OK, 1=bad			*/
/*									*/
/* Created February 7 1996 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "fringex.h"

int
set_loops (fxp, loops)
struct fxparam *fxp;
struct loops *loops;
    {
    int nap, i, j, nat_nrates, nat_ndelays;
    double time, ratebeam, raterange, ratestep;
    double delaybeam, delayrange, delaystep;
    static int nnsec, nrates, ndelays;
    static int first = TRUE;
					/* Record original settings */
    if (first)
	{
	nnsec = loops->nnsec;
	nrates = loops->nrates;
	ndelays = loops->ndelays;
// 	printf("At start of set_loops: nnsec=%d   nrates=%d   ndelays=%d\n",nnsec,nrates,ndelays);
	first = FALSE;
	}
					/* One-pass modes to pick up coherence */
					/* times, as determined by cofit */
    if (fxp->mode & SEARCH)
        {
        nap = fxp->srch_cotime / fxp->acc_period + 0.5;
        if (nap == 0) nap = 1;
        loops->nsecs[0] = nap * fxp->acc_period;
        loops->nnsec = 1;
        }
    else if (fxp->mode & NOLOSS)
        {
        nap = fxp->noloss_cotime / fxp->acc_period;
        if (nap == 0) nap = 1;
        loops->nsecs[0] = nap * fxp->acc_period;
        loops->nnsec = 1;
        }
					/* Multiple segmentation times, for input */
					/* to cofit */
    if (nnsec < 0)
        {
        loops->nnsec = -nnsec;
	j = 0;
        for (i=0; i<loops->nnsec; i++)
            {
            time = loops->nsecs[i] * fxp->acc_period;
            if (time >= fxp->adata.duration) break;
					/* Until afile supports sub-second */
					/* intervals, ignore short times */
	    if (time >= 1.0) 
		{
		loops->nsecs[j] = time;
		j++;
		}
            }
        loops->nnsec = j;
        }
					/* Special mode to use A-file rates */
					/* and delays */
    if (fxp->mode & SRCHPOS)
	{
	nrates = loops->nrates = loops->ndelays = 1;
	loops->rates[0] = fxp->rate;
	loops->delays[0] = fxp->delay * 1.0e3;
/* printf("rate=%f, delay=%f\n", fxp->rate,  fxp->delay); */
	}
					/* Multiple rates and delays, for input */
					/* to search */
    if (nrates <= 0)
	{
					/* Guard against screwups */
	if ((ndelays > 0) || (nnsec != 1))
	    {
	    msg ("Bad automatic looping parameters in set_loops (%d %d %d)", 2,
			nrates, ndelays, nnsec);
	    return (1);
	    }
					/* Figure out rate resolution (ps/s) */
					/* and Nyquist sample */
// 	printf("loops->nsecs[0] = %lf\n", loops->nsecs[0]);
	ratebeam = 1.0e6 / (fxp->ffit_reffreq * loops->nsecs[0]);
	raterange = ratebeam * loops->nsecs[0] / fxp->acc_period;
	ratestep = ratebeam / 2.0;
// 	printf("ratebeam = %f   raterange = %f   ratestep = %f\n",ratebeam,raterange,ratestep);

					/* Figure out delay resolution (ns) */
					/* and Nyquist sample */
	// printf("fxp->bandwidth = %lf\n",fxp->bandwidth);
	delaybeam = 1000.0 / fxp->bandwidth;
	delayrange = 1000.0 * fxp->adata.ambiguity;
	delaystep = delaybeam / 2.0;
	// printf("delaybeam = %f   delayrange = %f   delaystep = %f\n",delaybeam,delayrange,delaystep);
					/* Check to see if search window needs */
					/* trimming to fit natural window */
					/* Add 1 so can see cycling on plots */
	nat_nrates = raterange / ratestep + 1.5; 
	loops->nrates = -nrates;
	if (loops->nrates > nat_nrates)
	    {
	    msg ("Reduced rate window to natural size of %d cells", 2, nat_nrates);
	    msg ("rate beam=%f, range=%f\n", 0,ratebeam, raterange);
	    loops->nrates = nat_nrates;
	    }
	else if (loops->nrates == 0)
	    {
	    msg ("Natural rate window is %d cells", 2, nat_nrates);
	    loops->nrates = nat_nrates;
	    }
	nat_ndelays = delayrange / delaystep + 1.5; 
	loops->ndelays = -ndelays;
	if (loops->ndelays > nat_ndelays)
	    {
	    msg ("Reduced delay window to natural size of %d cells", 2, nat_ndelays);
	    msg ("delay beam=%f, range=%f\n", 0,delaybeam, delayrange);
	    loops->ndelays = nat_ndelays;
	    }
	else if (loops->ndelays == 0)
	    {
	    msg ("Natural delay window is %d cells", 2, nat_ndelays);
	    loops->ndelays = nat_ndelays;
	    }
					/* Fill in the looping values */
	for (i=0; i<loops->nrates; i++)
	    loops->rates[i] = ratestep * (i - loops->nrates/2);
	for (i=0; i<loops->ndelays; i++)
	    loops->delays[i] = delaystep * (i - loops->ndelays/2);
	}

    return (0);
    }
