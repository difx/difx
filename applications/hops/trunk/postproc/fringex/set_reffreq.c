/************************************************************************/
/*									*/
/* This routine combines the command-line information and the 		*/
/* frequencies actually found in the input type-2 file to set the	*/
/* reference frequency for the output data.				*/
/*									*/
/*	Inputs:		fxp		partially filled in		*/
/*									*/
/*	Output:		return value	reference frequency		*/
/*									*/
/* Created October 12 1995 by CJL					*/
/*									*/
/************************************************************************/
#include <math.h>
#include "fringex.h"

double
set_reffreq (fxp)
struct fxparam *fxp;
    {
    double freq, freqsum, freqmean, freqdiff;
    int ch, closest, nfreq;
					/* Find frequency mean and nearest */
					/* channel.  The fchan array entries */
					/* are non-negative only if data */
					/* exists for that channel */
    freqsum = 0.0; closest = -1; nfreq = 0;
    for (ch=0; ch<NFX_FCHAN; ch++)
	{
	if (fxp->fchan[ch] < 0.0) continue;
	freqsum += fxp->fchan[ch];
	nfreq++;
	}
    freqmean = freqsum / nfreq;
    freqdiff = freqmean;
    for (ch=0; ch<NFX_FCHAN; ch++)
        {
	if (fxp->fchan[ch] < 0.0) continue;
        if (fabs (freqmean - fxp->fchan[ch]) < freqdiff)
            {
            freqdiff = fabs (freqmean - fxp->fchan[ch]);
            closest = ch;
            }
        }
                                        /* Set ref freq, this is default */
    freq = fxp->ffit_reffreq;
                                        /* User override, check within */
                                        /* factor of 2 */
    if ((fxp->userfreq < fxp->ffit_reffreq*2.0) 
				&& (fxp->userfreq > fxp->ffit_reffreq*0.5))
        freq = fxp->userfreq;
                                        /* userfreq -1, nearest channel */
                                        /* Default is fourfit ref freq */
    else if (fxp->userfreq == -1.0)
        {
        if (closest >= 0) freq = fxp->fchan[closest];
        }
                                        /* userfreq -2, use actual mean */
    else if (fxp->userfreq == -2.0) freq = freqmean;

    return (freq);
    }
