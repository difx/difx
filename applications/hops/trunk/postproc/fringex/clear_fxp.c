/************************************************************************/
/*                                                                      */
/* Initializes the fxp structure                                        */
/*                                                                      */
/*      Inputs:         fxp             undefined state                 */
/*                      mode            0=complete, 1=arrays only       */
/*                                                                      */
/*      Output:         fxp             initialized                     */
/*                                                                      */
/* Created October 10 1995 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "fringex.h"

void clear_fxp (struct fxparam *fxp, int mode)
    {
    int i;

    if (mode == ALL)
        {
        fxp->mode = 0;
        fxp->no_amp_corr = TRUE;
        fxp->raoff = 0.0;
        fxp->decoff = 0.0;
        fxp->rateoff = 0.0;
        fxp->delayoff = 0.0;
        fxp->nsecs = 0.0;

        fxp->ffit_reffreq = 0.0;
        fxp->reffreq = 0.0;
        fxp->reftime = 0.0;
        fxp->dprate = 0.0;
        fxp->acc_period = 0.0;
        }

    if ((mode == ALL) || (mode == FILES))
        {
        fxp->fringe = NULL;
        fxp->sdata[0] = NULL;
        fxp->sdata[1] = NULL;
        fxp->srch_cotime = 0.0;
        fxp->noloss_cotime = 0.0;
        fxp->rate = 0.0;
        fxp->delay = 0.0;
        fxp->bandwidth = 0.0;

        clear_fsumm (&(fxp->adata));
        }

    if ((mode == ALL) || (mode == ACCUMS))
        {
        for (i=0; i<NFX_FCHAN; i++) fxp->fchan[i] = -1.0;
        fxp->nsegs = 0;
        fxp->numaccp = 0;
        fxp->tstart = 0;
        fxp->segstart = 0.0;
        for (i=0; i<MAXSEG; i++)
            {
            fxp->rsum[i] = 0.0;
            fxp->isum[i] = 0.0;
            fxp->segsec[i] = 0.0;
            fxp->segcount[i] = 0.0;
            fxp->seglen[i] = 0.0;
            }
        }

    }

/*
 * eof
 */
