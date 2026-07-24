/*
 * Replacements for numerical recipe fitting routines that use GSL.
 *
 * This finishes up after fitting to make final decisons.
 * rchi?? was initialized to -1 prior to fitting, so if
 * a driver was run, it becomes > 0 since chisq and at the
 * same time codatum->didfits was |= with FITOPS_AMP_??
 */
#include "fit_gsl.h"

int choose_best_amp_fit(cosumary *codatum)
{
    int fitflag = -2;
    double best_redchisq = -1.0;

    codatum->bestamp = 0;                               /* be sure */

    /* initialize with PS fit as most likely */
    if ((codatum->didfits & FITOPT_AMP_PS) &&
        codatum->redchisq[FITOPT_NDX_PS] > 0.0) {
            best_redchisq = codatum->redchisq[FITOPT_NDX_PS];
            codatum->bestamp = FITOPT_AMP_PS;
            msg("plateau-slope is candidate, redchisq=%g", 2, best_redchisq);
    }
    /* is plateau-only better or was PS not done */
    if ((codatum->didfits & FITOPT_AMP_PO) &&
        codatum->redchisq[FITOPT_NDX_PO] > 0.0) {
        if ((0 == codatum->bestamp) ||
            (codatum->redchisq[FITOPT_NDX_PO] < best_redchisq)) {
            best_redchisq = codatum->redchisq[FITOPT_NDX_PO];
            codatum->bestamp =  FITOPT_AMP_PO;
            msg("plateau-only is better, redchisq=%g", 2, best_redchisq);
        } else {
            msg("plateau-only is no better, redchisq=%g", 2,
                codatum->redchisq[FITOPT_NDX_PO]);
        }
    }
    /* finally, is slope-only best or only done */
    if ((codatum->didfits & FITOPT_AMP_SO) &&
        codatum->redchisq[FITOPT_NDX_SO] > 0.0) {
        if ((0 == codatum->bestamp) ||
            (codatum->redchisq[FITOPT_NDX_SO] < best_redchisq)) {
            best_redchisq = codatum->redchisq[FITOPT_NDX_SO];
            codatum->bestamp =  FITOPT_AMP_SO;
            msg("slope-only is better, redchisq=%g", 2, best_redchisq);
        } else {
            msg("slope-only is no better, redchisq=%g", 2,
                codatum->redchisq[FITOPT_NDX_SO]);
        }
    }

    if (codatum->bestamp == FITOPT_AMP_PS) {
        /* plateau-slope parameters */
        codatum->plateau = (float)codatum->pspar[0];     /* A */
        codatum->slope  = (float)codatum->pspar[1];      /* B */
        codatum->breakpoint = (float)codatum->pspar[2];  /* C */
        codatum->bestamp = FITOPT_AMP_PS;
        msg("plateau-slope is the best fit", 2);
        fitflag = 0;
    } else if (codatum->bestamp == FITOPT_AMP_PO) {
        /* plateau-only parameters */
        codatum->plateau = (float)codatum->popar[0];     /* A */
        codatum->slope = 0.0;
        codatum->breakpoint = 0.0;
        codatum->bestamp = FITOPT_AMP_PO;
        msg("plateau-only is the best fit", 2);
        fitflag = 0;
    } else if (codatum->bestamp == FITOPT_AMP_SO) {
        /* slope-only parameters;  A = A + B log10(t/t[0]) */
        codatum->plateau = (float)(codatum->sopar[0]);
        codatum->slope  = (float)codatum->sopar[1];      /* B  */
        codatum->breakpoint = codatum->seglen[0];        /* (pseudo) C' */
        codatum->bestamp = FITOPT_AMP_SO;
        msg("slope-only is the best fit", 2);
        fitflag = 0;
    } else {
        fitflag = -1;
    }

    return(fitflag);
}

/*
 * eof vim:nospell
 */
