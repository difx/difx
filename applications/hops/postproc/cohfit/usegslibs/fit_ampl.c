/************************************************************************/
/*                                                                      */
/* Fits a theoretical curve of amplitude versus segmentation time       */
/*                                                                      */
/*      Inputs:         codatum         structure of scan data          */
/*                      npt             number of array elements        */
/*      Output:         codatum         struct. with filled in amp. fit */
/*                      return value    0=OK, -1=noconvergence          */
/*                                      -2=Num. Rec. Error              */
/* Created Apr 26 1995 by GBC based on predecessor cofit sources        */
/************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "fit_gsl.h"

/* set a legal value for the gslegacy bits */
int get_gslegacy_default(void)
{
    if (GSLEGACY_DEFAULT < 0)
        msg("No gsl/gsl_multifit_nlin*.h support--cannot proceed.", 3);
    return(GSLEGACY_DEFAULT);
}

/*
 * We follow the GSL code example using Y(t) for the amplitude model,
 * and y(t) for the actual data, and f(t) for the fitting function.
 * The functional form is slightly different, but otherwise all is simlar
 * except we do not need to generate the fake data.  ;-)
 *
 * This file is thus similar to gslcheck/expfitexample.c, but with
 * various adaptations.  Instead of random data with a specified sigma,
 * we have amplitude/snr which varies per point.
 *
 * The amplitude function is a plateau [Y(t)=A] until a breakpoint (at t=C)
 * after which it decays logarithmically with some slope (B) which appears
 * linear in the logarithm of t space.  Note log10(t) = log(t)/log(10) so
 * this is really just a redefinition of B which has relevance for the
 * calculation of the coherence time.  For the Marquadt method, we also
 * need the partial derivatives of y with A, B and C:
 * 
 *        /  A                                      t < C
 * Y(t) = |
 *        \  A + B log10(t/C)                       t > C
 *
 *         = A + B log10(t) - B log10(C)
 *         = A + B log10(t) - B log(C)/log(10)
 *         = A + B log10(t) - B log(C)/M_LN10
 *
 * alt slope-only, fix breakpoint C at t[0]
 *         = A + B log10(t/t[0])
 *
 * fit function is
 *  f(t) = Y(t) - y(t)
 *
 * Note that in addition to fitting the complete function, we should also
 * fit the plateau and slope regions separately and compare the reduced
 * chisq to see which is the better fit to catch C outside the data.
 *
 * The Jacobian matrix J(i,j) = dfi / dxj for each case
 * df/dA = 1 for all t
 * df/dB = 0 or log(t/C)
 * df/dC = 0 or  - B / (C * M_LN10)
 *
 * See fit_amp_ps.c, fit_amp_po.c and fit_amp_so.c for the
 * fitting functions, the Jacobians, the callbacks and fit drivers.
 */

/* conventional wisdom is that the coherence time is that
 * atmospheric coherence times are about (90, 30, 20, 10) secs
 * for observing frequencies of (86, 230, 345, 690) GHz, which is
 * then 20.0 / (freqGHz / 345) seconds.  We use this only for a
 * starting guess; the original cofit code had 20.0 seconds. */
double cohereguess(cosumary *codatum)
{
    double freqGHz = codatum->datum->ref_freq / 1000.0;
    return(20.0 / (freqGHz / 345));
}

/* This contains the hand-shaking to the cohfit caller.  For each fit we
 * see what happens with initial chisq/dof (..chi[0]/dof) and the final
 * chisq/dof (..chi[1]/dof), where dof is npt - #params and also other
 * fit information, and make a choice and return results. */
int fit_ampl (cosumary *codatum, int npt, int gslegacy)
{
    int fitflag, ii;
    double tt[npt], yy[npt], weights[npt], sigma[npt];
    Data dd = { (size_t)npt, tt, yy, weights, sigma };

    /* create the incoming data, time vectors and parameter guesses */
    for (ii = 0; ii < npt; ii++) {
        tt[ii] = codatum->seglen[ii];
        yy[ii] = codatum->ampl[ii];
        sigma[ii] = codatum->ampl[ii]/codatum->snr[ii];
        weights[ii] = 1.0 / ( sigma[ii] * sigma[ii] );
    }
    codatum->pspar[0] = codatum->popar[0] = codatum->sopar[0] = yy[0];
    codatum->pspar[1] = codatum->sopar[1] =
        (yy[npt-1] - yy[0])/log10(tt[npt-1]/tt[0]);
    codatum->pspar[2] = cohereguess(codatum);

    /* mark as totally invalid */
    codatum->redchisq[FITOPT_NDX_PS] =
    codatum->redchisq[FITOPT_NDX_PO] =
    codatum->redchisq[FITOPT_NDX_SO] = -17.0;

    if (gslegacy == 1) {
#if HAVE_GSL_GSL_MULTIFIT_NLIN_H
        msg("Using GSL legacy code for ps, po and so fits", 2);
        if (codatum->fitmask & FITOPT_AMP_PS) plateau_slope_gcy(codatum, &dd);
        if (codatum->fitmask & FITOPT_AMP_PO) plateau_only_gcy(codatum, &dd);
        if (codatum->fitmask & FITOPT_AMP_SO) slope_only_gcy(codatum, &dd);
#endif /* HAVE_GSL_GSL_MULTIFIT_NLIN_H */ 
    } else if (gslegacy == 0) {
#if HAVE_GSL_GSL_MULTIFIT_NLINEAR_H
        msg("Using GSL modern code for ps, po and so fits", 2);
        if (codatum->fitmask & FITOPT_AMP_PS) plateau_slope_fit(codatum, &dd);
        if (codatum->fitmask & FITOPT_AMP_PO) plateau_only_fit(codatum, &dd);
        if (codatum->fitmask & FITOPT_AMP_SO) slope_only_fit(codatum, &dd);
#endif /* HAVE_GSL_GSL_MULTIFIT_NLINEAR_H */
    } else {
        msg("Developer error--should not ever get here.", 3);
        return(-1);
    }

    fitflag = choose_best_amp_fit(codatum);
    return(fitflag);
}

/*
 * eof: vim:nospell
 */
