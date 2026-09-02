/*
 * Do the cubic spline fit
 *
 * Fit is in of loglen[ii] = log10(codatum->seglen[ii]) & codatum->snr[ii]
 * and we should strive to a smallish number of breakpoints.
 *
 * This version uses the newer (GSL 2.8 and later) bspline calls
 *
 * Created GBC May 1 2026.
 */
#include "fit_gsl.h"

#if HAVEGSL2P8
double fit_cbs2p8(cosumary *codatum, int npt)
{
    const size_t k = 4, nbp = 2;    /* spline order, num breakpoints */
    double a, b, chisq, rchisq, peaksnr, peakslen;
    gsl_bspline_workspace *work = gsl_bspline_alloc(k, nbp);
    const size_t ppp = gsl_bspline_ncontrol(work), dof = npt - ppp;
    gsl_vector *c = gsl_vector_alloc(ppp);
    gsl_vector *x = gsl_vector_alloc(npt);
    gsl_vector *y = gsl_vector_alloc(npt);
    gsl_vector *w = gsl_vector_alloc(npt);
    double cbsyi, xi;
    int ii, kp, status;

    if (dof < 0) {
        msg("Too few dof (%d) for cubic spline fit", 3, dof);
        return(-2.0);
    }
    msg("", 1);
    msg("Doing the Cubic Spline (GSL2.8) SNR fit for maximum", 2);
    for (ii = 0; ii < npt; ii++) {
        gsl_vector_set(x, ii, log10(codatum->seglen[ii]));
        gsl_vector_set(y, ii, codatum->snr[ii]);
        gsl_vector_set(w, ii, 1.0); /* provisionally */
    }
    /* uniform breakpoints on [a,b] */
    a = gsl_vector_get(x, 0);
    b = gsl_vector_get(x, npt-1);
    gsl_bspline_init_uniform(a, b, work);

    /* solve least squares problem */
    gsl_bspline_wlssolve(x, y, w, c, &chisq, work);
    rchisq = chisq / dof;
    msg("With %d breakpoints, chisq/dof = %e dof = %d", 2, nbp, rchisq, dof);

    /* output cubic spline fit */
    for (ii = 0, kp = -1, peaksnr = 0.0; ii < npt; ii++) {
        xi = gsl_vector_get(x, ii);
        gsl_bspline_calc(xi, c, &cbsyi, work);
        codatum->fit2p8[ii] = cbsyi;
        if (cbsyi > peaksnr) {
            kp = ii; peaksnr = cbsyi;
        }
    }
    /* shift in from edges if necessary, and we know npt > 3 */
    if (kp == 0)          { kp++; peaksnr = codatum->fit2p8[kp]; }
    else if (kp == npt-1) { kp--; peaksnr = codatum->fit2p8[kp]; }

    /* The minimizer complains if kp is not an actual peak, but the error it
     * generates if caught allows it to soldier on to find a real minimum. */
    codatum->cbs_p2p8[0] = kp-1;
    codatum->cbs_p2p8[1] = kp;
    codatum->cbs_p2p8[2] = kp+1;
    peakslen = codatum->seglen[kp];
    msg("CBS2.8 peak of %lf near [0|%d,%d,%d|%d] seglen = %lf", 2,
        peaksnr, kp-1,kp,kp+1, npt-1, peakslen);

    /* find the peak using 1-d Minimization 1/snr */
    clear_err_handler_report();
    status = min_inv_snr_cbs2p8(codatum, npt, x, c, work, &peakslen);
    if (status == 0) {
        char *er = get_err_handler_report();
        if (2 >= msglev && *er) fputs(er, stderr);
        msg("CBSpline2.8 fit at seglen %lf (%d)", 2, peakslen, status);
    } else {
        fprintf(stderr, get_err_handler_report());
        status = 0;
        peakslen = codatum->seglen[kp];
        msg("CBSpline2.8 remains at seglen %lf (%d)", 2, peakslen, status);
    }
    clear_err_handler_report();

    /* cleanup */
    gsl_vector_free(x);
    gsl_vector_free(y);
    gsl_vector_free(w);
    gsl_vector_free(c);
    gsl_bspline_free(work);
    codatum->didfits |= FITOPT_SNR_2P8;
    codatum->redchisq[FITOPT_NDX_2P8] = rchisq;
    return(peakslen);
}
#endif /* HAVEGSL2P8 */

/*
 * eof vim:nospell
 */
