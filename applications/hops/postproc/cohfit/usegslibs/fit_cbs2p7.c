/*
 * Do the cubic spline fit
 *
 * Fit is in of loglen[ii] = log10(codatum->seglen[ii]) & codatum->snr[ii]
 * and we should strive to a smallish number of breakpoints.
 *
 * This version uses the older (GSL 2.7 and earlier) bspline calls
 *
 * The earlier calls are similar, but there are minor differences in
 * the external interface.  We set up to the same order and breaks.
 * gsl_bspline_wlssolve() and gsl_bspline_calc() at least are missing.
 *
 * Created GBC May 1 2026.
 */
#include "fit_gsl.h"

#if HAVEGSL2P7
double fit_cbs2p7(cosumary *codatum, int npt)
{
    const size_t k = 4, nbp = 2;    /* spline order, num breakpoints */
    double a, b, chisq, rchisq, peaksnr, peakslen;
    gsl_bspline_workspace *work = gsl_bspline_alloc(k, nbp);
    const size_t ncoeffs = gsl_bspline_ncoeffs(work), dof = npt - ncoeffs;
    gsl_vector *c = gsl_vector_alloc(ncoeffs);
    gsl_vector *x = gsl_vector_alloc(npt);
    gsl_vector *y = gsl_vector_alloc(npt);
    gsl_vector *w = gsl_vector_alloc(npt);
    gsl_vector *B = gsl_vector_alloc(ncoeffs);
    gsl_matrix *X = gsl_matrix_alloc(npt, ncoeffs);
    gsl_matrix *cov = gsl_matrix_alloc(ncoeffs, ncoeffs);
    gsl_multifit_linear_workspace *mw = gsl_multifit_linear_alloc(npt, ncoeffs);
    double cbsyi, xi, Bj, Rsq, tss, yerr;
    int ii, jj, kp, status;

    if (dof < 0) {
        msg("Too few dof (%d) for cubic spline fit", 3, dof);
        return(-2.0);
    }
    msg("", 1);
    msg("Doing the Cubic Spline (GSL2.7) SNR fit for maximum", 2);
    for (ii = 0; ii < npt; ii++) {
        gsl_vector_set(x, ii, log10(codatum->seglen[ii]));
        gsl_vector_set(y, ii, codatum->snr[ii]);
        gsl_vector_set(w, ii, 1.0); /* provisionally */
    }
    /* uniform breakpoints on [a,b] */
    a = gsl_vector_get(x, 0);
    b = gsl_vector_get(x, npt-1);
    gsl_bspline_knots_uniform(a, b, work);

    /* construct the fit matrix X */
    for (ii = 0; ii < npt; ii++) {
        xi = gsl_vector_get(x, ii);
        /* compute B_j(xi) for all j */
        gsl_bspline_eval(xi, B, work);
        /* fill in row i of X */
        for (jj = 0; jj < ncoeffs; ++jj) {
            Bj = gsl_vector_get(B, jj);
            gsl_matrix_set(X, ii, jj, Bj);
        }
    }

    /* solve least squares problem */
    gsl_multifit_wlinear(X, w, y, c, cov, &chisq, mw);
    tss = gsl_stats_wtss(w->data, 1, y->data, 1, y->size);
    Rsq = 1.0 - chisq / tss;
    rchisq = chisq / dof;
    msg("With %d breakpoints, chisq/dof = %e dof = %d", 2, nbp, rchisq, dof);

    /* output cubic spline fit at the input points */
    for (ii = 0, kp = -1, peaksnr = 0.0; ii < npt; ii++) {
        xi = gsl_vector_get(x, ii);
        gsl_bspline_eval(xi, B, work);
        gsl_multifit_linear_est(B, c, cov, &cbsyi, &yerr);
        codatum->fit2p7[ii] = cbsyi;
        if (cbsyi > peaksnr) {
            kp = ii; peaksnr = cbsyi;
        }
    }
    /* shift in from edges if necessary, and we know npt > 3 */
    if (kp == 0)          { kp++; peaksnr = codatum->fit2p7[kp]; }
    else if (kp == npt-1) { kp--; peaksnr = codatum->fit2p7[kp]; }

    /* The minimizer complains if kp is not an actual peak, but the error it
     * generates if caught allows it to soldier on to find a real minimum. */
    codatum->cbs_p2p7[0] = kp-1;
    codatum->cbs_p2p7[1] = kp;
    codatum->cbs_p2p7[2] = kp+1;
    peakslen = codatum->seglen[kp];
    msg("CBS2.7 peak of %lf near [0|%d,%d,%d|%d] seglen = %lf", 2,
        peaksnr, kp-1,kp,kp+1, npt-1, peakslen);

    /* find the peak using 1-d Minimization 1/snr */
    clear_err_handler_report();
    status = min_inv_snr_cbs2p7(codatum, npt, x, work, B, c, cov, &peakslen);
    if (status == 0) {
        char *er = get_err_handler_report();
        if (2 >= msglev && *er) fputs(er, stderr);
        msg("CBSpline2.7 fit at seglen %lf (%d)", 2, peakslen, status);
    } else {
        fputs(get_err_handler_report(), stderr);
        status = 0;
        peakslen = codatum->seglen[kp];
        msg("CBSpline2.7 remains at seglen %lf (%d)", 2, peakslen, status);
    }
    clear_err_handler_report();

    /* cleanup */
    gsl_bspline_free(work);
    gsl_vector_free(B);
    gsl_vector_free(x);
    gsl_vector_free(y);
    gsl_matrix_free(X);
    gsl_vector_free(c);
    gsl_vector_free(w);
    gsl_matrix_free(cov);
    gsl_multifit_linear_free(mw);
    codatum->didfits |= FITOPT_SNR_2P7;
    codatum->redchisq[FITOPT_NDX_2P7] = rchisq;
    return(peakslen);
}
#endif /* HAVEGSL2P7 */

/*
 * eof vim:nospell
 */
