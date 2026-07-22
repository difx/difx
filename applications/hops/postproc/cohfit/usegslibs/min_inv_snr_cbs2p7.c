/*
 * Support for the cubic spline fit:
 *
 * min_inv_snr_cbs() uses the data to find the minimum of the snr
 * as interpolated from the spline.  A nonzero return suggests an
 * error as explained by min_inv_snr_cbs_err().
 *
 * This version uses the older (GSL 2.7 and earlier) bspline calls
 */
#include <stdio.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_min.h>
#include "fit_gsl.h"

#if HAVEGSL2P7
static const double epsabs = 1e-6;
static const double epsrel = 0.0;

/* a structure to hold (hidden) spline parameters */
typedef struct snrmin2p7p {
    gsl_vector *B;
    gsl_vector *c;
    gsl_matrix *cov;
    gsl_bspline_workspace *work;
    char *debug;
} Snrmin2p7p;

/* something to explain any errors */
void min_inv_snr_cbs_err2p7(int status, int msglvl)
{
    switch (status) {
    case 0:
        msg("min_inv_snr_cbs_err2p7: success", msglvl);
        break;
    case -2:
        msg("min_inv_snr_cbs_err2p7: no fit was done", msglvl);
        break;
    default:
        msg("min_inv_snr_cbs_err2p7: developer error", 3); exit(1);
        break;
    }
}

/* provide 1/snr(x); Cf "output cubic spline fit" part of fit_cbs.c */
double invsnr2p7(double x, void *params)
{
    gsl_vector *B = ((Snrmin2p7p *)params)->B;
    gsl_vector *c = ((Snrmin2p7p *)params)->c;
    gsl_matrix *cov = ((Snrmin2p7p *)params)->cov;
    gsl_bspline_workspace *work = ((Snrmin2p7p *)params)->work;
    char *dbg = ((Snrmin2p7p *)params)->debug;
    double  thesnr, inver, yerr;
    gsl_bspline_eval(x, B, work);
    gsl_multifit_linear_est(B, c, cov, &thesnr, &yerr);
    inver = (thesnr == 0.0) ? HUGE_VAL : 1.0 / thesnr;
    msg("CBS2p7: snr(%8.3f) = %8.3f inv(%8.3f) = %g (%s)", 1,
        x, thesnr, x, inver, dbg);
    return(inver);
}

/* driver for the minimization */
int min_inv_snr_cbs2p7(cosumary *codatum, int npt, gsl_vector *x,
    gsl_bspline_workspace *work,
    gsl_vector *B, gsl_vector *c, gsl_matrix *cov, double *peakslen)
{
    Snrmin2p7p params;
    int status = -2, iter = 0, maxits = MAX_ITERS;
    const gsl_min_fminimizer_type *typ;
    double x_lower, x_minim, x_upper, expected, is_lower, is_minim, is_upper;
    gsl_min_fminimizer *s;
    gsl_function fun;

    params.B = B;
    params.c = c;
    params.cov = cov;
    params.work = work;
    params.debug = "debug";
    fun.function = &invsnr2p7;
    fun.params = &params;
    typ = gsl_min_fminimizer_brent;
    s = gsl_min_fminimizer_alloc(typ);
    x_lower = codatum->seglen[codatum->cbs_p2p7[0]];
    x_minim = codatum->seglen[codatum->cbs_p2p7[1]];
    x_upper = codatum->seglen[codatum->cbs_p2p7[2]];

    msg("CBS2.7: lower %d < min %d < upper %d (index)", 1,
        codatum->cbs_p2p7[0], codatum->cbs_p2p7[1], codatum->cbs_p2p7[2]);
    msg("CBS2.7: lower %8.3f < min %8.3f < upper %8.3f (x val)", 1,
        x_lower, x_minim, x_upper);
    msg("CBS2.7: lower %8.3f < min %8.3f < upper %8.3f (log10(x))", 1,
        log10(x_lower), log10(x_minim), log10(x_upper));
    msg("CBS2.7: lower %8.3f   min %8.3f   upper %8.3f (snr)", 1,
        codatum->snr[codatum->cbs_p2p7[0]], codatum->snr[codatum->cbs_p2p7[1]],
        codatum->snr[codatum->cbs_p2p7[2]]);

    params.debug = "lower"; is_lower = invsnr2p7(log10(x_lower), &params);
    params.debug = "minim"; is_minim = invsnr2p7(log10(x_minim), &params);
    params.debug = "upper"; is_upper = invsnr2p7(log10(x_upper), &params);
    params.debug = "-run-";
    msg("CBS2.7: lower %g   min %g   upper %g (1/snr)", 1,
        is_lower, is_minim, is_upper);
        
    x_lower = log10(x_lower);
    x_minim = log10(x_minim);
    x_upper = log10(x_upper);
    expected = (x_upper+x_lower)/2.0;
    gsl_min_fminimizer_set(s, &fun, x_minim, x_lower, x_upper);
    msg("using %s method", 1, gsl_min_fminimizer_name(s));
    msg("%5s [%9s, %9s] %9s %10s %9s", 1,
        "iter", "lower", "upper", "min", "(guess)", "err(est)");
    msg("%5d [%.7f, %.7f] %.7f %+.7f %.7f", 1,
        iter, x_lower, x_upper, x_minim, expected, x_upper-x_lower);

    do {
        iter++;
        (void)gsl_min_fminimizer_iterate(s);
        x_minim = gsl_min_fminimizer_x_minimum(s);
        x_lower = gsl_min_fminimizer_x_lower(s);
        x_upper = gsl_min_fminimizer_x_upper(s);
        status = gsl_min_test_interval(x_lower, x_upper, epsabs, epsrel);
        if (status == GSL_SUCCESS) msg("Converged", 1);
        msg("%5d [%.7f, %.7f] %.7f %+.7f %.7f", 1,
            iter, x_lower, x_upper, x_minim, expected, x_upper-x_lower);
    } while (status == GSL_CONTINUE && iter < maxits);
    *peakslen = pow(10.0, x_minim);
    min_inv_snr_cbs_err2p7(status, 2);
    codatum->iteratio[FITOPT_NDX_2P7] = iter;
    return(status);
}
#endif /* HAVEGSL2P7 */

/*
 * eof vim:nospell
 */
