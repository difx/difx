/*
 * Support for the cubic spline fit:
 *
 * min_inv_snr_cbs() uses the data to find the minimum of the snr
 * as interpolated from the spline.  A nonzero return suggests an
 * error as explained by min_inv_snr_cbs_err().
 *
 * This version uses the newer (GSL 2.8 and later) bspline calls
 */
#include <stdio.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_min.h>
#include "fit_gsl.h"

#if HAVEGSL2P8
static const double epsabs = 1e-6;
static const double epsrel = 0.0;

/* a structure to hold (hidden) spline parameters */
typedef struct snrminpars {
    gsl_vector *control;
    gsl_bspline_workspace *work;
    char *debug;
} Snrminpars;

/* something to explain any errors */
void min_inv_snr_cbs_err(int status, int msglvl)
{
    switch (status) {
    case 0:
        msg("min_inv_snr_cbs_err2p8: success", msglvl);
        break;
    case -2:
        msg("min_inv_snr_cbs_err2p8: no fit was done", msglvl);
        break;
    default:
        msg("min_inv_snr_cbs_err2p8: developer error", 3); exit(1);
        break;
    }
}

/* provide 1/snr(x); Cf "output cubic spline fit" part of fit_cbs.c */
double invsnr(double x, void *params)
{
    gsl_vector *control = ((Snrminpars *)params)->control;
    gsl_bspline_workspace *work = ((Snrminpars *)params)->work;
    char *dbg = ((Snrminpars *)params)->debug;
    double  thesnr, inver;
    gsl_bspline_calc(x, control, &thesnr, work);
    inver = (thesnr == 0.0) ? HUGE_VAL : 1.0 / thesnr;
    msg("CBS2.8: snr(%8.3f) = %8.3f inv(%8.3f) = %g (%s)", 1,
        x, thesnr, x, inver, dbg);
    return(inver);
}

/* driver for the minimization */
int min_inv_snr_cbs2p8(cosumary *codatum, int npt, gsl_vector *x,
    gsl_vector *control, gsl_bspline_workspace *work, double *peakslen)
{
    Snrminpars params;
    int status = -2, iter = 0, maxits = MAX_ITERS;
    const gsl_min_fminimizer_type *typ;
    double x_lower, x_minim, x_upper, expected, is_lower, is_minim, is_upper;
    gsl_min_fminimizer *s;
    gsl_function fun;

    params.control = control;
    params.work = work;
    params.debug = "debug";
    fun.function = &invsnr;
    fun.params = &params;
    typ = gsl_min_fminimizer_brent;
    s = gsl_min_fminimizer_alloc(typ);
    x_lower = codatum->seglen[codatum->cbs_p2p8[0]];
    x_minim = codatum->seglen[codatum->cbs_p2p8[1]];
    x_upper = codatum->seglen[codatum->cbs_p2p8[2]];

    msg("CBS2.8: lower %d < min %d < upper %d (index)", 1,
        codatum->cbs_p2p8[0], codatum->cbs_p2p8[1], codatum->cbs_p2p8[2]);
    msg("CBS2.8: lower %8.3f < min %8.3f < upper %8.3f (x val)", 1,
        x_lower, x_minim, x_upper);
    msg("CBS2.8: lower %8.3f < min %8.3f < upper %8.3f (log10(x))", 1,
        log10(x_lower), log10(x_minim), log10(x_upper));
    msg("CBS2.8: lower %8.3f   min %8.3f   upper %8.3f (snr)", 1,
        codatum->snr[codatum->cbs_p2p8[0]], codatum->snr[codatum->cbs_p2p8[1]],
        codatum->snr[codatum->cbs_p2p8[2]]);

    params.debug = "lower"; is_lower = invsnr(log10(x_lower), &params);
    params.debug = "minim"; is_minim = invsnr(log10(x_minim), &params);
    params.debug = "upper"; is_upper = invsnr(log10(x_upper), &params);
    params.debug = "-run-";
    msg("CBS2.8: lower %g   min %g   upper %g (1/snr)", 1,
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
    min_inv_snr_cbs_err(status, 2);
    codatum->iteratio[FITOPT_NDX_2P8] = iter;
    return(status);
}
#endif /* HAVEGSL2P8 */

/*
 * eof vim:nospell
 */
