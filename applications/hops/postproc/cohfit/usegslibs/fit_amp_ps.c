/*
 * Support for the plateau-slope fitting function.
 * see explanations in fit_ampl.c for model functions.
 *
 * Created Apr 26 1995 by GBC based on predecessor cofit sources
 */
#include "fit_gsl.h"

#if HAVE_GSL_GSL_MULTIFIT_NLINEAR_H
/* the plateau-slope fitting function */
int ps_f(const gsl_vector * x, void *data, gsl_vector * f)
{
    size_t n = ((Data *)data)->n, ii;
    double *t = ((Data *)data)->t;
    double *y = ((Data *)data)->y;
    double A = gsl_vector_get (x, 0);
    double B = gsl_vector_get (x, 1);
    double C = gsl_vector_get (x, 2), Yi;
    for (ii = 0; ii < n; ii++) {
        Yi = A;
        if (t[ii] > C) Yi += B * log10(t[ii]/C);
        gsl_vector_set(f, ii, Yi - y[ii]);
    }
    return(GSL_SUCCESS);
}
void populate_fitamp_ps(cosumary *codatum)
{
    int ii;
    double A = codatum->pspar[0], B = codatum->pspar[1], C = codatum->pspar[2];
    for (ii = 0; ii < codatum->nsegtime; ii++) {
        codatum->fitaps[ii] = A;
        if (codatum->seglen[ii] > C)
            codatum->fitaps[ii] += B * log10(codatum->seglen[ii]/C);
    }
}

/* The plateau-slope version of the Jacobian */
int ps_df(const gsl_vector * x, void *data, gsl_matrix * J)
{
    size_t n = ((Data *)data)->n, ii;
    double *t = ((Data *)data)->t;
    double A = gsl_vector_get (x, 0);
    double B = gsl_vector_get (x, 1);
    double C = gsl_vector_get (x, 2);
    for (ii = 0; ii < n; ii++) {
        if (t[ii] < C) {    /* plateau */
            gsl_matrix_set(J, ii, 0, 1.0);
            gsl_matrix_set(J, ii, 1, 0.0);
            gsl_matrix_set(J, ii, 2, 0.0);
        } else {            /* slope */
            gsl_matrix_set(J, ii, 0, 1.0);
            gsl_matrix_set(J, ii, 1, log(t[ii]/C));
            gsl_matrix_set(J, ii, 2, - B / (C * M_LN10));
        }
    }
    return(GSL_SUCCESS);
}

/* the plateau-slope callback */
void ps_callback(const size_t iter, void *params,
              const gsl_multifit_nlinear_workspace *w)
{
    gsl_vector *f = gsl_multifit_nlinear_residual(w);
    gsl_vector *x = gsl_multifit_nlinear_position(w);
    double rcond;
    /* compute reciprocal condition number of J(x) */
    gsl_multifit_nlinear_rcond(&rcond, w);
    msg("iter %2zu: A = %.4f, B = %.4f, C = %.4f, "
        "cond(J) = %8.4g, |f(x)| = %.4f", 0, iter,
        gsl_vector_get(x, 0), gsl_vector_get(x, 1), gsl_vector_get(x, 2),
        1.0 / rcond, gsl_blas_dnrm2(f));
}

/* the plateau-slope fit driver */
int ps_driver(Data *dp, double psini[3], double pserr[3], double chisq[2],
    int *dofp, int *status, int *info, int *niter, double *rchisq)
{
    const size_t ps = 3;
    const size_t n = dp->n;
    const gsl_multifit_nlinear_type *T = gsl_multifit_nlinear_trust;
    double efac;
    gsl_multifit_nlinear_workspace *wrk;
    gsl_multifit_nlinear_fdf fdf;
    gsl_multifit_nlinear_parameters fdf_params =
        gsl_multifit_nlinear_default_parameters();
    gsl_vector *f;
    gsl_matrix *J, *covar = gsl_matrix_alloc(ps, ps);
    gsl_vector_view x = gsl_vector_view_array (psini, ps);
    gsl_vector_view wts = gsl_vector_view_array(dp->wgt, n);
    /* connect up the plateau-slope, plateau-only or slope-only functions */
    fdf.f = ps_f; fdf.df = ps_df;
    fdf.fvv = NULL; fdf.n = n; fdf.p = ps; fdf.params = dp;
    /* various initializations; see gslcheck/expfitexample.c */
    wrk = gsl_multifit_nlinear_alloc (T, &fdf_params, n, ps);
    gsl_multifit_nlinear_winit (&x.vector, &wts.vector, &fdf, wrk);
    f = gsl_multifit_nlinear_residual(wrk);
    gsl_blas_ddot(f, f, &chisq[0]);
    /* solve the system with a maximum of maxits iterations */
    *status = gsl_multifit_nlinear_driver(
        maxits, xtol, gtol, ftol, ps_callback, NULL, info, wrk);
    /* compute best fit covariance and final cost*/
    J = gsl_multifit_nlinear_jac(wrk);
    gsl_multifit_nlinear_covar (J, 0.0, covar);
    gsl_blas_ddot(f, f, &chisq[1]);
    *dofp = n - ps;
    *rchisq = chisq[1] / *dofp;
    efac = GSL_MAX_DBL(1, sqrt(*rchisq));
    /* output fit summaries */
    msg("ps summary from method '%s/%s'", 1,
        gsl_multifit_nlinear_name(wrk), gsl_multifit_nlinear_trs_name(wrk));
    msg("ps number of iterations: %zu", 1,
        *niter = gsl_multifit_nlinear_niter(wrk));
    msg("ps function evaluations: %zu", 1, fdf.nevalf);
    msg("ps Jacobian evaluations: %zu", 1, fdf.nevaldf);
    msg("ps reason for stopping: %s", 1,
        (*info == 1) ? "small step size" : "small gradient");
    msg("ps initial |f(x)| = %f", 1, sqrt(chisq[0]));
    msg("ps final   |f(x)| = %f", 1, sqrt(chisq[1]));
    msg("ps chisq/dof = %g (%g)", 1, *rchisq, efac * efac);
    /* capture final parameter values for amp plots */
    msg("ps A = %.5f +/- %.5f", 1, psini[0] = FIT(0), pserr[0] = efac*ERR(0));
    msg("ps B = %.5f +/- %.5f", 1, psini[1] = FIT(1), pserr[1] = efac*ERR(1));
    msg("ps C = %.5f +/- %.5f", 1, psini[2] = FIT(2), pserr[2] = efac*ERR(2));
    msg("ps status (modern) = %s", 1, gsl_strerror(*status));
    gsl_multifit_nlinear_free (wrk);
    gsl_matrix_free (covar);
    return(GSL_SUCCESS);
}

/* plateau-slope fitting */
void plateau_slope_fit(cosumary *codatum, Data *dp)
{
    int psdof, rcps, stps, inps, psiter;
    double pschi[2], rchips = -1;
    codatum->didfits |= FITOPT_AMP_PS;
    msg("", 1); msg("plateau-slope amplitude fit (modern)", 2);
    msg("init ps: A = %.4f, B = %.4f, C = %.4f, ", 1,
        codatum->pspar[0], codatum->pspar[1], codatum->pspar[2]);
    rcps = ps_driver(dp, codatum->pspar, codatum->pserr, pschi,
        &psdof, &stps, &inps, &psiter, &rchips);
    msg("fini ps: dof=%d stat=%d info=%d iters=%d redchi=%g", 2,
        psdof, stps, inps, psiter, rchips);
    codatum->iteratio[FITOPT_NDX_PS] = psiter;
    codatum->redchisq[FITOPT_NDX_PS] = rchips;
    populate_fitamp_ps(codatum);
}

#endif /* HAVE_GSL_GSL_MULTIFIT_NLINEAR_H */
/*
 * eof: vim:nospell
 */
