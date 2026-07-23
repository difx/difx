/*
 * Support for the plateau-slope fitting function.
 * see explanations in fit_ampl.c for model functions.
 *
 * Created Apr 26 1995 by GBC based on predecessor cofit sources
 */
#include "fit_gsl.h"

#if HAVE_GSL_GSL_MULTIFIT_NLINEAR_H
/* the slope-only fitting function */
int so_f(const gsl_vector * x, void *data, gsl_vector * f)
{
    size_t n = ((Data *)data)->n, ii;
    double *t = ((Data *)data)->t;
    double *y = ((Data *)data)->y;
    double A = gsl_vector_get (x, 0), Yi;
    double B = gsl_vector_get (x, 1);
    double C = t[0];
    for (ii = 0; ii < n; ii++) {
        Yi = A + B * log10(t[ii]/C);
        gsl_vector_set(f, ii, Yi - y[ii]);
    }
    return(GSL_SUCCESS);
}
void populate_fitamp_so(cosumary *codatum)
{
    int ii;
    double A = codatum->sopar[0], B = codatum->sopar[1];
    double C = codatum->seglen[0];
    for (ii = 0; ii < codatum->nsegtime; ii++) {
        codatum->fitaso[ii] = A + B * log10(codatum->seglen[ii]/C);
    }
}

/* The slope-only version of the Jacobian */
int so_df(const gsl_vector * x, void *data, gsl_matrix * J)
{
    size_t n = ((Data *)data)->n, ii;
    double *t = ((Data *)data)->t;
    double A = gsl_vector_get (x, 0);
    double B = gsl_vector_get (x, 1);
    double C = t[0];
    for (ii = 0; ii < n; ii++) {
        gsl_matrix_set(J, ii, 0, 1.0);
        gsl_matrix_set(J, ii, 1, log(t[ii]/C));
    }
    return(GSL_SUCCESS);
}

/* the slope-only callback */
void so_callback(const size_t iter, void *params,
              const gsl_multifit_nlinear_workspace *w)
{
    gsl_vector *f = gsl_multifit_nlinear_residual(w);
    gsl_vector *x = gsl_multifit_nlinear_position(w);
    double rcond;
    /* compute reciprocal condition number of J(x) */
    gsl_multifit_nlinear_rcond(&rcond, w);
    msg("iter %2zu: A = %.4f, B = %.4f, "
        "cond(J) = %8.4g, |f(x)| = %.4f", 0, iter,
        gsl_vector_get(x, 0), gsl_vector_get(x, 1),
        1.0 / rcond, gsl_blas_dnrm2(f));
}

/* the plateau-slope fit driver */
int so_driver(Data *dp, double soini[2], double soerr[2], double chisq[2],
    int *dofp, int *status, int *info, int *niter, double *rchisq)
{
    const size_t so = 2;
    const size_t n = dp->n;
    const gsl_multifit_nlinear_type *T = gsl_multifit_nlinear_trust;
    double efac;
    gsl_multifit_nlinear_workspace *wrk;
    gsl_multifit_nlinear_fdf fdf;
    gsl_multifit_nlinear_parameters fdf_params =
        gsl_multifit_nlinear_default_parameters();
    gsl_vector *f;
    gsl_matrix *J, *covar = gsl_matrix_alloc(so, so);
    gsl_vector_view x = gsl_vector_view_array (soini, so);
    gsl_vector_view wts = gsl_vector_view_array(dp->wgt, n);
    /* connect up the plateau-slope, plateau-only or slope-only functions */
    fdf.f = so_f; fdf.df = so_df;
    fdf.fvv = NULL; fdf.n = n; fdf.p = so; fdf.params = dp;
    /* various initializations; see gslcheck/expfitexample.c */
    wrk = gsl_multifit_nlinear_alloc (T, &fdf_params, n, so);
    gsl_multifit_nlinear_winit (&x.vector, &wts.vector, &fdf, wrk);
    f = gsl_multifit_nlinear_residual(wrk);
    gsl_blas_ddot(f, f, &chisq[0]);
    /* solve the system with a maximum of maxits iterations */
    *status = gsl_multifit_nlinear_driver(
        maxits, xtol, gtol, ftol, so_callback, NULL, info, wrk);
    /* compute best fit covariance and final cost*/
    J = gsl_multifit_nlinear_jac(wrk);
    gsl_multifit_nlinear_covar (J, 0.0, covar);
    gsl_blas_ddot(f, f, &chisq[1]);
    *dofp = n - so;
    *rchisq = chisq[1] / *dofp;
    efac = GSL_MAX_DBL(1, sqrt(*rchisq));
    /* output fit summaries */
    msg("so summary from method '%s/%s'", 1,
        gsl_multifit_nlinear_name(wrk), gsl_multifit_nlinear_trs_name(wrk));
    msg("so number of iterations: %zu", 1,
        *niter = gsl_multifit_nlinear_niter(wrk));
    msg("so function evaluations: %zu", 1, fdf.nevalf);
    msg("so Jacobian evaluations: %zu", 1, fdf.nevaldf);
    msg("so reason for stopping: %s", 1,
        (*info == 1) ? "small step size" : "small gradient");
    msg("so initial |f(x)| = %f", 1, sqrt(chisq[0]));
    msg("so final   |f(x)| = %f", 1, sqrt(chisq[1]));
    msg("so chisq/dof = %g (%g)", 1, *rchisq, efac * efac);
    /* capture final parameter values for amp plots */
    msg("so A = %.5f +/- %.5f", 1, soini[0] = FIT(0), soerr[0] = efac*ERR(0));
    msg("so B = %.5f +/- %.5f", 1, soini[1] = FIT(1), soerr[1] = efac*ERR(1));
    msg("so status (modern) = %s", 1, gsl_strerror(*status));
    gsl_multifit_nlinear_free (wrk);
    gsl_matrix_free (covar);
    return(GSL_SUCCESS);
}

/* slope-only fitting */
void slope_only_fit(cosumary *codatum, Data *dp)
{
    int sodof, rcso, stso, inso, soiter;
    double sochi[2], rchiso = -1;
    codatum->didfits |= FITOPT_AMP_SO;
    msg("", 1); msg("slope-only amplitude fit (modern)", 2);
    msg("init so: A = %.4f, B = %.4f", 1,
        codatum->sopar[0], codatum->sopar[1]);
    rcso = so_driver(dp, codatum->sopar, codatum->soerr, sochi,
        &sodof, &stso, &inso, &soiter, &rchiso);
    msg("fini so: dof=%d stat=%d info=%d iters=%d redchi=%g", 2,
        sodof, stso, inso, soiter, rchiso);
    codatum->iteratio[FITOPT_NDX_SO] = soiter;
    codatum->redchisq[FITOPT_NDX_SO]= rchiso;
    populate_fitamp_so(codatum);
}

#endif /* HAVE_GSL_GSL_MULTIFIT_NLINEAR_H */
/*
 * eof: vim:nospell
 */
