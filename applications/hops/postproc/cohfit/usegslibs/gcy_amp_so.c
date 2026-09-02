/*
 * Support for the slope-only fitting function.
 * see explanations in fit_ampl.c for model functions.
 *
 * Created May 20 1995 by GBC based on predecessor cofit sources
 */
#include "fit_gsl.h"

#if HAVE_GSL_GSL_MULTIFIT_NLIN_H
/* the slope-only fitting function */
int gcy_so_f(const gsl_vector * x, void *data, gsl_vector * f)
{
    size_t n = ((Data *)data)->n, ii;
    double *t = ((Data *)data)->t;
    double *y = ((Data *)data)->y;
    double *sigma = ((Data *)data)->sigma;
    double A = gsl_vector_get (x, 0), Yi;
    double B = gsl_vector_get (x, 1);
    double C = t[0];
    for (ii = 0; ii < n; ii++) {
        Yi = A + B * log10(t[ii]/C);
        gsl_vector_set(f, ii, (Yi - y[ii]) / sigma[ii]);
    }
    return(GSL_SUCCESS);
}
void populate_gcyamp_so(cosumary *codatum)
{
    int ii;
    double A = codatum->sopar[0], B = codatum->sopar[1];
    double C = codatum->seglen[0];
    for (ii = 0; ii < codatum->nsegtime; ii++) {
        codatum->fitaso[ii] = A + B * log10(codatum->seglen[ii]/C);
    }
}

/* The slope-only version of the Jacobian and fdf */
int gcy_so_df(const gsl_vector * x, void *data, gsl_matrix * J)
{
    size_t n = ((Data *)data)->n, ii;
    double *t = ((Data *)data)->t;
    double *sigma = ((Data *)data)->sigma;
    double A = gsl_vector_get (x, 0);
    double B = gsl_vector_get (x, 1);
    double C = t[0];
    for (ii = 0; ii < n; ii++) {
        gsl_matrix_set(J, ii, 0, 1.0 / sigma[ii]);
        gsl_matrix_set(J, ii, 1, log(t[ii]/C) / sigma[ii]);
    }
    return(GSL_SUCCESS);
}
int gcy_so_fdf(const gsl_vector * x, void *data, gsl_vector * f, gsl_matrix * J)
{
    gcy_so_f(x, data, f);
    gcy_so_df(x, data, J);
    return(GSL_SUCCESS);
}

/* gsl_blas_dnrm2(x) = sqrt(sum(x^2)), i.e. sqrt(chi^2)
 * modern code computes chisq as gsl_blas_ddot(f, f, &chisq[1]);
 * where f is residual vector including weights */

/* the slope-only callback */
void gcy_so_callback(
    const size_t iter, gsl_multifit_fdfsolver *wrk, double *chisq)
{
    msg("iter %2zu: A = %.4f, B = %.4f, |f(x)| = %.4f", 0,
        iter, gsl_vector_get(wrk->x, 0), gsl_vector_get(wrk->x, 1),
        (*chisq = gsl_blas_dnrm2(wrk->f)));
}

/* the slope-only fit driver */
int gcy_so_driver(Data *dp, double soini[2], double soerr[2], double chisq[2],
    int *dofp, int *status, int *info, int *niter, double *rchisq)
{
    const size_t so = 2;
    const size_t n = dp->n;
    const gsl_multifit_fdfsolver_type *T = gsl_multifit_fdfsolver_lmsder;
    gsl_multifit_fdfsolver *wrk = gsl_multifit_fdfsolver_alloc (T, n, so);
    gsl_matrix *J = gsl_matrix_alloc(n, so);
    gsl_matrix *covar = gsl_matrix_alloc (so, so);
    gsl_multifit_function_fdf fun;
    gsl_vector_view x = gsl_vector_view_array(soini, so);
    size_t ii, iter = 0;
    double efac;
    /* connect up the plateau-slope, plateau-only or slope-only functions */
    fun.f = &gcy_so_f; fun.df = &gcy_so_df; fun.fdf = &gcy_so_fdf;
    fun.n = n; fun.p = so; fun.params = dp;
    gsl_multifit_fdfsolver_set (wrk, &fun, &x.vector);
    gcy_so_callback(iter, wrk, &chisq[0]);
    /* solve the system with a maximum of maxits iterations */
    do {
        iter++;
        *status = gsl_multifit_fdfsolver_iterate(wrk);
        gcy_so_callback(iter, wrk, &chisq[1]);
        if (*status) break;
        *status = gsl_multifit_test_delta(wrk->dx, wrk->x, xtol, xtol);
    } while (*status == GSL_CONTINUE && iter < maxits);
    gcy_so_df(wrk->x, dp, J);
    gsl_multifit_covar(J, 0.0, covar);
    *dofp = n - so;
    /* output fit summaries */
    msg("so summary from method '%s'", 1, gsl_multifit_fdfsolver_name(wrk));
    msg("so number of iterations: %zu", 1, (*niter = iter));
    msg("so reason for stopping: %s", 1,
        (*status == GSL_SUCCESS) ? "small step size" : "otherwise");
    *info = *status;
    msg("so initial |f(x)| = %f", 1, (chisq[0])); chisq[0] *= chisq[0];
    msg("so final   |f(x)| = %f", 1, (chisq[1])); chisq[1] *= chisq[1];
    *rchisq = chisq[1] / *dofp;
    efac = GSL_MAX_DBL(1, sqrt(*rchisq));
    msg("so chisq/dof = %g (%g)", 1, *rchisq, efac * efac);
    /* capture final parameter values for amp plots */
    msg("so A = %.5f +/- %.5f", 1, soini[0] = FIT(0), soerr[0] = efac*ERR(0));
    msg("so B = %.5f +/- %.5f", 1, soini[1] = FIT(1), soerr[1] = efac*ERR(1));
    msg("so status (legacy) = %s", 1, gsl_strerror(*status));
    /* cleanup */
    gsl_multifit_fdfsolver_free(wrk);
    gsl_matrix_free(J);
    gsl_matrix_free(covar);
    return(GSL_SUCCESS);
}

/* slope-only fitting */
void slope_only_gcy(cosumary *codatum, Data *dp)
{
    int sodof, rcso, stso, inso, soiter;
    double sochi[2], rchiso = -1;
    codatum->didfits |= FITOPT_AMP_SO;
    msg("", 1); msg("slope-only amplitude fit (legacy)", 2);
    msg("init so: A = %.4f, B = %.4f", 1,
        codatum->sopar[0], codatum->sopar[1]);
    rcso = gcy_so_driver(dp, codatum->sopar, codatum->soerr, sochi,
        &sodof, &stso, &inso, &soiter, &rchiso);
    msg("fini so: dof=%d stat=%d info=%d iters=%d redchi=%g", 2,
        sodof, stso, inso, soiter, rchiso);
    codatum->iteratio[FITOPT_NDX_SO] = soiter;
    codatum->redchisq[FITOPT_NDX_SO]= rchiso;
    populate_gcyamp_so(codatum);
}

#endif /* HAVE_GSL_GSL_MULTIFIT_NLIN_H */
/*
 * eof: vim:nospell
 */
