/*
 * Support for the plateau-slope fitting function.
 * see explanations in fit_ampl.c for model functions.
 *
 * Created May 20 1995 by GBC based on predecessor cofit sources
 */
#include "fit_gsl.h"

#if HAVE_GSL_GSL_MULTIFIT_NLIN_H
/* the plateau-slope fitting function */
int gcy_ps_f(const gsl_vector * x, void *data, gsl_vector * f)
{
    size_t n = ((Data *)data)->n, ii;
    double *t = ((Data *)data)->t;
    double *y = ((Data *)data)->y;
    double *sigma = ((Data *)data)->sigma;
    double A = gsl_vector_get (x, 0);
    double B = gsl_vector_get (x, 1);
    double C = gsl_vector_get (x, 2), Yi;
    for (ii = 0; ii < n; ii++) {
        Yi = A;
        if (t[ii] > C) Yi += B * log10(t[ii]/C);
        gsl_vector_set(f, ii, (Yi - y[ii]) / sigma[ii]);
    }
    return(GSL_SUCCESS);
}
void populate_gcyamp_ps(cosumary *codatum)
{
    int ii;
    double A = codatum->pspar[0], B = codatum->pspar[1], C = codatum->pspar[2];
    for (ii = 0; ii < codatum->nsegtime; ii++) {
        codatum->fitaps[ii] = A;
        if (codatum->seglen[ii] > C)
            codatum->fitaps[ii] += B * log10(codatum->seglen[ii]/C);
    }
}

/* The plateau-slope version of the Jacobian and fdf */
int gcy_ps_df(const gsl_vector * x, void *data, gsl_matrix * J)
{
    size_t n = ((Data *)data)->n, ii;
    double *t = ((Data *)data)->t;
    double *sigma = ((Data *)data)->sigma;
    double A = gsl_vector_get (x, 0);
    double B = gsl_vector_get (x, 1);
    double C = gsl_vector_get (x, 2);
    for (ii = 0; ii < n; ii++) {
        if (t[ii] < C) {    /* plateau */
            gsl_matrix_set(J, ii, 0, 1.0 / sigma[ii]);
            gsl_matrix_set(J, ii, 1, 0.0);
            gsl_matrix_set(J, ii, 2, 0.0);
        } else {            /* slope */
            gsl_matrix_set(J, ii, 0, 1.0 / sigma[ii]);
            gsl_matrix_set(J, ii, 1, log(t[ii]/C) / sigma[ii]);
            gsl_matrix_set(J, ii, 2, - B / (C * M_LN10) / sigma[ii]);
        }
    }
    return(GSL_SUCCESS);
}
int gcy_ps_fdf(const gsl_vector * x, void *data, gsl_vector * f, gsl_matrix * J)
{
    gcy_ps_f(x, data, f);
    gcy_ps_df(x, data, J);
    return(GSL_SUCCESS);
}

/* gsl_blas_dnrm2(x) = sqrt(sum(x^2)), i.e. sqrt(chi^2)
 * modern code computes chisq as gsl_blas_ddot(f, f, &chisq[1]);
 * where f is residual vector including weights */

/* the plateau-slope callback */
void gcy_ps_callback(
    const size_t iter, gsl_multifit_fdfsolver *wrk, double *chisq)
{
    msg("iter %2zu: A = %.4f, B = %.4f, C = %.4f, |f(x)| = %.4f", 0,
        iter, gsl_vector_get(wrk->x, 0), gsl_vector_get(wrk->x, 1),
        gsl_vector_get(wrk->x, 2), (*chisq = gsl_blas_dnrm2(wrk->f)));
}

/* the plateau-slope fit driver */
int gcy_ps_driver(Data *dp, double psini[3], double pserr[3], double chisq[2],
    int *dofp, int *status, int *info, int *niter, double *rchisq)
{
    const size_t ps = 3;
    const size_t n = dp->n;
    const gsl_multifit_fdfsolver_type *T = gsl_multifit_fdfsolver_lmsder;
    gsl_multifit_fdfsolver *wrk = gsl_multifit_fdfsolver_alloc (T, n, ps);
    gsl_matrix *J = gsl_matrix_alloc(n, ps);
    gsl_matrix *covar = gsl_matrix_alloc (ps, ps);
    gsl_multifit_function_fdf fun;
    gsl_vector_view x = gsl_vector_view_array(psini, ps);
    size_t ii, iter = 0;
    double efac;
    /* connect up the plateau-slope, plateau-only or slope-only functions */
    fun.f = &gcy_ps_f; fun.df = &gcy_ps_df; fun.fdf = &gcy_ps_fdf;
    fun.n = n; fun.p = ps; fun.params = dp;
    gsl_multifit_fdfsolver_set (wrk, &fun, &x.vector);
    gcy_ps_callback(iter, wrk, &chisq[0]);
    /* solve the system with a maximum of maxits iterations */
    do {
        iter++;
        *status = gsl_multifit_fdfsolver_iterate(wrk);
        gcy_ps_callback(iter, wrk, &chisq[1]);
        if (*status) break;
        *status = gsl_multifit_test_delta(wrk->dx, wrk->x, xtol, xtol);
    } while (*status == GSL_CONTINUE && iter < maxits);
    gcy_ps_df(wrk->x, dp, J);
    gsl_multifit_covar(J, 0.0, covar);
    *dofp = n - ps;
    /* output fit summaries */
    msg("ps summary from method '%s'", 1, gsl_multifit_fdfsolver_name(wrk));
    msg("ps number of iterations: %zu", 1, (*niter = iter));
    msg("ps reason for stopping: %s", 1,
        (*status == GSL_SUCCESS) ? "small step size" : "otherwise");
    *info = *status;
    msg("ps initial |f(x)| = %f", 1, (chisq[0])); chisq[0] *= chisq[0];
    msg("ps final   |f(x)| = %f", 1, (chisq[1])); chisq[1] *= chisq[1];
    *rchisq = chisq[1] / *dofp;
    efac = GSL_MAX_DBL(1, sqrt(*rchisq));
    msg("ps chisq/dof = %g (%g)", 1, *rchisq, efac * efac);
    /* capture final parameter values for amp plots */
    msg("ps A = %.5f +/- %.5f", 1, psini[0] = FIT(0), pserr[0] = efac*ERR(0));
    msg("ps B = %.5f +/- %.5f", 1, psini[1] = FIT(1), pserr[1] = efac*ERR(1));
    msg("ps C = %.5f +/- %.5f", 1, psini[2] = FIT(2), pserr[2] = efac*ERR(2));
    msg("ps status (legacy) = %s", 1, gsl_strerror(*status));
    /* cleanup */
    gsl_multifit_fdfsolver_free(wrk);
    gsl_matrix_free(J);
    gsl_matrix_free(covar);
    return(GSL_SUCCESS);
}

/* plateau-slope fitting */
void plateau_slope_gcy(cosumary *codatum, Data *dp)
{
    int psdof, rcps, stps, inps, psiter;
    double pschi[2], rchips = -1;
    codatum->didfits |= FITOPT_AMP_PS;
    msg("", 1); msg("plateau-slope amplitude fit (legacy)", 2);
    msg("init ps: A = %.4f, B = %.4f, C = %.4f, ", 1,
        codatum->pspar[0], codatum->pspar[1], codatum->pspar[2]);
    rcps = gcy_ps_driver(dp, codatum->pspar, codatum->pserr, pschi,
        &psdof, &stps, &inps, &psiter, &rchips);
    msg("fini ps: dof=%d stat=%d info=%d iters=%d redchi=%g", 2,
        psdof, stps, inps, psiter, rchips);
    codatum->iteratio[FITOPT_NDX_PS] = psiter;
    codatum->redchisq[FITOPT_NDX_PS] = rchips;
    populate_gcyamp_ps(codatum);
}

#endif /* HAVE_GSL_GSL_MULTIFIT_NLIN_H */
/*
 * eof: vim:nospell
 */
