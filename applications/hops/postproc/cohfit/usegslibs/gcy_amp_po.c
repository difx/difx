/*
 * Support for the plateau-only fitting function.
 * see explanations in fit_ampl.c for model functions.
 *
 * Created May 20 1995 by GBC based on predecessor cofit sources
 */
#include "fit_gsl.h"

#if HAVE_GSL_GSL_MULTIFIT_NLIN_H
/* the plateau-only fitting function */
int gcy_po_f(const gsl_vector * x, void *data, gsl_vector * f)
{
    size_t n = ((Data *)data)->n, ii;
    double *t = ((Data *)data)->t;
    double *y = ((Data *)data)->y;
    double *sigma = ((Data *)data)->sigma;
    double A = gsl_vector_get (x, 0), Yi;
    for (ii = 0; ii < n; ii++) {
        Yi = A;
        gsl_vector_set(f, ii, (Yi - y[ii]) / sigma[ii]);
    }
    return(GSL_SUCCESS);
}
void populate_gcyamp_po(cosumary *codatum)
{
    int ii;
    double A = codatum->popar[0];
    for (ii = 0; ii < codatum->nsegtime; ii++) {
        codatum->fitapo[ii] = A;
    }
}

/* The plateau-only version of the Jacobian and fdf */
int gcy_po_df(const gsl_vector * x, void *data, gsl_matrix * J)
{
    size_t n = ((Data *)data)->n, ii;
    double *t = ((Data *)data)->t;
    double *sigma = ((Data *)data)->sigma;
    double A = gsl_vector_get (x, 0);
    for (ii = 0; ii < n; ii++) {
        gsl_matrix_set(J, ii, 0, 1.0 / sigma[ii]);
    }
    return(GSL_SUCCESS);
}
int gcy_po_fdf(const gsl_vector * x, void *data, gsl_vector * f, gsl_matrix * J)
{
    gcy_po_f(x, data, f);
    gcy_po_df(x, data, J);
    return(GSL_SUCCESS);
}

/* gsl_blas_dnrm2(x) = sqrt(sum(x^2)), i.e. sqrt(chi^2)
 * modern code computes chisq as gsl_blas_ddot(f, f, &chisq[1]);
 * where f is residual vector including weights */

/* the plateau-only callback */
void gcy_po_callback(
    const size_t iter, gsl_multifit_fdfsolver *wrk, double *chisq)
{
    msg("iter %2zu: A = %.4f, |f(x)| = %.4f", 0,
        iter, gsl_vector_get(wrk->x, 0), (*chisq = gsl_blas_dnrm2(wrk->f)));
}

/* the plateau-only fit driver */
int gcy_po_driver(Data *dp, double poini[1], double poerr[1], double chisq[2],
    int *dofp, int *status, int *info, int *niter, double *rchisq)
{
    const size_t po = 1;
    const size_t n = dp->n;
    const gsl_multifit_fdfsolver_type *T = gsl_multifit_fdfsolver_lmsder;
    gsl_multifit_fdfsolver *wrk = gsl_multifit_fdfsolver_alloc (T, n, po);
    gsl_matrix *J = gsl_matrix_alloc(n, po);
    gsl_matrix *covar = gsl_matrix_alloc (po, po);
    gsl_multifit_function_fdf fun;
    gsl_vector_view x = gsl_vector_view_array(poini, po);
    size_t ii, iter = 0;
    double efac;
    /* connect up the plateau-slope, plateau-only or slope-only functions */
    fun.f = &gcy_po_f; fun.df = &gcy_po_df; fun.fdf = &gcy_po_fdf;
    fun.n = n; fun.p = po; fun.params = dp;
    gsl_multifit_fdfsolver_set (wrk, &fun, &x.vector);
    gcy_po_callback(iter, wrk, &chisq[0]);
    /* solve the system with a maximum of maxits iterations */
    do {
        iter++;
        *status = gsl_multifit_fdfsolver_iterate(wrk);
        gcy_po_callback(iter, wrk, &chisq[1]);
        if (*status) break;
        *status = gsl_multifit_test_delta(wrk->dx, wrk->x, xtol, xtol);
    } while (*status == GSL_CONTINUE && iter < maxits);
    gcy_po_df(wrk->x, dp, J);
    gsl_multifit_covar(J, 0.0, covar);
    *dofp = n - po;
    /* output fit summaries */
    msg("po summary from method '%s'", 1, gsl_multifit_fdfsolver_name(wrk));
    msg("po number of iterations: %zu", 1, (*niter = iter));
    msg("po reason for stopping: %s", 1,
        (*status == GSL_SUCCESS) ? "small step size" : "otherwise");
    *info = *status;
    msg("po initial |f(x)| = %f", 1, (chisq[0])); chisq[0] *= chisq[0];
    msg("po final   |f(x)| = %f", 1, (chisq[1])); chisq[1] *= chisq[1];
    *rchisq = chisq[1] / *dofp;
    efac = GSL_MAX_DBL(1, sqrt(*rchisq));
    msg("po chisq/dof = %g (%g)", 1, *rchisq, efac * efac);
    /* capture final parameter values for amp plots */
    msg("po A = %.5f +/- %.5f", 1, poini[0] = FIT(0), poerr[0] = efac*ERR(0));
    msg("po status (legacy) = %s", 1, gsl_strerror(*status));
    /* cleanup */
    gsl_multifit_fdfsolver_free(wrk);
    gsl_matrix_free(J);
    gsl_matrix_free(covar);
    return(GSL_SUCCESS);
}

/* plateau-only fitting */
void plateau_only_gcy(cosumary *codatum, Data *dp)
{
    int podof, rcpo, stpo, inpo, poiter;
    double pochi[2], rchipo = -1;
    codatum->didfits |= FITOPT_AMP_PO;
    msg("", 1); msg("plateau-only amplitude fit (legacy)", 2);
    msg("init po: A = %.4f", 1,
        codatum->popar[0]);
    rcpo = gcy_po_driver(dp, codatum->popar, codatum->poerr, pochi,
        &podof, &stpo, &inpo, &poiter, &rchipo);
    msg("fini po: dof=%d stat=%d info=%d iters=%d redchi=%g", 2,
        podof, stpo, inpo, poiter, rchipo);
    codatum->iteratio[FITOPT_NDX_PO] = poiter;
    codatum->redchisq[FITOPT_NDX_PO]= rchipo;
    populate_gcyamp_po(codatum);
}

#endif /* HAVE_GSL_GSL_MULTIFIT_NLIN_H */
/*
 * eof: vim:nospell
 */
