/*
 * Support for the plateau-only fitting function.
 * see explanations in fit_ampl.c for model functions.
 *
 * Created Apr 26 1995 by GBC based on predecessor cofit sources
 */
#include "fit_gsl.h"

#if HAVE_GSL_GSL_MULTIFIT_NLINEAR_H
/* the plateau-only fitting function */
int po_f(const gsl_vector * x, void *data, gsl_vector * f)
{
    size_t n = ((Data *)data)->n, ii;
    double *t = ((Data *)data)->t;
    double *y = ((Data *)data)->y;
    double A = gsl_vector_get (x, 0), Yi;
    for (ii = 0; ii < n; ii++) {
        Yi = A;
        gsl_vector_set(f, ii, Yi - y[ii]);
    }
    return(GSL_SUCCESS);
}
void populate_fitamp_po(cosumary *codatum)
{
    int ii;
    double A = codatum->popar[0];
    for (ii = 0; ii < codatum->nsegtime; ii++) {
        codatum->fitapo[ii] = A;
    }
}

/* The plateau-only version of the Jacobian */
int po_df(const gsl_vector * x, void *data, gsl_matrix * J)
{
    size_t n = ((Data *)data)->n, ii;
    double *t = ((Data *)data)->t;
    double A = gsl_vector_get (x, 0);
    for (ii = 0; ii < n; ii++) {
        gsl_matrix_set(J, ii, 0, 1.0);
    }
    return(GSL_SUCCESS);
}

/* the plateau-only callback */
void po_callback(const size_t iter, void *params,
              const gsl_multifit_nlinear_workspace *w)
{
    gsl_vector *f = gsl_multifit_nlinear_residual(w);
    gsl_vector *x = gsl_multifit_nlinear_position(w);
    double rcond;
    /* compute reciprocal condition number of J(x) */
    gsl_multifit_nlinear_rcond(&rcond, w);
    msg("iter %2zu: A = %.4f, "
        "cond(J) = %8.4g, |f(x)| = %.4f", 0, iter,
        gsl_vector_get(x, 0), 1.0 / rcond, gsl_blas_dnrm2(f));
}

/* the plateau-only fit driver */
int po_driver(Data *dp, double poini[1], double poerr[1], double chisq[2],
    int *dofp, int *status, int *info, int *niter, double *rchisq)
{
    const size_t po = 1;
    const size_t n = dp->n;
    const gsl_multifit_nlinear_type *T = gsl_multifit_nlinear_trust;
    double efac;
    gsl_multifit_nlinear_workspace *wrk;
    gsl_multifit_nlinear_fdf fdf;
    gsl_multifit_nlinear_parameters fdf_params =
        gsl_multifit_nlinear_default_parameters();
    gsl_vector *f;
    gsl_matrix *J, *covar = gsl_matrix_alloc(po, po);
    gsl_vector_view x = gsl_vector_view_array (poini, po);
    gsl_vector_view wts = gsl_vector_view_array(dp->wgt, n);
    /* connect up the plateau-slope, plateau-only or slope-only functions */
    fdf.f = po_f; fdf.df = po_df;
    fdf.fvv = NULL; fdf.n = n; fdf.p = po; fdf.params = dp;
    /* various initializations; see gslcheck/expfitexample.c */
    wrk = gsl_multifit_nlinear_alloc (T, &fdf_params, n, po);
    gsl_multifit_nlinear_winit (&x.vector, &wts.vector, &fdf, wrk);
    f = gsl_multifit_nlinear_residual(wrk);
    gsl_blas_ddot(f, f, &chisq[0]);
    /* solve the system with a maximum of maxits iterations */
    *status = gsl_multifit_nlinear_driver(
        maxits, xtol, gtol, ftol, po_callback, NULL, info, wrk);
    /* compute best fit covariance and final cost*/
    J = gsl_multifit_nlinear_jac(wrk);
    gsl_multifit_nlinear_covar (J, 0.0, covar);
    gsl_blas_ddot(f, f, &chisq[1]);
    *dofp = n - po;
    *rchisq = chisq[1] / *dofp;
    efac = GSL_MAX_DBL(1, sqrt(*rchisq));
    /* output fit summaries */
    msg("po summary from method '%s/%s'", 1,
        gsl_multifit_nlinear_name(wrk), gsl_multifit_nlinear_trs_name(wrk));
    msg("po number of iterations: %zu", 1,
        *niter = gsl_multifit_nlinear_niter(wrk));
    msg("po function evaluations: %zu", 1, fdf.nevalf);
    msg("po Jacobian evaluations: %zu", 1, fdf.nevaldf);
    msg("po reason for stopping: %s", 1,
        (*info == 1) ? "small step size" : "small gradient");
    msg("po initial |f(x)| = %f", 1, sqrt(chisq[0]));
    msg("po final   |f(x)| = %f", 1, sqrt(chisq[1]));
    msg("po chisq/dof = %g (%g)", 1, *rchisq, efac * efac);
    /* capture final parameter values for amp plots */
    msg("po A = %.5f +/- %.5f", 1, poini[0] = FIT(0), poerr[0] = efac*ERR(0));
    msg("po status (modern) = %s", 1, gsl_strerror(*status));
    gsl_multifit_nlinear_free (wrk);
    gsl_matrix_free (covar);
    return(GSL_SUCCESS);
}

/* plateau-only fitting */
void plateau_only_fit(cosumary *codatum, Data *dp)
{
    int podof, rcpo, stpo, inpo, poiter;
    double pochi[2], rchipo = -1;
    codatum->didfits |= FITOPT_AMP_PO;
    msg("", 1); msg("plateau-only amplitude fit (modern)", 2);
    msg("init po: A = %.4f", 1,
        codatum->popar[0]);
    rcpo = po_driver(dp, codatum->popar, codatum->poerr, pochi,
        &podof, &stpo, &inpo, &poiter, &rchipo);
    msg("fini po: dof=%d stat=%d info=%d iters=%d redchi=%g", 2,
        podof, stpo, inpo, poiter, rchipo);
    codatum->iteratio[FITOPT_NDX_PO] = poiter;
    codatum->redchisq[FITOPT_NDX_PO]= rchipo;
    populate_fitamp_po(codatum);
}

#endif /* HAVE_GSL_GSL_MULTIFIT_NLINEAR_H */
/*
 * eof: vim:nospell
 */
