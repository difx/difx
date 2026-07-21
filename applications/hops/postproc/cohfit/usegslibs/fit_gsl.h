/*
 * Replacements for numerical recipe fitting routines that use GSL.
 *
 * fit_snr.c does the 3pt parabolic fit (as SSD coded it) but
 * fit_msnr.c now calls it so that we have two fits.
 *
 * The GSL nonlinear fitting framework was revised for GSL 2.2
 * Prior to that (at least as early as GSL 1.15) an older ...nlin.h
 * framework existed.  We have support here for both as old code
 * may yet exist in newer versions of GSL for backwards compatibility.
 */
#ifndef __fit_gsl_h__
#define __fit_gsl_h__

#include "hops_config.h"

#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_bspline.h>
#include <gsl/gsl_min.h>
#include <gsl/gsl_multifit.h>

#ifndef HAVE_GSL_GSL_MULTIFIT_NLINEAR_H
#define HAVE_GSL_GSL_MULTIFIT_NLINEAR_H 0
#endif /* HAVE_GSL_GSL_MULTIFIT_NLINEAR_H */
#ifndef HAVE_GSL_GSL_MULTIFIT_NLIN_H
#define HAVE_GSL_GSL_MULTIFIT_NLIN_H 0
#endif /* HAVE_GSL_GSL_MULTIFIT_NLIN_H */

#if HAVE_GSL_GSL_MULTIFIT_NLINEAR_H
#include <gsl/gsl_multifit_nlinear.h>
#endif /* HAVE_GSL_GSL_MULTIFIT_NLINEAR_H */
#if HAVE_GSL_GSL_MULTIFIT_NLIN_H
#include <gsl/gsl_multifit_nlin.h>
#endif /* HAVE_GSL_GSL_MULTIFIT_NLIN_H */

/* which gsl/gsl_multifit library to use for amp fits */
#if HAVE_GSL_GSL_MULTIFIT_NLINEAR_H
#define GSLEGACY_DEFAULT    0
#elif HAVE_GSL_GSL_MULTIFIT_NLIN_H
#define GSLEGACY_DEFAULT    1
#else
#warning "missing gsl/gsl_multifit libraries"
#define GSLEGACY_DEFAULT    -1
#endif

#include <gsl/gsl_statistics.h>
#include "cohfit.h"

/* data structure holding fit points; wgt and sigma are redundant,
 * but the newer formulation uses wgt and the older one sigma,
 * so we will allow for both in this structure. */
typedef struct data {
    size_t n;       /* number of data points */
    double *t;      /* the time values */
    double *y;      /* the data to fit */
    double *wgt;    /* 1/sigma^2 */
    double *sigma;  /* for the */
} Data;

/* constants needed for gsl_multifit_nlinear_driver() */
extern const double xtol, gtol, ftol;
extern const size_t maxits;

/* wrk and cover are the variables in ??_driver() where these are used */
#define FIT(ii) gsl_vector_get(wrk->x, ii)
#define ERR(ii) sqrt(gsl_matrix_get(covar,ii,ii))

/* modern "fit" routines */
#if HAVE_GSL_GSL_MULTIFIT_NLINEAR_H

/* machinery for the plateau-slope fitting (ps) */
extern int ps_f(const gsl_vector * x, void *data, gsl_vector * f);
extern int ps_df(const gsl_vector * x, void *data, gsl_matrix * J);
extern void ps_callback(const size_t iter, void *params,
    const gsl_multifit_nlinear_workspace *w);
extern int ps_driver(Data *dp, double psi[3], double pse[3], double chisq[2],
    int *dofp, int *status, int *info, int *niter, double *rchisq);
extern void populate_fitamp_ps(cosumary *codatum);
extern void plateau_slope_fit(cosumary *codatum, Data *dp);

/* machinery for the plateau-only fitting (po) */
extern int po_f(const gsl_vector * x, void *data, gsl_vector * f);
extern int po_df(const gsl_vector * x, void *data, gsl_matrix * J);
extern void po_callback(const size_t iter, void *params,
    const gsl_multifit_nlinear_workspace *w);
extern int po_driver(Data *dp, double poi[1], double poe[1], double chisq[2],
    int *dofp, int *status, int *info, int *niter, double *rchisq);
extern void populate_fitamp_po(cosumary *codatum);
extern void plateau_only_fit(cosumary *codatum, Data *dp);

/* machinery for the slope-only fitting (so) */
extern int so_f(const gsl_vector * x, void *data, gsl_vector * f);
extern int so_df(const gsl_vector * x, void *data, gsl_matrix * J);
extern void so_callback(const size_t iter, void *params,
    const gsl_multifit_nlinear_workspace *w);
extern int so_driver(Data *dp, double soi[2], double soe[2], double chisq[2],
    int *dofp, int *status, int *info, int *niter, double *rchisq);
extern void populate_fitamp_so(cosumary *codatum);
extern void slope_only_fit(cosumary *codatum, Data *dp);

#endif /* HAVE_GSL_GSL_MULTIFIT_NLINEAR_H */

/* legacy "gcy" routines */
#if HAVE_GSL_GSL_MULTIFIT_NLIN_H

/* machinery for the plateau-slope fitting (ps) */
extern int gcy_ps_f(const gsl_vector * x, void *data, gsl_vector * f);
extern int gcy_ps_df(const gsl_vector * x, void *data, gsl_matrix * J);
extern int gcy_ps_fdf(
    const gsl_vector * x, void *data, gsl_vector * f, gsl_matrix * J);
extern void gcy_ps_callback(
    const size_t iter, gsl_multifit_fdfsolver *wrk, double *chisq);
extern void populate_gcyamp_ps(cosumary *codatum);
extern int gcy_ps_driver(
    Data *dp, double psi[3], double pse[3], double chisq[2],
    int *dofp, int *status, int *info, int *niter, double *rchisq);
extern void plateau_slope_gcy(cosumary *codatum, Data *dp);

/* machinery for the plateau-only fitting (po) */
extern int gcy_po_f(const gsl_vector * x, void *data, gsl_vector * f);
extern int gcy_po_df(const gsl_vector * x, void *data, gsl_matrix * J);
extern int gcy_po_fdf(
    const gsl_vector * x, void *data, gsl_vector * f, gsl_matrix * J);
extern void gcy_po_callback(
    const size_t iter, gsl_multifit_fdfsolver *wrk, double *chisq);
extern void populate_gcyamp_po(cosumary *codatum);
extern int gcy_po_driver(
    Data *dp, double poi[1], double poe[1], double chisq[2],
    int *dofp, int *status, int *info, int *niter, double *rchisq);
extern void plateau_only_gcy(cosumary *codatum, Data *dp);

/* machinery for the slope-only fitting (so) */
extern int gcy_so_f(const gsl_vector * x, void *data, gsl_vector * f);
extern int gcy_so_df(const gsl_vector * x, void *data, gsl_matrix * J);
extern int gcy_so_fdf(
    const gsl_vector * x, void *data, gsl_vector * f, gsl_matrix * J);
extern void gcy_so_callback(
    const size_t iter, gsl_multifit_fdfsolver *wrk, double *chisq);
extern void populate_gcyamp_so(cosumary *codatum);
extern int gcy_so_driver(
    Data *dp, double soi[2], double soe[2], double chisq[2],
    int *dofp, int *status, int *info, int *niter, double *rchisq);
extern void slope_only_gcy(cosumary *codatum, Data *dp);

#endif /* HAVE_GSL_GSL_MULTIFIT_NLIN_H */

/* top level entries are in cohfit.h: support for fit_ampl(): */
extern double cohereguess(cosumary *codatum);
extern int choose_best_amp_fit(cosumary *codatum);

/* support for fit_msnr(); 3-pnt fit: */
extern double fit_snr(cosumary *, int);

/* cubic spline fit and machinery */
extern double fit_cbs2p8(cosumary *, int);
extern double fit_cbs2p7(cosumary *, int);
void min_inv_snr_cbs_err2p8(int status, int msglvl);
void min_inv_snr_cbs_err2p7(int status, int msglvl);
double invsnr2p7(double x, void *params);

/* these are slightly different */
int min_inv_snr_cbs2p8(cosumary *codatum, int npt, gsl_vector *x,
    gsl_vector *control, gsl_bspline_workspace *work, double *peakslen);
int min_inv_snr_cbs2p7(cosumary *codatum, int npt, gsl_vector *x,
    gsl_bspline_workspace *work,
    gsl_vector *B, gsl_vector *c, gsl_matrix *cov, double *peakslen);

#endif /* __fit_gsl_h__ */
/*
 * eof vim:nospell
 */
