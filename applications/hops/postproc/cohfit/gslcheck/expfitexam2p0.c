//
// This is a revised version of expfitexam2p1.c adjusted to
// look like expfitexample.c, including the function tested.
// See expfitexam2p1.c for source code origin.
//
#include <stdlib.h>
#include <stdio.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_multifit_nlin.h>

#define N      100    /* number of data points to fit */
#define TMAX   (3.0)  /* time variable in [0,TMAX] */

struct data {
  size_t n;
  double * t;
  double * y;
  double * sigma;       // this is new
};

int
expb_f (const gsl_vector * x, void *data,
        gsl_vector * f)
{
  size_t n = ((struct data *)data)->n;
  double *t = ((struct data *)data)->t;
  double *y = ((struct data *)data)->y;
  double *sigma = ((struct data *) data)->sigma;

  double A = gsl_vector_get (x, 0);
  double lambda = gsl_vector_get (x, 1);
  double b = gsl_vector_get (x, 2);

  size_t i;

  for (i = 0; i < n; i++)
    {
      /* Model Yi = A * exp(-lambda * t_i) + b */
      double Yi = A * exp (-lambda * t[i]) + b;
      gsl_vector_set (f, i, (Yi - y[i])/sigma[i]);
    }

  return GSL_SUCCESS;
}

int
expb_df (const gsl_vector * x, void *data,
         gsl_matrix * J)
{
  size_t n = ((struct data *)data)->n;
  double *t = ((struct data *)data)->t;
  double *sigma = ((struct data *)data)->sigma;

  double A = gsl_vector_get (x, 0);
  double lambda = gsl_vector_get (x, 1);

  size_t i;

  for (i = 0; i < n; i++)
    {
      /* Jacobian matrix J(i,j) = dfi / dxj, */
      /* where fi = (Yi - yi)/sigma[i],      */
      /*       Yi = A * exp(-lambda * t) + b  */
      /* and the xj are the parameters (A,lambda,b) */
      double s = sigma[i];
      double e = exp(-lambda * t[i]);
      gsl_matrix_set (J, i, 0, e/s);
      gsl_matrix_set (J, i, 1, -t[i] * A * e/s);
      gsl_matrix_set (J, i, 2, 1.0/s);
    }

  return GSL_SUCCESS;
}

int
expb_fdf (const gsl_vector * x, void *params,
          gsl_vector * f, gsl_matrix * J)
{
  expb_f (x, params, f);
  expb_df (x, params, J);

  return GSL_SUCCESS;
}

void
callback(size_t iter, gsl_multifit_fdfsolver * s)
{
  printf("iter: %lu x = %10.8f %10.8f %10.8f |f(x)| = %g\n",
          iter,
          gsl_vector_get(s->x, 0),
          gsl_vector_get(s->x, 1),
          gsl_vector_get(s->x, 2),
          gsl_blas_dnrm2(s->f));
}

int
main (void)
{
  const gsl_multifit_fdfsolver_type *T;
  gsl_multifit_fdfsolver *s;
  int status;
  size_t i, iter = 0;

  const size_t n = N;
  const size_t p = 3;
  const double xtol = 5e-8;

  gsl_matrix *J = gsl_matrix_alloc(n, p);
  gsl_matrix *covar = gsl_matrix_alloc (p, p);
  double t[n], y[N], sigma[N];    // weights
  struct data d = { n, t, y, sigma };

  gsl_multifit_function_fdf f;
  double x_init[3] = { 1.0, 1.0, 0.0 }; /* starting values */

  gsl_vector_view x = gsl_vector_view_array (x_init, p);

  const gsl_rng_type *type;
  gsl_rng * r;
  gsl_rng_env_setup();
  type = gsl_rng_default;
  r = gsl_rng_alloc(type);

  /* define the function to be minimized */
  f.f = &expb_f;
  f.df = &expb_df;
  f.fdf = &expb_fdf;
  f.n = n;
  f.p = p;
  f.params = &d;

  /* this is the data to be fitted */
  for (i = 0; i < n; i++)
    {
      t[i] = (double)i * TMAX / ((double)n - 1.0);
      y[i] = 1.0 + 5 * exp (-1.5 * t[i])
           + gsl_ran_gaussian(r, 0.1);
      sigma[i] = 0.1;
      printf("data: %g %g %g\n", t[i], y[i], sigma[i]);
    };

  T = gsl_multifit_fdfsolver_lmsder;
  s = gsl_multifit_fdfsolver_alloc (T, n, p);
  gsl_multifit_fdfsolver_set (s, &f, &x.vector);

  callback(iter, s);

  // seems we have an explicit driver here
  // int gsl_multifit_test_delta (const gsl_vector * dx,
  //   const gsl_vector * x, double epsabs, double epsrel)
  // old: |dx_i| < epsabs + epsrel |x_i|
  //
  // 2.8: |dx_i| < xtol(|x_i| + xtol) = xtol^2 + xtol( |x_i| )
  do {
    iter++;
    status = gsl_multifit_fdfsolver_iterate (s);
    //printf ("status = %s\n", gsl_strerror (status));
    callback(iter, s);
    if (status) break;
    status = gsl_multifit_test_delta (s->dx, s->x, xtol, xtol);
  } while (status == GSL_CONTINUE && iter < 500);

  expb_df (s->x, &d, J);
  gsl_multifit_covar (J, 0.0, covar);

#define FIT(i) gsl_vector_get(s->x, i)
#define ERR(i) sqrt(gsl_matrix_get(covar,i,i))

  {
    printf("\n");
    printf("A      = %.5f +/- %.5f\n", FIT(0), ERR(0));
    printf("lambda = %.5f +/- %.5f\n", FIT(1), ERR(1));
    printf("b      = %.5f +/- %.5f\n", FIT(2), ERR(2));
  }
  printf ("status = %s\n", gsl_strerror (status));

// ffun: output final function evaluation
  for (i = 0; i < n; i++) {
      double Yi = FIT(2) + FIT(0) * exp (-FIT(1) * t[i]);
      double yi = 1.0 + 5 * exp (-1.5 * t[i]);
      printf ("fact: %g %g\n", t[i], yi);
      printf ("ffun: %g %g\n", t[i], Yi);
  }
// ffun: finished

  gsl_multifit_fdfsolver_free (s);
  gsl_matrix_free (covar);
  gsl_rng_free (r);

  return 0;
}

// eof vim:nospell
