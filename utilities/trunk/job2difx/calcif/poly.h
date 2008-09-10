#ifndef __POLY_H__
#define __POLY_H__

/* Implement a specific variant of Neville's algorithm for equally spaced
 * data.  The expansion is about x=0 and the input data points are 
 * expected to be at x = d*i for i = [0, n) 
 */
void computePoly(double *p, int n, double d);

/* Use Cramer's rule to evaluate polynomial */
double evaluatePoly(const double *p, int n, double x);

double evaluatePolyDeriv(const double *p, int n, double x);

#endif
