#ifndef FFMATH_H__
#define FFMATH_H__

extern int ap_mean (double start, double stop, double *coords, double *val1, double *val2, int n, int *nstart, double *result1, double *result2);
extern void bcd_to_2int (int input, int ndigits, int npoint, int result[2]);
extern void interp555 (double drf[5][5][5], double xi[3], double *drfval);
extern int linterp (double coord1, double value1, double coord2, double value2, double coord, double *value);
extern void max555 (double drf[5][5][5], double xlim[3][2], double xi[3], double *drfmax);  
//extern int minvert (size_t n,  double* a, double* ainv);
extern int minvert3( double a[3][3],double ainv[3][3]);
extern int minvert5( double a[5][5],double ainv[5][5]);
extern int parabola (double y[3], double lower, double upper, double* x_max, double* amp_max, double q[3]);

#endif /* end of include guard: FFMATH_H__ */
