/*******************************************************************************
*                                                                              *
*  parabola(): Interpolates a parabola through 3 equally spaced points,        *
*              which are specified at the domain points (-1, 0, +1)            *
*                                                                              *
*  Input:  y[3]        vector of 3 amplitude points, which will be fit         *
*          lower       lower bound on location of maximum                      *
*          upper       upper   "   "      "    "     "                         *
*                                                                              *
*  Output: x_max       value of x at which maximum amplitude occurs            *
*          amp_max     value of amplitude at x_max                             *
*          q[3]        quadratic polynomial coefficients of the interpolating  *
*                      polynomial: y = q[0] * x * x + q[1] * x + q[2]          *
*          parabola()  function returns these values:                          *
*                      0:  normal error-free return                            *
*                      1:  true maximum would have been out of bounds          *
*                      2:  no maximum: 3 point net has positive curvature      *
*                                                                              *
*                      rjc  94.1.10   Initial code                             *
*                      rjc  94.3.18   On non-negative curvature,pick high pt   *
*                      cjl  99.12.1   Make test for 1-return more robust       *
*******************************************************************************/
#include <math.h>

int parabola (y, lower, upper, x_max, amp_max, q)

double y[3],
       lower,
       upper,
       *x_max,
       *amp_max,
       q[3];
    {
    int i,
        rc;
    double x, range, dwin(), c_mag();

    range = fabs (upper - lower);


    q[0] = (y[0] - 2 * y[1] + y[2]) / 2;      /* This is trivial to derive,
		                              or see rjc's 94.1.10 derivation */
    q[1] = (y[2] - y[0]) / 2;

    q[2] = y[1];


    if (q[0] < 0.0)
        x = -q[1] / (2 * q[0]);                      /* x value at maximum y */
    else                                         /* no max, pick higher side */
        x = (y[2] > y[0]) ? 1.0 : -1.0;

    *x_max = dwin (x, lower, upper);

    *amp_max = q[0] * *x_max * *x_max  +  q[1] * *x_max  +  q[2];

                                    // Test for error conditions

    rc = 0;                         // default: indicates error-free interpolation
    if (q[0] >= 0)                  // 0 or positive curvature is an interpolation error
	    rc = 2;
                                    // Is maximum at either edge?
                                    // (simple floating point equality test can fail
                                    // in machine-dependent way)
    else if (fabs (*x_max - x) > (0.001 * range)) 
        rc = 1;

    return (rc); 
    }
