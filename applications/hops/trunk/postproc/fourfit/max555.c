// max555 - finds maximum of a gridded-function over a 5x5x5 cube
// 
// created                                            - rjc 2009.10.20
// force interpolation to lie within specified bounds - rjc 2013.9.23

#include <stdio.h>

#define min(a,b) (a<b)?(a):(b)
#define max(a,b) (a<b)?(b):(a)

void max555 (double drf[5][5][5],   // input: real function
             double xlim[3][2],     // input: lower & upper bounds in 3 dimensions
             double xi[3],          // output: coordinates at maximum value
             double *drfmax)        // output: maximum value
    {
    int i,
        j,
        k,
        l;

    double dx0,
           dx1,
           dx2,
           center[3],
           x[3],
           x0_lower, x1_lower, x2_lower,
           x0_upper, x1_upper, x2_upper,
           value,
           bestval,
           xbest[3],
           epsilon = 0.0001;        // convergence criterion
  
                                    // initialize search to center of cube
    for (l=0; l<3; l++)
        center[l] = 0.0;
    dx0 = dx1 = dx2 = 0.4;

    do
        {
                                    // search over 11x11x11 cube for max
                                    // first compress search range to fit into bounds
        x0_lower = max (xlim[0][0], center[0] - 5 * dx0);
        x1_lower = max (xlim[1][0], center[1] - 5 * dx1);
        x2_lower = max (xlim[2][0], center[2] - 5 * dx2);

        x0_upper = min (xlim[0][1], center[0] + 5 * dx0);
        x1_upper = min (xlim[1][1], center[1] + 5 * dx1);
        x2_upper = min (xlim[2][1], center[2] + 5 * dx2);

        dx0 = (x0_upper - x0_lower) / 10.0;
        dx1 = (x1_upper - x1_lower) / 10.0;
        dx2 = (x2_upper - x2_lower) / 10.0;
        
        center[0] = (x0_lower + x0_upper) / 2.0;
        center[1] = (x1_lower + x1_upper) / 2.0;
        center[2] = (x2_lower + x2_upper) / 2.0;

        bestval = 0.0;
        for (i=0; i<11; i++)
            for (j=0; j<11; j++)
                for (k=0; k<11; k++)
                    {
                    x[0] = center[0] + dx0 * (i-5);
                    x[1] = center[1] + dx1 * (j-5);
                    x[2] = center[2] + dx2 * (k-5);
                                    // find interpolated value at this point
                    interp555 (drf, x, &value);
                    // msg ("i %d j %d k %d value %lf", 2, i, j, k, value);
                                    // is this a new maximum? 
                                    // if so, save value and coords.
                    if (value > bestval)
                        {
                        bestval = value;
                        for (l=0; l<3; l++)
                            xbest[l] = x[l];
                        }
                    }
                                    // relocate center and reduce grid size
        for (l=0; l<3; l++)
            center[l] = xbest[l];
        dx0 /= 5.0;
        dx1 /= 5.0;
        dx2 /= 5.0;
        msg ("max value %f at %g %g %g", 0, 
              bestval, xbest[0], xbest[1], xbest[2]);
        }
    
    while (dx0 > epsilon || dx1 > epsilon || dx2 > epsilon);
                                    // return result to caller
    *drfmax = bestval;
    for (l=0; l<3; l++)
        xi[l] = xbest[l];
    }
