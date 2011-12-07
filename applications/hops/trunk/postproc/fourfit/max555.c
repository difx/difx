// max555 - finds maximum of a gridded-function over a 5x5x5 cube
// 
// created - rjc 2009.10.20

#include <stdio.h>

void max555 (double drf[5][5][5],   // input: real function
             double xi[3],          // output: coordinates at maximum value
             double *drfmax)        // output: maximum value
    {
    int i,
        j,
        k,
        l;

    double dx,
           center[3],
           x[3],
           value,
           bestval,
           xbest[3];
  
                                    // initialize search to center of cube
    for (l=0; l<3; l++)
        center[l] = 0.0;
    dx = 0.2;

    do
        {
                                    // search over 11x11x11 cube for max
        bestval = 0.0;
        for (i=0; i<11; i++)
            for (j=0; j<11; j++)
                for (k=0; k<11; k++)
                    {
                    x[0] = center[0] + dx * (i-5);
                    x[1] = center[1] + dx * (j-5);
                    x[2] = center[2] + dx * (k-5);
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
        dx /= 5.0;
        msg ("max value %lf at %lf %lf %lf", 0, 
              bestval, xbest[0], xbest[1], xbest[2]);
        }
    
    while (dx > 0.0001);
                                    // return result to caller
    *drfmax = bestval;
    for (l=0; l<3; l++)
        xi[l] = xbest[l];
    }
