/************************************************************************/
/*                                                                      */
/* Given a start and stop coordinate (for AP times here), and an        */
/* array of coordinates and values that span the AP, this routine does  */
/* an integration under the pointwise linear curve represented by the   */
/* array contents to compute an average value for the AP in question.   */
/* Utility routine for interpolating type-3 data onto accumulation pds  */
/*                                                                      */
/*      Inputs:     start           start coordinate of interval        */
/*                  stop            stop coordinate of interval         */
/*                  coords          Array of independent coordinates    */
/*                  val1            Array of values #1                  */
/*                  val2            Array of values #2                  */
/*                  n               Number of array entries             */
/*      I and O:    nstart          ptr to 1st value of array to examine*/
/*                                                                      */
/*      Output:     result1         Interpolated results                */
/*                  result2                                             */
/*                  return value    0=OK, 1=out of range, -1=error      */
/*                                                                      */
/* Created  14 March 2000 by CJL                                        */
/* Add nstart, and 2 dependent variables for efficiency  rjc  2011.11.9 */
/************************************************************************/
#include <stdio.h>
#include "mk4_data.h"
#define MAXSTATAP 100

int
ap_mean (/* start, stop, coords, values, n, result) */
double start,
double stop,
double *coords,
double *val1,  
double *val2,  
int n,
int *nstart,
double *result1,
double *result2)
    {
    int i, fst, np, ret, linterp();
    static int nxy;

    double apcoord[MAXSTATAP], apval1[MAXSTATAP], apval2[MAXSTATAP], val;
    static double apsize, begin, end, x[MAXSTATPER+2], y1[MAXSTATPER+2], y2[MAXSTATPER+2];

    apsize = stop - start;
    if (apsize <= 0.0)
        {
        msg ("Input error in ap_mean()", 2);
        return (-1);
        }

    if (*nstart == 0)
        {
                                        /* extend data array beyond full interval
                                         * of first and last tabular points */
        for (i=0; i<n; i++)
            {
            x[i+1] = coords[i];
            y1[i+1] = val1[i];
            y2[i+1] = val2[i];
            }

        if (n > 1)
            {
            x[0] = 2 * x[1] - x[2];
            y1[0] = 2 * y1[1] - y1[2];
            y2[0] = 2 * y2[1] - y2[2];
            x[n+1] = 2 * x[n] - x[n-1];
            y1[n+1] = 2 * y1[n] - y1[n-1];
            y2[n+1] = 2 * y2[n] - y2[n-1];
            }
        else                            /* if only one PC point, extrapolate
                                         * constant value.  rjc 2002.11.14 */
            {
            x[0] = x[1] - 10.0;
            y1[0] = y1[1];
            y2[0] = y2[1];
            x[2] = x[1] + 10.0;
            y1[2] = y1[1];
            y2[2] = y2[1];
            }
        nxy = n + 2;
                                        /* phase cal data really represents
                                         * interval from midway between the
                                         * two endpoints */
        begin = (x[0] + x[1]) / 2;
        end = (x[n] + x[n+1]) / 2;
        }
    
                                        /* Hopelessly out of range */
                                        /* Set to zero to indicate missing data */
    if (begin > stop || end < start)
        {
        msg ("Out of range in ap_mean(), %.10g %.10g %.10g %.10g", -1,
                    start, stop, begin, end);
        *result1 = 0.0;
        *result2 = 0.0;
        return (1);
        }

                                        /* Find and set up first point */
    for (fst=*nstart; fst<nxy; fst++) 
        if (x[fst] >= start) 
            break;
    apcoord[0] = 0.0;
                                        /* Start is before first coord, so just */
                                        /* use value of first array element */
    if (fst == 0) 
        {
        apval1[0] = y1[0];
        apval2[0] = y2[0];
        }
                                        /* Linearly interpolate to find value */
                                        /* at precise start coordinate */
    else
        {
        ret = linterp (x[fst-1], y1[fst-1], x[fst], y1[fst], start, &val);
        if (ret == 0)
            apval1[0] = val;
        else
            {
            msg ("Interpolation error in ap_mean()", 2);
            return (-1);
            }
        ret = linterp (x[fst-1], y2[fst-1], x[fst], y2[fst], start, &val);
        apval2[0] = val;
        }
                                        /* Get the points contained within */
                                        /* the start-stop interval */
    np = 1;
    for (i=fst+1; i<nxy; i++)
        {
                                        /* Coords must increase monotonically */
        if (x[i] <= x[i-1])
            {
            msg ("Mis-ordered or redundant coords: %13.3f %13.3f in ap_mean()", 
                 2, x[i-1], x[i]);
            return (-1);
            }
                                        /* Coords and values copied directly */
        if (x[i] < stop)
            {
            if (np >= MAXSTATAP)
                {
                msg ("Too many points per AP in ap_mean()", 2);
                return (-1);
                }
            apcoord[np] = (x[i] - start) / apsize;
            apval1[np] = y1[i];
            apval2[np] = y2[i];
            np++;
            }
        else 
            break;
        }
    *nstart = i - 1;                    // save starting point for next call
                                        /* Get the last point */
    apcoord[np] = 1.0;
                                        /* Stop is after last coord, so just */
                                        /* use value of last array element */
    if (i == nxy) 
        {
        apval1[np] = y1[n-1];
        apval2[np] = y2[n-1];
        }
                                        /* Linearly interpolate to find value */
                                        /* at precise stop coordinate */
    else
        {
        for (i=fst; i<nxy; i++) 
            if (x[i] > stop) 
                break;
        ret = linterp (x[i-1], y1[i-1], x[i], y1[i], stop, &val);
        if (ret == 0) 
            apval1[np] = val;
        else
            {
            msg ("Interpolation error in ap_mean()", 2);
            return (-1);
            }
        ret = linterp (x[i-1], y2[i-1], x[i], y2[i], stop, &val);
        apval2[np] = val;
        }
    np++;
                                        /* Perform the integration, pre-normalized */
    *result1 = 0.0;
    *result2 = 0.0;
    for (i=0; i<np-1; i++)
        {
        *result1 += 0.5 * (apval1[i] + apval1[i+1]) * (apcoord[i+1] - apcoord[i]);
        *result2 += 0.5 * (apval2[i] + apval2[i+1]) * (apcoord[i+1] - apcoord[i]);
        }
    return (0);
    }
