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
/*                  coords          Array of coordinates                */
/*                  values          Array of values                     */
/*                  n               Number of array entries             */
/*                                                                      */
/*      Output:     result          Interpolated result                 */
/*                  return value    0=OK, 1=out of range, -1=error      */
/*                                                                      */
/* Created  14 March 2000 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "mk4_data.h"
#define MAXSTATAP 100

int
ap_mean (/* start, stop, coords, values, n, result) */
double start,
double stop,
double *coords,
double *values,
int n,
double *result)
    {
    double apcoord[MAXSTATAP], apval[MAXSTATAP], apsize, val,
           begin, end, x[MAXSTATPER+2], y[MAXSTATPER+2];
    int i, fst, np, ret, linterp(), nxy;

    apsize = stop - start;
    if (apsize <= 0.0)
        {
        msg ("Input error in ap_mean()", 2);
        return (-1);
        }

                                        /* extend data array beyond full interval
                                         * of first and last tabular points */
    for (i=0; i<n; i++)
        {
        x[i+1] = coords[i];
        y[i+1] = values[i];
        }

    if (n > 1)
        {
        x[0] = 2 * x[1] - x[2];
        y[0] = 2 * y[1] - y[2];
        x[n+1] = 2 * x[n] - x[n-1];
        y[n+1] = 2 * y[n] - y[n-1];
        }
    else                                /* if only one PC point, extrapolate
                                         * constant value.  rjc 2002.11.14 */
        {
        x[0] = x[1] - 10.0;
        y[0] = y[1];
        x[2] = x[1] + 10.0;
        y[2] = y[1];
        }
    nxy = n + 2;
                                        /* phase cal data really represents
                                         * interval from midway between the
                                         * two endpoints */
    begin = (x[0] + x[1]) / 2;
    end = (x[n] + x[n+1]) / 2;
    
                                        /* Hopelessly out of range */
                                        /* Set to zero to indicate missing data */
    if (begin > stop || end < start)
        {
        msg ("Out of range in ap_mean(), %.10g %.10g %.10g %.10g", -1,
                    start, stop, begin, end);
        *result = 0.0;
        return (1);
        }

                                        /* Set up first point */
    for (fst=0; fst<nxy; fst++) if (x[fst] >= start) break;
    apcoord[0] = 0.0;
                                        /* Start is before first coord, so just */
                                        /* use value of first array element */
    if (fst == 0) apval[0] = y[0];
                                        /* Linearly interpolate to find value */
                                        /* at precise start coordinate */
    else
        {
        ret = linterp (x[fst-1], y[fst-1], x[fst], y[fst], start, &val);
        if (ret == 0) apval[0] = val;
        else
            {
            msg ("Interpolation error in ap_mean()", 2);
            return (-1);
            }
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
            apval[np] = y[i];
            np++;
            }
        else break;
        }
                                        /* Get the last point */
    apcoord[np] = 1.0;
                                        /* Stop is after last coord, so just */
                                        /* use value of last array element */
    if (i == nxy) apval[np] = y[n-1];
                                        /* Linearly interpolate to find value */
                                        /* at precise stop coordinate */
    else
        {
        for (i=fst; i<nxy; i++) if (x[i] > stop) break;
        ret = linterp (x[i-1], y[i-1], x[i], y[i], stop, &val);
        if (ret == 0) apval[np] = val;
        else
            {
            msg ("Interpolation error in ap_mean()", 2);
            return (-1);
            }
        }
    np++;
                                        /* Perform the integration, pre-normalized */
    *result = 0.0;
    for (i=0; i<np-1; i++)
        *result += 0.5 * (apval[i] + apval[i+1]) * (apcoord[i+1] - apcoord[i]);
    return (0);
    }
