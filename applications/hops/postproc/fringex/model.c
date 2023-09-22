// calculate phases from the genaroot quintic spline model
//
// first coded                        rjc  2007.10.5


#include "fringex.h"

void model (double epoch,           // i: epoch of evaluation in units of secs since 1980
            struct fxparam *fxp,    // i: pointer to main fringex data structure
            double *bl_phase)       // o: earth-centered genaroot model phase diff (rem-ref)

    {
    int i,
        n,
        npoly,
        nspl;

    double interval,
           delay[2],
           start,
           t;
          

                                    // calculate epoch for the start of the spline model
    start = time_to_int ((int)fxp->sdata[0]->t300->model_start.year,
                         (int)fxp->sdata[0]->t300->model_start.day, 
                         (int)fxp->sdata[0]->t300->model_start.hour,
                         (int)fxp->sdata[0]->t300->model_start.minute,
                         (int)fxp->sdata[0]->t300->model_start.second);
                                    // interval between polynomials
    interval = fxp->sdata[0]->t300->model_interval;
                                    // calculate index of which set of polynomials to use
    nspl = fxp->sdata[0]->t300->nsplines;
    npoly = (epoch - start) / interval;
                                    // fatal error if desired time not within the type 3 file
    if (npoly < 0 || npoly > nspl)  // warn & set to edge if out of range. rjc 2010.7.9
        {
        msg ("polynomial index %d out of type 3 range [0..%d], extrapolating.", 2, npoly, nspl);
        if (npoly < 0)
            npoly = 0;
        else
            npoly = nspl;
        }
                                    // calculate time within the npoly'th interval
    t = epoch - start - npoly * interval;

    for (i=0; i<2; i++)
        {
        delay[i] = 0.0;
        for (n=5; n>=0; n--)
            {
            delay[i] *= t;
            delay[i] += fxp->sdata[i]->model[0].t301[npoly]->delay_spline[n];
            }
        }
    *bl_phase = 3.6e8 * fxp->reffreq * (delay[1] - delay[0]);
    }

/*
 * eof
 */
