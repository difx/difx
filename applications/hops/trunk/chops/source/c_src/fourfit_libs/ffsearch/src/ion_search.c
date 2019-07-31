/************************************************************************/
/*                                                                      */
/* This routine performs a fringe search incorporating a loop over trial*/
/* values of the ionospheric differential TEC.                          */
/*                                                                      */
/* Split from fringe_search()                             2016.5.25 rjc */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "pass_struct.h"
#include "param_struct.h"
                                    // number of points in fine search
#define N_FINE_PTS 12
#define N_MED_PTS 12                // number of points in medium search
#define N_FINE_PTS_SMOOTH 24        // # of fine points with new smoothing algorithm

int ion_search (struct type_pass *pass)
    {
    int i,
        k,
        kmax,
        ilmax,
        level,
        ionloop,
        rc,
        koff,
        nip,
        win_sb_save[2],
        win_dr_save[2];

    double coarse_spacing,
           medium_spacing,
           fine_spacing,
           step,
           bottom,
           center,
           valmax,
           y[3],
           q[3],
           xmax,
           ampmax,
           xlo;

    extern int do_accounting;
    extern struct type_status status;
    extern struct type_param param;

    double values[MAX_ION_PTS];
    int parabola (double *, double, double, double *, double *, double *);
    void sort_tecs (void);


                                        // prepare for ionospheric search
    center = (param.win_ion[0] + param.win_ion[1]) / 2.0;
                                        // condition total # of points
    if (param.ion_pts > MAX_ION_PTS - N_MED_PTS - N_FINE_PTS - 1)   
        {
        param.ion_pts = MAX_ION_PTS - N_MED_PTS - N_FINE_PTS - 1;   
        msg ("limited ion search to %d points", 2, param.ion_pts);
        }
    coarse_spacing = param.win_ion[1] - param.win_ion[0];
    if (param.ion_pts > 1)
        {
        coarse_spacing /= param.ion_pts - 1;
        nip = 0;
        }

    medium_spacing = 2.0;
    fine_spacing = 0.4;
                                        // do search over ionosphere differential
                                        // TEC (if desired)
    for (level=0; level<4; level++)     // search level (coarse, medium, fine, final)
        {
        switch (level)
            {
            case 0:                     // set up for coarse ion search
                ilmax = param.ion_pts;
                step = coarse_spacing;
                bottom = center - (ilmax - 1) / 2.0 * step;
                if (param.ion_pts == 1)// if no ionospheric search, proceed
                    level = 3;          // immediately to final delay & rate search
                break;
            case 1:                     // set up for medium ion search 
                                        // find maximum from coarse search
                                        // should do parabolic interpolation here
                valmax = -1.0;
                for (k=0; k<ilmax; k++)
                    {
                    if (values[k] > valmax)
                        {
                        valmax = values[k];
                        kmax = k;
                        }
                                        // store this coarse ionosphere point
                    status.dtec[nip][0] = bottom + k * step;
                    status.dtec[nip++][1] = values[k];
                    }
                if (kmax == 0)          // coarse maximum up against lower edge?
                    center = bottom + (N_MED_PTS - 1) / 2.0 * medium_spacing;
                else if (kmax == param.ion_pts) // upper edge?
                    center = bottom + (kmax - 1) * step 
                                    - (N_MED_PTS - 1) / 2.0 * medium_spacing;
                else                    // max was one of the interior points
                    center = bottom + kmax * step;

                ilmax = N_MED_PTS;
                step = medium_spacing;
                                        // make medium search symmetric about level 0 max
                bottom = center - (ilmax - 1) / 2.0 * step;
                break;
            case 2:                     // set up for fine ion search 
                                        // find maximum from medium search
                                        // should do parabolic interpolation here
                valmax = -1.0;
                for (k=0; k<ilmax; k++)
                    {
                    if (values[k] > valmax)
                        {
                        valmax = values[k];
                        kmax = k;
                        }
                                        // store this medium ionosphere point
                    status.dtec[nip][0] = bottom + k * step;
                    status.dtec[nip++][1] = values[k];
                    }
                if (kmax == 0)          // medium maximum up against lower edge?
                    center = bottom + (N_FINE_PTS - 1) / 2.0 * fine_spacing;
                else if (kmax == param.ion_pts) // upper edge?
                    center = bottom + (kmax - 1) * step 
                                    - (N_FINE_PTS - 1) / 2.0 * fine_spacing;
                else                    // max was one of the interior points
                    center = bottom + kmax * step;

                ilmax = N_FINE_PTS;
                step = fine_spacing;
                                        // make fine search symmetric about level 0 max
                bottom = center - (ilmax - 1) / 2.0 * step;
                break;
            case 3:                     // final evaluation
                                        // find maximum from fine search
                valmax = -1.0;
                for (k=0; k<ilmax; k++)
                    {
                    if (values[k] > valmax)
                        {
                        valmax = values[k];
                        kmax = k;
                        }
                                        // store this fine ionosphere point
                    status.dtec[nip][0] = bottom + k * step;
                    status.dtec[nip++][1] = values[k];
                    }
                                        // should do parabolic interpolation here
                if (kmax == 0)
                    koff = +1;
                else if (kmax == ilmax - 1)
                    koff = -1;
                else
                    koff = 0;

                for (k=0; k<3; k++)
                    {
                    y[k] = values[kmax + k - 1 + koff];
                    xlo = bottom + (kmax - 1 + koff) * step;
                    }

                rc = parabola (y, -1.0, 1.0, &xmax, &ampmax, q);

                if (rc == 1)
                    msg ("TEC fine interpolation error; peak out of search range");
                else if (rc == 2)
                    msg ("TEC fine interpolation error; positive curvature");

                center = xlo + (xmax + 1.0) * step;

                bottom = center;
                ilmax = 1;
                step = 0.0;
                break;
            }
        for (ionloop=0; ionloop<ilmax; ionloop++)
            {
            status.loopion = ionloop;
                                        // offset ionosphere by search offset
            param.ion_diff = bottom + ionloop * step;

                                        // do 3-D grid search using FFT's
            rc = search(pass);
            if (rc < 0)
                {
                msg ("Error fringe searching", 2);
                return (-1);
                }
            else if (rc > 0)
                return (rc);

                                        // restore original window values for interpolation
            for (i=0; i<2; i++)
                {
                status.win_sb[i] = win_sb_save[i];
                status.win_dr[i] = win_dr_save[i];
                }
                                        // interpolate via direct counter-rotation for
                                        // more precise results
            interp (pass);
            if (do_accounting) 
                account ("Interpolate fringes");
            
                                        // save values for iterative search
            values[ionloop] = status.delres_max;
            msg ("ion search differential TEC %f amp %f",
                  1, param.ion_diff, status.delres_max);
            }
        }
                                        // save the final ion. point, if there is one
    if (param.ion_pts > 1)
        {
        status.dtec[nip][0] = center;
        status.dtec[nip++][1] = values[0];
        status.nion = nip;
        sort_tecs ();
        }
    else
        status.nion = 0;

    return (0);
    }




// experimental ion search, which performs a smoothing step of
// the coarse points, then goes immediately to a fine search
// around the maximum

int ion_search_smooth (struct type_pass *pass)
    {
    int i,
        k,
        kmax,
        ilmax,
        level,
        ionloop,
        rc,
        koff,
        nip,
        win_sb_save[2],
        win_dr_save[2];

    double coarse_spacing,
           fine_spacing,
           step,
           bottom,
           center,
           valmax,
           y[3],
           q[3],
           xmax,
           ampmax,
           xlo;

    extern int do_accounting;
    extern struct type_status status;
    extern struct type_param param;

    double values[MAX_ION_PTS];
    double smoothed_values[4*MAX_ION_PTS];
    int parabola (double *, double, double, double *, double *, double *);
    void sort_tecs (void);
    void smoother (double *, double *, double *, int *);


                                        // prepare for ionospheric search
    center = (param.win_ion[0] + param.win_ion[1]) / 2.0;
                                        // condition total # of points
    if (param.ion_pts > MAX_ION_PTS - N_FINE_PTS_SMOOTH - 1)   
        {
        param.ion_pts = MAX_ION_PTS - N_FINE_PTS_SMOOTH - 1;   
        msg ("limited ion search to %d points", 2, param.ion_pts);
        }
    coarse_spacing = param.win_ion[1] - param.win_ion[0];
    if (param.ion_pts > 1)
        {
        coarse_spacing /= param.ion_pts - 1;
        nip = 0;
        }

    fine_spacing = 0.4;
                                        // do search over ionosphere differential
                                        // TEC (if desired)
    for (level=0; level<3; level++)     // search level (coarse, fine, final)
        {
        switch (level)
            {
            case 0:                     // set up for coarse ion search
                ilmax = param.ion_pts;
                step = coarse_spacing;
                bottom = center - (ilmax - 1) / 2.0 * step;
                if (param.ion_pts == 1)// if no ionospheric search, proceed
                    level = 3;          // immediately to final delay & rate search
                break;

            case 1:                     // set up for fine ion search 
                                        // first, store the coarse ionosphere points
                for (k=0; k<ilmax; k++)
                    {
                    status.dtec[nip][0] = bottom + k * step;
                    status.dtec[nip++][1] = values[k];
                    msg("smoother input %d %f", -2, k, values[k]);
                    }
                                        // then smooth and interpolate coarse points
                smoother (values, smoothed_values, &step, &ilmax);
                for (k=0; k<ilmax; k++)
                    {
                    msg("smoother output %d %f", -2, k, smoothed_values[k]);
                    }
                                        // find maximum from smoothed coarse search
                valmax = -1.0;
                for (k=0; k<ilmax; k++)
                    {
                    if (smoothed_values[k] > valmax)
                        {
                        valmax = smoothed_values[k];
                        kmax = k;
                        }
                    }
                if (kmax == 0)          // coarse maximum up against lower edge?
                    center = bottom + (N_FINE_PTS_SMOOTH - 1) / 2.0 * fine_spacing;
                else if (kmax == param.ion_pts) // upper edge?
                    center = bottom + (kmax - 1) * step 
                                    - (N_FINE_PTS_SMOOTH - 1) / 2.0 * fine_spacing;
                else                    // max was one of the interior points
                    center = bottom + kmax * step;

                ilmax = N_FINE_PTS_SMOOTH;
                step = fine_spacing;
                                        // make fine search symmetric about level 0 max
                bottom = center - (ilmax - 1) / 2.0 * step;
                break;

            case 2:                     // final evaluation
                                        // find maximum from fine search
                valmax = -1.0;
                for (k=0; k<ilmax; k++)
                    {
                    if (values[k] > valmax)
                        {
                        valmax = values[k];
                        kmax = k;
                        }
                                        // store this fine ionosphere point
                    status.dtec[nip][0] = bottom + k * step;
                    status.dtec[nip++][1] = values[k];
                    }
                                        // should do parabolic interpolation here
                if (kmax == 0)
                    koff = +1;
                else if (kmax == ilmax - 1)
                    koff = -1;
                else
                    koff = 0;

                for (k=0; k<3; k++)
                    {
                    y[k] = values[kmax + k - 1 + koff];
                    xlo = bottom + (kmax - 1 + koff) * step;
                    }

                rc = parabola (y, -1.0, 1.0, &xmax, &ampmax, q);

                if (rc == 1)
                    msg ("TEC fine interpolation error; peak out of search range");
                else if (rc == 2)
                    msg ("TEC fine interpolation error; positive curvature");

                center = xlo + (xmax + 1.0) * step;

                bottom = center;
                ilmax = 1;
                step = 0.0;
                break;
            }
        for (ionloop=0; ionloop<ilmax; ionloop++)
            {
            status.loopion = ionloop;
                                        // offset ionosphere by search offset
            param.ion_diff = bottom + ionloop * step;

                                        // do 3-D grid search using FFT's
            rc = search(pass);
            if (rc < 0)
                {
                msg ("Error fringe searching", 2);
                return (-1);
                }
            else if (rc > 0)
                return (rc);

                                        // restore original window values for interpolation
            for (i=0; i<2; i++)
                {
                status.win_sb[i] = win_sb_save[i];
                status.win_dr[i] = win_dr_save[i];
                }
                                        // interpolate via direct counter-rotation for
                                        // more precise results
            interp (pass);
            if (do_accounting) 
                account ("Interpolate fringes");
            
                                        // save values for iterative search
            values[ionloop] = status.delres_max;
            msg ("ion search differential TEC %f amp %f",
                  1, param.ion_diff, status.delres_max);
            }
        }
                                        // save the final ion. point, if there is one
    if (param.ion_pts > 1)
        {
        status.dtec[nip][0] = center;
        status.dtec[nip++][1] = values[0];
        status.nion = nip;
        sort_tecs ();
        }
    else
        status.nion = 0;

    return (0);
    }

// sort tec array
void sort_tecs (void)
    {
    int i,
        n,
        changed = TRUE;

    double temp[2];

    extern struct type_status status;
  
    while (changed)
        {
        changed = FALSE;
        for (n=0; n<status.nion-1; n++)
            if (status.dtec[n][0] > status.dtec[n+1][0])
                {
                for (i=0; i<2; i++)
                    {
                    temp[i] = status.dtec[n][i];
                    status.dtec[n][i] = status.dtec[n+1][i];
                    status.dtec[n+1][i] = temp[i];
                    }
                changed = TRUE;
                }
        }
    }

// smooth an array of numbers and interpolate fourfold
// the algorithm takes the original data array f, inserts
// three 0-pts between each value, then does a convolution
// with a half-cycle of a cos curve, properly normalizing
// the result g

void smoother (double *f,           // input data array with arbitrary positive length
               double *g,           // output data array with fourfold interpolation
               double *tec_step,    // grid spacing of f in TEC units
               int *npts)           // pointer to length of input array - modified!
    {
    int i,
        j,
        k, kbeg, kend,
        n,
        nf,                         // # of input pts
        ng,                         // # of output pts
        ns;                         // # of smoothing curve pts
    double gwork[4*MAX_ION_PTS],
           shape[4*MAX_ION_PTS],
           ssum;
    
                                    // generate a smoothing curve. The shape of the idealized
                                    // curve for correlation as a function of TEC is dependent
                                    // on frequency distribution, but for a wide range of
                                    // reasonable 3-10 GHz distributions it has a half-width
                                    // of about 3 TEC units. Thus we use half a cosine curve,
                                    // having approximately that half power width.
    ns = 36 / *tec_step;
    ns |= 1;                        // make it odd, and ensure it isn't too large
    if (ns >= 4 * MAX_ION_PTS)
        ns = 4 * MAX_ION_PTS - 1;
    for (n=0; n<ns; n++)
        {
        shape[n] = cos (M_PI * (n - ns / 2) / ns);
        msg ("shape %d %f", -2, n, shape[n]);
        }
    *tec_step /= 4;                 // reduced step size for interpolation

    nf = *npts;
    ng = 4 * nf - 3;
    *npts = ng;                     // update caller's copy of length
                                    // form sparse g work array from f values
    for (i=0,j=0; j<ng; j++)
        {
        if (j % 4 == 0)
            gwork[j] = f[i++];
        else
            gwork[j] = 0.0;
        g[j] = 0;                   // also clear g for later use
        }

    for (j=0; j<ng; j++)            // convolution loop
        {
        kbeg = (ns - 1) / 2 - j;    // calculate part of shape function to convolve with
        if (kbeg < 0)
            kbeg = 0;
        kend = ng + (ns - 1) / 2 - j;
        if (kend > ns)
            kend = ns;

        ssum = 0;
        for (k=kbeg; k<kend; k++)
            {
            g[j] += gwork[j+k-(ns-1)/2] * shape[k];
                                    // sum used shape for normalization
            if (gwork[j+k-(ns-1)/2] != 0)
                ssum = ssum + shape[k];
            }
        if (ssum != 0)
            g[j] /= ssum;
        }
    }
