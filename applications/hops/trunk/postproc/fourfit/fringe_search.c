/************************************************************************/
/*                                                                      */
/* This routine performs a complete fringe search for a single pass     */
/* through the data file (there may be many passes, with differing      */
/* parameters or looking at different subsets of data).  The argument   */
/* is a pointer to a structure that contains a time/frequency array,    */
/* and several pass-specific parameters.  Parameters generic to this    */
/* program invokation/data file are maintained in the external          */
/* structure param.                                                     */
/*                                                                      */
/* Created April 13 1992 by CJL                                         */
/* new code to search for maximum over ionospheric values 2012.1.27 rjc */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "mk4_data.h"
#include "vex.h"
#include "pass_struct.h"
#include "param_struct.h"
                                    // number of points in fine search
#define N_FINE_PTS 9

int
fringe_search (root, param, pass)
struct vex *root;
struct type_param *param;
struct type_pass *pass;
    {
    int i, fr, ap, size, oret, 
        k,
        kmax,
        ilmax,
        level,
        ionloop,
        rc,
        koff,
        nip;
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
    struct data_corel *datum;
    complex *sbarray, *sbptr;
    extern int do_accounting;
    extern struct type_status status;
    double values[MAX_ION_PTS];
    int parabola (double *, double, double, double *, double *, double *);
    void sort_tecs (void);


    msg  ("Baseline %c%c subgroup %c", 1, 
           param->baseline[0], param->baseline[1], pass->pass_data[0].fgroup);

                                        /* Form control block for this pass */
    if (generate_cblock (root->ovex, param, pass) != 0)     
        {
        msg ("Error generating pass block",2);
        return (1);
        }
                                        /* Currently does default filtering */
    if (apply_filter (param, pass) != 0)
        {
        msg ("Error filtering data", 2);
        return (1);
        }
                                        /* Load in parameters needed for the */
                                        /* fringe search; do all precorrections */
    if (precorrect(root->ovex, param, pass) != 0)
        {
        msg ("Error precorrecting data", 2);
        return (1);
        }
                                        /* Allocate memory for SBD functions */
                                        /* Should allocate only for unflagged */
                                        /* data points, but approach below will */
                                        /* in general not be too wasteful because */
                                        /* pass parameters reflect user restrictions */
                                        /* on data extent */
    size = 2 * param->nlags * pass->nfreq * pass->num_ap;
    sbarray = (complex *)calloc (size, sizeof (complex));
    if (sbarray == NULL)
        {
        msg ("Memory allocation failure (%d bytes with ap %d nlags %d nfreq %d) in fringe_search()",
              2, size * sizeof (complex), pass->num_ap, param->nlags, pass->nfreq);
        return (-1);
        }
    sbptr = sbarray;
    for (fr=0; fr<pass->nfreq; fr++)
        for (ap=0; ap<pass->num_ap; ap++)
            {
            datum = pass->pass_data[fr].data + ap + pass->ap_off;
            datum->sbdelay = sbptr;
            sbptr += 2*param->nlags;
            }
                                        // prepare for ionospheric search
    center = (param->win_ion[0] + param->win_ion[1]) / 2.0;
                                        // condition total # of points
    if (param->ion_pts > MAX_ION_PTS - N_FINE_PTS - 1)   
        {
        param->ion_pts = MAX_ION_PTS - N_FINE_PTS - 1;   
        msg ("limited ion search to %d points", 2, param->ion_pts);
        }
    coarse_spacing = param->win_ion[1] - param->win_ion[0];
    if (param->ion_pts > 1)
        {
        coarse_spacing /= param->ion_pts - 1;
        nip = 0;
        }

    fine_spacing = 0.2 * coarse_spacing;
                                        // do search over ionosphere differential
                                        // TEC (if desired)
    for (level=0; level<3; level++)     // search level (coarse, fine, final)
        {
        switch (level)
            {
            case 0:                     // set up for coarse ion search
                ilmax = param->ion_pts;
                step = coarse_spacing;
                bottom = center - (ilmax - 1) / 2.0 * step;
                if (param->ion_pts == 1)// if no ionospheric search, proceed
                    level = 2;          // immediately to final delay & rate search
                break;
            case 1:                     // set up for fine ion search 
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
                    center = bottom + (N_FINE_PTS - 1) / 2.0 * fine_spacing;
                else if (kmax == param->ion_pts) // upper edge?
                    center = bottom + (kmax - 1) * step 
                                    - (N_FINE_PTS - 1) / 2.0 * fine_spacing;
                else                    // max was one of the interior points
                    center = bottom + kmax * step;

                ilmax = N_FINE_PTS;
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
                                        // offset ionosphere by search offset
            param->ion_diff = bottom + ionloop * step;

                                        /* Crunch numbers. */
            if (search(pass) != 0)
                {
                msg ("Error fringe searching", 2);
                return (1);
                }
                                        /* Correct for various effects here, */
                                        /* which typically depend on where */
                                        /* and what strength fringes were found */
                                        /* A stubbed routine for now */
            if (postcorrect(pass) != 0)
                {
                msg ("Error postcorrecting data", 2);
                return (1);
                }
                                        // save values for iterative search
            values[ionloop] = status.delres_max;
            msg ("ion search differential TEC %f amp %f",
                  1, param->ion_diff, status.delres_max);
            }
        }
                                        // save the final ion. point, if there is one
    if (param->ion_pts > 1)
        {
        status.dtec[nip][0] = center;
        status.dtec[nip++][1] = values[0];
        status.nion = nip;
        sort_tecs ();
        }
    else
        status.nion = 0;

                                        /* Write the fringe file to disk, with */
                                        /* or without traditional fringe plot */
                                        /* attached, depending on control info */
                                        /* Also, update memory image of root file, */
                                        /* and display a fringe plot, if requested */
    oret = output (root, pass);
    if (oret > 0)
        {
        msg ("Error writing results", 2);
        return (1);
        }
    else if (oret < 0) return (-1);
                                        /* Free allocated memory */
    free (sbarray);

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
