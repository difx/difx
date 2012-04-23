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

#define MAX_ION_PTS 100

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
        fine_pts;
    double coarse_spacing,
           fine_spacing,
           step,
           bottom,
           center,
           valmax;
    struct data_corel *datum;
    complex *sbarray, *sbptr;
    extern int do_accounting;
    extern struct type_status status;
    double values[MAX_ION_PTS];


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
    if (param->ion_pts > MAX_ION_PTS)   // condition total # of points
        {
        param->ion_pts = MAX_ION_PTS;
        msg ("limited ion search to %d points", 2, param->ion_pts);
        }
    coarse_spacing = param->win_ion[1] - param->win_ion[0];
    if (param->ion_pts > 1)
        coarse_spacing /= param->ion_pts - 1;
    fine_spacing = 0.2 * coarse_spacing;
    fine_pts = 7;
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
                    if (values[k] > valmax)
                        {
                        valmax = values[k];
                        kmax = k;
                        }
                if (kmax == 0)          // coarse maximum up against lower edge?
                    center = bottom + (fine_pts - 1) / 2.0 * fine_spacing;
                else if (kmax == param->ion_pts) // upper edge?
                    center = bottom + (kmax - 1) * step 
                                    - (fine_pts - 1) / 2.0 * fine_spacing;
                else                    // max was one of the interior points
                    center = bottom + kmax * step;

                ilmax = fine_pts;
                step = fine_spacing;
                                        // make fine search symmetric about level 0 max
                bottom = center - (ilmax - 1) / 2.0 * step;
                break;
            case 2:                     // final evaluation
                                        // find maximum from fine search
                valmax = -1.0;
                for (k=0; k<ilmax; k++)
                    if (values[k] > valmax)
                        {
                        valmax = values[k];
                        kmax = k;
                        }
                                        // should do parabolic interpolation here
                center = bottom + kmax * step;
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
