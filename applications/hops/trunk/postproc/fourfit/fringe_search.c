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
/* changed ionosphere search to be 3 level                2016.3.9  rjc */
/* refactored code to be 2 routines, this and ion_search  2016.5.25 rjc */
/* removed generate_cblock call, it's done in make_passes 2019.2.22 rjc */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "mk4_data.h"
#include "vex.h"
#include "pass_struct.h"
#include "param_struct.h"

int fringe_search ( struct vex* root, struct type_pass* pass)
    {
    int fr, ap, size, oret, rc;
    oret = 0;

    struct data_corel *datum;
    complex *sbarray, *sbptr;

    extern int do_accounting;
    extern struct type_status status;
    extern struct type_param param;

    msg  ("Baseline %c%c subgroup %c", 1, 
           param.baseline[0], param.baseline[1], pass->pass_data[0].fgroup);

                                        /* Currently does default filtering */
    if (apply_filter (pass) != 0)
        {
        msg ("Error filtering data", 2);
        return (-1);
        }
                                        /* Load in parameters needed for the */
                                        /* fringe search; do all precorrections */
    if (precorrect(root->ovex, pass) != 0)
        {
        msg ("Error precorrecting data", 2);
        return (-1);
        }
                                        /* Allocate memory for SBD functions */
                                        /* Should allocate only for unflagged */
                                        /* data points, but approach below will */
                                        /* in general not be too wasteful because */
                                        /* pass parameters reflect user restrictions */
                                        /* on data extent */
    size = 2 * param.nlags * pass->nfreq * pass->num_ap;

    sbarray = (complex *)calloc (size, sizeof (complex));
    if (sbarray == NULL)
        {
        msg ("Memory allocation failure (%d bytes with ap %d nlags %d nfreq %d) in fringe_search()",
              2, size * sizeof (complex), pass->num_ap, param.nlags, pass->nfreq);
        return (-1);
        }
    sbptr = sbarray;

    for (fr=0; fr<pass->nfreq; fr++)
        for (ap=0; ap<pass->num_ap; ap++)
            {
            datum = pass->pass_data[fr].data + ap + pass->ap_off;
            datum->sbdelay = sbptr;
            sbptr += 2*param.nlags;
            }
                                        // perform ionospheric search
    if (param.ion_smooth)
        {                               // fine search using smoothed coarse points
        rc = ion_search_smooth (pass);
        if (rc < 0)
            {
            msg ("Error return from ion_search_smooth", 2);
            free (sbarray);
            return (-1);
            }
        else if (rc > 0)                // non-fatal condition
            {
            free (sbarray);
            return (rc);
            }
        }
    else                                // 3-tiered non-smoothed ion search
        {
        rc = ion_search (pass);
        if (rc < 0)
            {
            free (sbarray);
            msg ("Error return from ion_search", 2);
            return (-1);
            }
        else if (rc > 0)                // non-fatal condition
            {
                free (sbarray);
                return (rc);
            }
        }

                                        /* Write the fringe file to disk, with */
                                        /* or without traditional fringe plot */
                                        /* attached, depending on control info */
                                        /* Also, update memory image of root file, */
                                        /* and display a fringe plot, if requested */
    #ifndef DISABLE_OUTPUT //allow for compiler flag to decouple search & plot libs
    oret = output (root, pass);
    #endif
    if (oret > 0)
        {
        msg ("Error writing results", 2);
        free (sbarray);
        return (-1);
        }
    else if (oret < 0)
        {
        msg ("User quit request", 1);
        free (sbarray);
        return (-2);
        }
                                        /* Free allocated memory */
    free (sbarray);

    return (0);
    }
