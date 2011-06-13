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
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "mk4_data.h"
#include "vex.h"
#include "pass_struct.h"
#include "param_struct.h"

int
fringe_search (root, param, pass)
struct vex *root;
struct type_param *param;
struct type_pass *pass;
    {
    int i, fr, ap, size, oret;
    struct data_corel *datum;
    complex *sbarray, *sbptr;
    extern int do_accounting;

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
                                        /* Load in parameters needed for the */
                                        /* fringe search; do all precorrections */
    if (precorrect(root->ovex, param, pass) != 0)
        {
        msg ("Error precorrecting data", 2);
        return (1);
        }
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
    if (do_accounting) account ("Write output files/plots");
                                        /* Free allocated memory */
    free (sbarray);

    return (0);
    }
