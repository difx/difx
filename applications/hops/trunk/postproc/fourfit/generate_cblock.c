/***********************************************************************
*                                                                      *
* For a single pass, generate_cblock creates a single c_block from     *
* the chained list of input c_blocks. It does so by figuring out which *
* of the blocks apply to the current pass, and overlaying them         *
* into a single c_block structure.                                     *
*                                                                      *
*                                                     RJC  92.8.25     *
* Modified for Mk4, CJL May 8 1998                                     *
*                                                                      *
***********************************************************************/
#include <stdio.h>
#include <string.h>
#include "ovex.h"
#include "control.h"
#include "pass_struct.h"
#include "param_struct.h"



int
generate_cblock (ovex, param, pass)
struct scan_struct *ovex;
struct type_param *param;
struct type_pass *pass;
    {
    extern struct c_block *cb_head;

    int time;
    char base[2],sour[32],group;
    struct c_block *cb_ptr;

                                        /* extract parameters of current pass */
    memcpy (base, param->baseline, 2);
    memcpy (sour,"                                ",32);
    memcpy (sour, ovex->src.source_name, strlen(ovex->src.source_name));

    group = pass->pass_data[0].fgroup;

    time = param->start_nom;


    nullify_cblock (&(pass->control));      /* create pass control block */
    default_cblock (&(pass->control));      /* with reasonable defaults    */


                                            /* Loop over all chained c_blocks */

    for (cb_ptr=cb_head; cb_ptr!=NULL; cb_ptr=cb_ptr->cb_chain)

                                   /* If this particular c_block's conditions 
                                        match those of the current pass, copy
                               all the non-null parameters into pass cblock */

        if (criteria_match (cb_ptr,base,sour,group,time))
            copy_cblock_parts (cb_ptr,&(pass->control));
    return(0);
    }
