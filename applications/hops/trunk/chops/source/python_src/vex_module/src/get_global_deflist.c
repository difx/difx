/************************************************************************/
/*                                                                      */
/* The job of this routine is to examine the contents of the GLOBAL     */
/* section and come up with a list of low level defs identified by      */
/* primitive section and def name.                                      */
/*                                                                      */
/*      Inputs:         vex             pointer to main vex structure   */
/*                                                                      */
/*      Output:         deflist         array of low level def ids      */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created 31 December 1997 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int
get_global_deflist (struct def_list **deflist)
    {
    int i, j, st;
    struct ref ref;
    extern int defspace, ndefs, nblock;
    extern struct block blist[];
    struct block *globalblock;
                                        /* Make some initial space */
    if (defspace == 0)
        {
        defspace = 1000;
        *deflist = (struct def_list *)calloc 
                        (defspace, sizeof (struct def_list));
        }
    if (*deflist == NULL)
        {
        msg ("Error allocating space for list of defs", 2);
        return (1);
        }
                                /* Locate GLOBAL section */
    for (i=0; i<nblock; i++) if (strcmp (blist[i].name, "GLOBAL") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $GLOBAL block", 2);
        return (1);
        }
    else globalblock = blist + i;
                                        /* Find the refs, and defs pointed to */
    for (st = globalblock->stno + 1; st <= globalblock->end; st++)
        {
        if (parse_ref (st, &ref) != 0)
            {
            msg ("Corrupt $GLOBAL section", 2);
            return (-1);
            }
                                        /* Locate the primitive block */
        for (i=0; i<nblock; i++) 
            if (strcmp (blist[i].name, ref.blockname) == 0) break;
        if (i == nblock)
            {
            msg ("ref to non-existent '$%s' block in $GLOBAL", 2, ref.blockname);
            return (-1);
            }
                                        /* Now locate the def */
        for (j=0; j<blist[i].ndef; j++)
            if (strcmp (blist[i].deflist[j].name, ref.keyword) == 0) break;
        if (j == blist[i].ndef)
            {
            msg ("ref to non-existent def '%s' in $GLOBAL", 2, ref.keyword);
            return (-1);
            }
                                        /* Store the def away */
        (*deflist)[ndefs].blockno = i;
        (*deflist)[ndefs].defno = j;
        ndefs++;
        if (ndefs >= defspace)
            {
            defspace += 1000;
            *deflist = (struct def_list *)realloc 
                                (*deflist, defspace * sizeof (struct def_list));
            if (*deflist == NULL)
                {
                msg ("Error expanding space for list of defs", 2);
                return (1);
                }
            }
        }

    return (0);
    }
