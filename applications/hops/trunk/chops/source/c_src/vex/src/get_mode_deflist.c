/************************************************************************/
/*                                                                      */
/* The job of this routine is to examine the contents of the specified  */
/* mode def, and append all referenced low-level defs to the list       */
/* of defs maintained in deflist.                                       */
/*                                                                      */
/*      Inputs:         modedef         Mode def                        */
/*                      stname          Name of station qualifier       */
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
get_mode_deflist (struct def *modedef,
                  char *stname,
                  struct def_list **deflist)
    {
    int i, m, blockno, defno, found;
    struct ref ref;
    struct def *newdef, *get_def();
    extern int defspace, ndefs;

                                        /* Loop over refs */
    for (m=modedef->start+1; m<modedef->end; m++)
        {
        if (parse_ref (m, &ref) != 0)
            {
            msg ("Error, bad 'ref' in $MODE def '%s'", 2, modedef->name);
            return (-1);
            }
                                        /* If qualifiers exist, requested */
                                        /* station must be one of them in */
                                        /* order for this ref to count */
        if (ref.nargs > 1)
            {
            found = FALSE;
            for (i=1; i<ref.nargs; i++)
                {
                if (strcmp (ref.args[i], stname) == 0) found = TRUE;
                }
            if (! found) continue;
            }
                                        /* We want this one, get it and */
                                        /* add to the list */
        newdef = get_def (ref.keyword, ref.blockname, &blockno, &defno);
        if (newdef == NULL)
            {
            msg ("Failed to get def pointed to in $MODE, '%s, %s'", 2,
                        ref.blockname, ref.keyword);
            return (-1);
            }
        (*deflist)[ndefs].blockno = blockno;
        (*deflist)[ndefs].defno = defno;
        
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
