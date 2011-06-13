/************************************************************************/
/*                                                                      */
/* The job of this routine is to examine the contents of the specified  */
/* station def, and append all referenced low-level defs to the list    */
/* of defs maintained in deflist.                                       */
/*                                                                      */
/*      Inputs:         stname          Name of station def             */
/*                                                                      */
/*      Output:         deflist         array of low level def ids      */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created 31 December 1997 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int
get_station_deflist (char *stname,
                     struct def_list **deflist)
    {
    int st, bno, dno;
    struct ref ref;
    struct def *newdef, *stdef, *get_def();
    extern int defspace, ndefs;

                                        /* get the station def */
    if ((stdef = get_def (stname, "STATION", &bno, &dno)) == NULL) return (1);
                                        /* Loop over refs */
    for (st=stdef->start+1; st<stdef->end; st++)
        {
        if (parse_ref (st, &ref) != 0)
            {
            msg ("Error, bad 'ref' in $STATION def '%s'", 2, stname);
            return (-1);
            }
        newdef = get_def (ref.keyword, ref.blockname, &bno, &dno);
        if (newdef == NULL)
            {
            msg ("Failed to get def pointed to in $STATION, '%s, %s'", 2,
                        ref.blockname, ref.keyword);
            return (-1);
            }
        (*deflist)[ndefs].blockno = bno;
        (*deflist)[ndefs].defno = dno;
        
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
