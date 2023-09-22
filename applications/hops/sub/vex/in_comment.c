/************************************************************************/
/*                                                                      */
/* Basic utility to determine if a memory address is within a comment,  */
/* generally meaning it should be ignored.  The routine locate_cq()     */
/* must first have been called.                                         */
/*                                                                      */
/*      Inputs:         ptr             Memory location in vex file     */
/*                                                                      */
/*      Output:         return value    true or false                   */
/*                                                                      */
/* Created 21 August 1998 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "vex.h"
#include "mk4_vex.h"

#define TRUE 1
#define FALSE 0

int
in_comment (char *ptr)
    {
    int i;
    char *start, *end;
    extern int ncom;
    extern struct comment clist[];

    for (i=0; i<ncom; i++)
        {
        start = clist[i].start;
        end = clist[i].end;
        if ((ptr >= start) && ((ptr <= end) || (end == NULL))) return (TRUE);
        }

    return (FALSE);
    }
