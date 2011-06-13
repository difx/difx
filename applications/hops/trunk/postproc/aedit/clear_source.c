/************************************************************************/
/*                                                                      */
/* This simply initializes the source summary structure pointed to by   */
/* the argument.  This is necessary, because these structures are       */
/* created using realloc(), which does not zero the returned memory     */
/* block.                                                               */
/*                                                                      */
/*      Inputs: sptr            Pointer to srcsum summary structure     */
/*                                                                      */
/*      Output: None                                                    */
/*                                                                      */
/* Created 5 February 1992 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "summary.h"

void
clear_source (sptr)
srcsum *sptr;
    {
    int i;

    sptr->name[0] = '\0';
    sptr->count = 0;
    sptr->begin = 0;
    sptr->end = 0;
    for (i=0; i<=MAXSTEXP; i++) sptr->stations[i] = '\0';
    sptr->nbtq = 0;
    if (sptr->btq_allocated)
        {
        free (sptr->btq);
        sptr->btq_allocated = FALSE;
        }
    for (i=0; i<20; i++) sptr->qcodes[i] = 0;
    sptr->snrmin = 1000000.0;
    sptr->snrmax = 0.0;
    }
