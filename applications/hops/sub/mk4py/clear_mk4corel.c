/************************************************************************/
/*                                                                      */
/* Resets a corel structure to its initial state, freeing up any        */
/* previously allocated memory.                                         */
/*                                                                      */
/*      Inputs:         corel           Target structure                */
/*                                                                      */
/*      Output:         corel           in pristine state               */
/*                                                                      */
/* Created 3 January 1997 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "mk4_data.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

void
clear_mk4corel (struct mk4_corel *corel)
    {
    int i;
    static int first = TRUE;
                                        /* Free all allocated memory */
    for (i=0; i<corel->nalloc; i++)
        {
        free (corel->allocated[i]);
        msg ("Calling free() on pointer %p", 0, corel->allocated[i]);
        }
    corel->nalloc = 0;
                                        /* Space for 101, 120 pointers */
    if (first) first = FALSE;
    else
        {
        for (i=0; i<corel->index_space; i++)
            {
            if (corel->index[i].t120 != NULL) 
                {
                free (corel->index[i].t120);
                }
            }
        free (corel->index);
        }
                                        /* Set all pointers to null */
    corel->file_image = NULL;
    corel->id = NULL;
    corel->t100 = NULL;
    corel->index_space = 0;
                                        /* No index space left, so no need */
                                        /* to clear it. */
/*    for (i=0; i<MAXIND; i++)
        {
        corel->index[i].t101 = NULL;
        corel->index[i].ap_space = 0;
        } */
    }
