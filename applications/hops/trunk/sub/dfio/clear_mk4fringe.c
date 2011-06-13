/************************************************************************/
/*                                                                      */
/* Resets a fringe structure to its initial state, freeing up any       */
/* previously allocated memory.                                         */
/*                                                                      */
/*      Inputs:         fringe           Target structure               */
/*                                                                      */
/*      Output:         fringe           in pristine state              */
/*                                                                      */
/* Created 2 September 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "mk4_data.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

void
clear_mk4fringe (struct mk4_fringe *fringe)
    {
    int i;
                                        /* Free all allocated memory */
    for (i=0; i<fringe->nalloc; i++)
        {
        msg ("Freeing memory block %d", -1, fringe->allocated[i]);
        free (fringe->allocated[i]);
        }
    fringe->nalloc = 0;
                                        /* Set all pointers to null */
    fringe->file_image = NULL;
    fringe->id = NULL;
    fringe->t200 = NULL;
    fringe->t201 = NULL;
    fringe->t202 = NULL;
    fringe->t203 = NULL;
    fringe->t204 = NULL;
    fringe->t205 = NULL;
    fringe->t206 = NULL;
    fringe->t207 = NULL;
    fringe->t208 = NULL;
    fringe->t210 = NULL;
    fringe->t220 = NULL;
    fringe->t221 = NULL;
    fringe->n212 = 0;
    fringe->n230 = 0;
    }
