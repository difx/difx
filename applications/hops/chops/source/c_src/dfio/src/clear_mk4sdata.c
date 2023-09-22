/************************************************************************/
/*                                                                      */
/* Resets a sdata structure to its initial state, freeing up any        */
/* previously allocated memory.                                         */
/*                                                                      */
/*      Inputs:         sdata           Target structure                */
/*                                                                      */
/*      Output:         sdata           in pristine state               */
/*                                                                      */
/* Created 11 March 1998 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "mk4_data.h"
#include "mk4_sizes.h"
#include "mk4_dfio.h"

void
clear_mk4sdata (struct mk4_sdata *sdata)
    {
    int i, j;
                                        /* Free all allocated memory */
    for (i=0; i<sdata->nalloc; i++)
        free (sdata->allocated[i]);
    sdata->nalloc = 0;
                                        /* Set all pointers to null */
    sdata->file_image = NULL;
    sdata->id = NULL;
    sdata->t300 = NULL;
    for (i=0; i<MAXFREQ; i++)
        {
        sdata->model[i].chan_id[0] = '\0';
        for (j=0; j<MAXSPLINES; j++)
            {
            sdata->model[i].t301[j] = NULL;
            sdata->model[i].t302[j] = NULL;
            sdata->model[i].t303[j] = NULL;
            }
        }
    sdata->n304 = 0;
    sdata->n305 = 0;
    sdata->n306 = 0;
    sdata->n307 = 0;
    sdata->n308 = 0;
    sdata->n309 = 0;
    for (i=0; i<MAXSTATPER; i++)
        {
        sdata->t304[i] = NULL;
        sdata->t305[i] = NULL;
        sdata->t306[i] = NULL;
        sdata->t307[i] = NULL;
        sdata->t308[i] = NULL;
        sdata->t309[i] = NULL;
        }
    }
