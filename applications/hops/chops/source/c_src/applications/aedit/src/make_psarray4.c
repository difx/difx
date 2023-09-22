/************************************************************************/
/*                                                                      */
/* Constructs a psplot_array                                            */
/* structure with all possible baseline/scan combinations represented.  */
/*                                                                      */
/*      Inputs:         psarray         Skeleton (unallocated) ps array */
/*                                                                      */
/*      Output:         psarray         Allocated, with axes defined    */
/*                                                                      */
/* Original 18 February 1993 by CJL                                     */
/* Adopted for Mk4 by CJL, 23 January 2001                              */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "psplot.h"
#include "aedit.h"

int make_psarray4 (struct ps_array *psarray)
    {
    int i, j, k, base, nscans, nstats, nbaselines;
    static int first = TRUE;
    extern int nscan;
                                        /* Figure out how much there is */
    nstats = strlen (psarray->stnlist);
    nbaselines = (nstats * (nstats - 1)) / 2;
    psarray->nbaselines = nbaselines;
    psarray->nscans = nscan;
                                        /* Some initialization to avoid screwups */
    for (i=0; i<17; i++) psarray->qtagged[i] = FALSE;
    if (first) 
        {
        first = FALSE;
        for (i=0; i<MAXBASE; i++) psarray->baseline[i].alloced = FALSE;
        }
    else ps_free (psarray);
                                        /* Allocate necessary memory */
    for (i=0; i<nbaselines; i++)
        {
        psarray->baseline[i].scan = 
                (struct psplot_cell *)calloc (nscan, sizeof (struct psplot_cell));
        if (psarray->baseline[i].scan == NULL)
            {
            msg ("Failed to allocate memory for psplot array", 2);
            ps_free (psarray);
            return (1);
            }
        else psarray->baseline[i].alloced = TRUE;
                                        /* Initialize data pointers to illegal */
        for (j=0; j<nscan; j++) 
            for (k=0; k<MAXBANDS; k++) psarray->baseline[i].scan[j].data_index[k] = -1;
        }
                                        /* Fill in baseline id's */
    base = 0;
    for (i=0; i<nstats-1; i++)
        for (j=i+1; j<nstats; j++)
            {
            if (base >= nbaselines)
                {
                msg ("This is a bug: too many baselines in make_psarray4", 2);
                return (1);
                }
            psarray->baseline[base].id[0] = psarray->stnlist[i];
            psarray->baseline[base].id[1] = psarray->stnlist[j];
            psarray->baseline[base].id[2] = '\0';
                                        /* Convenient to init here */
            for (k=0; k<MAXBANDS; k++)
                psarray->baseline[base].tagged[k] = FALSE;
            base++;
            }

    for (i=0; i<nscan; i++)
        {
                                        /* Convenient to init here */
        for (k=0; k<MAXBANDS; k++)
            psarray->time[i].tagged[k] = FALSE;
        }

    return (0);
    }
