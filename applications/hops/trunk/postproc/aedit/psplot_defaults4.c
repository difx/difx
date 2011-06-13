/************************************************************************/
/*                                                                      */
/* There are two special colour indices for minus'ed out scan/baselines */
/* and unprocessed scan/baselines.  These scan/baselines are            */
/* identifiable from the vex file, and will not appear in the main      */
/* data array, so we assign these colour values first, before looking   */
/* at the data.  Anything labelled with an "unprocessed" colour code    */
/* will be overwritten later on by real data.                           */
/*                                                                      */
/*      Inputs:         psarray         allocated but empty ps array    */
/*                                                                      */
/*      Output:         psarray         filled with default colours     */
/*                                                                      */
/* Original 18 February 1993 by CJL                                     */
/* Adapted to Mk4 by CJL, Jan 23 2001                                   */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "psplot.h"

int
psplot_defaults4 (psarray)
struct ps_array *psarray;
    {
    int i, j, k, m, base, nst, code, found, band, nbf, nbands;
    char stn1, stn2;
    char fg, *id;
    struct psplot_scantime *psscan;
                                        /* Loop through schedule, filling */
                                        /* with minused or unprocessed codes */
    for (i=0; i<psarray->nscans; i++)
        {
        psscan = psarray->time + i;
        nst = psscan->nst;
                                        /* Loop over baselines for this scan */
        for (j=0; j<nst-1; j++)
            for (k=j+1; k<nst; k++)
                {
                                        /* Get stations for this baseline */
                stn1 = psscan->stations[j].stn;
                stn2 = psscan->stations[k].stn;
                                        /* Set default colour code by checking */
                                        /* for minus'ed station */
                if ((psscan->stations[j].minus) || 
                            (psscan->stations[k].minus)) code = MINUS;
                else code = UNPROC;
                                        /* Find baseline in psarray */
                for (base=0; base < psarray->nbaselines; base++)
                    {
                    id = psarray->baseline[base].id;
                    if ((stn1 != id[0]) && (stn1 != id[1])) continue;
                    if ((stn2 != id[0]) && (stn2 != id[1])) continue;
                                        /* Got it.  Fill in all subgroups that both */
                                        /* stations were supposed to record */
                    found = TRUE;
                    for (m=0; m<MAXBANDS; m++)
                        {
                        fg = psarray->subgroups[m];
                                        /* End of global subgroup list */
                        if (fg == '\0') break;
                                        /* Not found for one of the stations */
                        if (strchr (psscan->stations[j].fglist, fg) == NULL) continue;
                        if (strchr (psscan->stations[k].fglist, fg) == NULL) continue;
                                        /* Found, fill in the default code */
                        psarray->baseline[base].scan[i].colour_index[m] = code;
                        }
                    }
                if (! found) msg ("Error: baseline %c%c not found!", 2, stn1, stn2);
                }
        }

    return (0);
    }
