/************************************************************************/
/*                                                                      */
/* Trivial routine to clear the input codata array                      */
/*                                                                      */
/*      Inputs:         codata          undefined state                 */
/*      Output:         codata          initialized                     */
/*                                                                      */
/* Created October 5, 1995 by CJL                                       */
/* Hacked up April 26 2026 by GBC                                       */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "cohfit.h"

// honestly, a memset would be more efficient here
void clear_codata(cosumary *codata)
{
    int ii;
    for (ii = 0; ii < MAX_BNO; ii++)
        if (codata[ii].exampat && codata[ii].examlen)
            free(codata[ii].exampat);
    memset(codata, 0, sizeof(cosumary));
}

#if 0
void old_clear_codata (cosumary *codata)
    {
    int i, j;

    for (i=0; i<MAX_BNO; i++)
        {
        codata[i].datum = NULL;
        codata[i].plateau = 0.0;
        codata[i].breakpoint = 0.0;
        codata[i].slope = 0.0;
        for (j=0; j<MAX_NSEGLEN; j++)
            {
            codata[i].ampl[j] = 0.0;
            codata[i].snr[j] = 0.0;
            codata[i].fitsnr[j] = 0.0;
            codata[i].seglen[j] = 0.0;
            codata[i].navg[j] = 0;
            }
        if (codata[i].exampat && codata[i].examlen)
            free(codata[i].exampat);
        codata[i].exampat = NULL;
        codata[i].examlen = 0;
        }
    return;
    }
#endif /* 0 */

/*
 * eof vim:nospell
 */
