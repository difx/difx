/************************************************************************/
/*                                                                      */
/* This routine takes a root and baseline specification, plus a list    */
/* of baselines and subgroups assembled by refringe_list(), and figures */
/* out whether this baseline needs fourfitting.  If so, it returns the  */
/* subset of frequency subgroups which need doing.                      */
/*                                                                      */
/* Note that files_index enters from the root file loop counter in      */
/* fourfit.c.  The root file array is filled by refringe_list(), which  */
/* places that same index in base_sgrp.                                 */
/*                                                                      */
/*      Inputs:         rbase           Specifies the baseline we are on*/
/*                      files_index     root file id, also in next arg  */
/*                      base_sgrp       array of baseline/subgroups     */
/*                                                                      */
/*      Output:         return value    NULL means no data requested    */
/*                                      if needed, a string composed    */
/*                                      of the requested subgroups      */
/*                                      is returned.                    */
/*                                                                      */
/* Created 20 January 1994 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include "mk4_data.h"
#include "refringe.h"

char *
check_rflist (baseline, files_index, base_sgrp)
char *baseline;
int files_index;
bsgstruct *base_sgrp;
    {
    int sgl_index, i, len;
    static char sglist[5];
    bsgstruct *bsg;
    sglist[0] = '\0';
                                        /* Look at all entries, skip those that */
                                        /* don't match root file, baseline, or that */
                                        /* have already been used in a previous call */
    sglist[0] = '\0';
    i = 0;
    while (base_sgrp[i].files_index != END_OF_LIST)
        {
        bsg = base_sgrp + i;
        i++;
        if (bsg->files_index == ALREADY_USED) continue;
        if (bsg->files_index != files_index) continue;
        if (strncmp (bsg->baseline, baseline, 2) != 0) continue;
                                        /* We have a match ... add it to sglist if */
                                        /* space, and if not already there */
        bsg->files_index = ALREADY_USED;
        if ((len = strlen (sglist)) == 4) continue;
        if (strchr (sglist, bsg->subgroup) != NULL) continue;
        sglist[len] = bsg->subgroup;
        sglist[len+1] = '\0';
        }
                                        /* Empty string means no data, so skip */
    if (strlen (sglist) == 0) return (NULL);
    else return (sglist);
    }

