/************************************************************************/
/*                                                                      */
/* This routine takes the indices of two data entries, and returns      */
/* TRUE if they are duplicate processings, FALSE if they are not        */
/* No checks for common sense are made (i,j >0, < fscan, i=j etc)       */
/*                                                                      */
/*      Inputs:         i, j            The 2 indices                   */
/*                                                                      */
/*      Output:         return value    TRUE for match                  */
/*                                                                      */
/* Created 11 April 1989 by CJL                                         */
/* return false if scan names are different  rjc  2009.8.28             */
/************************************************************************/
#include "aedata.h"

#define TRUE 1
#define FALSE 0

int
dup_check (fdata,i,j)
fringearray *fdata;
int i, j;
    {
    fringesum *datum1, *datum2;
    char revbase[3];
                                        /* For code clarity */
    datum1 = &(fdata[i].data);
    datum2 = &(fdata[j].data);
    revbase[0] = datum1->baseline[1];
    revbase[1] = datum1->baseline[0];
    revbase[2] = '\0';
                        /* Check ones most likely to mismatch first for speed */

        if ((strcmp(datum1->baseline, datum2->baseline) != 0) &&
                                (strcmp(revbase, datum2->baseline) != 0))
            return(FALSE);
        if (datum1->time_tag != datum2->time_tag) return(FALSE);
        if (datum1->freq_code != datum2->freq_code) return(FALSE); /* This checks types */
        if (datum1->expt_no != datum2->expt_no) return(FALSE);
        if (strcmp(datum1->source, datum2->source) != 0) return(FALSE);
        if (datum1->mode != datum2->mode) return(FALSE);
        if (strcmp(datum1->polarization, datum2->polarization) != 0) return(FALSE);
                                    // also diffentiate by scan names                 
        if (strcmp(datum1->scan_id, datum2->scan_id) != 0)
            return(FALSE);

        return (TRUE);  /* Ok, it matches everything important */
    }
