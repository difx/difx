/************************************************************************/
/*                                                                      */
/* Initializes a data summary structure                                 */
/*                                                                      */
/*      Inputs:         summ            Structure to be cleared         */
/*                                                                      */
/*      Output:         summ            Cleared                         */
/*                                                                      */
/* Created 22 August 1994 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "summary.h"
#include "aedit.h"

void clear_summ (struct datasumm *summ)
    {
    int i;
    
    summ->stations[0] = '\0';
    summ->nbtq = 0;
    summ->btq[0] = '\0';
    summ->frequencies[0] = '\0';
    summ->polarizations[0] = '\0';
    summ->nsource = 0;
    for (i=0; i<MAXEXPTS; i++) summ->experiments[i] = 0;
    summ->nexp = 0;
    for (i=0; i<20; i++) summ->qcodes[i] = 0;
    summ->snrmin = 100000.0;
    summ->snrmax = 0.0;
    for (i=0; i<MAXSRC; i++)
        {
        summ->source[i].count = 0;
        summ->source[i].name[0] = '\0';
        }
    for (i=0; i<summ->nfqex; i++) clear_fqex (summ->fqex + i);
    summ->nfqex = 0;

    return;
    }
