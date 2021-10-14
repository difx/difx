/************************************************************************/
/*                                                                      */
/* Part of the fringe data summarization functions.  This routine takes */
/* a pointer to a fringe data point and updates the internal list of    */
/* statistics for each frequency/experiment (used in plotting)          */
/*                                                                      */
/*      Inputs:         datum                                           */
/*                                                                      */
/*      Output:         summ            updated                         */
/*                                                                      */
/* Created ?? by CJL                                                    */
/* Cleaned up October 18 1993 by CJL                                    */
/* Generalized for closure support, August 23 1994 by CJL               */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "aedata.h"
#include "summary.h"
#include "aedit.h"

int update_fqex (void *datum, struct datasumm *summ, int type)
    {
    int i, j, k, l, fqindex, time_tag, expt_no, btqlen;
    char c, freq_code, quality, btq[5], source[32];
    char *btqptr;
    float snr;
    struct frqexp *fqptr;
    srcsum *sptr;
    static char *qstring = "ABCDEFGH0123456789";
    fringesum *fdatum;
    trianglesum *tdatum;
    quadsum *qdatum;
                                        /* Set pointers and variables appropriately */
    switch (type)
        {
        case BASELINE:
            fdatum = (fringesum *)datum;
            freq_code = fdatum->freq_code;
            expt_no = fdatum->expt_no;
            time_tag = fdatum->time_tag;
            strcpy (btq, fdatum->baseline);
            strcpy (source, fdatum->source);
            quality = mk3_qf (fdatum);
            snr = fdatum->snr;
            btqlen = 3;
            break;

        case TRIANGLE:
            tdatum = (trianglesum *)datum;
            freq_code = tdatum->freq_code;
            expt_no = tdatum->expt_no;
            time_tag = tdatum->time_tag;
            strcpy (btq, tdatum->triangle);
            strcpy (source, tdatum->source);
            quality = tdatum->scan_quality;
            snr = tdatum->bis_snr;
            btqlen = 4;
            break;

        case QUAD:
            qdatum = (quadsum *)datum;
            freq_code = qdatum->freq_code;
            expt_no = qdatum->expt_no;
            time_tag = qdatum->time_tag;
            strcpy (btq, qdatum->quad);
            strcpy (source, qdatum->source);
            quality = qdatum->quality;
            snr = 0.0;
            btqlen = 5;
            break;

        default:
            msg ("Invalid type passed to update_fqex()", 2);
            return (1);
        }
                                        /* Figure out which frequency/experiment */
                                        /* and identify fqex structure ... start */
                                        /* new one if necessary */
    for (fqindex=0; fqindex<summ->nfqex; fqindex++)
        {
        if ((summ->fqex[fqindex].freq_code == freq_code)
                         && (summ->fqex[fqindex].expt_no == expt_no)) break;
        }

    fqptr = summ->fqex + fqindex;               /* Convenience pointer */

    if (fqindex == summ->nfqex) /* New freq/expt, create new entry */
        {
        summ->nfqex++;
        if (summ->nfqex > MAXBANDS*MAXEXPTS)
            {
            msg ("Overflow condition in fqex summary structure ... abort!",3);
            return (-1);
            }
        fqptr->freq_code = freq_code;
        fqptr->expt_no = expt_no;
        fqptr->nsource = 0;
                                        /* Initialize */
        fqptr->begin = fqptr->end = time_tag;
        for (i=0; i<=MAXSTEXP; i++) fqptr->stations[i] = '\0';
        alloc_btq (&(fqptr->btq), &(fqptr->btq_allocated), type);
        strcpy (fqptr->btq, btq);
        fqptr->nbtq = 1;
        }
                                        /* Update variables in fqex itself */
    if (time_tag < fqptr->begin) fqptr->begin = time_tag;
    if (time_tag > fqptr->end) fqptr->end = time_tag;
                                        /* Check stations */
    i = 0;
    while ((c = btq[i++]) != 0)
        if (strchr (fqptr->stations, c) == NULL) 
            {
            l = strlen (fqptr->stations);
            fqptr->stations[l] = c;
            fqptr->stations[l+1] = '\0';
            }
                                        /* Check btqs, stored as series of null */
                                        /* terminated strings */
    for (i=0; i<fqptr->nbtq; i++)
        if (strcmp (fqptr->btq + i*btqlen, btq) == 0) break;
    if(i == fqptr->nbtq)
        {
        btqptr = fqptr->btq + fqptr->nbtq*btqlen;
        strcpy (btqptr, btq);
        fqptr->nbtq++;
        }
                                        /* Now move to slist entries */
                                        /* Check sources for local fqex structure */
    for(j=0;j<fqptr->nsource;j++)
        {
        if (strcmp(fqptr->slist[j].name, source) == 0)
            {
            fqptr->slist[j].count++;
            break;
            }
        }
                                        /* New source in this fqex */
    if (j == fqptr->nsource)
        {
                                        /* First allocate necessary memory */
        if (j == 0) 
            {
            if (fqptr->slist_allocated)
                {
                msg ("Allocation problems in update_fqex()",2);
                return (-1);
                }
            fqptr->slist = (srcsum *) calloc(MAXSRC, sizeof(srcsum));
            if (fqptr->slist == NULL)
                {
                msg ("Memory allocation failure in update_fqex()", 2);
                return (-1);
                }
            else fqptr->slist_allocated = TRUE;
            }

        fqptr->slist[j].btq_allocated = FALSE;
        clear_source (&(fqptr->slist[j]));
        alloc_btq (&(fqptr->slist[j].btq), &(fqptr->slist[j].btq_allocated), type);
        fqptr->nsource++;               /* set up new slist entry */
        strcpy (fqptr->slist[j].name, source);
        fqptr->slist[j].count = 1;
        }

    sptr = &(fqptr->slist[j]);  /* Convenience pointer */

                                        /* Check times.  Initialize if 1st source */
    if(sptr->count == 1) sptr->begin = sptr->end = time_tag;
    if(time_tag < sptr->begin) sptr->begin = time_tag;
    if(time_tag > sptr->end) sptr->end = time_tag;
                                        /* Check stations */
    i = 0;
    while ((c = btq[i++]) != 0)
        if (strchr (sptr->stations, c) == NULL) 
            {
            l = strlen (sptr->stations);
            sptr->stations[l] = c;
            sptr->stations[l+1] = '\0';
            }
                                        /* Check btqs, stored as series of null */
                                        /* terminated strings */
    for (i=0; i<sptr->nbtq; i++)
        if (strcmp (sptr->btq + i*btqlen, btq) == 0) break;
    if(i < sptr->nbtq)
        {
        btqptr = sptr->btq + sptr->nbtq*btqlen;
        strcpy (btqptr, btq);
        sptr->nbtq++;
        }
                                        /* Accumulate quality codes */
    k = 0;
    while (qstring[k])
        {
        if (qstring[k] == quality) break;
        k++;
        }
    sptr->qcodes[k]++;
                                        /* Check SNR limits */
    if (snr > sptr->snrmax) sptr->snrmax = snr;
    if (snr < sptr->snrmin) sptr->snrmin = snr;

    return(0);
    }
