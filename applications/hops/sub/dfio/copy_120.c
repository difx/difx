/************************************************************************/
/*                                                                      */
/* This is the inverse of addr_120().  It takes an application          */
/* structure and a target address, and places an overlay structure      */
/* of the appropriate version into the target address.  Sometimes this  */
/* will require a field-by-field copying operation, sometimes it will   */
/* be a simple pointer assignment operation, depending on version       */
/* control status                                                       */
/*                                                                      */
/*      Inputs:         t120            application structure pointer   */
/*                                                                      */
/*      Output:         ptr             overlay structure address       */
/*                                      with data filled in             */
/*                      return value    number of bytes filled in       */
/*                                                                      */
/* Created 3 January 1997 by CJL                                        */
/* Modified 13 March 1998 by CJL                                        */
/* added type 5 (spectral)  rjc  2010.3.15                              */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bytflp.h"
#include "type_120.h"
#include "mk4_util.h"

int
copy_120 (struct type_120 *t120,
          char **ptr)
    {
    short nlags;
    int i, size, version, lag_len;
    char type;
    struct type_120_v0 *t120_v0;
    struct counts_per_lag *cpl, *cpl_v0;
    struct lag_tag *lags, *lags_v0;
    struct auto_per_lag *apl, *apl_v0;
    struct spectral *sp, *sp_v0;
                                        /* What version is requested for */
                                        /* the disk format? */
    sscanf (t120->version_no, "%2d", &version);
                                        /* Used for size calculations */
    type = t120->type;
    nlags = t120->nlags;
                                        /* Size of variable length array */
    if (type == COUNTS_PER_LAG)     lag_len = 16 * nlags;
    else if (type == COUNTS_GLOBAL) lag_len = 8 * nlags + 8;
    else if (type == AUTO_PER_LAG)  lag_len = 8 * nlags;
    else if (type == AUTO_GLOBAL)   lag_len = 4 * nlags + 4;
    else if (type == SPECTRAL)      lag_len = 8 * nlags;
                                        /* Disk format same as app struct, */
                                        /* simple pointer assignment. */
    if (version == T120_VERSION) *ptr = (char *)t120;
    else if (version == 0)
        {
        size = sizeof (struct type_120_v0) - sizeof (union lag_data) + lag_len;
        *ptr = (char *)malloc (size);
        if (*ptr == NULL)
            {
            msg ("Memory allocation failure in copy_120()", 2);
            return (-1);
            }
        }
    else
        {
        msg ("Unrecognized version number %d in copy_120()", 2, version);
        return (-1);
        }
                                        /* Handle each version number */
                                        /* individually. */
    if (version == 0)
        {
        t120_v0 = (struct type_120_v0 *) *ptr;
        strncpy (t120_v0->record_id, "120", 3);
        strncpy (t120_v0->version_no, "00", 2);
        t120_v0->type = t120->type;
        cp_short (t120_v0->nlags, t120->nlags);
        strncpy (t120_v0->baseline, t120->baseline, 2);
        strncpy (t120_v0->rootcode, t120->rootcode, 6);
        cp_int (t120_v0->index, t120->index);
        cp_int (t120_v0->ap, t120->ap);
        if (type == SPECTRAL)
            {
            cp_float (t120_v0->fw.weight, t120->fw.weight);
            }
        else
            {
            cp_int (t120_v0->fw.flag, t120->fw.flag);
            }

        cp_int (t120_v0->status, t120->status);
        cp_int (t120_v0->fr_delay, t120->fr_delay);
        cp_int (t120_v0->delay_rate, t120->delay_rate);
        for (i=0; i<nlags; i++)
            {
            switch (type)
                {
                case COUNTS_PER_LAG:
                    cpl = t120->ld.cpl + i;
                    cpl_v0 = t120_v0->ld.cpl + i;
                    cp_int (cpl_v0->coscor, cpl->coscor);
                    cp_int (cpl_v0->cosbits, cpl->cosbits);
                    cp_int (cpl_v0->sincor, cpl->sincor);
                    cp_int (cpl_v0->sinbits, cpl->sinbits);
                    break;

                case COUNTS_GLOBAL:
                    if (i == 0)
                        {
                        cp_int (t120_v0->ld.cg.cosbits, t120->ld.cg.cosbits);
                        cp_int (t120_v0->ld.cg.sinbits, t120->ld.cg.sinbits);
                        }
                    lags = t120->ld.cg.lags + i;
                    lags_v0 = t120_v0->ld.cg.lags + i;
                    cp_int (lags_v0->coscor, lags->coscor);
                    cp_int (lags_v0->sincor, lags->sincor);
                    break;

                case AUTO_PER_LAG:
                    apl = t120->ld.apl + i;
                    apl_v0 = t120_v0->ld.apl + i;
                    cp_int (apl_v0->coscor, apl->coscor);
                    cp_int (apl_v0->cosbits, apl->cosbits);
                    break;

                case AUTO_GLOBAL:
                    if (i == 0) cp_int (t120_v0->ld.ag.cosbits, t120->ld.ag.cosbits);
                    cp_int (t120_v0->ld.ag.coscor[i], t120->ld.ag.coscor[i]);
                    break;

                case SPECTRAL:    
                    sp = t120->ld.spec + i;
                    sp_v0 = t120_v0->ld.spec + i;
                    cp_float (sp_v0->re, sp->re);
                    cp_float (sp_v0->im, sp->im);
                    break;

                default:
                    msg ("Bad type in addr_120, %d", 2, type);
                    return (-1);
                }
            }
        size = sizeof (struct type_120_v0) - sizeof (union lag_data) + lag_len;
        return (size);
        }
    else
        {
        msg ("Unrecognized version number %d in copy_120()", 2, version);
        return (-1);
        }
    }
