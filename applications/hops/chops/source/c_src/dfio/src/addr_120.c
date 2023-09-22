/************************************************************************/
/*                                                                      */
/* Standard record version control.  This routine returns the address   */
/* of a structure containing the desired record information.  This can  */
/* either be the address of the raw memory image of the disk record     */
/* that was read in, or a memory-allocated structure filled in element  */
/* by element, depending on whether or not the disk format and the      */
/* structure definitions match.                                         */
/*                                                                      */
/*      Inputs:         version         Version number of disk image    */
/*                      address         Memory address of disk image    */
/*                      size            True size of structure (bytes)  */
/*                                                                      */
/*      Output:         Return value    Address of filled app structure */
/*                                                                      */
/* Created 3 January 1997 by CJL                                        */
/* Revised 13 March 1998 by CJL                                         */
/* added type 5 (spectral)  rjc  2010.3.15                              */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "bytflp.h"
#include "type_120.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

struct type_120 *
addr_120 (short version,
          void *address,
          int *size)
    {
    int malloced, i, size_120, lag_len;
    short *dummy, nlags;
    char type, *caddress;
    struct type_120 *t120;
    struct type_120_v0 *t120_v0;
    struct counts_per_lag *cpl, *cpl_v0;
    struct lag_tag *lags, *lags_v0;
    struct auto_per_lag *apl, *apl_v0;
    struct spectral *sp, *sp_v0;
                                        /* Need type and number of lags */
                                        /* up front */
    caddress = (char *)address;
    type = caddress[5];
    dummy = (short *)address;
    cp_short (nlags, dummy[3]);
                                        /* Size of variable length array */
    if (type == COUNTS_PER_LAG)     lag_len = 16 * nlags;
    else if (type == COUNTS_GLOBAL) lag_len = 8 * nlags + 8;
    else if (type == AUTO_PER_LAG)  lag_len = 8 * nlags;
    else if (type == AUTO_GLOBAL)   lag_len = 4 * nlags + 4;
    else if (type == SPECTRAL)      lag_len = 8 * nlags;
                                        /* Create application structure, which */
                                        /* might be simply the overlay structure */
    malloced = FALSE;
    if (version == T120_VERSION) t120 = (struct type_120 *)address;
    else
        {
        size_120 = sizeof (struct type_120) - sizeof (union lag_data) + lag_len;
        t120 = (struct type_120 *) malloc (size_120);
        if (t120 == NULL)
            {
            msg ("Memory allocation failure in addr_120()", 2);
            return (NULL);
            }
        clear_120 (t120);
        malloced = TRUE;
        }
                                        /* Handle each version number */
                                        /* individually.  First overlay */
                                        /* a version-specific structure, */
                                        /* then copy structure elements */
    if (version == 0)
        {
                                        /* Must calculate true structure size */
                                        /* since sizeof() doesn't know we are */
                                        /* tricking compiler with variable numbers */
                                        /* of raw data blocks */
        *size = sizeof (struct type_120_v0) - sizeof (union lag_data) + lag_len;
        t120_v0 = (struct type_120_v0 *)address;
                                        /* Start copying structure elements, */
                                        /* with hidden byte flipping if needed */
                                        /* (see bytflp.h) */
        strncpy (t120->record_id, "120", 3);
        strncpy (t120->version_no, "00", 2);
        t120->type = t120_v0->type;
        cp_short (t120->nlags, t120_v0->nlags);
        if (t120 != t120_v0)            // don't do copy onto self
            {
            strncpy (t120->baseline, t120_v0->baseline, 2);
            strncpy (t120->rootcode, t120_v0->rootcode, 6);
            }
        cp_int (t120->index, t120_v0->index);
        cp_int (t120->ap, t120_v0->ap);
        if (type == SPECTRAL)
            {
            cp_float (t120->fw.weight, t120_v0->fw.weight);
            }
        else
            {
            cp_int (t120->fw.flag, t120_v0->fw.flag);
            }
        cp_int (t120->status, t120_v0->status);
        cp_int (t120->fr_delay, t120_v0->fr_delay);
        cp_int (t120->delay_rate, t120_v0->delay_rate);
        for (i=0; i<nlags; i++)
            {
            switch (type)
                {
                case COUNTS_PER_LAG:
                    cpl = t120->ld.cpl + i;
                    cpl_v0 = t120_v0->ld.cpl + i;
                    cp_int (cpl->coscor, cpl_v0->coscor);
                    cp_int (cpl->cosbits, cpl_v0->cosbits);
                    cp_int (cpl->sincor, cpl_v0->sincor);
                    cp_int (cpl->sinbits, cpl_v0->sinbits);
                    break;

                case COUNTS_GLOBAL:
                    if (i == 0)
                        {
                        cp_int (t120->ld.cg.cosbits, t120_v0->ld.cg.cosbits);
                        cp_int (t120->ld.cg.sinbits, t120_v0->ld.cg.sinbits);
                        }
                    lags = t120->ld.cg.lags + i;
                    lags_v0 = t120_v0->ld.cg.lags + i;
                    cp_int (lags->coscor, lags_v0->coscor);
                    cp_int (lags->sincor, lags_v0->sincor);
                    break;

                case AUTO_PER_LAG:
                    apl = t120->ld.apl + i;
                    apl_v0 = t120_v0->ld.apl + i;
                    cp_int (apl->coscor, apl_v0->coscor);
                    cp_int (apl->cosbits, apl_v0->cosbits);
                    break;

                case AUTO_GLOBAL:
                    if (i == 0) cp_int (t120->ld.ag.cosbits, t120_v0->ld.ag.cosbits);
                    cp_int (t120->ld.ag.coscor[i], t120->ld.ag.coscor[i]);
                    break;

                case SPECTRAL:    
                    sp = t120->ld.spec + i;
                    sp_v0 = t120_v0->ld.spec + i;
                    cp_float (sp->re, sp_v0->re);
                    cp_float (sp->im, sp_v0->im);
                    break;

                default:
                    msg ("Bad type in addr_120, %d", 2, type);
                    return (NULL);
                }
            }
        return (t120);
        }
    else
        {
        msg ("Unrecognized type 120 record version number %d", 2, version);
        if (malloced) free (t120);
        return (NULL);
        }
    }
