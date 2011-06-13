/************************************************************************/
/*                                                                      */
/* This routine is responsible for filling in a TRM_config structure.   */
/* It is passed the name of a $TRM_CONFIG def and a pointer to the      */
/* empty structure, and fills in the structure.                         */
/*                                                                      */
/*      Inputs:         defname         Name of $TRM_CONFIG def         */
/*                      trm_config      Pointer to TRM_config_struct    */
/*                                                                      */
/*      Output:         trm_config      Filled in                       */
/*                      return value    0=OK, else bad.                 */
/*                                                                      */
/* Created 2 February 1999 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

int
get_trm_config (char *defname,
                struct TRM_config_struct *trm_config)
    {
    int i, st, mux, bps;
    char *str;
    struct def *trmdef;
    struct block *blk;
    struct param_val p_val;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern int ivex_ver, ivex_del_list[];
                                        /* Initialize */
    for (i=0; i<8; i++) trm_config->mux4_2bit[i] = TRM_ABSENT;
    for (i=0; i<4; i++) trm_config->mux4_1bit[i] = TRM_ABSENT;
    for (i=0; i<4; i++) trm_config->mux2_2bit[i] = TRM_ABSENT;
    for (i=0; i<2; i++) trm_config->mux2_1bit[i] = TRM_ABSENT;
    for (i=0; i<2; i++) trm_config->mux1_2bit[i] = TRM_ABSENT;
    trm_config->mux1_1bit = TRM_ABSENT;
    trm_config->parity_error_limit = 0.0;
    trm_config->invalid_frame_control.CRC_error = FALSE;
    trm_config->invalid_frame_control.PE_limit_exceeded = FALSE;
    trm_config->invalid_frame_control.lost_sync = FALSE;
    trm_config->invalid_frame_control.unexpected_sync = FALSE;
                                        /* Locate $TRM_CONFIG block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "TRM_CONFIG") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $TRM_CONFIG block", 3);
        return (-1);
        }
    blk = blist + i;
                                        /* Find def name */
    for (i=0; i<blk->ndef; i++)
        if (strcmp (blk->deflist[i].name, defname) == 0) break;
    if (i == blk->ndef)
        {
        msg ("Could not find def '%s' in $TRM_CONFIG block", 3, defname);
        return (-1);
        }
    trmdef = blk->deflist + i;
                                        /* Undelete the def */
    for (st=trmdef->start; st<=trmdef->end; st++) ivex_del_list[st] = FALSE;
                                        /* Loop over statements in trmdef */
    for (st=trmdef->start+1; st<trmdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Must be parameter= statement */
        if (parse_pval (str, "TRM_CONFIG", IVEX | ivex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (-1);
            }
        if ISNAME ("TRM_order_within_channel")
            {
            mux = p_val.dval[0].data.intval;
            bps = p_val.dval[1].data.intval;
            if (p_val.nval != mux * bps + 2)
                {
                msg ("Badly formed statement '%s'", 2, str);
                return (-1);
                }
            if ((mux == 4) && (bps == 2))
                {
                for (i=0; i<8; i++)
                    trm_config->mux4_2bit[i] = 
                        bitstream (p_val.dval[i+2].data.strval);
                }
            else if ((mux == 4) && (bps == 1))
                {
                for (i=0; i<4; i++)
                    trm_config->mux4_1bit[i] = 
                        bitstream (p_val.dval[i+2].data.strval);
                }
            else if ((mux == 2) && (bps == 2))
                {
                for (i=0; i<4; i++)
                    trm_config->mux2_2bit[i] = 
                        bitstream (p_val.dval[i+2].data.strval);
                }
            else if ((mux == 2) && (bps == 1))
                {
                for (i=0; i<2; i++)
                    trm_config->mux2_1bit[i] = 
                        bitstream (p_val.dval[i+2].data.strval);
                }
            else if ((mux == 1) && (bps == 2))
                {
                for (i=0; i<2; i++)
                    trm_config->mux1_2bit[i] = 
                        bitstream (p_val.dval[i+2].data.strval);
                }
            else if ((mux == 1) && (bps == 1))
                {
                trm_config->mux1_1bit = 
                        bitstream (p_val.dval[2].data.strval);
                }
            }
        else if ISNAME ("parity_error_limit")
            trm_config->parity_error_limit = p_val.dval[0].data.realval;
        else if ISNAME ("invalid_frame_control")
            {
            if (strcmp (p_val.dval[0].data.strval, "on") == 0)
                trm_config->invalid_frame_control.CRC_error = TRUE;
            else trm_config->invalid_frame_control.CRC_error = FALSE;

            if (strcmp (p_val.dval[1].data.strval, "on") == 0)
                trm_config->invalid_frame_control.PE_limit_exceeded = TRUE;
            else trm_config->invalid_frame_control.PE_limit_exceeded = FALSE;

            if (strcmp (p_val.dval[2].data.strval, "on") == 0)
                trm_config->invalid_frame_control.lost_sync = TRUE;
            else trm_config->invalid_frame_control.lost_sync = FALSE;

            if (strcmp (p_val.dval[3].data.strval, "on") == 0)
                trm_config->invalid_frame_control.unexpected_sync = TRUE;
            else trm_config->invalid_frame_control.unexpected_sync = FALSE;
            }
        }

    return (0);
    }

int
bitstream (char *stream)
    {
    if (strcmp (stream, "sign0") == 0) return TRM_SIGN0;
    if (strcmp (stream, "sign1") == 0) return TRM_SIGN1;
    if (strcmp (stream, "sign2") == 0) return TRM_SIGN2;
    if (strcmp (stream, "sign3") == 0) return TRM_SIGN3;
    if (strcmp (stream, "mag0") == 0) return TRM_MAG0;
    if (strcmp (stream, "mag1") == 0) return TRM_MAG1;
    if (strcmp (stream, "mag2") == 0) return TRM_MAG2;
    if (strcmp (stream, "mag3") == 0) return TRM_MAG3;
    }
