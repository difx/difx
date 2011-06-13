/************************************************************************/
/*                                                                      */
/* This routine is responsible for filling in a SU_chan_out array.      */
/* It is passed the name of a $SU_CHAN_OUT def and a pointer to the     */
/* svex structure, and fills in the array.                              */
/*                                                                      */
/*      Inputs:         defname         Name of $SU_CHAN_OUT def        */
/*                      svex            Pointer to main svex struct     */
/*                                                                      */
/*      Output:         svex            SU_chan_out array filled in     */
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
get_su_chan_out (char *defname,
                 struct svex_struct *svex)
    {
    int i, st, su_out;
    char *str;
    struct def *scodef;
    struct block *blk;
    struct SU_chan_out_struct *sco;
    struct param_val p_val;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern int svex_ver, svex_del_list[];
                                        /* Initialize */
    for (i=0; i<16; i++)
        {
        svex->SU_chan_out[i].chan_ID[0] = '\0';
        svex->SU_chan_out[i].freq_a = -1.0;
        svex->SU_chan_out[i].freq_b = -1.0;
        svex->SU_chan_out[i].freq_c = -1.0;
        svex->SU_chan_out[i].freq_d = -1.0;
        }
                                        /* Locate $SU_CHAN_OUT block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "SU_CHAN_OUT") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $SU_CHAN_OUT block", 3);
        return (-1);
        }
    blk = blist + i;
                                        /* Find def name */
    for (i=0; i<blk->ndef; i++)
        if (strcmp (blk->deflist[i].name, defname) == 0) break;
    if (i == blk->ndef)
        {
        msg ("Could not find def '%s' in $SU_CHAN_OUT block", 3, defname);
        return (-1);
        }
    scodef = blk->deflist + i;
                                        /* Undelete the def */
    for (st=scodef->start; st<=scodef->end; st++) svex_del_list[st] = FALSE;
                                        /* Loop over statements in sucdef */
    for (st=scodef->start+1; st<scodef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse parameter= statement */
        if (parse_pval (str, "SU_CHAN_OUT", SVEX | svex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (-1);
            }
        if ISNAME ("channel_out_assignment")
            {
            su_out = p_val.dval[0].data.intval;
            sco = svex->SU_chan_out + su_out;
            strcpy (sco->chan_ID, p_val.dval[1].data.strval);
            if (p_val.nval >= 3) sco->freq_a = p_val.dval[2].data.realval;
            if (p_val.nval >= 4) sco->freq_b = p_val.dval[3].data.realval;
            if (p_val.nval >= 5) sco->freq_c = p_val.dval[4].data.realval;
            if (p_val.nval >= 6) sco->freq_d = p_val.dval[5].data.realval;
            }
        }

    return (0);
    }
