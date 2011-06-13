/************************************************************************/
/*                                                                      */
/* Given the name of a $CORR_BLOCK_MODE def, this routine returns a     */
/* pointer to a filled in structure representing the contents of the    */
/* def.  It seeks to be efficient by recognizing a def previously       */
/* parsed.  It must not overwrite other defs returned in previous calls */
/*                                                                      */
/*      Inputs:         block_mode      Name of def                     */
/*                                                                      */
/*      Output:         return value    Cvex_BlockMode struct pointer   */
/*                                                                      */
/* Created 16 November 1998 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

#define MAX_BLOCK_MODES 20

struct Cvex_BlockMode *
get_block_mode (char *block_mode)
    {
    static struct Cvex_BlockMode bmode[MAX_BLOCK_MODES];
    static int nbmode = 0;
    int i, st;
    char *str;
    struct def *mdef;
    struct block *blk;
    struct Cvex_BlockMode *bm;
    struct param_val p_val;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern int cvex_ver, cvex_del_list[];
                                        /* Check for special init mode */
    if (strcmp (block_mode, ":INIT:") == 0)
        {
        nbmode = 0;
        return (NULL);
        }
                                        /* Already parsed? just return it */
    for (i=0; i<nbmode; i++)
        if (strcmp (bmode[i].defId, block_mode) == 0) return (bmode + i);
                                        /* OK, gotta do the work */
                                        /* Have we exceeded capacity? */
    if (nbmode >= MAX_BLOCK_MODES)
        {
        msg ("Too many defs referenced in $CORR_BLOCK_MODE", 2);
        return (NULL);
        }
                                        /* Point to next bmode struct */
    bm = bmode + nbmode;
                                        /* Locate CORR_BLOCK_MODE block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "CORR_BLOCK_MODE") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $CORR_BLOCK_MODE block", 3);
        return (NULL);
        }
    blk = blist + i;
                                        /* Find def name */
    for (i=0; i<blk->ndef; i++)
        if (strcmp (blk->deflist[i].name, block_mode) == 0) break;
    if (i == blk->ndef)
        {
        msg ("Could not find mode '%s' in $CORR_BLOCK_MODE block", 3, block_mode);
        return (NULL);
        }
    mdef = blk->deflist + i;
                                        /* Undelete mdef */
    for (st=mdef->start; st<=mdef->end; st++)
        cvex_del_list[st] = FALSE;
                                        /* Loop over statements in mdef */
    for (st=mdef->start+1; st<mdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse "parameter=" statement */
        if (parse_pval (str, "CORR_BLOCK_MODE", CVEX | cvex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (NULL);
            }

        if ISNAME ("rotator_mode")
            {
            bm->lCellRotrMode = p_val.dval[0].data.intval;
            bm->rCellRotrMode = p_val.dval[1].data.intval;
            }

        else if ISNAME ("xdelay")
            {
            bm->lCellXDly = p_val.dval[0].data.intval;
            bm->rCellXDly = p_val.dval[1].data.intval;
            }

        else if ISNAME ("ydelay")
            {
            bm->lCellYDly = p_val.dval[0].data.intval;
            bm->rCellYDly = p_val.dval[1].data.intval;
            }

        else if ISNAME ("tap_motion_enable")
            bm->enableTap = p_val.dval[0].data.intval;

        else if ISNAME ("invalid_on_tap_motion")
            bm->invalidateOnTapMove = p_val.dval[0].data.intval;

        else if ISNAME ("header_mode")
            bm->headerMode = p_val.dval[0].data.intval;
        }

    if (strlen (block_mode) < MAX_NAMESIZE) strcpy (bm->defId, block_mode);
    else
        {
        msg ("Block mode name '%s' too long", 2, block_mode);
        return (NULL);
        }

    nbmode++;
    return (bm);
    }
