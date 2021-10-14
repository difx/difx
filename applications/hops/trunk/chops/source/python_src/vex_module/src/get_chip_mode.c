/************************************************************************/
/*                                                                      */
/* Given the name of a $CORR_CHIP_MODE def, this routine returns a      */
/* pointer to a filled in structure representing the contents of the    */
/* def.  It seeks to be efficient by recognizing a def previously       */
/* parsed.  It must not overwrite other defs returned in previous calls */
/*                                                                      */
/*      Inputs:         chip_mode       Name of def                     */
/*                                                                      */
/*      Output:         return value    Cvex_SectMode struct pointer    */
/*                                                                      */
/* Created 14 November 1998 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

#define MAX_CHIP_MODES 20

struct Cvex_ChipMode *
get_chip_mode (char *chip_mode)
    {
    static struct Cvex_ChipMode cmode[MAX_CHIP_MODES];
    static int ncmode = 0;
    int i, nchip_blk, nsnake, st;
    char *str, *block_mode;
    struct def *mdef;
    struct block *blk;
    struct Cvex_Block *block_struct;
    struct Cvex_Snake *snake_struct;
    struct Cvex_ChipMode *cm;
    struct Cvex_BlockMode *get_block_mode();
    struct param_val p_val;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern int cvex_ver, cvex_del_list[];
                                        /* Check for special init mode */
    if (strcmp (chip_mode, ":INIT:") == 0)
        {
        ncmode = 0;
        get_block_mode (":INIT:");
        return (NULL);
        }
                                        /* Already parsed? just return it */
    for (i=0; i<ncmode; i++)
        if (strcmp (cmode[i].defId, chip_mode) == 0) return (cmode + i);
                                        /* OK, gotta do the work */
                                        /* Have we exceeded capacity? */
    if (ncmode >= MAX_CHIP_MODES)
        {
        msg ("Too many defs referenced in $CORR_CHIP_MODE", 2);
        return (NULL);
        }
                                        /* Point to next cmode struct */
    cm = cmode + ncmode;
                                        /* Locate CORR_CHIP_MODE block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "CORR_CHIP_MODE") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $CORR_CHIP_MODE block", 3);
        return (NULL);
        }
    blk = blist + i;
                                        /* Find def name */
    for (i=0; i<blk->ndef; i++)
        if (strcmp (blk->deflist[i].name, chip_mode) == 0) break;
    if (i == blk->ndef)
        {
        msg ("Could not find mode '%s' in $CORR_CHIP_MODE block", 3, chip_mode);
        return (NULL);
        }
    mdef = blk->deflist + i;
                                        /* Undelete mdef */
    for (st=mdef->start; st<=mdef->end; st++)
        cvex_del_list[st] = FALSE;
                                        /* Loop over statements in mdef */
    nchip_blk = nsnake = 0;
    for (st=mdef->start+1; st<mdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse "parameter=" statement */
        if (parse_pval (str, "CORR_CHIP_MODE", CVEX | cvex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (NULL);
            }

        if ISNAME ("block")
            {
            if (nchip_blk >= MAX_BLKS_P_CHIP)
                {
                msg ("Maximum number of 'block =' statements exceeded", 2);
                print_location (st);
                return (NULL);
                }
            block_struct = cm->block + nchip_blk;
            block_mode = p_val.dval[0].data.strval;
            strcpy (block_struct->blockId, p_val.dval[1].data.strval);
            block_struct->mux[0] = p_val.dval[2].data.intval;
            block_struct->mux[1] = p_val.dval[3].data.intval;
            block_struct->mux[2] = p_val.dval[4].data.intval;
            block_struct->mux[3] = p_val.dval[5].data.intval;
                                        /* Make pointer to block mode */
            block_struct->mode = get_block_mode (block_mode);
            if (block_struct->mode == NULL)
                {
                msg ("Failure getting block mode struct '%s'", 2, block_mode);
                print_location (st);
                return (NULL);
                }
            nchip_blk++;
            }

        else if ISNAME ("snake")
            {
            if (nsnake >= MAX_SNAKES_P_CHIP)
                {
                msg ("Maximum number of 'snake =' statements exceeded", 2);
                print_location (st);
                return (NULL);
                }
            snake_struct = cm->snake + nsnake;
            strcpy (snake_struct->type, p_val.dval[0].data.strval);
            strcpy (snake_struct->refId, p_val.dval[1].data.strval);
            strcpy (snake_struct->remId, p_val.dval[2].data.strval);
            if (p_val.nval > 13)
                {
                msg ("Too many value fields in 'snake =' statement", 2);
                print_location (st);
                return (NULL);
                }
            for (i=3; i<p_val.nval; i++)
                strcpy (snake_struct->path[i-3], p_val.dval[i].data.strval);
            snake_struct->lenOfSnakePath = p_val.nval - 3;
            nsnake++;
            }
        }

    cm->numOfBlocks = nchip_blk;
    cm->numOfSnakes = nsnake;

    if (strlen (chip_mode) < MAX_NAMESIZE) strcpy (cm->defId, chip_mode);
    else
        {
        msg ("Chip mode name '%s' too long", 2, chip_mode);
        return (NULL);
        }
    ncmode++;
    return (cm);
    }
