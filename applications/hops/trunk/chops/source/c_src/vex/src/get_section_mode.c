/************************************************************************/
/*                                                                      */
/* Given the name of a $CORR_SECTION_MODE def, this routine returns a   */
/* pointer to a filled in structure representing the contents of the    */
/* def.  It seeks to be efficient by recognizing a def previously       */
/* parsed.  It must not overwrite other defs returned in previous calls */
/*                                                                      */
/*      Inputs:         sect_mode       Name of def                     */
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

#define MAX_SECT_MODES 20

struct Cvex_SectionMode *
get_section_mode (char *sect_mode)
    {
    static struct Cvex_SectionMode smode[MAX_SECT_MODES];
    static int nsmode = 0;
    int i, nchip, st;
    char *str, *chip_mode;
    struct def *mdef;
    struct block *blk;
    struct Cvex_Chip *chip_struct;
    struct Cvex_SectionMode *sm;
    struct Cvex_ChipMode *get_chip_mode();
    struct param_val p_val;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern int cvex_ver, cvex_del_list[];
                                        /* Check for special init mode */
    if (strcmp (sect_mode, ":INIT:") == 0)
        {
        nsmode = 0;
        get_chip_mode (":INIT:");
        return (NULL);
        }
                                        /* Already parsed? just return it */
    for (i=0; i<nsmode; i++)
        if (strcmp (smode[i].defId, sect_mode) == 0) return (smode + i);
                                        /* OK, gotta do the work */
                                        /* Have we exceeded capacity? */
    if (nsmode >= MAX_SECT_MODES)
        {
        msg ("Too many defs referenced in $CORR_SECTION_MODE", 2);
        return (NULL);
        }
                                        /* Point to next smode struct */
    sm = smode + nsmode;
                                        /* Locate CORR_SECTION_MODE block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "CORR_SECTION_MODE") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $CORR_SECTION_MODE block", 3);
        return (NULL);
        }
    blk = blist + i;
                                        /* Find def name */
    for (i=0; i<blk->ndef; i++)
        if (strcmp (blk->deflist[i].name, sect_mode) == 0) break;
    if (i == blk->ndef)
        {
        msg ("Could not find mode '%s' in $CORR_SECTION_MODE block", 3, sect_mode);
        return (NULL);
        }
    mdef = blk->deflist + i;
                                        /* Undelete mdef */
    for (st=mdef->start; st<=mdef->end; st++)
        cvex_del_list[st] = FALSE;
                                        /* Loop over statements in mdef */
    nchip = 0;
    for (st=mdef->start+1; st<mdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse "parameter=" statement */
        if (parse_pval (str, "CORR_SECTION_MODE", CVEX | cvex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (NULL);
            }

        if ISNAME ("chip")
            {
            if (nchip >= MAX_CHIPS_P_BRD)
                {
                msg ("Maximum number of 'chip =' statements exceeded", 2);
                print_location (st);
                return (NULL);
                }
            chip_struct = sm->chip + nchip;
            chip_mode = p_val.dval[0].data.strval;
            chip_struct->relChipNum = p_val.dval[1].data.intval;
            strcpy (chip_struct->input[0], p_val.dval[2].data.strval);
            strcpy (chip_struct->input[1], p_val.dval[3].data.strval);
            strcpy (chip_struct->input[2], p_val.dval[4].data.strval);
            strcpy (chip_struct->input[3], p_val.dval[5].data.strval);
                                        /* Make pointer to chip mode */
            chip_struct->mode = get_chip_mode (chip_mode);
            if (chip_struct->mode == NULL)
                {
                msg ("Failure getting chip mode struct '%s'", 2, chip_mode);
                print_location (st);
                return (NULL);
                }
            nchip++;
            }
        }

    sm->numOfChips = nchip;
    if (strlen (sect_mode) < MAX_NAMESIZE) strcpy (sm->defId, sect_mode);
    else
        {
        msg ("Section mode name '%s' too long", 2, sect_mode);
        return (NULL);
        }

    nsmode++;
    return (sm);
    }
