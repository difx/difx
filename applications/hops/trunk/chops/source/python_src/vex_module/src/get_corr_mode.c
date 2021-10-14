/************************************************************************/
/*                                                                      */
/*                                                                      */
/*      Inputs:         modename        A named cormode in the vex file */
/*                                                                      */
/*      Output:         return value    Pointer to Cvex_CorrMode struct */
/*                                      (null on error)                 */
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

struct Cvex_Mode *
get_corr_mode (char *modename)
    {
    int i, nboard, nxpol, sno, st;
    struct def *mdef;
    struct block *blk;
    struct param_val p_val;
    struct Cvex_Section *bsect;
    struct Cvex_SectionMode *get_section_mode();
    struct Cvex_Board *board_struct;
    struct Cvex_XPolPair *xpol_struct;
    struct Cvex_BoardParms *get_corr_bd_parms();
    static struct Cvex_Mode mode;
    char *str, *board_parms, *smode;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern int cvex_ver, cvex_del_list[];
                                        /* Locate CORR_MODE block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "CORR_MODE") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $CORR_MODE block", 3);
        return (NULL);
        }
    blk = blist + i;
                                        /* Retrieve mode def */
    for (i=0; i<blk->ndef; i++)
        if (strcmp (blk->deflist[i].name, modename) == 0) break;
    if (i == blk->ndef)
        {
        msg ("Could not find mode '%s' in $CORR_MODE block", 3, modename);
        return (NULL);
        }
    mdef = blk->deflist + i;
                                        /* Undelete mdef */
    for (st=mdef->start; st<=mdef->end; st++)
        cvex_del_list[st] = FALSE;
                                        /* Loop over statements in mdef */
    nboard = nxpol = 0;
    for (st=mdef->start+1; st<mdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse "parameter=" statement */
        if (parse_pval (str, "CORR_MODE", CVEX | cvex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (NULL);
            }

        if ISNAME ("board")
            {
                                        /* Point to next board structure */
            if (nboard >= MAX_BRDS_P_CORR)
                {
                msg ("Maximum number of 'board =' statements exceeded", 2);
                print_location (st);
                return (NULL);
                }
            board_struct = mode.board + nboard;
                                        /* Extract values */
            strcpy (board_struct->segId, p_val.dval[0].data.strval);
            strcpy (board_struct->sliceId, p_val.dval[1].data.strval);
                                        /* Get board parameters */
            board_parms = p_val.dval[2].data.strval;
            board_struct->mode = get_corr_bd_parms (board_parms);
            if (board_struct->mode == NULL)
                {
                msg ("Failure getting board params structure '%s'", 2, board_parms);
                print_location (st);
                return (NULL);
                }
                                        /* Section configuration */
            for (i=3; i<p_val.nval; i+=4)
                {
                                        /* Point to current section */
                sno = (i-3)/4;
                if (sno >= MAX_SECTS_P_BRD)
                    {
                    msg ("Number of sections/board exceeded", 2);
                    print_location (st);
                    return (NULL);
                    }
                bsect = board_struct->section + sno;
                                        /* Get the section mode */
                smode = p_val.dval[i].data.strval;
                bsect->mode = get_section_mode (smode);
                if (bsect->mode == NULL)
                    {
                    msg ("Failure section mode structure '%s'", 2, smode);
                    print_location (st);
                    return (NULL);
                    }
                                        /* Extract rest of section parms */
                bsect->phyChipNum = p_val.dval[i+1].data.intval;
                strcpy (bsect->refId, p_val.dval[i+2].data.strval);
                strcpy (bsect->remId, p_val.dval[i+3].data.strval);
                }
            board_struct->numOfSections = sno + 1;
                                        /* Keep count of board statements */
            nboard++;
            }

        else if ISNAME ("cross_pol_chan_pair")
            {
            if (nxpol >= MAX_XPOL_P_BRD)
                {
                msg ("Max. no. of 'cross_pol_chan_pair' statements exceeded", 2);
                print_location (st);
                return (NULL);
                }
            xpol_struct = mode.xPolPair + nxpol;
            strcpy (xpol_struct->chn1Id, p_val.dval[0].data.strval);
            strcpy (xpol_struct->chn2Id, p_val.dval[1].data.strval);
            nxpol++;
            }
        }

    mode.numOfBoards = nboard;
    mode.numOfXPolPairs = nxpol;
    if (strlen (modename) < MAX_NAMESIZE) strcpy (mode.defId, modename);
    else
        {
        msg ("Correlator mode name '%s' too long", 2, modename);
        return (NULL);
        }

    return (&mode);
    }
