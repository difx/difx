/************************************************************************/
/*                                                                      */
/* Given the name of a $CORR_BD_PARMS def, this routine returns a       */
/* pointer to a filled in structure representing the contents of the    */
/* def.  It seeks to be efficient by recognizing a def previously       */
/* parsed.  It must not overwrite other defs returned in previous calls */
/*                                                                      */
/*      Inputs:         parms_mode      Name of def                     */
/*                                                                      */
/*      Output:         return value    Cvex_BoardParms struct pointer  */
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

#define MAX_CPARMS 20

struct Cvex_BoardParms *
get_corr_bd_parms (char *parms_mode)
    {
    static struct Cvex_BoardParms cbmode[MAX_CPARMS];
    static int ncbmode = 0;
    int i, st;
    char *str;
    struct def *ddef;
    struct block *blk;
    struct Cvex_BoardParms *cp;
    struct param_val p_val;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern int cvex_ver, cvex_del_list[];
                                        /* Check for special init mode */
    if (strcmp (parms_mode, ":INIT:") == 0)
        {
        ncbmode = 0;
        return (NULL);
        }
                                        /* Already parsed? just return it */
    for (i=0; i<ncbmode; i++)
        if (strcmp (cbmode[i].defId, parms_mode) == 0) return (cbmode + i);
                                        /* OK, gotta do the work */
                                        /* Have we exceeded capacity? */
    if (ncbmode >= MAX_CPARMS)
        {
        msg ("Too many defs referenced in $CORR_BD_PARMS", 2);
        return (NULL);
        }
                                        /* Point to next cbmode struct */
    cp = cbmode + ncbmode;
                                        /* Locate CORR_PARMS block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "CORR_BD_PARMS") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $CORR_BD_PARMS block", 3);
        return (NULL);
        }
    blk = blist + i;
                                        /* Find def name */
    for (i=0; i<blk->ndef; i++)
        if (strcmp (blk->deflist[i].name, parms_mode) == 0) break;
    if (i == blk->ndef)
        {
        msg ("Could not find mode '%s' in $CORR_BD_PARMS block", 3, parms_mode);
        return (NULL);
        }
    ddef = blk->deflist + i;
                                        /* Undelete the def */
    for (st=ddef->start; st<=ddef->end; st++)
        cvex_del_list[st] = FALSE;
                                        /* Loop over statements in ddef */
    for (st=ddef->start+1; st<ddef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse "parameter=" statement */
        if (parse_pval (str, "CORR_BD_PARMS", CVEX | cvex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (NULL);
            }

        if ISNAME ("accum_divide_ratio")
            cp->accumRatio = p_val.dval[0].data.intval;

        else if ISNAME ("shsmp_divide_ratio")
            {
            cp->bocfShsmpRatio = p_val.dval[0].data.intval;
            cp->dataShsmpRatio = p_val.dval[1].data.intval;
            }
        else if ISNAME ("sample_count_per_lag_enable")
            {
            if (strcmp (p_val.dval[0].data.strval, "on") == 0)
                cp->sampleCntPerLagEnable = TRUE;
            else cp->sampleCntPerLagEnable = FALSE;
            }
        }


    if (strlen (parms_mode) < MAX_NAMESIZE) strcpy (cp->defId, parms_mode);
    else
        {
        msg ("CORR_BD_PARMS mode name '%s' too long", 2, parms_mode);
        return (NULL);
        }

    ncbmode++;
    return (cp);
    }
