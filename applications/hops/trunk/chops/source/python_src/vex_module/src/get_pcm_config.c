/************************************************************************/
/*                                                                      */
/* This routine is responsible for filling in the pcm_cfg struct array, */
/* and the pcm_tab structs in the svex structure.  It reads the         */
/* relevant sections in the SVEX file, and does some error checking.    */
/* It is passed the name of a $PCM_CONFIG def and a pointer to the      */
/* svex structure.                                                      */
/*                                                                      */
/*      Inputs:         defname         Name of $PCM_CONFIG def         */
/*                      svex            Pointer to main svex struct     */
/*                                                                      */
/*      Output:         svex            Appropriate parts filled in     */
/*                      return value    0=OK, else bad.                 */
/*                                                                      */
/* Created 15 February 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

int
get_pcm_config (char *defname,
                struct svex_struct *svex)
    {
    int i, j, st, cntr_no, ncfg;
    double coeff;
    char *str, table_ref[MAX_NAMESIZE];
    struct def *pcdef;
    struct block *blk;
    struct pcm_cfg_struct *pcm;
    struct param_val p_val;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern int svex_ver, svex_del_list[];
                                        /* Initialize */
    for (i=0; i<8; i++)
        {
        svex->pcm_cfg[i].freq[0] = '\0';
        for (j=0; j<9; j++) svex->pcm_cfg[i].counter_coeff[j] = 0.0;
        }
                                        /* Locate $PCM_CONFIG block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "PCM_CONFIG") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $PCM_CONFIG block", 3);
        return (-1);
        }
    blk = blist + i;
                                        /* Find def name */
    for (i=0; i<blk->ndef; i++)
        if (strcmp (blk->deflist[i].name, defname) == 0) break;
    if (i == blk->ndef)
        {
        msg ("Could not find def '%s' in $PCM_CONFIG block", 3, defname);
        return (-1);
        }
    pcdef = blk->deflist + i;
                                        /* Undelete the def */
    for (st=pcdef->start; st<=pcdef->end; st++) svex_del_list[st] = FALSE;
                                        /* Loop over statements in pcdef */
    table_ref[0] = '\0';
    ncfg = 0;
    for (st=pcdef->start+1; st<pcdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse parameter= statement */
        if (parse_pval (str, "PCM_CONFIG", SVEX | svex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (-1);
            }
        if ISNAME ("phase_cal")
            {
                                        /* Can be maximum of 8 */
            if (ncfg >= 8)
                {
                msg ("Too many phase-cal statements in def '%s'", 2, defname);
                print_location (st);
                return (-1);
                }
            pcm = svex->pcm_cfg + ncfg;
                                        /* Check we are not asked to do too much */
            if (strlen(table_ref) == 0) 
                strcpy (table_ref, p_val.dval[0].data.strval);
            else if (strcmp (table_ref, p_val.dval[0].data.strval) != 0)
                {
                msg ("Mixed PCM_TABLE defs in a PCM_CONFIG def not supported yet", 2);
                print_location (st);
                return (-1);
                }
                                        /* Can't resolve to a real frequency */
                                        /* yet, because OVEX might be needed */
            strcpy (pcm->freq, p_val.dval[1].data.strval);

            if (strcmp (p_val.dval[2].data.strval, "sin") == 0)
                pcm->type = PCAL_SIN;
            else pcm->type = PCAL_COS;
                                        /* Look for pairs of indices/coeffs */
            for (i=3; i<p_val.nval; i+=2)
                {
                cntr_no = p_val.dval[i].data.intval;
                                        /* Duplicates prohibited */
                if ((pcm->counter_coeff[cntr_no] != 0.0) && (cntr_no != 8))
                    {
                    msg ("Duplicated entry in counter coefficient table", 2);
                    print_location (st);
                    return (-1);
                    }
                coeff = p_val.dval[i+1].data.realval;
                pcm->counter_coeff[cntr_no] = coeff;
                }
                                        /* Last one must be validity counter */
            if (cntr_no != 8)
                {
                msg ("Invalid final entry in phase_cal statement", 2);
                print_location (st);
                return (-1);
                }
            ncfg++;
            }
        }
                                        /* Now need to get pcm_table itself */
    if (get_pcm_tables (table_ref, svex) != 0)
        {
        msg ("Error reading PCM_TABLES def '%s'", 2, table_ref);
        return (-1);
        }

    return (0);
    }
