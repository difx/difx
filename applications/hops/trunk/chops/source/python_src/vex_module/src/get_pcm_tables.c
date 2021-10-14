/************************************************************************/
/*                                                                      */
/* This routine is responsible for filling in the pcm_tab structs       */
/* the svex structure.  It reads the relevant sections in the SVEX file */
/* and does some error checking.  It is passed the name of a            */
/* $PCM_TABLES def and a pointer to the svex structure.                 */
/*                                                                      */
/*      Inputs:         defname         Name of $PCM_TABLES def         */
/*                      svex            Pointer to main svex struct     */
/*                                                                      */
/*      Output:         svex            Appropriate parts filled in     */
/*                      return value    0=OK, else bad.                 */
/*                                                                      */
/* Created 15 February 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)
#define TWOPI 2.0*3.14159265358979323846

int
get_pcm_tables (char *defname,
                struct svex_struct *svex)
    {
    int i, j, st, sample_val, type;
    double temp, junk;
    char *str;
    struct def *ptdef;
    struct block *blk;
    struct param_val p_val;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern int svex_ver, svex_del_list[];
                                        /* Initialize */
    svex->tables.delta_phase = 0.0;
    svex->tables.nval = 0;
    for (i=0; i<4; i++)
        for (j=0; j<MAXNVAL; j++)
            {
            svex->tables.sin_value[i][j] = 0;
            svex->tables.cos_value[i][j] = 0;
            }
                                        /* Locate $PCM_TABLES block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "PCM_TABLES") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $PCM_TABLES block", 3);
        return (-1);
        }
    blk = blist + i;
                                        /* Find def name */
    for (i=0; i<blk->ndef; i++)
        if (strcmp (blk->deflist[i].name, defname) == 0) break;
    if (i == blk->ndef)
        {
        msg ("Could not find def '%s' in $PCM_TABLES block", 3, defname);
        return (-1);
        }
    ptdef = blk->deflist + i;
                                        /* Undelete the def */
    for (st=ptdef->start; st<=ptdef->end; st++) svex_del_list[st] = FALSE;
                                        /* Must get the delta_phase first */
                                        /* Loop over statements in ptdef */
    for (st=ptdef->start+1; st<ptdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse parameter= statement */
        if (parse_pval (str, "PCM_TABLES", SVEX | svex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (-1);
            }
        if ISNAME ("tabular_delta_phase")
            {
                                        /* Values are returned in radians */
            svex->tables.delta_phase = p_val.dval[0].data.realval;
            temp = TWOPI / svex->tables.delta_phase;
            if (modf (temp, &junk) > 0.0001)
                {
                msg ("Phase increment does not divide evenly into 360 deg", 2);
                return (-1);
                }
                                        /* Had better be this many values */
                                        /* in the table_values statements */
            svex->tables.nval = rint (temp);
            if (svex->tables.nval > MAXNVAL)
                {
                msg ("Too small a phase increment specified", 2);
                print_location(st);
                return (-1);
                }
            }
        }
                                        /* Now loop through again looking for */
                                        /* table_values statements */
    for (st=ptdef->start+1; st<ptdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Parse parameter= statement */
        if (parse_pval (str, "PCM_TABLES", SVEX | svex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (-1);
            }
        if ISNAME ("table_values")
            {
            if (p_val.nval != (svex->tables.nval + 2))
                {
                msg ("Incorrect number of values in table_values statement", 2);
                print_location(st);
                return (-1);
                }
            sample_val = p_val.dval[0].data.intval;
            if (strcmp (p_val.dval[1].data.strval, "sin") == 0)
                type = PCAL_SIN;
            else type = PCAL_COS;
                                        /* Fill in the values */
            for (i=2; i<p_val.nval; i++)
                {
                if (type == PCAL_SIN)
                    svex->tables.sin_value[sample_val][i-2] =
                                                p_val.dval[i].data.intval;
                else if (type == PCAL_COS)
                    svex->tables.cos_value[sample_val][i-2] =
                                                p_val.dval[i].data.intval;
                }
            }
        }
            

    return (0);
    }
