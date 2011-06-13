/************************************************************************/
/*                                                                      */
/* This routine is responsible for filling in a su_connect structure.   */
/* It is passed the name of a $SU/CORR_CONNECT def and a pointer to the */
/* empty structure, and fills in the structure.                         */
/*                                                                      */
/*      Inputs:         defname         Name of $SU/CORR_CONNECT def    */
/*                      su_connect      Pointer to su_connect_struct    */
/*                                                                      */
/*      Output:         su_connect      Filled in                       */
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
get_su_connect (char *defname,
                struct su_connect_struct *su_connect)
    {
    int i, st, scg;
    char *str;
    struct def *sucdef;
    struct block *blk;
    struct param_val p_val;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern int ivex_ver, ivex_del_list[];
                                        /* Initialize */
    for (i=0; i<4; i++)
        {
        su_connect->corr_segment[i] = -1;
        su_connect->inputbd_chan_grp[i] = -1;
        }
                                        /* Locate $SU/CORR_CONNECT block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "SU/CORR_CONNECT") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $SU/CORR_CONNECT block", 3);
        return (-1);
        }
    blk = blist + i;
                                        /* Find def name */
    for (i=0; i<blk->ndef; i++)
        if (strcmp (blk->deflist[i].name, defname) == 0) break;
    if (i == blk->ndef)
        {
        msg ("Could not find def '%s' in $SU/CORR_CONNECT block", 3, defname);
        return (-1);
        }
    sucdef = blk->deflist + i;
                                        /* Undelete the def */
    for (st=sucdef->start; st<=sucdef->end; st++) ivex_del_list[st] = FALSE;
                                        /* Loop over statements in sucdef */
    for (st=sucdef->start+1; st<sucdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Must be parameter= statement */
        if (parse_pval (str, "SU/CORR_CONNECT", IVEX | ivex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (-1);
            }
        if ISNAME ("SU_connect")
            {
            scg = p_val.dval[0].data.intval;
            su_connect->corr_segment[scg] = p_val.dval[1].data.intval;
            su_connect->inputbd_chan_grp[scg] = p_val.dval[2].data.intval;
            }
        }

    return (0);
    }
