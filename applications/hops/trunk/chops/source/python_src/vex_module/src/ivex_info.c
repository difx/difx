/************************************************************************/
/*                                                                      */
/* This routine parses the ivex portion of the input file, which has    */
/* already been low-level parsed into memory by the parse_vexfile()     */
/* routine.  It fills an ivex_struct struct according to the key to the */
/* $CORR_INIT section that is passed to it.                             */
/*                                                                      */
/*      Inputs:         key             A def in the $CORR_INIT block   */
/*                                                                      */
/*      Output:         return value    Pointer to ivex_struct          */
/*                                      (null on error)                 */
/*                                                                      */
/* Created 21 January 1999 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

int ivex_del_list[2048];

struct ivex_struct *
ivex_info (char *key)
    {
    int i, j, nm, st, incomplete, nchar, retain, npbs, nctot, crate, hexval;
    struct def *idef;
    struct ref ref;
    struct block *blk;
    struct param_val p_val;
    char *str, *ptr, *hexstr;
    static struct ivex_struct ivex;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern char ivex_src[];
    extern int do_output, ivex_ver;
                                        /* Initialize */
    ivex.system_tempo = 0.0;
    ivex.bocf_period = 0;
    ivex.header_duration = 0;
    ivex.nmonitor = 0;
                                        /* Get the IVEX version number */
    if ((ivex_ver = get_version (IVEX)) == V_BAD)
        {
        msg ("Failed to get IVEX version number", 2);
        return (NULL);
        }
                                        /* Initialize the delete list */
    if (nstmt > 2048)
        {
        msg ("IVEX source file too big, more than 2048 statements", 2);
        return (NULL);
        }
    for (i=0; i<nstmt; i++) ivex_del_list[i] = TRUE;
                                        /* Retain all IVEX $BLOCK statements */
    for (i=0; i<nblock; i++) 
        {
        retain = FALSE;
        if (strcmp (blist[i].name, "CORR_INIT") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "PBS_INIT") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "DRIVE_INIT") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "TRM_CONFIG") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "SU/CORR_CONNECT") == 0) retain = TRUE;
        if (retain) ivex_del_list[blist[i].stno] = FALSE;
                                        /* Retain IVEX_REV section */
        if (strcmp (blist[i].name, "IVEX_REV") == 0)
            for (j=blist[i].stno; j<=blist[i].end; j++)
                ivex_del_list[j] = FALSE;
        }
                                        /* Locate CORR_INIT block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "CORR_INIT") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $CORR_INIT block", 3);
        return (NULL);
        }
                                        /* Check for null key and root file */
    blk = blist + i;
    if (key[0] == '\0')
        {
        if (blk->ndef != 1)
            {
            msg ("Multiple keys present in ivex file, but none specified", 2);
            return (NULL);
            }
        idef = blk->deflist;
        }
                                        /* Retrieve key def */
    else
        {
        for (i=0; i<blk->ndef; i++)
            if (strcmp (blk->deflist[i].name, key) == 0) break;
        if (i == blk->ndef)
            {
            msg ("Could not find key '%s' in $CORR_INIT block", 3, key);
            return (NULL);
            }
        idef = blk->deflist + i;
        }
                                        /* Undelete the def */
    for (st=idef->start; st<=idef->end; st++) ivex_del_list[st] = FALSE;
                                        /* Loop over statements in idef */
    npbs = 0;
    nm = 0;
    for (st=idef->start+1; st<idef->end; st++)
        {
        str = stlist[st].str;
                                        /* parameter and ref statements */
                                        /* may be mixed */
        if (strncmp (str, "ref ", 4) == 0)
            {
                                        /* Generic ref parsing */
            if (parse_ref (st, &ref) != 0)
                {
                msg ("Bad ref statement in IVEX def '%s'", 2, key);
                return (NULL);
                }
                                        /* pbs_init is simple keyword copy */
            if (strcmp (ref.blockname, "PBS_INIT") == 0)
                {
                if (get_pbs_init (ref.keyword, ivex.pbs_init + npbs) == 0)
                    npbs++;
                else
                    {
                    msg ("Error parsing PBS_INIT def '%s'", 2, ref.keyword);
                    return (NULL);
                    }
                }
            continue;
            }
                                        /* Parse "parameter=" statement */
        else if (parse_pval (str, "CORR_INIT", IVEX | ivex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (NULL);
            }

        if ISNAME ("system_tempo") 
            ivex.system_tempo = p_val.dval[0].data.realval;
        else if ISNAME ("bocf_period")
            ivex.bocf_period = p_val.dval[0].data.intval;
        else if ISNAME ("header_duration")
            ivex.header_duration = p_val.dval[0].data.intval;
        else if ISNAME ("CUCC_serial_ports")
            {
            crate = p_val.dval[0].data.intval;
            ivex.cucc_ports[crate].crate_number = crate;
            strcpy (ivex.cucc_ports[crate].server_envname, p_val.dval[1].data.strval);
            ivex.cucc_ports[crate].port_A_number = p_val.dval[2].data.intval;
            ivex.cucc_ports[crate].port_B_number = p_val.dval[3].data.intval;
            }
        else if ISNAME ("start_stop_counter")
            {
            strcpy (ivex.counter_envname, p_val.dval[0].data.strval);
            ivex.counter_port = p_val.dval[1].data.intval;
            }
        else if ISNAME ("analog_switch")
            {
            strcpy (ivex.switch_envname, p_val.dval[0].data.strval);
            ivex.switch_port = p_val.dval[1].data.intval;
            }
        else if ISNAME ("CF_edit_mask")
            {
            hexstr = p_val.dval[0].data.strval;
            if (sscanf (hexstr, "%x", &hexval) != 1)
                {
                msg ("Invalid hex string '%s'", 2, hexstr);
                return (NULL);
                }
            ivex.cf_edit_mask = hexval;
            }
        else if ISNAME ("timing_monitor")
            {
            if (nm >= 10)
                {
                msg ("Too many timing_monitor statements in $CORR_INIT", 2);
                return (NULL);
                }
            strcpy (ivex.timing_monitor[nm].measurement_name, p_val.dval[0].data.strval);
            ivex.timing_monitor[nm].switch_pos = p_val.dval[1].data.intval;
            ivex.timing_monitor[nm].minimum = p_val.dval[2].data.realval;
            ivex.timing_monitor[nm].maximum = p_val.dval[3].data.realval;
            hexstr = p_val.dval[4].data.strval;
            if (sscanf (hexstr, "%x", &hexval) != 1)
                {
                msg ("Invalid hex string '%s'", 2, hexstr);
                return (NULL);
                }
            ivex.timing_monitor[nm].fail_action = hexval;
            nm++;
            ivex.nmonitor = nm;
            }
        }
    ivex.nplayback_systems = npbs;
                                        /* Must meet minimum requirements */
    incomplete = FALSE;
    if (npbs == 0) incomplete = TRUE;
    if (ivex.system_tempo == 0.0) incomplete = TRUE;
    if (ivex.bocf_period == 0) incomplete = TRUE;
    if (incomplete)
        {
        msg ("Insufficient information supplied in IVEX def '%s'", 2, key);
        return (NULL);
        }
                                        /* Create stripped ascii output */
    if (do_output)
        {
        ptr = ivex_src;
        nctot = 0;
        for (i=0; i<nstmt; i++)
            {
            if (ivex_del_list[i] == TRUE) continue;
            nchar = stlist[i].end - stlist[i].start + 1;
            if ((nctot + nchar) >= 32768)
                {
                msg ("Stripped IVEX contents too large for output buffer", 2);
                return (NULL);
                }
            strncpy (ptr, stlist[i].start, nchar);
            ptr += nchar;
            nctot += nchar;
            }
        *ptr = '\n';
        ptr++;
        *ptr = '\0';
        }

    return (&ivex);
    }
