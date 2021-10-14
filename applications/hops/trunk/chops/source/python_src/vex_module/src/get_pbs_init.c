/************************************************************************/
/*                                                                      */
/* This routine is responsible for filling in a pbs_init structure.     */
/* It is passed the name of a $PBS_INIT def and a pointer to the        */
/* empty structure, and fills in the structure.                         */
/*                                                                      */
/*      Inputs:         defname         Name of $PBS_INIT def           */
/*                      pbs             Pointer to pbs_init_struct      */
/*                                                                      */
/*      Output:         pbs_init        Filled in                       */
/*                      return value    0=OK, else bad.                 */
/*                                                                      */
/* Created 22 January 1999 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

int
get_pbs_init (char *defname,
              struct pbs_init_struct *pbs)
    {
    int i, st, hexval;
    char *str, *hexstr;
    struct def *pbdef;
    struct ref ref;
    struct block *blk;
    struct param_val p_val;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern int ivex_ver, ivex_del_list[];

                                        /* Locate $PBS_INIT block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "PBS_INIT") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $PBS_INIT block", 3);
        return (-1);
        }
    blk = blist + i;
                                        /* Find def name */
    for (i=0; i<blk->ndef; i++)
        if (strcmp (blk->deflist[i].name, defname) == 0) break;
    if (i == blk->ndef)
        {
        msg ("Could not find def '%s' in $PBS_INIT block", 3, defname);
        return (-1);
        }
    pbdef = blk->deflist + i;
                                        /* Undelete the def */
    for (st=pbdef->start; st<=pbdef->end; st++) ivex_del_list[st] = FALSE;
                                        /* Loop over statements in pbdef */
    for (st=pbdef->start+1; st<pbdef->end; st++)
        {
        str = stlist[st].str;
                                        /* Is this a ref? */
        if (strncmp (str, "ref ", 4) == 0)
            {
                                        /* Generic ref parsing */
            if (parse_ref (st, &ref) != 0)
                {
                msg ("Bad ref statement in PBS_INIT def '%s'", 2, defname);
                return (-1);
                }
                                        /* Which section is ref'ed? */
            if (strcmp (ref.blockname, "TRM_CONFIG") == 0)
                {
                if (get_trm_config (ref.keyword, &(pbs->TRM_config)) != 0)
                    {
                    msg ("Error parsing TRM_CONFIG def '%s'", 2, ref.keyword);
                    return (-1);
                    }
                }
            else if (strcmp (ref.blockname, "DRIVE_INIT") == 0)
                {
                if (get_drive_init (ref.keyword, &(pbs->drive)) != 0)
                    {
                    msg ("Error parsing DRIVE_INIT def '%s'", 2, ref.keyword);
                    return (-1);
                    }
                }
            else if (strcmp (ref.blockname, "SU/CORR_CONNECT") == 0)
                {
                if (get_su_connect (ref.keyword, &(pbs->su_connect)) != 0)
                    {
                    msg ("Error parsing SU/CORR_CONNECT def '%s'", 2, ref.keyword);
                    return (-1);
                    }
                }
            continue;
            }
                                        /* Must be parameter= statement */
        else if (parse_pval (str, "PBS_INIT", IVEX | ivex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (-1);
            }
        if ISNAME ("SU_ID")
            {
            pbs->SU_ID = p_val.dval[0].data.intval;
            strcpy (pbs->cHost, p_val.dval[1].data.strval);
            }
        else if ISNAME ("SUCC_serial_ports")
            {
            strcpy (pbs->succ_port.server_envname, p_val.dval[0].data.strval);
            pbs->succ_port.console_port = p_val.dval[1].data.intval;
            pbs->succ_port.DPU_port = p_val.dval[2].data.intval;
            }
        else if ISNAME ("tape_servo_interval")
            pbs->tape_servo_interval = p_val.dval[0].data.realval;
        else if ISNAME ("timing_monitor")
            {
            strcpy (pbs->timing_monitor.measurement_name, p_val.dval[0].data.strval);
            pbs->timing_monitor.switch_pos = p_val.dval[1].data.intval;
            pbs->timing_monitor.minimum = p_val.dval[2].data.realval;
            pbs->timing_monitor.maximum = p_val.dval[3].data.realval;
            hexstr = p_val.dval[4].data.strval;
            if (sscanf (hexstr, "%x", &hexval) != 1)
                {
                msg ("Invalid hex string '%s'", 2, hexstr);
                return (-1);
                }
            pbs->timing_monitor.fail_action = hexval;
            }
        }

    return (0);
    }
