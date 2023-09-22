/************************************************************************/
/*                                                                      */
/* This routine is responsible for filling in a drive_init structure.   */
/* It is passed the name of a $DRIVE_INIT def and a pointer to the      */
/* empty structure, and fills in the structure.                         */
/*                                                                      */
/*      Inputs:         defname         Name of $DRIVE_INIT def         */
/*                      drive_init      Pointer to drive_init_struct    */
/*                                                                      */
/*      Output:         drive_init      Filled in                       */
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
get_drive_init (char *defname,
                struct drive_init_struct *drive_init)
    {
    int i, head, maxhead, st, npair, dim, hs;
    char *str, *hexstr;
    short drv;
    struct def *didef;
    struct block *blk;
    struct param_val p_val;
    unsigned int hexval;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern int ivex_ver, ivex_del_list[];
                                        /* Initialize */
    for (i=0; i<4; i++)
        {
        drive_init->DIM_connect[i].headstack = 0;
        drive_init->DIM_connect[i].parity = NONE;
        drive_init->DIM_connect[i].eqlzr = 0;
        }
    drive_init->nheads = 0;
                                        /* Locate $DRIVE_INIT block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "DRIVE_INIT") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $DRIVE_INIT block", 3);
        return (-1);
        }
    blk = blist + i;
                                        /* Find def name */
    for (i=0; i<blk->ndef; i++)
        if (strcmp (blk->deflist[i].name, defname) == 0) break;
    if (i == blk->ndef)
        {
        msg ("Could not find def '%s' in $DRIVE_INIT block", 3, defname);
        return (-1);
        }
    didef = blk->deflist + i;
                                        /* Undelete the def */
    for (st=didef->start; st<=didef->end; st++) ivex_del_list[st] = FALSE;
                                        /* Loop over statements in didef */
    maxhead = 0;
    for (st=didef->start+1; st<didef->end; st++)
        {
        str = stlist[st].str;
                                        /* Must be parameter= statement */
        if (parse_pval (str, "DRIVE_INIT", IVEX | ivex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (-1);
            }
        if ISNAME ("drive_type")
            {
            if (strcmp (p_val.dval[0].data.strval, "Mark4") == 0) drv = DRV_MK4;
            else if (strcmp (p_val.dval[0].data.strval, "VLBA") == 0) drv = DRV_VLBA;
            else if (strcmp (p_val.dval[0].data.strval, "Mark5P") == 0) drv = DRV_MK5P;
            else if (strcmp (p_val.dval[0].data.strval, "Mark5A") == 0) drv = DRV_MK5A;
            else if (strcmp (p_val.dval[0].data.strval, "Mark5B") == 0) drv = DRV_MK5B;
            drive_init->drive_type = drv;
            if ((drv == DRV_MK5P) || (drv == DRV_MK5A))
                strcpy (drive_init->computer_name, p_val.dval[1].data.strval);
            }
        else if ISNAME ("mcb_address")
            {
            hexstr = p_val.dval[0].data.strval;
            if (sscanf (hexstr, "%x", &hexval) != 1)
                {
                msg ("Invalid hex string '%s'", 2, hexstr);
                return (-1);
                }
            drive_init->mcb_addr_start = hexval;
            hexstr = p_val.dval[1].data.strval;
            if (sscanf (hexstr, "%x", &hexval) != 1)
                {
                msg ("Invalid hex string '%s'", 2, hexstr);
                return (-1);
                }
            drive_init->mcb_addr_id = hexval;
            }
        else if ISNAME ("capstan_relative_diameter")
            drive_init->capstan_relative_diameter = p_val.dval[0].data.realval;
        else if ISNAME ("vacuum_setting")
            {
            npair = p_val.nval / 2;
            if (npair > 2) npair = 2;
            for (i=0; i<npair; i++)
                {
                drive_init->vacuum[i].inches_of_H2O =
                                        p_val.dval[i*2].data.intval;
                hexstr = p_val.dval[i*2 + 1].data.strval;
                if (sscanf (hexstr, "%x", &hexval) != 1)
                    {
                    msg ("Invalid hex string '%s'", 2, hexstr);
                    return (-1);
                    }
                drive_init->vacuum[i].setting = hexval;
                }
            }
        else if ISNAME ("tape_acceleration")
            drive_init->tape_acceleration = p_val.dval[0].data.realval;
        else if ISNAME ("headstack/DIM_connect")
            {
            dim = p_val.dval[2].data.intval;
            head = p_val.dval[0].data.intval;
            if (head > maxhead) maxhead = head;
            drive_init->DIM_connect[dim].headstack = head;
            if (strcmp (p_val.dval[1].data.strval, "odd") == 0)
                drive_init->DIM_connect[dim].parity = ODD;
            else if (strcmp (p_val.dval[1].data.strval, "even") == 0)
                drive_init->DIM_connect[dim].parity = EVEN;
            drive_init->DIM_connect[dim].eqlzr = p_val.dval[3].data.intval;
            }
        else if ISNAME ("monitor_module_track")
            {
            hs = p_val.dval[0].data.intval;
            drive_init->monitor_module[hs].track[0] = p_val.dval[1].data.intval;
            drive_init->monitor_module[hs].track[1] = p_val.dval[2].data.intval;
            drive_init->monitor_module[hs].equalizer[0] = p_val.dval[3].data.intval;
            if (p_val.nval >= 5)
                drive_init->monitor_module[hs].equalizer[1] = p_val.dval[4].data.intval;
            else drive_init->monitor_module[hs].equalizer[1] = I_UNDEFINED;
            if (p_val.nval == 6)
                drive_init->monitor_module[hs].equalizer[2] = p_val.dval[5].data.intval;
            else drive_init->monitor_module[hs].equalizer[2] = I_UNDEFINED;
            }
        else if ISNAME ("headstack_parms")
            {
            hs = p_val.dval[0].data.intval;
            for (i=0; i<8; i++)
                {
                drive_init->headstack[hs].param[i] = p_val.dval[i+1].data.realval;
                }
            }
        }

    drive_init->nheads = maxhead;

    return (0);
    }
