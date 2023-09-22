/************************************************************************/
/*                                                                      */
/* This routine parses the lvex portion of the input file, which has    */
/* already been low-level parsed into memory by the parse_vexfile()     */
/* routine.  It fills an lvex_struct struct according to the scan ID    */
/* that is passed to it.                                                */
/*                                                                      */
/*      Inputs:         scanname        Must correspond to a valid scan */
/*                                                                      */
/*      Output:         return value    Pointer to lvex_struct          */
/*                                      (null on error)                 */
/*                                                                      */
/* Created 1 March 1999 by CJL                                          */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

int lvex_del_list[200000];

struct lvex_struct *
lvex_info (char *scanname)
    {
    int i, j, nst, nchar, nctot;
    struct def *ldef;
    struct station_log *stn;
    struct block *blk;
    char *ptr;
    static struct lvex_struct lvex;
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nstmt, nblock;
    extern char lvex_src[];
    extern int do_output, lvex_ver;
                                        /* Initialize */
                                        /* Get the LVEX version number */
    if ((lvex_ver = get_version (LVEX)) == V_BAD)
        {
        msg ("Failed to get LVEX version number", 2);
        return (NULL);
        }
                                        /* Initialize the delete list */
    if (nstmt > 200000)
        {
        msg ("LVEX source file too big, more than 200000 statements", 2);
        return (NULL);
        }
    for (i=0; i<nstmt; i++) lvex_del_list[i] = TRUE;
                                        /* Retain LVEX_REV section */
    for (i=0; i<nblock; i++) 
        {
        if (strcmp (blist[i].name, "LVEX_REV") == 0)
            for (j=blist[i].stno; j<=blist[i].end; j++)
                lvex_del_list[j] = FALSE;
                                        /* And $LOG statement */
        if (strcmp (blist[i].name, "LOG") == 0)
            lvex_del_list[blist[i].stno] = FALSE;
        }
                                        /* Locate LOG block */
    for (i=0; i<nblock; i++)
        if (strcmp (blist[i].name, "LOG") == 0) break;
    if (i == nblock)
        {
        msg ("Could not find $LOG block", 3);
        return (NULL);
        }
    blk = blist + i;
                                        /* Loop over all defs (stations) */
    nst = 0;
    for (i=0; i<blk->ndef; i++)
        {
                                        /* Convenience pointers */
        ldef = blk->deflist + i;
        stn = lvex.stn + nst;
                                        /* Init */
        stn->station = C_UNDEFINED;
        stn->linked_stations[0] = '\0';
        stn->vsn[0] = '\0';
        stn->disc_set_ID[0] = '\0';
        stn->ndiscs = I_UNDEFINED;
        for (j=0; j<MAXDISCS; j++)
            {
            stn->disc[j].disc_serial_num[0] = '\0';
            stn->disc[j].disc_model_num[0] = '\0';
            stn->disc[j].disc_size = I_UNDEFINED;
            }
        stn->headpos = F_UNDEFINED;
        clear_date (&(stn->tapestart));
        clear_date (&(stn->tapestop));
        stn->start_footage = F_UNDEFINED;
        stn->start_speed = F_UNDEFINED;
        stn->end_footage = F_UNDEFINED;
        clear_date (&(stn->discstart));
        clear_date (&(stn->discstop));
        stn->start_byte = I_UNDEFINED;
        stn->stop_byte = I_UNDEFINED;
        stn->source[0] = '\0';
        clear_date (&(stn->time_on_source));
        stn->autopeak_interval = I_UNDEFINED;
        stn->pass[0] = '\0';
                                        /* Identify station */
        stn->station = ldef->name[0];
        if (strlen (ldef->name) > 10)
            {
            msg ("Invalid def name in $LOG, '%s'", 2, ldef->name);
            return (NULL);
            }
        strcpy (stn->linked_stations, ldef->name + 1);
                                        /* Find scan and extract info */
        if (get_logscan (ldef, scanname, stn) != 0)
            {
            msg ("Warning, cannot find log for scan '%s', station %c", 
                                            1, scanname, stn->station);
            continue;
            }
        nst++;
        }

    lvex.nstation = nst;
                                        /* Create stripped ascii output */
    if (do_output)
        {
        ptr = lvex_src;
        nctot = 0;
        for (i=0; i<nstmt; i++)
            {
            if (lvex_del_list[i] == TRUE) continue;
            nchar = stlist[i].end - stlist[i].start + 1;
            if ((nctot + nchar) >= 200000)
                {
                msg ("Stripped LVEX contents too large for output buffer", 2);
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

    return (&lvex);
    }
