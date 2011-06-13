/************************************************************************/
/*                                                                      */
/* Given a def in a $LOG section, this routine finds the scan/endscan   */
/* section corresponding to the named scan.  It then extracts the log   */
/* information for that scan and places it in the logstn structure      */
/*                                                                      */
/*      Inputs:         ldef            def in $LOG block               */
/*                      scanname        Name of requested scan          */
/*                                                                      */
/*      Output:         logstn          Filled in                       */
/*                      return value    0=OK, -1 bad, -2 scan missing   */
/*                                                                      */
/* Created 2 March 1999 by CJL                                          */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define SCAN 1
#define ENDSCAN 2
#define OTHER 3

#define ISNAME(namestring) (strcmp (p_val.name, namestring) == 0)

int
get_logscan (struct def *ldef,
             char *scanname,
             struct station_log *logstn)
    {
    int i, start, end, in_scan, scan_start, scan_end, stmt, st, n, nd;
    int null_scan_done, scan_found;
    char stname[128], arg1[128], junk[128], *str;
    long long bytes;
    struct param_val p_val;
    extern int nstmt, lvex_ver, lvex_del_list[];
    extern struct statement *stlist;
                                        /* Get start/end statement #'s */
    start = ldef->start;
    end = ldef->end;
                                        /* Search for scan/endscan pairs */
    in_scan = null_scan_done = scan_found = FALSE;
    for (st=start+1; st<end; st++)
        {
                                        /* Identify the statement */
        n = sscanf (stlist[st].str, "%s %s %s", stname, arg1, junk);
        if (strcmp (stname, "scan") == 0) stmt = SCAN;
        else if (strcmp (stname, "endscan") == 0) stmt = ENDSCAN;
        else stmt = OTHER;
                                        /* Take appropriate action */
        switch (stmt)
            {
            case SCAN:
                if (in_scan)
                    {
                    msg ("Misplaced 'scan' statement", 2);
                    print_location (st);
                    return (-1);
                    }
                                        /* Valid scan has 1 argument */
                if (n != 2)
                    {
                    msg ("Malformed scan statement '%s'", 2, stlist[st].str);
                    print_location (st);
                    return (-1);
                    }
                if (strcmp (scanname, arg1) != 0)
                    {
                                        /* Non-null string that doesn't match */
                    if (strlen(scanname) != 0) break;
                                        /* Second scan but null input */
                    else if (null_scan_done)
                        {
                        msg ("Multiple scans found, but null scan specified", 2);
                        return (-1);
                        }
                    else 
                        {
                        null_scan_done = TRUE;
                        scan_start = st;
                        }
                    }
                                        /* Found it */
                else 
                    {
                    scan_found = TRUE;
                    scan_start = st;
                    }
                                        /* All OK */
                in_scan = TRUE;
                break;

            case ENDSCAN:
                if (n != 1)
                    {
                    msg ("Malformed endscan statement '%s'", 2, stlist[st].str);
                    print_location (st);
                    return (-1);
                    }
                if (in_scan && (scan_found || null_scan_done)) scan_end = st;
                in_scan = FALSE;
                break;

            case OTHER:
            default:
                break;
            }
        }
                                        /* Acceptable result? */
    if (null_scan_done)
        msg ("Successfully found the only log scan '%s'", -1, arg1);
    else if (scan_found)
        msg ("Successfully found log scan '%s'", 0, scanname);
    else
        {
        msg ("Cannot find scan '%s' for station '%c' in logvex", 1, scanname, logstn->station);
        return (-2);
        }
                                        /* Sanity check */
    if ((scan_start < start) || (scan_end > end))
        {
        msg ("Error in get_logscan(), bad scan statement numbers", 2);
        return (-1);
        }
                                        /* Now loop over scan itself */
    for (st=scan_start+1; st<scan_end; st++)
        {
        str = stlist[st].str;

        if (parse_pval (str, "LOG", LVEX | lvex_ver, &p_val) != 0)
            {
            msg ("Error parsing statement '%s'", 2, stlist[st].str);
            print_location (st);
            return (-1);
            }
        if ISNAME ("VSN") strcpy (logstn->vsn, p_val.dval[0].data.strval);
        else if ISNAME ("disc_set_ID") 
            {
            strcpy (logstn->disc_set_ID, p_val.dval[0].data.strval);
            logstn->ndiscs = p_val.dval[1].data.intval;
            }
        else if ISNAME ("disc_serial")
            {
            if (p_val.nval > 16)
                {
                msg ("Too many entries in disc_serial statement", 2);
                return (-1);
                }
            nd = p_val.nval;
            for (i=0; i<nd; i++)
                strcpy (logstn->disc[i].disc_serial_num, p_val.dval[i].data.strval);
            }
        else if ISNAME ("disc_model")
            {
            if (p_val.nval > 16)
                {
                msg ("Too many entries in disc_model statement", 2);
                return (-1);
                }
            nd = p_val.nval;
            for (i=0; i<nd; i++)
                strcpy (logstn->disc[i].disc_model_num, p_val.dval[i].data.strval);
            }
        else if ISNAME ("disc_size")
            {
            if (p_val.nval > 16)
                {
                msg ("Too many entries in disc_serial statement", 2);
                return (-1);
                }
            nd = p_val.nval;
            for (i=0; i<nd; i++)
                logstn->disc[i].disc_size = p_val.dval[i].data.intval;
            }
        else if ISNAME ("head_pos") logstn->headpos = p_val.dval[0].data.realval;
        else if ISNAME ("start_tape")
            {
            memcpy (&(logstn->tapestart), &(p_val.dval[0].data.epochval),
                        sizeof (struct date));
            logstn->start_footage = p_val.dval[1].data.realval;
            logstn->start_speed = p_val.dval[2].data.realval;
            }
        else if ISNAME ("stop_tape")
            {
            memcpy (&(logstn->tapestop), &(p_val.dval[0].data.epochval),
                        sizeof (struct date));
            logstn->end_footage = p_val.dval[1].data.realval;
            }
        else if ISNAME ("start_disc")
            {
            memcpy (&(logstn->discstart), &(p_val.dval[0].data.epochval),
                        sizeof (struct date));
            n = sscanf (p_val.dval[1].data.strval, "%lld", &bytes);
            if ((n != 1) || (bytes < 0))
                {
                msg ("Invalid byte offset in start_disc statement '%s'", 
                                2, p_val.dval[1].data.strval);
                return (-1);
                }
            logstn->start_byte = bytes;
            }
        else if ISNAME ("stop_disc")
            {
            memcpy (&(logstn->discstop), &(p_val.dval[0].data.epochval),
                        sizeof (struct date));
            n = sscanf (p_val.dval[1].data.strval, "%lld", &bytes);
            if ((n != 1) || (bytes < 0))
                {
                msg ("Invalid byte offset in stop_disc statement '%s'", 
                                2, p_val.dval[1].data.strval);
                return (-1);
                }
            logstn->stop_byte = bytes;
            }
        else if ISNAME ("source")
            {
            strcpy (logstn->source, p_val.dval[0].data.strval);
            if (p_val.nval == 2)
                memcpy (&(logstn->time_on_source), &(p_val.dval[0].data.epochval),
                        sizeof (struct date));
            }
        else if ISNAME ("autopeak_interval") 
            logstn->autopeak_interval = p_val.dval[0].data.intval;
        else if ISNAME ("pass") 
            strcpy (logstn->pass, p_val.dval[0].data.strval);
        }
                                        /* Undelete relevant statements */
    lvex_del_list[start] = FALSE;
    lvex_del_list[end] = FALSE;
    for (st=scan_start; st<=scan_end; st++) lvex_del_list[st] = FALSE;

    return (0);
    }
