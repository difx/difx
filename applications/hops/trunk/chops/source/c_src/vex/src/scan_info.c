/************************************************************************/
/*                                                                      */
/*                                                                      */
/*      Inputs:         scanname        A named scan in the vex file    */
/*                      stations        List of stations to process,    */
/*                                      blank means do all              */
/*                                                                      */
/*      Output:         return value    Pointer to scan_info structure  */
/*                                      (null on error)                 */
/*                                                                      */
/* Created January 6 1998 by CJL                                        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "vex.h"
#include "mk4_vex.h"
#include "mk4_util.h"

#define FALSE 0
#define TRUE 1

// int ndefs;
// int defspace = 0;
extern int ndefs, defspace;

struct scan_struct *
scan_info (char *scanname,
           char *stations)
    {
    int i, j, st, nst, ndefs_save, blockno, defno, d, bno, dno;
    int passno, nctot, nchar, retain;
    int del_list[200000];
    char subpass, *ptr, mk4_id;
    struct scan *scn;
    struct param_val p_val;
    char stkey[64][128];
    static struct scan_struct scan;
    static int first = TRUE;
    struct def *thisdef, *sourcedef, *experdef, *eopdef, *modedef, *get_def();
    static struct def_list *deflist = NULL;
    struct station_struct st_temp;
    extern struct scan slist[];
    extern struct statement *stlist;
    extern struct block blist[];
    extern int nscan, nstmt, nblock;
    extern char ovex_src[];
    extern int do_output, ovex_ver;
                                        /* Get the OVEX version number */
    if ((ovex_ver = get_version (OVEX)) == V_BAD)
        {
        msg ("Failed to get OVEX version number", 2);
        return (NULL);
        }
                                        /* For the moment, we don't support */
                                        /* the selection of subsets of stations */
/*     if (strlen (stations) > 0) */
/*         { */
/*         msg ("Station selection not yet supported by scan_info()", 2); */
/*         msg ("Continuing using all stations", 2); */
/*         } */
                                        /* get_vex() called parse_vexfile() */
                                        /* which got all the defs.  For this */
                                        /* application we also need the scans */
    if (fill_scanlist () != 0)
        {
        msg ("Failure in fill_scanlist()", 2);
        return (NULL);
        }
                                        /* Was this just a request for a list */
                                        /* of scans? */
    if (strcmp (scanname, "SCAN_LIST") == 0) return (NULL);
                                        /* Get the global stuff */
    ndefs = 0;
    if (get_global_deflist (&deflist) != 0) return (NULL);
                                        /* Check for null scan and root file */
    if (scanname[0] == '\0')
        {
        if (nscan != 1)
            {
            msg ("Multiple scans present in ovex file, but none specified", 2);
            return (NULL);
            }
        scn = slist;
        }
                                        /* Locate and retrieve scan def */
    else
        {
        for (i=0; i<nscan; i++)
            if (strcmp (slist[i].name, scanname) == 0) break;
        if (i == nscan)
            {
            msg ("Could not find scan named '%s'", 3, scanname);
            return (NULL);
            }
        scn = slist + i;
        }
                                        /* Allocate memory for station structs */
                                        /* Free memory from earlier calls */
    if (! first) free (scan.st);
    scan.st = (struct station_struct *)calloc 
                        (64, sizeof (struct station_struct));
    if (scan.st == NULL)
        {
        msg ("Memory allocation failure for station structs", 2);
        return (NULL);
        }
                                        /* Successful, set first false */
    first = FALSE;
                                        /* Initialize the structure */
    init_scan (&scan, 64);
   
/*     strcpy (scan.scan_name, scanname);  modified by rjc for mk4fit 99.8.20 */
    strcpy (scan.scan_name, scn -> name);
                                        /* Check space for delete list */
                                        /* and initialize to all deleted */
    if (nstmt >= 200000)
        {
        msg ("OVEX source file too big, more than 200000 statements", 2);
        return (NULL);
        }
    for (d=0; d<nstmt; d++) del_list[d] = TRUE;
                                        /* Retain all relevant $BLOCK statements */
    for (i=0; i<nblock; i++) 
        {
        retain = FALSE;
        if (strcmp (blist[i].name, "STATION") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "MODE") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "SCHED") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "ANTENNA") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "BBC") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "CLOCK") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "DAS") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "EOP") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "EXPER") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "FREQ") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "HEAD_POS") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "IF") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "PASS_ORDER") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "PHASE_CAL_DETECT") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "ROLL") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "SITE") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "SOURCE") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "TAPELOG_OBS") == 0) retain = TRUE;
        if (strcmp (blist[i].name, "TRACKS") == 0) retain = TRUE;
        if (retain) del_list[blist[i].stno] = FALSE;
                                        /* Retain entire GLOBAL section */
        if (strcmp (blist[i].name, "GLOBAL") == 0)
            for (j=blist[i].stno; j<=blist[i].end; j++)
                del_list[j] = FALSE;
                                        /* Retain OVEX_REV section */
        if (strcmp (blist[i].name, "OVEX_REV") == 0)
            for (j=blist[i].stno; j<=blist[i].end; j++)
                del_list[j] = FALSE;
        }
                                        /* Undelete statements in scan */
    for (d=scn->start; d<=scn->end; d++) del_list[d] = FALSE;
                                        /* Retrieve start time, and defs/refs */
    nst = 0;
    modedef = sourcedef = NULL;
    for (st=scn->start+1; st<scn->end; st++)
        {
                                        /* Statements in scans are syntactically */
                                        /* identical to primitive "parameter=" */
                                        /* statements */
        if (parse_pval (stlist[st].str, "SCHED", OVEX | ovex_ver, &p_val) != 0)
            {
            msg ("Parse error in scan '%s'", 2, scanname);
            return (NULL);
            }
                                        /* Where possible, copy the data */
                                        /* directly into scaninfo struct */
        if (strcmp (p_val.name, "start") == 0)
            memcpy (&(scan.start_time), 
                        &(p_val.dval[0].data.epochval), sizeof (struct date));
                                        /* Mode and source defs need to */
                                        /* be followed up in $MODE, $SOURCE */
                                        /* blocks.  Update delete list as well */
        else if (strcmp (p_val.name, "mode") == 0)
            {
            modedef = get_def (p_val.val[0], "MODE", &blockno, &defno);
            for (d=modedef->start; d<=modedef->end; d++) 
                del_list[d] = FALSE;
            }
        else if (strcmp (p_val.name, "source") == 0)
            {
            sourcedef = get_def (p_val.val[0], "SOURCE", &blockno, &defno);
            for (d=sourcedef->start; d<=sourcedef->end; d++) 
                del_list[d] = FALSE;
            }
                                        /* "station=" lines are fully parsed */
        else if (strcmp (p_val.name, "station") == 0)
            {
            if (nst >= 64)
                {
                msg ("Too many 'station=' lines in scan '%s'", 2, scanname);
                return (NULL);
                }
                                        /* Need the SITE def so we can find */
                                        /* the mk4 site ID for filtering */
                                        /* To do this, we process this station, */
                                        /* then "forget" the list of defs thus */
                                        /* generated by get_station_deflist() */
            strcpy (stkey[nst], p_val.val[0]);
            ndefs_save = ndefs;
            if (get_station_deflist (stkey[nst], &deflist) != 0)
                {
                msg ("Error processing station '%s' in obsvex", 2, stkey[nst]);
                return (NULL);
                }
            for (i=0; i<ndefs; i++)
                {
                if (strcmp (blist[deflist[i].blockno].name, "SITE") != 0) continue;
                if (do_site (deflist + i, &st_temp) != 0) return (NULL);
                }
            mk4_id = st_temp.mk4_site_id;
            ndefs = ndefs_save;
                                        /* If not in a supplied station list, */
                                        /* we need to skip it (and delete from output) */
            if (strlen (stations) > 0)
                if (strchr (stations, mk4_id) == NULL) 
                    {
                    del_list[st] = TRUE;
                    continue;
                    }

            scan.st[nst].start_offset = p_val.dval[1].data.realval;
            scan.st[nst].stop_offset = p_val.dval[2].data.realval;
            scan.st[nst].start_tapepos = p_val.dval[3].data.realval;
            sscanf (p_val.dval[4].data.strval, "%d%c", &passno, &subpass);
            scan.st[nst].passno = passno;
            scan.st[nst].subpass = subpass;
            scan.st[nst].drive_no = p_val.dval[6].data.intval;
            nst++;
            }
                                        /* Pick up the fourfit reftime if */
                                        /* present */
        else if (strcmp (p_val.name, "fourfit_reftime") == 0)
            memcpy (&(scan.ffit_reftime), 
                        &(p_val.dval[0].data.epochval), sizeof (struct date));
                                        /* Only 5 statement types allowed */
        else
            {
            msg ("Illegal statement in scan def", 2);
            return (NULL);
            }
        }
    scan.nst = nst;
                                        /* Check minimum requirements met */
    if ((scan.start_time.year == 0) || (modedef == NULL)
                || (sourcedef == NULL) || (nst == 0))
        {
        msg ("Incomplete scan specification for '%s'", 2, scanname);
        return (NULL);
        }
                                        /* Trim excess memory */
    scan.st = (struct station_struct *)realloc
                        (scan.st, nst * sizeof (struct station_struct));
    if (scan.st == NULL)
        {
        msg ("Memory reallocation failure for station structs", 2);
        return (NULL);
        }
                                        /* Get things found in GLOBAL */
                                        /* Find and extract the EXPER section */
    experdef = NULL;
    for (i=0; i<ndefs; i++)
        {
        blockno = deflist[i].blockno;
        if (strcmp (blist[blockno].name, "EXPER") != 0) continue;
        if (experdef != NULL)
            {
            msg ("Error, multiple EXPER defs present", 2);
            return (NULL);
            }
        experdef = blist[blockno].deflist + deflist[i].defno;
        }
    if (experdef == NULL)
        {
        msg ("EXPER def not found.", 2);
        return (NULL);
        }
    if (do_exper (experdef, &scan) != 0) return (NULL);

                                        /* Find and extract the EOP section */
    eopdef = NULL;
    for (i=0; i<ndefs; i++)
        {
        blockno = deflist[i].blockno;
        if (strcmp (blist[blockno].name, "EOP") != 0) continue;
        if (eopdef != NULL)
            {
            msg ("Error, multiple EOP defs present", 2);
            return (NULL);
            }
        eopdef = blist[blockno].deflist + deflist[i].defno;
        }
    if (eopdef == NULL)
        {
        msg ("EOP def not found.", 2);
        return (NULL);
        }
    if (do_eop (eopdef, &scan) != 0) return (NULL);
                                        /* Fill in the source portion of struct */
    do_source (sourcedef, &(scan.src));
                                        /* Loop over stations */
    ndefs_save = ndefs;
    for (i=0; i<nst; i++)
        {
                                        /* Make a list of station/mode */
                                        /* specific defs for this station */ 
                                        /* and add to GLOBAL ref'd defs */
        if (get_station_deflist (stkey[i], &deflist) != 0) return (NULL);
        if (get_mode_deflist (modedef, stkey[i], &deflist) != 0) return (NULL);
                                        /* Extract and interpret low level info */
        if (fill_station_parms (deflist, ndefs,
                                &(scan.start_time), scan.st + i) != 0) 
                {
                msg ("Error filling in station parameters", 2);
                return (NULL);
                }
                                        /* Remove all these defs from delete */
                                        /* list */
        for (j=0; j<ndefs; j++)
            {
            bno = deflist[j].blockno;
            dno = deflist[j].defno;
            thisdef = blist[bno].deflist + dno;
            for (d=thisdef->start; d<=thisdef->end; d++) del_list[d] = FALSE;
            }
                                        /* And remove the station defs too */
        thisdef = get_def (stkey[i], "STATION", &bno, &dno);
        for (d=thisdef->start; d<=thisdef->end; d++) del_list[d] = FALSE;
                                        /* Erases station/mode specific defs */
                                        /* from list, leaving only global info */
        ndefs = ndefs_save;
        }
                                        /* Process the delete list to generate */
                                        /* stripped output */
    if (do_output)
        {
        ptr = ovex_src;
        nctot = 0;
        for (i=0; i<nstmt; i++)
            {
            if (del_list[i] == TRUE) continue;
            nchar = stlist[i].end - stlist[i].start + 1;
            if ((nctot + nchar) >= 262144)
                {
                msg ("Stripped OVEX contents too large for output buffer", 2);
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
                                        
    return (&scan);
    }
