/************************************************************************/
/*                                                                      */
/* Takes the data in memory plus the name of an OVEX file on disk       */
/* and constructs a 2-D array of quality codes.  This is then displayed */
/* in an Xwindow PGPLOT window as a (possibly multi-page) colour-coded  */
/* matrix.  Interactive cursor operations are then invoked for data     */
/* perusal, editing, and fringe plot popups.                            */
/*                                                                      */
/*      Inputs:         sf (extern)     filled Sched structure          */
/*                                                                      */
/*      Output:                         return value 0 = OK, 1 = bad    */
/*                                                                      */
/* Created for Mk4, borrowed from psfile4() 2 Feb 2001 by CJL           */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include "psplot.h"
#include "aedata.h"
#include "vex.h"
#include "summary.h"
#include "aedit.h"
#include "mk4_vex.h"

int psplot4 (esum *data)
    {
    int i, s, j, nst, ch, ret, len, expno;
    char fg, st_id, vexname[128];
    struct scan_struct *scn, *scan_info();
    struct psplot_scantime *psscan;
    struct stat statbuf;
    static struct ps_array psarray;
    extern int data_version, nscan;
    extern struct datasumm fsumm;
    extern struct scan slist[];
    extern char datadir[];

    expno = fsumm.experiments[0];
                                        /* Read in the vex file */
                                        /* associated with the data in memory */
    sprintf (vexname, "%s/%04d/%04d.ovex", datadir, expno, expno);
    if (stat (vexname, &statbuf) != 0)
	    {
	    msg ("Could not find file '%s'", 3, vexname);
            msg ("(You can make one from vex.obs with hops_vex2ovex.py)", 3);
	    printf ("Enter full pathname of ovex file: ");
	    if (fgets (vexname, 128, stdin)) return(1);
	    len = strlen (vexname);
	    if (vexname[len-1] == '\n') vexname[len-1] = '\0';
	    }
    if (parse_vexfile (vexname) != 0)
        {
        msg ("Could not read vex file for experiment %d", 2, expno);
        return (1);
        }
                                        /* Now need to gather basic information */
                                        /* about each scan.  Start by generating */
                                        /* list of scans */
    scan_info ("SCAN_LIST", "");
                                        /* Make space for scan times names and stations */
    psarray.time = 
        (struct psplot_scantime *)calloc (nscan, sizeof (struct psplot_scantime));
    if (psarray.time == NULL)
        {
        msg ("Memory allocation failure in psfile4()", 2);
        return (1);
        }
                                        /* Init */
    for (i=0; i<MAXBANDS; i++) psarray.subgroups[i] = '\0';
    nst = 0;
    for (i=0; i<MAXSTEXP; i++) psarray.stnlist[i] = '\0';
                                        /* Loop over scans and parse in detail, */
                                        /* Storing enough to do psplot and psfile job */
    for (i=0; i<nscan; i++)
        {
                                        /* Init info for this scan */
        printf ("Parsing scan %d of %d\r", i, nscan); fflush (stdout);
        psscan = psarray.time + i;
        for (s=0; s<MAXSTEXP; s++) 
            {
            psscan->stations[s].stn = '\0';
            for (j=0; j<MAXBANDS; j++) psscan->stations[s].fglist[j] = '\0';
            psscan->stations[s].minus = FALSE;
            }
                                        /* Parse details of scan */
        scn = scan_info (slist[i].name, "");
                                        /* Store scan name */
        strcpy (psscan->scan_name, scn->scan_name);
                                        /* Now loop over stations in this scan */
        psscan->nst = scn->nst;
        for (s=0; s<scn->nst; s++)
            {
            st_id = scn->st[s].mk4_site_id;
                                        /* Add to global list of stations */
            if (! strchr (psarray.stnlist, st_id))
                {
                psarray.stnlist[nst] = st_id;
                nst++;
                }
                                        /* Store list of stations in scan */
            psscan->stations[s].stn = st_id;
                                        /* Has it been minus'ed? */
            if (scn->st[s].drive_no < 0) psscan->stations[s].minus = TRUE;
                                        /* And check up to MAXFREQ channels for */
                                        /* the frequency groups */
            for (ch=0; ch<MAXFREQ; ch++)
                {
                if (scn->st[s].channels[ch].chan_name[0] == '\0') continue;
                fg = scn->st[s].channels[ch].chan_name[0];
                                        /* Append to group list for scan */
                if (strchr (psscan->stations[s].fglist, fg) == NULL)
                    {
                    len = strlen (psscan->stations[s].fglist);
                    if (len >= 9)
                        {
                        msg ("Too many frequency groups in scan %s", 2, slist[i].name);
                        return (-1);
                        }
                    psscan->stations[s].fglist[len] = fg;
                    }
                                        /* Append to global group list */
                                        /* Keep old MkIII "subgroup" terminology */
                                        /* to avoid excessive code duplication */
                if (strchr (psarray.subgroups, fg) == NULL)
                    {
                    len = strlen (psarray.subgroups);
                    if (len >= 9)
                        {
                        msg ("Too many frequency groups in ovex file '%s'" ,2, scn->filename);
                        return (-1);
                        }
                    psarray.subgroups[len] = fg;
                    }
                }
            }
        }
        printf ("\r"); fflush (stdout);

                                        /* Fill up the ps array with data */

                                        /* First, set up the array itself by */
                                        /* allocating memory and tagging */
                                        /* baseline/scantime axes */
    ret = 1;
    if (make_psarray4 (&psarray) != 0)
        msg ("Could not make ps array, quitting psplot", 2);

                                        /* Initialize it with minus'ed and */
                                        /* unprocessed codes */
    else if (psplot_defaults4 (&psarray) != 0)
        msg ("Problem filling ps array with default values", 2);

                                        /* Figure out what data array */
                                        /* indices go with what psarray elements */
    else if (get_ps_indices4 (data, &psarray) != 0)
        msg ("Error filling ps array with data array indices", 2);

                                        /* Fill in qcode indices */
    else if (set_pscodes (data, &psarray) != 0)
        msg ("Error converting data indices to qcodes in set_pscodes()", 2);
					                    /* Pagination, cell sizes and the like */
    else if (set_psparam (&psarray) != 0)
	    msg ("Error figuring out psplot plotting parameters", 2);
                                        /* Success so far */
    else ret = 0;

    if (ret != 0) return (ret);

                                        /* Open device, draw basic layout */
    if (setup_psplot (&psarray) != 0)
        {
        msg ("Error in setup_psplot()", 2);
        return (1);
        }
                                        /* Display data on screen */
    if (display_psdata (&psarray) != 0)
        {
        msg ("Error attempting to display data on screen", 2);
        cleanup_psplot (&psarray);
        return (1);
        }
                                        /* start cursor loop */
    if (run_pscursor (&psarray, data) != 0)
        {
        msg ("Problem with cursor operations", 2);
        cleanup_psplot (&psarray);
        return (1);
        }
                                        /* Take action on results, if any */
    if (psarray.ntagged > 0) pstag_process (&psarray, data);

                                        /* Close down operations */
    cleanup_psplot (&psarray);

    return (0);
    }
