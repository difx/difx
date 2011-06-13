/************************************************************************/
/*                                                                      */
/* Constructs and writes a PS type file on disk, using the data in      */
/* in memory, in conjunction with a cc file which must have             */
/* previously been read into the sf memory structure.                   */
/* This routine is heavily derivative of the psplot code.  The only     */
/* real difference is the mode of display, plus some summary data       */
/*                                                                      */
/*      Inputs:         fname           output file name                */
/*                      ccfile (extern) filled ccfile structure         */
/*                                                                      */
/*      Output:                         Complete PS file on disk        */
/*                                      return value 0 = OK, 1 = bad    */
/*                                                                      */
/* Created 2 May 1995 by CJL                                            */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#include "aedata.h"
#include "vex.h"
#include "psplot.h"
#include "summary.h"

int
psfile4 (data, fname, mode)
esum *data;
char *fname;
int mode;
    {
    int i, s, j, ch, len, ret, nst, expno;
    char vexname[128], stnlist[32], global_fglist[10], scan_fglist[10];
    char st_id, fg, cmdname[7];
    struct scan_struct *scn, *scan_info();
    struct psplot_scantime *psscan;
    struct stat statbuf;
    static struct ps_array psarray;
    extern int rmdup, data_version, nscan;
    extern struct datasumm fsumm;
    extern struct scan slist[];
    extern char datadir[];
                                        /* Which command is this? */
    if (mode == 0) strcpy (cmdname, "psfile");
    else if (mode == 1) strcpy (cmdname, "reproc");
    else
        {
        msg ("Bad mode argument in psfile4()", 2);
        return (1);
        }
                                        /* Test for failure at every step */
    ret = 1;
    if (! rmdup)
        msg ("You must remove duplicates before attempting %s", 2, cmdname);
    else if (summ_data (data, STANDARD) != 0)
        msg ("Error summarizing data for %s", 2, cmdname);
    else if (data_version == 0)
        msg ("Your data are of mixed version numbers, cannot %s", 2, cmdname);
    else if (fsumm.nexp > 1)
        {
        msg ("Multiple experiments present ... please edit your", 2);
        msg ("dataset down to a single experiment before using %s", 2, cmdname);
        }
    else ret = 0;

    if (ret != 0) return (ret);

    expno = fsumm.experiments[0];
                                        /* Read in the vex file */
                                        /* associated with the data in memory */
    sprintf (vexname, "/correlator/data/%04d/%04d.ovex", expno, expno);
    if (stat (vexname, &statbuf) != 0)
	    {
        msg ("Could not find file '%s'", 3, vexname);
        printf ("Enter full pathname of ovex file: ");
        fgets (vexname, 128, stdin);
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
                                        /* And check up to 16 channels for */
                                        /* the frequency groups */
            for (ch=0; ch<16; ch++)
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
                                        /* Fill up the ps array with data */

                                        /* First, set up the array itself by */
                                        /* allocating memory and tagging */
                                        /* baseline/scantime axes */
    ret = 1;
    if (make_psarray4 (&psarray) != 0)
        msg ("Could not make ps array, quitting %s", 2, cmdname);

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
                                        /* Success so far */
    else ret = 0;

    if (ret != 0) return (ret);
                                        /* Summarize the psarray, and write */
                                        /* it to a disk file */
    if (mode == 0)
        {
        if (write_psfile (&psarray, fname, 1) != 0)
            {
            msg ("Error summarizing data and writing PS file", 2);
            ret = 1;
            }
        }
                                        /* Locate unprocessed data, fake up A-file, */
                                        /* and write it to a disk file */
    else if (mode == 1)
        {
        if (write_reproc (&psarray, fname) != 0)
            {
            msg ("Error writing reprocessing list", 2);
            ret = 1;
            }
        }


    ps_free (&psarray);
    return (ret);
    }
