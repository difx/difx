/******************************************************************************/
/*                                                                            */
/* MK4FIT ... a program to perform fringe searching for MkIV VLBI data        */
/*                                                                            */
/* This is the main routine for the program, is written for generic UNIX      */
/* systems, and uses X windows for graphical output.  In addition, at some    */
/* stage, it will employ the Caltech PGPLOT graphics package for output to    */
/* arbitrary devices.  Detailed descriptions of the program architecture,     */
/* function, and use can be found in the standard documentation directories.  */
/*                                                                            */
/* This version of the program is a Mk4 version of fourfit.  It begins life   */
/* as a standard version of fourfit based on Mk3 root files, but which is     */
/* capable of reading and fringe-fitting Mk4 type-1 files created using the   */
/* utility "mk3tomk4".                                                        */
/*                                                                            */
/* main routine ... original version  CJL June 19 1991                        */
/* Simplified by farming out lots of fiddly details to subroutines            */
/* CJL, April 11 1992                                                         */
/* Converted for Mk4 use starting in February 1997 by CJL                     */
/* Maintained by RJC until his retirement 2019                                */
/* Maintained also by GBC starting May 2010 until his retirement 2024         */
/* Maintained by JB from Nov 2016 ..                                          */
/*                                                                            */
/******************************************************************************/

#include <stdio.h>
#include <alloca.h>
#include <stdlib.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>

#include "vex.h"                        /* Needed for VEX root file format */
#include "msg.h"
#include "mk4_data.h"                   /* Definitions of Mk4 data structs */
#include "mk4_dfio.h"
#include "mk4_util.h"
#include "control.h"                    /* Definition of control structure */
#include "param_struct.h"               /* Def'n of 2 structs (param&status) */
#include "pass_struct.h"                /* Pass-specific parameters */
#include "meta_struct.h"                /* Various pass observables */
#include "fstruct.h"                    /* for file specifiers on cmd line */
#include "refringe.h"                   /* Specific to -r option */
#include "fileset.h"
#include "write_lock_mechanism.h"
#include "fourfit_signal_handler.h"
#include "ffsearch.h"
#include "ffcore.h"

struct type_param param;
struct type_status status;              /* External structure declarations */
struct mk4_fringe fringe;
struct mk4_corel cdata;
struct mk4_sdata sdata[MAXSTATIONS];
struct type_plot plot;
struct type_meta meta;

struct c_block *cb_head;

int baseline, base, ncorel_rec, lo_offset;
int test_mode = FALSE;
int write_xpower = FALSE;
int do_accounting = FALSE;
int do_estimation = FALSE;
int refringe = FALSE;
int ap_per_seg = 0;
int reftime_offset = 0;

//global variables provided for signal handler clean up of lock files
lockfile_data_struct global_lockfile_data;

char *pexec;                            // ptr to progam executable name
char version_no[] = FF_VER_NO;          // PACKAGE_VERSION from Makefile
// originally local to this function as char version_no[4]:
// CJL's last version number was 2.2; RJC bumped to 3.0 2001.5.25
// to 3.3 2011.04.1 onwards history maintained in README.vendor.txt
// as the version bumped with each distribution (mostly to DiFX).

#define MAXPASS 32
#define FALSE 0
#define TRUE 1

int main (int argc, char** argv)
    {
    struct vex root;
    struct freq_corel corel[MAXFREQ];
    /* msg ("MAXFREQ == %d\n", 0, MAXFREQ); */
    struct type_pass *pass;
    char *inputname, processmsg[512];
    char oldroot[256], rootname[256];
    int i, j, k, npass, fringes_todo, fringes_done;
    int nroots, fno, fs_ret;
    int user_ok = TRUE;
    fstruct *files, *fs;
    struct fileset fset;
    bsgstruct *base_sgrp;
    void *entry_alloc, *exit_alloc;

    set_progname(FF_PROGNAME);
    set_msglev(2);
    clear_global_lockfile_data();    //init lockfile data struct
    memset(&fset, 0, sizeof(struct fileset));
                                    /* set up signal action */
    struct sigaction handler_action;
    handler_action.sa_handler = &fourfit_signal_handler;
    handler_action.sa_flags = SA_RESTART;
    sigfillset(&handler_action.sa_mask);
                                    /* register the signal handler */
    fourfit_register_signal_handler(&handler_action);

    account ("!BEGIN");             /* Start accounting */
    environment();                  /* Get standard environment settings */
    if (argc == 1)                  /* Trap empty argument list */
        {
        syntax("$HeadURL$");
        return (1);
        }
    pexec = argv[0];                // point to executable name
                                    /* Initialize IO library allocation */
    entry_alloc = malloc(8);
    cdata.nalloc = 0;
    fringe.nalloc = 0;
    for (i=0; i<MAXSTATIONS; i++)
        sdata[i].nalloc = 0;
                                    /* Initialize main array for memory alloc */
                                    // corel is cleared within set_pointers()
    for (i=0; i<MAXFREQ; i++)
        corel[i].data_alloc = FALSE;
                                    /* More initialization */
    msg ("fourfit calling set_defaults...",0);
    if (set_defaults() != 0) exit (1);
                                    /* Interpret and act on any command */
                                    /* line flags, and generate a list of */
                                    /* type root files to be fringe searched, */
                                    /* based on possibly wildcarded file */
                                    /* or directory specifications, or an */
                                    /* A-file list for refringing */
    msg ("fourfit calling parse_cmdline...",0);
    if (parse_cmdline (argc, argv, &files, &base_sgrp, &param) != 0)
        {
        msg ("Fatal error interpreting command line arguments", 2);
        exit(1);
        }
    if (msglev <= 1)
        {
        for (i = 0; files[i].order >= 0; i++)
            msg("File[%d] order %d is %s",1,i,files[i].order,files[i].name);
        }
    if (do_accounting) account ("Interpret arguments");
                                    /* Main program loop ... fringe search */
                                    /* all selected files, one by one */
                                    /* All arguments are handled by the two */
                                    /* major data structures */
    i = nroots = fringes_todo = fringes_done = 0;
    *root.filename = 0;
    msg ("files[%d].order = %d",1, i, files[i].order);
    while (files[i].order >= 0 && user_ok)
        {
        msg ("(Starting loop on files)", 0);
        inputname = files[i++].name;
        msg("Working inputname files[%d] %s",1,i-1, inputname);
        snprintf(processmsg, sizeof(processmsg)-1,
            "(The above errors (if any) occurred while processing\n"
            "%s:   %s\n"
            "%s: the top-level resolution is as follows...)",
                FF_PROGNAME, inputname, FF_PROGNAME);
                                    /* Performs sanity check on req'd file */
                                    /* and reports internally.  Allows for */
                                    /* fringe_all=false, among other things */
                                    /* Fills in absolute pathname of root */
                                    /* we need to read */
        if (get_abs_path (inputname, rootname) != 0)
            {
            msg ("%sUnable to find abspath for %s, skipping", 2, rootname);
            msg(processmsg, 2);
            continue;
            }
        if (get_vex (rootname, OVEX | EVEX | IVEX | LVEX, "", &root) != 0)
            {
            msg ("%sError reading root for file %s, skipping", 2, inputname);
            msg(processmsg, 2);
            continue;
            }
        nroots++;
        if (do_accounting) account ("Read root files");
                                    /* copy in root filename for later use */
        strncpy (root.ovex->filename, rootname, 256);
                                    /* Record the accumulation period - the */
                                    /* only information needed from evex */
        param.acc_period = root.evex->ap_length;
        param.speedup = root.evex->speedup_factor;
                                    /* Find all files belonging to this root */
        if (get_fileset (rootname, &fset) != 0)
            {
            msg ("Error getting fileset of '%s'", 2, rootname);
            msg(processmsg, 2);
            continue;
            }
                                    /* Read in all the type-3 files */
        if (read_sdata (&fset, sdata) != 0)
            {
            msg ("Error reading in the sdata files for '%s'", 2, rootname);
            msg(processmsg, 2);
            continue;
            }
        msg ("Successfully read station data for %s", 0, rootname);
                                    /* Now loop over all baselines in this */
                                    /* root.  Baseline filtering of data is */
                                    /* taken care of in get_corel_data */
                                    /* and, if refringing, in check_rflist */
        fno = -1;
        while (fset.file[++fno].type > 0 && user_ok)
            {
            fs = fset.file + fno;
            msg ("Encountered type %d file[%d]:  %s", 1, fs->type, fno, fs->name);
                                    /* Interested only in type 1 files */
            if (fs->type != 1) continue;
                                    /* If this is a refringe, proceed only */
                                    /* if this baseline is in the list */
                                    /* rf_fglist is list of frequency */
                                    /* groups requested */
            param.rf_fglist = NULL;
            if (refringe)
                if ((param.rf_fglist =
                        check_rflist (fs->baseline, i-1, base_sgrp)) == NULL)
                    continue;
                                    /* This reads the relevant corel file */
                                    /* non-zero return generally means this */
                                    /* baseline isn't needed--not an error */
                                    /* (it looks at control information) */
            if (get_corel_data (fs, root.ovex, root.filename, &cdata) != 0)
                {
                msg ("Unable to get correlation data for %s/%s", 1,
                    inputname, fs->name);
                msg(processmsg, 1);
                continue;
                }
            if (do_accounting) account ("Read data files");
                                    /* Put data in useful form */
                                    /* Also, interpolate sdata info */
            msg ("Organizing data for file %s", 0, inputname);
            if (organize_data (&cdata, root.ovex, root.ivex,
                sdata, corel, &param, &status, cb_head) != 0)
                {
                msg ("Error organizing data for file %s, skipping", 2,
                    inputname);
                msg(processmsg, 2);
                continue;
                }
            if (do_accounting) account ("Organize data");
                                    /* Work out multiple passes through data */
                                    /* Put pass-specific params in elements */
                                    /* of the pass array */
            if (make_passes (root.ovex, corel, &param, &pass, &npass) != 0)
                {
                msg ("Error on fringe passes setup for %s, %2s, skipping", 2,
                         inputname, fs->baseline);
                msg(processmsg, 2);
                continue;
                }
            msg ("Proceed to %d passes", 1, npass);
            if (do_accounting) account ("Make passes");
                                    /* Now do the actual fringe searching. */
                                    /* Loop over all passes, accumulating */
                                    /* errors in ret.  Error reporting is */
                                    /* internal to fringe_search() */
            for (k=0; k<npass; k++)
                {
                fringes_todo++;
                if (fringes_todo == 1 && do_estimation) fs_ret = -3;
                else fs_ret = fringe_search (&root, pass + k);
                if (fs_ret < 0) break;
                fringes_done++;
                msg("Completed %d/%d %d/%d", 1, k, npass,
                    fringes_done, fringes_todo);
                }

            if (fs_ret < 0)         /* quit request */
                {
                /* avoiding num_ap<0 crash: Hotaka 9/28/2017 */
                if (pass->num_ap < 0)
                    {
                        msg ("stop_offset < start_offset !!", 2);
                        msg ("Skipping %s/%s and continuing", 2,
                            inputname, fs->name);
                        msg (processmsg, 2);
                    }
                else if (fs_ret == -2)
                    {
                        msg ("quitting by request", 1);
                        user_ok = FALSE;
                    }
                else if (fs_ret == -3)
                    {
                        msg ("estimation enabled--will stop after one pass",3);
                    }
                else
                    {   /* still try to continue with next fringe */
                        msg ("(fringe fail rv %d, %d/%d)", 3,
                            fs_ret, fringes_todo, fringes_done);
                        msg ("Failed to find fringe on", 2);
                        msg ("%s/%s (pol %s) and the user should ask why.", 2,
                            inputname, fs->name,
                            (0 <= pass[k].pol && pass[k].pol <= 3)
                                ? polab[pass[k].pol] : "??");
                        msg (processmsg, 2);
                        msg ("continuing", 2);
                        // continue and pray
                    }
                }                   /* end of fs_ret<0 processing */
            else
                msg("finished %d passes", 1, npass);
            }                       /* Move to next file in fileset */
        }                           /* End of filename loop */
                                    /* Complete accounting and exit */
    if (do_accounting) account ("Report results");
    if (do_accounting) account ("!REPORT");
    if (do_estimation) report_wallclock(npass, fringes_todo);
    free(exit_alloc); exit_alloc = malloc(8);
    free(entry_alloc);
    free(exit_alloc);
    msg("Entry alloc %p and Exit alloc %d diff by %ld bytes", 1,
        entry_alloc, exit_alloc, (exit_alloc - entry_alloc));

    // FIXME: pass data is allocated, but never freed.
    // see static int pass_alloc = FALSE; in make_passes()

    //free up control buffers
    free(param.control_file_buff);
    free(param.set_string_buff);
    if (!(fringes_todo == fringes_done || !user_ok))
        msg("Missed %d fringes (%s)",3,
            fringes_todo - fringes_done, !user_ok ? "q typed" : "real error");
    return( (fringes_todo == fringes_done || !user_ok) ? 0 : 1);
    }
/* eof */
