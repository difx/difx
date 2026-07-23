/*****************************************************/
/*                                                   */
/* output() takes care of the output functions       */
/* of fourfit.  It creates the data for the fringe   */
/* plot, optionally displays it (depending on the    */
/* program control parameters), fills in the fringe  */
/* records (type 4xxx and 5000), and writes a fully  */
/* backward-compatible fringe file to disk, properly */
/* named.                                            */
/*                                                   */
/* Created October 3 1991 by CJL                     */
/*                                                   */
/*****************************************************/

#include <errno.h>
#include <glob.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "msg.h"
#include "mk4_data.h"                   /* Definitions of data structures */
#include "mk4_dfio.h"
#include "mk4_util.h"
#include "pass_struct.h"
#include "param_struct.h"
#include "meta_struct.h"
#include "vex.h"
#include "ffio.h"
#include "fileset.h"
#include "write_lock_mechanism.h"
#include "plot_data_dir.h"

/*
 * Macros for code readability below
 */
#define MSGRETURN0(STR,ARG,ERR) do {\
    msg(STR,3,ARG);\
    fflush(stderr);return(ERR);} while(0)
#define MSGRETURN1(STR,ARG,ERR,FRE) do {\
    msg(STR,3,ARG);if(FRE)free(FRE);\
    fflush(stderr);return(ERR);} while(0)
#define MSGRETURN2(STR,ARG,ERR,FR1,FR2) do {\
    msg(STR,3,ARG);if(FR1)free(FR1);if(FR2)free(FR2);\
    fflush(stderr);return(ERR);} while(0)

/*
 * This utility creates thedir; caller is for debugging
 */
static int create_dirpath(char *thedir, char *caller)
{
    struct stat statbuf;
    int err = stat(thedir, &statbuf);
    msg("In create_dirpath(%s,%s)", 0, thedir, caller);
    if (err && errno == ENOENT) {
        err = mkdir(thedir, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);
        if (err) MSGRETURN0("Unable to create %s", thedir, 2);
        msg("Created dir %s", 0, thedir);
    }
    err = stat(thedir, &statbuf);
    if (err) {
        perror("create_dirpath-stat");
        return(1);
    }
    msg("Dir %s (now) exists", 1, thedir);
    return(0);
}

/*
 * This makes a link to the root as well as all station files
 * since fringex apparently requires them.
 */
static int link_root_and_station_files(const char *oldroot, const char *newroot)
{
    int err = symlink(oldroot, newroot);
    int gg, plen;
    char *rootid, *olddup, *slash, *newpath, *slush;
    char *pattern;
    glob_t globberish;

    /* check the link */
    if (err) MSGRETURN0("Unable to link %s", newroot, -1);
    msg("symlink(%s <- %s)", 2, oldroot, newroot);
    /*
     * beyond this point, we only care about errors if we are
     * planning to move on to fringex next, so zero return ok
     * for some of the things that might go wrong.
     */
    rootid = strrchr(newroot, '.');
    if (!rootid) MSGRETURN0("Malformed root name %s", newroot, -2);
    rootid++;

    /* this is for the dirname of original root */
    olddup = strdup(oldroot);
    if (!olddup) MSGRETURN0("No dup (old) root %s", oldroot, -3);
    slash = strrchr(olddup, '/');
    if (!slash) MSGRETURN1("Malformed dup name %s", olddup, -4, olddup);
    *slash = 0;

    /* this is for a pattern to glob station files on */
    pattern = malloc((plen = strlen(oldroot) + 12));
    if (!pattern) { perror("malloc:pattern");
        MSGRETURN1("Unable to malloc pattern bytes(%d)", plen, -4, olddup); }
    snprintf(pattern, plen, "%s/?..%s", olddup, rootid);
    msg("Globbing on %s", 1, pattern);

    /* and now do the actual globbing */
    memset(&globberish, 0, sizeof(globberish));
    switch (glob(pattern, GLOB_NOSORT, NULL, &globberish)) {
    case 0:
        msg("Globbed %d files", 1, globberish.gl_pathc);
        break;
    case GLOB_NOSPACE:
        msg("Running out of glob memory", 3);
        break;
    case GLOB_ABORTED:
        msg("Glob aborted, not all station files linked", 3);
        break;
    case GLOB_NOMATCH:
    default:
        msg("No station files linked", 3);
        break;
    }
    free(pattern);

    /* create the station file links */
    newpath = strdup(newroot);
    if (!newpath) MSGRETURN0("Unable to dup (new) root %s", newroot, -6);
    for (gg = 0; gg < globberish.gl_pathc; gg++) {
        slash = strrchr(newpath, '/');
        slush = strrchr(globberish.gl_pathv[gg], '/');
        sprintf(slash+1, "%s", slush+1);
        symlink(globberish.gl_pathv[gg], newpath);
        msg("symlink(%s <- %s)", 2, globberish.gl_pathv[gg], newpath);
    }
    msg("Linked %d files plus 'origin' file", 2, globberish.gl_pathc + 1);

    /* cleanup */
    free(olddup);
    free(newpath);
    globfree(&globberish);
    return(0);
}

/*
 * Create a proper directory hierarchy for a copy of the
 * original root.  Also place a marker file indicating where
 * it all came from.  (The locking machinery depends on the root
 * file being present.)
 *
 * On entry, and through most of this function, newrootnameptr points
 * to the existing root; on exit, it points to the full path to the copy.
 */
static int origin(char *outdir, char **newrootnameptr)
{
    struct stat statbuf;
    int err, nlen;
    char *newroot, *rootptr, *scanptr, *expnptr;
    msg("In origin(\n\t %s,\n\t %s)", 1, outdir, *newrootnameptr);
    /* .../expn/scan/root */
    rootptr = strrchr(*newrootnameptr, '/');
    if (!rootptr)
        MSGRETURN1("No / in root name\n\t %s", *newrootnameptr, 3, NULL);
    msg("RootPtr is %s", 1, rootptr);
    *rootptr = 0;
    scanptr = strrchr(*newrootnameptr, '/');
    *rootptr = '/';
    if (!scanptr)
        MSGRETURN1("No scandir in root name\n\t %s", *newrootnameptr, 4, NULL);
    msg("ScanPtr is %s", 1, scanptr);
    *scanptr = 0;
    expnptr = strrchr(*newrootnameptr, '/');
    *scanptr = '/';
    if (!expnptr)
        MSGRETURN1("No expno in root name\n\t %s", *newrootnameptr, 5, NULL);
    msg("ExpnPtr is %s", 1, expnptr);
    /* ok, now can create full newroot name */
    nlen = strlen(outdir) + strlen(expnptr) + 1;
    newroot = malloc(nlen);
    snprintf(newroot, nlen, "%s%s", outdir, expnptr);
    msg("Creating new root\n\t %s", 1, newroot);
    /* need to make sure we don't overwrite the actual root */
    if (!strcmp(newroot, *newrootnameptr))
        MSGRETURN1("Aborting overwrite of\n\t %s", newroot, 6, newroot);

    /* now need to create the hierarchy -- these tests should pass */
    rootptr = strrchr(newroot, '/');
    if (!rootptr) MSGRETURN1("No / in root name\n\t %s", newroot, 7, newroot);
    msg("RootPtr is %s", 1, rootptr);
    *rootptr = 0;
    scanptr = strrchr(newroot, '/');
    if (!scanptr)
        MSGRETURN1("No scandir in root name\n\t %s", newroot, 8, newroot);
    msg("ScanPtr is %s", 1, scanptr);
    *scanptr = 0;
    expnptr = strrchr(newroot, '/');
    if (!expnptr)
        MSGRETURN1("No expno in root name\n\t %s", newroot, 9, newroot);
    msg("ExpnPtr is %s", 1, expnptr);
    err = create_dirpath(newroot, "expn");
    if (err) MSGRETURN1("Cannot create\n\t %s", newroot, 10, newroot);
    *scanptr = '/';
    err = create_dirpath(newroot, "scan");
    if (err) MSGRETURN1("Cannot create\n\t %s", newroot, 11, newroot);
    *rootptr = '/';

    err = stat(newroot, &statbuf);
    if (err && (errno == ENOENT)) { /* not found, so create it */
        int len = strlen(newroot) + 10;
        char *nrorig = malloc(len);
        FILE *nrfp;
        if (!nrorig) { perror("malloc");
            MSGRETURN1("Issue with this\n\t %s", nrorig, 12, newroot); }
        snprintf(nrorig, len, "%s.origin", newroot);
        if (!(nrfp = fopen(nrorig, "w")))
            MSGRETURN2("Issue with root\n\t %s", nrorig, 13, newroot, nrorig);
        /* write out the original root path */
        fprintf(nrfp, "Original Root: %s\n", *newrootnameptr);
        fclose(nrfp);
        msg("Created origin root\n\t %s", 1, nrorig);
        free(nrorig);
        /* now create symbolic link to the original root */
        // err = symlink(*newrootnameptr, newroot);
        /* and for fringex, all station files as well */
        if (link_root_and_station_files(*newrootnameptr, newroot))
            MSGRETURN1("Link issues\n\t %s", newroot, 14, newroot);
    } else if (err) {   /* some other error, which is death */
        MSGRETURN1("Root present, but unusable\n\t %s", newroot, 15, newroot);
    }

    /* final check that we created it */
    err = stat(newroot, &statbuf);
    if (err) {
        perror("stat-newroot");
        MSGRETURN1("Unable to access origin root\n\t %s", newroot, 16, newroot);
    }
    /* make it so -- some one else must now eventually free newroot memory */
    *newrootnameptr = newroot;
    return(0);
}

static int ensure(char *outdir, char **newrootnameptr)
{
    int err;
    msg("Arranging for origin root:\n%s:    %s.origin\n%s: in %s", 1,
        progname, *newrootnameptr, progname, outdir);
    err = create_dirpath(outdir, "ensure");
    if (err) MSGRETURN1("Unable to create fringeout_dir, %d", err, 3, NULL);
    return(origin(outdir, newrootnameptr));
}

int
output (struct vex* root, struct type_pass* pass)
    {
    char fringe_name[256];
    char sg;
    int i, dret;
    extern int base, test_mode, do_accounting;
    extern struct mk4_fringe fringe;
    char **fplot;
    int the_seq_no;
    static struct type_221 *t221;
    static struct type_222 *t222;
    extern int msglev;
    extern struct type_param param;
    extern struct type_status status;
    extern struct type_meta meta;
    extern struct type_plot plot;

    /* honestly, these should be in a .h */
    extern int make_plotdata(struct type_pass*);
    extern int create_fname (char *,
        struct type_pass*, int seq, char fname[]);
    extern int display_fplot (struct mk4_fringe*);
    extern int make_postplot (struct scan_struct*,
                       struct type_pass*,
                       char*,
                       struct type_221**);
    extern void est_pc_manual(int, char*, struct type_pass*);

    // for locking, see below and include/write_lock_mechanism.h
    int lock_retval = LOCK_PROCESS_NO_PRIORITY;
    char lockfile_name[512] = {'\0'};
    char *fringeoutdir = pass->control.fringeout_dir, *newrootname = 0;

    if (pass->control.fringeout_dir[0])
        {
        int err;
        newrootname = root->ovex->filename;
        /* if there is some issue, we soldier on in the normal place */
        if ((err = ensure(fringeoutdir, &newrootname))) newrootname = 0;
        if (err) msg ("Error return %d from ensure()", 3, err);
        else msg ("Fringe(s) with %s", 2, newrootname);
        }

                                /* Generate information to create fringe plot */
                                /* Some of this also goes into fringe file */
    if (make_plotdata (pass) != 0)
        {
        if (newrootname) free(newrootname);
        msg ("Error creating plot data", 2);
        return (1);
        }
        
    //try to get a lock on the root directory in order to write the fringe
    //this is used to signal any other possible fourfit processes in this
    //directory that we are about to create a file so we can avoid name 
    //collisions.  The lock persists from point of acqusition until the
    //eventual call to write_mk4fringe() below.
    // FIXME: should worry about stale locks if ^C is hit: fix is to
    // install a signal handler here...and remove it by create_fname.
    if(!test_mode)
        {
        struct fileset fset;
        memset(&fset, 0, sizeof(struct fileset));
        //wait until we are the next process allowed to write an output file
        lock_retval = wait_for_write_lock(
            (newrootname) ? newrootname : root->ovex->filename,
            lockfile_name, &fset);
        //this number when ++ is the next unused seq number
        the_seq_no = pass->control.mod4numbering
                   ? fset.mxfiles[pass->pol]+(4-1)
                   : fset.maxfile;
        }
    else
        {
        // in test mode, nothing should be written, so the number is moot.
        the_seq_no = -1;
        }

    /* create_fname() will put the next seq number into the fringe name */
    the_seq_no++;
                                    /* Figure out the correct, full pathname */
    if (create_fname ((newrootname) ? newrootname : root->ovex->filename,
        pass, the_seq_no, fringe_name) != 0)
        {
        if (newrootname) free(newrootname);
        msg ("Error figuring out proper fringe filename", 2);
        return (1);
        }
    free(newrootname);              /* done with this by this point */
                                    /* Fill in fringe file structure */
    if (fill_fringe_info (root, pass, fringe_name) != 0)
        {
        msg ("Error filling fringe records", 2);
        return (1);
        }
        
    // provide suggestions
    if (param.est_pc_manual)
        est_pc_manual(param.est_pc_manual, root->ovex->filename, pass);

    if (make_postplot (root->ovex, pass, fringe_name, &t221) != 0)
        {
        msg ("Error creating postscript plot", 2);
        return (1);
        }
    fringe.t221 = t221;
    fringe.t221->ps_length = strlen (fringe.t221->pplot);
                                        /* Record the memory allocation */
    fringe.allocated[fringe.nalloc] = fringe.t221;
    fringe.nalloc += 1;
    
                                       /* Fill in the control file record */
                                       /* if desired */
    fringe.t222 = NULL;
    if(param.gen_cf_record)
        {
        if (fill_222 (&param, &t222) != 0)
            {
            msg ("Error filling control record", 2);
            return (1);
            }
        
        fringe.t222 = t222;
                                        /* Record the memory allocation */
        fringe.allocated[fringe.nalloc] = fringe.t222;
        fringe.nalloc += 1;
        }
                                        /* possibly dump the data */
    DUMP_PLOT_DATA2DIR(root, pass, &param, &status, &meta, &plot, &fringe);
                                        /* Actually write output fringe file */
    if( !test_mode)
        {
        if( lock_retval == LOCK_STATUS_OK)
            {
            //kludge to get fourfit to feed the generated fringe file name 
            //(but nothing else) as a return value to a
            //a python calling script (requires passing option "-m 4"); see
            //e.g. chops/source/python_src/hopstest_module/hopstestb/hopstestb.py
            //around line 74 in the FourFitThread class.
            if(msglev==4){msg ("%s",4,fringe_name);} //iff msglev=4
            if (write_mk4fringe (&fringe, fringe_name) < 0)
                {
                // pause 50ms, if a lock file was created, delete it now
                usleep(50000); remove_lockfile();
                msg ("Error writing fringe file", 2);
                return (1);
                }
            //if a lock file was created, delete it now
            usleep(50000); remove_lockfile();
            }
        else
            {
            msg ("Error getting write lock on directory.", 2);
            return (1);
            }
        }

    if (do_accounting) account ("Write output files/plots");
    dret = display_fplot (&fringe);
    if (do_accounting) account ("Wait for Godot");
    if (dret > 0) msg ("Display of fringe plot failed, continuing", 2);
    if (dret == 0) msg ("Display of fringe plot was fine, continuing", 1);
    if (dret < 0) return (-1);
    return (0);
    }
