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

#include <stdio.h>
#include <string.h>

#include "mk4_data.h"                       /* Definitions of data structures */
//#include "print_page.h"
#include "pass_struct.h"
#include "param_struct.h"
#include "vex.h"

#include "fileset.h"
#include "write_lock_mechanism.h"

int
output (struct vex* root, struct type_pass* pass)
    {
    char fringe_name[256], *create_fname();
    char sg;
    int i, dret;
    extern int base, test_mode, do_accounting;
    extern struct mk4_fringe fringe;
    char **fplot;
    static struct type_221 *t221;
    static struct type_222 *t222;
    extern int max_seq_no;
    extern int msglev;
    extern struct type_param param;

                                        /* Generate information to create fringe plot */
                                        /* Some of this also goes into fringe file */
    if (make_plotdata (pass) != 0)
        {
        msg ("Error creating plot data", 2);
        return (1);
        }
        
        //try to get a lock on the root directory in order to write the fringe
        //this is used to signal any other possible fourfit processes in this
        //directory that we are about to create a file so we can avoid name 
        //collisions
        int lock_retval = LOCK_PROCESS_NO_PRIORITY;
        char lockfile_name[512] = {'\0'};

        if(!test_mode)
            {
            struct fileset fset;
            //wait until we are the next process allowed to write an output file
            lock_retval = wait_for_write_lock(root->ovex->filename, lockfile_name, &fset);
            //make sure we have the correct file extent number
            max_seq_no = fset.maxfile;
            }

                                        /* Figure out the correct, full pathname */
    if (create_fname (root->ovex, pass, fringe_name) != 0)
        {
        msg ("Error figuring out proper fringe filename", 2);
        return (1);
        }
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
                                            /* Actually write the output fringe file */
    if( !test_mode)
        {
        if( lock_retval == LOCK_STATUS_OK)
            {
            //kludge to get fourfit to feed the generated fringe file name 
            //(but nothing else) as a return value to a
            //a python calling script (requires passing option "-m 4")
            if(msglev==4){msg ("%s",4,fringe_name);} //iff msglev=4
            if (write_mk4fringe (&fringe, fringe_name) < 0)
                {
                //if a lock file was created, delete it now
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
                                        /* This can be ascii plot, Xwindow plot, or */
                                        /* neither */
    if (do_accounting) account ("Write output files/plots");
    dret = display_fplot (&fringe);
    if (do_accounting) account ("Wait for Godot");
    if (dret > 0) msg ("Display of fringe plot failed, continuing", 2);
                                        /* Ascii plot need special free routine */

    if (dret < 0) return (-1);
    return (0);
    }
