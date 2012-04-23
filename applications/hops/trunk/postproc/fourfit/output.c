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
#include "vex.h"

int
output (root, pass)
struct vex *root;
struct type_pass *pass;
    {
    char fringe_name[256], *create_fname();
    char sg;
    int i, dret;
    extern int base, test_mode, do_accounting;
    extern struct mk4_fringe fringe;
    char **fplot;
    static struct type_221 *t221;

                                        /* Generate information to create fringe plot */
                                        /* Some of this also goes into fringe file */
    if (make_plotdata (pass) != 0)
        {
        msg ("Error creating plot data", 2);
        return (1);
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
                                        /* Actually write the output fringe file */
    if (! test_mode)
        if (write_mk4fringe (&fringe, fringe_name) < 0)
            {
            msg ("Error writing fringe file", 2);
            return (1);
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
