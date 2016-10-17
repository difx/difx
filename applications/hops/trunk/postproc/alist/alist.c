/****************************************************************/
/*                                                              */
/* ALIST ... a program to examine binary correlator files       */
/* and use them to generate an output A-file.  This program is  */
/* analogous to VLIST on the old HP-1000 systems, except that   */
/* it absolutely refuses to create bastardized versions of the  */
/* A-file format the way VLIST does.  If you want to examine    */
/* the data in more detail than is permitted by this format,    */
/* the recommended course of action is to invoke aedit and pop  */
/* up the relevant fringe plots.                                */
/*                                                              */
/* The input to alist is a file or directory specification,     */
/* possibly wildcarded.  If directories are used, they must be  */
/* at or below the experiment directory level in a standard     */
/* correlator data file area.  If binary data file names are    */
/* used directly, valid pathnames must be specified.            */
/*                                                              */
/* Created September 22 1992, CJL                               */
/* Make mk4 specific (only)   rjc  2010.3.30                    */
/* Made output_version reflect current reality gbc 2011.2.28    */
/****************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "fstruct.h"
#include "mk4_afio.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

char progname[6] = "alist";
int msglev = 2;
int output_version = CURRENT_VERSION;

extern int parse_cmdline (int argc, char **argv,
    fstruct **files, char *outfile);
extern int summarize_mk4fringe(struct mk4_fringe *fr, fringesum *fsumm);
extern FILE *open_output (char *outfile);

int main (int argc, char *argv[])
    {
    int i, ret, root_ok, nroot, ncorel, nfringe, opened, index;
    int root_done, fringe_done;
    char outfile[256];
    char *fname;
    struct mk4_fringe mk4fringe;
    rootsum rsumm;
    corelsum csumm;
    fringesum fsumm;
    fstruct *files;
    FILE *fp, *open_output();
                                        /* Initialize.  No fstruct entry with a NULL */
                                        /* filename is valid ... don't need to clear */
                                        /* the whole structure */
    mk4fringe.nalloc = 0;
                                        /* Check for option flags, then fill in the */
                                        /* files structure array, checking the file */
                                        /* type implied by the name in each case */

    environment();			/* Set up directories by env() */

    if (parse_cmdline (argc, argv, &files, outfile) != 0)
        {
        msg ("Fatal error interpreting command line", 2);
        syntax();
        exit(1);
        }
    if (files[0].order == -1)
        {
        msg ("No valid correlator files found/specified", 2);
        syntax();
        exit (1);
        }

                                        /* Open output file.  If the "outfile" */
                                        /* variable is empty, write to stdout */
    if ((fp = open_output (outfile)) == NULL)
        {
        msg ("Could not open output file '%s'", 2, outfile);
        exit (1);
        }

                                        /* Loop over all filenames, creating A-lines */
                                        /* as we go.  Dereference the 'order' structure */
                                        /* element to enable the sorting that has been */
                                        /* done */
    i = 0;
    root_ok = FALSE;
    nroot = ncorel = nfringe = 0;
    while ((index = files[i].order) >= 0)
        {
        fname = files[index].name;
        if (files[index].type == 0)
            {
            root_ok = root_done = FALSE;
            msg ("Mk4 type-0 file summaries unsupported as yet", 1);
            i++;
            continue;
            }

        else if (files[index].type == 1)
            {
            msg ("Mk4 type-1 file summaries unsupported as yet", 1);
            i++;
            continue;
            }

        else if (files[index].type == 2)
            {
            fringe_done = FALSE;
            if (read_mk4fringe (fname, &mk4fringe) != 0)
                msg ("Failure reading fringe file %s", 2, fname);
            else if ((ret = summarize_mk4fringe (&mk4fringe, &fsumm)) != 0)
                msg ("Failure summarizing fringe file %s", 2, fname);
            else fringe_done = TRUE;

            if (! fringe_done) { i++; continue; }

            else if (write_fsumm (&fsumm, fp) != 0)
                msg ("Failure writing fringe file summary on %s", 2, fname);
            else nfringe++;
            }
                                        /* Ignore Mk4 file types 3 and 4 */
        else if (files[index].type > 4)
            {
            msg ("Error in sort_names, bad file type, abort.", 2);
            exit(1);
            }

        i++;

        }

    i = nroot + ncorel + nfringe;
    if (i > 0)
        {
        msg ("Successfully wrote %d A-file lines to file %s", 2, i, outfile);
        msg ("comprised of %d root lines, %d corel lines and %d fringe lines",
            2, nroot, ncorel, nfringe);
        }
    else msg ("No lines written!  '%s' contains only a header", 2, outfile);

    return(0);
    }

