/****************************************************************/
/*                                                              */
/* A simple little program to access and display fringe plots   */
/* attached to type fringe files in a standard UNIX data area.  */
/* The user can specify either -x or -h flags, which result in  */
/* X-window on-screen display or direct hardcopy.  It's like    */
/* a faster, friendlier, more interactive (in -x mode) version  */
/* of the old HP-1000 FRNGP program.                            */
/*                                                              */
/* Created July 8 1993, CJL                                     */
/* remove mk3 code                 2010.6.8  rjc                */
/* revised as with fourfit         2011.11.15 gbc               */
/****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mk4_data.h"
#include "fstruct.h"
#include "fplot.h"

#ifdef P_tmpdir
# define P_tmpdir "/tmp"
#endif /* P_tmpdir */

char progname[6] = "fplot";
int msglev = 2;

main (int argc, char* argv[])
    {
    int i, display, ret, mk4, size, quit, prompt;
    char c, cmd[128], pmt[128], *file_name;
    struct mk4_fringe fringe4;
    fstruct *files;
    FILE *fp;
    static char ps_file[1024] = "fplot_";
                                        /* Initialize.  No fstruct entry */
                                        /* with a NULL filename is valid */
                                        /* no need to clear whole struct */
    fringe4.nalloc = 0;
                                        /* Check option flags, then fill */
                                        /* in the files structure array, */
                                        /* checking the file type implied */
                                        /* by the name in each case */
    if (parse_cmdline (argc, argv, &files, &display, &file_name) != 0)
        {
        msg ("Fatal error interpreting command line", 2);
        syntax("$HeadURL: https://vault.haystack.mit.edu/svn/hops/trunk/postproc/fplot/fplot.c $");
        exit(1);
        }
    if (files[0].order == -1)
        {
        msg ("No valid type-2 files found/specified", 2);
        syntax("$HeadURL: https://vault.haystack.mit.edu/svn/hops/trunk/postproc/fplot/fplot.c $");
        exit (1);
        }
                                        /* Loop over all filenames */
    i = 0;
    quit = FALSE;
    prompt = FALSE;
    while (files[i].order >= 0)
        {
        if (read_mk4fringe (files[i++].name, &fringe4) != 0)
            {
            msg ("Failure reading fringe file %s", 2, files[i-1].name);
            continue;
            }
                                    /* Display on screen if xwindow */
        if (display == XWINDOW || display == GSDEVICE)
            {
            if (display == XWINDOW) putenv("GS_DEVICE=x11");
            else                    prompt = TRUE;
            if (prompt) msg ("File %d: %s", 2, i-1, files[i-1].name);
            c = display_221 (fringe4.t221, 1);
            switch (c)
                {
                case 'q':
                    quit = TRUE;
                    break;
                case 'p':
                    if (i > 1) i -= 2;
                    else i -= 1;
                    break;
                case 'n':
                default:
                    break;
                }
            if (quit) break;
            }

        else if (display == HARDCOPY || display == PRINTLPR)
            {
            // ps_file = tmpnam (NULL);
            // if ((fp = fopen (ps_file, "w")) == NULL)
            strcpy(ps_file, P_tmpdir "/fplot_XXXXXX");
            if ((fp = fdopen (size=mkstemp(ps_file), "w")) == NULL)
                {
                msg ("PS file (%s,%d) for printing failed", 2, ps_file, size);
                return (0);
                }
            size = strlen (fringe4.t221->pplot);
            fwrite (fringe4.t221->pplot, 1, size, fp);
            fclose (fp);
            //sprintf (cmd, "pplot_print %s", ps_file);
            sprintf (cmd, "%s %s",
                (display==HARDCOPY)?"pplot_print":"lpr", ps_file);
            system (cmd);
            msg ("Printing hardcopy of fringe plot (%s)", 2, ps_file);
            unlink (ps_file);       /* Tidy up */
            }
        else if (display == DISKFILE)
            {
            snprintf(ps_file, sizeof(ps_file), file_name, i-1);
            if ((fp = fopen (ps_file, "w")) == NULL)
                {
                msg ("Could not open PS file (%s) for output", 2, ps_file);
                return(0);
                }
            size = strlen (fringe4.t221->pplot);
            fwrite (fringe4.t221->pplot, 1, size, fp);
            fclose (fp);
            msg ("Created PS plot %s", 1, ps_file);
            }
                                    /* Bad value for display */
        else
            {
            msg ("Bad value for 'display' variable: %d", 2, display);
            exit (1);
            }

        }
    }
