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
/****************************************************************/

#include <stdio.h>
#include <string.h>
#include "data.h"
#include "mk4_data.h"
#include "fstruct.h"
#include "fplot.h"

char progname[6] = "fplot";
int msglev = 2;

main (argc, argv)
int argc;
char *argv[];
    {
    int i, display, ret, mk4, size, quit;
    char c, *ps_file, *title[2], cmd[50];
    struct data_fringe fringe3;
    struct mk4_fringe fringe4;
    fstruct *files;
    FILE *fp;
                                        /* Initialize.  No fstruct entry with a NULL */
                                        /* filename is valid ... don't need to clear */
                                        /* the whole structure */
    fringe3.fplot_alloc = fringe3.alloc_5000 = FALSE;
    fringe4.nalloc = 0;
                                        /* Check for option flags, then fill in the */
                                        /* files structure array, checking the file */
                                        /* type implied by the name in each case */
    if (parse_cmdline (argc, argv, &files, &display) != 0)
        {
        msg ("Fatal error interpreting command line", 2);
        syntax();
        exit(1);
        }
    if (files[0].order == -1)
        {
        msg ("No valid type-2 files found/specified", 2);
        syntax();
        exit (1);
        }
                                        /* Just in case you were wondering, */
                                        /* the "title" variable is declared */
                                        /* like this to imitate argv, which */
                                        /* is what one of the X primitives likes */
                                        /* "make it work and don't ask questions" */
                                        /* Loop over all filenames */
    i = 0;
    quit = FALSE;
    while (files[i].order >= 0)
        {
        if (read_mk4fringe (files[i++].name, &fringe4) != 0)
            {
            msg ("Failure reading fringe file %s", 2, files[i-1].name);
            continue;
            }
                                    /* Display on screen if xwindow */
        if (display == XWINDOW)
            {
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

        else if (display == HARDCOPY)
            {
            ps_file = tmpnam (NULL);
            if ((fp = fopen (ps_file, "w")) == NULL)
                {
                msg ("Could not open temporary postscript file for printing", 2);
                return ('\0');
                }
            size = strlen (fringe4.t221->pplot);
            fwrite (fringe4.t221->pplot, 1, size, fp);
            fclose (fp);
            sprintf (cmd, "pplot_print %s", ps_file);
            system (cmd);
            msg ("Printing hardcopy of postscript fringe plot", 2);
                                    /* Tidy up */
            unlink (ps_file);
            }
                                    /* Bad value for display */
        else
            {
            msg ("Bad value for 'display' variable: %d", 2, display);
            exit (1);
            }

        }
    }
