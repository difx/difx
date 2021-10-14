/************************************************************************/
/*                                                                      */
/* This routine takes a type-221 record, which contains a postscript    */
/* fringeplot, and displays it in a (persistent) ghostscript window.    */
/* It modifies the plot to insert instructions at the bottom of the     */
/* plot for interactive control, then switches the user's terminal into */
/* unbuffered mode and captures the next keystroke.  It takes action    */
/* depending on the result.  There are two modes, which offer different */
/* actions.  The ghostscript executable is assumed to be in /usr/bin,   */
/* and the program is assumed to be running from a normal terminal      */
/* session which can be controlled using stty(1) commands.  The         */
/* hardcopy capability, which is system-dependent, is implemented in    */
/* the shell script $BIN/pplot_print.                                   */
/*                                                                      */
/*      Inputs:     t221        pointer to type 221 record, filled in   */
/*                  mode        0=basic, 1='p' and 'n' for prev, next   */
/*                              -1=display plot and return immediately  */
/*                              -2=shut down display window             */
/*                              -3 or less =display plot, sleep and ret */
/*                                                                      */
/*      Output:     display or hardcopy                                 */
/*                  return value    key pressed, or space for mission   */
/*                                  accomplished.  Null for error.      */
/*                                                                      */
/* Created November 10 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include "mk4_data.h"
#include "mk4_dfio.h"
#include "mk4_util.h"

#ifndef GS_EXEC
# define GS_EXEC "/usr/bin/gs"
# warning "GS_EXEC default of /usr/bin/gs used"
#else /* GS_EXEC */
# warning "GS_EXEC provided by preprocessor"
#endif /* GS_EXEC */
#ifndef GS_COPYPAGE_OK
# define GS_COPYPAGE_OK 1
#else /* GS_COPYPAGE_OK */
# define GS_COPYPAGE_OK 0
#endif /* GS_COPYPAGE_OK */

/* Assuming this logic works, GS_COPYPAGE_OK can be purged */
#ifndef GS_VERSION
# define GS_VERSION 0
#endif /* GS_VERSION */
#if GS_VERSION < 9100
# warning "copypage command should work for us"
#elif GS_VERSION < 9220
# warning ".outputpage command should work for us"
#else
# warning "flushpage is our best hope now"
#endif

#ifdef P_tmpdir
# define P_tmpdir "/tmp"
#endif /* P_tmpdir */

#define TRUE 1
#define FALSE 0
                                        /* Set up macro for inserting justified */
                                        /* text using native postscript fonts */
                                        /* (taken from make_postplot() in fourfit) */
#define psleft(xcoord, ycoord, ps_string)\
             {xval = xcoord * 7800; yval = ycoord * 10500;\
                sprintf (psbuf, "%d %d M (%s) SL\n", xval, yval, ps_string);}

char
display_221 (struct type_221 *t221,
             int mode)
    {
    char *pplot, *end_of_plot, c, *ptr, *gs_exec = getenv("GS_EXEC");
    char *gs_debug = getenv("GS_DEBUG");
    char *fgets_retval;
    int i, nchar, size, xval, yval, loop, system_retval;
    static int gsopen = FALSE;
    static FILE *gs;
    static char psbuf[2560], cmd[1280], response[1024], fname[1024];
    static char ps_file[1024] = "sub-dfio_", gse[1024];
    extern char progname[];
    FILE *fp, *fdbg;

    if (mode == -2)
        {
        if (gsopen) pclose (gs);
        else msg ("Error, attempt to close non-existent gs pipe", 2);
        gsopen = FALSE;
        return ('\0');
        }
    fdbg = (gs_debug) ? fopen(gs_debug, "w") : 0;   /* debug output file */

    pplot = t221->pplot;
                                        /* Locate end of original plot */
    end_of_plot = strstr (pplot, "PGPLOT restore showpage");
    if (end_of_plot == NULL)
        {
        msg ("Error, invalid postscript plot in type 221 record", 2);
        return ('\0');
        }
    nchar = end_of_plot - pplot;
    end_of_plot += strlen("PGPLOT restore showpage");
                                        /* Open gs and display the plot */
    if (! gsopen)
        {
        if (gs_exec)
            {
            snprintf(gse, sizeof(gse), "%s -q -", gs_exec);
            msg ("Using '%s' for gs child command", 3, gse);
            gs = popen (gse, "w"); 
            }
        else
            {
            gs = popen (GS_EXEC " -q -", "w");
            }
        gsopen = TRUE;
        if (gs == NULL)
            {
            msg ("Cannot open ghostscript display", 2);
            return ('\0');
            }
        }
                                        /* Last command must have been a */
                                        /* copypage (see below), so we still */
                                        /* need an erase */
    else
        {
        sprintf (psbuf, " erasepage\n");
        fwrite (psbuf, 1, strlen (psbuf), gs);
        }
                                        /* Dump the postscript instructions */
    fwrite (pplot, 1, nchar, gs);
    if (fdbg) fwrite (pplot, 1, nchar, fdbg);
                                        /* Now add some instructions at the */
                                        /* bottom of the plot */
    sprintf (psbuf, "/Helvetica findfont 110 scalefont setfont\n");
    fwrite (psbuf, 1, strlen (psbuf), gs);
    if (fdbg) fwrite (psbuf, 1, strlen (psbuf), fdbg);

    sprintf (psbuf, "0.7 0.0 0.7 setrgbcolor\n");
    fwrite (psbuf, 1, strlen (psbuf), gs);
    if (fdbg) fwrite (psbuf, 1, strlen (psbuf), fdbg);

    if (mode < 0)
        ptr = "                                                             ";
    else if (mode == 0)
        ptr = "Press a key: 'h'=hardcopy, 's'=save, 'q'=quit, other=continue";
    else if (mode == 1)
        ptr = "Press a key: 'h'=hardcopy, 'p'=previous, 'n'=next, 's'=save, 'q'=quit, other=continue";
    else
        {
        msg ("Invalid mode %d in display_221()", 2, mode);
        return ('\0');
        }

    for (i=0; i<200; i++)           // put in extra padding to circumvent flushing problem
        {                           // rjc 2009.3.11
        psleft (0.1, 0.01, ptr);
        fwrite (psbuf, 1, strlen (psbuf), gs);
        if (fdbg) fwrite (psbuf, 1, strlen (psbuf), fdbg);
        }
                                        /* Substitute copypage for showpage to */
                                        /* keep ghostscript happy */
#if GS_COPYPAGE_OK
    /* showpage->copypage worked through version 9.05 */
    sprintf (psbuf, "PGPLOT restore copypage");
#else /* GS_COPYPAGE_OK */
    /* this is required by version 9.14 */
    sprintf (psbuf, "PGPLOT restore false .outputpage\n"
                    "%% copypage changed in PostScriptLevel3");
    /*
     * To clarify, in the transition to PS3, Adobe dropped the
     * copypage command.  The internal .outputpage command used
     * this way has the net effect of flushing and copying the page.
     * The 'false' tells .outputpage (the worker behind showpage)
     * not to erase the buffer.  I think 'restore' leaves 1 on the
     * stack which gets used as the number of copies.
     */
#endif
#if GS_VERSION > 9210
    /* this seems to work for version 9.22 */
    sprintf (psbuf, "PGPLOT restore flushpage showpage");
#endif /* GS_VERSION > 9210 */
    /* another override */
    if (getenv("GS_COPYPAGE"))
        {
        strcpy(psbuf, getenv("GS_COPYPAGE"));
        msg ("Adusted PS restore to '%s'", 3, psbuf);
        }
    fwrite (psbuf, 1, strlen (psbuf), gs);
    if (fdbg) fwrite (psbuf, 1, strlen (psbuf), fdbg);
    fflush (gs);
    fflush (fdbg);
                                        /* Pause to flush and take any user */
                                        /* user input, unless mode==-1.  The */
                                        /* rest of the plot follows below. */
    loop = TRUE;
    if (mode < 0)
        {
        c = 'q';
        fwrite (end_of_plot, 1, strlen (end_of_plot), gs);
        if (fdbg) fwrite (end_of_plot, 1, strlen (end_of_plot), fdbg);
        if (fdbg) fclose(fdbg);
        fdbg = 0;
        if (mode < -2)
            {
            msg ("Sleeping for %d", 3, -mode-2);
            fflush(stderr);
            sleep(-mode-2);
            }
        loop = FALSE;
        }
    while (loop)
        {
        system_retval = system ("stty -echo -icanon min 1");
        c = getchar();
        if (end_of_plot)
        {
        fwrite (end_of_plot, 1, strlen (end_of_plot), gs);
        if (fdbg) fwrite (end_of_plot, 1, strlen (end_of_plot), fdbg);
        end_of_plot = 0;                /* finish plot, once */
        if (fdbg) fclose(fdbg);
        fdbg = 0;
        }
        system_retval = system ("stty echo icanon");

        switch (c)
            {
                                        /* Hardcopy to color printer */
            case 'h':
            case 'H':
                {
                strcpy(ps_file, P_tmpdir "/sub-dfio_XXXXXX");
                if ((fp = fdopen(size=mkstemp(ps_file), "w")) == NULL)
                    {
                    msg ("fdopen of PS file (%s,%d) for printing failed",   
                        2, ps_file, size);
                    return ('\0');
                    }
                size = strlen (pplot);
                fwrite (pplot, 1, size, fp);
                fclose (fp);
                sprintf (cmd, "pplot_print %s", ps_file);
                system_retval = system (cmd);
                msg ("Printing hardcopy of postscript fringe plot", 2);
                                        /* Tidy up */
                unlink (ps_file);
                }
                break;
                                        /* Save to a file */
            case 's':
            case 'S':
                printf ("%s: Type name of file to save plot in:  ", progname); 
                fflush(stdout);
                fgets_retval = fgets (response, 127, stdin);
                ptr = response;
                                        /* Find first whitespace-delimited */
                                        /* string in the response */
                for (i=0; i<strlen(response); i++)
                    if (! isspace(response[i])) break;
                sscanf (response+i, "%s", fname);
                                        /* Open and save */
                if ((fp = fopen (fname, "w")) == NULL)
                    {
                    msg ("Could not open file '%s' to save fringe plot", 2, fname);
                    return ('\0');
                    }
                size = strlen (pplot);
                nchar = fwrite (pplot, 1, size, fp);
                fclose (fp);
                msg ("Wrote %d bytes into file '%s'", 2, nchar, fname);
                break;
                                        /* Terminate loop and pass typed */
                                        /* character back to caller */
            default:
                loop = FALSE;
                break;
            }
        }

    return (c);
    }
