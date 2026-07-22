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
#include <unistd.h>
#include "mk4_data.h"
#include "mk4_dfio.h"
#include "fstruct.h"
#include "fplot.h"
#include "msg.h"

// defined in fourfit/pass_struct.h
#define POL_LL 0
#define POL_RR 1
#define POL_LR 2
#define POL_RL 3

#ifdef P_tmpdir
# define P_tmpdir "/tmp"
#endif /* P_tmpdir */

#ifndef PS2PDF
# define PS2PDF "false"
#endif /* PS2PDF */

// char progname[6] = "fplot";
// int msglev = 2;

// FIXME: move to sub/dfio/gen_psname.c (later)
/* this is a verbatim copy from fourfit/display_fplot.c */
/* note that offset here is 0 due to cmd line processing */

// 9 extra digits should suffice for an integer fn
static char *gen_psname(char *dn, int ofs, struct mk4_fringe *fringe, int fn)
{
    int mm = strlen(dn)+20, pi;
    char *nn = (char *)calloc(mm, 1), *pp, *svP, *svB, *svF;
    msg("Incoming filename is %s", 1, dn + ofs);
    svP = pp = strstr(dn, "%P");
    if (pp) {
        // total polarization kludge -- see fill_208.c
        switch(pi = fringe->t208->unused1[0]) {
        case POL_LL: pp[0] = 'L'; pp[1] = 'L'; break;
        case POL_RR: pp[0] = 'R'; pp[1] = 'R'; break;
        case POL_LR: pp[0] = 'L'; pp[1] = 'R'; break;
        case POL_RL: pp[0] = 'R'; pp[1] = 'R'; break;
        default:     pp[0] = pi;  pp[1] = pi;  break;
        }
    }
    svB = pp = strstr(dn, "%B");
    if (pp) {
        pp[0] = fringe->t202->baseline[0];
        pp[1] = fringe->t202->baseline[1];
    }
    svF = pp = strstr(dn, "%F");
    if (pp) {
        // NB: both must be the same group, but reducing
        // to on character is more work than we want here.
        pp[0] = fringe->t203->channels[0].ref_chan_id[0];
        pp[1] = fringe->t203->channels[0].rem_chan_id[0];
    }
    snprintf(nn, mm, dn + ofs, (fn & 0xFFFFFFFF));
    if (svP) { svP[0] = '%'; svP[1] = 'P'; }
    if (svB) { svB[0] = '%'; svB[1] = 'B'; }
    if (svF) { svF[0] = '%'; svF[1] = 'F'; }
    msg("Generated Filename is %s (dn is %s)", 1, nn, dn);
    return(nn);
}

int
main (int argc, char* argv[])
    {
    int i, display, ret, mk4, size, quit, prompt, poln;
    char c, cmd[128], pmt[128], *file_name, *psname;
    struct mk4_fringe fringe4;
    fstruct *files;
    FILE *fp;
    static char ps_file[2048] = "fplot_";
                                        /* Initialize.  No fstruct entry */
                                        /* with a NULL filename is valid */
                                        /* no need to clear whole struct */
    fringe4.nalloc = 0;
                                        /* Check option flags, then fill */
                                        /* in the files structure array, */
                                        /* checking the file type implied */
                                        /* by the name in each case */
    set_progname("fplot");
    set_msglev(2);
    if (parse_cmdline (argc, argv, &files, &display, &file_name, &poln) != 0)
        {
        msg ("Fatal error interpreting command line", 2);
        exit(1);
        }
    if (files[0].order == -1)
        {
        msg ("No valid type-2 files found/specified", 2);
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
        if (poln >= 0 && fringe4.t203 != 0 && fringe4.t205 != 0 &&
            skip_poln(files[i-1].name, fringe4.t203, fringe4.t205, poln))
            continue;
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
            sprintf (cmd, "%s %s",
                (display==HARDCOPY)?"pplot_print.sh":"lpr", ps_file);
            system (cmd);
            msg ("Printing hardcopy of fringe plot (%s)", 2, ps_file);
            unlink (ps_file);       /* Tidy up */
            }
        else if (display == DISKFILE || display == PSTOPDF)
            {
            psname = gen_psname(file_name, 0, &fringe4, i-1);
            //snprintf(ps_file, sizeof(ps_file), file_name, i-1);
            memcpy(ps_file, psname, strlen(psname)+1);
            if ((fp = fopen (ps_file, "w")) == NULL)
                {
                msg ("Could not open PS file (%s) for output", 2, ps_file);
                return(0);
                }
            size = strlen (fringe4.t221->pplot);
            fwrite (fringe4.t221->pplot, 1, size, fp);
            fclose (fp);
            msg ("Created PS plot %s", 1, ps_file);
            if (display == PSTOPDF) /* continue and use PS2PDF */
                {
                snprintf(cmd, sizeof(cmd), "%s %s", PS2PDF, ps_file);
                if (system(cmd))
                    msg ("ps2pdf na/failed, leaving %s", 2, ps_file);
                else if (unlink(ps_file))
                    msg ("Unable to remove %s", 2, ps_file);
                else
                    msg ("Created PDF from %s", 1, ps_file);
                }
            }
                                    /* Bad value for display */
        else
            {
            msg ("Bad value for 'display' variable: %d", 2, display);
            exit (1);
            }

        }
    exit (0);
    }
