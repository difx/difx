/*****************************************************/
/*                                                   */
/* Eventually, this routine will handle all the      */
/* fringe display options, but for now simply writes */
/* a traditional fringe plot to something in the     */
/* current directory.  Many options since added...   */
/*                                                   */
/* Created October 3 1991 by CJL                     */
/* Modernized/overhauled by GBC 2010..2023           */
/*                                                   */
/*****************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "msg.h"
#include "mk4_data.h"
#include "hops_config.h"
#include "pass_struct.h"

/* linked from sub/dfio/display_221 */
extern char display_221 (struct type_221 *, int);

#ifdef P_tmpdir
# define P_tmpdir "/tmp"
#endif /* P_tmpdir */

#ifndef PS2PDF
# define PS2PDF "false"
#endif /* PS2PDF */

#define TRUE 1
#define FALSE 0

// FIXME: move to sub/dfio/gen_psname.c (later)
/* this is a verbatim copy from fplot/fplot.c */

// ofs: takes us past diskfile: (9) or ps2pdf: (7)
// note that the caller must free the returned value
static char *gen_psname(char *dn, int ofs, struct mk4_fringe *fringe, int fn)
{
    int mm = strlen(dn)+20, pi;
    char *nn = (char*) calloc(mm, 1), *pp, *svP, *svB, *svF;
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
display_fplot (struct mk4_fringe *fringe)
    {
    FILE *fp, *fopen(const char*, const char*);
    char c, *ppr, *psname;
    int opt = -1, size, rc;
    extern int displayopt;
    extern char display_name[];
    static char *options[] = 
         // opt=0      1           2           3             4           5
         { "xwindow","diskfile","pshardcopy","hardcopy","psscreen","ps2pdf" };
    enum {  XWINDOW,  DISKFILE,  PSHARDCOPY,  HARDCOPY,  PSSCREEN,  PSTOPDF };
    static int noptions = sizeof(options)/sizeof(char *), pn = 0;
    static int gsopen = FALSE;
    static FILE *gs;
    static char cmd[2560];
    static char ps_file[2048] = "fourfit_";

    // only proceed if display was requested on the command line.
    if (! displayopt) return (0);

    // identify the plot options
    for (opt = 0; opt < noptions; opt++)
#if HAVE_STRCASECMP
        if (!strncasecmp(display_name, options[opt], strlen(options[opt])))
#else
        if (!strncmp(display_name, options[opt], strlen(options[opt])))
#endif /* HAVE_STRCASECMP */
            break;

    switch (opt)
        {
        case PSTOPDF:
        case DISKFILE:
            if (strlen(display_name) < 10)
            {
            msg ("Illegal diskfile request %s", 2, display_name);
            return(0);
            }
            // generate the output filename, see gen_psname() above
            psname = gen_psname(display_name,
                (DISKFILE == opt) ? 9 : 7, fringe, pn++);
            memcpy(ps_file, psname, strlen(psname)+1);
            if ((fp = fopen (ps_file, "w")) == NULL)
            {
                msg ("Could not open PS file (%s) for output", 2, ps_file);
                free(psname);
                return (0);
            }
            size = strlen (fringe->t221->pplot);
            fwrite (fringe->t221->pplot, 1, size, fp);
            fclose (fp);
            msg ("Created PS plot %s", 1, ps_file);
            if (DISKFILE == opt) break;

            // continue with system call for PSTOPDF
            snprintf(cmd, sizeof(cmd), "%s %s", PS2PDF, ps_file);
            if (system(cmd))
                msg ("ps2pdf na/failed, leaving %s", 2, ps_file);
            else if (unlink(ps_file))
                msg ("Unable to remove %s", 2, ps_file);
            else
                msg ("Created PDF from %s", 1, ps_file);
            free(psname);
            break;

        case HARDCOPY:
        case PSHARDCOPY:
            // create a proper tempfile
            strcpy(ps_file, P_tmpdir "/fourfit_XXXXXX");
            if ((fp = fdopen(size=mkstemp(ps_file), "w")) == NULL)
                    {
                    msg ("temp PS file (%s,%d) for printing failed", 2,
                        ps_file, size);
                    return (0);
                    }
            size = strlen (fringe->t221->pplot);
            fwrite (fringe->t221->pplot, 1, size, fp);
            fclose (fp);

            ppr = getenv("MK4_PRINTER");
            if (ppr) msg ("MK4_PRINTER is '%s' opt=%d", 3, ppr, opt);
            if (opt == 3) { // 3: HARDCOPY uses lpr
                // for unit testing, set MK4_PRINTER to no-such-printer
                if (ppr) snprintf(cmd, sizeof(cmd),"lpr -P %s %s",ppr,ps_file);
                else     snprintf(cmd, sizeof(cmd),"lpr %s",          ps_file);
            } else {        // 2: PSHARDCOPY uses pplot_print.sh
                // for unit testing, set MK4_PRINTER to ./pplot_printer.sh"
                snprintf (cmd, sizeof(cmd),
                    (ppr && !strcmp(ppr, "./pplot_printer.sh"))
                        ? "./pplot_printer.sh %s" : "pplot_print.sh %s", ps_file);
            }

            msg ("Printing cmd: %s", 3, cmd);
            if (0 == (rc = system (cmd)))
                msg ("Printed hardcopy of fringe plot (%s)", 1, ps_file);
            else
                msg ("Unable to (ps)hardcopy (%d), "
                     "try diskfile or ps2pdf methods", 3, rc);
            unlink (ps_file);           /* Tidy up */
            break;

        case XWINDOW:
            putenv("GS_DEVICE=x11");    /* fall through */
        case PSSCREEN:
            /* Mode 0 returns 'h', 's', 'q' via stty -echo -icanon min 1 */
            c = display_221 (fringe->t221, 0);
            msg ("display_221() character returned = '%c'", -1, c);
                                        /* we only care about the 'q' case */
            if (c == 'q') return (-1);
            /* else we are done with this fringe and eventually return 0 */
            break;
    
        default:
            msg ("Unrecognized display option '%s'", 2, display_name);
        }
    
    return (0);
    }
