/*****************************************************/
/*                                                   */
/* Eventually, this routine will handle all the      */
/* fringe display options, but for now simply writes */
/* a traditional fringe plot to temp.fplot in the    */
/* current directory                                 */
/*                                                   */
/* Created October 3 1991 by CJL                     */
/*                                                   */
/*****************************************************/
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "mk4_data.h"

extern void msg (char *, int, ...);
extern char display_221 (struct type_221 *, int);

#ifdef P_tmpdir
# define P_tmpdir "/tmp"
#endif /* P_tmpdir */

#ifndef PS2PDF
# define PS2PDF "false"
#endif /* PS2PDF */

#define TRUE 1
#define FALSE 0

int 
display_fplot (struct mk4_fringe *fringe)
    {
    FILE *fp, *fopen();
    char c;
    int i, size, ofs;
    extern int displayopt;
    extern char display_name[];
    static char *options[] = 
        {
        "xwindow",
        "diskfile",
        "hardcopy",
        "pshardcopy",
        "psscreen",
        "ps2pdf"
        };
    enum {XWINDOW, DISKFILE, HARDCOPY, PSHARDCOPY, PSSCREEN, PSTOPDF};
    static int noptions = 6, pn = 0;
    static int gsopen = FALSE;
    static FILE *gs;
    static char temp[1024], cmd[1280];
    static char ps_file[1024] = "fourfit_";

    strcpy (temp, display_name);

    if (! displayopt) 
        return (0);

    i = 0;
    while (c = temp[i++]) 
        if (isupper(c)) temp[i-1] = tolower(c);
    for (i=0; i<noptions; i++)
	if (strncmp (options[i], temp, 6) == 0) break;

    switch (i)
        {
        case PSTOPDF:
        case DISKFILE:
	    if (strlen(display_name) < 10)
		{
		msg ("Illegal diskfile request %s", 2, display_name);
		return(0);
		}
	    //strncpy(ps_file, display_name+9, sizeof(ps_file));
            ofs = (DISKFILE == i) ? 9 : 7;
            snprintf(ps_file, sizeof(ps_file), display_name+ofs, pn++);
	    if ((fp = fopen (ps_file, "w")) == NULL)
		{
                msg ("Could not open PS file (%s) for output", 2, ps_file);
                return (0);
		}
	    size = strlen (fringe->t221->pplot);
	    fwrite (fringe->t221->pplot, 1, size, fp);
	    fclose (fp);
	    msg ("Created PS plot %s", 1, ps_file);
            if (DISKFILE == i) break;
            // continue with system call for PSTOPDF
            snprintf(cmd, sizeof(cmd), "%s %s", PS2PDF, ps_file);
            if (system(cmd))
                msg ("ps2pdf na/failed, leaving %s", 2, ps_file);
            else if (unlink(ps_file))
                msg ("Unable to remove %s", 2, ps_file);
            else
                msg ("Created PDF from %s", 1, ps_file);
            break;

        case HARDCOPY:
        case PSHARDCOPY:
            // ps_file = tmpnam (NULL);
            // if ((fp = fopen (ps_file, "w")) == NULL)
	        strcpy(ps_file, P_tmpdir "/fourfit_XXXXXX");
	        if ((fp = fdopen(size=mkstemp(ps_file), "w")) == NULL)
                    {
                    msg ("PS file (%s,%d) for printing failed", 2, ps_file, size);
                    return (0);
                    }
            size = strlen (fringe->t221->pplot);
            fwrite (fringe->t221->pplot, 1, size, fp);
            fclose (fp);
            //sprintf (cmd, "pplot_print %s", ps_file);
	        sprintf (cmd, "%s %s", (i==3)?"pplot_print":"lpr", ps_file);
            system (cmd);
            msg ("Printing hardcopy of fringe plot (%s)", 1, ps_file);
            unlink (ps_file);		/* Tidy up */
            break;

        case XWINDOW:
	        putenv("GS_DEVICE=x11");	/* fall through */
        case PSSCREEN:
                                        /* Mode 0 is basic mode (no 'p', 'n') */
            c = display_221 (fringe->t221, 0);
            msg ("display_221() character returned = '%c'", -1, c);
                                        /* Special return for quit key */
            if (c == 'q') 
                return (-1);
            break;
    
        default:
            msg ("Unrecognized display option '%s'", 2, display_name);
        }
    
    return (0);
    }
