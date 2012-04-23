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
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "mk4_data.h"

#ifdef P_tmpdir
# define P_tmpdir "/tmp"
#endif /* P_tmpdir */

#define TRUE 1
#define FALSE 0

int 
display_fplot (fringe)
struct mk4_fringe *fringe;
    {
    FILE *fp, *fopen();
    char c;
    int i, size;
    extern int displayopt;
    extern char display_name[];
    static char *options[] = 
        {
        "xwindow",
        "diskfile",
        "hardcopy",
        "pshardcopy",
        "psscreen"
        };
    static int noptions = 5;
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
	if (strncmp (options[i], temp, 7) == 0) break;

    switch (i)
        {
        case 1:
	    if (strlen(display_name) < 10)
		{
		msg ("Illegal diskfile request %s", 2, display_name);
		return(0);
		}
	    strncpy(ps_file, display_name+9, sizeof(ps_file));
	    if ((fp = fopen (ps_file, "w")) == NULL)
		{
                msg ("Could not open PS file (%s) for output", 2, ps_file);
                return (0);
		}
	    size = strlen (fringe->t221->pplot);
	    fwrite (fringe->t221->pplot, 1, size, fp);
	    fclose (fp);
	    msg ("Created PS plot %s", 1, ps_file);
            break;

        case 2:
        case 3:
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

        case 0:
	    putenv("GS_DEVICE=x11");	/* fall through */
        case 4:
                                        /* Mode 0 is basic mode (no 'p', 'n') */
            c = display_221 (fringe->t221, 0);
            msg ("display_221() character returned = '%c'", -1, c);
                                        /* Special return for quit key */
            if (c == 'q') return (-1);
            break;
    
        default:
            msg ("Unrecognized display option '%s'", 2, display_name);
        }
    
    return (0);
    }
