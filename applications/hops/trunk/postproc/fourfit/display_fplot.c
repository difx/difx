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
#include "mk4_data.h"

#define TRUE 1
#define FALSE 0

int 
display_fplot (fringe)
struct mk4_fringe *fringe;
    {
    FILE *fp, *fopen();
    char c, *title[2], temp[100], cmd[128], *ps_file;
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

    strcpy (temp, display_name);

    title[0] = "Fourfit fringe plot";

    if (! displayopt) 
        return (0);

    i = 0;
    while (c = temp[i++]) 
        if (isupper(c)) 
            temp[i-1] = tolower(c);
    for (i=0; i<noptions; i++)
            if (strncmp (options[i], temp, strlen (temp)) == 0)
            break;

    switch (i)
        {
        case 0:
        case 1:
        case 2:
            msg ("Traditional ascii fringe plot no longer available", 2);
            break;

        case 3:
            ps_file = tmpnam (NULL);
            if ((fp = fopen (ps_file, "w")) == NULL)
                {
                msg ("Could not open temporary postscript file for printing", 2);
                return ('\0');
                }
            size = strlen (fringe->t221->pplot);
            fwrite (fringe->t221->pplot, 1, size, fp);
            fclose (fp);
            sprintf (cmd, "pplot_print %s", ps_file);
            system (cmd);
            msg ("Printing hardcopy of postscript fringe plot", 1);
                                        /* Tidy up */
            unlink (ps_file);
            break;

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
