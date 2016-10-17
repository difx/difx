/************************************************************************/
/*                                                                      */
/* This routine invokes the cursor to select a point on a plot,         */
/* and and pops up the corresponding fringe plot.                       */
/*                                                                      */
/*      Inputs:                                                         */
/*                                                                      */
/*      Output:                                                         */
/*                                                                      */
/* Created March 4 1992 by CJL                                          */
/* removed mk3 code                             2010.6.8  rjc           */
/************************************************************************/
#include <stdio.h>
#include "aedit.h"
#include "mk4_data.h"
#include "mk4_dfio.h"
#include "aedata.h"
#include "pstruct.h"


int fplot (esum *data)
    {
    extern struct inputs inp;
    extern struct plot_info pdata[];
    static struct mk4_fringe fringe4;
    int index, ret;
    char *fname, *fringename(), fullname[256];
    char c, *argv[2];
    int argc = 1;
    extern char datadir[];
    static int first = 1;

    if (first)
        {
        fringe4.nalloc = 0;
        first = 0;
        }

    if ((pdata[0].plotby == TRIANGLE_PLOT) || (pdata[0].plotby == QUAD_PLOT))
        {
        msg ("Fplot not yet implemented for closure quantities", 2);
        return (-1);
        }

    
                                /* Now loop until user hits 'x' */
    while ((ret = cursor_select (data, &index, 1)) > -4) 
        {
        if(index < 0) continue;
                                /* This gets filename of selected point */
        if ((fname = fringename (&(data->fdata[index].data))) == NULL)
            {
            msg ("Could not generate data file name", 2);
            continue;
            }
        sprintf (fullname, "%s/%s", datadir, fname);

        if (read_mk4fringe (fullname, &fringe4) != 0)
            {
            msg ("Failure reading fringe file %s", 2, fullname);
            continue;
            }
        if (ret == -3) c = display_221 (fringe4.t221, 0);
        else c = display_221 (fringe4.t221, -1);
        ret = 0;
        }
    display_221 (NULL, -2);

    return (ret);
    }
