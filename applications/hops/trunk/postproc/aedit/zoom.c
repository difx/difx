/************************************************************************/
/*                                                                      */
/* This routine simply invokes the cursor to select a point on a plot,  */
/* and prints more detailed information about it.                       */
/*                                                                      */
/*      Inputs:                                                         */
/*                                                                      */
/*      Output:                                                         */
/*                                                                      */
/* Created 4/26/90 by CJL                                               */
/*                                                                      */
/************************************************************************/
#include "aedit.h"
#include "aedata.h"
#include "pstruct.h"

int zoom (esum *data)
    {
    extern struct inputs inp;
    extern struct plot_info pdata[];
    int index, ret;
    float x, y;

    if ((pdata[0].plotby == TRIANGLE_PLOT) || (pdata[0].plotby == QUAD_PLOT))
        {
        msg ("Zoom not yet implemented for closure quantities", 2);
        return (-1);
        }

    while ((ret = cursor_select (data, &index, 1)) > -4)
        if(index >= 0) print_data (&(data->fdata[index]));

    if (ret < -4) return (-1);
    return (0);
    }
