/************************************************************************/
/*									*/
/* This routine is responsible for sorting out the two axes of a plot.	*/
/* It figures out whether the axes are compatible, and whether the data	*/
/* are to be plotted by station, baseline, triangle, quad, or all 	*/
/* together on one plot.						*/
/*									*/
/*	Inputs:		y_axis		string typed by user		*/
/*			x_axis		defaults to time if null	*/
/*									*/
/*	Output:		inp		axis elements filled in		*/
/*			return value	0=OK, else axes no good		*/
/*									*/
/* Created 10 February 1994 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "aedit.h"
#include "pstruct.h"

int axis (char *y_axis, char *x_axis)
    {
    char y_ax_name[30], x_ax_name[30], y_ax_units[20], x_ax_units[20];
    int i, y_ax_index, x_ax_index, x_plotby, y_plotby, plotby, raw_plotby;
    int x_aux, y_aux;
    extern struct inputs inp;
					/* Nothing specified, no change */
    if (strlen (y_axis) == 0) return(0);
					/* Determine requested axes */
    if (get_axis (y_axis, y_ax_name, &y_ax_index, 
			y_ax_units, &y_aux, &y_plotby) != 0)
	return (-1);
					/* Defaults to timetag if empty */
    if (strlen (x_axis) == 0)
	{
	if (get_axis ("timetag", x_ax_name, &x_ax_index, 
				x_ax_units, &x_aux, &x_plotby) != 0)
	    return (-1);
	}
    else
	{
	if (get_axis (x_axis, x_ax_name, &x_ax_index, 
				x_ax_units, &x_aux, &x_plotby) != 0)
	    return (-1);
	}
					/* Establish compatibility. Cannot */
					/* mix triangles and stations, and so on */
    if ((raw_plotby = x_plotby & y_plotby) == 0)
	{
	msg ("Sorry, plotting %s against %s makes no sense", 2, y_ax_name, x_ax_name);
	return (-1);
	}
					/* If 2 bits are set, choose lowest */
    for (i=0; i<5; i++)
	if ((plotby = raw_plotby & (1<<i)) != 0) break;
					/* Sanity check */
    if (plotby == 0)
	{
	msg ("Error in axis.c, messed up plotby variable", 2);
	return (-1);
	}
					/* Fill in input variables */
    if ((inp.yaind != y_ax_index) && 
		((inp.yscale[0] != 0.0) || (inp.yscale[1] != 0.0)))
	{
	msg ("Warning, Y-axis scale reset to default", 2);
	inp.yscale[0] = inp.yscale[1] = 0.0;
	}
    inp.yaind = y_ax_index;
    inp.y_aux = y_aux;
    strcpy (inp.y_axis, y_ax_name);
    strcpy (inp.y_units, y_ax_units);
    if ((inp.xaind != x_ax_index) && 
		((inp.xscale[0] != 0.0) || (inp.xscale[1] != 0.0)))
	{
	msg ("Warning, X-axis scale reset to default", 2);
	inp.xscale[0] = inp.xscale[1] = 0.0;
	}
    inp.xaind = x_ax_index;
    inp.x_aux = x_aux;
    strcpy (inp.x_axis, x_ax_name);
    strcpy (inp.x_units, x_ax_units);
    inp.plotby = plotby;

    return (0);
    }
