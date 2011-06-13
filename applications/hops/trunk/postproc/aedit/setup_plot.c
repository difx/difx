/************************************************************************/
/*									*/
/* This routine is responsible for locating the plot on the page, 	*/
/* storing this information in the pdata structure for possible future	*/
/* use with the cursor, and deciding on scales.  The X-axis scale is	*/
/* set by the timerange of all the data, or by the user, so that times	*/
/* will line up vertically on the page.  This assumes some intelligence	*/
/* on the part of the user when diagnosing a squashed plot.  setup_plot	*/
/* also sets up the pgplot viewport and window, and takes care of 	*/
/* drawing and labelling the frame.					*/
/*									*/
/*	Inputs:		pd		Pointer to plot data structure	*/
/*			nplot		Sequential plot number in pdata	*/
/*			fqex		freq/expt struct needed for	*/
/*					handling time axes		*/
/*									*/
/*	Output:		return value	0 = success			*/
/*									*/
/* Created 17 April 1989 by CJL						*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "cpgplot.h"
#include "aedit.h"
#include "pstruct.h"
#include "summary.h"

int
setup_plot (pd, nplot, fqex)
struct plot_info *pd;
int nplot;
struct frqexp fqex;
    {
    extern struct inputs inp;
    int i, year, day, hour, min, sec, tdiff;
    char buf[50];
    float xpos, ypos, ymin, ymax, ydif, yinc, xinc;
    float charsize;

    ypos = nplot%inp.grid[1];		/* 0==>top, grid[1]-1 ==> bottom */
    xpos = nplot/inp.grid[1];		/* 0-relative column number */
    xinc = 1.0/inp.grid[0];		/* label margins taken care of below */
    yinc = 0.85/inp.grid[1];		/* This leaves 10% at top for titling */
					/* and 5% at bottom for symbol identification */

    charsize = 4.0/inp.grid[1];		/* Small plots --> small writing */
    if(charsize > 1.0) charsize = 1.0;	/* 1.0 ==> 1/40 of page */
    if((inp.grid[0] == 2) && (charsize > 0.7)) charsize = 0.7;
    cpgsch(charsize);			/* so 0.1*charsize in ndc = 4 chars */

					/* Leave room for labels etc */
    pd->vport[0] = xpos*xinc + 0.1*charsize;	/* x - 4 chars margin */
    pd->vport[1] = (xpos+1.0)*xinc - 0.1*charsize;
    pd->vport[3] = (inp.grid[1]-ypos)*yinc + .05;	/* y - top <= 0.9 */
					/* Then leave 4 chars at bottom */
    pd->vport[2] = pd->vport[3] - yinc + 0.1*charsize;
					/* Tell pgplot about viewport */
    cpgsci (1);
    cpgsvp(pd->vport[0], pd->vport[1], pd->vport[2], pd->vport[3]);

					/* Generic scaling based on data */
    axis_scale (pd->xaind, pd->xmin, pd->xmax, pd->window, pd->window+1);
    axis_scale (pd->yaind, pd->ymin, pd->ymax, pd->window+2, pd->window+3);

					/* User override */
    if ((inp.xscale[0] != 0.0) || (inp.xscale[1] != 0))
	{
	pd->window[0] = inp.xscale[0];
	pd->window[1] = inp.xscale[1];
	}
    if ((inp.yscale[0] != 0.0) || (inp.yscale[1] != 0))
	{
	pd->window[2] = inp.yscale[0];
	pd->window[3] = inp.yscale[1];
	}
					/* Set up pgplot world-coord window */
					/* Need times relative to day 1 for */
					/* labelling */
    time_axis (pd, fqex);
    cpgswin (pd->window[0], pd->window[1], pd->window[2], pd->window[3]);
					/* Draw and label axes */
					/* Humour the joker who wants to break */
					/* the software */
    charsize *= 0.6;
    cpgsch (charsize);
    if ((pd->xaind == AX_TIMETAG) && (pd->yaind == AX_TIMETAG))
	cpgtbox ("BCZHYNTS", 0.0, 0, "BCZHYNTS", 0.0, 0);
					/* Sensible users ... */
    else if (pd->xaind == AX_TIMETAG)
	cpgtbox ("BCZHYNTS", 0.0, 0, "BCNTS", 0.0, 0);
    else if (pd->yaind == AX_TIMETAG)
	cpgtbox ("BCNTS", 0.0, 0, "BCZHYNTS", 0.0, 0);
    else
	cpgtbox ("BCNTS", 0.0, 0, "BCNTS", 0.0, 0);
					/* Warning about offscale points */
    if (pd->nbadscale > 0)
	{
	sprintf (buf, "%d pts missed", pd->nbadscale);
	cpgmtxt ("R",1.5, 0.5, 0.5, buf);
	if (pd->plotby == STATION_PLOT)
	    msg ("Warning: %d points off scale for station %c at %c-band", 2,
		pd->nbadscale, pd->station, pd->frq);
	else if (pd->plotby == BASELINE_PLOT)
	    msg ("Warning: %d points off scale for baseline %s at %c-band", 2,
		pd->nbadscale, pd->bas, pd->frq);
	else if (pd->plotby == TRIANGLE_PLOT)
	    msg ("Warning: %d points off scale for triangle %s at %c-band", 2,
		pd->nbadscale, pd->triangle, pd->frq);
	else if (pd->plotby == QUAD_PLOT)
	    msg ("Warning: %d points off scale for quad %s at %c-band", 2,
		pd->nbadscale, pd->ampcl, pd->frq);
	else
	    msg ("Warning: %d points off scale at %c-band", 2, pd->nbadscale, pd->frq);
	}
    charsize /= 0.6;
    cpgsch (charsize);
					/* Certain axes like 0-axis visible */
					/* put dotted line */
    if ((pd->xaind == AX_CPHASE) || (pd->xaind == AX_U) || (pd->xaind == AX_V))
	{
	cpgsls (4);
	cpgmove (0.0, pd->window[2]);
	cpgdraw (0.0, pd->window[3]);
	cpgsls (1);
	}
    if ((pd->yaind == AX_CPHASE) || (pd->yaind == AX_U) || (pd->yaind == AX_V))
	{
	cpgsls (4);
	cpgmove (pd->window[0], 0.0);
	cpgdraw (pd->window[1], 0.0);
	cpgsls (1);
	}
					/* Write title */
    if (pd->plotby == STATION_PLOT)
	sprintf (buf, "Station %c at %c-band", pd->station, pd->frq);
    else if (pd->plotby == BASELINE_PLOT)
	sprintf (buf, "Baseline %s at %c-band", pd->bas, pd->frq);
    else if (pd->plotby == TRIANGLE_PLOT)
	sprintf (buf, "Triangle %s at %c-band", pd->triangle, pd->frq);
    else if (pd->plotby == QUAD_PLOT)
	sprintf (buf, "Quad %s at %c-band", pd->ampcl, pd->frq);
    else sprintf (buf, "%c-band", pd->frq);
    if(strlen (pd->source) > 0) sprintf (buf,"%s   Source: %s", buf, pd->source);
    cpgmtxt ("T", 0.3, 0.0, 0.0, buf);
					/* Label axes, special if time */
    if (pd->xaind == AX_TIMETAG)
	{
	int_to_time (pd->toffset, &year, &day, &hour, &min, &sec);
	sprintf(buf,"Time (UT on day %d-%03d)",year,day);
	cpgmtxt("B",2.0,0.5,0.5,buf);
	}
    else cpgmtxt("B",2.0,0.5,0.5,pd->xtype);

    if (pd->yaind == AX_TIMETAG)
	{
	int_to_time (pd->toffset, &year, &day, &hour, &min, &sec);
	sprintf(buf,"Time (UT on day %d-%03d)",year,day);
	cpgmtxt("L",2.0,0.5,0.5,buf);
	}
    else cpgmtxt("L",2.0,0.5,0.5,pd->ytype);

    return(0);
    }
