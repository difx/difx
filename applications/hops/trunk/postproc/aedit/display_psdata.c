/************************************************************************/
/*									*/
/* Uses the information in the ps array to paint coloured rectangles	*/
/* in a grid on the screen.  A small space (specified by GUARD_BAND in	*/
/* psplot.h) is left blank around each rectangle for visual clarity.	*/
/* The correspondence between x-y position and data element is made by	*/
/* the routine locate_pscurs().						*/
/*									*/
/*	Inputs:		psarray		Contains all necessary info	*/
/*									*/
/*	Output:		none (except transcendental beauty)		*/
/*									*/
/* Created 19 February 1993 by CJL					*/
/*									*/
/************************************************************************/
#include "cpgplot.h"
#include "psplot.h"
#include "aedit.h"

int display_psdata (struct ps_array *psarray)
    {
    int nvertpage, basepage, scanpage, boffset, soffset, bno, sno;
    int base, scan, band;
    float ssep, bsep, xmin, xmax, ymin, ymax, x, y;

    basepage = psarray->param.basepage;
    scanpage = psarray->param.scanpage;
					/* These translate to psarray */
					/* index offsets */
    boffset = basepage * psarray->param.base_per_page;
    soffset = scanpage * psarray->param.scans_per_page;

					/* Clear whatever is on screen */
    cpgsfs (1);
    cpgsci (0);
    cpgrect (PLOT_XMIN, PLOT_XMAX, PLOT_YMIN, PLOT_YMAX);
					/* Redraw border */
    cpgsfs (2);
    cpgsci (TEXT);
    cpgslw (3);
    cpgrect (PLOT_XMIN, PLOT_XMAX, PLOT_YMIN, PLOT_YMAX);
    cpgsfs (1);

    ssep = psarray->param.scan_sep;	/* For clarity ... */
    bsep = psarray->param.base_sep;
					/* What band are we displaying? */
    band = psarray->param.band;
					/* Now loop over psarray for this */
					/* page */
					/* First loop over baselines (Y) */
    for (bno=0; bno<psarray->param.base_per_page; bno++)
	{
	base = bno + boffset;
	if (base >= psarray->nbaselines) break;

					/* Can now calc Y limits of rectangle */
	ymax = PLOT_YMAX - ((float)bno * bsep + GUARD_BAND);
	ymin = PLOT_YMAX - ((float)(bno+1) * bsep - GUARD_BAND);

					/* Loop over scans in X direction */
	for (sno=0; sno<psarray->param.scans_per_page; sno++)
	    {
	    scan = sno + soffset;
	    if (scan >= psarray->nscans) break;

					/* Calculate X limits of rectangle */
	    xmin = PLOT_XMIN + ((float)sno * ssep + GUARD_BAND);
	    xmax = PLOT_XMIN + ((float)(sno+1) * ssep - GUARD_BAND);

					/* And do the drawing */
	    cpgsci (psarray->baseline[base].scan[scan].colour_index[band]);
	    cpgrect (xmin, xmax, ymin, ymax);

	    if (psarray->baseline[base].scan[scan].flag[band])
		{
		cpgsci (TAG_COLOUR);
                cpgsch (psarray->param.tagsize);
		cpgslw (1);
		x = (xmin + xmax) / 2.0;
		y = (ymin + ymax) / 2.0;
                cpgpt (1, &x, &y, 13); 
		}
	    }
	}

    ps_baselabel (psarray);
    ps_scanlabel (psarray);

    psarray->displayed = TRUE;

    return (0);
    }
