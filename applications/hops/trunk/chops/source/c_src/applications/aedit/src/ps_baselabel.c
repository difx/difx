/************************************************************************/
/*									*/
/* Inserts new baseline labels on vertical axis, if necessary.		*/
/*									*/
/*	Inputs:		psarray		Contains all needed information	*/
/*									*/
/*	Output:		none						*/
/*									*/
/* Created 22 February 1993 by CJL					*/
/*									*/
/************************************************************************/
#include "cpgplot.h"
#include "psplot.h"
#include "aedit.h"

void ps_baselabel (struct ps_array *psarray)
    {
    int basepage, nvertpage, bno, boffset, base;
    float bsep, xpos, ypos, xmin, xmax, ymin, ymax;
    static int current_basepage;

    if (! psarray->displayed) current_basepage = -1;

    basepage = psarray->param.basepage;

					/* Already have correct labels */
    if (basepage == current_basepage) return;
    current_basepage = basepage;
					/* Erase existing label */
    cpgsci (0);
    cpgsfs (1);
    cpgrect (BASELABEL);
					/* Set up some parameters */
    boffset = basepage * psarray->param.base_per_page;
    bsep = psarray->param.base_sep;

    cpgsci (TEXT);
    cpgslw (1);
    cpgsfs (2);
    cpgsch (0.5 + (bsep / 75.0));
    for (bno=0; bno<psarray->param.base_per_page; bno++)
        {
					/* Get psarray baseline index */
        base = bno + boffset;
        if (base >= psarray->nbaselines) break;
                                        /* Can now calculate correct Y position */
        ypos = PLOT_YMAX - (float)(bno+1) * bsep + 4.0;
	xpos = PLOT_XMIN - 25.0;
					/* Draw text */
	cpgtext (xpos, ypos, psarray->baseline[base].id);
					/* Enclose it in a rectangle */
	xmin = PLOT_XMIN - 30.0;
	xmax = PLOT_XMIN;
	ymin = ypos - 4.0;
	ymax = ymin + bsep;
	cpgrect (xmin, xmax, ymin, ymax);
	}

    }
