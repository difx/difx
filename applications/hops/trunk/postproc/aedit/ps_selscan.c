/************************************************************************/
/*									*/
/* Tags all cells for the selected scan, visually tagging those on  	*/
/* screen                                                               */
/*									*/
/*	Inputs:		scan                    scan number to tag  	*/
/*                      psarray                 as usual                */
/*									*/
/*	Output:         psarray                 flags updated           */
/*									*/
/* Created 23 February 1993 by CJL					*/
/*									*/
/************************************************************************/
#include "cpgplot.h"
#include "psplot.h"

void
ps_selscan (scan, psarray)
int scan;
struct ps_array *psarray;
    {
    int i, bno, sno, basepage, setflag, band;
    float xpos, ypos;
    struct psplot_cell *cell;
    
    band = psarray->param.band;
					/* a bit of setup ... */
    cpgslw (1);
    cpgsci (TAG_COLOUR);
    cpgsch (psarray->param.tagsize);
    sno = scan % psarray->param.scans_per_page;
    xpos = PLOT_XMIN + (((float)sno + 0.5) * psarray->param.scan_sep);
					/* Which direction is toggle set? */
    if (psarray->time[scan].tagged[band] == 1)
	{
	psarray->time[scan].tagged[band] = 0;
	setflag = FALSE;
	}
    else
	{
	psarray->time[scan].tagged[band] = 1;
	setflag = TRUE;
	}

					/* Loop over all scans for */
					/* the requested baseline */
    for (i=0; i<psarray->nbaselines; i++)
	{
	cell = psarray->baseline[i].scan + scan;

	if (cell->data_index[band] < 0) continue;
					/* Set the tag flag */
	if (setflag == 1)
	    {
	    if (cell->flag[band] == 0) psarray->ntagged++;
	    else continue;
	    cell->flag[band] = 1;
	    }
	else
	    {
	    if (cell->flag[band] == 1) psarray->ntagged--;
	    else continue;
	    cell->flag[band] = 0;
	    }
					/* Is this cell on screen? */
	basepage = i / psarray->param.base_per_page;
	if (basepage == psarray->param.basepage)
					/* If so, write tag character */
	    {
	    bno = i % psarray->param.base_per_page;
	    ypos = PLOT_YMAX - (((float)bno + 0.5) * psarray->param.base_sep);
	    if (setflag == FALSE) cpgsci (cell->colour_index[band]);
	    cpgpt (1, &xpos, &ypos, 13);
	    }
	}
    }
