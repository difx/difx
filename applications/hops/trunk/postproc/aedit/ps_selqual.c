/************************************************************************/
/*									*/
/* Tags all cells for the selected baseline, visually tagging those on	*/
/* screen								*/
/*									*/
/*	Inputs:		baseline		baseline number to tag	*/
/*			psarray			as usual		*/
/*			x			x coord of key cell	*/
/*									*/
/*	Output:		psarray			flags updated		*/
/*									*/
/* Created 23 February 1993 by CJL					*/
/*									*/
/************************************************************************/
#include "cpgplot.h"
#include "psplot.h"
#include "aedit.h"

void ps_selqual (int colour, struct ps_array *psarray)
    {
    int i, j, bno, sno, scanpage, basepage, setflag, band;
    float xpos, ypos;
    struct psplot_cell *cell;
    
    band = psarray->param.band;
					/* a bit of setup ... */
    cpgslw (1);
    cpgsch (psarray->param.tagsize);
                                        /* Which direction is toggle set? */
    if (psarray->qtagged[colour] == 1)
        {
        psarray->qtagged[colour] = 0;
	cpgsci (colour);
        setflag = FALSE;
        }
    else
        {
        psarray->qtagged[colour] = 1;
        setflag = TRUE;
	cpgsci (TAG_COLOUR);
        }
					/* Put tag marker on key cell */
    xpos = KEY_XMIN + (KEYINC * (float)colour) + (KEYINC / 2.0);
    ypos = KEY_YMIN + (KEYINC / 2.0);
    cpgpt (1, &xpos, &ypos, 13);
					/* Loop over all scans */
    for (i=0; i<psarray->nbaselines; i++)
	for (j=0; j<psarray->nscans; j++)
	    {
	    cell = psarray->baseline[i].scan + j;

	    if (cell->data_index[band] < 0) continue;
	    if (cell->colour_index[band] != colour) continue;
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
	    scanpage = j / psarray->param.scans_per_page;
	    if ((scanpage == psarray->param.scanpage)
			&& (basepage == psarray->param.basepage))
					/* If so, write tag character */
		{
		sno = j % psarray->param.scans_per_page;
		xpos = PLOT_XMIN + (((float)sno + 0.5) * psarray->param.scan_sep);
		bno = i % psarray->param.base_per_page;
		ypos = PLOT_YMAX - (((float)bno + 0.5) * psarray->param.base_sep);
		cpgpt (1, &xpos, &ypos, 13);
		}
	    }
    }
