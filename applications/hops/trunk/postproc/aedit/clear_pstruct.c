/************************************************************************/
/*									*/
/* Trivial routine to empty a plot_info structure.  In principle, most	*/
/* of this should be unnecessary, since new information must be entered	*/
/* for anything to work, but this seems more robust.			*/
/*									*/
/*	Inputs:		ps		Pointer to plot_info structure	*/
/*									*/
/*	Output:		ps		cleared structure		*/
/*									*/
/* Created 25 February 1992 by CJL					*/
/*									*/
/************************************************************************/
#include "pstruct.h"
#include "aedit.h"

#define TRUE 1
#define FALSE 0

void clear_pstruct (struct plot_info *ps)
    {
    int i;

    ps->npts = ps->ngood = ps->nsusp = ps->nbad = 0;
    ps->nbadscale = 0;
    ps->xebar = ps->yebar = FALSE;
    ps->onscreen = FALSE;
    ps->plotby = 0;
    ps->xaind = NO_AXIS;
    ps->x_aux = -1;
    ps->xtype[0] = '\0';
    ps->yaind = NO_AXIS;
    ps->y_aux = -1;
    ps->ytype[0] = '\0';
    ps->xmax = -1000000.;
    ps->xmin = 1000000.;
    ps->ymax = -1000000.;
    ps->ymin = 1000000.;
    ps->toffset = 0;
    for (i=0; i<4; i++) ps->vport[i] = ps->window[i] = 0.0;
    for (i=0; i<MAXPLT; i++) ps->index[i] = ps->symbol[i] = 0;
    ps->frq = '\0';
    ps->expt = 0;
    ps->source[0] = '\0';
    ps->station = '\0';
    ps->bas[0] = '\0';
    ps->triangle[0] = '\0';
    ps->ampcl[0] = '\0';
    }
