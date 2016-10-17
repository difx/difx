/************************************************************************/
/*                                                                      */
/* This routine actually draws the data on the plot device via calls    */
/* to PGPLOT drawing routines.  It draws in 3 colours to indicate the   */
/* data quality.                                                        */
/*                                                                      */
/*      Inputs:         pd              plot-data structure pointer     */
/*                      pp              Set of point arrays to plot     */
/*                      symbol          pgplot symbols to use           */
/*                                                                      */
/*      Output:         on the plotting device                          */
/*                                                                      */
/* Created 23 February 1994 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "cpgplot.h"
#include "pstruct.h"
#include "aedit.h"

void plot_points (struct plot_info *pd, struct plot_points *pp, int symbol[3])
    {
    struct plot_ptqual *pt;
                                        /* Green for good */
    cpgsci (3);
    pt = &(pp->good);
    cpgpt (pd->ngood, pt->x, pt->y, symbol[0]);
    if (pd->xebar) cpgerrx (pd->ngood, pt->xerrl, pt->xerrh, pt->y, 0.0);
    if (pd->yebar) cpgerry (pd->ngood, pt->x, pt->yerrl, pt->yerrh, 0.0);

                                        /* Orange for suspect */
    cpgsci (8);
    pt = &(pp->suspect);
    cpgpt (pd->nsusp, pt->x, pt->y, symbol[1]);
    if (pd->xebar) cpgerrx (pd->nsusp, pt->xerrl, pt->xerrh, pt->y, 0.0);
    if (pd->yebar) cpgerry (pd->nsusp, pt->x, pt->yerrl, pt->yerrh, 0.0);

                                        /* Red for bad */
    cpgsci (2);
    pt = &(pp->bad);
    cpgpt (pd->nbad, pt->x, pt->y, symbol[2]);
    if (pd->xebar) cpgerrx (pd->nbad, pt->xerrl, pt->xerrh, pt->y, 0.0);
    if (pd->yebar) cpgerry (pd->nbad, pt->x, pt->yerrl, pt->yerrh, 0.0);

    cpgsci (1);
    return;
    }
