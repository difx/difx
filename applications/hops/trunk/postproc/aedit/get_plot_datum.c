/************************************************************************/
/*									*/
/* Low level routine to figure out the axis values for a data point	*/
/* for particular axis types.  For certain variables, the SNR-		*/
/* based error bars are also calculated.				*/
/*									*/
/*	Inputs:		plot_id		Identifies station if needed	*/
/*			data		Pointer to main data array 	*/
/*			sptr		Pointer srcsum element of fqex	*/
/*			ptr		index into relevant data array	*/
/*			n		Index into plot data arrays	*/
/*			toffset		For time axes only		*/
/*									*/
/*	Output:		pt		Pointer to plot data structure	*/
/*					has next element of y, errh,	*/
/*					errl arrays filled in		*/
/*									*/
/* Created 26 February 1992 by CJL					*/
/* Generalized for x-y plotting and moved actual extraction of data	*/
/* values to lower-level generic routines, 22 February 1994 by CJL	*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "aedata.h"
#include "aedit.h"
#include "pstruct.h"
#include "summary.h"

void get_plot_datum (char *plot_id, esum *data, srcsum *sptr,
    int ptr, int n, int toffset, struct plot_ptqual *pt)
    {
    extern struct inputs inp;
    float *value, *errl, *errh;
    int i, index, aux, refrem, tindex, qindex;
    fringearray *fdatum;
    trianglearray *tdatum;
    quadarray *qdatum;
					/* Do it twice, 1 for x, 1 for y */
    for (i=0; i<2; i++)
	{
					/* Set up pointers to plot array */
					/* elements we wish to fill */
	if (i == 0)
	    {
	    index = inp.xaind;
	    aux = inp.x_aux;
	    value = pt->x + n;
	    errh = pt->xerrh + n;
	    errl = pt->xerrl + n;
	    }
	else
	    {
	    index = inp.yaind;
	    aux = inp.y_aux;
	    value = pt->y + n;
	    errh = pt->yerrh + n;
	    errl = pt->yerrl + n;
	    }
					/* Initialize */
	*value = *errh = *errl = 0.0;
					/* Figuring out the plot values */
					/* is done not only when generating */
					/* the plot, but also when */
					/* identifying points by cursor, so */
					/* do the actual extraction in */
					/* generic subroutines */
	switch (inp.plotby)
	    {
					/* If this is a station plot must */
					/* figure out which station, ref */
					/* or remote, is needed */
	    case STATION_PLOT:
		fdatum = data->fdata + ptr;
		if (plot_id[0] == fdatum->data.baseline[0]) refrem = REFERENCE;
		else refrem = REMOTE;
		datum_value (index, aux, toffset, refrem, fdatum, value, errh, errl);
		break;
	    case BASELINE_PLOT:
		refrem = inp.refrem;
	    case ALL_PLOT:
		fdatum = data->fdata + ptr;
		datum_value (index, aux, toffset, refrem, fdatum, value, errh, errl);
		break;
	    case TRIANGLE_PLOT:
		tdatum = data->tdata + ptr;
		triangle_value (index, tdatum, toffset, value, errh, errl);
		break;
	    case QUAD_PLOT:
		qdatum = data->qdata + ptr;
		if (index == AX_CAMP)
		    *value = qdatum->data.cl_amp;
		else if (index == AX_TIMETAG)
		    *value = qdatum->data.time_tag - toffset;
		break;
	    default:
		msg ("Bad plot type in get_plot_datum(), %d", 2, inp.plotby);
	    }
	}
    }
