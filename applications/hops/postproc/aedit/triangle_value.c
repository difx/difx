/************************************************************************/
/*									*/
/* Extracts numeric values for closure quantities and errors from the	*/
/* closure array element supplied.  Low-level routine to support	*/
/* plotting and cursor identification of points.			*/
/*									*/
/*	Inputs:		index		Axis type specifier		*/
/*			ta		pointer to closure array element*/
/*			toffset		To make time_tag relative	*/
/*									*/
/*	Output:		value		Pointer to receive result	*/
/*			errh, errl	Pointers to error bar extrema	*/
/*									*/
/* Created 22 February 1994 by CJL					*/
/* Modified to use closure A-file data, 6 August 1994 by CJL		*/
/*									*/
/************************************************************************/
#include "aedata.h"
#include "pstruct.h"
#include "aedit.h"

void triangle_value (int index, trianglearray *ta, int toffset,
    float *value, float *errh, float *errl)
    {
    float error;

    switch (index)
	{
	case AX_TIMETAG:
	    *value = ta->data.time_tag - toffset;
	    break;
	case AX_CPHASE:
	    *value = ta->data.bis_phas;
	    if (ta->data.bis_snr == 0.0) error = 180.0;
	    else error = 57.3 / ta->data.bis_snr;
	    if (error > 180.0) error = 180.0;
	    *errh = *value + error;
	    *errl = *value - error;
	    break;
	case AX_CRATE:
	    *value = ta->data.cdelay_rate;
	    break;
	case AX_CSBDELAY:
	    *value = ta->data.csbdelay;
	    break;
	case AX_CMBDELAY:
	    *value = ta->data.cmbdelay;
	    break;
	default:
	    msg ("Bad axis in triangle_value(), %d", 2, index);
	}
    return;
    }
