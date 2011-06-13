/************************************************************************/
/*									*/
/* Low-level routine to figure out the appropriate scales for an axis,	*/
/* taking into account different defaults for different axis types.	*/
/*									*/
/*	Inputs:		index			Axis type		*/
/*			min, max		Extrema of data		*/
/*									*/
/*	Output:		win1, win2		Ready for pg_window()	*/
/*									*/
/* Created 22 February 1994 by CJL					*/
/*									*/
/************************************************************************/
#include <math.h>
#include "pstruct.h"

void
axis_scale (index, min, max, win1, win2)
int index;
float min, max, *win1, *win2;
    {
    float diff;
					/* Basic range plus 10% either end */
    diff = max - min;
    *win1 = min - 0.1*diff;
    *win2 = max + 0.1*diff;
					/* Scantime, degenerate => 10mins */
    if (index == AX_TIMETAG)
	if (*win1 == *win2)
	    {
	    *win1 -= 300.0;
	    *win2 += 300.0;
	    }
					/* Break degeneracy in generic way */
    if (*win1 == *win2)
	{
	diff = fabs ((double)max);
	if (diff == 0.0) diff = 1.0;
	*win1 = min - diff;
	*win2 = max + diff;
	}
					/* U and V should center around 0 */
    if ((index == AX_U) || (index == AX_V))
	{
	max = fabs ((double)*win1);
	if (fabs ((double)*win2) > max) max = fabs ((double)*win2);
	*win1 = -max;
	*win2 = max;
	}
					/* Phase-like quantities need */
					/* special attention */
/*    if ((index == AX_PHASE) || (index == AX_PCAL_PHASE) 
		|| (index == AX_PCAL_DIFF) || (index == AX_AZIMUTH)) */
					/* Temp fix for BEC */
    if ((index == AX_PHASE) || (index == AX_PCAL_PHASE) 
		|| (index == AX_AZIMUTH))
	{
	*win1 = 0.0;
	*win2 = 360.0;
	}
    else if (index == AX_CPHASE)
	{
	*win1 = -180.0;
	*win2 = 180.0;
	}
    else if (index == AX_ELEVATION)
	{
	*win1 = 0.0;
	*win2 = 90.0;
	}

    return;
    }
