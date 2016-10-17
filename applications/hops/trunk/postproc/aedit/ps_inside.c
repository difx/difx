/************************************************************************/
/*									*/
/* Trivial routine to check if x,y coordinates lie inside a rectangular	*/
/* region or not.  Intended for use in conjunction with psplot.h which	*/
/* has various button rectangles defined				*/
/*									*/
/*	Inputs:		x, y, and rectangle coordinates			*/
/*									*/
/*	Output:		return value		TRUE if inside, FALSE	*/
/*						if not			*/
/*									*/
/* Created 18 February 1993 by CJL					*/
/*									*/
/************************************************************************/
#define FALSE 0
#define TRUE 1
#include "aedit.h"

int ps_inside (float x, float y, float xmin, float xmax, float ymin, float ymax)
    {
    if ((x < xmin) || (x > xmax)) return (FALSE);
    if ((y < ymin) || (y > ymax)) return (FALSE);
    return (TRUE);
    }
