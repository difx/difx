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

int
ps_inside (x, y, xmin, xmax, ymin, ymax)
float x, y, xmin, xmax, ymin, ymax;
    {
    if ((x < xmin) || (x > xmax)) return (FALSE);
    if ((y < ymin) || (y > ymax)) return (FALSE);
    return (TRUE);
    }
