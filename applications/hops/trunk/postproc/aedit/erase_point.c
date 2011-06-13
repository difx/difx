/************************************************************************/
/*									*/
/* This is the routine which erases zapped points from the graphics	*/
/* display in the EDIT CURSOR command.  It tries to overwrite the	*/
/* symbol in the background colour causing it to disappear, but if the	*/
/* device cannot write in this colour, it places a large black square	*/
/* on top of the point.							*/
/*									*/
/*	Inputs:		x, y		device coordinates of point	*/
/*			xh,xl,yh,yl	Error bar information.		*/
/*			symbol		Pgplot symbol to overwrite with */
/*									*/
/*	Output:		None						*/
/*									*/
/* Created 4/26/90 by CJL						*/
/* Removed flagging from this routine Sept 6 1994, CJL			*/
/*									*/
/************************************************************************/
#include "cpgplot.h"

void
erase_point (x, y, xh, xl, yh, yl, symbol)
float x, y, xh, xl, yh, yl;
int symbol;
    {
    int clow, chigh, colour, flagged;
    float charsize, dumx, dumy, errl, errh;

    dumx = x; dumy = y;
    cpgqcol (&clow, &chigh);
    if(clow == 0) 
	{
					/* Erase is possible */
	cpgqci (&colour);
	cpgsci (0);
	cpgpt (1, &dumx, &dumy, symbol);
					/* Erase error bars if present */
	if ((xh - xl) > 0.0) 
	    {
	    errh = xh;
	    errl = xl;
	    cpgerrx (1, &errl, &errh, &dumy, 0.0);
	    }
	if ((yh - yl) > 0.0) 
	    {
	    errh = yh;
	    errl = yl;
	    cpgerry (1, &dumx, &errl, &errh, 0.0);
	    }
	cpgsci(colour);
	}
					/* OK, write a big square instead */
    else 
	{
	cpgqch (&charsize);
	cpgsch(1.7*charsize);
	cpgpt(1,&dumx,&dumy,16);
	cpgsch(charsize);
	}

    return;
    }
