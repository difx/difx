/************************************************************************/
/*									*/
/* This routine simply zeroes out the aedit-specific part of the data	*/
/* array.  Necessary because of the separation into aedit-specific and	*/
/* generic parts mandated by proper encapsulation of the AFIO library	*/
/*									*/
/*	Inputs:		tdatum		trianglearray structure element	*/
/*									*/
/*	Output:		tdatum		cleared out			*/
/*									*/
/* Created August 9 1994 by CJL						*/
/*									*/
/************************************************************************/
#include "aedata.h"

void
aeclr_triangle (tdatum)
trianglearray *tdatum;
    {
    int i;

    tdatum->order = 0;
    tdatum->lastorder = 0;
    tdatum->keyval = 0;
    tdatum->flag = 0;
    for (i=0; i<3; i++) tdatum->index[i] = -1;
    tdatum->reversed = 0;
    }
