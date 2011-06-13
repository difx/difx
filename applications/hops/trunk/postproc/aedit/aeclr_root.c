/************************************************************************/
/*									*/
/* This routine simply zeroes out the aedit-specific part of the data	*/
/* array.  Necessary because of the separation into aedit-specific and	*/
/* generic parts mandated by proper encapsulation of the AFIO library	*/
/*									*/
/*	Inputs:		rdatum		rootarray structure element	*/
/*									*/
/*	Output:		rdatum		cleared out			*/
/*									*/
/* Created October 18 1993 by CJL					*/
/*									*/
/************************************************************************/
#include "aedata.h"

void
aeclr_root (rdatum)
rootarray *rdatum;
    {
    rdatum->order = 0;
    rdatum->lastorder = 0;
    rdatum->keyval = 0;
    rdatum->flag = 0;
    }
