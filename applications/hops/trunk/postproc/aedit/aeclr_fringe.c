/************************************************************************/
/*									*/
/* This routine simply zeroes out the aedit-specific part of the data	*/
/* array.  Necessary because of the separation into aedit-specific and	*/
/* generic parts mandated by proper encapsulation of the AFIO library	*/
/*									*/
/*	Inputs:		fdatum		fringearray structure element	*/
/*									*/
/*	Output:		fdatum		cleared out			*/
/*									*/
/* Created October 18 1993 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "aedata.h"

void
aeclr_fringe (fdatum)
fringearray *fdatum;
    {
    fdatum->order = 0;
    fdatum->lastorder = 0;
    fdatum->keyval = 0;
    fdatum->flag = 0;
    fdatum->parent_corel = -1;
    fdatum->parent_root = -1;
    fdatum->param_ptr = -1;
    }
