/************************************************************************/
/*									*/
/* This routine simply zeroes out the aedit-specific part of the data	*/
/* array.  Necessary because of the separation into aedit-specific and	*/
/* generic parts mandated by proper encapsulation of the AFIO library	*/
/*									*/
/*	Inputs:		cdatum		corelarray structure element	*/
/*									*/
/*	Output:		cdatum		cleared out			*/
/*									*/
/* Created October 18 1993 by CJL					*/
/*									*/
/************************************************************************/
#include "aedata.h"

void aeclr_corel (corelarray *cdatum)
    {
    cdatum->order = 0;
    cdatum->lastorder = 0;
    cdatum->keyval = 0;
    cdatum->flag = 0;
    cdatum->parent_root = -1;
    }
