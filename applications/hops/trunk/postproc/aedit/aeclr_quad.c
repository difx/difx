/************************************************************************/
/*									*/
/* This routine simply zeroes out the aedit-specific part of the data	*/
/* array.  Necessary because of the separation into aedit-specific and	*/
/* generic parts mandated by proper encapsulation of the AFIO library	*/
/*									*/
/*	Inputs:		qdatum		quadarray structure element	*/
/*									*/
/*	Output:		qdatum		cleared out			*/
/*									*/
/* Created August 9 1994 by CJL						*/
/*									*/
/************************************************************************/
#include "aedata.h"

void
aeclr_quad (qdatum)
quadarray *qdatum;
    {
    int i;

    qdatum->order = 0;
    qdatum->lastorder = 0;
    qdatum->keyval = 0;
    qdatum->flag = 0;
    for (i=0; i<6; i++) qdatum->index[i] = 0;
    }
