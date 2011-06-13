/************************************************************************/
/*									*/
/* This routine checks the index of the next free slot in the main	*/
/* closure triangle data array.  If no more slots are available, it	*/
/* will dynamically allocate more memory.  This is a good idea because	*/
/* unlike baseline-oriented data which has a single point of entry into	*/
/* aedit, triangle (and quad) data can be generated internally.  We	*/
/* need to isolate allocation chores for these arrays.			*/
/*									*/
/*	Inputs:		data		pointer to main data array 	*/
/*									*/
/*	Output:		data->tdata	Possibly reallocated		*/
/*			return value    Index of next slot, <0 is error	*/
/*									*/
/* Created August 3 1994 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "aedata.h"

int
tarray_index (esum *data)
    {
    extern int tscan, tspace;

    tscan++;
    if (tscan >= tspace)
	{
	tspace += 50;
	data->tdata = (trianglearray *) realloc (data->tdata, 
						tspace*sizeof(trianglearray));
	if (data->tdata == NULL) 
	    {
	    perror ("realloc");
	    msg ("Fatal error allocating memory for closure triangles", 3);
	    tscan = -1;
	    }
	}
    return (tscan - 1);
    }
