/************************************************************************/
/*									*/
/* This routine checks the index of the next free slot in the main	*/
/* closure quad data array.  If no more slots are available, it		*/
/* will dynamically allocate more memory.  This is a good idea because	*/
/* unlike baseline-oriented data which has a single point of entry into	*/
/* aedit, triangle (and quad) data can be generated internally.  We	*/
/* need to isolate allocation chores for these arrays.			*/
/*									*/
/*	Inputs:		data		pointer to main data array 	*/
/*									*/
/*	Output:		data->qdata	Possibly reallocated		*/
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
qarray_index (data)
esum *data;
    {
    extern int qscan, qspace;

    qscan++;
    if (qscan >= qspace)
	{
	qspace += 50;
	data->qdata = (quadarray *) realloc (data->qdata, 
						qspace*sizeof(quadarray));
	if (data->qdata == NULL) 
	    {
	    perror ("realloc");
	    msg ("Fatal error allocating memory for closure quads", 3);
	    qscan = -1;
	    }
	}
    return (qscan);
    }
