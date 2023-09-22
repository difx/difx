/************************************************************************/
/*									*/
/* Simple routine to free up allocated memory in a ps_array structure	*/
/* Other routines must be sure to set or unset 'alloced' structure	*/
/* members as appropriate.						*/
/*									*/
/*	Inputs:		psarray		possibly allocated ps struct	*/
/*									*/
/*	Output:		psarray		unallocated struct		*/
/*									*/
/* Created 18 February 1993 by CJL					*/
/*									*/
/************************************************************************/
#include <stdlib.h>
#include "psplot.h"

void ps_free (struct ps_array *psarray)
    {
    int i;

    for (i=0; i<MAXBASE; i++)
	{
	if (psarray->baseline[i].alloced) free (psarray->baseline[i].scan);
	psarray->baseline[i].alloced = FALSE;
	}
    }
