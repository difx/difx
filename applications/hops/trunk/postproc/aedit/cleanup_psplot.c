/************************************************************************/
/*									*/
/* Simple tidy up routine						*/
/*									*/
/*	Inputs:		psarray		only for retain variable 	*/
/*									*/
/*	Output:		none						*/
/*									*/
/* Created 22 February 1993 by CJL					*/
/*									*/
/************************************************************************/
#include "cpgplot.h"
#include "psplot.h"

void
cleanup_psplot (psarray)
struct ps_array *psarray;
    {
    extern int psplot_open;

    ps_free (psarray);
    psarray->displayed = FALSE;
    if (psplot_open == FALSE) return;
    if (psarray->retain) psplot_open = TRUE;
    else 
	{
	psplot_open = FALSE;
	cpgend ();
	}
    }
