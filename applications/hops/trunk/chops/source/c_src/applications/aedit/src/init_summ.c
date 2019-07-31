/************************************************************************/
/*									*/
/* Initializes a summary structure.  This is necessary because we need	*/
/* to do some memory allocation.  Also some pointers need to be nulled	*/
/* because free() calls in clear_summ depend on it.			*/
/*									*/
/*	Inputs:		summ		summary structure		*/
/*			type		determines # of btq elements	*/
/*									*/
/*	Output:		summ		allocated/initialized		*/
/*									*/
/* Created 25 August 1994 by CJL					*/
/*									*/
/************************************************************************/
#include <stdlib.h>
#include "summary.h"
#include "aedit.h"

void init_summ (struct datasumm *summ, int type)
    {
    int i;
    
    summ->btq_allocated = FALSE;
    alloc_btq (&(summ->btq), &(summ->btq_allocated), type);

    for (i=0; i<MAXBANDS * MAXEXPTS; i++) 
	{
	summ->fqex[i].btq_allocated = FALSE;
	summ->fqex[i].slist_allocated = FALSE;
	}

    return;
    }
