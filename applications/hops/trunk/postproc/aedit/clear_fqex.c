/************************************************************************/
/*									*/
/* Initializes fqex structures, part of summarization code		*/
/*									*/
/*	Inputs:		fqex		Structure to be cleared		*/
/*									*/
/*	Output:		fqex		Cleared				*/
/*									*/
/* Created 22 August 1994 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "summary.h"
#include "aedit.h"

void clear_fqex (struct frqexp *fqex)
    {
    int i;

    fqex->freq_code = ' ';
    fqex->expt_no = 0;
    fqex->begin = 0;
    fqex->end = 0;
    fqex->stations[0] = '\0';
    fqex->nbtq = 0;
    if (fqex->btq_allocated) 
	{
	free (fqex->btq);
	fqex->btq_allocated = FALSE;
	}
    if (fqex->slist_allocated) 
	{
	for (i=0; i<fqex->nsource; i++) clear_source (fqex->slist + i);
	free (fqex->slist);
	fqex->slist_allocated = FALSE;
	}
    fqex->nsource = 0;

    return;
    }
