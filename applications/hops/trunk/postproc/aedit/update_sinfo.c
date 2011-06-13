/************************************************************************/
/*									*/
/* A trivial routine for upkeep of an alphabetical list of sources	*/
/* present in a summary structure.  Does an insertion sort.		*/
/*									*/
/*	Inputs:		sinfo		structure to modify		*/
/*			source		name of source to update with	*/
/*			nsrc		current # of entries in sinfo	*/
/*									*/
/*	Output:		sinfo		appropriately modified		*/
/*			return value	0=no sources added, 1=1 added	*/
/*									*/
/* Created 14 February 1992 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "summary.h"

int
update_sinfo (sinfo, source, nsrc)
struct source_info *sinfo;
char *source;
int nsrc;
    {
    int i, n, cmp, ret;

    n = nsrc;
    while (n)
	{
	cmp = strcmp (sinfo[n-1].name, source);
	if (cmp == 0)			/* Match ... this is another occurrence */
	    {				/* of existing source .. increment count */
	    sinfo[n-1].count++;
	    ret = 0;
	    break;
	    }
	else if (cmp < 0)		/* A new source ... insert here */
	    {
	    for (i=nsrc; i>n; i--)
		{
		sinfo[i].count = sinfo[i-1].count;
		strcpy (sinfo[i].name, sinfo[i-1].name);
		}
	    strcpy (sinfo[n].name, source);
	    sinfo[n].count = 1;
	    ret = 1;
	    break;
	    }
	n--;
	}

    if (n == 0)				/* Special case ... first source in list */
	{
	for (i=nsrc; i>n; i--)
            {
            sinfo[i].count = sinfo[i-1].count;
            strcpy (sinfo[i].name, sinfo[i-1].name);
            }
	sinfo[0].count = 1;
	strcpy (sinfo[0].name, source);
	ret = 1;
	}

    return (ret);
    }
