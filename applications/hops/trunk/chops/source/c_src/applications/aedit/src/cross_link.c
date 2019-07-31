/************************************************************************/
/*									*/
/* Figures out family relationships between record types		*/
/*									*/
/*	Inputs:		data						*/
/*									*/
/*	Output:		data		suitably modified		*/
/*									*/
/* Created March 2 1994 by CJL						*/
/* Dumb slow version, couldn't make sorted fast version work in a	*/
/* reasonably short time ... for a later release			*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "aedit.h"

int cross_link (esum *data)
    {
    extern int rscan, cscan, fscan;
    fringearray *fdata;
    corelarray *cdata;
    rootarray *rdata;
    int i, j;

    rdata = data->rdata;
    cdata = data->cdata;
    fdata = data->fdata;
					/* Corel first */
    for (i=0; i<cscan; i++)
	{
	cdata[i].parent_root = -1;
	if (cdata[i].flag) continue;

	for (j=0; j<rscan; j++)
	    {
	    if (rdata[j].flag) continue;

	    if (strcmp (rdata[j].data.root_id, cdata[i].data.root_id) == 0)
		{
		cdata[i].parent_root = j;
		break;
		}
	    }
	}
					/* Then fringe */
    for (i=0; i<fscan; i++)
	{
	fdata[i].parent_root = -1;
	fdata[i].parent_corel = -1;
	if (fdata[i].flag) continue;

	for (j=0; j<rscan; j++)
	    {
	    if (rdata[j].flag) continue;

	    if (strcmp (rdata[j].data.root_id, fdata[i].data.root_id) == 0)
		{
		fdata[i].parent_root = j;
		break;
		}
	    }

	for (j=0; j<cscan; j++)
	    {
	    if (cdata[j].flag) continue;

	    if (strcmp (cdata[j].data.root_id, fdata[i].data.root_id) != 0)
		continue;
	    if (fdata[i].data.parents[0] != cdata[j].data.extent_no) continue;
	    fdata[i].parent_corel = j;
	    }
	}

    return (0);
    }
