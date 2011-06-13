/************************************************************************/
/*									*/
/* Performs the operations needed to render a dataset internally 	*/
/* consistent vis-a-vis complete families of files (root, corel and 	*/
/* fringe) being represented.						*/
/*									*/
/*	Inputs:		data		all types			*/
/*			mode		0=parents, 1=children		*/
/*	
/*									*/
/*	Output:		nedit1, nedit2	Number pts flagged in this op.	*/
/*									*/
/* Created 2 March 1994 by CJL						*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "aedata.h"
#include "flags.h"

void
edit_families (data, mode, nedit1, nedit2)
esum *data;
int mode, *nedit1, *nedit2;
    {
    extern int fscan, fflag, cscan, cflag, rscan, rflag;
    fringearray *fdata;
    corelarray *cdata;
    rootarray *rdata;
    int *neditf, *neditc, *neditr, nr, nc, nf, i, index;

    if (mode == 0)
	{
	neditc = nedit1;
	neditr = nedit2;
	}
    else if (mode == 1)
	{
	neditf = nedit1;
	neditc = nedit2;
	}
    else
	{
	msg ("Bad mode argument in edit_families()", 2);
	return;
	}
					/* Make links between types */
    if (cross_link (data) != 0)
	{
	msg ("Error making cross-links in edit_families()", 2);
	return;
	}
					/* Convenience */
    fdata = data->fdata;
    cdata = data->cdata;
    rdata = data->rdata;
    nr = nc = nf = 0;
					/* Parent case first */
    if (mode == 0)
	{
	for (i=0; i<rscan; i++) rdata[i].flag |= CHILDLESS;
	for (i=0; i<cscan; i++) cdata[i].flag |= CHILDLESS;
	for (i=0; i<fscan; i++)
	    {
	    if (fdata[i].flag) continue;
	    if ((index = fdata[i].parent_root) >= 0)
			rdata[index].flag &= ~CHILDLESS;
	    if ((index = fdata[i].parent_corel) >= 0)
			cdata[index].flag &= ~CHILDLESS;
	    }
	for (i=0; i<cscan; i++)
	    {
	    if (cdata[i].flag) continue;
	    if ((index = cdata[i].parent_root) >= 0)
			rdata[index].flag &= ~CHILDLESS;
	    }
					/* Figure out what got flagged */
	for (i=0; i<rscan; i++) if (rdata[i].flag) nr++;
	for (i=0; i<cscan; i++) if (cdata[i].flag) nc++;
	*neditr = nr - rflag;
	*neditc = nc - cflag;
	}
					/* Now child case */
    else
	{
	for (i=0; i<cscan; i++)
	    {
	    if (cdata[i].flag) continue;
	    if (cdata[i].parent_root < 0) cdata[i].flag |= ORPHAN;
	    }
					/* Should be OK with this.  If */
					/* corel datum got zapped, it will */
					/* be parent of fringe datum about */
					/* to be zapped */
	for (i=0; i<fscan; i++)
	    {
	    if (fdata[i].flag) continue;
	    if ((fdata[i].parent_corel < 0) && (fdata[i].parent_root < 0))
		fdata[i].flag |= ORPHAN;
	    }
					/* Figure out what got flagged */
	for (i=0; i<cscan; i++) if (cdata[i].flag) nc++;
	for (i=0; i<fscan; i++) if (fdata[i].flag) nf++;
	*neditc = nc - cflag;
	*neditf = nf - fflag;
	}

    return;
    }
