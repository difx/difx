/************************************************************************/
/*									*/
/* This routine identifies all baselines in the main fringe data	*/
/* array for this scan/source/frequency/experiment, and forms all the	*/
/* required closure triangles from them.  It places the resulting	*/
/* closure data in the tdata array.  This will typically be immediately	*/
/* summarized by summ_tdata() to provide information for plotting.	*/
/*									*/
/*	Inputs:		nfe		index into fqex structure array */
/*			data		Pointer to main data array	*/
/*			dptr		index to current fdata location */
/*									*/
/*	Output:		more data in the tdata array			*/
/*			dptr		Updated fdata location		*/
/*									*/
/* Created 12 February 1992 by CJL					*/
/* Modified for full closure support 23 August 1994 by CJL		*/
/*									*/
/************************************************************************/
#include "summary.h"
#include "aedata.h"
#include <stdio.h>
#include <string.h>

int
fill_tdata (nfe, data, dptr)
int nfe, *dptr;
esum *data;
    {
    int i, j, k, index, snum, nt, inbreak, outbreak, nstat, maxtri;
    int indices[3], n, expt_no, cspace, time_tag, tindex;
    char freq_code, sname[9];
    struct frqexp *fqptr;
    fringearray *fdata;
    struct { int index; char baseline[3]; } blist[MAXBASE];
    struct { int present; char triangle[4]; } trilist[MAXCLOSE];
    extern struct datasumm fsumm;
    extern int fscan, tscan;
					/* Identify starting point & initialize */
    fdata = data->fdata;
    index = fdata[*dptr].order;
    strcpy (sname, fdata[index].data.source);
    expt_no = fsumm.fqex[nfe].expt_no;
    freq_code = fsumm.fqex[nfe].freq_code;
					/* Assign convenience pointer */
    fqptr = fsumm.fqex + nfe;
					/* Empty space for list of all poss. tri. */
    nstat = strlen (fqptr->stations);
    if (nstat < 3) return (0);
    maxtri = (nstat * (nstat-1) * (nstat-2)) /6;
    for (i=0; i<maxtri; i++) 
	{
	trilist[i].present = FALSE;
	trilist[i].triangle[0] = '\0';
	trilist[i].triangle[1] = '\0';
	trilist[i].triangle[2] = '\0';
	trilist[i].triangle[3] = '\0';
	}
					/* Generate list of all possible tri. */
    nt = 0;
    for (i=0; i<nstat-2; i++)
	{
	for (j=i+1; j<nstat-1; j++)
	    {
	    for (k=j+1; k<nstat; k++)
		{
		trilist[nt].triangle[0] = fqptr->stations[i];
		trilist[nt].triangle[1] = fqptr->stations[j];
		trilist[nt].triangle[2] = fqptr->stations[k];
		nt++;
		}
	    }
	}

    outbreak = FALSE;
    while (TRUE)		/* Loop until outbreak is set true by end of */
	{			/* source/freq/expt block */
	n = 0;
	inbreak = FALSE;
					/* Loop over data until end of sorted block */
					/* with same time/source/freq/expt */
	time_tag = fdata[fdata[*dptr].order].data.time_tag;
	for (i = *dptr; i<fscan; i++)
	    {
	    index = fdata[i].order;
	    if (fdata[index].flag != 0) continue;
	    if (fdata[index].data.time_tag != time_tag) inbreak = TRUE;
	    if (strcmp (fdata[index].data.source, sname) != 0) outbreak = TRUE;
	    if (fdata[index].data.freq_code != freq_code) outbreak = TRUE;
	    if (fdata[index].data.expt_no != expt_no) outbreak = TRUE;
	    if (inbreak || outbreak) break;
					/* Accumulate local summary this scan */
	    blist[n].index = index;
	    strcpy (blist[n].baseline, fdata[index].data.baseline);
	    n++;
	    }				/* Update pointer for next scan */
	*dptr = i;
					/* Loop over all possible triangles */
	for (j=0; j<maxtri; j++)
	    {				/* Does this scan contain this triangle? */
	    if (! trngl_present (trilist[j].triangle, blist, n, indices)) continue;
					/* Get next free trianglearray index */
	    tindex = tarray_index (data);
					/* Do the actual calculation here */
	    if (fill_closure (data->tdata + tindex,
				trilist[j].triangle, indices, fdata) != 0)
		{
		msg ("error filling closure structure", 2);
		if (tscan > 0) tscan--;
		continue;
		}
	    trilist[j].present = TRUE;
					/* This is the place to put triangle */
					/* redundancy information in */
	    }
					/* Quit on end of data */
	if (*dptr >= fscan) break;
					/* Loop back for next scan unless we're done */
	if (outbreak) break;
	}
					/* All done! */
    return (0);
    }
