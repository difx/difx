/************************************************************************/
/*									*/
/* This calculates the closure quantity numbers, and associated errors  */
/* and/or "suspect data" flags, placing the information in dynamically  */
/* allocated tdata. The actual insertion of the data into the tdata 	*/
/* array is done for each individual exp/freq/source by fill_tdata(). 	*/
/*									*/
/*	Inputs:		data		Main data structure		*/
/*									*/
/*	Output:		Filled tdata structure array			*/
/*									*/
/* Created 11 February 1992 by CJL					*/
/* Lots of mods, culminating in full closure support 23/8/94 CJL	*/
/*									*/
/************************************************************************/
#include "aedata.h"
#include "summary.h"
#include <stdio.h>
#include <string.h>

int
calc_close (data)
esum *data;
    {
    int i, j, k, index, ret, save_sstat[10];
    short *save_sort, *save_last;
    fringearray *fdata;
    extern int fscan, fsortstat[10];
    extern struct datasumm fsumm;

    fdata = data->fdata;
					/* Preserve original sort order/status */
    save_sort = (short *)malloc (fscan * 2);
    save_last = (short *)malloc (fscan * 2);
    if ((save_sort == NULL) || (save_last == NULL))
	{
	msg ("malloc failure in calc_close()",2);
	return (-1);
	}
    for (i=0; i<fscan; i++) 
	{
	save_sort[i] = fdata[i].order;
	save_last[i] = fdata[i].lastorder;
	}
    for (i=0; i<=fsortstat[0]; i++) save_sstat[i] = fsortstat[i];

					/* Now sort into needed order */
    fsortstat[0] = 0;			/* First remove existing sort order */
    ret = 0;				/* And then sort four ways */
    summ_data (data, VERSION);		/* For init */
    if (sorter(fdata, "timetag", 2) != 0) ret = -1;
    else if (sorter(fdata, "source", 2) != 0) ret = -1;
    else if (sorter(fdata, "frequency", 2) != 0) ret = -1;
    else if (sorter(fdata, "experiment", 2) != 0) ret = -1;
    if (ret != 0)
	{
	msg ("Error sorting data in calc_close()", 2);
	return (-1);
	}

    for (i=0; i<fsumm.nfqex; i++)	/* Loop over all expts/freqs */
	{
	for (j=0; j<fscan; j++)		/* Fast forward until find expt/freq */
	    {
	    index = fdata[j].order;
	    if (fdata[index].flag != 0) continue;
	    if ((fdata[index].data.expt_no == fsumm.fqex[i].expt_no)
		    && (fdata[index].data.freq_code == fsumm.fqex[i].freq_code)) break;
	    }
			/* This loop does the work, fill_tdata updates data pointer */
			/* j to point at next source.  Error jumps to next */
			/* expt/freq combination.  Thus, fill_tdata will step */
			/* through all the sources for this expt/freq, one by one */
	for (k=0; k<fsumm.fqex[i].nsource; k++) 
	    {
	    if (fill_tdata (i, data, &j) != 0)	/* Uses fsumm internally */
		{
		msg ("Error processing closure quantities for expt %d, freq %c",2,
			fsumm.fqex[i].expt_no, fsumm.fqex[i].freq_code);
		break;
		}
	    }
	}			/* That should have filled in the closure quantity */
				/* data array.  It must be summarized before */
				/* plotting */

					/* Restore original sort order */
    for (i=0; i<fscan; i++) 
	{
	fdata[i].order = save_sort[i];
	fdata[i].lastorder = save_last[i];
	}
    for (i=0; i<=save_sstat[0]; i++) fsortstat[i] = save_sstat[i];
    free (save_sort);
    free (save_last);

    return (0);
    }
