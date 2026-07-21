/************************************************************************/
/*									*/
/* This routine sorts data read by average.  A shell			*/
/* sort is performed, but with no physical rearrangement of the		*/
/* data array.  Instead, the "order" element of the data structure is	*/
/* filled with the number of the scan which would occupy this slot in 	*/
/* the sorted order.							*/
/*									*/
/* This algorithm was taken from RSORT, an HP1000 program by RJC, which	*/
/* in turn was based on Knuth's sorting and searching volume, p.85,	*/
/* Algorithm D.  The implementation here has an added piece of code to	*/
/* stabilize the sort, using the "lastorder" element of the data	*/
/* structure to break ties in the comparison of data values.		*/
/*									*/
/*	Inputs:		data		Main data array, keyval filled	*/
/*			nseg		number of records		*/
/*									*/
/*	Output:		data		order, lastorder modified	*/
/*									*/
/* Created September 9 1994 by CJL, stolen from aedit (much simplified) */
/*									*/
/************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include "average.h"

void
sorter (seg_data *data,
        int nseg)
    {
    int i, j, k, ival, jval, jptr, interval, npass;
    int lasti, lastj;
					/* Save current order in lastorder */
					/* Note that lastorder is not a simple */
					/* copy of order.  It is the position */
					/* in the sorted array that this record */
					/* would have, not the index of the */
					/* record that would occupy this slot in */
					/* the sorted array */
    for (i=0; i<nseg; i++)
	data[data[i].order].lastorder = i;
					/* Find coarsest sort interval */
					/* and passes required */
    interval = 1; npass = 0;
    while ((interval = 3*interval + 1) <= nseg) npass++;
    interval = interval/3;
    if (interval < 4) interval = 4;
    if (npass < 1) npass = 1;
					/* Actually do the sort */
    for (k=0; k<npass; k++) 
	{
					/* Progressively shorter intervals */
	interval /= 3;
	for (j=interval; j<nseg; j++) 
	    {
					/* All done with pointers in */
					/* order element of data array */
	    jptr = data[j].order;
	    jval = data[jptr].keyval;
	    i = j - interval;
		 			/* Loop till we find slot for j'th */
	    while (TRUE)
		{
		ival = data[data[i].order].keyval;
					/* Found it */
		if (jval > ival) break;
			 		/* Stabilize sort by breaking tie */
		else if (jval == ival)
		    {
		    lastj = data[jptr].lastorder;
		    lasti = data[data[i].order].lastorder;
		    if (lastj > lasti) break;
		    }

					/* Drop i'th record 1 interval */
		data[i+interval].order = data[i].order;
		i -= interval;
					/* Fall through ... loop finished */
		if (i < 0) break;
		}
					/* This is correct slot for j'th */
	    data[i+interval].order = jptr;
	    }
	}

    return;
    }
