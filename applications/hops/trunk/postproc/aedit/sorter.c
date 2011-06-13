/************************************************************************/
/*									*/
/* This routine takes care of all sorting operations.  First, the 	*/
/* requested key for sorting is decoded, and makekey is called to fill	*/
/* the keyval fields in the data with appropriate integer values.  Then	*/
/* a shell sort is performed, but with no physical rearrangement of the */
/* data array.  Instead, the "order" element of the data structure is	*/
/* filled with the number of the scan which would occupy this slot in 	*/
/* the sorted order.							*/
/*									*/
/* Other routines can then access the unsorted data just as 		*/
/* before, but if you want to read them in sorted order (for example to */
/* do a duplicate removal job or write out a sorted list to a file),	*/
/* you must use an extra level of indirection.				*/
/*									*/
/* This algorithm was taken from RSORT, an HP1000 program by RJC, which	*/
/* in turn was based on Knuth's sorting and searching volume, p.85,	*/
/* Algorithm D.  The implementation here has an added piece of code to	*/
/* stabilize the sort, using the "lastorder" element of the esum data	*/
/* structure to break ties in the comparison of data values.		*/
/*									*/
/*	Inputs:		arg1		String containing key to sort	*/
/*			type		0, 1, 2, 3 or 4			*/
/*									*/
/*	Output:		data		order, sortkey fields modified	*/
/*									*/
/* Created 13 April 1990 by CJL						*/
/* Modified to support root and corel data, 28 February 1994 by CJL	*/
/* Modified to support closure data, 29 August 1994 by CJL		*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "aedata.h"
#include "sort.h"

#define TRUE 1
#define FALSE 0

int
sorter (dptr, arg1, type)
char *dptr;
char *arg1;
int type;
    {
    extern int rscan, cscan, fscan, tscan, qscan;
    extern int fsortstat[], csortstat[], rsortstat[], tsortstat[], qsortstat[];
    int i, j, k, n, sortkey, ival, jval, jptr, interval, npass, npts;
    int lasti, lastj, family_sort;
    int *sortstat;
    char c, temparg[50];
    rootarray *rdata;
    corelarray *cdata;
    fringearray *fdata;
    trianglearray *tdata;
    quadarray *qdata;

    family_sort = FALSE;
					/* There is a lot of type-dependent */
					/* setup that has to be done before we */
					/* get anywhere near the sort itself */

					/* Set up pointers appropriately */
    if (type == 0) 
	{
	if (rscan == 0) return (0);
	rdata = (rootarray *)dptr;
	sortstat = rsortstat;
	}
    else if (type == 1) 
	{
	if (cscan == 0) return (0);
	cdata = (corelarray *)dptr;
	sortstat = csortstat;
	}
    else if (type == 2) 
	{
	if (fscan == 0) return (0);
	fdata = (fringearray *)dptr;
	sortstat = fsortstat;
	}
    else if (type == 3) 
	{
	if (tscan == 0) return (0);
	tdata = (trianglearray *)dptr;
	sortstat = tsortstat;
	}
    else if (type == 4) 
	{
	if (qscan == 0) return (0);
	qdata = (quadarray *)dptr;
	sortstat = qsortstat;
	}
    else
	{
	msg ("Invalid data type '%d' passed to sorter()", 2, type);
	return (-1);
	}

    n = strlen(arg1);			/* Convert to lower case */
    for (i=0; i<n; i++) 
	{
	c = arg1[i];
	if (isupper(c)) c = tolower(c);
	temparg[i] = c;
	}
    temparg[n] = '\0';
					/* Check for match */
    if      (strncmp (temparg,"timetag",n) == 0)    sortkey = S_TIMETAG;
    else if (strncmp (temparg,"procdate",n) == 0)   sortkey = S_PROCDATE;
    else if (strncmp (temparg,"snr",n) == 0)        sortkey = S_SNR;
    else if (strncmp (temparg,"length",n) == 0)     sortkey = S_LENGTH;
    else if (strncmp (temparg,"baseline",n) == 0)   sortkey = S_BASELINE;
    else if (strncmp (temparg,"triangle",n) == 0)   sortkey = S_TRIANGLE;
    else if (strncmp (temparg,"quad",n) == 0)       sortkey = S_QUAD;
    else if (strncmp (temparg,"frequency",n) == 0)  sortkey = S_FREQUENCY;
    else if (strncmp (temparg,"sourcename",n) == 0) sortkey = S_SOURCENAME;
    else if (strncmp (temparg,"qcode",n) == 0)      sortkey = S_QCODE;
    else if (strncmp (temparg,"experiment",n) == 0) sortkey = S_EXPERIMENT;
    else if (strncmp (temparg,"rootcode",n) == 0)   sortkey = S_ROOTCODE;
    else if (strncmp (temparg,"family",n) == 0) 
	{
	sortkey = S_FAMILY;
	family_sort = TRUE;
	}
    else 
	{
	msg("Unrecognized sort key %s ",2,temparg);
	return(-1);
	}
					/* Already in requested sort order */
    if (sortstat[0] != 0 && sortstat[sortstat[0]] == sortkey) return (0);
					/* Some keys not allowed for some */
					/* types of data. Treat as a no-op */
    switch (type)
	{
	case 0:
	    if ((sortkey == S_SNR) || (sortkey == S_LENGTH) 
		|| (sortkey == S_BASELINE) || (sortkey == S_TRIANGLE)
		|| (sortkey == S_QUAD) || (sortkey == S_FREQUENCY)
		|| (sortkey == S_QCODE))
		return (0);
	    break;
	case 1:
	    if ((sortkey == S_SNR) || (sortkey == S_LENGTH)
		|| (sortkey == S_TRIANGLE) || (sortkey == S_QUAD)
		|| (sortkey == S_FREQUENCY))
		return (0);
	    break;
	case 2:
	    if ((sortkey == S_TRIANGLE) || (sortkey == S_QUAD))
		return (0);
	    break;
	case 3:
	    if ((sortkey == S_PROCDATE) || (sortkey == S_LENGTH)
		|| (sortkey == S_BASELINE) || (sortkey == S_QUAD)
		|| (sortkey == S_ROOTCODE) || (sortkey == S_FAMILY))
		return (0);
	    break;
	case 4:
	    if ((sortkey == S_PROCDATE) || (sortkey == S_LENGTH)
		|| (sortkey == S_BASELINE) || (sortkey == S_TRIANGLE)
		|| (sortkey == S_ROOTCODE) || (sortkey == S_FAMILY)
		|| (sortkey == S_SNR))
		return (0);
	    break;
	default:
	    ;
	}
						/* Fill keyval elements */
    if (family_sort) sortkey = S_ROOTCODE;
    if (makekey (dptr, sortkey, type) != 0) return (-1);
    if (family_sort) sortkey = S_FAMILY;
			 			/* Initialize starting order */
    switch (type)
	{
	case 0:
	    npts = rscan;
	    if (sortstat[0] == 0)
		for (i=0; i<npts; i++) rdata[i].order = i;
	    for (i=0; i<npts; i++) rdata[rdata[i].order].lastorder = i;
	    break;
	case 1:
	    npts = cscan;
	    if (sortstat[0] == 0)
		for (i=0; i<npts; i++) cdata[i].order = i;
	    for (i=0; i<npts; i++) cdata[cdata[i].order].lastorder = i;
	    break;
	case 2:
	    npts = fscan;
	    if (sortstat[0] == 0)
		for (i=0; i<npts; i++) fdata[i].order = i;
	    for (i=0; i<npts; i++) fdata[fdata[i].order].lastorder = i;
	    break;
	case 3:
	    npts = tscan;
	    if (sortstat[0] == 0)
		for (i=0; i<npts; i++) tdata[i].order = i;
	    for (i=0; i<npts; i++) tdata[tdata[i].order].lastorder = i;
	    break;
	case 4:
	    npts = qscan;
	    if (sortstat[0] == 0)
		for (i=0; i<npts; i++) qdata[i].order = i;
	    for (i=0; i<npts; i++) qdata[qdata[i].order].lastorder = i;
	    break;
	default:
	    ;
	}
						/* This is the sort itself. */
						/* Sure would be a lot easier to */
						/* read with only one data type! */

						/* Find coarsest sort interval */
						/* and passes required */
    interval = 1; npass = 0;
    while ((interval = 3*interval + 1) <= npts) npass++;
    interval = interval/3;
    if (interval < 4) interval = 4;
    if (npass < 1) npass = 1;

    for (k=0; k<npass; k++) 			/* Actually do the sort */
	{
	interval /= 3;				/* Progressively shorter intervals */
	for (j=interval; j<npts; j++) 
	    {
						/* All done with pointers in */
						/* order element of data array */
	    if (type == 0)
		{
		jptr = rdata[j].order;
		jval = rdata[jptr].keyval;
		}
	    else if (type == 1)
		{
		jptr = cdata[j].order;
		jval = cdata[jptr].keyval;
		}
	    else if (type == 2)
		{
		jptr = fdata[j].order;
		jval = fdata[jptr].keyval;
		}
	    else if (type == 3)
		{
		jptr = tdata[j].order;
		jval = tdata[jptr].keyval;
		}
	    else if (type == 4)
		{
		jptr = qdata[j].order;
		jval = qdata[jptr].keyval;
		}
	    i = j - interval;
	    while (TRUE) 			/* Loop till we find slot for j'th */
		{
		if      (type == 0) ival = rdata[rdata[i].order].keyval;
		else if (type == 1) ival = cdata[cdata[i].order].keyval;
		else if (type == 2) ival = fdata[fdata[i].order].keyval;
		else if (type == 3) ival = tdata[tdata[i].order].keyval;
		else if (type == 4) ival = qdata[qdata[i].order].keyval;

		if (jval > ival) break;		/* Found it */

		else if (jval == ival) 		/* Stabilize sort by breaking tie */
		    {
		    if (type == 0)
			{
			lastj = rdata[jptr].lastorder;
			lasti = rdata[rdata[i].order].lastorder;
			}
		    else if (type == 1)
			{
			lastj = cdata[jptr].lastorder;
			lasti = cdata[cdata[i].order].lastorder;
			}
		    else if (type == 2)
			{
			lastj = fdata[jptr].lastorder;
			lasti = fdata[fdata[i].order].lastorder;
			}
		    else if (type == 3)
			{
			lastj = tdata[jptr].lastorder;
			lasti = tdata[tdata[i].order].lastorder;
			}
		    else if (type == 4)
			{
			lastj = qdata[jptr].lastorder;
			lasti = qdata[qdata[i].order].lastorder;
			}
		    if (lastj > lasti) break;
		    }

						/* Drop i'th record 1 interval */
		if      (type == 0) rdata[i+interval].order = rdata[i].order;
		else if (type == 1) cdata[i+interval].order = cdata[i].order;
		else if (type == 2) fdata[i+interval].order = fdata[i].order;
		else if (type == 3) tdata[i+interval].order = tdata[i].order;
		else if (type == 4) qdata[i+interval].order = qdata[i].order;
		i -= interval;
		if (i < 0) break;		/* Fall through ... loop finished */
		}
						/* This is correct slot for j'th */
	    if      (type == 0) rdata[i+interval].order = jptr;
	    else if (type == 1) cdata[i+interval].order = jptr;
	    else if (type == 2) fdata[i+interval].order = jptr;
	    else if (type == 3) tdata[i+interval].order = jptr;
	    else if (type == 4) qdata[i+interval].order = jptr;
	    }
	}
						/* Update sort status array */
    sortstat[0]++;
    sortstat[sortstat[0]] = sortkey;

    return(0);
    }
