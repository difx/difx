/************************************************************************/
/*									*/
/* Writes out A-file lines for all baselines of a scan, as contained	*/
/* in the srchdata structure.  These A-file lines have the rate, delay	*/
/* and snr fields filled in, ready for plotting and subsequent		*/
/* visibility generation.						*/
/*									*/
/*	Inputs:		srchdata	representing 1 scan		*/
/*			fpout		open output file		*/
/*									*/
/*	Output:		A-file lines	written to fpout		*/
/*			return value	Number of lines written		*/
/*									*/
/* Created February 5 1996 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "search.h"

int
write_srchdata (struct srchsummary *srchdata, FILE* fpout)
    {
    int i;

    i = 0;
    while (srchdata[i].datum != NULL)
	{
					/* This tags data as coming from search */
	srchdata[i].datum->datatype[0] = 'L';
	write_fsumm (srchdata[i].datum, fpout);
	i++;
	}

    return (i);
    }
