/************************************************************************/
/*									*/
/* Trivial routine to clear srchsummary structure array			*/
/*									*/
/*	Inputs:		srchdata		undefined state		*/
/*									*/
/*	Output:		srchdata		initialized		*/
/*									*/
/* Created January 29 1996 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "search.h"

void
clear_srchdata (struct srchsummary srchdata[])
    {
    int i, j, k;

    for (i=0; i<MAX_BNO; i++)
	{
	if (!srchdata[i].datum && srchdata[i].nd == 0) break;
	srchdata[i].datum = NULL;
	srchdata[i].nd = 0;
	srchdata[i].nrate = 0;
	srchdata[i].ndelay = 0;
	srchdata[i].min_rate = 0.0;
	srchdata[i].max_rate = 0.0;
	srchdata[i].min_delay = 0.0;
	srchdata[i].max_delay = 0.0;
	
	for (j=0; j<MAX_NRATE; j++)
	    for (k=0; k<MAX_NDELAY; k++)
		srchdata[i].snr[j][k] = 0.0;
	}
    return;
    }
