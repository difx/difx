/************************************************************************/
/*									*/
/* This routine loops through all the data in memory and figures out	*/
/* some of its characteristics.  Some it transmits back to the main	*/
/* program, some it uses to decide if we can work at all with this data */
/*									*/
/*	Inputs:		data		Main data array			*/
/*			nseg		Number of data records present	*/
/*									*/
/*	Output:		dsumm		Summary of all data in memory	*/
/*			return value	number of dsumm elements filled	*/
/*									*/
/* Created 9 September 1994 by CJL					*/
/* Too complex, moved functionality to subroutines, CJL 10 April 1995	*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "average.h"

int
assess_data (seg_data *data,
             int nseg,
             struct config configuration,
             summary dsumm[])
    {
    int i, j, nsource;
					/* Record indices of sources/tribases */
					/* for later use */
    nsource = index_data (data, nseg, dsumm);
					/* Look for fatal errors in the data */
					/* and fill in valid data flags in dsumm */
    if (nsource >= MAXSRC)
	{
	    msg("Maximum number of sources (%d) exceeded.", 3,  MAXSRC);
	    msg("Try again with less input.", 3);
	    return(-1);
	}
    msg("Found %d sources.", 0, nsource);
    for (i=0; i<nsource; i++) 
	{
	check_source (data, dsumm + i);
	for (j=0; j<dsumm[i].nid; j++) 
	    check_tribase (data, dsumm[i].tribase + j);
	}
					/* Reconcile segment lengths, integration */
					/* time, and averaging interval boundaries */
    for (i=0; i<nsource; i++)
	check_times (data, dsumm + i, configuration.int_time);

    return (nsource);
    }
