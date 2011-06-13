/************************************************************************/
/*									*/
/* This routine loops through the root data in memory, and accumulates	*/
/* certain statistics.  This information can be printed (pr_summary.c)	*/
/*									*/
/*	Inputs:		rdata						*/
/*			mode			defined in summary.h	*/
/*									*/
/*	Output:		filled "summ" structure				*/
/*			return value		0 = OK, -1 = failure	*/
/*									*/
/* Created 9 April 1989 by CJL						*/
/* Added mode support for data versions 1 February 1994 by CJL		*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "summary.h"

int
summ_root (rdata, mode)
rootarray *rdata;
int mode;
    {
    extern struct datasumm rsumm;
    extern int rscan, rflag;
    int dtime, ptime;
    int i, j, l, match, first, found;
    char c;
    rootsum *datum;

    first = TRUE;
			 		/* Loop over all root records */
    for (i=0; i<rscan; i++)
	{
					/* Flagged, ignore */
	if (rdata[i].flag != 0) continue;
					/* Point at data section */
	datum = &(rdata[i].data);
					/* Version number count */
	rsumm.version[datum->version] += 1;
	if (mode == VERSION) continue;
	    				/* Check times */
	dtime = datum->time_tag;
	ptime = datum->procdate;
	if(first)                       /* Initialize */
	    {
	    rsumm.begin = rsumm.end = dtime;
	    rsumm.proc_begin = rsumm.proc_end = ptime;
	    first = FALSE;
	    }
	if (dtime < rsumm.begin) rsumm.begin = dtime;
	if (dtime > rsumm.end) rsumm.end = dtime;
	if (ptime < rsumm.proc_begin) rsumm.proc_begin = ptime;
	if (ptime > rsumm.proc_end) rsumm.proc_end = ptime;
	dtime = datum->time_tag;
	if (first) 			/* Initialize */
	    {
	    rsumm.begin = rsumm.end = dtime;
	    first = FALSE;
	    }
	if (dtime < rsumm.begin) rsumm.begin = dtime;
	if (dtime > rsumm.end) rsumm.end = dtime;
					/* Check stations */
	j = 0;
	while ((c = datum->stations[j++]) != '\0')
	    {
	    if (strlen (rsumm.stations) < MAXSTTOT)
		add_station (c, rsumm.stations);
	    }
					/* Check experiment numbers */
	match = FALSE;
	for (j=0; j<rsumm.nexp; j++) 
	    {
	    if (rsumm.experiments[j] == datum->expt_no)
		{
		match = TRUE;
		break;
		}
	    }
	if (! match && (rsumm.nexp < MAXEXPTS)) 
	    rsumm.experiments[rsumm.nexp++] = datum->expt_no;

					/* This updates source list */
	if (rsumm.nsource < MAXSRC)
	    rsumm.nsource += 
		update_sinfo (rsumm.source, datum->source, rsumm.nsource);

	}		/* End for loop */
    return (0);
    }
