/************************************************************************/
/*									*/
/* This routine loops through the quad data in memory, and accumulates	*/
/* certain statistics.  This information can be printed (pr_summary.c)	*/
/*									*/
/*	Inputs:		qdata						*/
/*			mode			defined in summary.h	*/
/*									*/
/*	Output:		filled "qsumm" structure			*/
/*			return value		0 = OK, -1 = failure	*/
/*									*/
/* Created 9 April 1989 by CJL						*/
/* Added mode support for data versions 1 February 1994 by CJL		*/
/* Cloned for quads 24 August 1994 by CJL				*/
/*									*/
/************************************************************************/

#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "summary.h"
#include "aedit.h"

int summ_quad (quadarray *qdata, int mode)
    {
    extern struct datasumm qsumm;
    extern int qscan, qflag;
    int dtime;
    int i, j, l, match, first, found;
    quadsum *datum;

    first = TRUE;

    for (i=0; i<qscan; i++) 		/* Loop over all quad records */
	{
					/* Flagged, ignore */
	if (qdata[i].flag != 0) continue;
					/* Point to data section */
	datum = &(qdata[i].data);
					/* Version number count */
	qsumm.version[datum->version] += 1;
	if (mode == VERSION) continue;
	    				/* Check times */
	dtime = datum->time_tag;
	if(first) 			/* Initialize */
	    {
	    qsumm.begin = qsumm.end = dtime;
	    first = FALSE;
	    }
	if (dtime < qsumm.begin) qsumm.begin = dtime;
	if (dtime > qsumm.end) qsumm.end = dtime;

					/* Check stations */
	for (j=0; j<4; j++)
	    {
	    if (strlen (qsumm.stations) < MAXSTTOT)
		add_station (datum->quad[j], qsumm.stations);
	    }
					/* Check baselines */
	for (j=0; j<qsumm.nbtq; j++)
	    if (strcmp (datum->quad, qsumm.btq + 5*j) == 0) break;
	if (j == qsumm.nbtq)
	    {
	    strcpy (qsumm.btq + 5*qsumm.nbtq, datum->quad);
	    qsumm.nbtq++;
	    }
					/* Check experiment numbers */
	match = FALSE;
	for (j=0; j<qsumm.nexp; j++) 
	    {
	    if (qsumm.experiments[j] == datum->expt_no)
		{
		match = TRUE;
		break;
		}
	    }
	if (! match && (qsumm.nexp < MAXEXPTS)) 
	    qsumm.experiments[qsumm.nexp++] = datum->expt_no;

					/* Accumulate quality codes */
	switch (datum->quality)
	    {
	    case 'A':
		qsumm.qcodes[0]++;
		break;
	    case 'B':
		qsumm.qcodes[1]++;
		break;
	    case 'C':
		qsumm.qcodes[2]++;
		break;
	    case 'D':
		qsumm.qcodes[3]++;
		break;
	    case 'E':
		qsumm.qcodes[4]++;
		break;
	    case 'F':
		qsumm.qcodes[5]++;
		break;
	    case '0':
		qsumm.qcodes[6]++;
		break;
	    case '1':
		qsumm.qcodes[7]++;
		break;
	    case '2':
		qsumm.qcodes[8]++;
		break;
	    case '3':
		qsumm.qcodes[9]++;
		break;
	    case '4':
		qsumm.qcodes[10]++;
		break;
	    case '5':
		qsumm.qcodes[11]++;
		break;
	    case '6':
		qsumm.qcodes[12]++;
		break;
	    case '7':
		qsumm.qcodes[13]++;
		break;
	    case '8':
		qsumm.qcodes[14]++;
		break;
	    case '9':
		qsumm.qcodes[15]++;
		break;
	    default:
		qsumm.qcodes[16]++;
	    }

					/* This updates source list */
	if (qsumm.nsource < MAXSRC)
            qsumm.nsource += 
		update_sinfo (qsumm.source, datum->source, qsumm.nsource);

	}		/* End for loop */
    return(0);
    }
