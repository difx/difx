/************************************************************************/
/*									*/
/* This routine loops through the data in memory, and accumulates	*/
/* certain statistics.  This information can be printed  (pr_summary.c)	*/
/* or can be used to guide the plotting operations.			*/
/*									*/
/*	Inputs:		data						*/
/*			mode		Values defined in summary.h	*/
/*									*/
/*	Output:		filled "summ" structure				*/
/*			return value		0 = OK, -1 = failure	*/
/*									*/
/* Created 9 April 1989 by CJL						*/
/* Radically reorganized for 3.0, March 1992, CJL			*/
/* Added multiple A-file version support, 1 February 1994, CJL		*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include "aedata.h"
#include "summary.h"
#include "aedit.h"

int summ_data (esum *data, int mode)
    {
    extern struct datasumm rsumm, csumm, fsumm, tsumm, qsumm;
    extern int rscan, cscan, fscan, tscan, qscan, up_to_date;
    extern int rflag, cflag, fflag, tflag, qflag;
    extern int data_version;
    static int first = TRUE;
    int i, j, ret, nver;
					/* Allocate btq memory, set */
					/* pointers to NULL etc */
    if (first)
	{
	init_summ (&rsumm, NONE);
	init_summ (&csumm, BASELINE);
	init_summ (&fsumm, BASELINE);
	init_summ (&tsumm, TRIANGLE);
	init_summ (&qsumm, QUAD);
	first = FALSE;
	}
					/* Is user sentient? */
    if((rscan + cscan + fscan + tscan + qscan 
			- rflag - cflag - fflag - tflag - qflag) == 0) 
	{
	msg("No data!",2);
	return(-1);
	}
					/* Unnecessary call, but always */
					/* do it if closures are requested */
    if (up_to_date && (mode != CLOSURE)) return(0);

    for (i=1; i<=MAXVERSION; i++)
	{
	rsumm.version[i] = 0;
	csumm.version[i] = 0;
	fsumm.version[i] = 0;
	tsumm.version[i] = 0;
	qsumm.version[i] = 0;
	}
    					/* Don't do all this if all we */
					/* want is version control */
    if (mode != VERSION)
	{
					/* Initialize all relevant quantities */
	clear_summ (&rsumm);
	clear_summ (&csumm);
	clear_summ (&fsumm);
	clear_summ (&tsumm);
	clear_summ (&qsumm);
	}
				/* Actual work done by 5 specialized routines */
    if((ret = summ_root  (data->rdata, mode)) != 0) return (ret);
    if((ret = summ_corel (data->cdata, mode)) != 0) return (ret);
					/* This also can form closures, so needs */
					/* main data structure */
    if((ret = summ_fringe (data, mode)) != 0) return (ret);
    if((ret = summ_triangle (data->tdata, mode)) != 0) return (ret);
    if((ret = summ_quad (data->qdata, mode)) != 0) return (ret);
					/* Set version #, 0 if mixed */
    data_version = 0;
    for (i=1; i<=MAXVERSION; i++)
	if ((rsumm.version[i] + csumm.version[i] + fsumm.version[i]
		+ tsumm.version[i] + qsumm.version[i]) > 0)
	    {
	    if (data_version > 0)
		{
		data_version = 0;
		break;
		}
	    else data_version = i;
	    }

    if ((ret == 0) && (mode != VERSION)) up_to_date = TRUE;

    return (ret);
    }
