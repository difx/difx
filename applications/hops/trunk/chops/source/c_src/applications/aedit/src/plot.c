/************************************************************************/
/*									*/
/* This routine causes all the data in memory to be plotted on a	*/
/* device of the user's choice, subject to the input settings.  	*/
/* The data are first filtered according to the input settings, and	*/
/* summarized to figure out what needs plotting.  Then it loops through */
/* all frequency/experiment combinations present, and passes control	*/
/* to the lower-level routine plot_fqex().				*/
/*									*/
/*	Inputs:		data						*/
/*			y_axis, x_axis		User-supplied strings	*/
/*									*/
/*	Output:		return value		0=success		*/
/*									*/
/* Created 17 April 1989 by CJL						*/
/* Architecture overhauled for increased capability and robustness,	*/
/* much functionality transferred to plot_fqex() and lower routines,	*/
/* by CJL, 24 February 1992						*/
/* Renamed and given both x and y axis capability, CJL Feb 21 1994	*/
/* Expanded to accommodate full closure data, CJL 29 August 1994	*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "cpgplot.h"
#include "aedata.h"
#include "aedit.h"
#include "pstruct.h"
#include "summary.h"

#define QUICK 0
#define SLOW 1

int interactive = FALSE;

int plot (esum *data, char *y_axis, char *x_axis)
    {
    extern struct inputs inp;
    extern struct datasumm fsumm, tsumm, qsumm;
    extern struct plot_info pdata[];
    extern int fscan, tscan, qscan;
    extern int plot_open, psplot_open, up_to_date, batch, data_version;
    extern int fsortstat[], tsortstat[], qsortstat[];
    struct datasumm *summ;
    fringearray *fdata;
    trianglearray *tdata;
    quadarray *qdata;
    char dummy[200];
    int i, ret, idummy, scan, nflag, length;
					/* First figure out what axes we want */
					/* to plot */
    if (axis (y_axis, x_axis) != 0)
	{
	msg ("Invalid axes '%s %s'", 2, y_axis, x_axis);
	return (-1);
	}
					/* Save flag and sort information */
    save (data, SAVE);
					/* Convenience pointers */
    fdata = data->fdata;
    tdata = data->tdata;
    qdata = data->qdata;
					/* Are we using fdata, tdata or qdata? */
    active_filter();
    if ((inp.plotby == STATION_PLOT) || (inp.plotby == BASELINE_PLOT)
		|| (inp.plotby == ALL_PLOT)) 
	{
	if (fscan == 0)
	    {
	    msg ("No baseline data present.  You'll have to read some in.", 2);
	    save (data, RESTORE);
	    return (-1);
	    }
	summ = &fsumm;
	scan = fscan;
	nflag = 0;
	for (i=0; i<fscan; i++)
	    {
	    if (! fdata[i].flag) fdata[i].flag = ffilter (fdata+i, QUICK);
	    if (fdata[i].flag) nflag++;
	    }
	if (nflag == fscan)
	    {
	    msg ("All baseline data rejected by current input filters", 2);
	    save (data, RESTORE);
	    return (-1);
	    }
	}
    else if (inp.plotby == TRIANGLE_PLOT) 
	{
	if (tscan == 0)
	    {
	    msg ("No triangle data present.  Either read some in,", 2);
	    msg ("  or calculate some with the 'close' command.", 2);
	    save (data, RESTORE);
	    return (-1);
	    }
	summ = &tsumm;
	scan = tscan;
	nflag = 0;
	for (i=0; i<tscan; i++)
	    {
	    if (! tdata[i].flag) tdata[i].flag = tfilter (tdata+i, QUICK);
	    if (tdata[i].flag) nflag++;
	    }
	if (nflag == tscan)
	    {
	    msg ("All triangle data rejected by current input filters", 2);
	    save (data, RESTORE);
	    return (-1);
	    }
	}
    else if (inp.plotby == QUAD_PLOT) 
	{
	if (qscan == 0)
	    {
	    msg ("No quad data present.  Either read some in,", 2);
	    msg ("  or calculate some with the 'close' command.", 2);
	    save (data, RESTORE);
	    return (-1);
	    }
	summ = &qsumm;
	scan = qscan;
	nflag = 0;
	for (i=0; i<qscan; i++)
	    {
	    if (! qdata[i].flag) qdata[i].flag = qfilter (qdata+i, QUICK);
	    if (qdata[i].flag) nflag++;
	    }
	if (nflag == qscan)
	    {
	    msg ("All quad data rejected by current input filters", 2);
	    save (data, RESTORE);
	    return (-1);
	    }
	}
					/* Summarize data, yielding much of the */
					/* information needed for plotting */
    up_to_date = FALSE;
    ret = summ_data (data, STANDARD);
    if (ret != 0)
	{
	msg ("Error summarizing data in plot_data()",2);
	save (data, RESTORE);
	up_to_date = FALSE;		/* Label summary information as outdated */
	return (-1);
	}
					/* Now set up the plot device */
    if (batch && (strcmp (inp.device,"?") == 0))	/* Trap this goof */
	{
	msg("You must specify an output device ahead of time in batch mode!", 2);
	save (data, RESTORE);
	up_to_date = FALSE;		/* Label summary information as outdated */
	return(-1);
	}
					/* Check to see if user changed */
    length = sizeof (dummy);
    cpgqinf("DEV/TYPE",dummy,&length);		/* plot device */
    if (strcmp(dummy,inp.device) != 0) 
	{
	cpgend();
	plot_open = FALSE;
	psplot_open = FALSE;
	}
    if (psplot_open) cpgend();
    if (! plot_open) 
	{ 				/* Open specified device */
	if (cpgbeg(0,inp.device,1,1) != 1)
	    {
	    msg ("Plot open failed - check device specification", 2);
	    save (data, RESTORE);
	    up_to_date = FALSE;		/* Label summary information as outdated */
	    return(-1);
	    }
	length = sizeof (dummy);
	cpgqinf ("HARDCOPY", dummy, &length);
	if (strncmp (dummy, "NO", 2) == 0) interactive = TRUE;
	else interactive = FALSE;
	for (i=0; i<30; i++) clear_pstruct(pdata+i);
	}
					/* Make inp.device same as internal */
					/* pgplot device specification */
    length = sizeof (inp.device);
    cpgqinf("DEV/TYPE",inp.device,&length);
    plot_open = TRUE;
    cpgask (FALSE);			/* New page confirmation handled by aedit */

					/* Some plotting modes need a time sort, */
					/* so do it here outside tight loops */
    if (fsortstat[fsortstat[0]] != 1) sorter (fdata, "timetag", 2);
    if (tsortstat[tsortstat[0]] != 1) sorter (tdata, "timetag", 3);
    if (qsortstat[qsortstat[0]] != 1) sorter (qdata, "timetag", 4);
					/* Loop through freq/expt combinations */
					/* plotting each in turn. Must pass main */
					/* data array, not just fdata, because */
					/* there may be closure data */
    ret = 0;
    for (i=0; i<summ->nfqex; i++)
	if ((ret = plot_fqex (data, summ->fqex[i])) != 0) break;
    if (ret == 1) ret = 0;		/* User said no to next page */
                                        /* Undo sorts only */
    if (save (data, RESTORE_NOFLAG) != 0) ret = 1;
    up_to_date = FALSE;			/* Label summary information as outdated */

					/* If automatic hardcopy, do it */
    auto_hardcopy();

    return (ret);
    }
