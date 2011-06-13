/************************************************************************/
/*									*/
/* This routine takes care of clearing out the memory of aedit.  It	*/
/* takes a string "data", "inputs", "plot" or "all", and respectively	*/
/* clears the array of data structures, the input structure, the plot	*/
/* surface (on hardcopy devices this amounts to a page eject), or all	*/
/* three.								*/
/*									*/
/*	Input:		string		what to clear			*/
/*									*/
/* Created 31 March 1989 by CJL						*/
/* Cleaned up and added closure array clearing, 9 August 1994 by CJL	*/
/*									*/
/************************************************************************/
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "cpgplot.h"
#include "aedit.h"
#include "aedata.h"
#include "pstruct.h"

clear (data, string)
char *string;
esum *data;
    {
    extern int fscan, fflag, fspace, plot_open, up_to_date;
    extern int cscan, cflag, cspace;
    extern int rscan, rflag, rspace;
    extern int tscan, tflag, tspace;
    extern int qscan, qflag, qspace;
    extern struct plot_info pdata[];
    int n, i, ier, dodata, doinputs, doplot, dopage, closureonly;
    char c;
					/* Initialize */
    ier = 0;
    dodata = doinputs = doplot = dopage = closureonly = FALSE;
					/* Convert to lower case */
    n = strlen (string);
    for (i=0; i<n; i++) 
	{
	c = string[i];
	if (isupper (c)) c = tolower (c);
	string[i] = c;
	}
					/* Minimum match arguments and set */
					/* flags to clear various things */
    if (strncmp (string, "data", n) == 0) 
	{
	dodata = TRUE;
	if ((pdata[0].plotby == TRIANGLE_PLOT) || (pdata[0].plotby == QUAD_PLOT))
	    dopage = FALSE;
	else
	    dopage = TRUE;
	}
    else if (strncmp (string, "close", n) == 0)
	{
	dodata = closureonly = TRUE;
	if ((pdata[0].plotby == TRIANGLE_PLOT) || (pdata[0].plotby == QUAD_PLOT))
	    dopage = TRUE;
	}
    else if (strncmp (string, "inputs", n) == 0) doinputs = TRUE;
    else if (strncmp (string, "plot", n) == 0) doplot = TRUE;
    else if(strncmp(string,"all",n) == 0) 
	dodata = doinputs = doplot = TRUE;
    else 
	{
	msg ("Unrecognized CLEAR option %s", 2, string);
	msg ("\tValid options are DATA, INPUTS, PLOT, CLOSE, ALL", 2);
	ier = -1;
	}
					/* Do actual clearing here */
    if (dodata)
	{
	if ((fscan + cscan + rscan + tscan + qscan) == 0)
		{
		msg ("You must read some data in before it makes sense", 2);
		msg ("to invoke the 'clear' command!", 2);
		return (-1);
		}
	if(! confirm ("Physically erase data.  Are you sure?")) return(-1);
					/* Do closure data first */
	tscan = qscan = tflag = qflag = 0;
	free (data->tdata);
	free (data->qdata);
	tspace = 200;
	qspace = 200;
	data->tdata = (trianglearray *) calloc(tspace,sizeof(trianglearray));
	data->qdata = (quadarray *) calloc(qspace,sizeof(quadarray));
					/* Then main data arrays */
	if (! closureonly)
	    {
	    fscan = cscan = rscan = fflag = cflag = rflag = 0;
	    free (data->fdata);
	    free (data->cdata);
	    free (data->rdata);
	    rspace = 100;
	    cspace = 500;
	    fspace = 500;
	    data->rdata = (rootarray *) calloc(rspace,sizeof(rootarray));
	    data->cdata = (corelarray *) calloc(cspace,sizeof(corelarray));
	    data->fdata = (fringearray *) calloc(fspace,sizeof(fringearray));
	    }
	up_to_date = FALSE;
	}
					/* Merely clear page, but don't close */
					/* device (for when plotted data are */
					/* cleared) */
    if (dopage && plot_open) 
	{
	cpgpage();
	for (i=0; i<30; i++) clear_pstruct (pdata + i);
	}
					/* Actually shut down plot device */
    if (doplot && plot_open)
	{
	cpgend ();
	for (i=0; i<30; i++) clear_pstruct (pdata + i);
	msg ("Plot flushed", 1);
	plot_open = FALSE;
	}

    if (doinputs) init_inputs();

    return (ier);
    }
