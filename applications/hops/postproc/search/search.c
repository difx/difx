/************************************************************************/
/*									*/
/* This program reads in data which has been segmented by fringex, then	*/
/* averaged incoherently by the average program.  It looks for data	*/
/* on any given baseline which was segmented using a variety of 	*/
/* residual rates and delays, and places the incoherent SNRs in a grid	*/
/* of rate and delay.  It then interpolates a peak in this grid, plots	*/
/* a contour map of the grid with the peak fit indicated, and writes	*/
/* the fitted rate and delay offsets into an output data record.  This	*/
/* process is repeated for all baseline/scans fed to the program.	*/
/*									*/
/* The structure of this program is modelled after cofit.		*/
/*									*/
/*	Inputs:		filename		Input A-file data	*/
/*									*/
/*	Output:		filename		Output A-file data	*/
/*									*/
/* Created January 26 1996 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>  
#include <string.h>  
#include <math.h>
#include <stdlib.h>
#include <errno.h>
#include "cpgplot.h"
#include "search.h"
#include "msg.h"

// char progname[8] = "search";
// int msglev = 1;
int datatype = 0;
int space = 500;

int main (int argc, char* argv[])
    {
    int i, navg, nout, scan_boundary, order, oldtime, bno, npt, plot;
    int rate_index, delay_index, oldextent, square;
    avg_data *data;
    char oldbl[3], oldroot[7];
    static struct srchsummary srchdata[MAX_BNO];
    fringesum *datum;
    FILE *fpout;
    extern int optind;
    set_progname(BIGGER ? "search": "soirch");
    set_msglev(1);

    //declare helper functions
    extern int read_data (avg_data** data, char* filename, int* navg);
    extern int sort_data (avg_data* data, int navg);
    extern void clear_srchdata (struct srchsummary srchdata[]);
    extern int fit_peaks (struct srchsummary *srchdata);
    extern int write_srchdata (struct srchsummary *srchdata, FILE* fpout);

					/* Allocate some space to start */
    data = (avg_data *) calloc (space, sizeof (avg_data));
    if (data == NULL)
	{
	perror ("calloc");
	msg ("Could not allocate memory for main data array.", 3);
	exit (1);
	}
					/* Interpret command line */
    if (parse_cmdline (argc, argv, &fpout, &plot, &square) != 0) 
	exit (1);
    if (BIGGER == 0) exit(0);   /* test code */
					/* Read in the data */
					/* After parse_cmdline, optind points */
					/* at the arguments following command */
					/* line flags.  Read all files in. */
    navg = 0;
    if (optind < argc)
	{
	for ( ; optind<argc; optind++) 
	    if (read_data (&data, argv[optind], &navg) != 0) exit (1);
	}
    else if (read_data (&data, "stdin", &navg) != 0) exit (1);
					/* Sort data into proper order */
    if (sort_data (data, navg) != 0)
	exit (1); 
					/* Loop over all data */
    nout = 0;
    scan_boundary = TRUE;
    for (i=0; i<navg; i++)
	{
	order = data[i].order;
	datum = &(data[order].fdata);
					/* Initialize each scan */
	if (scan_boundary)
	    {
	    oldtime = datum->time_tag;
	    oldextent = datum->extent_no;
	    strcpy (oldroot, datum->root_id);
	    strcpy (oldbl, "  ");
	    bno = -1;
	    scan_boundary = FALSE;
	    }
					/* New baseline */
					/* (always true for new scan) */
	if (strcmp (oldbl, datum->baseline) != 0) 
	    {
	    bno++;
	    if (bno > MAX_BNO)
		{
		msg ("Too many baselines, abort!", 3);
		exit (1);
		}
	    srchdata[bno].datum = datum;
	    strcpy (oldbl, datum->baseline);
					/* Remember to force timetag to be */
					/* actual scan time for subsequent */
					/* processing */
	    srchdata[bno].datum->time_tag -= srchdata[bno].datum->scan_offset;
	    srchdata[bno].datum->offset += srchdata[bno].datum->scan_offset;
	    srchdata[bno].datum->scan_offset = 0;
	    }
					/* Store away data pointer for use at */
					/* end of scan (below) */
	npt = srchdata[bno].nd;
	srchdata[bno].darray[npt] = datum;
	srchdata[bno].nd += 1;
					/* Is this a scan boundary (or EOF)? */
	if ((i+1) == navg) scan_boundary = TRUE;
	else
	    {
	    order = data[i+1].order;
	    if (data[order].fdata.time_tag != oldtime) scan_boundary = TRUE;
	    if (data[order].fdata.extent_no != oldextent) scan_boundary = TRUE;
	    if (strcmp (data[order].fdata.root_id, oldroot) != 0)
		scan_boundary = TRUE;
	    }
					/* If yes, process previous scan */
					/* and clear for next scan */
	if (scan_boundary)
	    {
	    if (fill_grids (srchdata) != 0) 
		{
		clear_srchdata (srchdata);
		continue;
		}
	    if (fit_peaks (srchdata) != 0) continue;
	    if (plot) plot_srchdata (srchdata, square);
	    nout += write_srchdata (srchdata, fpout);
	    clear_srchdata (srchdata);
	    }
	}
    msg ("Wrote %d fringe_fitted output records", 1, nout);

    if (plot) cpgend();
    exit (0);

    return 0;
    }
