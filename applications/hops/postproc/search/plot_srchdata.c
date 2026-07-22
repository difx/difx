/************************************************************************/
/*									*/
/* This routine takes a filled in array of srchdata structures, and 	*/
/* makes a contour plot of the snr in the rate/delay grid for each	*/
/* baseline.  It uses the PGPLOT subroutine library, and		*/
/* sends the output to the device specified in the original cpgbeg call	*/
/* The plot device is assumed to be open already.			*/
/*									*/
/*	Inputs:		srchdata	struct array filled with data	*/
/*									*/
/*	Output:		graphics					*/
/*									*/
/* Created February 5 1996 by CJL					*/
/*									*/
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include "cpgplot.h"
#include "search.h"

void
plot_srchdata (struct srchsummary srchdata[], int square)
    {
    int i, j, gridindex, base, nbase, size;
    int year, day, hour, min, sec;
    int npos, nneg;
    char label[50];
    float ratesize, delaysize, peakrate[2], peakdelay[2], tr[6];
    float red_clev[2] = {1.0, 2.0};
    float violet_clev[2] = {3.0, 4.0};
    float green_clev[15] = {5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 12.0, 15.0, 
			    20.0, 30.0, 50.0, 100.0, 200.0, 500.0, 1000.0};
    float testcont;
#if BIGGER
    static float grid[MAX_NRATE*MAX_NDELAY];
#else /* BIGGER */
    float grid[MAX_NRATE*MAX_NDELAY];
#endif /* BIGGER */
    struct srchsummary *sb;
    extern char* fringename(fringesum *fsumm);
					/* How many baselines are there? */
    nbase = 0;
    while (srchdata[nbase].datum != NULL) nbase++;
					/* Set character height */
    cpgsch (2.0);
					/* Loop on all baselines in srchdata */
    for (base=0; base<nbase; base++)
	{
					/* What is the grid size? */
	sb = srchdata + base;
	if ((sb->nrate == 1) || (sb->ndelay == 1))
	    {
	    msg ("Cannot plot 1-D grid for %s", 2, fringename (sb->datum));
	    continue;
	    }
					/* Next page/panel, reset colour */
	cpgpage();
	cpgsci (1);
					/* Make cells square to indicate true */
					/* search space represented */
	size = sb->nrate;
	if (sb->ndelay > size) size = sb->ndelay;
	ratesize = 0.7 * ((double)sb->nrate / (double)size);
	delaysize = 0.7 * ((double)sb->ndelay / (double)size);
#if BIGGER
	if (square) delaysize = ratesize = 0.7;	    /* or not */
	msg ("Delaysize is %g, Ratesize is %g", 2, delaysize, ratesize);
#else /* BIGGER */
#endif /* BIGGER */
	cpgsvp (0.1, 0.1+ratesize, 0.15, 0.15+delaysize);
	cpgswin (sb->min_rate, sb->max_rate, sb->min_delay, sb->max_delay);
					/* Draw labelled box */
	cpgbox ("BCNST", 0.0, 0.0, "BCNST", 0.0, 0.0);
	int_to_time (sb->datum->time_tag, &year, &day, &hour, &min, &sec);
	sprintf (label, "File: %03d-%02d%02d%02d/%s.%c.%d.%s", day, hour, min, sec,
		sb->datum->baseline, sb->datum->freq_code,
		sb->datum->extent_no, sb->datum->root_id);
	cpgmtxt ("T", 1.5, 0.0, 0.0, label);
	sprintf (label, "Source: %s  SNR: %5.2f", 
			sb->datum->source, sb->datum->snr);
	cpgmtxt ("T", 0.5, 0.0, 0.0, label);
	cpgmtxt ("B", 2, 0.5, 0.5, "Resid. rate (psec/sec)");
	cpgmtxt ("L", 1.7, 0.5, 0.5, "Resid. delay (usec)");
					/* Set up transformation matrix */
					/* Remember that in definition of tr[] */
					/* pgplot uses fortran 1-rel addressing */
	tr[1] = (sb->max_rate - sb->min_rate) / (double)(sb->nrate - 1);
	tr[0] = sb->min_rate - tr[1];
	tr[2] = 0.0;
	tr[4] = 0.0;
	tr[5] = (sb->max_delay - sb->min_delay) / (double)(sb->ndelay - 1);
	tr[3] = sb->min_delay - tr[5];
					/* Do the contour plot */
	npos=nneg=0;
	for (i=0; i<sb->ndelay; i++)
	    {
	    /* printf("delay %d: ", i); */
	    for (j=0; j<sb->nrate; j++)
		{
		gridindex = i * sb->nrate + j;
		grid[gridindex] = sb->snr[j][i];
		/* printf("%5.2f ", sb->snr[j][i]); */
		if (sb->snr[j][i] > 0.0) npos++;
		else nneg++;
		}
	    /* printf("\n"); */
	    }
	msg ("npos=%d, nneg=%d\n",2, npos,nneg); 

	cpgsci (2);	/* red */
	for (i=0; i<2; i++)
	    cpgcont (grid, sb->nrate, sb->ndelay, 1, sb->nrate, 1, sb->ndelay,
			red_clev+i, 1, tr);
	cpgsci (6);	/* violet */
	for (i=0; i<2; i++)
	    cpgcont (grid, sb->nrate, sb->ndelay, 1, sb->nrate, 1, sb->ndelay,
			violet_clev+i, 1, tr);
	cpgsci (3);	/* green */
	for (i=0; i<15; i++)
	    cpgcont (grid, sb->nrate, sb->ndelay, 1, sb->nrate, 1, sb->ndelay,
			green_clev+i, 1, tr);
					/* Indicate the fitted peak position */
	if (sb->datum->snr < 3.0) cpgsci (2);
	else if (sb->datum->snr < 4.0) cpgsci (6);
	else cpgsci (3);
	peakrate[0] = sb->datum->delay_rate;
	peakdelay[0] = sb->datum->mbdelay;
	cpgsch (4.0);
	cpgpt (1, peakrate, peakdelay, 5);
	cpgsch (2.0);
	}

    return;
    }
