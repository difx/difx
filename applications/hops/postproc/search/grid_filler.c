/*
 * $Id: grid_filler.c 439 2011-08-29 18:11:51Z gbc $
 *
 * Modified version of fill_grids.c which inserts the rate/delay
 * data into the cells of the matrix for pgplot to plot.
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "search.h"

#define VERY_BIG 1.0e99
#define REMLIMIT 0.10

static double	remlimit;

/*
 * Diagnostic--print out values
 */
static void rep_deltas(double *v, int nn, int after, int ismbd)
{
    int jj;
    printf("Array of %d %s values, %s sort\n", nn,
	ismbd ? "delay" : "rate",
	after ? " after" : "before");
    for (jj = 0; jj < nn; jj++) printf (" %d:%g", jj, v[jj]);
    printf("\n");
    fflush(stdout);
}

/*
 * Find the extrema of the data, and figure out what the step size was.
 * If we can find a way past the input stage, we don't need to do this.
 *
 * In any case, the data is sorted at this stage so we look at deltas.
 * The median/mode of the distribution of deltas should be a good hint.
 * If we are pushing the limits of precision, then substantial numbers of
 * shoulders on the mode should allow us to reconstruct the actual step.
 *
 * The same code can be used for rate and delay after setup.
 * The tests for MAX_* are probably formal.
 */
static int less_than(const void *p1, const void *p2)
{
    double d1 = *(double*)p1;
    double d2 = *(double*)p2;
    if (d1 < d2) return(-1);
    if (d1 > d2) return( 1);
    return(0);
}
static int get_rd_extrema(struct srchsummary *sb, int ismbd)
{
    static double rd_vals[MAX_NRATE * MAX_NDELAY];
    static double rd_dels[MAX_NRATE * MAX_NDELAY];
    double rd_min, rd_max, rd_del, *rdp;
    int jj, j1, nn;

    if (sb->nd > MAX_NRATE * MAX_NDELAY) {
	msg ("Too much data %d > (%d*%d)", 2, sb->nd, MAX_NRATE, MAX_NDELAY);
	return(1);
    }

    for (jj=0, rdp = rd_vals; jj<sb->nd; jj++, rdp++) {
	*rdp = (ismbd) ? sb->darray[jj]->mbdelay : sb->darray[jj]->delay_rate;
	if (*rdp < rd_min || jj == 0) rd_min = *rdp;
	if (*rdp > rd_max || jj == 0) rd_max = *rdp;
    }
    for (jj=0, j1=1, nn = 0; j1<sb->nd; jj++, j1++) {
	rd_dels[nn] = rd_vals[j1] - rd_vals[jj];
	if (rd_dels[nn] > 0) nn++;
    }

    if (nn == 0) {
	msg ("No positive %s deltas", 2, ismbd ? "delay" : "rate" );
	return(4);
    }

    if (msglev < 0) rep_deltas(rd_dels, nn, 0, ismbd);
    if (getenv("HOPS_SEARCH_INCR")) {
	qsort(rd_dels, nn, sizeof(double), less_than);
	rd_del = rd_dels[nn/2];		/* median */
    } else {
	for (jj=0, rd_del = 0.0; jj < nn; jj++) rd_del += rd_dels[jj];
	rd_del /= (double)nn;
    }
    if (msglev < 1) rep_deltas(rd_dels, nn, 1, ismbd);

    if (ismbd) {
	jj = sb->ndelay = 1.0 + rint((rd_max - rd_min)/rd_del);
	if (sb->ndelay > MAX_NDELAY) return(2);
	sb->min_delay = rd_min;
	sb->max_delay = rd_max;
    } else {
	jj = sb->nrate = 1.0 + rint((rd_max - rd_min)/rd_del);
	if (sb->nrate > MAX_NRATE) return(3);
	sb->min_rate = rd_min;
	sb->max_rate = rd_max;
    }
    msg("%s with %d values from %g .. %g, stepsize %g", 2,
	(ismbd) ? "Delay rate" : "Multiband delay",
	jj, rd_min, rd_max, rd_del);
    return(0);
}

/*
 * Fill one grid only.
 */
static int fill_grid_ss(struct srchsummary *sb)
{
    double fp_rcell, fp_dcell, rinc, dinc, rem;
    double rem_rmin = VERY_BIG, rem_rmax = 0;
    double rem_dmin = VERY_BIG, rem_dmax = 0;
    int jj, rcell, dcell;

    if ((jj = get_rd_extrema(sb, 0))) {	/* dLyrate */
	msg ("Too many (delay) rate cells [%d], %d", 3, sb->nrate, jj);
	return(1);
    }
    rinc = (sb->max_rate - sb->min_rate) / ((double)sb->nrate - 1.0);
    msg("Rate from %g .. %g in %d steps delta %g", 2,
	sb->min_rate, sb->max_rate, sb->nrate, rinc);

    if ((jj = get_rd_extrema(sb, 1))) {	/* mbdelay */
	msg ("Too many (multiband) delay cells [%d], %d", 3, sb->ndelay, jj);
	return(1);
    }
    dinc = (sb->max_delay - sb->min_delay) / ((double)sb->ndelay - 1.0);
    msg("Delay from %g .. %g in %d steps delta %g", 2,
	sb->min_delay, sb->max_delay, sb->ndelay, dinc);

    for (jj=0; jj<sb->nd; jj++) {
	fp_rcell = (sb->darray[jj]->delay_rate - sb->min_rate) / rinc;
	rcell = (int)(fp_rcell+0.5);
	rem = fabs(fp_rcell - (double)rcell);
	if (rem < rem_rmin) rem_rmin = rem;
	if (rem > rem_rmax) rem_rmax = rem;
	if (rem > remlimit) break;

	fp_dcell = (sb->darray[jj]->mbdelay - sb->min_delay) / dinc;
	dcell = (int)(fp_dcell+0.5);
	rem = fabs(fp_dcell - (double)dcell);
	if (rem < rem_dmin) rem_dmin = rem;
	if (rem > rem_dmax) rem_dmax = rem;
	if (rem > remlimit) break;

	sb->snr[rcell][dcell] = sb->darray[jj]->snr;
    }
    msg("Delay rate remainders from %g .. %g", 2, rem_rmin, rem_rmax);
    msg("MBand delay remainders from %g .. %g", 2, rem_dmin, rem_dmax);

    if (jj < sb->nd) {			/* did we bail early? */
	msg ("Uneven delay distribution", 3);
	msg ("If desparate, set HOPS_SEARCH_REMLIMIT > %g", 3, remlimit);
	return(1);
    }
    return(0);
}

/*
 * Main entry; returns nonzero if resource limits were exceeded.
 */
static int grid_filler(struct srchsummary *srchdata)
{
    int	ii, xx;
    char *rlp = getenv("HOPS_SEARCH_REMLIMIT");
    remlimit = rlp ? atof(rlp) : REMLIMIT;
    for (ii = 0; ii < MAX_BNO && srchdata[ii].datum; ii++)
	if ((xx = fill_grid_ss(srchdata + ii))) return(xx);
    return(0);
}

/*
 * Switch into the two options
 */
int fill_grids (struct srchsummary *srchdata)
{
    if (getenv("HOPS_SEARCH_OLDWAY")) return(fill_grids_orig(srchdata));
    return(grid_filler(srchdata));
}

/*
 * eof
 */
