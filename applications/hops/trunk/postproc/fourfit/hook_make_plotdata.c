/*
 * $Id$
 *
 * Hook for pulling numerical data out of make_plotdata
 */

#include <stdio.h>
#include <stdlib.h>
#include "type_comp.h"

static FILE *fx = 0;
static int xpspec = 0;

static void report_cp_spectrum(void);
static int load_plot_arrays(double frq[], double mag[], double phs[]);
static void connect_pointers(double **frqp, double **magp, double **phsp);
static char *spectral_legend(void);

/*
 * one time setup
 */
static int hackage_setup(void)
{
    char *ep;
    ep = getenv("HOPS_FEARFIT_XPSPEC");
    xpspec = ep ? atoi(ep) : 0;
    if (!xpspec) return(0);
    fx = fopen("fearfit.spec", "w");
    if (!fx) { perror("report_cp_spectrum:fopen"); exit(1); }
}

static void report_cp_spectrum(void)
{
    static int once = 1;
    double *frq, *mag, *phs;
    int nn, ii;
    if (once) once = hackage_setup();
    if (!xpspec) return;
    connect_pointers(&frq, &mag, &phs);
    nn = load_plot_arrays(frq, mag, phs);
    fprintf(fx, "## %s\n", spectral_legend());
    for (ii = 0; ii < nn; ii++)
	fprintf(fx, "%+e %+e %+e\n", frq[ii], mag[ii], phs[ii]);
    fprintf(fx, "\n\n");
}

#define PLOTDATA_HOOK\
    report_cp_spectrum()
#include "make_plotdata.c"

/*
 * param is defined in type_param.h but it's already included by
 * norm.c and it's not #ifdef-wrapped for re-inclusion
 * ditto status
 *
 * Code here is cribbed from make_postplot.c, but we'll just output
 * both sidebands.
 */
static int load_plot_arrays(double frq[], double mag[], double phs[])
{
    extern struct type_plot plot;
    extern struct type_param param;
    extern struct type_status status;
    double bandwidth, xstart, xend;
    double c_phase(), c_mag();
    int ii, ncp;

    bandwidth = 0.25 / status.sbd_sep;
    xstart = -bandwidth;
    xend = bandwidth;
    ncp = 2 * param.nlags;
    for (ii=0; ii<ncp; ii++) {
	frq[ii] = xstart + (xend - xstart) * ii / ncp;
	mag[ii] = c_mag (plot.cp_spectrum[ii]);
	phs[ii] = c_phase (plot.cp_spectrum[ii]) * 180.0 / M_PI;
    }
    return(ncp);
}

static void connect_pointers(double **frqp, double **magp, double **phsp)
{
    static double frq[2*MAXMAX];
    static double mag[2*MAXMAX];
    static double phs[2*MAXMAX];
    *frqp = frq;
    *magp = mag;
    *phsp = phs;
}

static char *spectral_legend(void)
{
    static char mine[80];
    static int index = 0;
    extern struct type_param param;
    snprintf(mine, sizeof(mine), " index %d baseline %c%c",
	index++, param.baseline[0], param.baseline[1]);
    return(mine);
}

/*
 * eof
 */
