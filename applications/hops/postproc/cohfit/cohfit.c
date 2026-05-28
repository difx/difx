/************************************************************************/
/*                                                                      */
/* This program reads in data which has been segmented by fringex, then */
/* averaged incoherently by the average program.  It looks for data     */
/* on any given baseline which was segmented using a variety of segment */
/* lengths, and fits the resulting amplitudes and SNRs as a function of */
/* segment length.  These fits constitute coherence analysis, the       */
/* results of which are plotted and written out in the form of 1 A-file */
/* line per baseline/scan, with coherence time fields filled in.        */
/*                                                                      */
/*      Inputs:         filename                Input A-file data       */
/*      Output:         filename                Output A-file data      */
/*                                                                      */
/* The two fits (of Amplitude and SNR are preformed within fit_codata   */
/* using the data captured in the cosumary structure.  The original     */
/* Amplitude fit was of the Marquardt type, but coded with Numerical    */
/* Recipes (which is not distributable).  The SNR fit was a 3-point     */
/* around the maximum SNR (regardless of noise peaks).  Such was the    */
/* state of things; and it appears to be version of code developed      */
/* following Shep Doeleman's these (June 1995).                         */
/*                                                                      */
/* This version uses the GNU Science Library (GSL) for both, and also   */
/* makes possible exporting the data to fit to external files suitable  */
/* for explorations outside this tool.  This version of tool has been   */
/* renamed cohfit to avoid confusion with the previous version.  The    */
/* original fits may be generated if HOPS3 is configured with the       */
/* --enable-nr flag in a non-public versions of HOPS3 which retains the */
/* cofit tool as originally named.                                      */
/*                                                                      */
/* In this version the default msg level is 2, limiting the chatter.    */
/*                                                                      */
/* Created October 5 1995 by CJL                                        */
/* Hacked up May-April 2026 by GBC                                      */
/************************************************************************/
#include <math.h>
#include <stdio.h>  
#include <stdlib.h>
#include <string.h>  
#include <errno.h>
#include "mk4_util.h"
#include "cpgplot.h"
#include "msg.h"

/* define several GSL fitting tolerance parameters here */
#define requirefitolerances
#include "cohfit.h"

/* command options and amassed knowledge on a per scan-boundary basis */
static examdata exd = {
    .gnuplot = 1, .montage = 1, .density = 150, .customlimits = 0,
    .rnc = { 2, 2 },
    /* default closs = 0.05 is 5% loss or 95% of amplitude */
    .closs = 0.05, .mxcotime = MAX_COTIME,
    .pdfile = NULL, .epat = NULL, .elen = 0, .edit = NULL,
    .labels = 0, .fitmask = FITOPT_ALLFITS, .gslegacy = 0,
    .gpdashtype = 1, .gpboldface = 1
};
/* holder for afile averaged data as read and then sorted */
static avgspace avg = {
    .space = INIT_AVSP,
    .data = NULL
};
/* holder for per scan-boundary averaged data for coh fitting */
static cosumary codata[MAX_BNO];

int main (int argc, char *argv[])
    {
    int i, navg, nout, scan_boundary, order, oldtime, bno, nsegtime, nfail, ens;
    char oldbl[3], oldpl[3];
    fringesum *datum;
    FILE *fpout;
    set_progname("cohfit");
    set_msglev(2);
    environment();

                                        /* Preliminary setups */
    (void)gsl_set_error_handler(&err_handler);
    set_gnuplot_opts(&exd);
    if ((exd.gslegacy = get_gslegacy_default()) < 0) exit(1);
    for (i = 0; i < MAX_BNO; i++) codata[i].examlen = 0;

                                        /* Allocate avg space to start */
    avg.data = (avg_data *) calloc (avg.space, sizeof (avg_data));
    if (avg.data == NULL)
        {
        perror ("calloc");
        msg ("Could not allocate memory for main data array.", 3);
        exit (1);
        }
                                        /* Interpret command line */
    ens = parse_cmdline(argc, argv, &fpout, &exd);
    if (ens > 0) exit(ens);             /* explanation in exam_default.c */
    if (ens < 0) exit(0);

                                        /* Read in the data */
                                        /* After parse_cmdline, optind points */
                                        /* at the arguments following command */
                                        /* line flags.  Read all files in. */
    navg = 0;
    if (optind < argc)
        {
        for ( ; optind<argc; optind++) 
            if (read_data (&avg, argv[optind], &navg, &nfail) != 0) exit (1);
        }
    else if (read_data (&avg, "stdin", &navg, &nfail) != 0) exit (1);
                                        /* Sort data into proper order */
    msg ("Working with %d averaged data.", 1, navg);
    if (nfail) msg ("Working with %d averaged data, %d failed to average",
        2, navg, nfail);
    if (sort_data (avg.data, navg) != 0)
        exit (1);
                                        /* Loop over all data */
    nout = 0;
    scan_boundary = TRUE;
    for (i=0; i<navg; i++)
        {
        order = avg.data[i].order;
        datum = &(avg.data[order].fdata);
                                        /* Initialize each scan */
        if (scan_boundary)
            {
            oldtime = datum->time_tag;
            strcpy (oldbl, "  ");
            strcpy (oldpl, "  ");
            bno = -1;
            scan_boundary = FALSE;
            }
                                        /* New baseline */
                                        /* (always true for new scan) */
                                        /* new pol is effectively a new bl */
        if (strcmp (oldbl, datum->baseline) != 0 ||
            strcmp (oldpl, datum->polarization) != 0) 
            {
            if (++bno > MAX_BNO)
                {
                msg ("Too many baselines, abort!", 3);
                exit (1);
                }
            nsegtime = 0;
            codata[bno].datum = datum;
            strcpy (oldbl, datum->baseline);
            strcpy (oldpl, datum->polarization);
                                        /* Remember to force timetag to be */
                                        /* actual scan time for subsequent */
                                        /* processing */
            codata[bno].datum->time_tag -= codata[bno].datum->scan_offset;
            codata[bno].datum->offset += codata[bno].datum->scan_offset;
            codata[bno].datum->scan_offset = 0;
            codata[bno].labels = exd.labels;
            codata[bno].fitmask = exd.fitmask;
            codata[bno].bestamp = FITOPT_NOFITS;
            codata[bno].bestsnr = FITOPT_NOFITS;
            codata[bno].cohereloss = exd.closs;
            codata[bno].maxcotime = exd.mxcotime;
            codata[bno].exampat = NULL;
            codata[bno].examlen = 0;
            if (exd.elen > 0)
                {
                codata[bno].exampat = malloc(exd.elen + 32);
                if (codata[bno].exampat)
                    {
                    /* file ordinals are sequential with previous output */
                    snprintf(codata[bno].exampat,
                        exd.elen + 8, exd.epat, nout+bno);
                    codata[bno].examlen = strlen(codata[bno].exampat);
                    exd.edata[bno].examfile =
                        malloc(codata[bno].examlen + 3);
                    strcpy(exd.edata[bno].examfile, codata[bno].exampat);
                    }
                }
            else
                {
                codata[bno].exampat = NULL;
                codata[bno].examlen = 0;
                exd.edata[bno].examfile = NULL;
                }
            }
                                        /* Record amplitude, SNR and */
                                        /* segment time for fitting/plotting */
        if (nsegtime > MAX_NSEGLEN)
            msg ("Too many seg. time values, ignoring some data", 3);
        else
            {
            codata[bno].ampl[nsegtime] = datum->amp;
            codata[bno].snr[nsegtime] = datum->snr;
            codata[bno].seglen[nsegtime] = datum->duration;
            codata[bno].navg[nsegtime] = datum->srch_cotime;
            codata[bno].nsegtime = ++nsegtime;
            }
                                        /* Is this a scan boundary (or EOF)? */
        if ((i+1) == navg) scan_boundary = TRUE;
        else
            {
            order = avg.data[i+1].order;
            if (avg.data[order].fdata.time_tag != oldtime)
                scan_boundary = TRUE;
            }
                                        /* If yes, process previous scan */
                                        /* and clear for next scan */
        if (scan_boundary)
            {
            /* the number of data is counted by non-NULL datum in codata */
            fit_codata (codata, &exd);
            if (exd.devp) plot_codata (codata);
            exd.pbno = nout;
            nout += write_codata (codata, fpout);
            exd.nbno = nout - exd.pbno;
            exam_gnuplot(&exd);
            for (bno = 0; bno < exd.nbno; bno++)
                if (exd.edata[bno].examfile)
                    free(exd.edata[bno].examfile);
            exd.nbno = exd.pbno = 0;
            memset(&exd.edata, 0, sizeof(exd.edata));
            clear_codata (codata);
            }
        }
    msg ("", 2);
    msg ("Wrote %d coherence-analyzed output records", 2, nout);
    if (exd.epat) free(exd.epat);
    if (exd.edit) free(exd.edit);

    if (exd.devp) { cpgend(); free(exd.devp); }
    if (exd.pdfile)
        {
        int len = strlen(exd.pdfile) + 20;
        char *cmd = malloc(len), *slash;
        snprintf(cmd, len-1, "%s %s", PS2PDF, exd.pdfile);
        msg("Converting with: %s", 1, cmd);
        if (system(cmd))
            msg("conversion failed", 3);
        else
            {
            msg("Unlinking %s", 1, exd.pdfile);
            unlink(exd.pdfile);
            slash = strstr(exd.pdfile, ".ps");
            if (slash) { slash[2] = 'd'; slash[3] = 'f'; slash[4] = 0; }
            msg("Plot is %s", 2, exd.pdfile);
            }
        free(exd.pdfile);
        free(cmd);
        }
    exit (0);
    }

/*
 * eof vim:nospell
 */
