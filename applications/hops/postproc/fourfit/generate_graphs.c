/************************************************************************/
/*                                                                      */
/* Generates the graphical part of a fringe plot                        */
/*                                                                      */
/* Created make_postplot original            October 1999 by CJL        */
/* Refactored into a separate routine            2014.7.30   rjc        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "msg.h"
#include "hops_complex.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "meta_struct.h"
#include "ovex.h"
#include "cpgplot.h"

extern struct mk4_fringe fringe;
extern struct type_plot plot;
extern struct type_param param;
extern struct type_status status;
extern struct type_meta meta;

// since PGPLOT labels are rendered, there is no good way to label
// parts of the PGPLOT generated plots and other graphic items.

/*
 * Adjust the segmented plot list to respect display_chans; skips
 * must be set up so that in this loop:
 *  for (i=start_plot; i<start_plot+nplots; i++)
 * i=start_plot is drawn first and i=limit_plot is ALL
 *
 * This is much harder, since channels are in the arrays in different
 * places according to whether chid_ids was used or not.  When display
 * chans is created, we also do not know which channels will be in pass.
 */
static void set_seg_plot_limits(struct type_pass *pass)
{
    int ii, skip, insk, lastii;
    char *atsign = strrchr(pass->control.display_chans, '@');
    skip = atsign ? atoi(atsign+1) : 0;
    /* initial setup, consider -f/-n, but not display_chans (yet) */
    meta.start_plot = (param.first_plot == FALSE) ? 0 : param.first_plot;
    meta.limit_plot = (param.nplot_chans == FALSE)
                    ? pass->nfreq : param.nplot_chans;
    meta.nplots = (meta.limit_plot == 1) ? 1 : meta.limit_plot+1;
    msg("set_seg_plot_limit: '%s' %d %s; ii %d..%d (%d)",1,
        pass->control.display_chans, skip, atsign,
        meta.start_plot, meta.limit_plot, meta.nplots);
    memset(meta.skip_plots, '-', sizeof(meta.skip_plots)-1);
    if (meta.nplots == 1 || pass->control.display_chans[0] == 0) {
        /* we are done if only one */
        meta.skip_plots[meta.start_plot+meta.nplots] = 0;
        meta.vplots = meta.nplots;
        return;
    }
    msg("set_seg_plot_flags: %s", 1, pass->control.chid);
    meta.skip_plots[strlen(pass->control.chid)] = 0;
    /* populate from param.display_chans[]; note that i in pass->nfreq
     * counts frequencies in the pass, ie. it refers to the frequency
     * with code pass->pass_data[i].fcode_index. */
    for (ii = meta.start_plot; ii < meta.start_plot+meta.nplots; ii++) {
        /* mark codes in display with 'o' */
        if (strchr(pass->control.display_chans,
                   pass->pass_data[ii].freq_code))
            meta.skip_plots[ii] = 'o';
        msg("set_seg_plot_flags: %s at %d", 1, meta.skip_plots, ii);
    }
    /* find the new start */
    for (ii = meta.start_plot; ii < meta.limit_plot; ii++) {
        if (meta.skip_plots[ii] == 'o') break;
        meta.start_plot++;
    }
    msg("set_seg_plot_flags: %s %d..%d (%d) start", 1, meta.skip_plots,
        meta.start_plot, meta.limit_plot, meta.nplots);
    /* find the new end */
    for (ii = meta.limit_plot-1; ii > meta.start_plot; ii--) {
        if (meta.skip_plots[ii] == 'o') break;
        meta.limit_plot--;
    }
    /* fix nplots so we end up with limit plot last */
    meta.nplots = meta.limit_plot+1 - meta.start_plot;
    msg("set_seg_plot_flags: %s %d..%d (%d) end", 1, meta.skip_plots,
        meta.start_plot, meta.limit_plot, meta.nplots);
    /* ok, start/limit are assigned, handle skips */
    insk = 0;
    for (insk = 0, ii = meta.start_plot; ii <= meta.limit_plot; ii++) {
        msg("set_seg_plot_flags: %s %d..%d (%d) %d", 1, meta.skip_plots,
            meta.start_plot, meta.limit_plot, meta.nplots, ii);
        /* first and limit plot are never skipped */
        if (ii == meta.start_plot || ii == meta.limit_plot-1) {
            continue;
        }
        /* skip plots not in display_chans */
        if (meta.skip_plots[ii] == '-') {
            meta.skip_plots[ii] == 'x';
            continue;
        }
        /* now into skip control */
        if (insk < skip) {
            meta.skip_plots[ii] = 'x';
            insk++;
            continue;
        }
        /* Nth case: do not skip */
        meta.skip_plots[ii] = 'n';
        insk = 0;
    }
    /* be sure of null termination */
    meta.skip_plots[meta.start_plot] = 'S';
    meta.skip_plots[meta.limit_plot-1] = 'L';
    meta.skip_plots[meta.limit_plot] = 'A';
    meta.skip_plots[meta.limit_plot+1] = 0;
    /* now count the number of non-x plots */
    for (meta.vplots = 0, ii = meta.start_plot; ii <= meta.limit_plot; ii++)
        if (meta.skip_plots[ii] != 'x') meta.vplots++;
    msg("set_seg_plot_flags: %s %d..%d (%d) %d:%d", 1, meta.skip_plots,
        meta.start_plot, meta.limit_plot, meta.nplots, ii, meta.vplots);
}

int generate_graphs (struct scan_struct *root,
                     struct type_pass *pass,
                     char *fringename,
                     char *ps_file,
                     double *tickinc)
    {
    int i, j, maxj, ii, nsbd, ncp, np;
    char buf[2560], device[256], pbfr[2][44], *skip_plots;
    double drate, mbd, sbd;
    static float xr[2*MAXMAX], yr[2*MAXMAX], zr[2*MAXMAX];
    float xmin, xmax, ymin, ymax, plotwidth;
    float xpos, offset, lwid, yplace;
    double max_dr_win, max_sb_win, max_mb_win;
    float max_amp;
    double plotstart, plotdur, plotend, totdur, majinc, ticksize;
    float plot_time, bandwidth, xstart, xend, xszm, xezm;
    int start_plot, limit_plot, nplots, vplots, vth;
    int totpts, wrap;
    int nlsb, nusb, izero;
    double vwgt;
    char *cptr;
    int vclr, cloner = 0;
    int notchpass, nnp, nn;
                                        /* Build the proper device string for */
                                        /* vertically oriented color postscript */
    sprintf (device, "%s/VCPS", ps_file);
                                        /* Open the pgplot device */
    if (cpgopen (device) <= 0)
        {
        msg ("Postscript plot open failed (%s)", 2, ps_file);
        return (-1);
        }
                                        /* Redefine pgplot green to be a bit darker */
    cpgscr (3, 0.0, 0.6, 0.0);

                                        /* Create color for passband/notches warning */
    cpgscr (8, 0.8, 0.4, 0.0);          // Cf. setdarkorange of generate_text.c
    notchpass = (param.nnotches > 0 ||
        param.passband[0] != 0.0 || param.passband[1] != 1.0E6) ? 1 : 0;

    if (param.avxplopt[1] == 7 || param.avxplopt[1] == -7)
        {   /* overkill, but tastes vary  */
        extern char *getenv(const char *name);
        double rr, gg, bb;
        char *rgbclr = getenv("HOPS_FF_PHASE_CLR"), rgbdfl[]="0.93,0.89,0.99";
        if (!rgbclr) rgbclr = rgbdfl;
        (void)sscanf(rgbclr, "%lg,%lg,%lg", &rr, &gg, &bb);
        cpgscr (7, rr, gg, bb);
        }
                                        /* Make pgplot compute a bounding */
                                        /* box encompassing the whole page */
    cpgsvp (0.0, 1.0, 0.0, 1.0);
    cpgswin (0.0, 1.0, 0.0, 1.0);       /* dummy world coordinates to start */
    cpgsci (0);                         /* invisible to prime the pump */
    cpgmove (0.0, 0.0);
    cpgdraw (1.0, 0.0);
    cpgdraw (0.0, 1.0);
    cpgsci (1);
    max_amp = 1e4 * fringe.t208->amplitude; /* Whitneys */

    // original order: DR first, then MBD overlaid (if MBD exists)
    if (param.mbdrplopt[0] == 0 || pass->nfreq == 1)
        {
                                            /* Delay-rate spectrum */
        xmin = xmax = 0;
        for(i=0; i<plot.dr_size_max; i++)
            {                               /* Convert to ns/s */
            drate = (double)i - (double)plot.dr_size_max / 2.0;
            drate /= plot.dr_size_max * param.acc_period * fringe.t205->ref_freq;
            drate *= 1000.0;
            if (drate < xmin) xmin = drate;
            if (drate > xmax) xmax = drate;
            xr[i] = drate;
            yr[i] = plot.d_rate[i];
            }
        if (param.mbdrplopt[1] > 1)
            {
            float xcen = (xmax + xmin)/2.0;
            float xhwd = (xmax - xmin)/(2.0*(float)param.mbdrplopt[1]);
            xmin = xcen - xhwd;
            xmax = xcen + xhwd;
            }
        cpgsvp (0.05, 0.80, 0.77, 0.95);
        cpgswin(xmin, xmax, 0.0, max_amp);
        cpgsch (0.5);
        cpgbox ("BNST", 0.0, 0.0, "BNST", 0.0, 0.0);
        cpgsch (0.7);
        cpgsci (2);
        cpgmtxt("L", 1.5, 0.5, 0.5, "amplitude");
        cpgmtxt("B", 1.8, 0.5, 0.5, "delay rate (ns/s)");
        cpgline(plot.dr_size_max, xr, yr);
                                            /* Draw in search window */
        max_dr_win = 0.5 / (param.acc_period * param.ref_freq);
        if ((param.win_dr[0] > -max_dr_win) || (param.win_dr[1] < max_dr_win))
            {   /* draw delay rate red bar */
            cpgsvp (0.05, 0.80, 0.767, 0.768);
            cpgswin (xmin, xmax, 0.0, 1.0);
            xpos = param.win_dr[0] * 1.0e3;
            cpgmove (xpos, 0.5);
            xpos = param.win_dr[1] * 1.0e3;
            cpgslw (3);
            cpgdraw (xpos, 0.5);
            cpgslw (1);
            }
        cpgsci (1);
        }

                                        /* Multiband delay resolution function */
                                        // in some cases the MBDfunc reduces
                                        // to a flatline at the max amplitude
    if (pass->nfreq > 1)
        {
        xmin = xmax = 0;
        for (i=0; i<plot.num_mb_pts; i++)
            {
            mbd = i - plot.num_mb_pts/2;
            mbd /= plot.num_mb_pts;
            mbd /= status.freq_space;
            if (mbd < xmin) xmin = mbd;
            if (mbd > xmax) xmax = mbd;
            xr[i] = mbd;
            yr[i] = plot.mb_amp[i];
            }
        cpgsvp (0.05, 0.80, 0.77, 0.95);
        cpgswin (xmin, xmax, 0.0, max_amp);
        cpgsch (0.5);
        cpgbox ("CMST", 0.0, 0.0, "CMST", 0.0, 0.0);
        cpgsch (0.7);
        cpgsci (4);
        cpgmtxt("R", 2.0, 0.5, 0.5, "amplitude");
        cpgmtxt("T", 1.5, 0.5, 0.5, "multiband delay (\\gms)");
        cpgline(plot.num_mb_pts, xr, yr);
                                        /* Draw in search window */
        max_mb_win = 0.5 / status.freq_space;
        if ((param.win_mb[0] > -max_mb_win) || (param.win_mb[1] < max_mb_win))
            {   /* draw multi-band delay blue bar */
                                        /* This is complicated by wrap possibility */
            wrap = FALSE;
            if (param.win_mb[0] > param.win_mb[1]) wrap = TRUE;
            cpgsvp (0.05, 0.80, 0.952, 0.953);
            cpgswin (xmin, xmax, 0.0, 1.0);
            xpos = param.win_mb[0];
            cpgmove (xpos, 0.5);
            cpgslw (3);
            if (wrap)
                {
                cpgmove (xmin, 0.5);
                cpgdraw (param.win_mb[1], 0.5);
                cpgmove (xmax, 0.5);
                cpgdraw (param.win_mb[0], 0.5);
                }
            else
                {
                xmin = param.win_mb[0];
                xmax = param.win_mb[1];
                cpgmove (xmin, 0.5);
                cpgdraw (xmax, 0.5);
                }
            cpgslw (1);
            }
        cpgsci (1);
        }
                                        /* No MBD, must complete the box */
    else cpgbox ("CST", 0.0, 0.0, "CST", 0.0, 0.0);

    if (param.mbdrplopt[0] == 1 && pass->nfreq > 1)
        {
                                            /* Delay-rate spectrum */
        xmin = xmax = 0;
        for(i=0; i<plot.dr_size_max; i++)
            {                               /* Convert to ns/s */
            drate = (double)i - (double)plot.dr_size_max / 2.0;
            drate /= plot.dr_size_max * param.acc_period * fringe.t205->ref_freq;
            drate *= 1000.0;
            if (drate < xmin) xmin = drate;
            if (drate > xmax) xmax = drate;
            xr[i] = drate;
            yr[i] = plot.d_rate[i];
            }
        if (param.mbdrplopt[1] > 1)
            {
            float xcen = (xmax + xmin)/2.0;
            float xhwd = (xmax - xmin)/(2.0*(float)param.mbdrplopt[1]);
            xmin = xcen - xhwd;
            xmax = xcen + xhwd;
            }
        cpgsvp (0.05, 0.80, 0.77, 0.95);
        cpgswin(xmin, xmax, 0.0, max_amp);
        cpgsch (0.5);
        cpgbox ("BNST", 0.0, 0.0, "BNST", 0.0, 0.0);
        cpgsch (0.7);
        cpgsci (2);
        cpgmtxt("L", 1.5, 0.5, 0.5, "amplitude");
        cpgmtxt("B", 1.8, 0.5, 0.5, "delay rate (ns/s)");
        cpgline(plot.dr_size_max, xr, yr);
                                            /* Draw in search window */
        max_dr_win = 0.5 / (param.acc_period * param.ref_freq);
        if ((param.win_dr[0] > -max_dr_win) || (param.win_dr[1] < max_dr_win))
            {   /* draw delay rate red bar */
            cpgsvp (0.05, 0.80, 0.767, 0.768);
            cpgswin (xmin, xmax, 0.0, 1.0);
            xpos = param.win_dr[0] * 1.0e3;
            cpgmove (xpos, 0.5);
            xpos = param.win_dr[1] * 1.0e3;
            cpgslw (3);
            cpgdraw (xpos, 0.5);
            cpgslw (1);
            }
        cpgsci (1);
        }


                                        /* Singleband delay function */
    nsbd = 2 * param.nlags;
    xmin = xmax = 0;
    for (i=0; i<nsbd; i++)
        {
        sbd = (double)i - (double)nsbd / 2.0;
        sbd *= status.sbd_sep;
        if (sbd < xmin) xmin = sbd;
        if (sbd > xmax) xmax = sbd;
        xr[i] = sbd;
        yr[i] = plot.sb_amp[i];
        }
    if (param.mbdrplopt[2] > 1)
        {
        float xcen = (xmax + xmin)/2.0;
        float xhwd = (xmax - xmin)/(2.0*(float)param.mbdrplopt[2]);
        xmin = xcen - xhwd;
        xmax = xcen + xhwd;
        }
    cpgsvp (0.05, 0.35, 0.63, 0.74);
    cpgswin (xmin, xmax, 0.0, max_amp);
    cpgsch (0.5);
    if (status.nion > 0)                // upper tics suppressed if ion display
        cpgbox ("BNST", 0.0, 0.0, "BCNST", 0.0, 0.0);
    else
        cpgbox ("BCNST", 0.0, 0.0, "BCNST", 0.0, 0.0);
    cpgsch (0.7);
    cpgsci (3);
    cpgmtxt("B", 2.0, 0.5, 0.5, "singleband delay (\\gms)");
    cpgmtxt("L", 1.5, 0.5, 0.5, "amplitude");
    cpgline (nsbd, xr, yr);
    cpgsci (1);

                                        /* Draw search window bar */
    max_sb_win = 0.5e+06 * param.samp_period * 1e+3;
    if ((param.win_sb[0] > -max_sb_win) || (param.win_sb[1] < max_sb_win))
        {   /* draw green sbd window */
        cpgsci (3);
        cpgsvp (0.05, 0.35, 0.627, 0.628);
        cpgswin (xmin, xmax, 0.0, 1.0);
        xpos = param.win_sb[0];
        cpgmove (xpos, 0.5);
        xpos = param.win_sb[1];
        cpgslw (3);
        cpgdraw (xpos, 0.5);
        cpgslw (1);
        }
    cpgsci (1);

                                        // ion. search points (iff present)
    if (status.nion > 0)
        {
        for(i=0; i<status.nion; i++)
            {
            xr[i] = status.dtec[i][0];
            yr[i] = status.dtec[i][1];
                                        // debug print
            msg("TEC %f amp %f", 0, status.dtec[i][0], status.dtec[i][1]);
            }
        xmin = status.dtec[0][0];
        xmax = status.dtec[status.nion-1][0];
        cpgsvp (0.05, 0.35, 0.63, 0.74);
        cpgswin (xmin, xmax, 0.0, max_amp);
        cpgsch (0.5);
        cpgbox ("CMST", 0.0, 0.0, "", 0.0, 0.0);
        cpgsch (0.7);
        cpgsci (2);
        cpgmtxt("T", 0.5, 0.5, 0.5, "ion. TEC");
        cpgline (status.nion, xr, yr);
        cpgsci (1);
        }


                                        /* Cross-power spectrum - amplitude */
    nlsb = nusb = 0;                    /* count up total usb & lsb AP's */
    for (i=0; i<pass->nfreq; i++)
        {
        nusb += status.ap_num[0][i];
        nlsb += status.ap_num[1][i];
        }

    bandwidth = 0.25 / status.sbd_sep;

    if (nusb > 0 && nlsb > 0)           /* DSB */
        {
        xstart = -bandwidth;
        xend = bandwidth;
        ncp = 2 * param.nlags;
        izero = 0;
        }
    else if (nlsb > 0)                  /* LSB only */
        {
        xstart = -bandwidth;
        xend = 0.0;
        ncp = param.nlags;
        izero = 0;
        }
    else                                /* USB only */
        {
        xstart = 0.0;
        xend = bandwidth;
        ncp = param.nlags;
        izero = param.nlags;
        }

    if (param.avxpzoom[1] == 0.0)
        {   /* no zoom requested use xstart and xend values for window */
        xszm = xstart;
        xezm = xend;
        }
    else
        {   /* set xszm/xezm to zoom[0] -/+ 0.5 zoom[1] in bw units */
        xezm =
        xszm = xstart * (1.0 - param.avxpzoom[0]) + xend * param.avxpzoom[0];
        xszm -= (xend - xstart)*param.avxpzoom[1] / 2.0;
        xezm += (xend - xstart)*param.avxpzoom[1] / 2.0;
        if (xszm < xstart) xszm = xstart;
        if (xezm > xend)   xezm = xend;
        }

    ymax = 0;
    for (i=0; i<ncp; i++)
        {
        xr[i] = xstart + (xend - xstart) * i / ncp;
        yr[i] = abs_complex (plot.cp_spectrum[i+izero]);
        zr[i] = arg_complex (plot.cp_spectrum[i+izero]) * 57.3;
        if (yr[i] > ymax) ymax = yr[i];
        msg ("cp_spectrum[%d] %6.1f %7.1f", -3, i, yr[i], zr[i]);
        }
    /* eliminate the points when param.avxplopt[1] is nonzero -- 300px/in */
    ymin = (param.avxplopt[1]) ? ymax * 3.3333e-3 * param.avxplopt[0] : 0.0;
    if (ymin > 0.0) for (i=0; i<ncp; i++)
        if (yr[i]<ymin) { yr[i] = -1.0 ; zr[i] = NAN; }

    ymax *= 1.2;
    if (ymax == 0.0)
        {
        msg ("overriding ymax of 0 in Xpower Spectrum plot; data suspect", 2);
        ymax = 1.0;
        }
    cpgsvp (0.43, 0.80, 0.63, 0.74);

    if (param.avxplopt[1] < 0)  /* revised location */
        {       /* Cross-power phase first */
        cpgswin(xszm, xezm, -180.0, 180.0);
        cpgsci (1);
        cpgsch (0.5);
        cpgbox ("", 0.0, 0.0, "CMST", 90.0, 3.0);
        if (param.avxplopt[1] == 7 || param.avxplopt[1] == -7)
            {
            cpgsci (7);
            cpgline (ncp, xr, zr);
            }
        cpgsci (2);
        cpgslw (5.0);
        cpgpt (ncp, xr, zr, -1);
        cpgslw (1.0);
        cpgsch (0.7);
        cpgmtxt ("R", 2.0, 0.5, 0.5, "phase (deg)");
        }

    cpgsci (1);
    cpgswin(xszm, xezm, 0.0, ymax);
    cpgsch (0.5);
    cpgbox ("BCNST", 0.0, 0.0, "BNST", 0.0, 0.0);
    cpgsch (0.7);
    if (notchpass)
        {
        cpgsci (8);
        cpgmtxt("B", 2.0, 0.5, 0.5, "*** Avgd. Xpower Spectrum (MHz) ***");
        cpgsci (1);
        }
    else
        {
        cpgmtxt("B", 2.0, 0.5, 0.5, "Avgd. Xpower Spectrum (MHz)");
        }
    cpgmove (0.0, 0.0);
    cpgdraw (0.0, max_amp);

                                        /* Blue dots */
    cpgsci (4);
    cpgslw (5.0);
    cpgpt (ncp, xr, yr, -1);
    cpgslw (1.0);

                                        /* Connect dots in cyan */
    cpgsci (5);
    cpgline (ncp, xr, yr);
    cpgsci (4);
    cpgmtxt ("L", 1.5, 0.5, 0.5, "amplitude");

    // drop in tick marks to give the user a hint about passband/notches
    if (notchpass)
        {
        cpgsci (8);
        cpgslw (1.0);
        nnp = (param.nnotches > 0) ? 2*param.nnotches : 2;
        for (nn = 0; nn < nnp; nn++)
            {
            xr[0] = xr[1] = status.xpnotchpband[nn];
            yr[0] = ymax * 0.25;
            yr[1] = ymax;
            cpgline (2, xr, yr);
            }
        if (param.nnotches == 0)    // passband: label cuts
            {
            cpgsch (0.4);
            for (ii=0; ii<2; ii++)
                {
                snprintf(pbfr[ii], 40, "%c%lf%c",
                    ii?' ':'<', param.passband[ii], ii?'>':' ');
                //xr[ii] = (status.xpnotchpband[ii] - xstart) / (xend - xstart);
                xr[ii] = (status.xpnotchpband[ii] - xszm) / (xezm - xszm);
                cpgmtxt ("T", -1.0-ii, xr[ii], ii?1.0:0.0, pbfr[ii]);
                }
            }
        }

    if (param.avxplopt[1] >= 0)         /* original location */
        {       /* Cross-power phase second */
        cpgswin(xszm, xezm, -180.0, 180.0);
        cpgsci (1);
        cpgsch (0.5);
        cpgbox ("", 0.0, 0.0, "CMST", 90.0, 3.0);
        if (param.avxplopt[1] == 7 || param.avxplopt[1] == -7)
            {
            cpgsci (7);
            cpgline (ncp, xr, zr);
            }
        cpgsci (2);
        cpgslw (5.0);
        cpgpt (ncp, xr, zr, -1);
        cpgslw (1.0);
        cpgsch (0.7);
        cpgmtxt ("R", 2.0, 0.5, 0.5, "phase (deg)");
        }

                                        /* Now set up channel/time plots */
                                        /* Work out width of individual plot */
    cpgsci (1);

    set_seg_plot_limits(pass);          /* sort out segmented plot panels */
    start_plot = meta.start_plot;
    limit_plot = meta.limit_plot;
    skip_plots = meta.skip_plots;
    nplots = meta.nplots;
    vplots = meta.vplots;

    plotwidth = 0.88 / (double)vplots;  /* 88% of full width */
    if (vplots == 1) plotwidth = 0.8;   /* otherwise 80% */
    msg("set_seg_plot_limit: %f*%d = %f",1,
        plotwidth,vplots,plotwidth*vplots);
    /* Adjust line width to make dots legible */
    totpts = vplots * status.nseg;
    if (totpts > 500) lwid = 2.0;
    else if (totpts > 350) lwid = 3.0;
    else if (totpts > 200) lwid = 4.0;
    else lwid = 5.0;
                                        /* Figure out time axis in seconds */
                                        /* Need only relative minute marks, so */
                                        /* can fmod plotstart to simplify things */
                                        /* Segment 0 starts at param.minap */
    plotstart = param.start + pass->ap_off * param.acc_period;
    plotstart = fmod (plotstart, 60.0);
    plotdur = status.nseg * status.apseg * param.acc_period;
    plotend = plotstart + plotdur;
    /* Figure out how many minor ticks to  draw (if any) */
    totdur = plotdur * vplots;
    if (totdur > 6000) {*tickinc = 60.0; majinc = 60.0;}
    else if (totdur > 3000.0) {*tickinc = 30.0; majinc = 60.0;}
    else if (totdur > 1000.0) {*tickinc = 10.0; majinc = 60.0;}
    else if (totdur > 500.0) {*tickinc = 5.0; majinc = 30.0;}
    else if (totdur > 200.0) {*tickinc = 2.0; majinc = 10.0;}
    else {*tickinc = 1.0; majinc = 5.0;}
                                        /* Segment 0 starts at param.minap */
                                        /* Loop over plots */
    for (vth = -1, i=start_plot; i<start_plot+nplots; i++)
        {
        if (*skip_plots++ == 'x') continue;
        vth++;
        /* otherwise we plot it */
        np = status.nseg;
        offset = 0.0;
        if ((i == nplots+limit_plot-1) && (nplots != 1)) offset = 0.01;
        msg("set_seg_window %f..%f %d/%d",1,
            0.05 + (vth)*plotwidth + offset,
            0.05 + (vth+1)*plotwidth + offset, vth,vplots);
        cpgsvp (0.05 + (vth)*plotwidth + offset,
                0.05 + (vth+1)*plotwidth + offset, 0.44, 0.56);
                                        /* Draw tick marks on top edge, in */
                                        /* real seconds/minutes */
        xmin = plotstart;
        xmax = plotend;
        cpgswin (xmin, xmax, 0.0, 1.0);
        for (plot_time=0.0; plot_time<plotend; plot_time+=*tickinc)
            {
            if (plot_time < plotstart) continue;
            ticksize = 0.015;
            if (fmod ((double)plot_time, majinc) < 0.01) ticksize = 0.03;
            if (fmod ((double)plot_time, 60.0) < 0.01) ticksize = 0.05;
            cpgmove (plot_time, 1.0 - ticksize);
            cpgdraw (plot_time, 1.0);
            }
                                        /* Switch to segment/amplitude space */
        ymax = status.delres_max * 3.0;
        if (ymax == 0.0)
            {
            msg ("overriding ymax of 0 in channel plot; data suspect", 2);
            ymax = 1.0;
            }
        cpgswin (0.0, (float)pass->num_ap, 0.0, ymax);
        if (i == start_plot)
            {
            cpgsch (0.5);
            cpgbox ("BC", 0.0, 0.0, "C", 0.0, 0.0);
            cpgbox ("", 0.0, 0.0, "BNST", 0.0, 0.0);
            cpgsch (0.7);
            cpgsci (4);
            cpgmtxt ("L", 1.5, 0.5, 0.5, "amplitude");
            cpgsci (1);
            }
        else cpgbox ("BC", 0.0, 0.0, "BC", 0.0, 0.0);
                                        /* Fourfit freq identifiers */
        cpgsch (0.7);
        if (i == start_plot+nplots-1)
            cpgmtxt ("T", 0.5, 0.5, 0.5, "All");
        else
            {
            if ((cptr = strchr(param.clones[0], pass->pass_data[i].freq_code)))
                {   // normal channel source of clone at [cptr-clones[0]]
                cloner++;
                sprintf (buf, "%c(%c)", pass->pass_data[i].freq_code,
                    param.clones[1][cptr - param.clones[0]]);
                cpgsci (3);
                }
            else if ((cptr = strchr(param.clones[1],
                pass->pass_data[i].freq_code)))
                {   // clone of normal channel at [cptr - clones[1]]
                cloner++;
                cpgsci (3);
                sprintf (buf, "%c(%c)", pass->pass_data[i].freq_code,
                    param.clones[0][cptr - param.clones[1]]);
                }
            else
                {   // normal freq_code
                sprintf (buf, "%c", pass->pass_data[i].freq_code);
                }
            cpgmtxt ("T", 0.5, 0.5, 0.5, buf);
            cpgsci (1);
            }
                                        /* Amplitudes in blue */
        cpgsci (4);
        cpgslw (lwid);
        for (j=0; j<np; j++)
            {
            xr[j] = plot.mean_ap[i][j];
            yr[j] = plot.seg_amp[i][j];
            if (yr[j] > 0.0) cpgpt1 (xr[j], yr[j], -1);
            }
        cpgslw (1.0);
                                        /* Connect lines in cyan */
        cpgsci (5);
        for (j=0; j<np; j++)
            {
            if (j > 0)
                {
                if ((yr[j] > 0.0) && (yr[j-1] > 0.0)) cpgdraw (xr[j], yr[j]);
                else cpgmove (xr[j], yr[j]);
                }
            else cpgmove (xr[j], yr[j]);
            }
        cpgsci (1);
                                        /* Phase as red dots */
        cpgswin (0.0, (float)pass->num_ap, -180.0, 180.0);
        if (i == (start_plot+nplots-1))
            {
            cpgsch (0.5);
            cpgbox ("", 0.0, 0.0, "CMST", 90.0, 3.0);
            cpgsch (0.7);
            cpgsci (2);
            if (cloner) {
                cpgmtxt ("R", 1.9, 0.5, 0.5, "phase");
                cpgsci (3);
                cpgmtxt ("R", 3.1, 0.5, 0.5, "(CLONES)");
            } else
                cpgmtxt ("R", 2.0, 0.5, 0.5, "phase");
            cpgsci (1);
            }
        cpgsci (2);
        cpgslw (lwid);
        for (j=0; j<np; j++)
            {
            yr[j] = plot.seg_phs[i][j] * 57.29578;
            if (plot.seg_amp[i][j] > 0.0) cpgpt1 (xr[j], yr[j], -1);
            }
                                        /* Rest of plots absent for */
                                        /* allfreq plot */
        if ((i == start_plot+nplots-1) && (nplots > 1)) continue;
                                        /* Fractional data plots */
        cpgsci (1);
        cpgslw (1);
        msg("set_seg_window %f..%f %d/%d",1,
            0.05 + (vth)*plotwidth,
            0.05 + (vth+1)*plotwidth, vth,vplots);
        cpgsvp (0.05 + (vth)*plotwidth,
                0.05 + (vth+1)*plotwidth, 0.41, 0.44);
        cpgswin (0.0, (float)pass->num_ap, -1.1, 1.1);
                                        /* feedback on weight trimming */
        vwgt = (pass->control.min_weight > 0) ? pass->control.min_weight : 0.95;
        vclr = (pass->control.min_weight > 0) ? 8   /* orange */         : 2;
                                        /* USB numbers, color coded */
        cpgsci (3);                                         /* green */
        for (j=0; j<np; j++)
            {
            if (plot.seg_frac_usb[i][j] < 0.95) continue;   /* valid weight */
            cpgmove (xr[j], 0.0);
            cpgdraw (xr[j], plot.seg_frac_usb[i][j]);
            }
        cpgsci (vclr);                                      /* red */
        for (j=0; j<np; j++)
            {
            if (plot.seg_frac_usb[i][j] >= 0.95) continue;  /* valid weight */
            if (plot.seg_frac_usb[i][j] <= 0.0) continue;
            cpgmove (xr[j], 0.0);
            cpgdraw (xr[j], plot.seg_frac_usb[i][j]);
            }
                                        /* LSB bars point down */
        cpgsci (3);                                         /* green */
        for (j=0; j<np; j++)
            {
            if (plot.seg_frac_lsb[i][j] < 0.95) continue;   /* valid weight */
            cpgmove (xr[j], 0.0);
            cpgdraw (xr[j], -plot.seg_frac_lsb[i][j]);
            }
        cpgsci (vclr);                                      /* red */
        for (j=0; j<np; j++)
            {
            if (plot.seg_frac_lsb[i][j] >= 0.95) continue;  /* valid weight */
            if (plot.seg_frac_lsb[i][j] <= 0.0) continue;
            cpgmove (xr[j], 0.0);
            cpgdraw (xr[j], -plot.seg_frac_lsb[i][j]);
            }
                                        /* Draw box last - zero line is impt. */
        cpgsci (1);
        cpgbox ("AB", 0.0, 0.0, "BC", 0.0, 0.0);
                                        /* State count plots */
        if (status.stc_present)         // draw box only if at least one plot non-empty
            {
            cpgsci (1);
            cpgslw (1.0);
            cpgsch (0.5);
            msg("set_seg_window %f..%f %d/%d",1,
                0.05 + (vth)*plotwidth,
                0.05 + (vth+1)*plotwidth, vth,vplots);
            cpgsvp (0.05 + (vth)*plotwidth,
                    0.05 + (vth+1)*plotwidth, 0.395, 0.41);
            cpgswin (0.0, (float)pass->num_ap, -0.02, 0.02);
            cpgbox ("BC", 0.0, 0.0, "BC", 0.0, 0.0);
            }
                                        /* Ref station green */
        if (status.stc_present & 1)
            {
            cpgsci (3);
            maxj = 0;
            for (j=0; j<np; j++)
                {
                yr[j] = plot.seg_refbias_usb[i][j];
                if (yr[j] < -1.0) ;
                else if (yr[j] < -0.02)     /* Indicate limits with bars */
                    {
                    cpgmove (xr[j],-.015);
                    cpgdraw (xr[j],-.02);
                    }
                else if (yr[j] > 0.02)
                    {
                    cpgmove (xr[j],.015);
                    cpgdraw (xr[j],.02);
                    }
                if (yr[j] > -1.0) maxj = j;
                }
            cpgline (maxj, xr, yr);
            }
                                        /* Rem station magenta */
        if (status.stc_present & 2)
            {
            cpgsci (6);
            maxj = 0;
            for (j=0; j<np; j++)
                {
                yr[j] = plot.seg_rembias_usb[i][j];
                if (yr[j] < -1.0) ;
                else if (yr[j] < -0.02)     /* Indicate limits with bars */
                    {
                    cpgmove (xr[j],-.015);
                    cpgdraw (xr[j],-.02);
                    }
                else if (yr[j] > 0.02)
                    {
                    cpgmove (xr[j],.015);
                    cpgdraw (xr[j],.02);
                    }
                if (yr[j] > -1.0) maxj = j;
                }
            cpgline (maxj, xr, yr);
            }
                                        /* LSB */
        if (status.stc_present)         // draw box only if at least one plot non-empty
            {
            cpgsci (1);
            cpgslw (1.0);
            cpgsch (0.5);
            msg("set_seg_window %f..%f %d/%d",1,
                0.05 + (vth)*plotwidth,
                0.05 + (vth+1)*plotwidth, vth,vplots);
            cpgsvp (0.05 + (vth)*plotwidth,
                    0.05 + (vth+1)*plotwidth, 0.38, 0.395);
            cpgswin (0.0, (float)pass->num_ap, -0.02, 0.02);
            cpgbox ("BC", 0.0, 0.0, "BC", 0.0, 0.0);
            }
                                        /* Ref station green */
        if (status.stc_present & 1)
            {
            cpgsci (3);
            maxj = 0;
            for (j=0; j<np; j++)
                {
                yr[j] = plot.seg_refbias_lsb[i][j];
                if (yr[j] < -1.0) ;
                else if (yr[j] < -0.02)     /* Indicate limits with bars */
                    {
                    cpgmove (xr[j],-.015);
                    cpgdraw (xr[j],-.02);
                    }
                else if (yr[j] > 0.02)
                    {
                    cpgmove (xr[j],.015);
                    cpgdraw (xr[j],.02);
                    }
                if (yr[j] > -1.0) maxj = j;
                }
            cpgline (maxj, xr, yr);
            }
                                        /* Rem station magenta */
        if (status.stc_present & 2)
            {
            cpgsci (6);
            maxj = 0;
            for (j=0; j<np; j++)
                {
                yr[j] = plot.seg_rembias_lsb[i][j];
                if (yr[j] < -1.0) ;
                else if (yr[j] < -0.02)     /* Indicate limits with bars */
                    {
                    cpgmove (xr[j],-.015);
                    cpgdraw (xr[j],-.02);
                    }
                else if (yr[j] > 0.02)
                    {
                    cpgmove (xr[j],.015);
                    cpgdraw (xr[j],.02);
                    }
                if (yr[j] > -1.0) maxj = j;
                }
            cpgline (maxj, xr, yr);
            }
                                        /* Big versus small */
        if (status.stc_present)         // draw box only if at least one plot non-empty
            {
            cpgsci (1);
            cpgslw (1.0);
            cpgsch (0.5);
            msg("set_seg_window %f..%f %d/%d",1,
                0.05 + (vth)*plotwidth,
                0.05 + (vth+1)*plotwidth, vth,vplots);
            cpgsvp (0.05 + (vth)*plotwidth,
                    0.05 + (vth+1)*plotwidth, 0.36, 0.38);
            cpgswin (0.0, (float)pass->num_ap, 0.54, 0.72);
            cpgbox ("BC", 0.0, 0.0, "BC", 0.0, 0.0);
            }
        if (status.stc_present & 1)
            {
                                        /* Ref station blue */
            cpgsci (4);
            maxj = 0;
            for (j=0; j<np; j++)
                {                           /* Check range of data */
                yr[j] = plot.seg_refscnt_usb[i][j];
                if (yr[j] < 0.0) ;
                else if (yr[j] < 0.54)     /* Indicate limits with bars */
                    {
                    cpgmove (xr[j],0.56);
                    cpgdraw (xr[j],0.54);
                    }
                else if (yr[j] > 0.72)
                    {
                    cpgmove (xr[j],0.70);
                    cpgdraw (xr[j],0.72);
                    }
                if (yr[j] > 0.0) maxj = j;
                }
            cpgline (maxj, xr, yr);
            }
                                        /* Rem station red */
        if (status.stc_present & 2)
            {
            cpgsci (2);
            maxj = 0;
            for (j=0; j<np; j++)
                {                           /* Check range of data */
                yr[j] = plot.seg_remscnt_usb[i][j];
                if (yr[j] < 0.0) ;
                else if (yr[j] < 0.54)     /* Indicate limits with bars */
                    {
                    cpgmove (xr[j],0.56);
                    cpgdraw (xr[j],0.54);
                    }
                else if (yr[j] > 0.72)
                    {
                    cpgmove (xr[j],0.70);
                    cpgdraw (xr[j],0.72);
                    }
                if (yr[j] > 0.0) maxj = j;
                }
            cpgline (maxj, xr, yr);
            }
                                        /* LSB */
        if (status.stc_present)         // draw box only if at least one plot non-empty
            {
            cpgsci (1);
            cpgslw (1.0);
            cpgsch (0.5);
            msg("set_seg_window %f..%f %d/%d",1,
                0.05 + (vth)*plotwidth,
                0.05 + (vth+1)*plotwidth, vth,vplots);
            cpgsvp (0.05 + (vth)*plotwidth,
                    0.05 + (vth+1)*plotwidth, 0.34, 0.36);
            cpgswin (0.0, (float)pass->num_ap, 0.54, 0.72);
            cpgbox ("BC", 0.0, 0.0, "BC", 0.0, 0.0);
            }
                                        /* Ref station blue */
        if (status.stc_present & 1)
            {
            cpgsci (4);
            maxj = 0;
            for (j=0; j<np; j++)
                {
                yr[j] = plot.seg_refscnt_lsb[i][j];
                if (yr[j] < 0.0) ;
                else if (yr[j] < 0.54)     /* Indicate limits with bars */
                    {
                    cpgmove (xr[j],0.56);
                    cpgdraw (xr[j],0.54);
                    }
                else if (yr[j] > 0.72)
                    {
                    cpgmove (xr[j],0.70);
                    cpgdraw (xr[j],0.72);
                    }
                if (yr[j] > 0.0) maxj = j;
                }
            cpgline (maxj, xr, yr);
            }
                                        /* Rem station red */
        if (status.stc_present & 2)
            {
            cpgsci (2);
            maxj = 0;
            for (j=0; j<np; j++)
                {
                yr[j] = plot.seg_remscnt_lsb[i][j];
                if (yr[j] < 0.0) ;
                else if (yr[j] < 0.54)     /* Indicate limits with bars */
                    {
                    cpgmove (xr[j],0.56);
                    cpgdraw (xr[j],0.54);
                    }
                else if (yr[j] > 0.72)
                    {
                    cpgmove (xr[j],0.70);
                    cpgdraw (xr[j],0.72);
                    }
                if (yr[j] > 0.0) maxj = j;
                }
            cpgline (maxj, xr, yr);
            }
        yplace = (status.stc_present) ? 0.34 : 0.41;

                                        /* Phasecal phase plots */
        cpgsci (1);
        cpgslw (1.0);
        cpgsch (0.5);
        msg("set_seg_window %f..%f %d/%d",1,
            0.05 + (vth)*plotwidth,
            0.05 + (vth+1)*plotwidth, vth,vplots);
        cpgsvp (0.05 + (vth)*plotwidth,
                0.05 + (vth+1)*plotwidth,
                yplace - 0.05, yplace);
        yplace -= 0.065;
                                        /* Draw tick marks on bottom edge, in */
                                        /* real seconds/minutes */
        xmin = plotstart;
        xmax = plotend;
        cpgswin (xmin, xmax, 0.0, 1.0);
        for (plot_time=0.0; plot_time<plotend; plot_time+=*tickinc)
            {
            if (plot_time < plotstart) continue;
            ticksize = 0.039;
            if (fmod ((double)plot_time, majinc) < 0.01) ticksize = 0.078;
            if (fmod ((double)plot_time, 60.0) < 0.01) ticksize = 0.13;
            cpgmove (plot_time, ticksize);
            cpgdraw (plot_time, 0.0);
            }
                                        /* Back to AP space */
        cpgswin (0.0, (float)pass->num_ap, -180.0, 180.0);
        cpgbox ("BC", 0.0, 0.0, "B", 0.0, 0.0);
                                        /* Ref station green */
        cpgsci (3);
        cpgslw (lwid);
        for (j=0; j<np; j++)
            yr[j] = plot.seg_refpcal[i][j];
        cpgpt (np, xr, yr, -1);
                                        /* Rem station magenta */
        cpgsci (6);
        for (j=0; j<np; j++)
            yr[j] = plot.seg_rempcal[i][j];
        cpgpt (np, xr, yr, -1);
                                        /* Left ticks, axis labels */
        cpgslw (1.0);
        cpgsci (1);
        if (i == start_plot)
            {
            cpgsch (0.35);
            cpgbox ("", 0.0, 0.0, "BST", 0.0, 0.0);
            cpgsch (0.7);
            cpgmtxt ("L", 1.5, 0.5, 0.5, "pcal \\gh");
            }
                                        /* Right ticks/axis labels */
        if ((i == nplots-2+start_plot) || (nplots == 1))
            {
            cpgsch (0.35);
            cpgbox ("", 0.0, 0.0, "CMST", 0.0, 0.0);
            }
        }

    cpgsci (1);
                                        /* Finished with graphics, and PGPLOT */
    cpgend();
    return (0);
    }
