/************************************************************************/
/*                                                                      */
/* Generates the graphical part of a fringe plot                        */
/*                                                                      */
/* Created make_postplot original            October 1999 by CJL        */
/* Refactored into a separate routine            2014.7.30   rjc        */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include "param_struct.h"
#include "pass_struct.h"
#include "ovex.h"
#include "type_comp.h"
#include "cpgplot.h"

int generate_graphs (struct scan_struct *root,
                     struct type_pass *pass,
                     char *fringename,
                     char *ps_file,
                     double *tickinc)
    {
    extern struct mk4_fringe fringe;
    extern struct type_plot plot;
    extern struct type_param param;
    extern struct type_status status;
    extern double c_phase(), c_mag();
    int i, j, maxj;
    int start_plot, limit_plot;
    char buf[2560], device[256];
    double drate, mbd, sbd;
    struct tm *gmtime();
    float xr[2*MAXMAX], yr[2*MAXMAX], zr[2*MAXMAX];
    float xmin, xmax, ymin, ymax, plotwidth;
    float xpos, offset, lwid, yplace;
    double max_dr_win, max_sb_win, max_mb_win;
    float max_amp;
    double plotstart, plotdur, plotend, totdur, majinc, ticksize;
    float plot_time, bandwidth, xstart, xend;
    int nsbd, ncp, np, nplots;
    int totpts, wrap;
    int nlsb, nusb, izero;
                                        /* Build the proper device string for */
                                        /* vertically oriented color postscript */
    sprintf (device, "%s/vcps", ps_file);
                                        /* Open the pgplot device */
    if (cpgopen (device) <= 0)
        {
        msg ("Postscript plot open failed (%s)", 2, ps_file);
        return (-1);
        }
                                        /* Redefine pgplot green to be a bit darker */
    cpgscr (3, 0.0, 0.6, 0.0);
                                        /* Make pgplot compute a bounding */
                                        /* box encompassing the whole page */
    cpgsvp (0.0, 1.0, 0.0, 1.0);
    cpgswin (0.0, 1.0, 0.0, 1.0);
    cpgsci (0);
    cpgmove (0.0, 0.0);
    cpgdraw (1.0, 0.0);
    cpgdraw (0.0, 1.0);
    cpgsci (1);
                                        /* Delay-rate spectrum */
    xmin = xmax = 0;
    max_amp = 1e4 * fringe.t208->amplitude;
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
        {
        cpgsvp (0.05, 0.80, 0.767, 0.768);
        cpgswin (xmin, xmax, 0.0, 1.0);
        xpos = param.win_dr[0] * 1.0e3;
        cpgmove (xpos, 0.5);
        xpos = param.win_dr[1] * 1.0e3;
        cpgslw (3);
        cpgdraw (xpos, 0.5);
        cpgslw (1);
                                        /* Restore viewport for MBD */
        cpgsvp (0.05, 0.80, 0.77, 0.95);
        }
    cpgsci (1);
                                        /* Multiband delay resolution function */
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
            {
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
                                        /* Draw in search window */
    max_sb_win = 0.5e+06 * param.samp_period;
    if ((param.win_sb[0] > -max_sb_win) || (param.win_sb[1] < max_sb_win))
        {
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
                                        // ionosphere search points (iff present)
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

    ymax = 0;
    for (i=0; i<ncp; i++)
        {
        xr[i] = xstart + (xend - xstart) * i / ncp;
        yr[i] = c_mag (plot.cp_spectrum[i+izero]);
        zr[i] = c_phase (plot.cp_spectrum[i+izero]) * 57.3;
        if (yr[i] > ymax) ymax = yr[i];
        }
    ymin = (param.passband[0] == 0.0 && param.passband[1] == 1.0E6)
         ? 0 : ymax * 9e-3;                 /* about 3 pixels at 300px/in */
    for (i=0; i<ncp; i++) if (yr[i] < ymin) zr[i] = 0;
    ymax *= 1.2;
    if (ymax == 0.0)
        {
        msg ("overriding ymax of 0 in Xpower Spectrum plot; data suspect", 2);
        ymax = 1.0;
        }
    cpgsvp (0.43, 0.80, 0.63, 0.74);
    cpgswin(xstart, xend, 0.0, ymax);
    cpgsch (0.5);
    cpgbox ("BCNST", 0.0, 0.0, "BNST", 0.0, 0.0);
    cpgsch (0.7);
    cpgmtxt("B", 2.0, 0.5, 0.5, "Avgd. Xpower Spectrum (MHz)");
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
                                        /* Cross-power phase */
    cpgswin(xstart, xend, -180.0, 180.0);
    cpgsci (1);
    cpgsch (0.5);
    cpgbox ("", 0.0, 0.0, "CMST", 90.0, 3.0);
    cpgsch (0.7);
    cpgsci (2);
    cpgslw (5.0);
    cpgpt (ncp, xr, zr, -1);
    cpgslw (1.0);
    cpgmtxt ("R", 2.0, 0.5, 0.5, "phase (deg)");
                                        /* Now set up channel/time plots */
                                        /* Figure out width of individual plot */
    cpgsci (1);

    start_plot = (param.first_plot == FALSE) ? 0 : param.first_plot;
    limit_plot = (param.nplot_chans == FALSE) ? pass->nfreq : param.nplot_chans;

    nplots = (limit_plot == 1) ? 1 : limit_plot+1;
   
    plotwidth = 0.88 / (double)nplots;
    if (nplots == 1) plotwidth = 0.8;
                                        /* Adjust line width to make dots legible */
    totpts = nplots * status.nseg;
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
                                        /* Figure out how many minor ticks to */
                                        /* draw (if any) */
    totdur = plotdur * nplots;
    if (totdur > 6000) {*tickinc = 60.0; majinc = 60.0;}
    else if (totdur > 3000.0) {*tickinc = 30.0; majinc = 60.0;}
    else if (totdur > 1000.0) {*tickinc = 10.0; majinc = 60.0;}
    else if (totdur > 500.0) {*tickinc = 5.0; majinc = 30.0;}
    else if (totdur > 200.0) {*tickinc = 2.0; majinc = 10.0;}
    else {*tickinc = 1.0; majinc = 5.0;}
                                        /* Segment 0 starts at param.minap */
                                        /* Loop over plots */
    
    for (i=start_plot; i<start_plot+nplots; i++)
        {
        np = status.nseg;
        offset = 0.0;
        if ((i == nplots+limit_plot-1) && (nplots != 1)) offset = 0.01;
        cpgsvp (0.05 + (i-start_plot)*plotwidth + offset, 0.05 + (i+1-start_plot)*plotwidth + offset, 
                                                                    0.44, 0.56);
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
        if (i == start_plot+nplots-1) cpgmtxt ("T", 0.5, 0.5, 0.5, "All");
        else 
            {
            sprintf (buf, "%c", pass->pass_data[i].freq_code);
            cpgmtxt ("T", 0.5, 0.5, 0.5, buf);
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
        cpgsvp (0.05 + (i-start_plot)*plotwidth, 0.05 + (i+1-start_plot)*plotwidth, 0.41, 0.44);
        cpgswin (0.0, (float)pass->num_ap, -1.1, 1.1);
                                        /* USB numbers, color coded */
        cpgsci (3);
        for (j=0; j<np; j++)
            {
            if (plot.seg_frac_usb[i][j] < 0.95) continue;
            cpgmove (xr[j], 0.0);
            cpgdraw (xr[j], plot.seg_frac_usb[i][j]);
            }
        cpgsci (2);
        for (j=0; j<np; j++)
            {
            if (plot.seg_frac_usb[i][j] >= 0.95) continue;
            if (plot.seg_frac_usb[i][j] <= 0.0) continue;
            cpgmove (xr[j], 0.0);
            cpgdraw (xr[j], plot.seg_frac_usb[i][j]);
            }
                                        /* LSB bars point down */
        cpgsci (3);
        for (j=0; j<np; j++)
            {
            if (plot.seg_frac_lsb[i][j] < 0.95) continue;
            cpgmove (xr[j], 0.0);
            cpgdraw (xr[j], -plot.seg_frac_lsb[i][j]);
            }
        cpgsci (2);
        for (j=0; j<np; j++)
            {
            if (plot.seg_frac_lsb[i][j] >= 0.95) continue;
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
            cpgsvp (0.05 + (i-start_plot)*plotwidth, 0.05 + (i+1-start_plot)*plotwidth, 0.395, 0.41);
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
            cpgsvp (0.05 + (i-start_plot)*plotwidth, 0.05 + (i+1-start_plot)*plotwidth, 0.38, 0.395);
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
            cpgsvp (0.05 + (i-start_plot)*plotwidth, 0.05 + (i+1-start_plot)*plotwidth, 0.36, 0.38);
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
            cpgsvp (0.05 + (i-start_plot)*plotwidth, 0.05 + (i+1-start_plot)*plotwidth, 0.34, 0.36);
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
        cpgsvp (0.05 + (i-start_plot)*plotwidth, 0.05 + (i+1-start_plot)*plotwidth, 
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
