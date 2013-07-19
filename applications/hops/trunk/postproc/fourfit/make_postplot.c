/************************************************************************/
/*                                                                      */
/* Creates a postscript Mk4 fringe plot.  It does this in two parts,    */
/* one graphical and one textual.  The graphical part (plus some graph  */
/* labelling) is done using PGPLOT, and the output is flushed to a      */
/* scratch file.  The file is then read in, and textual information is  */
/* spliced in using a standard postscript font, yielding much more      */
/* compact files than are obtained with PGPLOT vector fonts.            */
/*                                                                      */
/*                                                                      */
/*                                                                      */
/* Created October 1999 by CJL                                          */
/*                                                                      */
/************************************************************************/
#include "mk4_data.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <math.h>
#include <string.h>
#include <limits.h>
#include "param_struct.h"
#include "filter.h"
#include "statistics.h"
#include "pass_struct.h"
#include "ovex.h"
#include "type_comp.h"
#include "cpgplot.h"

#ifdef P_tmpdir
# define P_tmpdir "/tmp"
#endif /* P_tmpdir */

#define pi 3.141592654
                                        // polararization bit masks
#define LREF 1
#define RREF 2
#define LREM 4
#define RREM 8
                                        /* Set up some convenient macros */
                                        /* for inserting justified text using */         
                                        /* native postscript fonts */
#define pscat(ps_string) strcat (pplot, ps_string)
#define psleft(xcoord, ycoord, ps_string)\
             {xval = xcoord * 7570; yval = ycoord * 10500;\
                sprintf (psbuf, "%d %d M (%s) SL\n", xval, yval, ps_string);\
                pscat (psbuf);}
#define psright(xcoord, ycoord, ps_string)\
             {xval = xcoord * 7570; yval = ycoord * 10500;\
                sprintf (psbuf, "%d %d M (%s) SR\n", xval, yval, ps_string);\
                pscat (psbuf);}
                                        /* Color shorthand */
#define setred pscat ("1.0 0.0 0.0 setrgbcolor\n")
#define setblue pscat ("0.0 0.0 1.0 setrgbcolor\n")
#define setcyan pscat ("0.0 1.0 1.0 setrgbcolor\n")
#define setorange pscat ("1.0 0.5 0.0 setrgbcolor\n")
#define setgreen pscat ("0.0 0.6 0.0 setrgbcolor\n")
#define setmagenta pscat ("1.0 0.0 1.0 setrgbcolor\n")
#define setblack pscat ("0.0 0.0 0.0 setrgbcolor\n")

                                        /* Simple little routine to convert dumb */
                                        /* space-delimited list into minimal */
                                        /* comma-separated one */
void
stripbuf (/* list) */
char *list)
    {
    int i, j, started;

    i = 0;
    started = FALSE;
    for (j=0; j<strlen(list); j++)
        {
        if (list[j] == ' ')
            {
            if (started && (list[i-1] != ',')) list[i] = ',';
            else i--;
            }
        else
            {
            started = TRUE;
            list[i] = list[j];
            }
        i++;
        }
    if (list[i-1] == ',') i--;
    list[i] = '\0';
    }

int
make_postplot (/* root, pass, fringename, t221) */
struct scan_struct *root,
struct type_pass *pass,
char *fringename,
struct type_221 **t221)
    {
    extern struct mk4_fringe fringe;
    extern struct type_plot plot;
    extern struct type_param param;
    extern struct type_status status;
    extern struct type_filter filter;
    extern struct type_statistics statistics;
    extern int test_mode;
    extern int msglev;
    extern char control_filename[];
    extern char *pexec;
    extern char version_no[], progname[];
    extern char *sprint_char_arr(), *sprint_date();
    extern double c_phase(), c_mag();
    struct date tmp_date;
    struct stat file_status, xeq_stat;
    int i, line, j, maxj, k, dist, plotchans, pcref, pcrem, ref, started, end_at;
    int start_plot, limit_plot;
    char *rootname, *temp;
    char buf[2560], psbuf[2560], device[256], output_filename[256];
    char pol[3],input_filename[256], polstr[13];
    float tempfl,mb_width;
    double phase, ampl, drate, mbd, sbd;
    time_t tm;
    struct tm *utc_now, *gmtime(), *utc_pgm;
    float xr[2*MAXMAX], yr[2*MAXMAX], zr[2*MAXMAX];
    float xmin, xmax, ymin, ymax, sec1, sec2, plotwidth;
    float xpos, xposref,xposrem, ypos, spacing, offset, labelpos, lwid, yplace;
    double max_dr_win, max_sb_win, max_mb_win;
    double plotstart, plotdur, plotend, totdur, tickinc, majinc, ticksize;
    float plot_time, trackerr, digitwidth, bandwidth, xstart, xend;
    int nc, nsbd, ncp, np, hr1, hr2, min1, min2, xval, yval, nplots, year;
    int totpts, wrap, len, first, nb, fd, size, filesize;
    int nlsb, nusb, izero, numsb, srate;
    int pcaref, pcarem;
    int eff_npols, polars;
    FILE *fp, *gs, *popfil;
    char *pplot, *showpage, *end, trailer[1024];
    char absexec[256], which_command[256];
    char buffer[32];
    static char *pcstr[5]={"","NORMAL","AP BY AP","MANUAL", "MULTITONE"};
    static char ps_file[1024];
                                        /* Create a temporary file to hold */
                                        /* the postscript output */
    strcpy(ps_file, P_tmpdir "/fourfit_XXXXXX");
    close(mkstemp(ps_file));
    msg ("Temporary postscript filename = '%s'", 0, ps_file);
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
    ymax = 0;
    for(i=0; i<plot.dr_size_max; i++)
        {                               /* Convert to ns/s */
        drate = (double)i - (double)plot.dr_size_max / 2.0;
        drate /= plot.dr_size_max * param.acc_period * fringe.t205->ref_freq;
        drate *= 1000.0;
        if (drate < xmin) xmin = drate;
        if (drate > xmax) xmax = drate;
        xr[i] = drate;
        yr[i] = plot.d_rate[i];
        if (yr[i] > ymax) ymax = yr[i];
        }
    if (ymax == 0.0)
        {
        msg ("overriding ymax of 0 in delay rate spectrum; data suspect", 2);
        ymax = 1.0;
        }
    cpgsvp (0.05, 0.80, 0.77, 0.95);
    cpgswin(xmin, xmax, 0.0, ymax);
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
        ymax = 0;
        for (i=0; i<256; i++)
            {
            mbd = (double)i - 128.0;
            mbd /= 256.0;
            mbd /= status.freq_space;
            if (mbd < xmin) xmin = mbd;
            if (mbd > xmax) xmax = mbd;
            xr[i] = mbd;
            yr[i] = plot.mb_amp[i];
            if (yr[i] > ymax) ymax = yr[i];
            }
        if (ymax == 0.0)
            {
            msg ("overriding ymax of 0 in multiband delay plot; data suspect", 2);
            ymax = 1.0;
            }
        cpgswin (xmin, xmax, 0.0, ymax);
        cpgsch (0.5);
        cpgbox ("CMST", 0.0, 0.0, "CMST", 0.0, 0.0);
        cpgsch (0.7);
        cpgsci (4);
        cpgmtxt("R", 2.0, 0.5, 0.5, "amplitude");
        cpgmtxt("T", 1.5, 0.5, 0.5, "multiband delay (\\gms)");
        cpgline(256, xr, yr);
                                        /* Draw in search window */
        max_mb_win = 0.5 / status.freq_space;
        if ((param.win_mb[0] > -max_mb_win) || (param.win_mb[1] < max_mb_win))
            {
                                        /* This is complicated by wrap possibility */
            wrap = FALSE;
            if (param.win_mb[0] > param.win_mb[1]) wrap = TRUE;
            cpgsvp (0.05, 0.80, 0.952, 0.953);
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
    ymax = 0;
    for (i=0; i<nsbd; i++)
        {
        sbd = (double)i - (double)nsbd / 2.0;
        sbd *= status.sbd_sep;
        if (sbd < xmin) xmin = sbd;
        if (sbd > xmax) xmax = sbd;
        xr[i] = sbd;
        yr[i] = plot.sb_amp[i];
        if (yr[i] > ymax) ymax = yr[i];
        }
    if (ymax == 0.0)
        {
        msg ("overriding ymax of 0 in singleband delay plot; data suspect", 2);
        ymax = 1.0;
        }
    cpgsvp (0.05, 0.35, 0.63, 0.74);
    cpgswin (xmin, xmax, 0.0, ymax);
    cpgsch (0.5);
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
                                        /* Cross-power spectrum - amplitude */
    nlsb = nusb = numsb = 0;            /* count up total usb & lsb AP's */
    for (i=0; i<pass->nfreq; i++)
        {
        nusb += status.ap_num[0][i];
        nlsb += status.ap_num[1][i];
        if (status.ap_num[0][i])        /* Also tally # active sidebands */
            numsb++;
        if (status.ap_num[1][i])
            numsb++;
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
    cpgdraw (0.0, ymax);
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
//    nplots = pass->nfreq + 1;
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
    if (totdur > 6000) {tickinc = 60.0; majinc = 60.0;}
    else if (totdur > 3000.0) {tickinc = 30.0; majinc = 60.0;}
    else if (totdur > 1000.0) {tickinc = 10.0; majinc = 60.0;}
    else if (totdur > 500.0) {tickinc = 5.0; majinc = 30.0;}
    else if (totdur > 200.0) {tickinc = 2.0; majinc = 10.0;}
    else {tickinc = 1.0; majinc = 5.0;}
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
        for (plot_time=0.0; plot_time<plotend; plot_time+=tickinc)
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
        for (plot_time=0.0; plot_time<plotend; plot_time+=tickinc)
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
                                        /* Now need to read in the resulting */
                                        /* postscript file.  This is done by */
                                        /* creating a type 221 record in */
                                        /* allocated memory, and reading the */
                                        /* file into the pplot member */
    fp = fopen (ps_file, "r");
                                        /* Map stream pointer onto file */
                                        /* descriptor, and make stat() call */
                                        /* to figure out file size */
    if ((fd = fileno (fp)) < 0)
        {
        msg ("Problem with stream pointer in read_mk4file()", 2);
        return (-1);
        }
    if (fstat (fd, &file_status) != 0)
        {
        msg ("Problem making stat call in read_mk4file()", 2);
        return (-1);
        }
    filesize = file_status.st_size;
                                        /* Add 50,000 to allow for text */
    size = filesize + 50000;
                                        /* Allocate memory for type_221 record */
    if ((*t221 = (struct type_221 *)malloc (size)) == NULL)
        {
        msg ("Memory allocation error in read_mk4file()", 2);
        return (-1);
        }
                                        /* Initialize it */
    clear_221 (*t221);
                                        /* Make sure we are at start of file */
    rewind (fp);
                                        /* Figure out starting address of the */
                                        /* postscript instructions */
    pplot = (*t221)->pplot;
                                        /* Read file in a single call, let */
                                        /* system figure out best buffering */
    nb = fread (pplot, sizeof(char), filesize, fp);
    pplot[filesize] = 0;                // terminate with null to be safe
                                        /* Did it go OK? */
    if (nb != filesize)
        {
        msg ("Error, expected %d bytes, read %d bytes", 2, filesize, nb);
        return (-1);
        }
                                        /* Tidy up */
    fclose (fp);
    unlink (ps_file);
                                        /* Forcibly null-terminate file image */
    if ((end = strstr (pplot, "EOF\n")) != NULL)
        *(end+4) = '\0';
                                        /* Store away trailing part of file */
    if ((showpage = strstr (pplot, "PGPLOT restore showpage")) != NULL)
        {
        strcpy (trailer, showpage);
                                        /* Null terminate what's left */
        showpage[0] = '\0';
        }
                                        /* Build up text part of file */
                                        /* Start appending strings */
    pscat ("/SL {show} def\n");
    pscat ("/SR {dup stringwidth neg exch neg exch rmoveto show} def\n");
    pscat ("/M {moveto} def\n");
    pscat ("/Helvetica-Bold findfont 180 scalefont setfont\n");

    if (param.corr_type == DIFX)
        sprintf (buffer, "Mk4/DiFX %s %s rev %hd", 
                 progname, version_no, fringe.t200->software_rev[0]);
    else
        sprintf (buffer, "Mk4/hdw. %s %s rev %hd", 
                 progname, version_no, fringe.t200->software_rev[0]);
    psleft (0.0, 0.98, buffer)
                                        /* Identification information */
    if ((rootname = strrchr (root->filename, '/')) == NULL)
        rootname = root->filename;
    else rootname++;
    sprintf (buf, "%s, %s, %s", rootname, root->scan_name, param.baseline);
    psright (1.0, 0.98, buf);
                                        /* Convert to pol. string */
    polars = 0;
    if (param.pol == 0)
        {
        if (pass->pol == POL_LL) 
            {
            sprintf (polstr, "LL");
            polars |= LREF | LREM;
            }
        else if (pass->pol == POL_RR)
            {
            sprintf (polstr, "RR");
            polars |= RREF | RREM;
            }
        else if (pass->pol == POL_LR) 
            {
            sprintf (polstr, "LR");
            polars |= LREF | RREM;
            }
        else if (pass->pol == POL_RL) 
            {
            sprintf (polstr, "RL");
            polars |= RREF | LREM;
            }
        }
    else if (param.pol < 16)
        {
        polstr[0] = '\0';
        if (param.pol & POLMASK_LL)
            {
            strcat (polstr, "+LL");
            polars |= LREF | LREM;
            }
        if (param.pol & POLMASK_RR)
            {
            strcat (polstr, "+RR");
            polars |= RREF | RREM;
            }
        if (param.pol & POLMASK_LR)
            {
            strcat (polstr, "+LR");
            polars |= LREF | RREM;
            }
        if (param.pol & POLMASK_RL)
            {
            strcat (polstr, "+RL");
            polars |= RREF | LREM;
            }
        strcpy (polstr, polstr+1);      // chop off leading '+'
        }
    else
        {
        sprintf (polstr, "Ixy");
        polars |= LREF | LREM | RREF | RREM;
        }
    sprintf (buf, "%.8s - %.8s, fgroup %c, pol %s", fringe.t202->ref_name,
                    fringe.t202->rem_name, pass->pass_data[0].fgroup, polstr);
    pscat ("/Helvetica-Bold findfont 140 scalefont setfont\n");
    psright (1.0, 0.965, buf);
                                        /* Some solution essentials in a visible spot */
    pscat ("/Helvetica findfont 130 scalefont setfont\n");
    spacing = 0.0128;
    ypos = 0.945;
    sprintf (buf, "%c", fringe.t208->quality);
    psright (1.0, ypos, buf); ypos -= spacing;
    if (fringe.t208->errcode != ' ')
        {
        setred;
        sprintf (buf, "Error code  %c", fringe.t208->errcode);
        psright (1.0, ypos, buf);
        setblack;
        }
    ypos -= spacing;
    sprintf (buf, "%.1f", fringe.t208->snr);
    psright (1.0, ypos, buf); ypos -= spacing;
    sprintf (buf, "%.3f", fringe.t206->intg_time);
    psright (1.0, ypos, buf); ypos -= spacing;
    sprintf (buf, "%.3f", fringe.t208->amplitude * 10000.);
    psright (1.0, ypos, buf); ypos -= spacing;
    sprintf (buf, "%.1f", fringe.t208->resphase);
    psright (1.0, ypos, buf); ypos -= spacing;
    sprintf (buf, "%.1e", fringe.t208->prob_false);
    psright (1.0, ypos, buf); ypos -= spacing;
    ypos -= spacing;
    sprintf (buf, "%.6f", fringe.t208->resid_sbd);
    psright (1.0, ypos, buf); ypos -= spacing;
    sprintf (buf, "%.6f", fringe.t208->resid_mbd);
    psright (1.0, ypos, buf); ypos -= spacing;
    ypos -= spacing;
    sprintf (buf, "%.6f", status.dr_max_global * param.ref_freq);
    psright (1.0, ypos, buf); ypos -= spacing;
    sprintf (buf, "%6.2f", param.ion_diff);
    psright (1.0, ypos, buf); ypos -= spacing;
    ypos -= spacing;
    sprintf (buf, "%.4f", fringe.t205->ref_freq);
    psright (1.0, ypos, buf); ypos -= spacing;
    sprintf (buf, "%.3f", param.acc_period);
    psright (1.0, ypos, buf); ypos -= 1.5 * spacing;
                                        /* Global scan information */
    sprintf (buf, "%.8s", root->exper_name);
    psright (1.0, ypos, buf); ypos -= spacing;
    sprintf (buf, "%d", root->exper_num);
    psright (1.0, ypos, buf); ypos -= spacing;
    year = fringe.t200->scantime.year;
    if (year < 150) year += 1900;
    sprintf (buf, "%d:%03d", year, fringe.t200->scantime.day);
    psright (1.0, ypos, buf); ypos -= spacing;
    sec1 = fmod (pass->start, 8.64e4);
    hr1 = sec1 / 3.6e3;
    sec1 -= hr1 * 3.6e3;
    min1 = sec1 / 60;
    sec1 -= min1 * 60;
    sec2 = fmod (pass->stop, 8.64e4);
    hr2 = sec2 / 3.6e3;
    sec2 -= hr2 * 3.6e3;
    min2 = sec2 / 60;
    sec2 -= min2 * 60;
    sprintf (buf, "%02d%02d%05.2f", hr1, min1, sec1);
    psright (1.0, ypos, buf); ypos -= spacing;
    sprintf (buf, "%02d%02d%05.2f", hr2, min2, sec2);
    psright (1.0, ypos, buf); ypos -= spacing;
    sprintf (buf, "%02d%02d%05.2f", fringe.t200->frt.hour, fringe.t200->frt.minute,
                                                        fringe.t200->frt.second);
    psright (1.0, ypos, buf); ypos -= 2.0 * spacing;
    year = fringe.t200->corr_date.year;
    if (year < 150) year += 1900;
    sprintf (buf, "%d:%03d:%02d%02d%02d", year,
                                fringe.t200->corr_date.day,
                                fringe.t200->corr_date.hour,
                                fringe.t200->corr_date.minute,
                                (int)fringe.t200->corr_date.second);
    psright (1.0, ypos, buf); ypos -= spacing;
                                    // fourfit execution timestamp
    year = fringe.t200->fourfit_date.year;
    if (year < 150) year += 1900;
    sprintf (buf, "%d:%03d:%02d%02d%02d", year,
                                fringe.t200->fourfit_date.day,
                                fringe.t200->fourfit_date.hour,
                                fringe.t200->fourfit_date.minute,
                                (int)fringe.t200->fourfit_date.second);
    psright (1.0, ypos, buf); ypos -= spacing;
                                    // fourfit executable timestamp
    sprintf (which_command, "which %s", pexec);
                                    // use which command to get absolute path
    popfil = popen (which_command, "r");
    fscanf (popfil, "%s", absexec);
    pclose (popfil);
                                    // use stat to get file creation timestamp
    stat (absexec, &xeq_stat);
    utc_pgm = gmtime (&(xeq_stat.st_mtime));
    sprintf (buf, "%d:%03d:%02d%02d%02d", utc_pgm->tm_year+1900, utc_pgm->tm_yday+1,
                     utc_pgm->tm_hour, utc_pgm->tm_min, utc_pgm->tm_sec);
    psright (1.0, ypos, buf); ypos -= 2.0 * spacing;
    sprintf (buf, "%02dh%02dm%7.4fs", fringe.t201->coord.ra_hrs,
                            fringe.t201->coord.ra_mins,
                            fringe.t201->coord.ra_secs);
    psright (1.0, ypos, buf); ypos -= spacing;
    sprintf (buf, "%+02d\312%02d'%6.3f\"", fringe.t201->coord.dec_degs,
                            fringe.t201->coord.dec_mins,
                            fringe.t201->coord.dec_secs);
    psright (1.0, ypos, buf); ypos -= spacing;
                                        /* Labels in dark green for contrast */
    setgreen;
    ypos = 0.945;
    psleft (0.87, ypos, "Fringe quality"); ypos -= 2.0 * spacing;
    psleft (0.87, ypos, "SNR"); ypos -= spacing;
    psleft (0.87, ypos, "Int time"); ypos -= spacing;
    psleft (0.87, ypos, "Amp"); ypos -= spacing;
    psleft (0.87, ypos, "Phase"); ypos -= spacing;
    psleft (0.87, ypos, "PFD"); ypos -= spacing;
    psleft (0.87, ypos, "Delays (us)"); ypos -= spacing;
    psleft (0.87, ypos, "SBD"); ypos -= spacing;
    psleft (0.87, ypos, "MBD"); ypos -= spacing;
    psleft (0.87, ypos, "Fringe rate (Hz)"); ypos -= 2.0 * spacing;
    psleft (0.87, ypos, "Ion TEC "); ypos -= spacing;
    psleft (0.87, ypos, "Ref freq (MHz)"); ypos -= 2.0 * spacing;
    psleft (0.87, ypos, "AP (sec)"); ypos -= 1.5 * spacing;
    psleft (0.87, ypos, "Exp."); ypos -= spacing;
    psleft (0.87, ypos, "Exper #"); ypos -= spacing;
    psleft (0.87, ypos, "Yr:day"); ypos -= spacing;
    psleft (0.87, ypos, "Start"); ypos -= spacing;
    psleft (0.87, ypos, "Stop"); ypos -= spacing;
    psleft (0.87, ypos, "FRT"); ypos -= spacing;
    
    psleft (0.87, ypos, "Corr/FF/build"); 
    ypos -= 4.0 * spacing;
    if (fringe.t201->epoch == 1950) sprintf (buf, "RA & Dec (B1950)");
    else if (fringe.t201->epoch == 2000) sprintf (buf, "RA & Dec (J2000)");
    else sprintf (buf, "RA & Dec (J??? )");
    psleft (0.87, ypos, buf);
    setblack;
                                        /* Channel-by-channel text */
    sprintf (buf, 
        "Amp. and Phase vs. time for each freq., %d segs, %d APs / seg\
 (%.2f sec / seg.), time ticks %d sec", 
            status.nseg, status.apseg, status.apseg * param.acc_period,
            (int)tickinc);
    psleft (0.05, 0.58, buf);   
                                        /* Switch to 110 font for the rest */
    if (nplots > 17) pscat ("/Helvetica findfont 95 scalefont setfont\n");
    else pscat ("/Helvetica findfont 110 scalefont setfont\n");
                                        /* See comment below for 1.03 factor */
    if (nplots > 1)
        labelpos = 0.05 + (nplots-1) * plotwidth * 1.0304;
    else
        labelpos = 0.05 + plotwidth * 1.0304;

    psright (0.050,  0.429, "U");
    psleft  (0.000,  0.422, "Validity");
    psright (0.049,  0.413, "L");
                                        /* Label for tape error plots */
    xpos = labelpos + 0.003;
    if (status.stc_present)
        {
        psright (0.050,  0.399, "U");
        psleft  (0.000,  0.392, "Bias");
        psright (0.049,  0.385, "L");
        psright (0.050,  0.365, "U");
        psleft  (0.000,  0.355, "Level");
        psright (0.049,  0.348, "L");
                                        /* Color coded stations for bias */
        sprintf (buf, "%c", param.baseline[0]);
        setgreen;
        psleft (xpos, 0.39, buf);
        sprintf (buf, "%c", param.baseline[1]);
        setmagenta;
        xpos += 0.015;
        psleft (xpos, 0.39, buf);
        xpos -= 0.015;
                                        /* Color coded stations for level */
        sprintf (buf, "%c", param.baseline[0]);
        setblue;
        psleft (xpos, 0.355, buf);
        sprintf (buf, "%c", param.baseline[1]);
        setred;
        xpos += 0.015;
        psleft (xpos, 0.355, buf);
        }
                                        /* Color coded stations for phasecal */
    xpos += 0.015;
    ypos = (status.stc_present) ? 0.310 : 0.380;
    sprintf (buf, "%c", param.baseline[0]);
    setgreen;
    psleft (xpos, ypos, buf);
    sprintf (buf, "%c", param.baseline[1]);
    setmagenta;
    xpos += 0.015;
    psleft (xpos, ypos, buf);
    setblack;
                                        /* Relocate text if only one plot */
    if (nplots == 2) labelpos = 0.30;
                                        /* use smaller font for bottom of plot rjc 030430 */
    if (nplots < 10)
        pscat ("/Helvetica findfont 95 scalefont setfont\n");
    else if (nplots < 18)
        pscat ("/Helvetica findfont 85 scalefont setfont\n");
    else
        pscat ("/Helvetica findfont 55 scalefont setfont\n");
    xposref = 0.0;
    xposrem = 0.0;
                                        // print manual pc_phases to zero out phase, mbd
    if (msglev < 2)
        {
        fprintf (stderr, "pc_phases ");
        for (i=start_plot; i<start_plot+limit_plot; i++)
            fprintf (stderr, "%c", pass->pass_data[i].freq_code);
        fprintf (stderr, " ");
        }
                                        /* Start main loop over freqs */
    for (i=start_plot; i<start_plot+limit_plot; i++)  // TODO: can change here and i*plotwidth*x
        {
                                        /* 1.03 correction factor to allow for */
                                        /* use of 7570 instead of 7800 in psright */
                                        /* and psleft macros, to support A4 paper */
                                        /* since pgplot coord system is letter */
        xpos = 0.05 + (i-start_plot)*plotwidth * 1.0304;
        ypos = yplace;
        spacing = 0.0095;
        sprintf (buf, "%.2f", pass->pass_data[i].frequency);
        psleft (xpos, ypos, buf);
        if (i == start_plot) psleft (labelpos, ypos, "Freq (MHz)");
        ypos -= spacing;
        sprintf (buf, "%.1f", c_phase (status.fringe[i]) * 180.0 / pi);
        psleft (xpos, ypos, buf);
        if (i == start_plot) psleft (labelpos, ypos, "Phase");
        ypos -= spacing;
        sprintf (buf, "%.1f", c_mag (status.fringe[i]));
        psleft (xpos, ypos, buf);
        if (i == start_plot) psleft (labelpos, ypos, "Ampl.");
        ypos -= spacing;
        sprintf (buf, "%.1f", status.sbdbox[i]);
        psleft (xpos, ypos, buf);
        if (i == start_plot) psleft (labelpos, ypos, "Sbd box");
        ypos -= spacing;
        sprintf (buf, "%d/%d", status.ap_num[0][i], status.ap_num[1][i]);
        psleft (xpos, ypos, buf);
        if (i == start_plot) 
            {
            psleft (labelpos, ypos, "APs used");
            psright (0.04, ypos, "U/L");
            }
        ypos -= spacing;

        if (param.pc_mode[0] != MULTITONE)    // don't print mt freq (at center)
            {
            sprintf (buf, "%d",               // reference pcal frequencies
                (int)pass->pass_data[i].pc_freqs[0][pass->pci[0][i]]/1000);
            psleft (xpos, ypos, buf);
            if (i == start_plot) 
                {
                psleft (labelpos, ypos, "PC freqs");
                sprintf (buf, "%c", param.baseline[0]);
                psright (0.04, ypos, buf);
                }
            ypos -= spacing;
            }

        if (param.pc_mode[1] != MULTITONE)    // don't print mt freq (at center)
            {
            sprintf (buf, "%d",               // remote pcal frequencies
                (int)pass->pass_data[i].pc_freqs[1][pass->pci[1][i]]/1000);
            psleft (xpos, ypos, buf);
            if (i == start_plot) 
                {
                psleft (labelpos, ypos, "PC freqs");
                sprintf (buf, "%c", param.baseline[1]);
                psright (0.04, ypos, buf);
                }
            ypos -= spacing;
            }
                                        // reference mt delays by channel
        if (param.pc_mode[0] == MULTITONE && polars & LREF)
            {
            sprintf (buf, "%.1f", 1e9 * status.pc_delay[i][0][0]);
            psleft (xpos, ypos, buf);
            if (i == start_plot) 
                {
                psleft (labelpos, ypos, "PC L/X/H delays (ns)");
                sprintf (buf, "%c", param.baseline[0]);
                psright (0.04, ypos, buf);
                }
            ypos -= spacing;
            }

        if (param.pc_mode[1] == MULTITONE && polars & LREM) 
            {
            sprintf (buf, "%.1f", 1e9 * status.pc_delay[i][1][0]);
            psleft (xpos, ypos, buf);
            if (i == start_plot) 
                {
                psleft (labelpos, ypos, "PC L/X/H delays (ns)");
                sprintf (buf, "%c", param.baseline[1]);
                psright (0.04, ypos, buf);
                }
            ypos -= spacing;
            }

        if (param.pc_mode[0] == MULTITONE && polars & RREF)
            {
            sprintf (buf, "%.1f", 1e9 * status.pc_delay[i][0][1]);
            psleft (xpos, ypos, buf);
            if (i == start_plot) 
                {
                psleft (labelpos, ypos, "PC R/Y/V delays (ns)");
                sprintf (buf, "%c", param.baseline[0]);
                psright (0.04, ypos, buf);
                }
            ypos -= spacing;
            }

        if (param.pc_mode[1] == MULTITONE && polars & RREM)
            {
            sprintf (buf, "%.1f", 1e9 * status.pc_delay[i][1][1]);
            psleft (xpos, ypos, buf);
            if (i == start_plot) 
                {
                psleft (labelpos, ypos, "PC R/Y/V delays (ns)");
                sprintf (buf, "%c", param.baseline[1]);
                psright (0.04, ypos, buf);
                }
            ypos -= spacing;
            }

        sprintf (buf, "%d:%d", (int)rint((double)fringe.t207->ref_pcphase[i].lsb),
                               (int)rint((double)fringe.t207->rem_pcphase[i].lsb));
        psleft (xpos, ypos, buf);
        if (i == start_plot) 
            {
            psleft (labelpos, ypos, "PC phase");
            sprintf (buf, "%c:%c", param.baseline[0], param.baseline[1]);
            psright (0.04, ypos, buf);
            }
        ypos -= spacing;
        sprintf (buf, "%d:%d", (int)rint((double)fringe.t207->ref_pcoffset[i].lsb),
                               (int)rint((double)fringe.t207->rem_pcoffset[i].lsb));
        psleft (xpos, ypos, buf);
        if (i == start_plot) 
            {
            psleft (labelpos, ypos, "Manl PC");
            sprintf (buf, "%c:%c", param.baseline[0], param.baseline[1]);
            psright (0.04, ypos, buf);
            }
        ypos -= spacing;

                                    // color code pcal amplitudes  rjc 2009.3.10
        if (i == start_plot) 
            {
            psleft (labelpos, ypos, "PC amp");
            sprintf (buf, "%c", param.baseline[0]);
            psright (0.04, ypos, buf);
            }
        pcaref = rint (1000.0 * (double)fringe.t207->ref_pcamp[i].lsb);
        pcarem = rint (1000.0 * (double)fringe.t207->rem_pcamp[i].lsb);
        
        if (pcaref < 4 || pcaref >= 150)
            setred;
        else if (pcaref < 10 || pcaref >= 100)
            setorange;
        else
            setgreen;
        sprintf (buf, "%d", pcaref);
        psleft (xpos, ypos, buf);
        ypos -= spacing;

        if (pcarem < 4 || pcarem >= 150)
            setred;
        else if (pcarem < 10 || pcarem >= 100)
            setorange;
        else
            setgreen;
        sprintf (buf, "%d", pcarem);

        psleft (xpos, ypos, buf);
        setblack;
        if (i == start_plot) 
            {
            sprintf (buf, "%c", param.baseline[1]);
            psright (0.04, ypos, buf);
            }
        ypos -= spacing;
                                        /* Channel ids, reference station */
        if (polstr[0] == 'L')
            sprintf (buf, "%s %s", pass->pass_data[i].ch_usb_lcp[0],
                                   pass->pass_data[i].ch_lsb_lcp[0]);
        else if (polstr[0] == 'R')
            sprintf (buf, "%s %s", pass->pass_data[i].ch_usb_rcp[0],
                                   pass->pass_data[i].ch_lsb_rcp[0]);
        stripbuf (buf);
        psleft (xpos, ypos, buf);
        if (i == start_plot)
            {
            psleft (labelpos, ypos, "Chan ids");
            ypos -= spacing/2.0;
            sprintf (buf, "%c", param.baseline[0]);
            psright (0.04, ypos, buf);
            ypos += spacing/2.0;
            }
        ypos -= spacing;
                                        /* reference station */
        buf[0] = '\0';
                                        /* Empirical value for 110-scaled helvetica */
        digitwidth = 0.0066;            /* 95-scaled helvetica */
                                        /* keep tracks from overwriting previous tracks */
        if (xpos > xposref + digitwidth)
            offset = 0.0;
        else
            offset = xposref - xpos + 3 * digitwidth; 
        first = TRUE;
        for (j=0; j<16; j++)
            {
            if ((pass->pass_data[i].trk_lcp[0][j] > 0) && (polstr[0] == 'L'))
                {
                sprintf (buf, "%d", pass->pass_data[i].trk_lcp[0][j]);
                xposref = xpos + offset;
                if (! first) {psleft (xposref, ypos, ","); offset += 0.5*digitwidth;}
                xposref = xpos + offset;
                psleft (xposref, ypos, buf);
                offset += digitwidth * strlen (buf);
                first = FALSE;
                }
            if ((pass->pass_data[i].trk_rcp[0][j] > 0) && (polstr[0] == 'R'))
                {
                sprintf (buf, "%d", pass->pass_data[i].trk_rcp[0][j]);
                xposref = xpos + offset;
                if (! first) {psleft (xposref, ypos, ","); offset += 0.5*digitwidth;}
                xposref = xpos + offset;
                psleft (xposref, ypos, buf);
                offset += digitwidth * strlen (buf);
                first = FALSE;
                }
            }
        setblack;
        if (i == start_plot+limit_plot-2 && labelpos > xposref)  psleft (labelpos, ypos, "Tracks");
        ypos -= spacing;
                                        /* Channel ids, remote station */
        if (polstr[1] == 'L')
            sprintf (buf, "%s %s", pass->pass_data[i].ch_usb_lcp[1],
                    pass->pass_data[i].ch_lsb_lcp[1]);
        if (polstr[1] == 'R')
            sprintf (buf, "%s %s", pass->pass_data[i].ch_usb_rcp[1],
                    pass->pass_data[i].ch_lsb_rcp[1]);
        stripbuf (buf);
        psleft (xpos, ypos, buf);
        if (i == start_plot) 
            {
            psleft (labelpos, ypos, "Chan ids");
            ypos -= spacing/2.0;
            sprintf (buf, "%c", param.baseline[1]);
            psright (0.04, ypos, buf);
            ypos += spacing/2.0;
            }
        ypos -= spacing;
                                        /* Color-coded tracks, remote station */
        buf[0] = '\0';
                                        /* keep tracks from overwriting previous tracks */
        if (xpos > xposrem + digitwidth)
            offset = 0.0;
        else
            offset = xposrem - xpos + 3 * digitwidth; 
        first = TRUE;
        for (j=0; j<16; j++)
            {
            if ((pass->pass_data[i].trk_lcp[1][j] > 0) && (polstr[1] == 'L'))
                {
                sprintf (buf, "%d", pass->pass_data[i].trk_lcp[1][j]);
                xposrem = xpos + offset;
                if (! first) {psleft (xposrem, ypos, ","); offset += 0.5*digitwidth;}
                xposrem = xpos + offset;
                psleft (xposrem, ypos, buf);
                offset += digitwidth * strlen (buf);
                first = FALSE;
                }
            if ((pass->pass_data[i].trk_rcp[1][j] > 0) && (polstr[1] == 'R'))
                {
                sprintf (buf, "%d", pass->pass_data[i].trk_rcp[1][j]);
                xposrem = xpos + offset;
                if (! first) {psleft (xposrem, ypos, ","); offset += 0.5*digitwidth;}
                xposrem = xpos + offset;
                psleft (xposrem, ypos, buf);
                offset += digitwidth * strlen (buf);
                first = FALSE;
                }
            }
        setblack;
        if (i == start_plot+limit_plot-2 && labelpos > xposrem) psleft (labelpos, ypos, "Tracks");
                                    // helpful printout of manual phase cal adjustments,
                                    // to allow zeroing out phase and mbd residuals for
                                    // combining multiple bands    rjc 2010.1.5
        if (msglev < 2)
            fprintf (stderr, "%6.1f ", fmod((double)fringe.t207->ref_pcoffset[i].lsb 
              - (double)fringe.t207->rem_pcoffset[i].lsb
              + c_phase (status.fringe[i]) * 180.0 / pi
              + 360 * fringe.t208->resid_mbd * 
                       (pass->pass_data[i].frequency 
                      - fringe.t205->ref_freq), 360.0));
	    // fmod returns -360 .. 360 depending on sign of the phase
	    // pcoffset.*lsb is identical to pcoffset.*usb
        /*  fprintf (stderr, "%f %f %f %f %f %f\n", 
                    (double)fringe.t207->ref_pcoffset[i].lsb, 
                    (double)fringe.t207->rem_pcoffset[i].lsb,
                    c_phase (status.fringe[i]) * 180.0 / pi,
                    fringe.t208->resid_mbd, 
                    pass->pass_data[i].frequency, 
                    fringe.t205->ref_freq); */
        }

    if (msglev < 2)
        fprintf (stderr, "\n");
    if (nplots > 2)
        {
        xpos = 1.0;
        ypos = yplace;
        psright (xpos, ypos, "All");
        ypos -= spacing;
        sprintf (buf, "%.1f", status.resid_phase);
        psright (xpos, ypos, buf);
        ypos -= spacing;
        sprintf (buf, "%.1f", fringe.t208->inc_chan_ampl);
        psright (xpos, ypos, buf);
        ypos -= spacing;
        sprintf (buf, "%.1f", status.sbdbox[MAXFREQ]);
        psright (xpos, ypos, buf);
        }
    yplace -= 0.145;
    ypos = yplace;
    pscat ("/Helvetica findfont 95 scalefont setfont\n");
    psleft (0.0, ypos, "Group delay (usec)"); ypos -= 0.01;
    psleft (0.0, ypos, "Sband delay (usec)"); ypos -= 0.01;
    psleft (0.0, ypos, "Phase delay (usec)"); ypos -= 0.01;
    psleft (0.0, ypos, "Delay rate (us/s)"); ypos -= 0.01;
    psleft (0.0, ypos, "Total phase (deg)");
    ypos = yplace;
    sprintf (buf, "%.11E", fringe.t208->tot_mbd);
    psright (0.29, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.11E", fringe.t208->tot_sbd);
    psright (0.29, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.11E", fringe.t208->adelay + status.resid_ph_delay);
    psright (0.29, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.11E", fringe.t208->tot_rate);
    psright (0.29, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.1f", fringe.t208->totphase);
    psright (0.29, ypos, buf);
    ypos = yplace;
    psleft (0.32, ypos, "Apriori delay (usec)"); ypos -= 0.01;
    psleft (0.32, ypos, "Apriori clock (usec)"); ypos -= 0.01;
    psleft (0.32, ypos, "Apriori clockrate (us/s)"); ypos -= 0.01;
    psleft (0.32, ypos, "Apriori rate (us/s)"); ypos -= 0.01;
    psleft (0.32, ypos, "Apriori accel (us/s/s)");
    ypos = yplace;
    sprintf (buf, "%.11E", fringe.t208->adelay);
    psright (0.62, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.7E", fringe.t202->rem_clock - fringe.t202->ref_clock);
    psright (0.62, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.7E", 1.0e6 * (fringe.t202->rem_clockrate - 
                                            fringe.t202->ref_clockrate));
    psright (0.62, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.11E", fringe.t208->arate);
    psright (0.62, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.11E", fringe.t208->aaccel);
    psright (0.62, ypos, buf); ypos -= 0.01;
    ypos = yplace;
    psleft (0.65, ypos, "Resid mbdelay (usec)"); ypos -= 0.01;
    psleft (0.65, ypos, "Resid sbdelay (usec)"); ypos -= 0.01;
    psleft (0.65, ypos, "Resid phdelay (usec)"); ypos -= 0.01;
    psleft (0.65, ypos, "Resid rate (us/s)"); ypos -= 0.01;
    psleft (0.65, ypos, "Resid phase (deg)");
    ypos = yplace;
    sprintf (buf, "%.5E", fringe.t208->resid_mbd);
    psright (0.90, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.5E", fringe.t208->resid_sbd);
    psright (0.90, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.5E", status.resid_ph_delay);
    psright (0.90, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.5E", fringe.t208->resid_rate);
    psright (0.90, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.1f", fringe.t208->resphase);
    psright (0.90, ypos, buf);
    ypos = yplace;
    psleft (0.92, ypos, "+/-"); ypos -= 0.01;
    psleft (0.92, ypos, "+/-"); ypos -= 0.01;
    psleft (0.92, ypos, "+/-"); ypos -= 0.01;
    psleft (0.92, ypos, "+/-"); ypos -= 0.01;
    psleft (0.92, ypos, "+/-");
    ypos = yplace;
    sprintf (buf, "%.1E", fringe.t208->mbd_error);
    psright (1.0, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.1E", fringe.t208->sbd_error);
    psright (1.0, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.1E", status.ph_delay_err);
    psright (1.0, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.1E", fringe.t208->rate_error);
    psright (1.0, ypos, buf); ypos -= 0.01;
    sprintf (buf, "%.1f", status.phase_err);
    psright (1.0, ypos, buf);
    yplace -= 0.050;
    ypos = yplace;
    ypos -= 0.005;
    psleft (0.1, ypos, "RMS");
    psleft (0.15, ypos, "Theor.");
    psleft (0.23, ypos, "Amplitude");
    sprintf (buf, "%.3f +/- %.3f", fringe.t208->amplitude*10000, status.amp_err);
    psleft (0.35, ypos, buf);
    sprintf (buf, "Pcal mode: %s,  %s", pcstr[param.pc_mode[0]], pcstr[param.pc_mode[1]]);
    psleft (0.5, ypos, buf);
    sprintf (buf, "Pcal period (AP's) %d,  %d", param.pc_period[0], param.pc_period[1]);
    psleft (0.75, ypos, buf);
    ypos -= 0.01;
    psleft (0.0, ypos, "ph/seg (deg)");
    sprintf (buf,"%.1f",status.timerms_phase);
    psleft (0.1, ypos, buf);
    sprintf (buf,"%.1f",status.th_timerms_phase);
    psleft (0.15, ypos, buf);
    sprintf (buf, "Search (%dX%d)", status.drsp_size, status.grid_points);
    psleft (0.23, ypos, buf);
    sprintf (buf, "%.3f", status.search_amp);
    psleft (0.35, ypos, buf);
    sprintf (buf, "Pcal rate: %.3E,  %.3E  (us/s)", 
                        fringe.t207->ref_pcrate, fringe.t207->rem_pcrate);
    psleft (0.5, ypos, buf);
    sprintf (buf, "sb window (us)");
    psleft (0.82, ypos, buf);
    sprintf (buf, "%8.3lf %8.3lf", param.win_sb[0], param.win_sb[1]);
    psleft (0.92, ypos, buf);
    ypos -= 0.01;
    psleft (0.0, ypos, "amp/seg (%)");
    sprintf (buf,"%.1f",status.timerms_amp);
    psleft (0.1, ypos, buf);
    sprintf (buf,"%.1f",status.th_timerms_amp);
    psleft (0.15, ypos, buf);
    psleft (0.23, ypos, "Interp.");
    sprintf (buf, "%.3f", status.interp_amp);
    psleft (0.35, ypos, buf);
    sprintf (buf, "Bits/sample: %d", param.bits_sample);
    psleft (0.5, ypos, buf);
    if (param.use_sample_cnts)
        sprintf (buf, "SampCntNorm: enabled ");
    else
        sprintf (buf, "SampCntNorm: disabled");
    psleft (0.65, ypos, buf);
    sprintf (buf, "mb window (us)");
    psleft (0.82, ypos, buf);
    sprintf (buf, "%8.3lf %8.3lf", param.win_mb[0], param.win_mb[1]);
    psleft (0.92, ypos, buf);
    ypos -= 0.01;
    psleft (0.0, ypos, "ph/frq (deg)");
    sprintf (buf,"%.1f",status.freqrms_phase);
    psleft (0.1, ypos, buf);
    sprintf (buf,"%.1f",status.th_freqrms_phase);
    psleft (0.15, ypos, buf);
    psleft (0.23, ypos, "Inc. seg. avg.");
    sprintf (buf, "%.3f", fringe.t208->inc_seg_ampl);
    psleft (0.35, ypos, buf);
    
    srate = 1e-6/param.samp_period + 0.5;
    sprintf (buf, "Sample rate(MSamp/s): %d", srate);
    psleft (0.5, ypos, buf);
    sprintf (buf, "dr window (ns/s)");
    psleft (0.82, ypos, buf);
    sprintf (buf, "%8.3lf %8.3lf", 1e3 * param.win_dr[0], 1e3 * param.win_dr[1]);
    psleft (0.92, ypos, buf);
    ypos -= 0.01;
    psleft (0.0, ypos, "amp/frq (%)");
    sprintf (buf,"%.1f",status.freqrms_amp);
    psleft (0.1, ypos, buf);
    sprintf (buf,"%.1f",status.th_freqrms_amp);
    psleft (0.15, ypos, buf);
    psleft (0.23, ypos, "Inc. frq. avg.");
    sprintf (buf, "%.3f", fringe.t208->inc_chan_ampl);
    psleft (0.35, ypos, buf);
                                    // effective number of polarizations is max of 2
    eff_npols = (pass->npols > 2) ? 2 : pass->npols;
    sprintf (buf, "Data rate(Mb/s): %d", numsb * eff_npols * srate * param.bits_sample);
    psleft (0.5, ypos, buf);
    sprintf (buf, "nlags: %d", param.nlags);
    psleft (0.65, ypos, buf);
                                    // print coherence time if non-infinite
    if (pass->control.t_cohere > 0.0)
        sprintf (buf,"t_cohere (s) %.1f", pass->control.t_cohere);
    else
        sprintf (buf, "t_cohere infinite");
    psleft (0.72, ypos, buf);
                                    // ionosphere window limits
    sprintf (buf, "ion window (TEC)");
    psleft (0.82, ypos, buf);
    sprintf (buf, "%8.2f %8.2f", param.win_ion[0], param.win_ion[1]);
    psleft (0.92, ypos, buf);
    ypos -= 0.015;
                                    // az, el, par. angle
    sprintf (buf, "%c: az %.1f  el %.1f  pa %.1f", param.baseline[0],
            fringe.t202->ref_az, fringe.t202->ref_elev, 
            param.par_angle[0] * 180 / M_PI);
    psleft (0.00, ypos, buf);
    sprintf (buf, "%c: az %.1f  el %.1f  pa %.1f", param.baseline[1], 
            fringe.t202->rem_az, fringe.t202->rem_elev,
            param.par_angle[1] * 180 / M_PI);
    psleft (0.20, ypos, buf);
                                    // u and v
    sprintf (buf, "u,v (fr/asec) %.3f  %.3f", fringe.t202->u, fringe.t202->v);
    psleft (0.40, ypos, buf);
                                    // print interpolation method
    if (param.interpol == ITERATE)
        sprintf (buf, "iterative interpolator");
    else if (param.interpol == SIMUL)
        sprintf (buf, "simultaneous interpolator");
    psleft (0.90, ypos, buf);
    ypos -= 0.015;

    if (test_mode) strcpy (output_filename, "Suppressed by test mode");
    else strcpy (output_filename, fringename);
                                        /* recreate input filename from output */
    strcpy (input_filename, fringename);
    i = strlen (input_filename);
    while (input_filename[i] != '/')
        i--;
                                    // construct type 1 name from type 2 name
    strcpy (buf, input_filename+strlen(input_filename)-7);
    strcpy (input_filename+i+4, buf);
    sprintf (buf, "Control file: %s    Input file: %s    Output file: %s", 
             control_filename, input_filename, output_filename);
    psleft (0.00, ypos, buf);
                                        /* Re-attach trailing part of file */
    strcat (pplot, trailer);
    (*t221)->ps_length = strlen (pplot);

    return (0);
    }
