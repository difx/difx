/************************************************************************/
/*                                                                      */
/* Generates the textual part of a fringe plot                          */
/*                                                                      */
/* Created make_postplot original                   October 1999 by CJL */
/* Refactored into a separate routine                     2014.7.30 rjc */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <math.h>
#include <string.h>
#include <complex.h>
#include "param_struct.h"
#include "pass_struct.h"
#include "meta_struct.h"
#include "ovex.h"

#define pi 3.141592654
                                        // polarization bit masks
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
                snprintf (psbuf, sizeof(psbuf),\
                    "%d %d M (%s) SL\n", xval, yval, ps_string);\
                pscat (psbuf);}
#define psright(xcoord, ycoord, ps_string)\
             {xval = xcoord * 7570; yval = ycoord * 10500;\
                snprintf (psbuf, sizeof(psbuf),\
                    "%d %d M (%s) SR\n", xval, yval, ps_string);\
                pscat (psbuf);}
                                        /* Color shorthand */
#define setred pscat ("1.0 0.0 0.0 setrgbcolor\n")
#define setblue pscat ("0.0 0.0 1.0 setrgbcolor\n")
#define setcyan pscat ("0.0 1.0 1.0 setrgbcolor\n")
#define setorange pscat ("1.0 0.5 0.0 setrgbcolor\n")
#define setgreen pscat ("0.0 0.6 0.0 setrgbcolor\n")
#define setmagenta pscat ("1.0 0.0 1.0 setrgbcolor\n")
#define setblack pscat ("0.0 0.0 0.0 setrgbcolor\n")



void generate_text (struct scan_struct *root,
                    struct type_pass *pass,
                    char *fringename,
                    char *pplot,
                    double tickinc)
    {
    extern struct mk4_fringe fringe;
    extern struct type_param param;
    extern struct type_status status;
    extern struct type_meta meta;
    extern int test_mode;
    extern int msglev;
    extern char control_filename[];
    extern char *pexec;
    extern char version_no[], progname[];
    extern char *sprint_char_arr();
    struct stat xeq_stat;
    int i, j, n, k;
    int start_plot, limit_plot;
    char *rootname;
    char buf[2560], psbuf[2560], output_filename[256];
    char input_filename[256], polstr[13], polstrx[13];
    struct tm *gmtime(), *utc_pgm;
    float sec1, sec2, plotwidth;
    float xpos, xposref,xposrem, ypos, spacing, offset, labelpos, yplace;
    float digitwidth;
    int hr1, hr2, min1, min2, xval, yval, nplots, year;
    int first;
    int numsb, srate;
    int pcaref, pcarem;
    int eff_npols, polars, polmsk;
    char polchar[2][2] = {'L', 'R', 'X', 'Y'};
    FILE *popfil;
    char absexec[256], which_command[256];
    char buffer[32];
    static char *pcstr[5]={"","NORMAL","AP BY AP","MANUAL", "MULTITONE"};
    double delta_delay;
    void stripbuf (char *),
         modify_pol (struct type_pass *, char *);
    

                                        // precalculate general quantities
    numsb = 0;
    for (i=0; i<pass->nfreq; i++)
        {
        if (status.ap_num[0][i])        // tally # active sidebands
            numsb++;
        if (status.ap_num[1][i])
            numsb++;
        }
                                        // effective number of polarizations is max of 2
    eff_npols = (pass->npols > 2) ? 2 : pass->npols;
    srate = 1e-6 / param.samp_period + 0.5;

                                        // precalculate a few plot-related quantities
    start_plot = (param.first_plot == FALSE) ? 0 : param.first_plot;
    limit_plot = (param.nplot_chans == FALSE) ? pass->nfreq : param.nplot_chans;

    nplots = (limit_plot == 1) ? 1 : limit_plot+1;
   
    plotwidth = 0.88 / (double)nplots;
    if (nplots == 1) 
        plotwidth = 0.8;

    start_plot = (param.first_plot == FALSE) ? 0 : param.first_plot;
    limit_plot = (param.nplot_chans == FALSE) ? pass->nfreq : param.nplot_chans;

    yplace = (status.stc_present) ? 0.275 : 0.345;

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
    strncpy(meta.corrvers, buffer, sizeof(meta.corrvers));
                                        /* Identification information */
    if ((rootname = strrchr (root->filename, '/')) == NULL)
        rootname = root->filename;
    else rootname++;
    param.baseline[2] = 0;
    sprintf (buf, "%s, %s, %s", rootname, root->scan_name, param.baseline);
    psright (1.0, 0.98, buf);
    strncpy(meta.rt_sn_bl, buf, sizeof(meta.rt_sn_bl));
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
        else
            {
            sprintf (polstr, "nopol");
            }
        }
    else if (param.pol < 16)
        {
        polstrx[0] = '\0';
        if (param.pol & POLMASK_LL && pass->pprods_present[0])
            {
            strcat (polstrx, "+LL");
            polars |= LREF | LREM;
            }
        if (param.pol & POLMASK_RR && pass->pprods_present[1])
            {
            strcat (polstrx, "+RR");
            polars |= RREF | RREM;
            }
        if (param.pol & POLMASK_LR && pass->pprods_present[2])
            {
            strcat (polstrx, "+LR");
            polars |= LREF | RREM;
            }
        if (param.pol & POLMASK_RL && pass->pprods_present[3])
            {
            strcat (polstrx, "+RL");
            polars |= RREF | LREM;
            }
        if (polstrx[0] == '+')
            strcpy (polstr, polstrx+1);      // chop off leading '+'
        else
            sprintf(polstr, "NOPOL");
        }
    else
        {
        sprintf (polstr, "Ixy");
        polars |= LREF | LREM | RREF | RREM;
        }
                                    // substitute linear pol symbols if apropos
    modify_pol (pass, polstr);
    sprintf (buf, "%.8s - %.8s, fgroup %c, pol %s", fringe.t202->ref_name,
                    fringe.t202->rem_name, pass->pass_data[0].fgroup, polstr);
    strncpy(meta.polstr, polstr, sizeof(meta.polstr));
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
    sprintf (buf, "%7.3f", param.ion_diff);
    psright (1.0, ypos, buf); ypos -= spacing;
    ypos -= spacing;
    sprintf (buf, "%.4f", fringe.t205->ref_freq);
    psright (1.0, ypos, buf); ypos -= spacing;
    sprintf (buf, "%.3f", param.acc_period);
    psright (1.0, ypos, buf); ypos -= 1.5 * spacing;
                                        /* Global scan information */
    sprintf (buf, "%.8s", root->exper_name);
    psright (1.0, ypos, buf); ypos -= spacing;
    strncpy(meta.exper_name, root->exper_name, sizeof(meta.exper_name));
    sprintf (buf, "%d", root->exper_num);
    psright (1.0, ypos, buf); ypos -= spacing;
    meta.exper_num = root->exper_num;
    year = fringe.t200->scantime.year;
    if (year < 150) year += 1900;
    sprintf (buf, "%d:%03d", year, fringe.t200->scantime.day);
    psright (1.0, ypos, buf); ypos -= spacing;
    strncpy(meta.yr_doy, buf, sizeof(meta.yr_doy));
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
    strncpy(meta.pass_start, buf, sizeof(meta.pass_start));
    sprintf (buf, "%02d%02d%05.2f", hr2, min2, sec2);
    psright (1.0, ypos, buf); ypos -= spacing;
    strncpy(meta.pass_stop, buf, sizeof(meta.pass_stop));
    sprintf (buf, "%02d%02d%05.2f", fringe.t200->frt.hour,
        fringe.t200->frt.minute, fringe.t200->frt.second);
    psright (1.0, ypos, buf); ypos -= 2.0 * spacing;
    strncpy(meta.pass_frt, buf, sizeof(meta.pass_frt));
    year = fringe.t200->corr_date.year;
    if (year < 150) year += 1900;
    sprintf (buf, "%d:%03d:%02d%02d%02d", year,
                                fringe.t200->corr_date.day,
                                fringe.t200->corr_date.hour,
                                fringe.t200->corr_date.minute,
                                (int)fringe.t200->corr_date.second);
    psright (1.0, ypos, buf); ypos -= spacing;
    strncpy(meta.corr_time, buf, sizeof(meta.corr_time));
                                    // fourfit execution timestamp
    year = fringe.t200->fourfit_date.year;
    if (year < 150) year += 1900;
    sprintf (buf, "%d:%03d:%02d%02d%02d", year,
                                fringe.t200->fourfit_date.day,
                                fringe.t200->fourfit_date.hour,
                                fringe.t200->fourfit_date.minute,
                                (int)fringe.t200->fourfit_date.second);
    psright (1.0, ypos, buf); ypos -= spacing;
    strncpy(meta.ff_time, buf, sizeof(meta.ff_time));
                                    // fourfit executable timestamp
    sprintf (which_command, "which %s", pexec);
                                    // use which command to get absolute path
    popfil = popen (which_command, "r");
    fscanf (popfil, "%s", absexec);
    pclose (popfil);
    strncpy(meta.fourfitcmd, absexec, sizeof(meta.fourfitcmd));
                                    // use stat to get file creation timestamp
    stat (absexec, &xeq_stat);
    utc_pgm = gmtime (&(xeq_stat.st_mtime));
    sprintf (buf, "%d:%03d:%02d%02d%02d", utc_pgm->tm_year+1900, utc_pgm->tm_yday+1,
                     utc_pgm->tm_hour, utc_pgm->tm_min, utc_pgm->tm_sec);
    psright (1.0, ypos, buf); ypos -= 2.0 * spacing;
    strncpy(meta.build_time, buf, sizeof(meta.build_time));
    sprintf (buf, "%02dh%02dm%09.6fs", fringe.t201->coord.ra_hrs,
                            fringe.t201->coord.ra_mins,
                            fringe.t201->coord.ra_secs);
    psright (1.0, ypos, buf); ypos -= spacing;
    strncpy(meta.ra, buf, sizeof(meta.ra));
    // %+02d doesn't do what one might wish
    sprintf (buf, "%c%02d\312%02d'%09.6f\"",
        fringe.t201->coord.dec_degs >= 0 ? '+' : '-',
                            fringe.t201->coord.dec_degs,
                            fringe.t201->coord.dec_mins,
                            fringe.t201->coord.dec_secs);
    strncpy(meta.dec, buf, sizeof(meta.dec)); meta.dec[3] = 'o';
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
    if (fringe.t201->epoch == 1950) 
        sprintf (buf, "RA & Dec (B1950)");
    else if (fringe.t201->epoch == 2000) 
        sprintf (buf, "RA & Dec (J2000)");
    else 
        sprintf (buf, "RA & Dec (J??? )");
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
    if (nplots == 2) 
        labelpos = 0.30;
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
        {                               // precalculate diff. delay
        delta_delay = (param.mbd_anchor == MODEL) ?
                      fringe.t208->resid_mbd :
                      fringe.t208->resid_mbd - fringe.t208->resid_sbd;
        fprintf (stderr, "delta_delay %g\n", delta_delay);
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
        sprintf (buf, "%.1f", carg (status.fringe[i]) * 180.0 / pi);
        psleft (xpos, ypos, buf);
        if (i == start_plot) psleft (labelpos, ypos, "Phase");
        ypos -= spacing;
        sprintf (buf, "%.1f", cabs (status.fringe[i]));
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
                                        // print out mt delays for channel
        polmsk = 1;
        for (n=0;n<2;n++)               // stn loop over ref & rem
            if (param.pc_mode[n] == MULTITONE)
                for (k=0;k<2;k++)       // pol loop over L/X & R&Y
                    {
                    if (polars & polmsk)
                        {
                        sprintf (buf, "%.1f", 1e9 * status.pc_delay[i][n][k]);
                        psleft (xpos, ypos, buf);
                        if (i == start_plot) 
                            {
                            sprintf (buf, "PC %c delays (ns)", polchar[pass->linpol[n]][k]);
                            psleft (labelpos, ypos, buf);
                            sprintf (buf, "%c", param.baseline[n]);
                            psright (0.04, ypos, buf);
                            }
                        ypos -= spacing;
                        }
                    polmsk <<= 1;
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

        /* prevent junk chan/track ids from being written out to 
	the postscript file in the case of a combined polarization fit */
        if(polstr[0] != 'I') 
        {
                                        /* Channel ids, reference station */
        if (polstr[0] == 'L' || polstr[0] == 'X')
            sprintf (buf, "%s %s", pass->pass_data[i].ch_usb_lcp[0],
                                   pass->pass_data[i].ch_lsb_lcp[0]);
        else if (polstr[0] == 'R' || polstr[0] == 'Y')
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
            if ((pass->pass_data[i].trk_lcp[0][j] > 0) 
             && ((polstr[0] == 'L') || (polstr[0] == 'X')))
                {
                sprintf (buf, "%d", pass->pass_data[i].trk_lcp[0][j]);
                xposref = xpos + offset;
                if (! first) {psleft (xposref, ypos, ","); offset += 0.5*digitwidth;}
                xposref = xpos + offset;
                psleft (xposref, ypos, buf);
                offset += digitwidth * strlen (buf);
                first = FALSE;
                }
            if ((pass->pass_data[i].trk_rcp[0][j] > 0) 
             && ((polstr[0] == 'R') || (polstr[0] == 'Y')))
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
        if (polstr[1] == 'L' || polstr[1] == 'X')
            sprintf (buf, "%s %s", pass->pass_data[i].ch_usb_lcp[1],
                    pass->pass_data[i].ch_lsb_lcp[1]);
        else if (polstr[1] == 'R' || polstr[1] == 'Y')
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
            if ((pass->pass_data[i].trk_lcp[1][j] > 0) 
             && ((polstr[1] == 'L') || (polstr[1] == 'X')))
                {
                sprintf (buf, "%d", pass->pass_data[i].trk_lcp[1][j]);
                xposrem = xpos + offset;
                if (! first) {psleft (xposrem, ypos, ","); offset += 0.5*digitwidth;}
                xposrem = xpos + offset;
                psleft (xposrem, ypos, buf);
                offset += digitwidth * strlen (buf);
                first = FALSE;
                }
            if ((pass->pass_data[i].trk_rcp[1][j] > 0) 
             && ((polstr[1] == 'R') || (polstr[1] == 'Y')))
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
	}  /* end of if(polstr[0] != 'I') */ 
        if (msglev < 2)
            {
            fprintf (stderr, "%6.1f ", fmod(carg (status.fringe[i]) * 180.0 / pi
              + 360 * delta_delay * (pass->pass_data[i].frequency 
                      - fringe.t205->ref_freq), 360.0));
            }
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
    if (param.mbd_anchor == MODEL)
        {
        psleft (0.0, ypos, "Group delay (usec)(model)"); 
        }
    else
        {
        psleft (0.0, ypos, "Group delay (usec)(sbd)"); 
        }
    ypos -= 0.01;
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
    psleft (0.93, ypos, "+/-"); ypos -= 0.01;
    psleft (0.93, ypos, "+/-"); ypos -= 0.01;
    psleft (0.93, ypos, "+/-"); ypos -= 0.01;
    psleft (0.93, ypos, "+/-"); ypos -= 0.01;
    psleft (0.93, ypos, "+/-");
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
    sprintf (buf, "PC period (AP's) %d,  %d", param.pc_period[0], param.pc_period[1]);
    psleft (0.73, ypos, buf);
    if (status.nion)
        {
        psleft (0.89, ypos, "dTEC");
        psleft (0.93, ypos, "+/-");
        sprintf (buf, "%6.3f", status.ion_sigmas[2]);
        psright (1.0, ypos, buf);
        }
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
    sprintf (buf, "Bits/sample: %dx%d", param.bits_sample[0], param.bits_sample[1]);
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
    sprintf (buf, "Data rate(Mb/s): %d", (int)(numsb * eff_npols * srate 
            * sqrt ((double)(param.bits_sample[0] * param.bits_sample[1])) + 0.5));
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
    ypos -= 0.012;
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
    ypos -= 0.012;

    if (test_mode) strcpy (output_filename, "Suppressed by test mode");
    else strcpy (output_filename, fringename);
                                        /* make input filename from output */
    strcpy (input_filename, fringename);
    i = strlen (input_filename);
    while ((input_filename[i] != '/') && (i > 0))
        i--;
                                    // construct type 1 name from type 2 name
    strcpy (buf, input_filename+strlen(input_filename)-7);
    strcpy (input_filename+i+4, buf);
    sprintf (buf, "Control file: %s    Input file: %s    Output file: %s", 
             control_filename, input_filename, output_filename);
    psleft (0.00, ypos, buf);
    ypos -= 0.01;
                                    // samplers line, possibly
    if (pass->control.nsamplers)
        {
        sprintf (buf, "Samplers: ");
        for (i=0; i<pass->control.nsamplers; i++)
            {
            sprintf (buffer, "%s ", pass->control.psamplers[i]);
            strcat (buf, buffer);
            }
        psleft (0.00, ypos, buf);
        }
    }



                                        /* Simple little routine to convert dumb */
                                        /* space-delimited list into minimal */
                                        /* comma-separated one */
void stripbuf (char *list)
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

void modify_pol (struct type_pass *p, char *polstr)
    {
    int i;

    for (i=0; i<strlen(polstr); i+=3)
        {
        if (p->linpol[0])           // reference linear pol?
            {
            if (polstr[i] == 'L')
                polstr[i] = 'X';
            else if (polstr[i] == 'R')
                polstr[i] = 'Y';
            }

        if (p->linpol[1])           // remote linear pol?
            {
            if (polstr[i+1] == 'L')
                polstr[i+1] = 'X';
            else if (polstr[i+1] == 'R')
                polstr[i+1] = 'Y';
            }
        }
    }
