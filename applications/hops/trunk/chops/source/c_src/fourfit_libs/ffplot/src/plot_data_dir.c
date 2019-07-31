/*
 * Machinery to support dumping plot data to one or more files
 *
 * The main routine manufactures a suitable filename and hands
 * off the details of populating the file to other functions.
 */
#include <assert.h>
#include <errno.h>
#include <libgen.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#define PLOT_DATA_DIR_IMPLEMENTATION 1
#include "plot_data_dir.h"


/* implementation details, not needed by callers */
#define PDDCOLS 132
/* double expansion required for some reason */
#define PDD_DESCRIBE(F,X)   fprintf(F,"#  %-21.20s 0x%08X\n", #X, X)
#define PDD_DESCRIBe(F,X,C) fprintf(F,"#  %-21.20s 0x%08X  (%s)\n", #X, X, C)
/* components of HOPS_PLOT_DATA_MASK */
#define PDD_HEADER          0x00000001
#define PDD_SBD_AMP         0x00000002
#define PDD_MBD_AMP         0x00000004
#define PDD_DLYRATE         0x00000008
#define PDD_XPSPEC          0x00000010
#define PDD_PHASOR          0x00000020
#define PDD_WEIGHTS         0x00000040
#define PDD_MEAN_AP         0x00000080
#define PDD_SEG_AMP         0x00000100
#define PDD_SEG_PHS         0x00000200
#define PDD_SEG_FRAC_USB    0x00000400
#define PDD_SEG_FRAC_LSB    0x00000800
#define PDD_SEG_REFSCNT_USB 0x00001000
#define PDD_SEG_REFSCNT_LSB 0x00002000
#define PDD_SEG_REMSCNT_USB 0x00004000
#define PDD_SEG_REMSCNT_LSB 0x00008000
#define PDD_SEG_REFBIAS_USB 0x00010000
#define PDD_SEG_REFBIAS_LSB 0x00020000
#define PDD_SEG_REMBIAS_USB 0x00040000
#define PDD_SEG_REMBIAS_LSB 0x00080000
#define PDD_SEG_REFPCAL     0x00100000
#define PDD_SEG_REMPCAL     0x00200000
#define PDD_SEG_PLOT_INFO   0x00400000
#define PDD_SEG_RESERVED1   0x00800000
#define PDD_SEG_AMP_FILTER  0x01000000
#define PDD_MODELINFO       0x02000000
#define PDD_FINEPRINT       0x04000000
#define PDD_SEG_RESERVED2   0x08000000
#define PDD_SEG_RESERVED3   0x10000000
#define PDD_SEG_RESERVED4   0x20000000
#define PDD_SEG_RESERVED5   0x40000000
#define PDD_LEGEND          0x80000000
#define PDD_ALL             0x83FFFFFF
#define PDD_SOME \
    (PDD_HEADER|PDD_SBD_AMP|PDD_MBD_AMP|PDD_DLYRATE|PDD_XPSPEC|PDD_PHASOR \
    |PDD_WEIGHTS|PDD_MEAN_AP|PDD_SEG_AMP|PDD_SEG_PHS|PDD_LEGEND)


#define R2D(RAD) ((RAD) * (M_1_PI * 180.0))
extern void msg (char *, int, ...);
extern char control_filename[];

/*
 * This dumps all the important numbers and some others.
 */
static void dump_header(FILE *fp, char *outname, struct type_dump *dp)
{
    time_t now = time(0);
    int ii;
    char *epoch;
    msg("Dumping plot data to %s", 1, outname);
    fprintf(fp, "#\n");
    fprintf(fp, "# GENERAL\n");
    fprintf(fp, "# Dumpfile         '%s'\n", outname);
    fprintf(fp, "# DumpCreated      '%24.24s UTC'\n", asctime(gmtime(&now)));
    /* miscellaneous numbers from meta */
    fprintf(fp, "# CorrVers         '%s'\n", dp->meta->corrvers);
    fprintf(fp, "# RootScanBaseline '%s'\n", dp->meta->rt_sn_bl);
    fprintf(fp, "# PolStr           '%s'\n", dp->meta->polstr);
    fprintf(fp, "# SNR              %.4f\n", dp->fringe->t208->snr);
    fprintf(fp, "# IntgTime         %.4f\n", dp->fringe->t206->intg_time);
    fprintf(fp, "# Amp              %.4f\n",
        dp->fringe->t208->amplitude * 10000.0);
    fprintf(fp, "# ResPhase         %.4f\n",
        dp->fringe->t208->resphase);
    fprintf(fp, "# Quality          '%c'\n", dp->fringe->t208->quality);
    fprintf(fp, "# Errcode          '%c'\n", dp->fringe->t208->errcode);
    fprintf(fp, "# PFD              %.4E\n", dp->fringe->t208->prob_false);
    fprintf(fp, "# ResidSbd(us)     %.8f\n", dp->fringe->t208->resid_sbd);
    fprintf(fp, "# ResidMbd(us)     %.8f\n", dp->fringe->t208->resid_mbd);
    fprintf(fp, "# FringeRate(Hz)   %.8f\n",
        dp->status->dr_max_global * dp->param->ref_freq);
    fprintf(fp, "# DelayRate(ps/s)  %.8f\n",
        dp->status->dr_max_global * 1000000.0);
    fprintf(fp, "# IonTEC(TEC)      %.4f\n", dp->param->ion_diff);
    fprintf(fp, "# RefFreq(MHz)     %.6f\n", dp->fringe->t205->ref_freq);
    fprintf(fp, "# AP(sec)          %.6f\n", dp->param->acc_period);
    fprintf(fp, "# ExperName        %s\n", dp->meta->exper_name);
    fprintf(fp, "# ExperNum         %d\n", dp->meta->exper_num);
    fprintf(fp, "# YearDOY          %s\n", dp->meta->yr_doy);
    fprintf(fp, "# Start            %s\n", dp->meta->pass_start);
    fprintf(fp, "# Stop             %s\n", dp->meta->pass_stop);
    fprintf(fp, "# FRT              %s\n", dp->meta->pass_frt);
    fprintf(fp, "# CorrTime         %s\n", dp->meta->corr_time);
    fprintf(fp, "# FFTime           %s\n", dp->meta->ff_time);
    fprintf(fp, "# BuildTime        %s\n", dp->meta->build_time);
    fprintf(fp, "# FourfitCmd       %s\n", dp->meta->fourfitcmd);
    switch (dp->fringe->t201->epoch) {
    case 2000:  epoch = "(J2000)"; break;
    case 1950:  epoch = "(B1950)"; break;
    default:    epoch = "(epoch)"; break;
    }
    fprintf(fp, "# RA               %s\n", dp->meta->ra);
    fprintf(fp, "# Dec              %s\n", dp->meta->dec);
    fprintf(fp, "# Epoch            %s\n", epoch);
    fprintf(fp, "#\n");
    fprintf(fp, "# NumAP            %d\n", dp->plot->num_ap);
    fprintf(fp, "# NumFreq          %d\n", dp->plot->num_freq);
    fprintf(fp, "# DRSize           %d\n", dp->plot->dr_size);
    fprintf(fp, "# RSizeMax         %d\n", dp->plot->dr_size_max);
    fprintf(fp, "# NumMBPts         %d\n", dp->plot->num_mb_pts);
    fprintf(fp, "# APOff            %d\n", dp->pass->ap_off);
    fprintf(fp, "# APSeg            %d\n", dp->status->apseg);
    fprintf(fp, "# NSeg             %d\n", dp->status->nseg);
    fprintf(fp, "# AmpCorrFact      %f\n", dp->status->amp_corr_fact);
    fprintf(fp, "#\n");
    fprintf(fp, "# StartPlot        %d\n", dp->meta->start_plot);
    fprintf(fp, "# NPlots           %d\n", dp->meta->nplots);
    fprintf(fp, "# ChannelsBefore   '");
    for (ii = 0; ii < dp->meta->start_plot; ii++)
        fprintf(fp, " %c", dp->pass->pass_data[ii].freq_code);
    fprintf(fp, "'\n");
    fprintf(fp, "# ChannelsPlotted  '");
    for ( ; ii < dp->meta->start_plot + dp->meta->nplots; ii++)
        fprintf(fp, " %c", dp->pass->pass_data[ii].freq_code);
    fprintf(fp, "  All'\n");
    fprintf(fp, "# ChannelsAfter    '");
    for ( ; ii < dp->pass->nfreq; ii++)
        fprintf(fp, " %c", dp->pass->pass_data[ii].freq_code);
    fprintf(fp, "'\n");
    fprintf(fp, "#\n");
}

/*
 * This dumps the per-channel plotting legends and data that are hard to read.
 * This is pivoted from the plot to allow a fixed number of colums.
 * FIXME: not correct for more than 1 pol or ...
 * FIXME: incomplete..
 */
static void dump_plot_info(FILE *fp, struct type_dump *dp)
{
    static char polchar[2][2] = {'L', 'R', 'X', 'Y'};
    int ii;
    fprintf(fp, "# PLOT_INFO %d channels\n", dp->meta->nplots);
    fprintf(fp,
        "#Ch Freq(MHz)   Phase    Ampl SbdBox APsRf APsRm"
        " PCdlyRf PCdlyRm PCPhsRf PCPhsRm PCOffRf PCOffRm PCAmpRf PCAmpRm"
        " ChIdRf TrkRf ChIdRm TrkRm\n");
    for (ii = dp->meta->start_plot;
         ii < dp->meta->start_plot + dp->meta->nplots - 1; ii++) {
        fprintf(fp,
            "  %c %9.4f %+7.2f %7.3f %6.2f %5d %5d %+7.1f %+7.1f"
            " %+7.1f %+7.1f %+7.1f %+7.1f %+7.1f %+7.1f"
            " %6s %5d %6s %5d\n",
            /* line one */
            dp->pass->pass_data[ii].freq_code,
            dp->pass->pass_data[ii].frequency,
            carg (dp->status->fringe[ii]) * 180.0 / M_PI,
            cabs (dp->status->fringe[ii]),
            dp->status->sbdbox[ii],
            dp->status->ap_num[0][ii], dp->status->ap_num[1][ii],
            /* line two */
            dp->status->pc_delay[ii][0][0],
            dp->status->pc_delay[ii][1][0],
            rint((double)dp->fringe->t207->ref_pcphase[ii].lsb),
            rint((double)dp->fringe->t207->rem_pcphase[ii].lsb),
            rint((double)dp->fringe->t207->ref_pcoffset[ii].lsb),
            rint((double)dp->fringe->t207->rem_pcoffset[ii].lsb),
            rint (1000.0 * (double)dp->fringe->t207->ref_pcamp[ii].lsb),
            rint (1000.0 * (double)dp->fringe->t207->rem_pcamp[ii].lsb),
            /* line three */
            "ch",0,
            "ch",0
        );
    }
    /* and the All datum */
    fprintf(fp,
        "%s %+7.2f %7.3f %6.2f\n", "All          ",
        dp->status->resid_phase,
        dp->fringe->t208->inc_chan_ampl,
        dp->status->sbdbox[MAXFREQ]);
    fprintf(fp, "#\n");
}
/*
 * This dumps the model info just before the bottom.
 */
static void dump_modelinfo(FILE *fp, struct type_dump *dp)
{
    fprintf(fp, "# MODELINFO\n");
    fprintf(fp, "# GroupDelay%s   %+.11E\n",
        dp->param->mbd_anchor == MODEL ? "Model(usec)" : "SBD(usec)  ",
        dp->fringe->t208->tot_mbd);
    fprintf(fp, "# SbandDelay(usec)        %+.11E\n",
        dp->fringe->t208->tot_sbd);
    fprintf(fp, "# PhaseDelay(usec)        %+.11E\n",
        dp->fringe->t208->adelay + dp->status->resid_ph_delay);
    fprintf(fp, "# TotalPhase(deg)         %+.11E\n",
        dp->fringe->t208->totphase);
    fprintf(fp, "# AprioriDelay(usec)      %+.11E\n",
        dp->fringe->t208->adelay);
    fprintf(fp, "# AprioriClock(usec)      %+.7E\n",
        dp->fringe->t202->rem_clock - dp->fringe->t202->ref_clock);
    fprintf(fp, "# AprioriClockrate(us/s)  %+.7E\n", 1.0e6 *
        (dp->fringe->t202->rem_clockrate - dp->fringe->t202->ref_clockrate));
    fprintf(fp, "# AprioriRate(us/s)       %+.11E\n",
        dp->fringe->t208->arate);
    fprintf(fp, "# AprioriAccel(us/s/s)    %+.11E\n",
        dp->fringe->t208->aaccel);
    fprintf(fp, "# ResidMbdelay(usec)      %+.5E\n",
        dp->fringe->t208->resid_mbd);
    fprintf(fp, "# ResidSbdelay(usec)      %+.5E\n",
        dp->fringe->t208->resid_sbd);
    fprintf(fp, "# ResidPhdelay(usec)      %+.5E\n",
        dp->status->resid_ph_delay);
    fprintf(fp, "# ResidRate(us/s)         %+.5E\n",
        dp->fringe->t208->resid_rate);
    fprintf(fp, "# ResidPhase(deg)         %+.5E\n",
        dp->fringe->t208->resphase);
    fprintf(fp, "# ResidMbdelayError(usec) %+.5E\n",
        dp->fringe->t208->mbd_error);
    fprintf(fp, "# ResidSbdelayError(usec) %+.5E\n",
        dp->fringe->t208->sbd_error);
    fprintf(fp, "# ResidPhdelayError(usec) %+.5E\n",
        dp->status->ph_delay_err);
    fprintf(fp, "# ResidRateError(us/s)    %+.5E\n",
        dp->fringe->t208->rate_error);
    fprintf(fp, "# ResidPhaseError(deg)    %+.5E\n",
        dp->status->phase_err);
    fprintf(fp, "#\n");
}
/*
 * This dumps the fine print at the bottom.
 */
static void dump_fineprint(FILE *fp, struct type_dump *dp)
{
    static char *pcstr[5]={"","NORMAL","AP BY AP","MANUAL", "MULTITONE"};
    static char tcohere[33];
    int ii, numsb, srate, eff_npols, drate;
    fprintf(fp, "# FINEPRINT\n");
    fprintf(fp, "# Amplitude               %+.5E\n",
        dp->fringe->t208->amplitude*10000.0);
    fprintf(fp, "# AmplitudeError          %+.5E\n",
        dp->status->amp_err);
    fprintf(fp, "# PhPerSegRMS(deg)        %+.5E\n",
        dp->status->timerms_phase);
    fprintf(fp, "# PhPerSegTh(deg)         %+.5E\n",
        dp->status->th_timerms_phase);
    fprintf(fp, "# AmpPerSegRMS(%%)         %+.5E\n",
        dp->status->timerms_amp);
    fprintf(fp, "# AmpPerSegTh(%%)          %+.5E\n",
        dp->status->th_timerms_amp);
    fprintf(fp, "# PhsPerFreqRMS(deg)      %+.5E\n",
        dp->status->freqrms_phase);
    fprintf(fp, "# PhsPerFreqTh(deg)       %+.5E\n",
        dp->status->th_freqrms_phase);

    fprintf(fp, "# DelayRateSearchPoints   %d\n", dp->status->drsp_size);
    fprintf(fp, "# MBDelaySearchPoints     %d\n", dp->status->grid_points);
    fprintf(fp, "# Interpolation           %+.5E\n", dp->status->interp_amp);
    fprintf(fp, "# IncSegAve               %+.5E\n",
        dp->fringe->t208->inc_seg_ampl);
    fprintf(fp, "# IncFreqAve              %+.5E\n",
        dp->fringe->t208->inc_chan_ampl);
    fprintf(fp, "# BitsPerSampleRef        %d\n", dp->param->bits_sample[0]);
    fprintf(fp, "# BitsPerSampleRem        %d\n", dp->param->bits_sample[1]);
    fprintf(fp, "# SampCntNorm             '%s'\n",
        dp->param->use_sample_cnts ? "enabled" : "disabled");

    fprintf(fp, "# PcalRateRef(us/s)       %+.3E\n",
        dp->fringe->t207->ref_pcrate);
    fprintf(fp, "# PcalRateRem(us/s)       %+.3E\n",
        dp->fringe->t207->rem_pcrate);
    fprintf(fp, "# PcalModeRef             '%s'\n",
        pcstr[dp->param->pc_mode[0]]);
    fprintf(fp, "# PcalModeRem             '%s'\n",
        pcstr[dp->param->pc_mode[1]]);
    fprintf(fp, "# PcalPeriodRef(AP)       %d\n", dp->param->pc_period[0]);
    fprintf(fp, "# PcalPeriodRem(AP)       %d\n", dp->param->pc_period[1]);
    srate = (int)rint(1e-6 / dp->param->samp_period + 0.5);
    fprintf(fp, "# SampleRate(MSamp/s)     %d\n", srate);
    eff_npols = (dp->pass->npols > 2) ? 2 : dp->pass->npols;
    for (ii = 0, numsb = 0; ii < dp->pass->nfreq; ii++) {
        if (dp->status->ap_num[0][ii]) numsb++;
        if (dp->status->ap_num[1][ii]) numsb++;
    }
    drate = (int)(numsb * eff_npols * srate *
        sqrt((double)(dp->param->bits_sample[0] *
                      dp->param->bits_sample[1])) + 0.5);
    fprintf(fp, "# DataRate(Mb/s)          %d\n", drate);
    fprintf(fp, "# NumLags                 %d\n", dp->param->nlags);
    if (dp->pass->control.t_cohere > 0.0)
        snprintf(tcohere,sizeof(tcohere)," %+%5E",dp->pass->control.t_cohere);
    else
        strncpy(tcohere, "infinite", sizeof(tcohere));
    fprintf(fp, "# CoherenceTime           '%s'\n", tcohere);

    fprintf(fp, "# IonWindow(TEC)          '%8.2f %8.2f'\n",
        dp->param->win_ion[0], dp->param->win_ion[1]);
    fprintf(fp, "# SBWindow(us)            '%8.3f %8.3f'\n",
        dp->param->win_sb[0], dp->param->win_sb[1]);
    fprintf(fp, "# MBWindow(us)            '%8.3f %8.3f'\n",
        dp->param->win_mb[0], dp->param->win_mb[1]);
    fprintf(fp, "# InterpolationMethod     '%s'\n",
        (dp->param->interpol == ITERATE) ? "iterative interpolator" :
            (dp->param->interpol == SIMUL) ? "simultaneous interpolator" :
                "undefined");

    fprintf(fp, "# ControlFile             '%s'\n", control_filename);
    //FIXME -- yet morethings
}

/*
 * This dumps out the one-d plot data for the 5 curves in the 3 figures
 */
static void dump_onedim(FILE *fp, char *lb, int pr, int npts, double *data)
{
    char fmt[20];
    int width = 5 + pr, dpl = PDDCOLS / width - 2, ii;
    int full = npts/(dpl+1), runt = npts - (dpl+1)*full;
    snprintf(fmt, 20, "%%+%d.%df%%c", width, pr);
    fprintf(fp, "# %s %d {%s(%dx%d+%d)}\n", lb, npts, fmt, dpl+1, full, runt);
    for (ii = 0; 0 < npts--; data++) {
        fprintf(fp, fmt, *data, (ii == dpl) ? '\n' : ' ');
        if (ii == dpl) ii = 0 ; else ii++;
    }
    if (ii) fprintf(fp, "\n");
}
/*
 * Calls dump_onedim twice for magnitude and phase
 */
static void dump_xpspec(FILE *fp, char *lb, int pr, int npts, complex *data)
{
    static char lab[20];
    static double vec[2*MAXLAG];
    int ii;
    snprintf(lab, sizeof(lab), "%s-ABS", lb);
    for (ii = 0; ii < npts; ii++) vec[ii] = cabs(data[ii]);
    dump_onedim(fp, lab, pr, npts, vec);
    snprintf(lab, sizeof(lab), "%s-ARG", lb);
    for (ii = 0; ii < npts; ii++) vec[ii] = R2D(carg(data[ii]));
    dump_onedim(fp, lab, pr, npts, vec);
}

/*
 * This dumps out two-d plot data[MAXFREQ+1][MAXAP] for phasor or weights.
 * Logic is similar to dump_onedim, except we loop over AP (which
 * inverts the normal order order, but is perhaps more useful).
 * npts is num freq + 1 for the total fringe data
 *
 * thyme = ((ap + 0.5) * pp->acc_period + pp->start) / 8.64e4
 */
static void dump_twodim(FILE *fp, char *lb, int pr, int npts, int apo, int nap,
    double period, double start, double w[][MAXAP])
{
    static char tfmt[] = "%12.8f %4d%c";
    char fmt[20];
    int width = 5 + pr, dpl = PDDCOLS / width - 2, ii, ap, nn;
    int full = npts/(dpl+1), runt = npts - (dpl+1)*full;
    int ntphs = (strcmp(lb, "PHASOR-ARG")) ? 1 : 0, nptsm1 = npts - 1;
    double thyme, val, dnptsm1 = (double)(nptsm1);
    snprintf(fmt, 20, "%%+%d.%df%%c", width, pr);
    fprintf(fp, "# %s %dx%d {[%s+%s(%dx%d+%d)]x%d}\n",
        lb, npts, nap, tfmt, fmt, dpl+1, full, runt, nap);
    for (ap = apo; ap < nap; ap++) {
        thyme = ((ap + 0.5) * period + start) / 8.64e4;
        fprintf(fp, tfmt, thyme, ap, '\n');
        for (ii = 0, nn = 0; nn < npts; nn++) {
            if (ntphs && (nn == nptsm1)) val = w[nn][ap] / dnptsm1;
            else                         val = w[nn][ap];
            fprintf(fp, fmt, val, (ii == dpl) ? '\n' : ' ');
            if (ii == dpl) ii = 0 ; else ii++;
        }
        if (ii) fprintf(fp, "\n");
    }
}
/*
 * Call dump_twodim twice for magnitude and phase
 * Cf fill_212.c for use of status->amp_corr_fact
 */
static void dump_phasor(FILE *fp, char *lb, int pr, int npts, int apo, int nap,
    double period, double start, complex w[][MAXAP], double acf)
{
    static char lab[20];
    static double vec[MAXFREQ+1][MAXAP];
    int ii, ap;
    snprintf(lab, sizeof(lab), "%s-ABS", lb);
    for (ap = apo; ap < nap; ap++)
        for (ii = 0; ii < npts; ii++)
            vec[ii][ap] = cabs(w[ii][ap]) * acf;
    dump_twodim(fp, lab, pr, npts, apo, nap, period, start, vec);
    snprintf(lab, sizeof(lab), "%s-ARG", lb);
    for (ap = apo; ap < nap; ap++)
        for (ii = 0; ii < npts; ii++)
            vec[ii][ap] = R2D(carg(w[ii][ap]));
    dump_twodim(fp, lab, pr, npts, apo, nap, period, start, vec);
}

/*
 * This dumps out the mean_ap[MAXFREQ+1][MAXAP].
 * Time is now provided by the mean_ap array, so the output format
 * is different.  This forms the xaxis of the plot
 *
 * thyme = ((mean_ap + 0.5) * pp->acc_period + pp->start) / 8.64e4
 */
static void dump_mean_ap(FILE *fp, char *lb, int pr, int npts, int nseg,
    int apseg, double period, double start, double x[][MAXAP])
{
    static char tfmt[30] = "%4d %9.4f%c";
    char fmt[20];
    int width = 5 + pr, dpl = PDDCOLS / width - 2, sap, ii, nn;
    int full = npts/(dpl+1), runt = npts - (dpl+1)*full;
    double thyme;
    snprintf(fmt, 20, "%%+%d.%df%%c", width, pr);
    fprintf(fp, "# %s %dx%d {[%s+%s(%dx%d+%d)]x%d}\n",
        lb, npts, nseg, tfmt, fmt, dpl+1, full, runt, nseg);
    for (sap = 0; sap < nseg; sap++) {
        fprintf(fp, tfmt, sap, ((double)(sap)+0.5)*(double)(apseg), '\n');
        for (ii = 0, nn = 0; nn < npts; nn++) {
            thyme = ((x[nn][sap] + 0.5) * period + start) / 8.64e4;
            fprintf(fp, fmt, thyme, (ii == dpl) ? '\n' : ' ');
            if (ii == dpl) ii = 0 ; else ii++;
        }
        if (ii) fprintf(fp, "\n");
    }
}

/*
 * This dumps out segmented [MAXFREQ+1][MAXAP] which is the yaxis
 * of the plot data.
 */
static void dump_segment(FILE *fp, char *lb, int pr, int npts, int nseg,
    double y[][MAXAP])
{
    static char tfmt[30] = "%4d%c";
    char fmt[20];
    int width = 5 + pr, dpl = PDDCOLS / width - 2, sap, ii, nn;
    int full = npts/(dpl+1), runt = npts - (dpl+1)*full;
    snprintf(fmt, 20, "%%+%d.%df%%c", width, pr);
    fprintf(fp, "# %s %dx%d {[%s+%s(%dx%d+%d)]x%d}\n",
        lb, npts, nseg, tfmt, fmt, dpl+1, full, runt, nseg);
    for (sap = 0; sap < nseg; sap++) {
        fprintf(fp, tfmt, sap, '\n');
        for (ii = 0, nn = 0; nn < npts; nn++) {
            fprintf(fp, fmt, y[nn][sap], (ii == dpl) ? '\n' : ' ');
            if (ii == dpl) ii = 0 ; else ii++;
        }
        if (ii) fprintf(fp, "\n");
    }
}

/*
 * This prints something that can easily suggest how to filter data
 */
static void dump_ampfilter(FILE *fp, char *lb, int pr, int npts, int nseg,
    int apseg, double period, double start,
    double x[][MAXAP], double y[][MAXAP])
{
    static char tfmt[] = "%12.8f ", ffmt[] = "%02X%s";
    char fmt[20], *ahf = getenv("HOPS_AMP_SEG_FILTER");
    double thyme, ahfv = (ahf) ? atof(ahf) : 0.0;
    int width = 5 + pr, sap, nn, nptsm1 = npts-1;
    snprintf(fmt, 20, "%%+%d.%df%%c", width, pr);
    fprintf(fp, "* %s %dx%d {[%s+%s(%d)]x%d}\n",
        lb, npts, nseg, tfmt, fmt, npts, nseg);
    for (sap = 0; sap < nseg; sap++) {
        for (nn = 0; nn < npts; nn++) {
            if (nn == 0) {
                thyme = ((x[nn][sap] + 0.5) * period + start) / 8.64e4;
                fprintf(fp, tfmt, thyme);
            }
            if (ahfv > 0.0)
                fprintf(fp, ffmt, (y[nn][sap] > ahfv) ? 0xFF : 0x00,
                    (nn == nptsm1) ? "\n" : "");
            else
                fprintf(fp, fmt, y[nn][sap], (nn == nptsm1) ? '\n' : ' ');
        }
    }
}

static void dump_legend(FILE *fp, char *lb)
{
    fprintf(fp,
        "# %s: you can set HOPS_PLOT_DATA_MASK in your environment\n"
        "# as an or'd mask of the following to control the contents\n"
        "# of this file.  The bits in the mask are as follows:\n", lb);
    PDD_DESCRIBE(fp, PDD_HEADER);
    PDD_DESCRIBE(fp, PDD_SBD_AMP);
    PDD_DESCRIBE(fp, PDD_MBD_AMP);
    PDD_DESCRIBE(fp, PDD_DLYRATE);
    PDD_DESCRIBE(fp, PDD_XPSPEC);
    PDD_DESCRIBE(fp, PDD_PHASOR);
    PDD_DESCRIBE(fp, PDD_WEIGHTS);
    PDD_DESCRIBE(fp, PDD_MEAN_AP);
    PDD_DESCRIBE(fp, PDD_SEG_AMP);
    PDD_DESCRIBE(fp, PDD_SEG_PHS);
    PDD_DESCRIBE(fp, PDD_SEG_FRAC_USB);
    PDD_DESCRIBE(fp, PDD_SEG_FRAC_LSB);
    PDD_DESCRIBE(fp, PDD_SEG_REFSCNT_USB);
    PDD_DESCRIBE(fp, PDD_SEG_REFSCNT_LSB);
    PDD_DESCRIBE(fp, PDD_SEG_REMSCNT_USB);
    PDD_DESCRIBE(fp, PDD_SEG_REMSCNT_LSB);
    PDD_DESCRIBE(fp, PDD_SEG_REFBIAS_USB);
    PDD_DESCRIBE(fp, PDD_SEG_REFBIAS_LSB);
    PDD_DESCRIBE(fp, PDD_SEG_REMBIAS_USB);
    PDD_DESCRIBE(fp, PDD_SEG_REMBIAS_LSB);
    PDD_DESCRIBE(fp, PDD_SEG_REFPCAL);
    PDD_DESCRIBE(fp, PDD_SEG_REMPCAL);
    PDD_DESCRIBE(fp, PDD_SEG_PLOT_INFO);
    PDD_DESCRIBE(fp, PDD_SEG_AMP_FILTER);
    PDD_DESCRIBE(fp, PDD_MODELINFO);
    PDD_DESCRIBE(fp, PDD_FINEPRINT);
    PDD_DESCRIBE(fp, PDD_LEGEND);
    PDD_DESCRIBe(fp, PDD_SOME, "the default");
    PDD_DESCRIBe(fp, PDD_ALL, "everything");
    fprintf(fp,
        "# PDD_SEG_AMP_FILTER is special in that if you then supply\n"
        "# an environment variable HOPS_AMP_SEG_FILTER; a table of ad\n"
        "# hoc flags will be generated in the output between the lines\n"
        "# containing SEG_AMP_FILTER and END_AMP_FILTER.  This table\n"
        "# may then be used directly as an adhoc_flag_file to remove\n"
        "# snippets of data with a lower amplitude than the value that\n"
        "# was specified by HOPS_AMP_SEG_FILTER.  Use with caution.\n");
    fprintf(fp,
        "#\n"
        "# More generally, some of the data in this file may be limited\n"
        "# to the channels selected for plotting (e.g. a frequency group\n"
        "# or limited by the -f -n command line options.\n");
}

/*
 * Main entry to writing out the data; returns system errno on errors.
 */
static int populate_data_file(char *outname, struct type_dump *dp)
{
    char *hpdm = getenv("HOPS_PLOT_DATA_MASK");
    FILE *fp = fopen(outname, "w");
    int rv, nlags2 = 2 * dp->param->nlags;
    int drates = dp->status->drsp_size < 256 ? 256 : dp->status->drsp_size;
    int datamask = (hpdm) ? (int)strtoul(hpdm, 0, 16) : PDD_SOME;

    if (!fp) {
        perror("dump_plot_data2dir:fopen");
        msg("Unable to open %s for plot data", 3, outname);
        return(errno);
    }

    /* all the magic numbers */
    if (datamask & PDD_HEADER) {
      dump_header(fp, outname, dp);
      fprintf(fp, "# HOPS_PLOT_DATA_MASK %0X (%s)\n", datamask,
        hpdm ? hpdm : "undefined");
      fprintf(fp, "\n");
    }

    /* the 3 main fringe plots */
    if (datamask & PDD_SBD_AMP)
      dump_onedim(fp, "SBD_AMP", 6, nlags2, dp->plot->sb_amp);
    if (datamask & PDD_MBD_AMP)
      dump_onedim(fp, "MBD_AMP", 6, dp->plot->num_mb_pts, dp->plot->mb_amp);
    if (datamask & PDD_DLYRATE)
      dump_onedim(fp, "DLYRATE", 6, drates, dp->plot->d_rate);
    if (datamask & PDD_XPSPEC)
      dump_xpspec(fp, "XPSPEC",  6, nlags2, dp->plot->cp_spectrum);
    if (datamask & (PDD_SBD_AMP|PDD_MBD_AMP|PDD_DLYRATE|PDD_XPSPEC))
      fprintf(fp, "\n");

    /* the fringe data by freq and ap */
    if (datamask & PDD_PHASOR)
      dump_phasor(fp, "PHASOR",  6,
        dp->plot->num_freq + 1, dp->pass->ap_off, dp->plot->num_ap,
        dp->param->acc_period, dp->param->start, dp->plot->phasor,
        dp->status->amp_corr_fact);
    if (datamask & PDD_WEIGHTS)
      dump_twodim(fp, "WEIGHTS", 6,
        dp->plot->num_freq + 1, dp->pass->ap_off, dp->plot->num_ap,
        dp->param->acc_period, dp->param->start, dp->plot->weights);
    if (datamask & (PDD_PHASOR|PDD_WEIGHTS))
      fprintf(fp, "\n");

    /* this is the x-axis for all the segmented plot data */
    if (datamask &  PDD_MEAN_AP) {
      dump_mean_ap(fp, "MEAN_AP", 8,
        dp->plot->num_freq + 1, dp->status->nseg, dp->status->apseg,
        dp->param->acc_period, dp->param->start, dp->plot->mean_ap);
      fprintf(fp, "\n");
    }

    /* the segmented plot data by selected freq and seg ap */
    if (datamask &  PDD_SEG_AMP)
      dump_segment(fp, "SEG_AMP", 6,
        dp->plot->num_freq + 1, dp->status->nseg, dp->plot->seg_amp);
    if (datamask &  PDD_SEG_PHS)
      dump_segment(fp, "SEG_PHS", 6,
        dp->plot->num_freq + 1, dp->status->nseg, dp->plot->seg_phs);
    if (datamask &  PDD_SEG_FRAC_USB)
      dump_segment(fp, "SEG_FRAC_USB", 6,
        dp->plot->num_freq + 1, dp->status->nseg, dp->plot->seg_frac_usb);
    if (datamask &  PDD_SEG_FRAC_LSB)
      dump_segment(fp, "SEG_FRAC_LSB", 6,
        dp->plot->num_freq + 1, dp->status->nseg, dp->plot->seg_frac_lsb);
    if (datamask & (PDD_SEG_AMP|PDD_SEG_PHS|PDD_SEG_FRAC_USB|PDD_SEG_FRAC_LSB))
      fprintf(fp, "\n");

    /* seg_referr seg_remerr look to be zero these days--no tapes */

    /* the segmented state counts */
    if (datamask &  PDD_SEG_REFSCNT_USB)
      dump_segment(fp, "SEG_REFSCNT_USB", 6,
        dp->plot->num_freq + 1, dp->status->nseg, dp->plot->seg_refscnt_usb);
    if (datamask &  PDD_SEG_REFSCNT_LSB)
      dump_segment(fp, "SEG_REFSCNT_LSB", 6,
        dp->plot->num_freq + 1, dp->status->nseg, dp->plot->seg_refscnt_lsb);
    if (datamask &  PDD_SEG_REMSCNT_USB)
      dump_segment(fp, "SEG_REMSCNT_USB", 6,
        dp->plot->num_freq + 1, dp->status->nseg, dp->plot->seg_remscnt_usb);
    if (datamask &  PDD_SEG_REMSCNT_LSB)
      dump_segment(fp, "SEG_REMSCNT_LSB", 6,
        dp->plot->num_freq + 1, dp->status->nseg, dp->plot->seg_remscnt_lsb);
    if (datamask & (PDD_SEG_REFSCNT_USB|PDD_SEG_REFSCNT_LSB|
                    PDD_SEG_REMSCNT_USB|PDD_SEG_REMSCNT_LSB))
      fprintf(fp, "\n");

    /* the segmented state counts bias */
    if (datamask &  PDD_SEG_REFBIAS_USB)
      dump_segment(fp, "SEG_REFBIAS_USB", 6,
        dp->plot->num_freq + 1, dp->status->nseg, dp->plot->seg_refbias_usb);
    if (datamask &  PDD_SEG_REFBIAS_LSB)
      dump_segment(fp, "SEG_REFBIAS_LSB", 6,
        dp->plot->num_freq + 1, dp->status->nseg, dp->plot->seg_refbias_lsb);
    if (datamask &  PDD_SEG_REMBIAS_USB)
      dump_segment(fp, "SEG_REMBIAS_USB", 6,
        dp->plot->num_freq + 1, dp->status->nseg, dp->plot->seg_rembias_usb);
    if (datamask &  PDD_SEG_REMBIAS_LSB)
      dump_segment(fp, "SEG_REMBIAS_LSB", 6,
        dp->plot->num_freq + 1, dp->status->nseg, dp->plot->seg_rembias_lsb);
    if (datamask & (PDD_SEG_REFBIAS_USB|PDD_SEG_REFBIAS_LSB|
                    PDD_SEG_REMBIAS_USB|PDD_SEG_REMBIAS_LSB))
      fprintf(fp, "\n");

    /* the segmented phase cals */
    if (datamask &  PDD_SEG_REFPCAL)
      dump_segment(fp, "SEG_REFPCAL", 6,
        dp->plot->num_freq + 1, dp->status->nseg, dp->plot->seg_refpcal);
    if (datamask &  PDD_SEG_REMPCAL)
      dump_segment(fp, "SEG_REMPCAL", 6,
        dp->plot->num_freq + 1, dp->status->nseg, dp->plot->seg_rempcal);
    if (datamask & (PDD_SEG_REFPCAL|PDD_SEG_REMPCAL))
      fprintf(fp, "\n");

    /* the stuff you cannot read */
    if (datamask & PDD_SEG_PLOT_INFO)
      dump_plot_info(fp, dp);
    if (datamask & PDD_MODELINFO)
      dump_modelinfo(fp, dp);
    if (datamask & PDD_FINEPRINT)
      dump_fineprint(fp, dp);
    if (datamask & (PDD_SEG_PLOT_INFO|PDD_MODELINFO|PDD_FINEPRINT))
      fprintf(fp, "\n");

    /* dump out a flagging table */
    if (datamask & PDD_SEG_AMP_FILTER) {
      dump_ampfilter(fp, "SEG_AMP_FILTER", 3,
        dp->plot->num_freq + 1, dp->status->nseg, dp->status->apseg,
        dp->param->acc_period, dp->param->start,
        dp->plot->mean_ap, dp->plot->seg_amp);
      fprintf(fp, "* END_AMP_FILTER\n");
      fprintf(fp, "\n");
    }

    if (datamask & PDD_LEGEND)
      dump_legend(fp, "LEGEND");

    fprintf(fp, "#\n# eof\n#\n");
    if (fclose(fp)) {
        perror("dump_plot_data2dir:fclose");
        msg("Unable to close %s with plot data", 3, outname);
    }
    return(errno);
}

/*
 * Create a name for the data file; then open, populate and close file.
 */
void dump_plot_data2dir(struct type_dump *dump)
{
    static char rootname[1024], outname[1024], scanname[1024];
    char *outdir = (char*)0, *sn, *rc;
    int rv;
    struct stat sbuf;

    /* one of these must have been set for us to be here */
    assert(dump->param->plot_data_dir[0][0] ||
           dump->param->plot_data_dir[1][0]);
    if (dump->param->plot_data_dir[0][0])
        outdir = dump->param->plot_data_dir[0];
    else
        outdir = dump->param->plot_data_dir[1];

    /* make sure the directory exists */
    rv = stat(outdir, &sbuf);
    msg("Stat rv is %d, errno is %d on outdir '%s'", 1, rv, errno, outdir);
    if (rv < 0 && errno == ENOENT) {
        rv = mkdir(outdir, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
        if (rv < 0) perror("dump_plot_data2dir:mkdir");
    } else if (rv < 0) {
        perror("dump_plot_data2dir:stat");
    }
    if (rv < 0) {
        msg("Unable to create output directory", 3);
        return;
    }

    /* similar to create_fname (root->ovex, pass, fringe_name) */
    strcpy(rootname, dump->root->filename);
    strcpy(scanname, dirname(rootname));
    sn = strrchr(scanname, '/') + 1;
    rc = strrchr(dump->root->filename, '.') + 1;
    snprintf(outname, sizeof(outname), "%s/%s-%c%c-%c-%s.%s",
        outdir, sn, dump->param->baseline[0], dump->param->baseline[1],
        dump->pass->pass_data[0].fgroup, dump->meta->polstr, rc);

    /* hand off for the actual work */
    rv = populate_data_file(outname, dump);
    if (rv) msg("Issue %d while writing plot data %s", 3, rv, outname);
}

/*
 * eof
 */
