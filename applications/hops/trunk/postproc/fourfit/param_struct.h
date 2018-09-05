#ifndef PARAM_STRUCT_H__
#define PARAM_STRUCT_H__
                                        /*  Definitions of major parameter structures */
#define MAXINDEX 256
#define MAX_ION_PTS 100
#define LAG 0
#define GLOBAL 1
#define MAXNOTCH (8*MAXFREQ)

#include "plot_struct.h"

struct type_param 
    {
                                        /* Filled in by time_range() */
    double      start_nom;              /* nom. scan start time from ovex (sBOY) */
    double      stop_nom;               /*  "     "  stop   "     "    "  (sBOY) */
    int         start_offset;
    int         stop_offset;
    double      start;                  /* actual start time of earliest AP (sBOY) */
    double      stop;                   /*    "   stop    "   " latest    " (sBOY) */
    double      reftime;                /* fourfit ref. time (midpoint in s since BOY) */
    double      frt_offset;             /* Offset of ref time from scan start */
    double      acc_period;             /* Length of accumulation period (sec)  */
    int         minap;                  /* First AP number */
    int         maxap;                  /* Last AP number */
    int         num_ap;                 /* Number of Accumulation Periods */
    char        baseline[2];            /* Carried for convenience */
    short       ov_bline[2];            // index into ovex station array for ref/rem
    short       corr_type;              /* Determines type_120 format */
    short       pol;                    // Polarization type - vals def'd in pass_struct.h
    char        *rf_fglist;             /* List of fgroups when refringing */
    double      cor_limit;              /* Maximum correlation amplitude */
    int         bits_sample[2];         /* Bits per sample (1 or 2) for ref & rem */
    double      samp_period;            // Len sample period (interval between samp in s)
    short       pc_mode[2];             /* Phase cal mode by station */
    short       pc_period[2];           // Phase cal integration period (ap's)
    double      pc_freq[MAXFREQ][2];    /* phase cal tone freq (KHz) by chan & stn */
    int         pc_tonemask[MAXFREQ][2];// pc multitone exclusion mask by chan & stn
    double      win_sb[2];              /* Single band search window microsec */
    double      win_dr[2];              /* Delay rate search window microsec per sec */
    double      win_mb[2];              /* Multi band search window  microsec */
    double      win_ion[2];             // bounds of ionosphere search window (TEC units)
    double      inv_sigma;              /* 1 / (1 sigma noise) */
    double      epoch_time;             /* Epoch time */
    int         cormode;                /* determines type 120 union */
    int         nlags;                  /* Variable in Mk4 */
    double      ref_freq;               /* Reference frequency in MHz */
    double      ah_tref;                /* Ad hoc reference time (s past hour) */
    double      ah_period;              /* Ad hoc sinusoid period (s) */
    double      ah_amp;                 /* Ad hoc sinusoid amplitude (rad) */
    double      ah_poly[6];             /* Ad hoc polynomial coefficients */
    int         ah_phase;               /* Ad hoc phase correction mode */
    char        ah_file[2][256];        // file names for ad hoc file mode pcal
    char        ah_file_chans[2][256];  // channel codes for ah_file contents
    char        ah_flag_files[2][256];  /* Ad hoc flag files */
    char        plot_data_dir[2][256];  /* for dumping plot data */
    unsigned long bocf_period;          /* Correlator frame in systicks */
    int         bocfs_per_ap;           /* Correlator frames per AP */
    short       su_number[2];           /* Ref and remote SU number */
    unsigned long error_mask;           /* Mask used to edit AP data by flags */
    short       use_sample_cnts;        /* iff true, use sample counts to normalize */
    short       dc_block;               // iff true, zero out DC subchannel in spectrum
    double      passband[2];            /* passband for spectral filtering (MHz) */
    int         gen_cf_record;          /* whether to generate cf record */
    int         nnotches;               /* alternative to passband */
    double      notches[MAXNOTCH][2];   /* alternative to passband */
    double      speedup;                /* ration of playback speed to record speed */
    int         first_plot;             // number of first chan to plot, when overridden
    int         nplot_chans;            // number of plot channels when overridden
    int         interpol;               // interpolation method
    int         mbd_anchor;             // anchor total mbd to model or sbd
    double      pcal_spacing[2];        // pcal tone spacing (Hz) for ref & rem
    double      ion_diff;               // differential ionosphere (rem-ref in TEC units)
    int         ion_pts;                // number of pts in ionosphere coarse search
    short       ion_smooth;             // iff true, use smoothed coarse pts for fine search
    double      par_angle[2];           // parallactic angle (rad) for ref & rem
    double      weak_channel;           // G code threshhold for single-chan-amp / coherent-sum-amp
    double      pc_amp_hcode;           // H code iff any pc amplitude less than this
    double      fmatch_bw_pct;          // fractional bw % used for frequency matching
    char*       control_file_buff;      // stripped but unparsed contents of control file
    char*       set_string_buff;        // stripped but unparsed contents of set commands
    int         est_pc_manual;          // estimate pc manual values
    };

#define WIN_EDGE_SBD   0x01             /* masks for status.interp_err */
#define WIN_EDGE_MBD   0x02
#define WIN_EDGE_RATE  0x04
#define INTP_ERR_SBD   0x08
#define INTP_ERR_MBD   0x10
#define INTP_ERR_RATE  0x20

#define MK4HDW 0x01                     // values for corr_type
#define DIFX   0x02

struct type_status 
    {
    double      freq_space;             /* Freq spacing in FFT to MBdelay */
    int         grid_points;            /* # of points in FFT to MBdelay */
    double      freq_spread;            /* Sum of (Freq - avg freq) squared */
    int         large_errors;           /* # of large errors */
    int         sliver_errors;          // # of AP's with sliver (too little data) errors
    int         zero_errors;            // # of AP's with zero (some lag count 0) errors
    int         total_ap;               /* Total # of ap's processed (both sb's )*/
    float       total_ap_frac;          /* Same, but with microediting */
    float       total_usb_frac;         // usb subtotal of total_ap_frac 
    float       total_lsb_frac;         // lsb    "      "    "     "
    int         ap_num[2][MAXFREQ];     /* # of aps for each sideband & freq */
    double      ap_frac[2][MAXFREQ];    /* Same, but with microediting */
    double      epoch_err[MAXFREQ];     /* Epoch error (offset from ref. time) */
    double      epoch_off_cent;         /* Epoch offset from center  */
    int         mb_index[MAXFREQ];      /* index for freq in FFT to MBdelay */
    double      pc_meas[MAXFREQ][2][2]; // phase-cal phase as measured[chan][ref:rem][L:R]
    double      pc_phase[MAXFREQ][2][2];// phase-cal phase as used    [chan][ref:rem][L:R]
    double      pc_amp[MAXFREQ][2][2];  // phase-cal magnitude        [chan][ref:rem][L:R]
    double      pc_offset[MAXFREQ][2][2];//Additive/manual pcal values[chan][ref:rem][L:R]
    double      pcals_accum[2];         /* # of phase-cals for current channel */
    double      lsb_phoff[2];           /* LSB phase offset in radians */
    double      pc_delay[MAXFREQ][2][2];// multitone pcal based delays[chan][ref:rem][L:R]
    int         space_err;              /* =1 if error if freq spacing is too large */
    int         drsp_size;              /* # of points in delay rate spectrum */
    int         f_rate_size;            /* # of points in FFT to Fringe rate */
    int         freq;                   /* Current frequency */
    int         lag;                    /* Current lag # */
    int         dr;                     /* Current delay rate # */
    int         mbd;                    /* Current multi band delay # */
    int         ap;                     /* Current acc per */
    double      dr_max[2*MAXLAG];       /* Delay rate at max by lag */
    double      dr_max_global;          /* Delay rate at global max */
    double      corr_dr_max;            /* Delay rate max corrected for pcal rate */
    double      sbd_max;                /* single band delay at max */
    double      mbd_max[2*MAXLAG];      /* multi band delay at max by lag */
    double      mbd_max_global;         /* multi band delay at global max */
    int         max_delchan;            /* Single band delay channel at max */
    double      delres_max;             /* max value of delay resolution function */
    double      rate_sep;               /* Rate separation of FFT points for freq #1 */
    double      mbd_sep;                /* Multi band delay separation of FFt points */
    double      sbd_sep;                /* Single band delay separation  microsec */
    double      search_amp;             /* Cor. amplitude after initial search */
    double      interp_amp;             /* Amplitude after 1st interpolation */
    double      pc_rate[2];             /* Interpolated phasecal rate */
    double      int_pc_amp[2];          /* Interpolated phasecal amplitude */
    double      amp_corr_fact;          /* Amplitude corr. factor for interpolation */
    double      amp_rate_corr;          /* Rate correction to amplitude */
    int         interp_err;             /* =1 if error in interpolation */
    double      AN;                     /* Normalization from FFT (from FRNGE) */
    int         win_sb[2];              /* Single band search indices */
    int         win_dr[2];              /* Delay rate search indices */
    int         win_mb[2];              /* Multi band search indices */
    int         win_sb_save[2];         /* Saved single band search indices */
    int         win_dr_save[2];         /* Saved delay rate search indices */
    int         win_mb_save[2];         /* Saved multi band search indices */
    int         pts_searched;           /* Number of points searched */
    complex     fringe[MAXFREQ+1];      /* Fringe phase & amp for each freq */
    double      sbdbox[MAXFREQ+1];      /* Single band delay box for each freq */
    double      snr;                    /* Signal to Noise ratio */
    double      prob_false;             /* Probability of false detection */
    int         nseg;                   /* Number of segments in computing time RMS */
    int         apseg;                  /* Number of APs in each segment */
    int         stc_present;            // bits 0|1 set iff ref|rem state counts present
    double      th_timerms_phase;       /* theoretical RMS phase with time */
    double      th_timerms_amp;         /* theoretical RMS amp with time */
    double      th_freqrms_phase;       /* theoretical RMS phase with freq */
    double      th_freqrms_amp;         /* theoretical RMS amp with freq */
    double      timerms_phase;          /* RMS phase with time */
    double      timerms_amp;            /* RMS amp with time */
    double      freqrms_phase;          /* RMS phase with freq */
    double      freqrms_amp;            /* RMS amp with freq */
    double      integ_time;             /* Total integration time */
    double      inc_avg_phase;          /* Fringe phase for incoh. avg. over freq */
    double      inc_avg_amp;            /* Fringe amp. from inc. sum   of all points */
    double      coh_avg_phase;          /* Fringe phase from coh. sum over freq */
    double      inc_avg_amp_freq;       /* Fringe amp from incoh sum over freq */
    double      GHA;                    /* Greenwich hour angle (hrs) */
    double      apdelay;                /* Adjusted apriori delay */
    double      amp_err;                /* Error in fringe amplitude */
    double      apphase;                /* Apriori phase */
    double      resid_phase;            /* Residual phase */
    double      mod_resid_phase;        /* Modified residual phase */
    double      phase_err;              /* Error in fringe phase */
    double      resid_ph_delay;         /* Residual phase delay */
    double      ph_delay_err;           /* Error in phase delay */
    double      sbavg;
    double      rate_ra_width;
    double      rate_dec_width;
    double      sbd_ra_width;
    double      sbd_dec_width;
    double      delay_offs[MAXFREQ][2]; // delay offsets (ns) by channel and station ##DELAY_OFFS##
    double      delay_offs_pol[MAXFREQ][2][2]; // delay offsets (ns) by channel and station and pol
    double      dtec[MAX_ION_PTS][2];   // differential TEC pairs [index#][TEC:amplitude]
    double      ion_sigmas[3];          // std dev of tau (ns), phi0 (rot), dTEC (TECU)
    int         nion;                   // number of points in the dtec array
    int         loopion;                // index for ionospheric search loop (0 if none)
    int         sb_indx;                // index of max when searching over sb delay
    int         mb_indx;                // index of max when searching over mb delay
    int         dr_indx;                // index of max when searching over delay rate
    };
    
#endif
