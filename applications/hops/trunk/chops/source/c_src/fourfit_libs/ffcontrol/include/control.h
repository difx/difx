#ifndef control_done
#define control_done

#include "mk4_sizes.h"

#define MAX_SAMP 16
#define MAXNOTCH (8*MAXFREQ)


struct gat_struct
   {
   short on_delay;                 /* in secs since gate reference epoch */
   short duration;                 /* in secs */
   };

struct istats 
    {
    int             ref;
    int             rem;
    };

struct dstats 
    {
    double          ref;
    double          rem;
    };

struct c_block                     /* Elemental control block structure */
   {
   struct c_block *cb_chain;       /* Chained pointer to next c_block */

         /* Conditions that scan must meet to have this block be applied */

   char baseline[2];               /* station codes at both ends of baseline;
                                      either or both can be wildcards */
   char source[32];                /* source name, or single wildcard */
   char f_group;                   /* frequency group code, or wildcard */
   int scan[2];                    /* acceptance interval (inclusive) for start 
                                      of scan; in seconds since 1980.0 */
   short knot[4];                  /* knot[i] true when cond[i] "not-ed" */


          /* Filter, corrections, etc. to apply to data within qualifying scan */

   short skip;                     /* iff true, don't fourfit matching scans */
   double min_weight;              /* min t120->fw.weight for AP acceptance */
   double ref_freq;                /* force fourfit to use this ref. freq. (MHz) */
   int accept_sbs[MAXFREQ];        /* accept USB, LSB, DSB iff = 1, 2, 3 */
   short index[2*MAXFREQ];         /* index numbers of acceptable sidebands */
   struct istats pc_mode;          /* phase cal modes */
   struct istats pc_period;        // phase cal integration period (in ap's)
   struct dstats pc_freq[MAXFREQ]; /* phase cal freqs (KHz) by channel */
   struct dstats pc_phase_offset[2];// manual phase offset applied to all channels, by pol 
   struct dstats pc_phase[MAXFREQ][2];/* phase cal phases by channel and pol 
                                             for manual or additive pcal */
   struct istats pc_tonemask[MAXFREQ];// tone exclusion mask by channel in multitone
   struct dstats lsb_offset;       /* LSB phase offset in degrees */
   short x_crc;                    /* flag to keep/discard AP having a crc error */
   short y_crc;
   short x_slip_sync;              /* max. # of frame resyncs to still use AP's data */
   short y_slip_sync;
   double sb_window[2];            /* bounds of single band delay search window (us) */
   double mb_window[2];            /* bounds of multi-band delay search window (us) */
   double dr_window[2];            /* bounds of delay-rate search window (us/s) */
   double ion_window[2];           // bounds of ionospheric search window (TEC units)
   double ra_offset;               /* offset RA (arcsec) to shift windows by */
   double dec_offset;              /*    "   DEC    "     "   "      "     " */
   int ion_npts;                   // # of pts in ionosphere coarse search
   int  time_span[2];              /* acceptance window start and stop (s BOY) */
   short ion_smooth;               // iff true, use smoothed, interpolated TEC grid pts
   short switched_mode;            /* defines switching cycle start epoch */
   short switched_period;          /* switching cycle period (s) */
   short use_samples;              /* iff true, use sample counts to normalize */
   short dc_block;                 // iff true, zero out DC subchannel of spectrum
   short optimize_closure;         // iff true, keep closure triangle noise small as possible
   struct gat_struct gates[MAXFREQ]; /* relative on/off epochs (s), code, for each channel */
   int adhoc_phase;                /* defines type of ad hoc phase adjustments */
   double adhoc_tref;              /* reference time for either ad hoc model (s past hour */
   double adhoc_period;            /* period of sinewave (s) */
   double adhoc_amp;               /* amplitude in radians */
   double adhoc_poly[6];           /* ad hoc phase polynomial coefficients (rad/sec^n) */
   char adhoc_file[2][256];        // file names if station(s) is in pc_mode file
   char adhoc_file_chans[2][128];  // channel codes [a..zA..Z$%] for pc file phase fields
   char adhoc_flag_files[2][256];  /* file names for ad hoc data flagging */
   char plot_data_dir[2][256];     /* dir name(s) for dumping plot data */
   double passband[2];             /* passband for spectral filtering (MHz) */
   int gen_cf_record;              /* whether to general control file record */
   int nnotches;                   /* alternative to passband */
   double notches[MAXNOTCH][2];    /* alternative to passband */
   double t_cohere;                /* coherence time (s) for co-adding fringe rates */
   struct dstats ionosphere;       // a priori ionospheres (TEC units = 1e16 el/m^2)
   struct dstats delay_offs[MAXFREQ];// additive delay offset(ns) by channel  ##DELAY_OFFS##
   struct dstats delay_offs_pol[MAXFREQ][2];// additive delay offset(ns) by channel and pol
   int nsamplers;                  // number of sampler strings
   char *psamplers[MAX_SAMP];      // pointer to each sampler string (or NULL)
   char sampler_codes[256];        // contains all sampler strings
   struct dstats sampler_delay[MAX_SAMP][2]; // additive delay per sampler (s), in sampler
                                   // order, for each of 2 polarizations (L/R = X/Y)
   int est_pc_manual;              // provide estimates of manual pc phases
   int interpolator;               // interpolation method
   int mbd_anchor;                 // mbd ambiguity choice: model or sbd
   struct dstats station_delay;    // station delay pc inject->digitizer (s)
   struct dstats pc_delay_l;       // delay diff (feed->inject)-(pulsegen->inject) (s)
   struct dstats pc_delay_r;       // same, but for RCP (or Y or V)
   double weak_channel;            // G code if single_chan_amp/coherent_amp < weak_channel
   double pc_amp_hcode;            // H code iff any pc amplitude less than this
   double fmatch_bw_pct;           // fractional bw % used for frequency matching
   char chid[MAXFREQ];             // single letter ch id codes for freq override
   double chid_rf[MAXFREQ];        // freqs corresponding to above codes
   int vbp_correct;                // iff true, modify xpower phase with video bandpass model
   int vbp_fit;                    // iff true, do algebraic model fit for video bandpass
   struct dstats vbp_coeffs[5];    // video bandpass model coeffs (deg)
   char vbp_file[2][256];          // if not null, specifies filename for video bandpass corr.
   };

          /* Defined values for various structure variables */

#define WILDCARD  '?'              /* station, baseline, source, f_group */
#define COMCHAR   '*'              /* comment character */

#define KEEP      32767            /* x_crc, y_crc, x_slip_sync, y_slip_sync */
#define DISCARD   1 

#define NORMAL    1                /* pc_mode */
#define AP_BY_AP  2 
#define MANUAL    3 
#define MULTITONE 4

#define USB 1                      /* frequency array values */
#define LSB 2 
#define DSB 3 

#define SCAN_START  1              /* strobe_mode */
#define EACH_MINUTE 2 

#define SINEWAVE    1              /* ad hoc phase adjustment mode */
#define POLYNOMIAL  2
#define PHYLE       3

#define NULLINT   -12345           /* place-holder for no assigned integer value */
#define NULLFLOAT 508.4482826      /*   "     "     "   "    "     floating   "  */
#define NULLCHAR  0                /*   "     "     "   "    "     char       "  */

                                   // interpolation methods
#define ITERATE 1
#define SIMUL   2
                                   // mbd anchor choice
#define MODEL 1
#define SBD   2

#define FCHARS "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$%"
#endif
