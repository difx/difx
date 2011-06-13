#ifndef control_done
#define control_done

#include "mk4_data.h"
#include "mk4_sizes.h"


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
   double max_parity;              /* maximum parity error rate for AP acceptance */
   double ref_freq;                /* force fourfit to use this ref. freq. (MHz) */
   int frequency[MAX_CHAN_PP];     /* accept USB, LSB, DSB iff = 1, 2, 3 */
   short index[2*MAX_CHAN_PP];     /* index numbers of acceptable sidebands */
   struct istats pc_mode;          /* phase cal modes */
   struct istats pc_period;        // phase cal integration period (in ap's)
   struct dstats pc_freq[MAX_CHAN_PP]; /* phase cal freqs (KHz) by channel */
   struct dstats pc_phase[MAX_CHAN_PP];/* phase cal phases by channel for manual or additive*/
   struct istats pc_tonemask[MAX_CHAN_PP];// tone exclusion mask by channel in multitone
   struct dstats lsb_offset;       /* LSB phase offset in degrees */
   short x_crc;                    /* flag to keep/discard AP having a crc error */
   short y_crc;
   short x_slip_sync;              /* max. # of frame resyncs to still use AP's data */
   short y_slip_sync;
   double sb_window[2];            /* bounds of single band delay search window (us) */
   double mb_window[2];            /* bounds of multi-band delay search window (us) */
   double dr_window[2];            /* bounds of delay-rate search window (us/s) */
   double ra_offset;               /* offset RA (arcsec) to shift windows by */
   double dec_offset;              /*    "   DEC    "     "   "      "     " */
   int  time_span[2];              /* acceptance window start and stop (s BOY) */
   short switched_mode;            /* defines switching cycle start epoch */
   short switched_period;          /* switching cycle period (s) */
   short use_samples;              /* iff true, use sample counts to normalize */
   struct gat_struct gates[MAX_CHAN_PP]; /* relative on/off epochs (s), code, for each channel */
   int adhoc_phase;                /* defines type of ad hoc phase adjustments */
   double adhoc_tref;              /* reference time for either ad hoc model (s past hour */
   double adhoc_period;            /* period of sinewave (s) */
   double adhoc_amp;               /* amplitude in radians */
   double adhoc_poly[6];           /* ad hoc phase polynomial coefficients (rad/sec^n) */
   double passband[2];             /* passband for spectral filtering (MHz) */
   double t_cohere;                /* coherence time (s) for co-adding fringe rates */
   struct dstats ionosphere;       // a priori ionospheres (TEC units = 1e16 el/m^2)
   };

          /* Defined values for various structure variables */

#define WILDCARD  '?'              /* station, baseline, source, f_group */

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

#define NULLINT   -12345           /* place-holder for no assigned integer value */
#define NULLFLOAT 508.4482826      /*   "     "     "   "    "     floating   "  */

#endif
