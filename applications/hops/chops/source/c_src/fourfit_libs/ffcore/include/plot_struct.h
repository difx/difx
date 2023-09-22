/***************************************/
/*  Definition of structure containing */
/*  fringe plot information.           */
/*  8/7/91      - cmn                  */
/***************************************/

#include "hops_complex.h"

#ifndef MAXAP
#include "mk4_data.h"
#endif
#include "mk4_sizes.h"

#ifndef MBDMXPTS
#define MBDMXPTS 8192
#endif /* MBD_GRID_MAX == MBDMXPTS */

struct type_plot {
        int             num_ap;             /* Number of accumulation periods  */
        int             num_freq;           /* Number of frequencies  */
        int             dr_size;            /* Size of delay rate spectrum */
        int             dr_size_max;        /* Size of dr spec in max sbd chan */
        int             num_mb_pts;         // # of multiband plot points
        double          sb_amp[2*MAXLAG];   /* Single band delay amplitude*/
        double          mb_amp[MBDMXPTS];   /* Multi band delay  amplitude*/
        double          d_rate[MAXAP];      /* Drate spect in max sbd chan */
        hops_complex         cp_spectrum[2*MAXLAG];   /* Cross power spectrum , phase & mag. */
        hops_complex         phasor[MAXFREQ+1][MAXAP];    /* Fringe phase & mag. */
                                               /* (last element = total over fr.)  */
        double          weights[MAXFREQ+1][MAXAP];  /* weight of phasor for each ap */
                                                          /* (depends on # sbands) */
        double          seg_amp[MAXFREQ+1][MAXAP];  /* Amplitude by channel/segment */
        double          seg_phs[MAXFREQ+1][MAXAP];  /* Phase by channel/segment */
        double          mean_ap[MAXFREQ+1][MAXAP];  /* Average AP number for data in segment */
        double          seg_frac_usb[MAXFREQ+1][MAXAP]; /* Fraction of expected data in seg */
        double          seg_frac_lsb[MAXFREQ+1][MAXAP]; /* Fraction of expected data in seg */
        double          seg_referr[MAXFREQ+1][MAXAP]; /* Averaged tape error rates */
        double          seg_remerr[MAXFREQ+1][MAXAP]; /* Averaged tape error rates */
        double          seg_refscnt_usb[MAXFREQ+1][MAXAP]; /* Seg averaged state counts */
        double          seg_refscnt_lsb[MAXFREQ+1][MAXAP]; /* Seg averaged state counts */
        double          seg_remscnt_usb[MAXFREQ+1][MAXAP]; /* Seg averaged state counts */
        double          seg_remscnt_lsb[MAXFREQ+1][MAXAP]; /* Seg averaged state counts */
        double          seg_refbias_usb[MAXFREQ+1][MAXAP]; /* Seg averaged count bias */
        double          seg_refbias_lsb[MAXFREQ+1][MAXAP]; /* Seg averaged count bias */
        double          seg_rembias_usb[MAXFREQ+1][MAXAP]; /* Seg averaged count bias */
        double          seg_rembias_lsb[MAXFREQ+1][MAXAP]; /* Seg averaged count bias */
        double          seg_refpcal[MAXFREQ+1][MAXAP]; /* seg averaged phasecal phase */
        double          seg_rempcal[MAXFREQ+1][MAXAP]; /* seg averaged phasecal phase */
        };
