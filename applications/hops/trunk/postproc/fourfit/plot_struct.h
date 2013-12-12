/***************************************/
/*  Definition of structure containing */
/*  fringe plot information.           */
/*  8/7/91      - cmn                  */
/***************************************/

#ifndef MAXAP
#include "mk4_data.h"
#endif
#include "mk4_sizes.h"

#include "type_comp.h"


struct type_plot {
        int             num_ap;             /* Number of accumulation periods  */
        int             num_freq;           /* Number of frequencies  */
        int             dr_size;            /* Size of delay rate spectrum */
        int             dr_size_max;        /* Size of dr spec in max sbd chan */
        double          sb_amp[2*MAXLAG];   /* Single band delay amplitude*/
        double          mb_amp[8192];       /* Multi band delay  amplitude*/
        double          d_rate[MAXAP];      /* Drate spect in max sbd chan */
        complex         cp_spectrum[2*MAXLAG];   /* Cross power spectrum , phase & mag. */ 
        complex         phasor[MAX_CHAN_PP+1][MAXAP];    /* Fringe phase & mag. */
                                               /* (last element = total over fr.)  */
        double          weights[MAX_CHAN_PP+1][MAXAP];  /* weight of phasor for each ap */
                                                          /* (depends on # sbands) */
        double          seg_amp[MAX_CHAN_PP+1][MAXAP];  /* Amplitude by channel/segment */
        double          seg_phs[MAX_CHAN_PP+1][MAXAP];  /* Phase by channel/segment */
        double          mean_ap[MAX_CHAN_PP+1][MAXAP];  /* Average AP number for data in segment */
        double          seg_frac_usb[MAX_CHAN_PP+1][MAXAP]; /* Fraction of expected data in seg */
        double          seg_frac_lsb[MAX_CHAN_PP+1][MAXAP]; /* Fraction of expected data in seg */
        double          seg_referr[MAX_CHAN_PP+1][MAXAP]; /* Averaged tape error rates */
        double          seg_remerr[MAX_CHAN_PP+1][MAXAP]; /* Averaged tape error rates */
        double          seg_refscnt_usb[MAX_CHAN_PP+1][MAXAP]; /* Seg averaged state counts */
        double          seg_refscnt_lsb[MAX_CHAN_PP+1][MAXAP]; /* Seg averaged state counts */
        double          seg_remscnt_usb[MAX_CHAN_PP+1][MAXAP]; /* Seg averaged state counts */
        double          seg_remscnt_lsb[MAX_CHAN_PP+1][MAXAP]; /* Seg averaged state counts */
        double          seg_refbias_usb[MAX_CHAN_PP+1][MAXAP]; /* Seg averaged count bias */
        double          seg_refbias_lsb[MAX_CHAN_PP+1][MAXAP]; /* Seg averaged count bias */
        double          seg_rembias_usb[MAX_CHAN_PP+1][MAXAP]; /* Seg averaged count bias */
        double          seg_rembias_lsb[MAX_CHAN_PP+1][MAXAP]; /* Seg averaged count bias */
        double          seg_refpcal[MAX_CHAN_PP+1][MAXAP]; /* seg averaged phasecal phase */
        double          seg_rempcal[MAX_CHAN_PP+1][MAXAP]; /* seg averaged phasecal phase */
        };
