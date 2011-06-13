#ifndef TYPE_4400_H
#define TYPE_4400_H

struct type_4400 {
        short           record_id;              /* record identifier */
        short           baseline_no;            /* baseline # as defined in root */
        short           spare[2];               /*  */
        double          lo_freqs[14];           /* LO frequencies by channel */
        double          ref_freq;               /* RF freq (MHz) to reference phase */
        double          delay_obs;              /* Observed group delay usec */
        double          rate_obs;               /* Observed delay rate usec/sec */
        double          nb_delay;               /* Narrow band delay usec */
        double          gd_ambig;               /* group_delay ambiguity usec */
        double          clock;                  /* Apriori clock usec */
        double          ref_clock_epoch;        /* Reference stat. clock epoch usec */
        double          delay_obs_c;            /* Observed delay central epoch */
        double          rate_obs_c;             /* Observed delay rate central epoch */
        double          delayplus1;             /* phase delay at epoch + 1sec */
        double          delayminus1;            /* phase delay at epoch - 1sec */
        double          rate_error;             /* Delay rate error calc. from data */
        float           azimuth[2];             /* Ref/remote azimuths (temporary */
                                                /* for fourfit) */
        double          amp_corr_fact;          /* Used for mm-VLBI */
        double          unused[3];
};
        
#endif
