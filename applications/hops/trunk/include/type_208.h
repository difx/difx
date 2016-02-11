#ifndef T208_VERSION
#define T208_VERSION 1


                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_208_v1 type_208

struct type_208_v0
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    char                quality;                /* Fringe quality 0 to 9 */
    char                errcode;                /* A to F, maybe others */
    char                tape_qcode[6];          /* For A-file backward compat. */
    double              adelay;                 /* Apriori delay at FRT (usec) */
    double              arate;                  /* Apriori rate at FRT (usec/sec) */
    double              aaccel;                 /* Apriori accel at FRT (usec/sec^2) */
    double              tot_mbd;                /* Total observed MBD (usec) */
    double              tot_sbd;                /* Total observed SBD (usec) */
    double              tot_rate;               /* Total observed rate (usec/sec) */
    double              tot_mbd_ref;            /* Total observed MBD (usec) at ref stn epoch */
    double              tot_sbd_ref;            /* Total observed SBD (usec) at ref stn epoch */
    double              tot_rate_ref;           /* Total observed rate (usec/sec) at ref stn epoch */
    float               resid_mbd;              /* MBD residual to model (usec) */
    float               resid_sbd;              /* SBD residual to model (usec) */
    float               resid_rate;             /* Rate residual to model (usec/sec) */
    float               mbd_error;              /* MBD error calc'd from data (usec) */
    float               sbd_error;              /* SBD error calc'd from data (usec) */
    float               rate_error;             /* Rate error calc'd from data (usec/sec) */
    float               ambiguity;              /* MBD ambiguity (usec) */
    float               amplitude;              /* Coherent amplitude (corr. coeff.) */
    float               inc_seg_ampl;           /* Incoherent segment addition amp. */
    float               inc_chan_ampl;          /* Incoherent channel addition amp. */
    float               snr;                    /* SNR in sigmas */
    float               prob_false;             /* Probability of false detection */
    float               totphase;               /* Total observed fringe phase (deg) */
    float               resphase;               /* Residual earth-centered phase (deg) */
    };

struct type_208 
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    char                quality;                /* Fringe quality 0 to 9 */
    char                errcode;                /* A to F, maybe others */
    char                tape_qcode[6];          /* For A-file backward compat. */
    double              adelay;                 /* Apriori delay at FRT (usec) */
    double              arate;                  /* Apriori rate at FRT (usec/sec) */
    double              aaccel;                 /* Apriori accel at FRT (usec/sec^2) */
    double              tot_mbd;                /* Total observed MBD (usec) */
    double              tot_sbd;                /* Total observed SBD (usec) */
    double              tot_rate;               /* Total observed rate (usec/sec) */
    double              tot_mbd_ref;            /* Total observed MBD (usec) at ref stn epoch */
    double              tot_sbd_ref;            /* Total observed SBD (usec) at ref stn epoch */
    double              tot_rate_ref;           /* Total observed rate (usec/sec) at ref stn epoch */
    float               resid_mbd;              /* MBD residual to model (usec) */
    float               resid_sbd;              /* SBD residual to model (usec) */
    float               resid_rate;             /* Rate residual to model (usec/sec) */
    float               mbd_error;              /* MBD error calc'd from data (usec) */
    float               sbd_error;              /* SBD error calc'd from data (usec) */
    float               rate_error;             /* Rate error calc'd from data (usec/sec) */
    float               ambiguity;              /* MBD ambiguity (usec) */
    float               amplitude;              /* Coherent amplitude (corr. coeff.) */
    float               inc_seg_ampl;           /* Incoherent segment addition amp. */
    float               inc_chan_ampl;          /* Incoherent channel addition amp. */
    float               snr;                    /* SNR in sigmas */
    float               prob_false;             /* Probability of false detection */
    float               totphase;               /* Total observed fringe phase (deg) */
    float               totphase_ref;           /* Total phase at ref stn epoch */
    float               resphase;               /* Residual earth-centered phase (deg) */
    float               tec_error;              // std dev of tec estimate (TEC units)
    };

#endif

