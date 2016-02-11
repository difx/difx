#ifndef T203_VERSION
#define T203_VERSION 1

#include "mk4_sizes.h"
                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_203_v1 type_203

struct ch_struct
    {
    short               index;                  /* Index from type-1 file (t101) */
    short               sample_rate;            /* Kilosamples/sec */
    char                refsb;                  /* Ref ant sideband (U/L) */
    char                remsb;                  /* Rem ant sideband (U/L) */
    char                refpol;                 /* Ref ant polarization (R/L) */
    char                rempol;                 /* Rem ant polarization (R/L) */
    double              ref_freq;               /* Sky freq at ref station (MHz) */
    double              rem_freq;               /* Sky freq at rem station (MHz) */
    char                ref_chan_id[8];         /* Ref station channel ID */
    char                rem_chan_id[8];         /* Rem station channel ID */
    };

struct type_203_v0
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    struct ch_struct    channels[32];           /* channel-by-channel info */
    };

struct type_203 
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    struct ch_struct    channels[8*MAXFREQ];    /* channel-by-channel info */
    };

#endif
