#ifndef T205_VERSION
#define T205_VERSION 1

#include "mk4_typedefs.h"


                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_205_v1 type_205

struct type_205_v0
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    struct date         utc_central;            /* Central time of scan */
    float               offset;                 /* Offset of FRT from scan ctr sec */
    char                ffmode[8];              /* Fourfit execution modes */
    float               search[6];              /* SBD, MBD, rate search windows */
                                                /* (usec, usec, usec/sec) */
    float               filter[8];              /* Various filter thresholds */
    struct date         start;                  /* Start of requested data span */
    struct date         stop;                   /* End of requested data span */
    double              ref_freq;               /* Fourfit reference frequency Hz */
    struct
        {
        char            ffit_chan_id;           /* Fourfit channel letter id */
        char            unused;                 /* Alignment padding */
        short           channels[4];            /* Indices into type 203 */
        } ffit_chan[16];                        /* Fourfit channel id info */
    };

struct type_205 
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    struct date         utc_central;            /* Central time of scan */
    float               offset;                 /* Offset of FRT from scan ctr sec */
    char                ffmode[8];              /* Fourfit execution modes */
    float               search[6];              /* SBD, MBD, rate search windows */
                                                /* (usec, usec, usec/sec) */
    float               filter[8];              /* Various filter thresholds */
    struct date         start;                  /* Start of requested data span */
    struct date         stop;                   /* End of requested data span */
    double              ref_freq;               /* Fourfit reference frequency Hz */
    struct
        {
        char            ffit_chan_id;           /* Fourfit channel letter id */
        char            unused;                 /* Alignment padding */
        short           channels[4];            /* Indices into type 203 */
        } ffit_chan[64];                        /* Fourfit channel id info */
    };

#endif
