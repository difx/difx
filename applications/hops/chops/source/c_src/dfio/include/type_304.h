#ifndef T304_VERSION
#define T304_VERSION 0

#include "mk4_typedefs.h"

                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_304_v0 type_304

struct type_304
    {
    char        record_id[3];           /* Standard 3-digit id */
    char        version_no[2];          /* Standard 2-digit version # */
    char        unused1[3];             /* Reserved space */
    struct date time;                   /* Start time for this span of data */
    float       duration;               /* Duration of this data span (sec) */
    struct
        {
        int     error_count;            /* Parity error count */
        int     frames;                 /* Count of frames */
        int     bad_frames;             /* Count of bad frames */
        int     slip_sync;              /* Count of slip syncs */
        int     missing_sync;           /* Count of missing syncs */
        int     crc_error;              /* Count of CRC errors */
        } trackstats[64];
    };

#endif
