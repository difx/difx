#ifndef T300_VERSION
#define T300_VERSION 0

#include "mk4_typedefs.h"

                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_300_v0 type_300

struct type_300
    {
    char        record_id[3];           /* Standard 3-digit id */
    char        version_no[2];          /* Standard 2-digit version # */
    char        unused1[2];             /* Reserved space */
    U8          SU_number;              /* Station unit, filled by suman */
    char        id;                     /* 1-char vex letter code */
    char        intl_id[2];             /* 2-char international id code */
    char        name[32];               /* Full station name, null-term. */
    char        unused2;                /* Padding */
    struct date model_start;            /* Start time for 1st spline */
    float       model_interval;         /* Spline interval secs (rec time) */
    short       nsplines;               /* Number of splines in scan */
    };

#endif
