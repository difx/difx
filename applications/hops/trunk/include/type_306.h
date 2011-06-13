#ifndef T306_VERSION
#define T306_VERSION 0

#include "mk4_typedefs.h"

                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_306_v0 type_306

struct type_306
    {
    char        record_id[3];           /* Standard 3-digit id */
    char        version_no[2];          /* Standard 2-digit version # */
    char        unused1[3];             /* Reserved space */
    struct date time;                   /* Start time for this span of data */
    float       duration;               /* Duration of this data span (sec) */
    struct
        {
        char    chan_id[32];            /* Channel identifier */
        int     bigpos;                 /* state counts by voltage bin */
        int     pos;                    /* 1-bit samples use only "pos" */
        int     neg;                    /* and "neg" */
        int     bigneg;
        } stcount[16];
    };

#endif
