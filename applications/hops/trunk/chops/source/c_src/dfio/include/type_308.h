#ifndef T308_VERSION
#define T308_VERSION 0

#include "mk4_typedefs.h"

                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_308_v0 type_308

struct type_308
    {
    char        record_id[3];           /* Standard 3-digit id */
    char        version_no[2];          /* Standard 2-digit version # */
    char        unused1[3];             /* Reserved space */
    struct date time;                   /* Start time for this span of data */
    float       duration;               /* Duration of this data span (sec) */
    struct
        {
        char    chan_id[8];             /* Channel identifier */
        float   frequency;              /* Hz relative to channel freq */
        float   real;
        float   imaginary;
        } pcal[32];                     /* Detected phasecal info */
    };

#endif
