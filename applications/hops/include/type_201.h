#ifndef T201_VERSION
#define T201_VERSION 0

#include "mk4_typedefs.h"

                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_201_v0 type_201

struct type_201 
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    char                source[32];             /* Source name from OVEX */
    struct sky_coord    coord;                  /* Source coordinates */
    short               epoch;                  /* 1950 or 2000 */
    char                unused2[2];             /* Padding */
    struct date         coord_date;             /* Date of coordinate meas. */
    double              ra_rate;                /* Proper motion (rad/sec) */
    double              dec_rate;               /* Proper motion (rad/sec) */
    double              pulsar_phase[4];        /* Polynomial coeffs for timing */
    double              pulsar_epoch;           /* Reference time for polynomial */
    double              dispersion;             /* Pulsar dispersion measure */
    };

#endif
