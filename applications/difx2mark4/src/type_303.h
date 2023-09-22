#ifndef T303_VERSION
#define T303_VERSION 0

#include "mk4_typedefs.h"

                    /* Set this to current version, */
                    /* defined to be same as app struct */
#define type_303_v0 type_303

struct type_303
    {
    char        record_id[3];           /* Standard 3-digit id */
    char        version_no[2];          /* Standard 2-digit version # */
    char        unused1[3];             /* Reserved space */
    short   interval;                   /* Sequential model interval number */
    char    chan_id[32];                /* Frequency channel identifier */
    char    unused2[6];                 /* Padding */
    double  azimuth[6];                 // Azimuth (deg) coefficients
    double  elevation[6];               // Elevation (deg) coefficients
    double parallactic_angle[6];        // Par. angle (deg CCW el line from RA line)
    double u[6];                        // Baseline projections toward source (m)
    double v[6];
    double w[6];
    };

void write_t303 (struct type_303 *, FILE *);
#endif
