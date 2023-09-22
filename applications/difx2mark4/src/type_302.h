#ifndef T302_VERSION
#define T302_VERSION 0

#include "mk4_typedefs.h"

					/* Set this to current version, */
					/* defined to be same as app struct */
#define type_302_v0 type_302

struct type_302
    {
    char        record_id[3];           /* Standard 3-digit id */
    char        version_no[2];          /* Standard 2-digit version # */
    char        unused1[3];             /* Reserved space */
    short	interval;		/* Sequential model interval number */
    char	chan_id[32];		/* Frequency channel identifier */
    char	unused2[6];		/* Padding */
    double	phase_spline[6];	/* Phase spline coefficients */
    };

void write_t302 (struct type_302 *, FILE *);
#endif
