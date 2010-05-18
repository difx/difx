#ifndef T301_VERSION
#define T301_VERSION 0

#include "mk4_typedefs.h"

					/* Set this to current version, */
					/* defined to be same as app struct */
#define type_301_v0 type_301

struct type_301
    {
    char        record_id[3];           /* Standard 3-digit id */
    char        version_no[2];          /* Standard 2-digit version # */
    char        unused1[3];             /* Reserved space */
    short	interval;		/* Sequential model interval number */
    char	chan_id[32];		/* Frequency channel identifier */
    char	unused2[6];		/* Padding */
    double	delay_spline[6];	/* Delay spline coefficients */
    };

#endif
