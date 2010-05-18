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
/* FORMAT TO BE DETERMINED */
    };

#endif
