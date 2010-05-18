#ifndef T305_VERSION
#define T305_VERSION 0

#include "mk4_typedefs.h"

					/* Set this to current version, */
					/* defined to be same as app struct */
#define type_305_v0 type_305

struct type_305
    {
    char        record_id[3];           /* Standard 3-digit id */
    char        version_no[2];          /* Standard 2-digit version # */
    char        unused1[3];             /* Reserved space */
/* FORMAT TO BE DETERMINED */
    };

#endif
