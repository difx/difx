#ifndef T100_VERSION
#define T100_VERSION 0

#include "mk4_typedefs.h"

                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_100_v0 type_100

struct type_100 
    {
    char         record_id[3];          /* Standard 3-digit id */
    char         version_no[2];         /* Standard 2-digit version # */
    char         unused1[3];            /* Reserved space */
    struct date  procdate;              /* Correlation time */
    char         baseline[2];           /* Standard baseline id */
    char         rootname[34];          /* Root filename, null-terminated */
    char         qcode[2];              /* Quality code of correlation */
    char         unused2[6];            /* Padding */
    float        pct_done;              /* 0-100% of scheduled data processed */
    struct date  start;                 /* Time of first AP */
    struct date  stop;                  /* Time of last AP */
    int          ndrec;                 /* Number of data records */
    int          nindex;                /* Number of index numbers present */
    short        nlags;                 /* # of lags in a type_120 record */
    short        nblocks;               /* # blocks per index number */
    };

#endif
