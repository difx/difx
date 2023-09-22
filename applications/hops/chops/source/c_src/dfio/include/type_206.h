#ifndef T206_VERSION
#define T206_VERSION 2

#include "mk4_typedefs.h"


                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_206_v2 type_206

struct sidebands
    {
    short               lsb;
    short               usb;
    };

struct sbweights
    {
    double              lsb;
    double              usb;
    };

struct type_206_v0
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    struct date         start;                  /* Time at start of AP zero */
    short               first_ap;               /* Number of 1st valid AP */
    short               last_ap;                /* Number of last valid AP */
    struct sidebands    accepted[16];           /* APs accepted by chan/sband */
    float               intg_time;              /* Effective integration time (sec) */
    float               accept_ratio;           /* % ratio min/max data accepted */
    float               discard;                /* % data discarded */
    struct sidebands    reason1[16];            /* APs filtered by chan/sband */
    struct sidebands    reason2[16];            /* APs filtered by chan/sband */
    struct sidebands    reason3[16];            /* APs filtered by chan/sband */
    struct sidebands    reason4[16];            /* APs filtered by chan/sband */
    struct sidebands    reason5[16];            /* APs filtered by chan/sband */
    struct sidebands    reason6[16];            /* APs filtered by chan/sband */
    struct sidebands    reason7[16];            /* APs filtered by chan/sband */
    struct sidebands    reason8[16];            /* APs filtered by chan/sband */
    short               ratesize;               /* Size of fringe rate transform */
    short               mbdsize;                /* Size of MBD transform */
    short               sbdsize;                /* Size of SBD transform */
    char                unused2[6];             /* Padding */
    };

struct type_206_v1
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    struct date         start;                  /* Time at start of AP zero */
    short               first_ap;               /* Number of 1st valid AP */
    short               last_ap;                /* Number of last valid AP */
    struct sidebands    accepted[16];           /* APs accepted by chan/sband */
    struct sbweights    weights[16];            /* Samples per channel/sideband */
    float               intg_time;              /* Effective integration time (sec) */
    float               accept_ratio;           /* % ratio min/max data accepted */
    float               discard;                /* % data discarded */
    struct sidebands    reason1[16];            /* APs filtered by chan/sband */
    struct sidebands    reason2[16];            /* APs filtered by chan/sband */
    struct sidebands    reason3[16];            /* APs filtered by chan/sband */
    struct sidebands    reason4[16];            /* APs filtered by chan/sband */
    struct sidebands    reason5[16];            /* APs filtered by chan/sband */
    struct sidebands    reason6[16];            /* APs filtered by chan/sband */
    struct sidebands    reason7[16];            /* APs filtered by chan/sband */
    struct sidebands    reason8[16];            /* APs filtered by chan/sband */
    short               ratesize;               /* Size of fringe rate transform */
    short               mbdsize;                /* Size of MBD transform */
    short               sbdsize;                /* Size of SBD transform */
    char                unused2[6];             /* Padding */
    };

struct type_206
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    struct date         start;                  /* Time at start of AP zero */
    short               first_ap;               /* Number of 1st valid AP */
    short               last_ap;                /* Number of last valid AP */
    struct sidebands    accepted[64];           /* APs accepted by chan/sband */
    struct sbweights    weights[64];            /* Samples per channel/sideband */
    float               intg_time;              /* Effective integration time (sec) */
    float               accept_ratio;           /* % ratio min/max data accepted */
    float               discard;                /* % data discarded */
    struct sidebands    reason1[64];            /* APs filtered by chan/sband */
    struct sidebands    reason2[64];            /* APs filtered by chan/sband */
    struct sidebands    reason3[64];            /* APs filtered by chan/sband */
    struct sidebands    reason4[64];            /* APs filtered by chan/sband */
    struct sidebands    reason5[64];            /* APs filtered by chan/sband */
    struct sidebands    reason6[64];            /* APs filtered by chan/sband */
    struct sidebands    reason7[64];            /* APs filtered by chan/sband */
    struct sidebands    reason8[64];            /* APs filtered by chan/sband */
    short               ratesize;               /* Size of fringe rate transform */
    short               mbdsize;                /* Size of MBD transform */
    short               sbdsize;                /* Size of SBD transform */
    char                unused2[6];             /* Padding */
    };

#endif
