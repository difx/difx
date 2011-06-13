#ifndef T200_VERSION
#define T200_VERSION 0

#include "mk4_typedefs.h"

                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_200_v0 type_200

struct type_200
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    short               software_rev[10];       /* Revision levels for online progs */
    int                 expt_no;                /* Experiment number */
    char                exper_name[32];         /* Observing program name */
    char                scan_name[32];          /* Scan label from OVEX */
    char                correlator[8];          /* Correlator identification */
    struct date         scantime;               /* Scan time to 1 second */
    int                 start_offset;           /* Nom. bline start rel. to scantime (s) */
    int                 stop_offset;            /* Nom. bline stop rel. to scantime (s) */
    struct date         corr_date;              /* Time of correlation */
    struct date         fourfit_date;           /* Time of fourfit processing */
    struct date         frt;                    /* Fourfit reference time */
    };

#endif
