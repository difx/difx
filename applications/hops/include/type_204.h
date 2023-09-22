#ifndef T204_VERSION
#define T204_VERSION 0

#include "mk4_typedefs.h"


                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_204_v0 type_204

struct type_204 
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    short               ff_version[2];          /* Fourfit revision level */
    char                platform[8];            /* hppa, linux, alpha etc */
    char                control_file[96];       /* Control file full pathname */
    struct date         ffcf_date;              /* Control file mod. date */
    char                override[128];          /* Command line override string */
    };

#endif
