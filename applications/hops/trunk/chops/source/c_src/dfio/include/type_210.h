#ifndef T210_VERSION
#define T210_VERSION 1


                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_210_v1 type_210

struct polars
    {
    float               ampl;                   /* Correlation coefficient */
    float               phase;                  /* Degrees */
    };

struct type_210_v0
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    struct polars       amp_phas[16];           /* Residual fringe amp/phase */
    };

struct type_210 
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    struct polars       amp_phas[64];           /* Residual fringe amp/phase */
    };

#endif

