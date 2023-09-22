#ifndef T202_VERSION
#define T202_VERSION 0

                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_202_v0 type_202

struct type_202 
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    char                baseline[2];            /* 2-char baseline ID */
    char                ref_intl_id[2];         /* Reference station int'l ID */
    char                rem_intl_id[2];         /* Reference station int'l ID */
    char                ref_name[8];            /* Reference station name */
    char                rem_name[8];            /* Remote station name */
    char                ref_tape[8];            /* Reference station tape VSN */
    char                rem_tape[8];            /* Remote station tape VSN */
    short               nlags;                  /* # lags used for correlation */
    double              ref_xpos;               /* Station X-coord (meters) */
    double              rem_xpos;               /* Station X-coord (meters) */
    double              ref_ypos;               /* Station Y-coord (meters) */
    double              rem_ypos;               /* Station Y-coord (meters) */
    double              ref_zpos;               /* Station Z-coord (meters) */
    double              rem_zpos;               /* Station Z-coord (meters) */
    double              u;                      /* Fringes/arcsec E-W 1GHz */
    double              v;                      /* Fringes/arcsec N-S 1GHz */
    double              uf;                     /* mHz/arcsec/GHz in R.A. */
    double              vf;                     /* mHz/arcsec/GHz in dec. */
    float               ref_clock;              /* Ref station clock (usec) */
    float               rem_clock;              /* Rem station clock (usec) */
    float               ref_clockrate;          /* Ref clockrate (sec/sec) */
    float               rem_clockrate;          /* Rem clockrate (sec/sec) */
    float               ref_idelay;             /* Ref station instr. delay (usec) */
    float               rem_idelay;             /* Rem station instr. delay (usec) */
    float               ref_zdelay;             /* Ref station z.atm. delay (usec) */
    float               rem_zdelay;             /* Rem station z.atm. delay (usec) */
    float               ref_elev;               /* Elevation at ref. antenna (deg) */
    float               rem_elev;               /* Elevation at rem. antenna (deg) */
    float               ref_az;                 /* Azimuth at ref. antenna (deg) */
    float               rem_az;                 /* Azimuth at rem. antenna (deg) */
    };

#endif
