#ifndef T101_VERSION
#define T101_VERSION 0

#include "mk4_typedefs.h"

                                       /* Set this to current version, */
                                       /* defined to be same as app struct */
#define type_101_v0 type_101

struct type_101 
    {
    char         record_id[3];          /* Standard 3-digit id */
    char         version_no[2];         /* Standard 2-digit version # */
    char         status;                /* Reserved space */
    short        nblocks;               /* Needed up front for IO library */
    short        index;                 /* Index number */
    short        primary;               /* Index number of primary 101 */
    char         ref_chan_id[8];        /* Ref station channel id */
    char         rem_chan_id[8];        /* Rem station channel id */
    short        corr_board;            /* Correlator board serial # */
    short        corr_slot;             /* Correlator board slot */
    short        ref_chan;              /* Ref station SU channel number */
    short        rem_chan;              /* Rem station SU channel number */
    int          post_mortem;           /* 32 1-bit flags */
    int          blocks[1];             /* One entry per block in snake */
    };

void write_t101 (struct type_101 *, FILE *);
#endif
