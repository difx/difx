#ifndef T203_VERSION
#define T203_VERSION 1

#include "mk4_sizes.h"
                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_203_v1 type_203


/* Note: 12/16/16
The element 'sample_rate' in the ch_struct below is not currently used
in the HOPS code. However, it is calculated and filled in the function fill_203.c.
This function does the calculation with double precision but then casts the result
to a short, leading to an overflow error. To fix this properly would require 
changing the data type of 'sample_rate' to a double. This would require re-padding
this type for alignment though, so, since sample_rate isn't actually used anywhere,
for the time being the type_203 will be left as is.
*/

struct ch_struct
    {
    short               index;                  /* Index from type-1 file (t101) */
    unsigned short int  sample_rate;            // Ksamp/sec (has max of 65.536 MSamp/s)
    char                refsb;                  /* Ref ant sideband (U/L) */
    char                remsb;                  /* Rem ant sideband (U/L) */
    char                refpol;                 /* Ref ant polarization (R/L) */
    char                rempol;                 /* Rem ant polarization (R/L) */
    double              ref_freq;               /* Sky freq at ref station (MHz) */
    double              rem_freq;               /* Sky freq at rem station (MHz) */
    char                ref_chan_id[8];         /* Ref station channel ID */
    char                rem_chan_id[8];         /* Rem station channel ID */
    };

struct type_203_v0
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    struct ch_struct    channels[32];           /* channel-by-channel info */
    };

struct type_203 
    {
    char                record_id[3];           /* Standard 3-digit id */
    char                version_no[2];          /* Standard 2-digit version # */
    char                unused1[3];             /* Reserved space */
    struct ch_struct    channels[8*MAXFREQ];    /* channel-by-channel info */
    };

#endif
