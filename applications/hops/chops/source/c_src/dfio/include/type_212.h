#ifndef T212_VERSION
#define T212_VERSION 1

#include "mk4_typedefs.h"
                                    /* Set this to current version, */
                                    /* defined to be same as app struct */
#define type_212_v1 type_212

struct phasor
    {
    float amp;
    float phase;
    };

struct newphasor
    {
    float amp;
    float phase;
    float weight;                   /* Requested by L. Petrov */
    };

struct type_212_v0
    {
    char            record_id[3];   /* Standard 3-digit id */
    char            version_no[2];  /* Standard 2-digit version # */
    char            unused;
    short           nap;            /* Needed by IO library */
    short           first_ap;       /* Number of first ap in record */
    short           channel;        /* fourfit channel number */
    short           sbd_chan;       /* Singleband delay channel */
    char            unused2[2];
    struct phasor   data[1];        /* data values, variable length array */
    };

struct type_212
    {
    char            record_id[3];   /* Standard 3-digit id */
    char            version_no[2];  /* Standard 2-digit version # */
    char            unused;
    short           nap;            /* Needed by IO library */
    short           first_ap;       /* Number of first ap in record */
    short           channel;        /* fourfit channel number */
    short           sbd_chan;       /* Singleband delay channel */
    char            unused2[2];
    struct newphasor data[1];        /* data values, variable length array */
    };

#endif
