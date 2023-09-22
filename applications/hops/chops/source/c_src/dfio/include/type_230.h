#ifndef T230_VERSION
#define T230_VERSION 0

#include "mk4_typedefs.h"
#include "hops_complex.h"
                                        /* Set this to current version, */
                                        /* defined to be same as app struct */
#define type_230_v0 type_230

struct type_230
    {
    char            record_id[3];       /* Standard 3-digit id */
    char            version_no[2];      /* Standard 2-digit version # */
    char            unused1;
    short           nspec_pts;          /* Needed by IO library */
/*    char            rootcode[6];         Root suffix */
/*    char            unused2[2]; */
    int             frq;                /* Index into type 205 */
    int             ap;                 /* AP number, refer to 206 */
    float           lsbweight;          /* fraction of AP represented */
    float           usbweight;          /* fraction of AP represented */
    hops_scomplex    xpower[1];          /* Array of spectrum values */
    };

#endif
