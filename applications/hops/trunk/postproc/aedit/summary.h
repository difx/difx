#define TRUE 1
#define FALSE 0

#define VERSION 0                       /* These determine how much work is done */
#define STANDARD 1                      /* by summ_data().  They describe the mode */
#define CLOSURE 2                       /* argument */

#define NONE     0                      /* For alloc_btq */
#define BASELINE 1
#define TRIANGLE 2
#define QUAD     3

#include "sizelimits.h"

struct source_info {
        int     count;
        char    name[32];
};

typedef struct {                        /* One of these per source/experiment/frequency */
        char                    name[32];
        int                     count;
        int                     begin;
        int                     end;
        char                    stations[MAXSTEXP + 1];
        int                     nbtq;   /* "btq" entities can be baselines, */
        char                    *btq;   /* triangles or quads, and are dealt */
                                        /* with dynamically as such according */
                                        /* to type of summary structure they */
                                        /* occur in */
        int                     btq_allocated;
        int                     qcodes[20];
        float                   snrmin;
        float                   snrmax;
} srcsum;

struct frqexp {
        char                    freq_code;
        int                     expt_no;
        int                     begin;
        int                     end;
        char                    stations[MAXSTEXP + 1];
        int                     nbtq;
        char                    *btq;
        int                     btq_allocated;
        int                     nsource;
        srcsum                  *slist;         /* Pointer to all src this exp/freq */
        int                     slist_allocated;
};

struct datasumm {
        int                     begin;  /* Seconds since 0000 Jan 1 1980 */
        int                     end;
        int                     proc_begin;
        int                     proc_end;
        char                    stations[MAXSTTOT];
        int                     nbtq;
        char                    *btq;
        int                     btq_allocated;
        char                    frequencies[MAXBANDS];
        char                    polarizations[49];
        int                     experiments[MAXEXPTS];
        int                     nexp;
        int                     qcodes[20];
        int                     version[MAXVERSION+1];
        float                   snrmin;
        float                   snrmax;
        struct source_info      source[MAXSRC];
        int                     nsource;
        struct frqexp           fqex[MAXBANDS * MAXEXPTS];
        int                     nfqex;
};
