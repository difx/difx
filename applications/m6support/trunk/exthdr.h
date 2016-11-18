/*
 * $Id: exthdr.h 4135 2016-09-09 21:07:15Z gbc $
 *
 * Support for extended headers
 */

#include <stdio.h>
#include <stdint.h>

/* work area */
extern struct ext_hdr_work {
    /* globals */
    int verb;           /* verbosity */
    FILE *fp;           /* output file */
    int typeset;        /* type was asserted:*/
    uint32_t typeval;   /* of ver2 ext header */
    uint32_t type;      /* of ver2 ext header */
    uint32_t filecnt;   /* number of files examined */
    long int mask;      /* things to ignore */
    int jump;           /* how to report time */
    /* used by search */
    double last_datum;  /* last pps read */
    uint32_t last_frame;/* and frame */
    uint32_t last_valid;/* and validity */
    /* per file */
    uint32_t pktcnt;
    uint32_t secsre;    /* secs of ref epoch */
    /* ALMA data */
    int id, pers;
    uint32_t flags;     /* status bits */
                        /* cnt, sum(->ave), dev */
    uint64_t gps_pic_pps[3];
    uint64_t maser_pic_pps[3];
    uint64_t te_pic_pps[3];
    double fpga_degc;
    /* R2DBE v0 data */
    int pol;
    double r2dbe_gps_pps[3];
    /* add others here */
} ext_hdr_work;

/* prepare for extended headers using option string */
extern int extended_hdr_opt(const char *opt);
extern void extended_hdr_verb(const int verb, const char *name);

/* called every packet to examine extended header */
extern void extended_hdr_chk(const uint32_t *pkt);

/* called when done to print summary information */
extern void extended_hdr_sum(const uint32_t opt);

#define EXT_HDR_STAMP   "%u+%-6u"

/* per-dbe-flavor support */
#define ALMA_EXT_HDR        0xA5AE50
extern void alma_hdr_help(void);
extern void alma_hdr_chk(const int id, const uint32_t status,
    const uint32_t frame);
extern void alma_hdr_sum(FILE *fp);

#define R2DBEv0_EXT_HDR     0x000000
extern void r2dbev0_hdr_help(void);
extern void r2dbev0_hdr_chk(const int id, const uint32_t status,
    const uint32_t frame);
extern void r2dbev0_hdr_sum(FILE *fp);

/*
 * Datum for a pair of packets with a GPS PPS gap between them
 * bigger and lesser refer to offsets within the file.
 */
typedef struct srch_stack {
    uint32_t    lesser;
    double      ldatum;
    uint32_t    bigger;
    double      bdatum;
    uint32_t    pkt[2];
} SrchStack;

/*
 * At this offset of the sequence, the GPS PPS measurement.
 */
typedef struct srch_data {
    int64_t     offset;
    double      zdatum;
    uint32_t    count;
} SrchData;

/*
 * Everything for the search.
 */
#define EXT_SS_BEGIN    0
#define EXT_SS_FIRST    1
#define EXT_SS_FINAL    2
#define EXT_SS_SEARCH   3
#define EXT_SS_FINISH   4
#define EXT_SS_SORTED   5
#define SRCH_ALLOC      1000
typedef struct srch_state {
    int         where;              /* 0 at startup */
    double      maxgap;             /* work.pps_search */
    double      last_datum;         /* last datum in history */
    uint32_t    last_pkt[2];        /* last packet times */
    char        lab[64];            /* for output */
    uint32_t    srch_first;         /* starter offset */
    uint32_t    srch_final;         /* final offset */
    uint32_t    srch_next;          /* next value of pkts_seqoffset */
    uint32_t    srch_midway;        /* direction of invalid stepping */

    uint32_t    srch_s_read;        /* read  point into stack */
    uint32_t    srch_swrite;        /* write point into stack */
    uint32_t    srch_salloc;        /* size of allocation */
    SrchStack   *srch_stack;        /* stack of search pairs */
    uint32_t    srch_halloc;         /* size of allocation */
    uint32_t    srch_hwrite;        /* write point into history */
    SrchData    *srch_history;      /* binary search history */
} SrchState;

extern int extended_hdr_search(uint32_t *pkt, SrchState *wss);
extern void extended_report(char *lab, SrchState *wss);

/*
 * eof
 */
