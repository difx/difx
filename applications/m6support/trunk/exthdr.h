/*
 * $Id: exthdr.h 2562 2014-10-09 16:39:23Z gbc $
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
    uint32_t type;      /* of ver2 ext header */
    uint32_t filecnt;   /* number of files examined */
    long int mask;      /* things to ignore */
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
#define ALMA_EXT_HDR    0xA5AE50
extern void alma_hdr_help(void);
extern void alma_hdr_chk(const int id, const uint32_t status,
    const uint32_t frame);
extern void alma_hdr_sum(FILE *fp);

/*
 * eof
 */
