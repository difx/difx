/*
 * $Id: sc_stats.h 1694 2013-12-27 20:31:27Z gbc $
 *
 * Statistics checker for scan check
 */

#ifndef sc_stats_h
#define sc_stats_h

#include <stdint.h>

/*
 * A structure to hold working data
 */
typedef struct bstats_info {
    uint64_t    bpkts;          /* number of packets visited */
    uint64_t    bcounts;        /* total of bstates */
    uint64_t    bstates[4];     /* 00 01 10 11 counters */
    uint32_t    packet_octets;  /* octets in the packet */
    uint32_t    bits_sample;    /* bits per sample, normally 2 */
} BSInfo;

/*
 * A diagnostic method to describe the BSInfo contents: stdout or buffer.
 * If lab is not NULL, it is inserted at the beginning of each line.
 */
extern void stats_report(BSInfo *bsi, char *label);
extern char *stats_repstr(BSInfo *bsi, char *label);

/* Accumulator of statistics on the packet data starting with optr */
extern void stats_check_2bits(BSInfo *bsi, uint64_t *optr);
extern void stats_check_1bit(BSInfo *bsi, uint64_t *optr);
extern void stats_check(BSInfo *bsi, uint64_t *optr);

#endif /* sc_stats_h */

/*
 * eof
 */
