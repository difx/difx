/*
 * $Id: sc_stats.h 4454 2017-09-29 15:33:21Z gbc $
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
    uint32_t    channel_mask;   /* restriction on channels */
    uint32_t    channel_bits;   /* count of such channels */
} BSInfo;

/*
 * A diagnostic method to describe the BSInfo contents: stdout or buffer.
 * If lab is not NULL, it is inserted at the beginning of each line.
 */
extern void stats_report(BSInfo *bsi, char *label);
extern char *stats_repstr(BSInfo *bsi, char *label);
extern void stats_chmask(BSInfo *bsi, char *csv);

/* Accumulator of statistics on the packet data starting with optr */
extern void stats_check_2bits(BSInfo *bsi, uint64_t *optr);
extern void stats_check_1bit(BSInfo *bsi, uint64_t *optr);
extern void stats_check(BSInfo *bsi, uint64_t *optr);

/* Delta stats module */
extern void stats_delta(BSInfo *bsi, BSInfo *lst, BSInfo *del,
                        uint32_t *pkt, int count, int fnum, void *start);
extern char *stats_timestamp(uint32_t *pkt);
void stats_set_packet_rate(unsigned long newrate);
unsigned long stats_get_packet_rate(void);

#endif /* sc_stats_h */

/*
 * eof
 */
