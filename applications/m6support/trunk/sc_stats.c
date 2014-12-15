/*
 * $Id: sc_stats.c 1726 2014-01-02 22:05:47Z gbc $
 *
 * Statistics checker for scan check
 */

#include <stdio.h>
#include "sc_stats.h"

/*
 * A diagnostic method to describe the BSInfo contents: stdout or buffer.
 * If lab is not NULL, it is inserted at the beginning of each line.
 */
char *stats_repstr(BSInfo *bsi, char *label)
{
    static char buf[1024];
    char *lab = label ? label : "";
    if (!bsi->bcounts) snprintf(buf, sizeof(buf),
        "%s%lu samples\n", lab, bsi->bcounts);
    else if (bsi->bits_sample == 2) snprintf(buf, sizeof(buf),
        "%s%lu samples [00 01 10 11] %lu pkts\n"
        "%s%13lu %13lu %13lu %13lu\n"
        "%s %12f%% %12f%% %12f%% %12f%%\n",
        lab, bsi->bcounts, bsi->bpkts,
        lab, bsi->bstates[0], bsi->bstates[1],
             bsi->bstates[2], bsi->bstates[3],
        lab, (double)bsi->bstates[0] / (double)bsi->bcounts,
             (double)bsi->bstates[1] / (double)bsi->bcounts,
             (double)bsi->bstates[2] / (double)bsi->bcounts,
             (double)bsi->bstates[3] / (double)bsi->bcounts);
    else if (bsi->bits_sample == 1) snprintf(buf, sizeof(buf),
        "%s%lu samples [00 01] %lu pkts\n"
        "%s%13lu %13lu\n"
        "%s %12f%% %12f%%\n",
        lab, bsi->bcounts, bsi->bpkts,
        lab, bsi->bstates[0], bsi->bstates[1],
        lab, (double)bsi->bstates[0] / (double)bsi->bcounts,
             (double)bsi->bstates[1] / (double)bsi->bcounts);
    return(buf);
}
void stats_report(BSInfo *bsi, char *label)
{
    fputs(stats_repstr(bsi, label), stdout);
}

/* Accumulator(s) of statistics on the packet data starting with optr */
void stats_check_2bits(BSInfo *bsi, uint64_t *optr)
{
    int ii, ss;
    uint64_t val;
    for (ii = 0; ii < bsi->packet_octets; ii++)
        for (val = *optr++, ss = 0; ss < 32; ss++, val >>= 2)
            bsi->bstates[val & 0x3] ++;
    bsi->bcounts += 32 * bsi->packet_octets;
}
void stats_check_1bit(BSInfo *bsi, uint64_t *optr)
{
    int ii, ss;
    uint64_t val;
    for (ii = 0; ii < bsi->packet_octets; ii++)
        for (val = *optr++, ss = 0; ss < 64; ss++, val >>= 1)
            bsi->bstates[val & 0x1] ++;
    bsi->bcounts += 32 * bsi->packet_octets;
}
void stats_check(BSInfo *bsi, uint64_t *optr)
{
    bsi->bpkts ++;
    if (2 == bsi->bits_sample) return(stats_check_2bits(bsi, optr));
    if (1 == bsi->bits_sample) return(stats_check_1bit(bsi, optr));
}

/*
 * eof
 */
