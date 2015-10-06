/*
 * $Id: sc_stats.c 3495 2015-10-05 19:42:26Z gbc $
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
 * Delta stats--called with new data in bsi, lst a copy from
 * previous call and del a place to put manufactured diff.
 */
void stats_delta(BSInfo *bsi, BSInfo *lst, BSInfo *del,
                 uint32_t *pkt, int count, int fnum, void *start)
{
    static char lab[] = "0000               ";
    char *rep;
    int ii;
    snprintf(lab, 13, "%05d:delta:", fnum);

    del->bpkts = bsi->bpkts - lst->bpkts;
    del->bcounts = bsi->bcounts - lst->bcounts;
    for (ii = 0; ii < 4; ii++)
        del->bstates[ii] = bsi->bstates[ii] - lst->bstates[ii];
    del->packet_octets = bsi->packet_octets;
    del->bits_sample = bsi->bits_sample;
    rep = stats_repstr(del, lab);
    fprintf(stdout, "%s delta at pkt %08X(%u) for %d\n%s",
        lab, ((void*)pkt - start), ((void*)pkt - start), count, rep);
    *lst = *bsi;    /* for next time */
}

/*
 * eof
 */
