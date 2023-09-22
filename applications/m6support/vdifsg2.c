/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdifsg2.c 5776 2023-03-27 16:42:34Z gbc $
 *
 * This file provides support for the fuse interface.
 * This version is rather primitive in many respects.
 *
 * This file provides glue to the basic sg_access library
 * to allow some of the complexity that vdifuse requires.
 */

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "vdifsg2.h"
#include "vdfxmsg.h"

#if (VDIFTRACE_MAX < 3)
#define sg_secs_since(A,B)
#define DOUBLE_P12
#define DOUBLE_P123
#define TIMEVAL_NOW
#define VDIFTRACE12(X, ...)
#define VDIFTRACE123(X, ...)
#else /* VDIFTRACE_MAX < 3 */
#warning "sg_secs_since() will be called"
#define DOUBLE_P12              double part1, part2
#define DOUBLE_P123             double part1, part2, part3
#define TIMEVAL_NOW             struct timeval now
#define VDIFTRACE12(X, ...)     vdiftrace(X, __VA_ARGS__)
#define VDIFTRACE123(X, ...)    vdiftrace(X, __VA_ARGS__)
#endif /* VDIFTRACE_MAX < 3 */

/*
 * Functions related to a quasi-monotonic packet time for sgv2 packets
 */

/*
 * This code calculates PACKET_TIME() with a willingness to extrapolate
 * over invalid packets.  The presumption is that we are at the beginning
 * or end of a block, so
 *   If pt < pl, we want the start time for the member (pt+dp later than pt)
 *   if pt > pl, we want the end time for the member (pt-dp earler than pt)
 * Thus a count of invalid packets (and the count stepped over allows us
 * to extrapolate to the time of the original packet.
 *
 * The macro PACKET_TIME() only uses w1.secs_inre and w2.df_num_insec.
 * frames_fw and frames_bw is the count of invalid packets seen and
 * fcount the adjusted frame count (i.e. frames_fw or frames_bw).
 *
 * This routine is called to set the beginning and end packet times
 * (ptbe and pten) for a fragment block.  So coping with invalid packets
 * must be done so that block movement still works properly.
 *
 * This version deals with mono vthreads: only one thread and it will
 * increase monotonically from one packet to the next.
 */
static inline double packet_time_mono(uint32_t *pt, uint32_t *pl,
    uint32_t dp, uint32_t max_frame_count, short nvthreads, int npkts,
    int mmfd, char *dirs)
{
    VDIFHeader pkt;
    int frames = 0, frames_fw = 0, frames_bw = 0, tick_dn = 0, tick_up = 0;
    int fcount;
    double tt;

    if (pt < pl) {          /* start, ptbe case */
      while (pt < pl) {   /* want start time, forward advance */
        frames = frames_fw;
        if (((VDIFHeader *)pt)->w1.invalid) { pt += dp; frames_fw ++; }
        else break;
      }
    } else if (pt > pl) {   /* end, pten case */
      while (pt > pl) {   /* want end time, backward advance */
        frames = frames_bw;
        if (((VDIFHeader *)pt)->w1.invalid) { pt -= dp; frames_bw --; }
        else break;
      }
    } else {
      /* this would be a compiler error, mono case */
      vdiftrace(-1,VDT("holy f*ck, mother of god, robin"));
      return 0.0;
    }
    pkt.w1.secs_inre = ((VDIFHeader *)pt)->w1.secs_inre;
    fcount = (int)( ((VDIFHeader *)pt)->w2.df_num_insec ) - frames;
    if (((VDIFHeader *)pt)->w1.invalid)
        vdiftrace(-1, VDT("No valid pkts in block %p FAIL-INI.\n"), pkt);
        /* lasciate ogni speraza (no valid packets in block -- 1) */
        /* one could step over the block header and continue */
    if (fcount < 0) {                   /* need to decrement seconds */
        vdiftrace(-1, VDT(" fcount %d < %d #%02d (mono) %cw %d frames %s\n"),
            fcount, 0, mmfd, 'f', frames, dirs);
        fcount += (max_frame_count+1);  /* as correct as max_frame_count */
        pkt.w1.secs_inre --;
        tick_dn ++;
    } else if (fcount > max_frame_count) {  /* need to inc seconds */
        vdiftrace(-1, VDT(" fcount %d > %d #%02d (mono) %cw %d frames %s\n"),
            fcount, max_frame_count, mmfd, 'b', frames, dirs);
        fcount -= (max_frame_count+1);  /* as correct as max_frame_count */
        pkt.w1.secs_inre ++;
        tick_up ++;
    }
    pkt.w2.df_num_insec = fcount;
    /* finally construct the packet time */
    tt = PACKET_TIME(&pkt);
    if (vdifuse_debug>1 || tick_dn || tick_up) vdiftrace(4,
        VDT("mono %s pkt time %17.8lf [%+d|%+d %+d|%d %d|%d]#%02d-%s %d-thr\n"),
        ((pt < pl) ? "start" : ((pt > pl) ? " end " : "ERROR")), tt,
        frames, frames_fw, frames_bw, tick_dn, tick_up, max_frame_count,
        mmfd, dirs, nvthreads);
    return tt;
}

/*
 * This code calculates PACKET_TIME() with a willingness to extrapolate
 * over invalid packets.  The presumption is that we are at the beginning
 * or end of a block, so
 *   If pt < pl, we want the start time for the member (pt+dp later than pt)
 *   if pt > pl, we want the end time for the member (pt-dp earler than pt)
 * Thus a count of invalid packets (and the count stepped over allows us
 * to extrapolate to the time of the original packet.
 *
 * The macro PACKET_TIME() only uses w1.secs_inre and w2.df_num_insec.
 * frames_fw and frames_bw is the count of invalid or same-time packets seen
 * fcount the adjusted frame count (i.e. frames_fw or frames_bw).
 *
 * This routine is called to set the beginning and end packet times
 * (ptbe and pten) for a fragment block.  So coping with invalid packets
 * must be done so that block movement still works properly.
 *
 * This version deals with _seq vthreads:  a series of packets may
 * share the same seconds count.  Note that a group with the same
 * time may be split across a block boundary, so we need to make the
 * edge unambiguous.  In effect, we treat the first/last few
 * same-data-frame-num packets as if they were invalid to get to
 * unique times.
 */
static inline double packet_time_seq(uint32_t *pt, uint32_t *pl,
    uint32_t dp, uint32_t max_frame_count, short nvthreads, int npkts,
    int mmfd, char *dirs)
{
    VDIFHeader pkt;
    int frames = 0, frames_fw = 0, frames_bw = 0, tick_dn = 0, tick_up = 0;
    int fcount;
    double tt;

    if (pt < pl) {          /* start, ptbe case */
      while (pt < pl) {   /* want start time, forward advance */
        frames = frames_fw;
        if (((VDIFHeader *)pt)->w1.invalid ||
            ((VDIFHeader *)pt+dp)->w1.invalid ||
            (((VDIFHeader *)(pt))->w2.df_num_insec ==
             ((VDIFHeader *)(pt+dp))->w2.df_num_insec))
                { pt += dp; frames_fw ++; }
        else break;
      }
      /* shift once more to pt+dp which is not invalid */
      pt += dp; frames_fw ++;
      frames = frames_fw / nvthreads;
    } else if (pt > pl) {   /* end, pten case */
      while (pt > pl) {   /* want end time, backward advance */
        frames = frames_bw;
        if (((VDIFHeader *)pt)->w1.invalid ||
            ((VDIFHeader *)pt-dp)->w1.invalid ||
            (((VDIFHeader *)(pt))->w2.df_num_insec ==
             ((VDIFHeader *)(pt-dp))->w2.df_num_insec))
                { pt -= dp; frames_bw --; }
        else break;
      }
      /* shift once more to pt-dp which is not invalid */
      pt -= dp;
      frames_bw --;
      frames = -(-frames_bw / nvthreads);
    } else {
      /* this would be a compiler error, _seq case */
      vdiftrace(-1,VDT("holy f*ck, mother of god, batman"));
      return 0.0;
    }
    pkt.w1.secs_inre = ((VDIFHeader *)pt)->w1.secs_inre;
    fcount = (int)( ((VDIFHeader *)pt)->w2.df_num_insec ) - frames;
    if (((VDIFHeader *)pt)->w1.invalid)
        vdiftrace(-1, VDT("No valid packets in block %p FAIL-INI.\n"));
        /* lasciate ogni speraza (no valid packets in block -- 2) */
        /* one could step over the block header and continue */
    if (fcount < 0) {                   /* need to decrement seconds */
        vdiftrace(-1, VDT(" fcount %d < %d #%02d (_seq) %cw %d frames %s\n"),
            fcount, 0, mmfd, 'f', frames, dirs);
        fcount += max_frame_count+1;    /* as correct as max_frame_count */
        pkt.w1.secs_inre --;
        tick_dn ++;
    } else if (fcount > max_frame_count) {  /* need to inc seconds */
        vdiftrace(-1, VDT(" fcount %d > %d #%02d (_seq) %cw %d frames %s\n"),
            fcount, max_frame_count, mmfd, 'b', frames, dirs);
        fcount -= max_frame_count+1;    /* as correct as max_frame_count */
        pkt.w1.secs_inre ++;
        tick_up ++;
    }
    pkt.w2.df_num_insec = fcount;
    /* finally construct the packet time */
    tt = PACKET_TIME(&pkt);
    if (vdifuse_debug>1 || tick_dn || tick_up) vdiftrace(4,
        VDT("_seq %s pkt %17.8lf [%+d|%+d %+d|%d %d|%d]#%02d-%s %d-thr\n"),
        ((pt < pl) ? "start" : ((pt > pl) ? " end " : "ERROR")), tt,
        frames, frames_fw, frames_bw, tick_dn, tick_up, max_frame_count,
        mmfd, dirs, nvthreads);
    return tt;
}

/*
 * The Delta(t) on the block should be somewhat sane most of the time.
 * Recall that the sub-sec is scaled by PKTDT(small) = 1.0/PKTDT_INV
 *
 * For vthreads in sequences, the time advances slower by the number of
 * vthreads, but the number of frames should correspond to the number of
 * packets.  (So dt isn't a time, but a number of frames, here.)
 *
 * The routine returns non-zero if the file should be considered corrupt,
 * however, this check is disabled, so all you get is a trace comment.
 */
static int pkt_time_chk(SGV2sfrag *sfp, int ismono)
{
    double bt = sfp->ptbe, et = sfp->pten;
    double dt = et - bt;
    uint32_t mfc;
    int npkts = sfp->nblk, pkterr, rv = 0;
    if (round(dt) > 0.0) {	/* second boundary */
        dt -= 1.0;
        if (ismono) mfc = sfp->sgi->frame_cnt_max;
        else mfc = sfp->sgi->frame_cnt_max/sfp->sgi->nvthreads;
        dt += (mfc + 1)/((float)PKTDT_INV);
    }
    dt *= (float)PKTDT_INV;	/* dt now frame ticks */
    if (!ismono) dt *= sfp->sgi->nvthreads;
    pkterr = round(dt) - (npkts - 1);
    if (pkterr == 0) return(0);
    if (!ismono && pkterr < sfp->sgi->nvthreads) return(0);
    // If we want to add this bit of anality:
    // if (pkterr > npkts/2 || -pkterr > npkts/2) rv = 1;
    vdiferror(sfp->fip,
        VDT("pkt_time_chk: dt=%.0f. on %d is %d rv=%d "
        "[fd:%02d blk:%ld {%d:%d:%d} vt:%d] FAIL-IGN.\n"),
        dt, npkts, pkterr, rv,
        sfp->sgi->smi.mmfd, sfp->cblk,
        sfp->bnum_prev, sfp->bnum, sfp->bnum_next,
        sfp->sgi->nvthreads);
    /*
     * NOTION: do a binary search in the block to find the internal jump
     *
     * The fundamental issue for a failure is likely due to incomplete
     * (fill packets) which could lead to a significant jump in time.
     *
     * dplane has a limit on fill packets which is programmable.
     *
     * At the moment, this function is used below in member_move()
     * and with the introduction of sfp->pmid is ignorable if there
     * are any valid packets in the block.
     */
    rv = 0;
    return(rv);
}

/*
 * support cache creation of sgv2 data was moved to vdifsp2.c
 */

/*
 * vorr support follows
 */

/*
 * A trace version of the above, somewhat reduced.  Since we don't
 * use the real frame rate for time calculations, the time difference
 * gets huge (+/-) and we may not have the true frame rate to get it
 * correct, so we merely flag it (+/- 6.66).  Otherwise d is frame
 * count offset from the start of the stripe (t0).
 *
 * This is called from stripe_trace.  t0 is the beginning time (ptbe)
 * for the stripe.  See discussion in comment preceding  stripe_sqze().
 */
static void member_trace(SGV2sfrag *sfn, double t0, char label, int gg)
{
    double d = rint((sfn->ptbe - t0)/PKTDT);
    if (d < -99999) d = -6.666;
    if (d >  99999) d = +6.666;
    vdiftrace(gg,
        VDT("[%c] %u/%u %6g "
            "%17.8lf < %17.8lf < %17.8lf < %17.8lf "
            "{%d:%d:%d} "
            "%lu[%lu]%lu\n"),
        label, sfn->cblk, sfn->sgi->sg_total_blks, d,
        sfn->first, sfn->ptbe, sfn->pten, sfn->final,
        sfn->bnum_prev, sfn->bnum, sfn->bnum_next,
        sfn->bybb, sfn->byib, sfn->byab);
}

/*
 * Move a member to an absolute block position.  Note that we
 * refuse to move outside the legal position.  In that case, an
 * error is set, and we do not move.  Otherwise, we update the
 * fragment (sfp) the new particulars and bump movecount.
 */
static void member_move(SGV2sfrag *sfp, off_t blk, int dir)
{
    uint32_t *pb, *pe, *px, ps, *endp;
    int prev, this, next, nblk, chknext, wbsize;
    char *dirs = dir>0 ? "fw" : "bw";
    off_t bybb, byab;
#if VDIFTRACE_MAX > 2
    char buff[256];
#endif /* VDIFTRACE_MAX > 2 */

    if (((blk > sfp->cblk) && (dir < 1)) ||
        ((blk < sfp->cblk) && (dir > 1))    ) vdiftrace(-1,
            "member_move(blk %lu <> cblk %lu dir %d) FAIL-IGN.\n",
            blk, sfp->cblk, dir);

    vdiftrace(5, VDT("member_move(%s) %s %ld\n"), sfp->sgi->name, dirs, blk);
    if (blk < 0 || blk >= sfp->sgi->sg_total_blks) {
        sfp->err |= SGV2_ERR_BLKERR;
        vdiferror(sfp->fip,
            VDT("member_move %lu(blk <0 or >=%lu) FAIL-ERR.\n"),
            blk, sfp->sgi->sg_total_blks);
        return;
    }
#if VDIFTRACE_MAX > 2
    /* first the blocks and bytes; nblk is nlft in sg_pkt_by_blkall() */
    buff[0] = buff[255] = 0;
    pe = pb = sg_pkt_by_blkall_dbg(sfp->sgi, blk, &nblk,
        &px, &bybb, &byab, &prev, &this, &next, buff, sizeof(buff)-1);
    vdiftrace(3, VDT("%s\n"), buff);
#else /* VDIFTRACE_MAX > 2 */
    pe = pb = sg_pkt_by_blkall(sfp->sgi, blk, &nblk,
        &px, &bybb, &byab, &prev, &this, &next);
#endif /* VDIFTRACE_MAX > 2 */
    if (!pb) {
        sfp->err |= SGV2_ERR_ACCESS;
        vdiferror(sfp->fip,
            VDT("sg_pkt_blkall %s %lu %u (%lu %lu) FAIL-ERR.\n"),
            sfp->sgi->name, blk, sfp->sgi->sg_total_blks, sfp->bybb,sfp->byab);
        return;
    }

    /* pe/pb are already offset to packet start (within the read size) */
    /* ps is the read size for a packet converted to uint32_t */
    pe += (ps = (sfp->sgi->read_size/sizeof(uint32_t))) * (nblk);
    endp = pe - sfp->sgi->pkt_offset;   /* true end of the block */
    pe -= ps;                           /* ending packet of block */
    if (endp != px) {
        sfp->err |= SGV2_ERR_BLKADR;
        vdiferror(sfp->fip, VDT(
            "move sg_pkt_blkall block end (%p - %p = %lu) FAIL-ERR.\n"),
            endp, px, endp - px);
        return;
    }
    chknext = (endp < (uint32_t*)sfp->sgi->smi.eomem)
        ? ((SGV2BlkNum*)endp)->blocknum : SG_BLOCKNUM_NONEXT;
    wbsize = (endp < (uint32_t*)sfp->sgi->smi.eomem)
        ? ((SGV2BlkNum*)endp)->wb_size : sfp->sgi->sg_wr_block;
    if (next != chknext) {
        sfp->err |= SGV2_ERR_BLKNXT;
        vdiferror(sfp->fip, VDT(
            "move sg_pkt_blkall block next {%d:%d:(%d != %d;%d)} FAIL-ERR.\n"),
            prev, this, next, chknext, wbsize);
        return;
    }

    /* committed to the move, now, so update things */
    sfp->mcnt ++;
    sfp->cblk = blk;
    sfp->nblk = nblk;
    sfp->bybb = bybb;
    sfp->byab = byab;
    sfp->byib = sfp->nblk * sfp->sgi->pkt_size;
    sfp->addr = pb;
    sfp->bnum_prev = prev;
    sfp->bnum = this;
    sfp->bnum_next = next;

    /* now work out the first and final times times of block */
    if (sfp->sgi->nvthreads < 2) {
        /* dealing with mono vthread: nvthreads = 0 or 1 */
        sfp->ptbe = packet_time_mono(pb, pe, ps, sfp->sgi->frame_cnt_max,
            sfp->sgi->nvthreads, sfp->nblk, sfp->sgi->smi.mmfd, dirs);
        sfp->pten = packet_time_mono(pe, pb, ps, sfp->sgi->frame_cnt_max,
            sfp->sgi->nvthreads, sfp->nblk, sfp->sgi->smi.mmfd, dirs);
    } else {
        /* dealing with _seq vthread: nvthreads == 2 or more */
        sfp->ptbe = packet_time_seq(pb, pe, ps, sfp->sgi->frame_cnt_max,
            sfp->sgi->nvthreads, sfp->nblk, sfp->sgi->smi.mmfd, dirs);
        sfp->pten = packet_time_seq(pe, pb, ps, sfp->sgi->frame_cnt_max,
            sfp->sgi->nvthreads, sfp->nblk, sfp->sgi->smi.mmfd, dirs);
    }
    /* and in retrospect, the mid-time of a block should be robust */
    sfp->pmid = (sfp->ptbe + sfp->ptbe) / 2.0;
    /* TODO: _dir vthread case which gets its own ptbe and pten calculus */

    /* check that the packet times are consistent with #packets in block */
    if (sfp->ptbe == 0.0 || sfp->pten == 0.0) sfp->err |= SGV2_ERR_TIME_0;
    if (pkt_time_chk(sfp, sfp->sgi->nvthreads < 2)) sfp->err |= SGV2_ERR_TIME_C;
    if (sfp->pten < sfp->ptbe) sfp->err |= SGV2_ERR_TIME_B;
    /* advise mmap() machinery of our intentions */
    sg_advice(sfp->sgi, (dir > 0) ? pb : pe, dir);
}

/*
 * Open the frag and initialize it to point to the first block
 * As a complication, we identify the size of the smallest short
 * block, since we'll need it later.
 */
static int member_init(SGV2sfrag *sfp, int pgfcmx)
{
    VDIFHeader pkt;
    int msbs = sfp->sgi->sg_wr_pkts;
    uint64_t cksum = sfp->sgi->checksum;
    vdiftrace(-1, VDT("member_init entry[%d thr %d sep] %s\n"),
        sfp->sgi->nvthreads, sfp->sgi->vthreadsep, sfp->sgi->name);
    if (sfp->sgi->frame_cnt_max < pgfcmx) {
        vdiftrace(1, VDT("Fixing fcm: %d -> %d (too small, OK\n"),
            sfp->sgi->frame_cnt_max, pgfcmx);
        sfp->sgi->frame_cnt_max = pgfcmx;
    }
    sfp->err = (sg_reopen(sfp->sgi)) ? SGV2_ERR_VIRGIN : SGV2_ERR_REOPEN;
    /* if the human supplied a max frame count as a parameter... */
    if (sfp->sgi->frame_cnt_max > pgfcmx &&
        pgfcmx > 1 && pgfcmx < SG_FR_CNT_MAX) {
        vdiftrace(-1, VDT("Fixing fcm: %d -> %d (too large), FAIL-INI.\n"),
            sfp->sgi->frame_cnt_max, pgfcmx);
        sfp->sgi->frame_cnt_max = pgfcmx;
        sfp->err |= SGV2_ERR_PFGCMX;
        /* ...this error is handled in the caller */
    }
    /* make sure nvthreads is 1 even if we don't care */
    if (sfp->sgi->nvthreads == 0) {
        vdiftrace(-1, VDT("Fixing thr: one thread\n"));
        sfp->sgi->nvthreads = 1;
        sfp->sgi->vthreadsep = 0;
        sfp->sgi->vthreads[0] = VTHREAD_BOGUS;
    } else {
        /* note vthreadsep is not yet used */
        vdiftrace(-1, VD3(
            "%d thr %d %d %d %d %d %d %d %d\n",
            "%d sep %d %d %d %d %d %d %d %d\n"),
            sfp->sgi->nvthreads, sfp->sgi->vthreads[0],
            sfp->sgi->vthreads[1], sfp->sgi->vthreads[2],
            sfp->sgi->vthreads[3], sfp->sgi->vthreads[4],
            sfp->sgi->vthreads[5], sfp->sgi->vthreads[6],
            sfp->sgi->vthreads[7],
            sfp->sgi->vthreadsep, sfp->sgi->vthreads[8],
            sfp->sgi->vthreads[9], sfp->sgi->vthreads[10],
            sfp->sgi->vthreads[11], sfp->sgi->vthreads[12],
            sfp->sgi->vthreads[13], sfp->sgi->vthreads[14],
            sfp->sgi->vthreads[15]);
    }
    /* notice if we did not load data that was cached */
    if (cksum != sfp->sgi->checksum) sfp->err |= SGV2_ERR_CHKSUM;
    /* initialize times */
    pkt.w1.secs_inre = sfp->sgi->first_secs;
    pkt.w2.df_num_insec = sfp->sgi->first_frame;
    sfp->first = PACKET_TIME(&pkt);
    pkt.w1.secs_inre = sfp->sgi->final_secs;
    pkt.w2.df_num_insec = sfp->sgi->final_frame;
    sfp->final = PACKET_TIME(&pkt);

    /* identify the minimum short block size for this fragment */
    if (sfp->final < sfp->first) sfp->err |= SGV2_ERR_TIME_F;
    if (0 < sfp->sgi->sg_sh_pkts && sfp->sgi->sg_sh_pkts < msbs)
        msbs = sfp->sgi->sg_sh_pkts;
    if (0 < sfp->sgi->sg_se_pkts && sfp->sgi->sg_se_pkts < msbs)
        msbs = sfp->sgi->sg_se_pkts;

    /* move it to the beginning */
    sfp->mcnt = 0;
    sfp->cblk = 0;
    member_move(sfp, 0, 1);

    vdiftrace(-1, VDT("member_init exit "
        "#%02d@%ld %.8f..%.8f %u pps msbs %d (%04X) %d{%d:%d:%d}%d\n"),
        sfp->sgi->smi.mmfd, sfp->cblk, sfp->first, sfp->final,
        sfp->sgi->frame_cnt_max, msbs, sfp->err,
        sfp->sgi->sg_first_bnum, sfp->bnum_prev,
        sfp->bnum, sfp->bnum_next, sfp->sgi->sg_final_bnum);
    return(msbs);
}

/*
 * Generate a count of total moves within this stripe.
 */
static inline off_t calculate_moves(SGV2sdata *sdp)
{
    off_t scnt = 0;
    int ss;
    for (ss = 0; ss < sdp->numb; ss++) scnt += sdp->stripe[ss].sfrag->mcnt;
    return(scnt);
}

/*
 * A bread version of stripe_trace.  Check the move count, and only
 * if it has changed do we issue the bread crumb report on the stripe.
 *
 * s0 is t0 with the ones digit and fractions nuked.  Thus '(%.8f)'
 * gives an offset of at most 10 seconds from s0.  The following function
 * generates the breadcrumb data into a buffer and returns a pointer to it.
 */
static inline char *stripe_bread_fmt(SGV2sdata *sdp, char *where)
{
    static char buf[80*(VDIFUSE_MAX_SEQI+6)], memtmp[60];
    double s0;
    off_t scnt = calculate_moves(sdp);
    int ss, sep, len = 0;
    char endl, *lead;
    SGV2sfrag *sfp;
    if (scnt <= sdp->scnt) return(0);  /* nothing changed */
    sdp->diag[SGV2_DIAG_BREAD] ++;
    sdp->scnt = scnt;
    s0 = floor(sdp->stripe[0].sfrag->ptbe / 10.0) * 10.0;
    buf[0] = 0;
    snprintf(buf, 6*80, "%s time is "
        "%0.f+(X.xxx) fd.#bk:(X.xxx){blks} stripe <..|..> "
        "read:size %lu:%lu moved %lu %.2f%% before\n  "
        "%d frags in stripe (0..%d,%d..%d,%d..%d) "
        "bybs %lu [byis %lu] %lu byas, total %luB "
        "bnums {%d{%d:%d}%d} %.2f%% follows\n",
        where, s0, sdp->roff, sdp->size, sdp->scnt,
        (100.0 * (double)sdp->bybs / (double)sdp->bygt),
        sdp->numb, ((sdp->zero == 0) ? 0 : sdp->zero - 1),
        sdp->zero, sdp->sgap - 1, sdp->sgap,
        sdp->numb, sdp->bybs, sdp->byis, sdp->byas, sdp->bygt,
        sdp->min_bnum, sdp->prev_bnum, sdp->max_bnum, sdp->max_bnum,
        (100.0 * (double)(sdp->bybs+sdp->byis) / (double)sdp->bygt));
    len = strlen(buf);
    for (ss = 0; ss < sdp->numb; ss++) {
        if (ss == sdp->zero)        sep = '<';
        else if (ss==sdp->sgap)     sep = '|';
        else if (ss==sdp->numb-1)   sep = '>';
        else                        sep = '.';
        endl = ((ss%4) == 3) ? '\n' : ' ';
        lead = ((ss%4) == 0) ? "  " : "";
        sfp = sdp->stripe[ss].sfrag;
        snprintf(memtmp, sizeof(memtmp),
            "%s%02d%c#%02d:%lu(%.9f){%4d:%4d:%4d}%c",
            lead, ss, sep, sfp->sgi->smi.mmfd, sfp->cblk, sfp->ptbe - s0,
            sfp->bnum_prev, sfp->bnum, sfp->bnum_next, endl);
        strncat(buf, memtmp, sizeof(buf) - len);
        len += strlen(memtmp);
    }
    /* kill off possible blank line at end of buffer */
    if (buf[len-1] == '\n') return(buf);
    strncat(buf, "\n", 2);
    return(buf);
}
static void stripe_bread_code(SGV2sdata *sdp, char *where)
{
    char *buf = stripe_bread_fmt(sdp, where);
    if (buf) {
        vdifuse_bread("%s", buf);
        // vdifuse_flush_bread("wheat");
        vproc_update_file(sdp->fip, "bread", buf, VPROC_TRUNCATE);
    }
}
#define stripe_bread(G,S,W) do {                    \
    if(((G)<VDIFTRACE_MAX) && (vdifuse_debug>(G)))  \
        stripe_bread_code(S,W);                     \
    } while(0)

/*
 * A rather verbose tracing of a stripe.  This is normally only called
 * on the way out the door, we don't worry about the verbosity.
 *
 * Note, after this routine is defined, we macro it into a gated version
 */
static void stripe_trace_code(int gg, SGV2sdata *sdp, char *where)
{
    int ss;
    int z = sdp->zero, n = sdp->sgap-1, m = (z==0?0:z-1);
    off_t fragsum = 0;
    double t0 = sdp->stripe[0].sfrag->ptbe;
    sdp->diag[SGV2_DIAG_TRACE] ++;
    
    vdiftrace(gg, VDT("stripe_trace(%s) %s(%d)\n"), sdp->vs->fuse, where, gg);
    vdiftrace(gg, VDT("  (0..%d,%d..%d,%d..%d): %lu[%lu]%lu (%lu) B\n"),
        m, z, n, sdp->sgap, sdp->numb,
        sdp->bybs, sdp->byis, sdp->byas, sdp->bygt);
    vdiftrace(gg, VDT("  from[%2d] %.9f..%.9f   %lu..\n"),
        z, sdp->stripe[z].sfrag->ptbe, sdp->stripe[z].sfrag->pten, sdp->bybs);
    vdiftrace(gg, VDT("  thru[%2d] %.9f..%.9f ..%lu\n"),
        n, sdp->stripe[n].sfrag->ptbe, sdp->stripe[n].sfrag->pten, sdp->byas);
    vdiftrace(gg, VDT("  t0 is %.9f\n"), t0);

    for (ss = 0; ss < sdp->numb; ss++) {
        double d = rint((sdp->stripe[ss].sfrag->ptbe - t0)/PKTDT);
        if (d > 0.5 / PKTDT) d -= (1.0/PKTDT - (sdp->fcmx+1));
        vdiftrace(gg, 
            VDT("%c %c#%02d@%-2d#%lu[%2d:%1X]%-5g%c %lu[%lu]%lu {%lu..%lu}\n"),
            '0' + ss, (ss == sdp->zero) ? '|' : ' ',
            sdp->stripe[ss].index, sdp->stripe[ss].sfrag->sgi->smi.mmfd,
            sdp->stripe[ss].sfrag->cblk, sdp->stripe[ss].mcpys,
            sdp->stripe[ss].sfrag->err, d,
            (ss == sdp->sgap-1) ? '%' : ' ',
            sdp->stripe[ss].sfrag->bybb,
            sdp->stripe[ss].sfrag->byib,
            sdp->stripe[ss].sfrag->byab,
            sdp->bybs + fragsum,
            sdp->bybs + sdp->stripe[ss].sfrag->byib);
        if (sdp->stripe[ss].sfrag->cblk - 1 > 0) {
            /* make a copy of the frag to report on move back one */
            SGV2sfrag tmp;
            memcpy(&tmp, sdp->stripe[ss].sfrag, sizeof(tmp));
            member_move(&tmp, sdp->stripe[ss].sfrag->cblk - 1, -1);
            member_trace(&tmp, t0, 'A' + ss, gg);
        }
        member_trace(sdp->stripe[ss].sfrag, t0, '0' + ss, gg);
        if (sdp->stripe[ss].sfrag->cblk + 1 <
            sdp->stripe[ss].sfrag->sgi->sg_total_blks) {
            SGV2sfrag tmp;
            memcpy(&tmp, sdp->stripe[ss].sfrag, sizeof(tmp));
            member_move(&tmp, sdp->stripe[ss].sfrag->cblk + 1,  1);
            member_trace(&tmp, t0, 'a' + ss, gg);
        }
        fragsum += sdp->stripe[ss].sfrag->byib;
    }
    vdiftrace(gg, VDT("stripe_trace(%s) complete\n"), where);
}
#define stripe_trace(G,S,W) do {                    \
    if(((G)<VDIFTRACE_MAX) && (vdifuse_debug>(G)))  \
        stripe_trace_code(G,S,W);                   \
    } while(0)

/*
 * (Time) comparison function for qsort; it compares the
 * block times contained in sfrag.  Note that in mono
 * and _seq vthreads case, times should be well-ordered.
 * In _dir vthreads case that need not be true.
 */
#if STRIPE_COMP_HOW == STRIPE_COMP_USE_BE
/* original version using ptbe, block start time */
static int stripe_comp(const void *sa, const void *sb)
{
    SBlock *a = (SBlock*)sa;
    SBlock *b = (SBlock*)sb;
    if (a->sfrag->ptbe < b->sfrag->ptbe) return(-1);
    if (a->sfrag->ptbe > b->sfrag->ptbe) return( 1);
    /* we should never get to this case */
    vdiftrace(-1,
        VDT("stripe_comp %.8lf (fd #%02d) == %.8lf (fd #%02d)\n"),
        a->sfrag->ptbe, a->sfrag->sgi->smi.mmfd,
        b->sfrag->ptbe, b->sfrag->sgi->smi.mmfd);
    return(0);
}
#else /* STRIPE_COMP_HOW == STRIPE_COMP_USE_BE */
#if STRIPE_COMP_HOW == STRIPE_COMP_MIDDLE
/* revised version using pmid, average if first and final */
static int stripe_comp(const void *sa, const void *sb)
{
    SBlock *a = (SBlock*)sa;
    SBlock *b = (SBlock*)sb;
    if (a->sfrag->pmid < b->sfrag->pmid) return(-1);
    if (a->sfrag->pmid > b->sfrag->pmid) return( 1);
    vdiftrace(-1,
        VDT("stripe_comp %.8lf (fd #%02d) == %.8lf (fd #%02d)\n"),
        a->sfrag->pmid, a->sfrag->sgi->smi.mmfd,
        b->sfrag->pmid, b->sfrag->sgi->smi.mmfd);
    return(0);
}
#else /* STRIPE_COMP_HOW == STRIPE_COMP_MIDDLE */
/* revised version using pmid and blocks */
static int stripe_comp(const void *sa, const void *sb)
{
    SBlock *a = (SBlock*)sa;
    SBlock *b = (SBlock*)sb;
    int pmid, bnum;
    if      (a->sfrag->pmid < b->sfrag->pmid) pmid = (-1);
    else if (a->sfrag->pmid > b->sfrag->pmid) pmid = ( 1);
    else                                      pmid = ( 0);
    if      (a->sfrag->bnum < b->sfrag->bnum) bnum = (-1);
    else if (a->sfrag->bnum > b->sfrag->bnum) bnum = ( 1);
    else                                      bnum = ( 0);
    /* if they agree, that is good */
    if (pmid == bnum) return(pmid);
    vdiftrace(-1,
        VDT("stripe_comp %.8lf (fd #%02d,%d) == %.8lf (fd #%02d,%d)\n"),
        a->sfrag->pmid, a->sfrag->bnum, a->sfrag->sgi->smi.mmfd,
        b->sfrag->pmid, b->sfrag->bnum, b->sfrag->sgi->smi.mmfd);
#if STRIPE_COMP_TIEBRK == STRIPE_COMP_PMID
    return(pmid);
#else /* STRIPE_COMP_TIEBRK == STRIPE_COMP_PMID */
    return(bnum);
#endif /* STRIPE_COMP_TIEBRK == STRIPE_COMP_PMID */
}
#endif /* STRIPE_COMP_HOW == STRIPE_COMP_MIDDLE */
#endif /* STRIPE_COMP_HOW == STRIPE_COMP_USE_BE */

/*
 * Sort the members of the sequence by block start time.
 */
static void stripe_sort(SGV2sdata *sdp)
{
    SBlock *sbp = sdp->stripe;
    qsort(sbp, sdp->numb, sizeof(SBlock), stripe_comp);
}

/*
 * Check that a stripe is sorted, which it must always be up through
 * the gap; ie. [0..sgap] must be sorted, [sgap+1..numb-1] need not be,
 * but there is no harm in sorting them for the diagnostics.
 * sgap is the index of the first fragment after the gap; if
 *      there is no gap it should be numb (initial setting).
 *
 * When we are done [0..sgap-1] should be sane and usable.
 */
static int stripe_schk(SGV2sdata *sdp)
{
    int ii, nn;
    vdiftrace(5, VDT("stripe_schk(%s)\n"), sdp->vs->fuse);
    for (ii = 0; ii < sdp->numb-1; ii++)
        if (stripe_comp((void*)&sdp->stripe[ii],
                        (void*)&sdp->stripe[ii+1]) >= 0) {
            if (ii < sdp->sgap) {   /* ii+1 == sdp->sgap */
                vdiferror(sdp->fip,
                    VDT("stripe_schk %d v %d (%.8lf v %.8lf) FAIL-ERR.\n"),
                    ii, ii+1, sdp->stripe[ii].sfrag->ptbe,
                    sdp->stripe[ii+1].sfrag->ptbe);
                stripe_bread(-1, sdp, "schk");
                stripe_trace(-1, sdp, "sortchk");
                return(SGV2_ERR_SRTCHK);
            } else {                /* sort last part */
                nn = sdp->numb - (ii + 1);
                vdiftrace(2,
                    VDT("Sort check resort 0..%d..%d,%d; (at %d sort %d)\n"),
                    sdp->zero, sdp->sgap-1, sdp->sgap, ii+1, nn);
                qsort(sdp->stripe + ii + 1, nn, sizeof(SBlock), stripe_comp);
            }
        } /* else we have the proper order:  stripe[ii] < stripe[ii+1] */
    vdiftrace(1,
        VDT("stripe %lu..%lu covers %.8lf..%.8lf\n"),
        sdp->bybs, sdp->bybs + sdp->byis,
        sdp->stripe[sdp->zero].sfrag->ptbe,
        sdp->stripe[sdp->sgap-1].sfrag->pten);
    stripe_bread(-1, sdp, "schk");
    return(0);
}

/*
 * Set the "zero" point based on the edge.  The blocks should
 * be time-sorted at this point, and any leading reads marked
 * -1 rather than zero, should form the basis of the "zero" group.
 * We don't care about direction or time, since stripe_walk() did.
 *
 * The zero point is the first fragment that hasn't been read.
 */
static int stripe_zero(SGV2sdata *sdp)
{
    int ii;
    sdp->sgap = sdp->numb;  /* set stripe gap */
    for (ii = 0; ii < sdp->numb; ii++)
        if (sdp->stripe[ii].mcpys == 0) break;
    sdp->zero = ii;         /* set stripe zero */
    return(sdp->zero);
}

/*
 * Support for strip_dgap(): check if any predecessors of the jth
 * block are earlier but no later than pten (end of ith).  If so,
 * make the move.  Return value indicates if there is a gap or not
 * after all tries have been exhausted.
 */
static int predecessors_leave_gap(int jj, SGV2sfrag *jth,
    double pten, double mint, char *lab)
{
    double dt = jth->ptbe - pten, odt;
    int mmfd = jth->sgi->smi.mmfd;
    while (dt >= mint && jth->cblk > 0) {
        /* peek at jth's predecessor */
        SGV2sfrag tmp = *jth;
        member_move(&tmp, jth->cblk - 1, -1);
        odt = dt;
        dt = tmp.ptbe - pten;   /* trial dt with jth's preceding block */
        if (dt > (double)(tmp.sgi->nvthreads-1)*PKTDT) {
            /* tmp is earlier than jth, but later than ith */
            vdiftrace(2,VD3(
                "Gap%02d#%02d: have %+.8lf =(jth-be)%.8lf-(ith-en)%.8lf\n",
                "Gap%02d#%02d: take %+.8lf =(jm1-be)%.8lf-(ith-en)%.8lf**\n"),
                jj, mmfd, odt, jth->ptbe, pten, jj, mmfd, dt, tmp.ptbe, pten);
            *jth = tmp;
        } else {
            vdiftrace(2,VD3(
                "Ngp%02d#%02d: keep %+.8lf =(jth-be)%.8lf-(ith-en)%.8lf**\n",
                "Ngp%02d#%02d: skip %+.8lf =(jm1-be)%.8lf-(ith-en)%.8lf\n"),
                jj, mmfd, odt, jth->ptbe, pten, jj, mmfd, dt, tmp.ptbe, pten);
            dt = odt;
            break;
        }
    }
    vdiftrace(2, VDT("gap%d:predecessors_leave_gap(%s) %s %.8lf > %.8lf\n"),
        jj, lab, (dt >= mint) ? "left gap" : "(no gap)", dt, mint);
    return(dt >= mint ? 1 : 0);
}

/*
 * Examine the successors of the nacent stripe and see if any of
 * them fall earlier than ptbe, the beginning of the first block
 * after the gap.  If one does, then the gap survives this fragment.
 */
static int successor_closes_gap(SGV2sdata *sdp, double ptbe, int jj)
{
    int ii;
    double dt;
    for (ii = sdp->zero; ii < jj; ii++) {
        /* peek at ith's successor */
        SGV2sfrag tmp = *(sdp->stripe[ii].sfrag);
        if (tmp.cblk < tmp.sgi->sg_total_blks - 1) {
            member_move(&tmp, tmp.cblk + 1, 1);
            dt = tmp.pten - ptbe;
            if (dt < -(double)(tmp.sgi->nvthreads-1)*PKTDT) {
                /* found an earlier block */
                vdiftrace(2, VDT("successor_closes_gap() w/"
                    " gap(%+.8lf = %.8lf - %.8lf)\n"), dt, tmp.pten, ptbe);
                return(1);
            }
        }
    }
    vdiftrace(2, VDT("gap%d:successor_closes_gap(), NOT\n"), jj);
    return(0);
}

/*
 * Adjust the stripe to remove gaps.  Here we would like to make
 * sure that we have a monotonically increasing set of blocks across
 * the stripe.  If we find a gap, we want to see if an earlier block
 * of that fragment closes the gap, or if it is to be found on another
 * fragment.  If blocks are truly missing, that's ok.
 *
 * First we move past any blocks that fall earlier than what we
 * need.  (stripe_walk() did its work so that stripe_zero() can
 * set "zero" to reflect these earlier blocks.)
 *
 * Then, we walk across, looking for a gap
 *  zero
 *  t0 < ... < ti < tj < tk < ... < tnm1
 *                  tj might shift earlier (predecessors_leave_gap())
 *                       tk might shift earlier (predecessors_leave_gap())
 *
 * Once we have found a real gap, we then need to test all further
 * blocks for predecessors that might close the gap.  If so, we swap
 * and keep going.
 *
 *  t0 < ... < ti < tj < tk < ... < tnm1
 *                gap
 *  t0'           < tj establishes gap
 *             tk' could be earlier
 *
 * Once we have the gap, we need to examine the predecessors of post
 * gap fragments--these might actually be part of the stripe.  If found
 * we SWAP them into place.
 *
 * Finally, we check that nothing is out of order.
 *
 * Once we have completed this step, the true byte offset in the file
 * can be reliably computed, assuming no corruption of the data....
 *
 * TODO: optimize stripe_spot/stripe_dgap/stripe_sqze.
 *
 *  In the normal walk-by-one approach, the stripe is likely to stay
 *  close in time, so the stripe_dgap() approach should work.  With
 *  larger jumps [stripe_spot()] there could be a spread of times,
 *  and stripe_dgap() does not work some of the time.  stripe_sqze()
 *  makes a pass to squeeze the strip to be blocks just after the zero
 *  block.  It might be more optimal to use the middle block and move
 *  some forward and others backwards.
 */
static void stripe_sqze(SGV2sdata *sdp)
{
    int jj, ii;
    SGV2sfrag *ith;
    vdiftrace(5, VDT("stripe_sqze(%s)\n"), sdp->vs->fuse);
    jj = stripe_zero(sdp) + 1;  /* and also set stripe zero */
    ith = sdp->stripe[ii = sdp->zero].sfrag;
    while (jj < sdp->numb) {
        SGV2sfrag *jth = sdp->stripe[jj].sfrag;
        (void)predecessors_leave_gap(jj,jth,ith->pten,sdp->msbs*PKTDT,"sqze");
        jj++;
    }
    stripe_sort(sdp);
}
static void stripe_dgap(SGV2sdata *sdp)
{
    int jj, ii;
    SGV2sfrag *ith;
    SBlock tmp;
    vdiftrace(5, VDT("stripe_dgap(%s)\n"), sdp->vs->fuse);
    jj = stripe_zero(sdp) + 1;  /* and also set stripe zero */
    ith = sdp->stripe[ii = sdp->zero].sfrag;
    while (jj < sdp->numb) {
        SGV2sfrag *jth = sdp->stripe[jj].sfrag;
        /* shift jth block earlier as needed to close any gap */
        if (predecessors_leave_gap(jj,jth,ith->pten,sdp->msbs*PKTDT,"dgap")) {
            /* if a successor of ith sits in gap, it's a real one */
            if (successor_closes_gap(sdp, jth->ptbe, jj)) break;
        }
        /* we move on if the gap is either closed, or a real one */
        ii = jj++;
        ith = jth;
    }

    /* at this point, the gap starts here, and j-1st is last of stripe */
    sdp->sgap = jj++;   /* set stripe gap */
    /* or there is no gap, and sdp->sgap == jj == sdp->numb */
    if ((sdp->sgap < sdp->numb || sdp->zero > 0)) vdiftrace(2,
        VDT("Gap after ii=%d < sgap=%d < jj=%d  numb=%d\n"),
        ii, sdp->sgap, jj, sdp->numb);

    /* blocks after the gap should still be moved back if possible */
    while (jj < sdp->numb) {
        SGV2sfrag *jth = sdp->stripe[jj].sfrag;
        /* shift jth block earlier as needed to close any gap */
        if (!predecessors_leave_gap(jj,jth,ith->pten,sdp->msbs*PKTDT,"DGAP")) {
            /* the new jth closes the gap, so swap jth for ith+1 */
            tmp = sdp->stripe[++ii];
            sdp->stripe[ii] = sdp->stripe[jj];
            sdp->stripe[jj] = tmp;
            ith = sdp->stripe[ii].sfrag;
            jj = sdp->sgap = ii+1;  /* re-set stripe gap */
            if (sdp->sgap > sdp->numb) sdp->sgap = sdp->numb;
            jth = sdp->stripe[jj].sfrag;
        } else {
            jj++;
        }
    }
}

/*
 * Compute byte offset of the stripe.  At this point, the
 * fragments of the stripe are time ordered, and any gaps
 * are real.  Fragments 0..[zero-1] are part of the set,
 * but the times of interest have moved beyond them.
 * (In theory, we've reached the end of file on them.)
 * fragments after sgap all belong after the stripe for
 * the purpose of offset calculations.
 */
static void stripe_boff(SGV2sdata *sdp)
{
    int ii;
    SGV2sfrag *ith;
    vdiftrace(5, VDT("stripe_boff(%s)\n"), sdp->vs->fuse);
    sdp->bybs = 0;
    sdp->byis = 0;
    sdp->byas = 0;
    /* account for bytes before the stripe */
    for (ii = 0; ii < sdp->zero && ii < sdp->numb; ii++) {
        ith = sdp->stripe[ii].sfrag;
        sdp->bybs += ith->bybb;
        sdp->bybs += ith->byib;
        if (ith->byab > 0)
            vdiferror(sdp->fip, VDT("Error on %d < \"zero\" FAIL-IGN.\n"), ii);
    }
    /* account for bytes within the stripe */
    for ( ; ii < sdp->sgap && ii < sdp->numb; ii++) {
        ith = sdp->stripe[ii].sfrag;
        sdp->bybs += ith->bybb;
        sdp->byis += ith->byib;
        sdp->byas += ith->byab;
    }
    /* account for bytes before or after the stripe */
    for ( ; ii < sdp->numb; ii++) {
        ith = sdp->stripe[ii].sfrag;
        sdp->bybs += ith->bybb;
        sdp->byas += ith->byib + ith->byab;
    }
    /* calculate bytes of grand total -- should be sequence size */
    sdp->bygt = sdp->bybs + sdp->byis + sdp->byas;
    vdiftrace(2, VDT("  stripe_boff: %lu+%lu+%lu = %lu B (%lu B)\n"),
        sdp->bybs, sdp->byis, sdp->byas, sdp->bygt, sdp->vs->u.vfuse.st_size);
    /* check with the cached grand total value */
    if (sdp->bygt != sdp->vs->u.vfuse.st_size)
        vdiferror(sdp->fip, VDT("Inconsistent sizing: %lu != %lu FAIL-IGN.\n"),
            sdp->bygt, sdp->vs->u.vfuse.st_size);
}

/*
 * Prior to this all fragments should have been initialized with
 * bnum, bnum_prev(as SG_BLOCKNUM_INIT) and bnum_next.  Now set
 * the global values across the stripe and the sequence.
 */
static void init_stripe_bnums(SGV2sdata *sdp)
{
    int nn, smallest = 0x7fffffff, thenext = 0x7fffffff;
    int largest = -1, greatest = -1;
    for (nn = 0; nn < sdp->numb; nn++) {
        if (sdp->stripe[nn].sfrag->bnum < smallest)
            smallest = sdp->stripe[nn].sfrag->bnum;
        if (sdp->stripe[nn].sfrag->bnum_next < thenext)
            thenext = sdp->stripe[nn].sfrag->bnum_next;
        if (sdp->stripe[nn].sfrag->bnum > largest)
            largest = sdp->stripe[nn].sfrag->bnum;
        if (sdp->stripe[nn].sfrag->sgi->sg_final_bnum > greatest)
            greatest = sdp->stripe[nn].sfrag->sgi->sg_final_bnum;
    }
    sdp->min_bnum = smallest;
    sdp->max_bnum = greatest;
    sdp->prev_bnum = SG_BLOCKNUM_INIT;  /* nothing previous */
    sdp->next_bnum = thenext;
    vdiftrace(-1, VDT("init_stripe_bnums %d < %d < %d < %d\n"),
        sdp->prev_bnum, sdp->min_bnum, sdp->next_bnum, sdp->max_bnum);
}

/*
 * Initial pointing of the stripe to the start of the sequence
 * at the beginning of operations.
 *
 * Implicitly all the read points are at the start of the file,
 * and the "last read offset", roff is also zero.  So we sort
 * the stripes, and close up the gaps.
 *
 * After this routine is finished, we should be able to read data.
 */
static int stripe_init(SGV2sdata *sdp, uint32_t fcmx)
{
    vdiftrace(-1, VDT("stripe_init(%s)\n"), sdp->vs->fuse);
    vdiftrace(-1, VDT("   init min sh block size %d (%.8lf) fcmx %u\n"),
        sdp->msbs, sdp->msbs*PKTDT, fcmx);
    sdp->zero = 0;
    sdp->sgap = sdp->numb;
    sdp->fcmx = fcmx;
    init_stripe_bnums(sdp);

    /* hereafter, zero is managed by walk, sgap by dgap */
    stripe_trace(5, sdp, "init");
    stripe_sort(sdp);
    stripe_dgap(sdp);
    stripe_boff(sdp);
    stripe_trace(4, sdp, "init");
    return(stripe_schk(sdp));
}

/*
 * Write helper: check that the bytes are coming from legal places
 * and that they are going to legal places.
 */
static inline int memory_check(char *buf, void *addr, size_t todo,
    SGV2sdata *sdp, SGV2sfrag *ith)
{
    SGMMInfo *smi = &ith->sgi->smi;
    if (addr < smi->start || addr + todo > smi->eomem) {
        vdiftrace(-1, VDT("read outside fragment: %p<%p | %p>%p\n"),
            addr, smi->start, addr + todo, smi->eomem);
        return(1);
    }
    if (buf < sdp->sowb || buf + todo > sdp->eowb) {
        vdiftrace(-1, VDT("write outside buffer:  %p<%p | %p>%p\n"),
            buf, sdp->sowb, buf + todo, sdp->eowb);
        return(1);
    }
    return(0);
}

/*
 * This is only called in the dir<0 case (bw to an earlier time).
 * bydg is the bytes before stripe at entry here.  On exit, we
 * should be increasing that (moving the stripe forwards).
 *
 * We move one the 0-th fragment at a time and then loop in the caller.
 *
 * Returns 1 if we are done (break loop), otherwise 0 to try again.
 */
static int stripe_vern(SGV2sdata *sdp, off_t bydg)
{
    int nblk;
    off_t obybs = sdp->bybs;
    sdp->diag[SGV2_DIAG_VERN] ++;
    SGV2sfrag *ith;

    /* break loop if we have already shifted the stripe earlier */
    if (sdp->bybs + sdp->byis >= bydg) return(1);

    vdiftrace(-1,VDT("vern: %lu < %lu\n"), sdp->bybs + sdp->byis, bydg);
    /* else (sdp->bybs + sdp->byis < bydg) and we have to shift stripe */
    ith = sdp->stripe[0].sfrag;
    nblk = ith->cblk + 1;
    /* keep nblk legal */
    if (nblk >= ith->sgi->sg_total_blks) nblk = ith->sgi->sg_total_blks-1;
    if (ith->cblk == nblk) {
        /* its is at the end of the blocks and moving it won't help */
        sdp->stripe[0].mcpys = -1;
    } else {
        /* move it forward--it may supply missing bytes */
        member_move(ith, nblk, 1);
        sdp->stripe[0].mcpys = 0;
    }

    stripe_sort(sdp);
    stripe_dgap(sdp);
    stripe_boff(sdp);
    stripe_bread(-1, sdp, VDX("vern"));

    /* the requested motion should always move us forward */
    if (sdp->bybs <= obybs) {
        vdiferror(sdp->fip,
            VDT("vern: %lu <= %lu, FAIL-IGN.\n"), sdp->bybs, obybs);
        /* break loop */
        return(1);
    }

    /* break loop if we have now shifted the stripe earlier */
    if (sdp->bybs + sdp->byis >= bydg) return(1);

    /* try again */
    return(0);
}

/*
 * Shift the (active) stripe later (dir>0) or earlier (dir<0)
 * If there is a serious issue, return nonzero.
 *
 * if dir>0 we want the stripe after  > pten(sgap-1) = t(bybs+byis)
 * if dir<0 we want the stripe before < ptbe(zero)   = t(bybs)
 *
 * We mark a frag with mcpys 0 if it looks as if it should be part of
 * the new stripe, and -1 if not.  (mcpys is otherwise just a diagnostic.)
 *
 * mcpys == 0 is used to set the new zero in stripe_zero.
 *
 * edge is the edge time of the last fragment used
 * bydg is the number of bytes preceding stripe (prior to motion)
 *
 * We know here that the current stripe blocks are too late or too soon,
 * so we can just select predecessors or successors and follow the steps
 * to make the stripe usable similar to what stripe_init() does.
 *
 * The tricky bit is that we want to make sure that the new stripe abuts
 * the existing one so that reads at the edge [read_rdfw() and read_rdbw()]
 * will be able to succeed.  The stripe_dgap() logic appears to be sound
 * against dir>0, but if dir<0, we might wind up with a gap before
 * the bytes we're looking for.  stripe_vern() attempts to address this
 * by moving some fragments forward in time (i.e. dir>0).
 */
static int stripe_walk(SGV2sdata *sdp, int dir)
{
    int ii, nblk, cblk, ord, newmcpys, vcnt = 0;
    double edge, ptbe, pten;
    DOUBLE_P123;
    SGV2sfrag *ith;
    TIMEVAL_NOW;
    off_t bydg = 0;
    sg_secs_since(&now, NULL);
    sdp->diag[SGV2_DIAG_WALK] ++;
    vdiftrace(5, VDT("stripe_walk(%s)\n"), sdp->vs->fuse);
    stripe_trace(5, sdp, "prewalk");

    if (dir == 0) {
        vdiferror(sdp->fip,VDT("illegal stripe shift of 0, FAIL-ERR.\n"));
        return(1);
    } else if (dir > 0) {
        edge = sdp->stripe[sdp->sgap - 1].sfrag->pten;
        /* bydg would be sdp->bybs + sdp->byis; but it is unused for dir > 0 */
    } else if (dir < 0) {
        edge = sdp->stripe[sdp->zero].sfrag->ptbe;
        bydg = sdp->bybs;   /* bytes before stripe on walk entry */
    }

    /* move stripe to predecessors/successors; FIXME: pmid & blocks */
    for (ii = 0; ii < sdp->numb; ii++) {
        ith = sdp->stripe[ii].sfrag;
        ord = sdp->stripe[ii].mcpys;

        cblk = ith->cblk;
        ptbe = ith->ptbe;
        pten = ith->pten;

        /* work out the new nblk, keeping it bounded */
        nblk = ith->cblk + dir;
        if (nblk < 0) nblk = 0;
        if (nblk >= ith->sgi->sg_total_blks) nblk = ith->sgi->sg_total_blks-1;

        /* do something based on block placement relative to edge */
        if (ith->cblk == nblk) {
            /* this can only happen at the extremes of the data */
            if (dir > 0) newmcpys = (pten > edge) ? 0 : -1;
            else         newmcpys = (ptbe < edge) ? 0 : -1;
            vdiftrace(1,
                VDT("  stripe%02d#%02d @%u %.8lf (%d) == same\n"),
                ii, ith->sgi->smi.mmfd, cblk, ptbe, ord);
        } else if (dir > 0 && pten > edge) {
            newmcpys = 0;
            vdiftrace(1,
                VDT("  stripe%02d#%-2d @%u %.8lf (%d) == greater "
                    "(%.8lf > %.8lf)\n"),
                ii, ith->sgi->smi.mmfd, cblk, ptbe, ord, pten, edge);
        } else if (dir < 0 && ptbe < edge) {
            newmcpys = 0;
            vdiftrace(1,
                VDT("  stripe%02d#%-2d @%u %.8lf (%d) == lesser "
                    "(%.8lf < %.8lf)\n"),
                ii, ith->sgi->smi.mmfd, cblk, ptbe, ord, ptbe, edge);
        } else {
            newmcpys = 0;
            member_move(ith, nblk, dir);
            vdiftrace(1,
                VDT("  stripe%02d#%-2d @%u %.8lf (%d) -> #%u %.8lf %lu\n"),
                ii, ith->sgi->smi.mmfd, cblk, ptbe, ord,
                nblk, ith->ptbe, ith->bybb);
        }
        sdp->stripe[ii].mcpys = newmcpys;
    }

    /* then always make sure we have a sane stripe */
    sg_secs_since(&now, &part1);
    stripe_sort(sdp);
    stripe_dgap(sdp);
    stripe_boff(sdp);
    stripe_trace(5, sdp, "walkend");
    sg_secs_since(&now, &part2);

    /* give control to stripe_vern for eventually quitting */
    while (dir<0) {
        if (stripe_vern(sdp, bydg)) break;
        vcnt ++;
        if (vcnt > 2*VDIFUSE_MAX_SEQI) {
            vdiferror(sdp->fip,
                VDT("stripe_vern count exceeded %d FAIL-IGN.\n"), vcnt);
            break;
        }
    }
    sg_secs_since(&now, &part3);
    VDIFTRACE123(0, VDT("stripe_walk(%s) took (%f+%f+%f)/%f, vcnt %d\n"),
        sdp->vs->fuse, part1, part2, part3, part1 + part2 + part3, vcnt);
    ii = stripe_schk(sdp);
    if (ii) vdiferror(sdp->fip, VDT("stripe_walk ii = %04X FAIL-IGN.\n"), ii);
    vdifuse_flush_bread("walk");
    return(ii);
}

/*
 * Place a stripe at an absolute position determined by a set of
 * block numbers.  This is a stripped-down version of stripe_walk()
 * since we don't have as much thinking to do about block positions.
 */
static void stripe_spot(SGV2sdata *sdp, off_t cblks[])
{
    int ii, dir;
    TIMEVAL_NOW;
    DOUBLE_P12;
    off_t oblks = 0, nblks = 0;
    sg_secs_since(&now, NULL);
    for (ii = 0; ii < sdp->numb; ii++) {
        dir = (cblks[ii] >= sdp->stripe[ii].sfrag->cblk) ? 1 : -1;
        oblks += sdp->stripe[ii].sfrag->cblk;
        nblks += cblks[ii];
        member_move(sdp->stripe[ii].sfrag, cblks[ii], dir);
        sdp->stripe[ii].mcpys = 0;
    }
    sg_secs_since(&now, &part1);
    stripe_sort(sdp);
    stripe_sqze(sdp);
    stripe_dgap(sdp);
    stripe_boff(sdp);
    sg_secs_since(&now, &part2);
    VDIFTRACE12(-1,VDT("stripe_spot took (%f+%f) %f s, %lu->%lu blks/stripe\n"),
        part1, part2, part1 + part2, oblks/sdp->numb, nblks/sdp->numb);
    ii = stripe_schk(sdp);
    if (ii) vdiferror(sdp->fip, VDT("stripe_spot ii = %04X FAIL-IGN.\n"), ii);
}

/*
 * Make a large change in the stripe position, either to later blocks
 * (dir>1) or earlier (dir<-1) ones.  We can use stripe_walk() to
 * reposition the fragments, but need to do a bit more work to see if
 * we have found the appropriate part of the file.
 *
 * Might perhaps use a binary search strategy, but a one-shot plan
 * might work and is what is implemented:  assume a uniform distribution,
 * and compute new blocks based on that.  Then:
 * a) move each member to this offset using stripe_spot
 * which sorts the stripe into some order.  After we return stripe_move
 * should take care of walking near the spot to get into position.
 */
static void stripe_find(SGV2sdata *sdp, int dir)
{
    off_t nblks[VDIFUSE_MAX_SEQI];
    int ii;
    double ffrac;
    SGInfo *sgi;
    sdp->diag[SGV2_DIAG_FIND] ++;
    vdiftrace(5, VDT("stripe_find(%s)\n"), sdp->vs->fuse);
    ffrac = (double)(sdp->roff + sdp->size/2) / (double)sdp->bygt;
    vdiftrace(-1, VDT("stripe_find to %lf within fragments\n"), ffrac);
    for (ii = 0; ii < sdp->numb; ii++) {
        sgi = sdp->stripe[ii].sfrag->sgi;
        nblks[ii] = rint((double)sgi->sg_total_blks * ffrac);
        /* just to be safe: keep it legal */
        if (nblks[ii] >= sgi->sg_total_blks)
            nblks[ii] = sgi->sg_total_blks - 1;
        else if (nblks[ii] <= 0) nblks[ii] = 0;
    }
    stripe_spot(sdp, nblks);
}

/*
 * Consider a large shift of the stripe position via stripe_find().
 * The distance is to the middle of the read request and stripe_find()
 * moves all the members to be in about the correct position.
 */
static void stripe_jump(SGV2sdata *sdp)
{
    int dir = 0;
    off_t distance = 0;
    if (sdp->roff + sdp->size < sdp->bybs) {                /* before */
        dir = -1;
        distance = sdp->bybs - (sdp->roff + sdp->size) - sdp->size/2;
    } else if (sdp->roff > sdp->bybs + sdp->byis) {         /* after */
        dir = 1;
        distance = sdp->roff - (sdp->bybs + sdp->byis) + sdp->size/2;
    }
    if (dir && (distance > STRIPE_JUMP_TRIGGER)) {
        stripe_bread(4, sdp, VDX("j-in"));
        vdiftrace(-1, VDT("stripe_jump %s: %lu>trigger %lu\n"),
            (dir > 0) ? "forward" : "backward", distance, STRIPE_JUMP_TRIGGER);
        stripe_find(sdp, dir);
        // stripe_bread(-1, sdp, VDX("jout"));
        vdiftrace(-1, VDT("stripe covers %lu..%lu\n"),
            sdp->bybs, sdp->bybs + sdp->byis);
        vdifuse_flush_bread("stripe_jump");
    }
}

/*
 * Check where the "offset" lies when the relative to the current stripe.
 * For nearby requests, we walk forward or backwards until we overlap.
 * For distant requests, we make a jump and then settle on it.
 *
 * Return values: 0 = no change, 1 = ok change, -1 = death.
 */
static int stripe_move(SGV2sdata *sdp)
{
    int rv = -1, strikes = 0;
    off_t distance, newdist;
    TIMEVAL_NOW;
    DOUBLE_P12;

    sdp->diag[SGV2_DIAG_MOVE] ++;
    vdiftrace(5, VDT("stripe_move(%s)\n"), sdp->vs->fuse);
    stripe_jump(sdp);
    sg_secs_since(&now, NULL);

    distance = sdp->bybs - (sdp->roff + sdp->size);
    /* sdp->roff + sdp->size <= sdp->bybs */
    while (distance >= 0) {
        vdiftrace(1, VDT("  Req. %lu:%lu fully precedes stripe by %lu\n"),
            sdp->roff, sdp->size, distance);
        if (stripe_walk(sdp, -1)) break;
        newdist = sdp->bybs - (sdp->roff + sdp->size);
        if (newdist > distance) {
            vdiftrace(1, VDT("  Distance increased to %lu (%d)\n"),
                newdist, strikes);
            vdiftrace(-1, VDT("stripe_move A: %lu>%lu (%d)"),
                newdist, distance, strikes);
            if (++strikes > 5) break;
        }
        distance = newdist;
        rv = 1;
    }
    sg_secs_since(&now, &part1);
    strikes = 0;

    distance = sdp->roff - (sdp->bybs + sdp->byis);
    /* sdp->roff >= sdp->bybs + sdp->byis */
    while (distance >= 0) {
        vdiftrace(1, VDT("  Req. %lu:%lu fully follows stripe by %lu\n"),
            sdp->roff, sdp->size, distance);
        if (stripe_walk(sdp,  1)) break;
        newdist = sdp->roff - (sdp->bybs + sdp->byis);
        if (newdist > distance) {
            vdiftrace(1, VDT("  Distance increased to %lu (%d)\n"),
                newdist, strikes);
            vdiftrace(-1, VDT("stripe_move B: %lu>%lu (%d)\n"),
                newdist, distance, strikes);
            if (++strikes > 5) break;
        }
        distance = newdist;
        rv = 1;
    }
    sg_secs_since(&now, &part2);

    /* the cases we can handle are consistent with this */
    if (sdp->roff < sdp->bybs + sdp->byis &&
        sdp->bybs < sdp->roff + sdp->size) {
        vdiftrace(3, VD3(
            "  Req. start %lu < %lu is before stripe end\n",
            "  Req. end   %lu < %lu is after stripe start\n"),
            sdp->roff, sdp->bybs + sdp->byis,
            sdp->roff + sdp->size, sdp->bybs);
        rv = 0;
    } else {
      vdiftrace(3, VD3(
            "  Req. start %lu > %lu is after stripe end, OR\n",
            "  Req. end   %lu > %lu is before stripe start\n"),
            sdp->roff, sdp->bybs + sdp->byis,
            sdp->roff + sdp->size, sdp->bybs);
      vdiftrace(-1, 
        VDT("stripe_move req %lu > %lu or req %lu > %lu, rv %d\n"),
        sdp->roff, sdp->bybs + sdp->byis,
        sdp->roff + sdp->size, sdp->bybs, rv);
    }
    VDIFTRACE12(4, VDT("stripe_move(%s) took (%f+%f) %fs\n"),
        sdp->vs->fuse, part1, part2, part1 + part2);
    return(rv);
}

/*
 * Check me again, I'm paranoid.
 */
static int stripe_anal(SGV2sdata *sdp)
{
    vdiftrace(5, VDT("stripe_anal(%s)\n"), sdp->vs->fuse);
    if (sdp->roff >= sdp->bybs &&
               sdp->roff + sdp->size <= sdp->bybs + sdp->byis) {
        vdiftrace(3, VDT("Request %lu:%lu fully in stripe\n"),
            sdp->roff, sdp->size);
    } else if (sdp->roff >= sdp->bybs &&
               sdp->roff < sdp->bybs + sdp->byis) {
        vdiftrace(1, VDT("Request %lu:%lu begins in stripe, runs after\n"),
            sdp->roff, sdp->size);
    } else if (sdp->roff < sdp->bybs &&
               sdp->roff + sdp->size <= sdp->bybs + sdp->byis) {
        vdiftrace(1, VDT("Request %lu:%lu ends in stripe, starts before\n"),
            sdp->roff, sdp->size);
    } else {
        /* this was preceded by a stripe_move() which should have succeeded */
        vdiftrace(-1,
            VDT("Request %lu:%lu outside of stripe %lu:%lu\n"),
            sdp->roff, sdp->size, sdp->bybs, sdp->bybs + sdp->byis);
        vdiftrace(-1, VDT("EIO request %lu:%lu stripe %lu:%lu FAIL-ERR.\n"),
            sdp->roff, sdp->size, sdp->bybs, sdp->bybs + sdp->byis);
        stripe_bread(-1, sdp, VDX("anal"));
        stripe_trace(-1, sdp, "anal");
        return(-EIO);
    }
    return(0);
}

/*
 * This function is used to supply read requests that may be fully
 * satisfied from within the existing stripe.  If we got here, we
 * know that the request is fully satisfyable within the data we have.
 * So we just need to transfer the parts from each fragment in order.
 *
 * We leave sdp->roff and sdp->size alone to reflect what should
 * be in the buffer and return the number of bytes transferred.
 * TODO: handle pkt offset > 0 case: memcpy -> a loop over packets
 */
static ssize_t stripe_read(SGV2sdata *sdp, char *buf, char *lab)
{
    int ii;
    SGV2sfrag *ith;
    off_t soff = sdp->bybs, roff = sdp->roff;
    size_t todo = 0, size = sdp->size;
    ssize_t rb = 0;
    void *addr;
    sdp->diag[SGV2_DIAG_READ] ++;
    vdiftrace(5, VDT("stripe_read(%s)\n"), sdp->vs->fuse);
    for (ii = sdp->zero; ii < sdp->sgap && size > 0; ii++) {
        ith = sdp->stripe[ii].sfrag;
        if (soff <= roff && roff < soff + ith->byib) {
            sdp->stripe[ii].mcpys ++;
            addr = (void*)ith->addr + (roff - soff);
            todo = soff + ith->byib - roff;
            if (todo > size) todo = size;
            /* this is the only place bytes get moved */
            vdiftrace(4, VDT("Reading %lu from %d-th frag of "
                "stripe (%lu:%lu) %s\n"),
                todo, ii, roff, size, lab);
            if (memory_check(buf, addr, todo, sdp, ith)) break;
            memcpy(buf, addr, todo);
            /* advance pointers */
            size -= todo;
            roff += todo;
            buf += todo;
            rb += todo;
        }
        soff += ith->byib;
    }
    if (rb != sdp->size) {
        vdiferror(sdp->fip, VDT("incomplete read  %lu for %lu (read %lu)"
            " todo:%lu after %d-th frag of stripe (%lu:%lu) %s FAIL-ERR.\n"),
            sdp->roff, sdp->size, rb,
            todo, ii-1, sdp->bybs, sdp->bybs + sdp->byis, lab);
        stripe_bread(-1, sdp, VDX("read"));
        stripe_trace(-1, sdp, lab);
        return(-EFAULT);    /* stripe_read incomplete read */
    }
    return(rb);
}

/*
 * Supply read requests moving the stripe forwards.  We know that the
 * request originates in the stripe, so we can satisfy that part first,
 * and then walk the stripe forward until we are done.
 *
 * stripe_read() respects sdp->roff and sdp->size, so we'll adjust them
 * as well as the buf pointer.
 */
static int stripe_rdfw(SGV2sdata *sdp, char *buf)
{
    off_t roff = sdp->roff;
    size_t size = sdp->size, todo = size, done = 0, td0 = 0, td1 = 0;
    char *tdx = "";
    sdp->diag[SGV2_DIAG_RDFW] ++;
    vdiftrace(5, VDT("stripe_rdfw(%s)\n"), sdp->vs->fuse);
    vdiftrace(1, VDT("stripe_rdfw need %lu:%lu\n"), roff, size);
    while (todo > 0) {
        int rb = 0;
        /* read all or part of the stripe */
        sdp->size = sdp->bybs + sdp->byis - sdp->roff;
        if (sdp->size > todo) sdp->size = todo;
        /* for debugging */
        if (vdifuse_debug>1) {
            if (td0 == 0) td0 = sdp->size;
            else if (td1 == 0) td1 = sdp->size;
            else tdx = "+...";
        }
        rb = stripe_read(sdp, buf, "rdfw");
        if (rb < 0) break;
        /* ok, account for rb bytes read */
        todo -= rb;
        done += rb;
        buf += rb;
        sdp->roff += rb;
        /* and move forward */
        if (todo == 0) break;
        vdiftrace(4, VDT("stripe_rdfw calling...\n"));
        if (stripe_walk(sdp, 1)) break;
        stripe_bread(3, sdp, VDX("rdfw"));
    }
    if (done != size) {
        stripe_bread(-1, sdp, VDX("fEIO"));
        vdiftrace(-1,
            VDT("stripe_rdfw(%lu != %lu) at %lu trying %lu FAIL-IGN.\n"),
            done, size, roff, todo);
    }
    vdiftrace(1, VDT("stripe_rdfw read %lu:%lu[%lu+%lu%s]\n"),
        roff, done, td0, td1, tdx);
    return(done);
}

/*
 * Supply read requests moving the stripe backwards.  We follow the same
 * logic as stripe_rdfw() except we fill the buffer from the back end,
 * and walk forward.
 */
static int stripe_rdbw(SGV2sdata *sdp, char *buf)
{
    off_t roff = sdp->roff;
    size_t size = sdp->size, todo = size, done = 0, td0 = 0, td1 = 0;
    char *tdx = "";
    sdp->diag[SGV2_DIAG_RDBW] ++;
    vdiftrace(5, VDT("stripe_rdbw(%s)\n"), sdp->vs->fuse);
    vdiftrace(1, VDT("stripe_rdbw need %lu:%lu\n"), roff, size);
    buf += size;
    sdp->roff += size;
    /* first pass request is just beginning of existing stripe */
    sdp->size = sdp->roff - sdp->bybs;
    while (todo > 0) {
        int rb = 0;
        /* read all or part of the stripe */
        if (sdp->size > todo) sdp->size = todo;
        if (vdifuse_debug>1) {
            if (td0 == 0) td0 = sdp->size;
            else if (td1 == 0) td1 = sdp->size;
            else tdx = "+...";
        }
        sdp->roff -= sdp->size;
        buf -= sdp->size;
        rb = stripe_read(sdp, buf, "rdbw");
        if (rb < 0) break;
        /* ok, account for rb bytes read */
        todo -= rb;
        done += rb;
        /* and move backwards */
        if (todo == 0) break;
        vdiftrace(4, VDT("stripe_rdbw calling...\n"));
        if (stripe_walk(sdp, -1)) break;
        stripe_bread(3, sdp, VDX("rdbw"));
        /* after the first all reads try to do entire stripe */
        sdp->size = sdp->bybs - roff;
    }
    if (done != size) {
        stripe_bread(-1, sdp, VDX("bEIO"));
        vdiftrace(-1, VDT("short stripe_rdbw(%lu != %lu) at %lu trying %lu\n"),
            done, size, roff, todo);
    }
    vdiftrace(1, VDT("stripe_rdbw read %lu:%lu[%lu+%lu%s]\n"),
        roff, done, td0, td1, tdx);
    return(done);
}

/*
 * Use mkstemp() to generate a unique file descriptor for tracking the
 * non-existant sgv2 file we are opening.
 */
static int generate_fh(void)
{
    char *tempname = malloc(strlen(P_tmpdir) + 32);
    int fh;
    if (!tempname) return(perror("malloc"),vorrfd);
    strcpy(tempname, P_tmpdir);
    strcat(tempname, "/");
    strcat(tempname, "vdifuse_sgv2_XXXXXX");
    fh = mkstemp(tempname);
    if (fh < 0) perror("generate_fh");
    vdiftrace(1, VDT("Generated FH %d (%s) which is not %d or %d\n"),
        fh, tempname, realfd, vorrfd);
    unlink(tempname);
    free(tempname);
    return(fh >= 0 ? fh : vorrfd);
}
static void release_fh(int fh)
{
    vdiftrace(1, VDT("Releasing FH %d\n"), fh);
    if (fh != vorrfd) close(fh);
}

/*
 * Open the fragments which form the sequence.
 *
 * Cf. finalize_sgv2_sequence(): the seq anc entry points to a list
 * of fragments, each of which has an anc entry with the SGInfo data.
 * So each fragment gets sg_open()'d and a pointer to the sgi data must
 * be held in the ffi->sfrag area for eventual release/read use.
 * The global sdata area holds access information for using these.
 *
 * We follow the logic of finalize_sgv2_sequence(), without all the
 * error checking, since it was already done....
 *
 * The return value is one of the valid fd's under which further access
 * will be indexed in the master table of open sequences.
 *
 * For sane timing, we overwrite the 'maximum frame count'
 * with the value specified in the parameters area, the
 * "parameters global frame count max" (pgfcmx).
 *
 * Note that ffi is a temporary pointer--see update_sgv2_seq() below.
 */
int open_sgv2_seq(VDIFUSEntry *vs, FFInfo *ffi)
{
    VDIFUSEntry *vdccs = current_cache_start();
    VDIFUSEntry *vsanc = vdccs + vs->cindex, *vfrag, *vfanc;
    SGV2sdata *sdp;
    SGV2sfrag *sfp;
    int num, ccount = 0, operrs = 0, msbs;
    uint32_t fcmx = 0, pgfcmx = vdccs->u.vpars.pkts_per_sec - 1;
    char *bf;

    vdiftrace(5, VDT("open_sgv2_seq(%s)\n"), vs->fuse);
    ffi->fh = vorrfd;
    ffi->numb = vs->u.vfuse.st_nlink;
    ffi->stype = vs->stype;

    /* first set up the private stripe data */
    sdp = (SGV2sdata *)(ffi->sdata = calloc(1, sizeof(SGV2sdata)));
    if (!ffi->sdata) return(perror("open_sgv2_seq:calloc(sdata)"),vorrfd);
    vdiftrace(3, VDT("sdp at %p:%lu\n"), sdp, sizeof(SGV2sdata));
    sdp->vs = vs;
    sdp->numb = ffi->numb;
    sdp->bygt = vs->u.vfuse.st_size;
    sdp->msbs = 0x7FFFFFFF;     /* big enough */
    sdp->page = 32 * sysconf(_SC_PAGESIZE);

    /* set up fragments now; loading ancillary data */
    sfp = (SGV2sfrag *)(ffi->sfrag = calloc(ffi->numb, sizeof(SGV2sfrag)));
    if (!ffi->sfrag) return(perror("open_sgv2_seq:calloc(sfrag)"),vorrfd);
    vdiftrace(3, VDT("sfp at %p:%lu\n"), sfp, ffi->numb * sizeof(SGV2sdata));

    for (num = 0; num < ffi->numb; num++, sfp++) {
        if (ccount == VDIFUSE_MAX_SEQI) {
            vsanc = vdccs + vsanc->cindex;
            ccount = 0;
        }
        vfrag = vdccs + vsanc->u.vseqi[ccount++];
        vfanc = vdccs + vfrag->cindex;
        sfp->sgi = (SGInfo *)vfanc->u.voids;
        sfp->sgi->name = vfanc->path;           /* attach sg access name */
        msbs = member_init(sfp, pgfcmx);
        if (msbs < sdp->msbs) sdp->msbs = msbs;

        /* get cached info on the fragment */
        bf = sg_repstr(sfp->sgi, "               :");
        vdiftrace(-1, VDT("\n%s"), bf);

        /* the next few lines are uncessary if per-member cached fcmx is ok */
        if (sfp->sgi->frame_cnt_max > fcmx) fcmx = sfp->sgi->frame_cnt_max;
        if (pgfcmx > 1 && pgfcmx < SG_FR_CNT_MAX)
            sfp->sgi->frame_cnt_max = pgfcmx;

        sdp->stripe[num].index = num;
        sdp->stripe[num].mcpys = 0;
        sdp->stripe[num].sfrag = sfp;
        sfp->fip = 0;   /* pending update with permanent link */
        operrs |= sfp->err;
        if (sfp->err) vdiftrace(-1,
            VD3("open_sgv2_seq(%s)#%02d:%x msbs %d ",
                "err %x with %d pps -> %d pps FAIL-INI.\n"),
            sfp->sgi->name, sfp->sgi->smi.mmfd, sfp->err, sdp->msbs,
            operrs, fcmx, pgfcmx);
            /* possible action is to delete the member */
    }
    operrs |= stripe_init(sdp, pgfcmx);
    if (operrs) {
        vdiftrace(-1 , VDT("Corrupt file %s [error %X]\n"), vs->fuse, operrs);
        ffi->fh = vorrfd;   /* mark it bad */
    } else {
        ffi->fh = generate_fh();
        vproc_status_subdir(ffi, "status", operrs?"SGV2-errors":"SGV2-open");
        difxmsg_open_message(vs->fuse);     /* DIFXMESSAGE */
    }
    return(ffi->fh);
}

/*
 * callback from do_vorr_open() for access to cached ffinfo data
 * (After the ffi open request succeeds, the ffi data is cached elsewhere.)
 * We need to save access to ffi for vdiferror(ffi...) calls.
 */
void update_sgv2_seq(FFInfo *ffi)
{
    int num;
    SGV2sdata *sdp = (SGV2sdata *)ffi->sdata;
    sdp->fip = ffi;
    for (num = 0; num < sdp->numb; num++)
        sdp->stripe[num].sfrag->fip = ffi;
    vdiferror(ffi, VDT("opened\n"));
}

/*
 * Dump the diagnostics to the trace.
 */
static void diag_counts(off_t *diag, const FFInfo *ffi)
{
    char buf[160];
    snprintf(buf, sizeof(buf),
        VDT("Diag: move  %7lu trace %7lu walk  %7lu "
                  "vern  %7lu shown %7lu find  %7lu\n"),
        diag[SGV2_DIAG_MOVE], diag[SGV2_DIAG_TRACE], diag[SGV2_DIAG_WALK],
        diag[SGV2_DIAG_VERN], diag[SGV2_DIAG_SHOWN], diag[SGV2_DIAG_FIND]);
    vproc_update_file(ffi, "counts", buf, VPROC_TRUNCATE);
    vdiftrace(-1, buf);

    snprintf(buf, sizeof(buf),
        VDT("Diag: doit  %7lu read  %7lu rdfw  %7lu "
                  "rdbw  %7lu bread %7lu spare %7lu\n"),
        diag[SGV2_DIAG_DOIT], diag[SGV2_DIAG_READ], diag[SGV2_DIAG_RDFW],
        diag[SGV2_DIAG_RDBW], diag[SGV2_DIAG_BREAD], diag[SGV2_DIAG_SPARE]);
    vproc_update_file(ffi, "counts", buf, VPROC_LENGTHEN);
    vdiftrace(-1, buf);
}

/*
 * Close the open fragments
 */
void release_sgv2_seq(FFInfo *ffi)
{
    int num;
    SGV2sdata *sdp = (SGV2sdata *)ffi->sdata;
    SGV2sfrag *sfp = (SGV2sfrag *)ffi->sfrag;
    vdiftrace(5, VDT("release_sgv2_seq(%s)\n"), sdp->vs->fuse);
    for (num = 0; num < ffi->numb; num++, sfp++) {
        vdiftrace(1, VDT("release_sgv2_seq(%s)@%d:%x\n"),
            sfp->sgi->name, sfp->sgi->smi.mmfd, sfp->err);
        if ((sfp->err & SGV2_ERR_REOPEN) == 0)
            sg_close(sfp->sgi);
    }
    vproc_update_file(ffi, "status", "SGV2-closed", VPROC_IF_FOUND);
    vdiferror(ffi, VDT("closed\n"));
    difxmsg_release_message(sdp->vs->fuse);
    diag_counts(sdp->diag, sdp->fip);
    free(ffi->sfrag);
    free(ffi->sdata);
    release_fh(ffi->fh);
    return;
}

/*
 * For development/diagnostic/debugging use
 */
static void show_sgv2_state(SGV2sdata *sdp)
{
    static unsigned long cnt = 0;
    if ((cnt % 50) == 0) {
        vdiftrace(3, VDT(">>>READ[%lu] %lu at offset %lu\n"),
            cnt++, sdp->size, sdp->roff);
        vdiftrace(3, VDT("   read_sgv2_seq(%s)\n"), sdp->vs->fuse);
        stripe_bread(1, sdp, VDX("shft"));
        stripe_trace(5, sdp, "READ");
    }
}

/*
 * Support function for do_read_sgv2_seq:
 * Returns <0 for faults, >0 for new size and 0 otherwise (ok).
 */
static int is_silly_read(SGV2sdata *sdp)
{
    /* the following would be silly and returns a serious indication */
    if (sdp->roff < 0) {
        vdiferror(sdp->fip, VDT("Read EFAULT: %lu:%lu FAIL-ERR.\n"),
            sdp->roff, sdp->size);
        *(sdp->vffp) |= VDIFUSE_FFIERROR_BEFORE;
        return(-EFAULT);    /* read before file */
    }
    /* as would this attempt to read totally beyond the end of the file*/
    if (sdp->roff >= sdp->bygt) {
        vdiferror(sdp->fip, VDT("Read after EOF %lu:%lu FAIL-ERR.\n"),
            sdp->roff, sdp->size);
        /* prevent further reads */
        *(sdp->vffp) |= VDIFUSE_FFIERROR_AFTER;
        return(-EFAULT);    /* read after file */
    }
    /* truncate addresses past eof to the end of file */
    if (sdp->roff + sdp->size > sdp->bygt) {
        sdp->size -= (sdp->roff + sdp->size - sdp->bygt);
        vdiftrace(-1, VDT("Read trimmed to EOF: %lu:%lu->%lu\n"),
            sdp->roff, sdp->size, sdp->size);
        /* prevent further (repeat) reads if the request was beyond the pale */
        if (sdp->size == 0) {
            *(sdp->vffp) |= VDIFUSE_FFIERROR_EOF;
            vdiferror(sdp->fip, VDT("Read trim to EOF FAIL-ERR.\n"));
            return(-EFAULT);
        }
    }
    return(0);
}

/*
 * Support function for do_read_sgv2_seq:  shift the stripe so that
 * one of the stripe_read(), stripe_rdfw() or stripe_rdbw() works.
 * Returns <0 for faults, 0 otherwise (ok).
 */
static int shift_for_read(SGV2sdata *sdp)
{
    show_sgv2_state(sdp);
    if (stripe_move(sdp) < 0) {
        /* well, that didn't work. */
        vdiferror(sdp->fip, VDT("Seek fail EIO %lu:%lu FAIL-ERR.\n"),
            sdp->roff, sdp->size);
        stripe_bread(-1, sdp, VDX("sEIO"));
        stripe_trace(-1, sdp, "sgv2-ck");
        return(-EIO);   /* Seek fail */
    }
    if (stripe_anal(sdp)) return(-EIO); /* stripe_anal fail */
    difxmsg_read_message(sdp->roff, sdp->vs->fuse, nanf("0"));
    return(0);
}

/*
 * Read ffi->size bytes from ffi->offset in the virtual file
 * Offset must be transformed into a packet (or part thereof)
 * and the ordering among the fragments is by VDIF time.
 *
 * The logic here switches into the various ways we load the buffer.
 * We always try to fully populate the buffer (since the caller might
 * not be prepared to handle partial reads).
 *
 * We use EFAULT for things we can't or shouldn't handle (normal).
 * We use EIO for things that we should stop and take a look at.
 *
 * This function first transfers to sdp frequently used variables,
 * checks if the read request should be rejected outright (is_silly_read),
 * shift stripe so that the read request can be satisfied (shift_for_read),
 * and then finally calls the appropriate read methods.  These helper
 * functions to do these things precede this one.
 */
static int do_read_sgv2_seq(char *buf, FFInfo *ffi)
{
    SGV2sdata *sdp = (SGV2sdata *)ffi->sdata;
    int rb;

    /* if the counter is zero we are just starting */
    if (0 == sdp->diag[SGV2_DIAG_DOIT])
        vproc_update_file(ffi, "status", "SGV2-reading", VPROC_IF_FOUND);

    /* working copies for convenience and simple checking */
    sdp->diag[SGV2_DIAG_DOIT] ++;   /* bump do_read counter */
    sdp->roff = ffi->offset;        /* read (byte) offset */
    sdp->size = ffi->size;          /* number of bytes to read */
    sdp->vffp = &ffi->vffers;       /* for flagging errors */
    sdp->sowb = buf;
    sdp->eowb = buf + sdp->size;
    /* i.e. writes >= sdp->sowb && < sdp->eowb are legal */

    /* rejected outright ? */
    rb = is_silly_read(sdp);
    if (rb > 0) ffi->size = sdp->size;
    else if (rb < 0) return(rb);    /* fault */

    /* shift as needed ? */
    rb = shift_for_read(sdp);
    if (rb < 0) return(rb);         /* fault */

    /* Usually: data request is fully within the stripe */
    if (sdp->roff >= sdp->bybs &&
        sdp->roff + sdp->size <= sdp->bybs + sdp->byis) {
            int head = sdp->roff - sdp->bybs;
            int tail = (sdp->bybs + sdp->byis) - (sdp->roff + sdp->size);
            rb = (stripe_read(sdp, buf, "sgv2"));
            if (head < sdp->page || tail < sdp->page) vdifuse_bread(
                VDT("read %lu:%lu is near edge of stripe %lu:%lu did %d\n"),
                sdp->roff, sdp->roff + sdp->size,
                sdp->bybs, sdp->bybs + sdp->byis, rb);
            return(rb);
    }

    /* Or less commonly: data request starts in stripe, runs after it */
    if (sdp->roff >= sdp->bybs &&
        sdp->roff < sdp->bybs + sdp->byis) {
            rb = (stripe_rdfw(sdp, buf));
            vdifuse_bread(
                VDT("read %lu:%lu inside and after stripe %lu:%lu did %d\n"),
                sdp->roff, sdp->roff + sdp->size,
                sdp->bybs, sdp->bybs + sdp->byis, rb);
            return(rb);
    }

    /* Or rarely: data request starts before stripe, runs into it */
    if (sdp->roff < sdp->bybs &&
        sdp->roff + sdp->size <= sdp->bybs + sdp->byis) {
            rb = (stripe_rdbw(sdp, buf));
            vdifuse_bread(
                VDT("read %lu:%lu before and inside stripe %lu:%lu did %d\n"),
                sdp->roff, sdp->roff + sdp->size,
                sdp->bybs, sdp->bybs + sdp->byis, rb);
            return(rb);
    }

    vdiferror(sdp->fip,
        VDT("Read logic error %lu:%lu FAIL-ERR.\n"), sdp->roff, sdp->size);
    stripe_bread(-1, sdp, "sgv2");
    stripe_trace(-1, sdp, "sgv2-nd");
    return(-EIO);   /* Read logic error */
}

/*
 * Wrapper around the read request so that we can flush bread crumbs
 * if the read does not transfer the number of bytes as expected.
 * read_sgv2_seq() is called from vorr_read():
 *
 * vorr_read()
 *   mutex_lock
 *   do_vorr_read()
 *     read_sgv2_seq()
 *       do_read_sgv2_seq()
 *   mutex_unlock
 *  
 *  With some rework, the mutex could be pushed down to here, but it is
 *  not worth the effort to save a few ms wait if multiple clients read.
 */
int read_sgv2_seq(char *buf, FFInfo *ffi)
{
    int rb;
    rb = do_read_sgv2_seq(buf, ffi);
    if (rb != ffi->size) {
        char where[80];
        snprintf(where, sizeof(where),
            "read_sgv2_seq: %d != %ld", rb, ffi->size);
        vdifuse_flush_bread(where);
    }
    return(rb);
}

/*
 * eof
 */
