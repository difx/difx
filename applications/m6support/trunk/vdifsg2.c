/*
 * $Id: vdifsg2.c 4017 2016-06-30 18:36:03Z gbc $
 *
 * This file provides support for the fuse interface.
 * This version is rather primitive in many respects.
 *
 * This file provides glue to the sg_access library.
 *
 * FIXME:  blck numbers should be off_t throughout.
 */

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "vdifsg2.h"
#include "vdif_epochs.h"

/*
 * A general util, maybe belongs elsewhere
 * Compute and return the time since *when,
 * store the current time in *when.
 */
double secs_since(struct timeval *when)
{
    double secs;
    struct timeval now;
    gettimeofday(&now, 0);
    secs  = (double)(now.tv_sec - when->tv_sec);
    secs += (double)(now.tv_usec - when->tv_usec) * 1e-6;
    *when = now;
    return(secs);
}

/*
 * Wrapper around sg_access signature method.
 */
uint64_t sg_signature(uint32_t *vh)
{
    return(sg_get_vsig(vh, (void*)0, 0, "sgn:", (VDIFsigu*)0));
}

/*
 * A protected version of PACKET_TIME where we might have to deal
 * with invalid packets.  pt points to the target, pt is a limit,
 * and dp is the packet advance size in uint32_t words.  If pt < pl,
 * we want the start time for the member (+dp advances), if pt > pl,
 * we want the end time for the member (-dp advances).  The macro
 * PACKET_TIME only uses w1.secs_inre and w2.df_num_insec.
 *
 * This routine is called to set the beginning and end packet times
 * for a fragment block.  So coping with invalid packets must be done
 * so that block movement still works properly.
 */
static inline double packet_time(uint32_t *pt, uint32_t *pl,
    uint32_t dp, uint32_t max_frame_count, int npkts)
{
    VDIFHeader pkt;
    int frames = 0, frames_fw = 0, frames_bw = 0, tick_dn = 0, tick_up = 0;
    int fcount;
    double tt;

    while (pt < pl) {   /* want start time, forward advance */
        frames = frames_fw;
        if (((VDIFHeader *)pt)->w1.invalid) { pt += dp; frames_fw ++; }
        else break;
    }
    while (pt > pl) {   /* want end time, backward advance */
        frames = frames_bw;
        if (((VDIFHeader *)pt)->w1.invalid) { pt -= dp; frames_bw --; }
        else break;
    }
    pkt.w1.secs_inre = ((VDIFHeader *)pt)->w1.secs_inre;
    fcount = (int)((VDIFHeader *)pt)->w2.df_num_insec - frames;
    if (((VDIFHeader *)pt)->w1.invalid)
        vdifuse_trace(VDT("No valid packets in block, guessing.\n"));
        /* FIXME:lasciate ogni speraza */
    if (fcount < 0) {
        fcount += max_frame_count;      /* as correct as max_frame_count */
        pkt.w1.secs_inre --;
        tick_dn ++;
    }
    if (fcount > max_frame_count) {
        vdifuse_trace(VDT(" %d > %d\n"), fcount, max_frame_count);
        fcount -= max_frame_count;      /* as correct as max_frame_count */
        pkt.w1.secs_inre ++;
        tick_up ++;
    }
    pkt.w2.df_num_insec = fcount;
    tt = PACKET_TIME(&pkt);
    if (frames || frames_fw || frames_bw || tick_dn || tick_up)
        vdifuse_trace(VDT("invalid %s pkt %17.8lf [%d|%d %d|%d %d|%d]\n"),
            ((pt < pl) ? "start" : ((pt > pl) ? " end " : "ERROR")), tt,
            frames, frames_fw, frames_bw, tick_dn, tick_up, max_frame_count);
    return tt;
}

/*
 * The Delta(t) on the block should be somewhat sane most of the time.
 * Recall that the sub-sec is scaled by PKTDT = 1.0/PKTDT_INV
 * The routine returns non-zero if the file should be considered corrupt.
 */
static int pkt_time_chk(SGV2sfrag *sfp)
{
    double bt = sfp->ptbe, et = sfp->pten;
    double dt = et - bt;
    uint32_t mfc = sfp->sgi->frame_cnt_max;
    int npkts = sfp->nblk, pkterr, rv = 0;
    if (round(dt) > 0.0) {
        dt -= 1.0;
        dt += (mfc + 1)/((float)PKTDT_INV);
    }
    dt *= (float)PKTDT_INV;
    pkterr = round(dt) - (npkts - 1);
    if (pkterr == 0) return(0);
    if (pkterr > npkts/2 || -pkterr > npkts/2) rv = 1;
    vdifuse_trace(VDT("times: dt=%.0f on %d is %d rv=%d [fd:%d blk:%ld]\\n"),
        dt, npkts, pkterr, rv, sfp->sgi->smi.mmfd, sfp->cblk);
    rv = 0; // FIXME: may want to enable this protection
    return(rv);
}

/*
 * Developmental stuff
 */
int sg_info_size(void)
{
    return(sizeof(SGInfo));
}

/*
 * This might be a switch into methods on a per-stype basis,
 * but at the moment, only SGv2 calls for action here.
 *
 * This version now calls sg_access() to get the access info
 * for the file.  This could be moved up to an earlier point
 * in analysis, but in the current development path, it is
 * easiest to add it here.  If there are any problems, we
 * will need to invalidate this ancillary entry as well as the
 * fragment to which it refers.
 */
void attach_sgv2_anc(uint32_t vx_index)
{
    VDIFUSEntry *vc = create_cache_entry();
    VDIFUSEntry *vdccs = current_cache_start();
    VDIFUSEntry *vx = vdccs + vx_index;
    SGInfo *sgi;

    if (vdifuse_debug>3) fprintf(vdflog,
        "    attach_sgv2_anc %p[%d] for %p[%d:%d]\n",
            vc, vc->index, vx, vx_index, vx->index);

    sgi = (SGInfo *)vc->u.voids;
    vc->etype = VDIFUSE_ENTRY_ANCILLARY;
    memset(&vc->u, 0, sizeof(union vdifuse_union));
    vc->stype = VDIFUSE_STYPE_INFO;
    vc->cindex = 0;
    vc->ccount = 0;
    strcpy(vc->path, vx->path);
    strcpy(vc->fuse, vx->fuse);
    vx->cindex = vc->index;

    if (vdifuse_debug>4) fprintf(vdflog,
        "    sg_access(%s)...\n", vc->path);
    if (vdccs->u.vpars.pkts_per_sec > 1 &&
        vdccs->u.vpars.pkts_per_sec < SG_FR_CNT_MAX)
        sgi->frame_cnt_max = vdccs->u.vpars.pkts_per_sec - 1;
    sg_access(vc->path, sgi);
    /* check that it is ok */
    if (sgi->sg_version == SG_VERSION_OK_2) {
        if (vdifuse_debug>2) fprintf(vdflog,
            "    SGv2 '%s'\n", vc->path);
    } else {
        if (vdifuse_debug>0) fprintf(vdflog,
            "Problematic SGv2 file %s\n", vc->path);
        vc->etype = VDIFUSE_ENTRY_INVALID;
        vx->etype = VDIFUSE_ENTRY_INVALID;
    }

    /* nuke the mmap data and readdress the internal file pointer */
    memset(&sgi->smi, 0, sizeof(SGMMInfo));
    free(sgi->name);
    sgi->name = 0;      /* sg_access name must be dereferenced in cache */
}

/*
 * Provide a more detailed description of what is in this ancillary
 * entry.  The contents of u.voids interpreted as SGInfo are useful.
 * This function is only invoked at if (vdifuse_debug>2).
 */
int describe_ancillary_sgv2(VDIFUSEntry *vc)
{
    SGInfo *sgi = (SGInfo *)vc->u.voids;
    if (!sgi) fprintf(vdflog, "Null SGI pointer\n");
    fprintf(vdflog,
        "  (%s%s)\n"
        "  (%d=%d+%d+%d+%d pkts [%uB] %02d@%d+%d..%02d@%d+%d)\n",
        sgi->name == 0 ? "ok:" : "!!:", vc->path,
        sgi->total_pkts,
        sgi->sg_wr_pkts_bs, sgi->sg_sh_pkts,
        sgi->sg_wr_pkts_as, sgi->sg_se_pkts,
        sgi->pkt_size,
        sgi->ref_epoch, sgi->first_secs, sgi->first_frame,
        sgi->ref_epoch, sgi->final_secs, sgi->final_frame);
    return(0);
}

/*
 * Helper functions to update time entries
 *   mtime is "first" so update if frag is earlier
 *   ctime is "final" so update if frag is later
 *   atime is duration; convert from secs/frame to time_t on all
 */
static void update_first(struct stat *vfuse, uint32_t secs, uint32_t frame)
{
    if (secs < vfuse->st_mtime) {
        vfuse->st_mtime = secs;
        vfuse->st_mtim.tv_nsec = frame;
    } else if ((secs == vfuse->st_mtime) &&
               (frame < vfuse->st_mtim.tv_nsec)) {
        vfuse->st_mtim.tv_nsec = frame;
    }
}
static void update_final(struct stat *vfuse, uint32_t secs, uint32_t frame)
{
    if (secs > vfuse->st_ctime) {
        vfuse->st_ctime = secs;
        vfuse->st_ctim.tv_nsec = frame;
    } else if ((secs == vfuse->st_ctime) &&
               (frame > vfuse->st_ctim.tv_nsec)) {
        vfuse->st_ctim.tv_nsec = frame;
    }
}
static void update_duration(struct stat *vfuse, uint32_t pps, int epoch)
{
    vfuse->st_mtime += vdif_epochs[epoch];
    vfuse->st_mtim.tv_nsec *= VDIFUSE_ONE_SEC_NS / pps;
    vfuse->st_ctime += vdif_epochs[epoch];
    vfuse->st_ctim.tv_nsec *= VDIFUSE_ONE_SEC_NS / pps;
    vfuse->st_atime = vfuse->st_ctime - vfuse->st_mtime;
    vfuse->st_atim.tv_nsec = vfuse->st_ctim.tv_nsec - vfuse->st_mtim.tv_nsec;
    vfuse->st_atim.tv_nsec += VDIFUSE_ONE_SEC_NS / pps; /* final frame */
    if (vfuse->st_atim.tv_nsec >= VDIFUSE_ONE_SEC_NS) {
        vfuse->st_atim.tv_nsec -= VDIFUSE_ONE_SEC_NS;
        vfuse->st_atime ++;
    }
    if (vfuse->st_atim.tv_nsec < 0) {
        vfuse->st_atim.tv_nsec += VDIFUSE_ONE_SEC_NS;
        vfuse->st_atime --;
    }
}

/*
 * work out size of sgv2 sequences and update
 * vp->u.vfuse.st_size from the bogus starter value (0).
 * For each fragment we increment the total size by
 * total_pkts * pkt_size of that fragment using
 * the SGInfo data for that fragment.  And the
 * nlink is the total count of fragments, and we check
 * it and a few other things for consistency.
 *
 * Then update remaining vp->u.vfuse entries as with frags.
 * st_mtime (data start time), st_ctime (data end time) and
 * st_atime (total scan duration) were populated with the
 * cache creation time, originally.
 */
int finalize_sgv2_sequence(VDIFUSEntry *vp)
{
    int vdcne = current_cache_entries();
    VDIFUSEntry *vdccs = current_cache_start(), *vsanc, *vfrag, *vfanc;
    SGInfo *sgi = 0;
    nlink_t nlink = 0;
    int errs = 0, ccount = 0, psize = 0, epoch = -1;

    if (vp->u.vfuse.st_size != 0) {
        fprintf(stderr, "Seq entry %d already has size %lu\n",
            vp->index, vp->u.vfuse.st_size);
        errs ++;
    }
    if (vp->u.vfuse.st_nlink == 0) {
        fprintf(stderr, "Seq entry %d has no links\n", vp->index);
        errs ++;
    }
    if (vp->cindex == 0 && vp->cindex >= vdcne) {
        fprintf(stderr, "Seq continuation oob %u\n", vp->cindex);
        errs ++;
    }
    if (vp->u.vfuse.st_blocks != 0) {
        fprintf(stderr, "Seq entry %d already has packets %lu\n",
            vp->index, vp->u.vfuse.st_blocks);
        errs ++;
    }
    vp->u.vfuse.st_mtime = 0x7FFFFFFF;     /* big enough */
    vp->u.vfuse.st_mtim.tv_nsec = 0;
    vp->u.vfuse.st_ctime = 0;
    vp->u.vfuse.st_ctim.tv_nsec = 0;

    vsanc = vdccs + vp->cindex;
    while (!errs && (nlink < vp->u.vfuse.st_nlink)) {
        if (ccount == VDIFUSE_MAX_SEQI) {
            if (vsanc->cindex == 0 || vsanc->cindex >= vdcne) break;
            vsanc = vdccs + vsanc->cindex;
            ccount = 0;
        }
        vfrag = vdccs + vsanc->u.vseqi[ccount++];
        vfanc = vdccs + vfrag->cindex;
        sgi = (SGInfo *)vfanc->u.voids;
        vp->u.vfuse.st_size += (off_t)sgi->total_pkts * (off_t)sgi->pkt_size;
        vp->u.vfuse.st_blocks += sgi->total_pkts;
        update_first(&vp->u.vfuse, sgi->first_secs, sgi->first_frame);
        update_final(&vp->u.vfuse, sgi->final_secs, sgi->final_frame);
        if (epoch == -1) epoch = sgi->ref_epoch;
        if (psize == 0) psize = sgi->pkt_size;
        else if (psize != sgi->pkt_size) break;
        nlink++;
    }
    vp->u.vfuse.st_blksize = psize;
    update_duration(&vp->u.vfuse, vdccs->u.vpars.pkts_per_sec, epoch);

    if (nlink != vp->u.vfuse.st_nlink) {
        fprintf(stderr,
            "Missing seq entries (%lu != %lu; %u >= %u; %u != %u)\n",
            nlink, vp->u.vfuse.st_nlink, vsanc->cindex, vdcne,
            psize, sgi->pkt_size);
        errs ++;
    }
    return(errs);
}

/*
 * vorr support follows
 */

/*
 * A diagnostic function that shows what we know about
 * the ith block of the current stripe.
 * This is called once, with:  sfn == sdp->stripe[ii].sfrag
 */
static void member_show(SGV2sfrag *sfn, int ith)
{
    if (vdifuse_debug>5) fprintf(vdflog, "member_show(%s)\n", sfn->sgi->name);
    fprintf(vdflog,
        "  [%d]%04x@%d %lu/%u b/%luB i/%luB [0x%lx] a/%luB [%luB]\n"
        "    %.8lf < %.8lf < %.8lf < %.8lf\n",
        ith, sfn->err, sfn->sgi->smi.mmfd,
        sfn->cblk, sfn->sgi->sg_total_blks,
        sfn->bybb, sfn->byib,
        ((void*)sfn->addr - sfn->sgi->smi.start), sfn->byab,
        sfn->bybb + sfn->byib + sfn->byab,
        sfn->first, sfn->ptbe, sfn->pten, sfn->final); 
}

/*
 * A trace version of the above, somewhat reduced.  Since we don't
 * use the real frame rate for time calculations, the time difference
 * gets huge (+/-) and we don't have the true frame rate to get it
 * correct, so we merely flag it (+/- 6.66).
 */
static void member_trace(SGV2sfrag *sfn, double t0, char label)
{
    double d = rint((sfn->ptbe - t0)/PKTDT);
    if (d < 0) d = -6.66;
    if (d > PKTDT_INV/2) d = +6.66;
    vdifuse_trace(
        VDT("[%c] %u/%u %5g "
            "%17.8lf < %17.8lf < %17.8lf < %17.8lf "
            "%lu[%lu]%lu\n"),
        label, sfn->cblk, sfn->sgi->sg_total_blks, d,
        sfn->first, sfn->ptbe, sfn->pten, sfn->final,
        sfn->bybb, sfn->byib, sfn->byab);
}

/*
 * Move a member to an absolute block position.  Note that we
 * refuse to move outside the legal position.
 */
static void member_move(SGV2sfrag *sfp, off_t blk, int dir)
{
    uint32_t *pp, *pb, *pe, ps;
    if (vdifuse_debug>5) fprintf(vdflog,
        "member_move(%s) %s\n", sfp->sgi->name, dir>0 ? "fw" : "bw");
    if (blk < 0 || blk >= sfp->sgi->sg_total_blks) {
        sfp->err |= SGV2_ERR_BLKERR;
        vdifuse_trace(VDT("member_move %lu(<0 or >=%lu)\n"),
            blk, sfp->sgi->sg_total_blks);
        return;
    }
    /* first the blocks and bytes */
    sfp->cblk = blk;
    pp = sg_pkt_blkby(sfp->sgi, blk, &sfp->nblk, &sfp->bybb, &sfp->byab);
    if (!pp) {
        sfp->err |= SGV2_ERR_ACCESS;
        vdifuse_trace(VDT("move sg_pkt_blkby fail %s %lu %u (%lu %lu)\n"),
            sfp->sgi->name, blk, sfp->sgi->sg_total_blks, sfp->bybb,sfp->byab);
        return;
    }
    sfp->mcnt ++;
    sfp->byib = sfp->nblk * sfp->sgi->pkt_size;
    sfp->addr = pb = pp;
    /* pkt offset within read size already accounted for */
    pp += (ps = (sfp->sgi->read_size/sizeof(uint32_t))) * (sfp->nblk-1);
    pe = pp;
    /* now work out the times */
    sfp->ptbe = packet_time(pb, pe, ps, sfp->sgi->frame_cnt_max, sfp->nblk);
    sfp->pten = packet_time(pe, pb, ps, sfp->sgi->frame_cnt_max, sfp->nblk);
    /* check that the packet times are consistent with #packets in block */
    if (pkt_time_chk(sfp)) sfp->err |= SGV2_ERR_TIME_C;
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
    vdifuse_trace(VDT("init %s\n"), sfp->sgi->name);
    if (vdifuse_debug>5) fprintf(vdflog, "member_init(%s)\n", sfp->sgi->name);
    if (sfp->sgi->frame_cnt_max < pgfcmx) {
        vdifuse_trace(VDT("Fixing fcm: %d -> %d (too small)\n"),
            sfp->sgi->frame_cnt_max, pgfcmx);
        sfp->sgi->frame_cnt_max = pgfcmx;
    }
    if (sfp->sgi->frame_cnt_max > pgfcmx &&
        pgfcmx > 1 && pgfcmx < SG_FR_CNT_MAX) {
        vdifuse_trace(VDT("Fixing fcm: %d -> %d (too large)\n"),
            sfp->sgi->frame_cnt_max, pgfcmx);
        sfp->sgi->frame_cnt_max = pgfcmx;
        // FIXME: the following times might then be incorrect
    }
    sfp->err = (sg_reopen(sfp->sgi)) ? SGV2_ERR_VIRGIN : SGV2_ERR_REOPEN;
    pkt.w1.secs_inre = sfp->sgi->first_secs;
    pkt.w2.df_num_insec = sfp->sgi->first_frame;
    sfp->first = PACKET_TIME(&pkt);
    pkt.w1.secs_inre = sfp->sgi->final_secs;
    pkt.w2.df_num_insec = sfp->sgi->final_frame;
    sfp->final = PACKET_TIME(&pkt);
    sfp->mcnt = 0;
    if (sfp->final < sfp->first) sfp->err |= SGV2_ERR_TIME_F;
    if (0 < sfp->sgi->sg_sh_pkts && sfp->sgi->sg_sh_pkts < msbs)
        msbs = sfp->sgi->sg_sh_pkts;
    if (0 < sfp->sgi->sg_se_pkts && sfp->sgi->sg_se_pkts < msbs)
        msbs = sfp->sgi->sg_se_pkts;
    member_move(sfp, 0, 1);         /* start at the beginning */
    if (vdifuse_debug>1) fprintf(vdflog,
        "member_init %d#%ld %.8f..%.8f %u pps msbs %d\n",
            sfp->sgi->smi.mmfd, sfp->cblk, sfp->first, sfp->final,
            sfp->sgi->frame_cnt_max, msbs);
    vdifuse_trace(VDT("init %d#%ld %.8f..%.8f %u pps msbs %d\n"),
        sfp->sgi->smi.mmfd, sfp->cblk, sfp->first, sfp->final,
        sfp->sgi->frame_cnt_max, msbs);
    return(msbs);
}

/*
 * Generate a count of total moves within this stripe.
 */
static off_t calculate_moves(SGV2sdata *sdp)
{
    off_t scnt = 0;
    int ss;
    for (ss = 0; ss < sdp->numb; ss++) scnt += sdp->stripe[ss].sfrag->mcnt;
    return(scnt);
}

/*
 * A bread version of stripe_show.  Check the move count, and only
 * if it has changed do we issue the bread crumb report on the stripe.
 */
static void stripe_bread(SGV2sdata *sdp)
{
    static char buf[2560], tmp[80];
    double s0;
    off_t scnt = calculate_moves(sdp);
    int ss, sep, len = 0;
    if (scnt <= sdp->scnt) return;  /* nothing changed */
    sdp->diag[SGV2_DIAG_BREAD] ++;
    sdp->scnt = scnt;
    buf[0] = 0;
    s0 = floor(sdp->stripe[0].sfrag->ptbe / 10.0) * 10.0;
    vdifuse_bread(VDT("(0..%d,%d..%d,%d..%d): %lu[%lu]%lu (%lu)B at %.2f...\n"),
        ((sdp->zero == 0) ? 0 : sdp->zero - 1),
        sdp->zero, sdp->sgap - 1, sdp->sgap, sdp->numb,
        sdp->bybs, sdp->byis, sdp->byas, sdp->bygt, s0);
    for (ss = 0; ss < sdp->numb; ss++) {
        if (ss == sdp->zero)    sep = '|';
        else if (ss==sdp->sgap) sep = '%';
        else                    sep = ',';
        snprintf(tmp, sizeof(tmp), "%02d%c%02d:%lu(%.8f)",
            ss, sep, sdp->stripe[ss].sfrag->sgi->smi.mmfd,
            sdp->stripe[ss].sfrag->cblk,
            sdp->stripe[ss].sfrag->ptbe - s0);
        strncat(buf, tmp, sizeof(buf) - len);
        len += strlen(tmp);
        if ((ss%4) == 3) {
            vdifuse_bread(VDT("%s\n"), buf);
            buf[0] = 0;
            len = 0;
        }
    }
    if (buf[0]) vdifuse_bread(VDT("%s\n"), buf);
}

/*
 * A trace version of the stripe_show.  Since we only intend to call this
 * on the way out the door, we don't worry about the verbosity.
 */
static void stripe_trace(SGV2sdata *sdp, char *where, int adj)
{
    int ss;
    int z = sdp->zero, n = sdp->sgap-1, m = (z==0?0:z-1);
    off_t fragsum = 0;
    sdp->diag[SGV2_DIAG_TRACE] ++;
    vdifuse_trace(VDT("stripe_show(%s) %s (%d)\n"), sdp->vs->fuse, where, adj);
    vdifuse_trace(VDT("  (0..%d,%d..%d,%d..%d): %lu[%lu]%lu (%lu) B\n"),
        m, z, n, sdp->sgap, sdp->numb,
        sdp->bybs, sdp->byis, sdp->byas, sdp->bygt);
    vdifuse_trace(VDT("  from[%2d] %.8f..%.8f   %lu..\n"),
        z, sdp->stripe[z].sfrag->ptbe, sdp->stripe[z].sfrag->pten, sdp->bybs);
    vdifuse_trace(VDT("  thru[%2d] %.8f..%.8f ..%lu\n"),
        n, sdp->stripe[n].sfrag->ptbe, sdp->stripe[n].sfrag->pten, sdp->byas);
    for (ss = 0; ss < sdp->numb; ss++) {
        SGV2sfrag tmp;
        double t0 = sdp->stripe[0].sfrag->ptbe;
        double d = rint((sdp->stripe[ss].sfrag->ptbe - t0)/PKTDT);
        if (d > 0.5 / PKTDT) d -= (1.0/PKTDT - (sdp->fcmx+1));
        vdifuse_trace(
            VDT("%c %c%02d@%-2d#%lu[%2d:%1X]%-5g%c %lu[%lu]%lu {%lu..%lu}\n"),
            '0' + ss, (ss == sdp->zero) ? '|' : ' ',
            sdp->stripe[ss].index, sdp->stripe[ss].sfrag->sgi->smi.mmfd,
            sdp->stripe[ss].sfrag->cblk, sdp->stripe[ss].reads,
            sdp->stripe[ss].sfrag->err, d,
            (ss == sdp->sgap-1) ? '%' : ' ',
            sdp->stripe[ss].sfrag->bybb,
            sdp->stripe[ss].sfrag->byib,
            sdp->stripe[ss].sfrag->byab,
            sdp->bybs + fragsum,
            sdp->bybs + sdp->stripe[ss].sfrag->byib);
        /* make a copy of the stripe ... */
        tmp.sgi = sdp->stripe[ss].sfrag->sgi;
        tmp.first = sdp->stripe[ss].sfrag->first;
        tmp.final = sdp->stripe[ss].sfrag->final;
        tmp.err = sdp->stripe[ss].sfrag->err;
        /* ...for these debugging reports */
        member_move(&tmp, sdp->stripe[ss].sfrag->cblk - 1, -1);
        member_trace(&tmp, t0, 'A' + ss);
        member_trace(sdp->stripe[ss].sfrag, t0, '0' + ss);
        if (sdp->stripe[ss].sfrag->cblk + 1 <
            sdp->stripe[ss].sfrag->sgi->sg_total_blks) {
            member_move(&tmp, sdp->stripe[ss].sfrag->cblk + 1,  1);
            member_trace(&tmp, t0, 'a' + ss);
        }
        fragsum += sdp->stripe[ss].sfrag->byib;
    }
}

/*
 * A diagnostic function that shows what we know about the stripe.
 */
static void stripe_show(SGV2sdata *sdp, char *where, int adj)
{
    int ss, tre = 3, two = 0, one = 0, zer = 0;
    int z = sdp->zero, n = sdp->sgap-1, m = z==0?0:z-1;
    sdp->diag[SGV2_DIAG_SHOWN] ++;
    if (adj < 0) tre = two = one = zer = 0;
    if (vdifuse_debug>tre+adj) fprintf(vdflog,
        "stripe_show(%s)\n", sdp->vs->fuse);
    if (vdifuse_debug>zer+adj) fprintf(vdflog,
        "%7s (0..%d,%d..%d,%d..%d): %lu[%lu]%lu (%lu) B\n",
        where, m, z, n, sdp->sgap, sdp->numb,
        sdp->bybs, sdp->byis, sdp->byas, sdp->bygt);
    if (vdifuse_debug>two+adj) fprintf(vdflog,
        "%7s from[%2d] %.8f..%.8f   %lu..\n"
        "%7s thru[%2d] %.8f..%.8f ..%lu\n",
        where, z, sdp->stripe[z].sfrag->ptbe, sdp->stripe[z].sfrag->pten,
        sdp->bybs,
        where, n, sdp->stripe[n].sfrag->ptbe, sdp->stripe[n].sfrag->pten,
        sdp->byas);
    if (vdifuse_debug>one+adj) {
        for (ss = 0; ss < sdp->numb; ss++) {
            double d = rint((sdp->stripe[ss].sfrag->ptbe -
                             sdp->stripe[0].sfrag->ptbe)/PKTDT);
            /* use estimate of frame rate to fake it */
            if (d > 0.5 / PKTDT) d -= (1.0/PKTDT - (sdp->fcmx+1));
            fprintf(vdflog,
                "%s%c%02d@%-2d#%lu[%2d]%-5g%c%s", 
                (ss%4 == 0) ? " " : "",
                (ss == sdp->zero) ? '|' : ' ',
                sdp->stripe[ss].index, sdp->stripe[ss].sfrag->sgi->smi.mmfd,
                sdp->stripe[ss].sfrag->cblk, sdp->stripe[ss].reads, d,
                (ss == sdp->sgap-1) ? '|' : ' ',
                (ss%4 == 3) ? "\n" : "");
        }
        if (ss%4 != 0) fprintf(vdflog, "\n");
    }
    if (adj < 0) stripe_trace(sdp, where, adj);
}

/*
 * (Time) comparison function for qsort; it compares the
 * block starting times contained in sfrag->ptbe.
 */
static int stripe_comp(const void *sa, const void *sb)
{
    SBlock *a = (SBlock*)sa;
    SBlock *b = (SBlock*)sb;
    if (vdifuse_debug>6) fprintf(vdflog,
        "stripe_comp(%p[%d,%p],%p[%d,%p])\n",
            a, a->index, a->sfrag,
            b, b->index, b->sfrag);
    if (a->sfrag->ptbe < b->sfrag->ptbe) return(-1);
    if (a->sfrag->ptbe > b->sfrag->ptbe) return( 1);
    /* we should never get to this case */
    vdifuse_trace(VDT("stripe_comp %.8lf (fd %d) == %.8lf (fd %d)\n"),
        a->sfrag->ptbe, a->sfrag->sgi->smi.mmfd,
        b->sfrag->ptbe, b->sfrag->sgi->smi.mmfd);
    return(0);
}

/*
 * Sort the members of the sequence by block start time.
 */
static void stripe_sort(SGV2sdata *sdp)
{
    SBlock *sbp = sdp->stripe;
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_sort(%s)\n", sdp->vs->fuse);
    qsort(sbp, sdp->numb, sizeof(SBlock), stripe_comp);
}

/*
 * Check that a stripe is sorted, which it must always be up through
 * the gap; ie. [0..sgap] must be sorted, [sgap+1..numb-1] need not be,
 * but there is no harm in sorting them for the diagnostics.
 *
 * sgap is the index of the first fragment after the gap; if
 *      there is no gap it should be numb (initial setting).
 *
 * When we are done [0..sgap-1] should be sane and usable.
 */
static int stripe_schk(SGV2sdata *sdp)
{
    int ii, nn;
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_schk(%s)\n", sdp->vs->fuse);
    for (ii = 0; ii < sdp->numb-1; ii++)
        if (stripe_comp((void*)&sdp->stripe[ii],
                        (void*)&sdp->stripe[ii+1]) >= 0) {
            if (ii < sdp->sgap) {   /* ii+1 == sdp->sgap */
                if (vdifuse_debug>0) fprintf(stderr,
                    "stripe_schk fail %d v %d (%.8lf v %.8lf)\n", ii, ii+1,
                    sdp->stripe[ii].sfrag->ptbe,
                    sdp->stripe[ii+1].sfrag->ptbe);
                vdifuse_trace(
                    VDT("stripe_schk fail %d v %d (%.8lf v %.8lf)\n"),
                    ii, ii+1, sdp->stripe[ii].sfrag->ptbe,
                    sdp->stripe[ii+1].sfrag->ptbe);
                stripe_show(sdp, "sortchk", -1);
                for (ii = 0; ii < sdp->numb; ii++)
                    member_show(sdp->stripe[ii].sfrag, ii);
                return(SGV2_ERR_SRTCHK);
            } else {                /* sort last part */
                nn = sdp->numb - (ii + 1);
                if (vdifuse_debug>2) fprintf(vdflog,
                    "Sort check resort 0..%d..%d,%d; (at %d sort %d)\n",
                    sdp->zero, sdp->sgap-1, sdp->sgap, ii+1, nn);
                qsort(sdp->stripe + ii + 1, nn, sizeof(SBlock), stripe_comp);
            }
        } /* else we have the proper order:  stripe[ii] < stripe[ii+1] */
    if (vdifuse_debug>1) fprintf(vdflog,
        "Stripe %lu..%lu covers %.8lf..%.8lf\n",
        sdp->bybs, sdp->bybs + sdp->byis,
        sdp->stripe[sdp->zero].sfrag->ptbe,
        sdp->stripe[sdp->sgap-1].sfrag->pten);
    if (vdifuse_debug>1) vdifuse_trace(
        VDT("stripe %lu..%lu covers %.8lf..%.8lf\n"),
        sdp->bybs, sdp->bybs + sdp->byis,
        sdp->stripe[sdp->zero].sfrag->ptbe,
        sdp->stripe[sdp->sgap-1].sfrag->pten);
    return(0);
}

/*
 * Set the "zero" point based on the edge.  The blocks should
 * be time-sorted at this point, and any leading reads marked
 * -1 rather than zero, should form the basis of the "zero" group.
 * We don't care about direction or time, since stripe_walk() did.
 */
static int stripe_zero(SGV2sdata *sdp)
{
    int ii;
    sdp->sgap = sdp->numb;  /* set stripe gap */
    for (ii = 0; ii < sdp->numb; ii++)
        if (sdp->stripe[ii].reads == 0) break;
    sdp->zero = ii;         /* set stripe zero */
    return(sdp->zero);
}

/*
 * Support for strip_dgap(): check if any predecessors of the jth
 * block are earlier but no later than pten.  If so, make the move.
 * Return value indicates if there is a gap or not after all tries
 * have been exhausted.
 */
static int predecessors_leave_gap(int jj, SGV2sfrag *jth,
    double pten, double mint)
{
    double dt = jth->ptbe - pten, odt;
    int fd = jth->sgi->smi.mmfd;
    while (dt >= mint && jth->cblk > 0) {
        /* peek at jth's predecessor */
        SGV2sfrag tmp = *jth;
        member_move(&tmp, jth->cblk - 1, -1);
        odt = dt;
        dt = tmp.ptbe - pten;
        if (dt > 0.0) {
            /* tmp is earlier than jth, but later than ith */
            if (vdifuse_debug>4) fprintf(vdflog,
                "Gap%02d@%-2d: have %+.8lf = (jth-be)%.8lf - (ith-en)%.8lf\n"
                "Gap%02d@%-2d: take %+.8lf = (jm1-be)%.8lf - (ith-en)%.8lf**\n",
                jj, fd, odt, jth->ptbe, pten, jj, fd, dt, tmp.ptbe, pten);
            *jth = tmp;
        } else {
            if (vdifuse_debug>4) fprintf(vdflog,
                "Ngp%02d@%-2d: keep %+.8lf = (jth-be)%.8lf - (ith-en)%.8lf**\n"
                "Ngp%02d@%-2d: skip %+.8lf = (jm1-be)%.8lf - (ith-en)%.8lf\n",
                jj, fd, odt, jth->ptbe, pten, jj, fd, dt, tmp.ptbe, pten);
            dt = odt;
            break;
        }
    }
    if (vdifuse_debug>3 && (dt >= mint)) fprintf(vdflog,
        "predecessors_leave_gap() left gap %.8lf > %.8lf\n", dt, mint);
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
            if (dt < 0.0) {
                /* found an earlier block */
                if (vdifuse_debug>3) fprintf(vdflog,
                    "successor_closes_gap() w/ gap(%+.8lf = %.8lf - %.8lf)\n",
                    dt, tmp.pten, ptbe);
                return(1);
            }
        }
    }
    if (vdifuse_debug>3) fprintf(vdflog, "successor_closes_gap(), NOT\n");
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
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_sqze(%s)\n", sdp->vs->fuse);
    jj = stripe_zero(sdp) + 1;  /* and also set stripe zero */
    ith = sdp->stripe[ii = sdp->zero].sfrag;
    while (jj < sdp->numb) {
        SGV2sfrag *jth = sdp->stripe[jj].sfrag;
        (void)predecessors_leave_gap(jj, jth, ith->pten, sdp->msbs*PKTDT);
        jj++;
    }
    stripe_sort(sdp);
}
static void stripe_dgap(SGV2sdata *sdp)
{
    int jj, ii;
    SGV2sfrag *ith;
    SBlock tmp;
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_dgap(%s)\n", sdp->vs->fuse);
    jj = stripe_zero(sdp) + 1;  /* and also set stripe zero */
    ith = sdp->stripe[ii = sdp->zero].sfrag;
    while (jj < sdp->numb) {
        SGV2sfrag *jth = sdp->stripe[jj].sfrag;
        /* shift jth block earlier as needed to close any gap */
        if (predecessors_leave_gap(jj, jth, ith->pten, sdp->msbs*PKTDT)) {
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
    if (vdifuse_debug>3 &&
        (sdp->sgap < sdp->numb || sdp->zero > 0)) fprintf(vdflog,
        "Gap after ii=%d < sgap=%d < jj=%d  numb=%d\n",
        ii, sdp->sgap, jj, sdp->numb);

    /* blocks after the gap should still be moved back if possible */
    while (jj < sdp->numb) {
        SGV2sfrag *jth = sdp->stripe[jj].sfrag;
        /* shift jth block earlier as needed to close any gap */
        if (!predecessors_leave_gap(jj, jth, ith->pten, sdp->msbs*PKTDT)) {
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
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_boff(%s)\n", sdp->vs->fuse);
    sdp->bybs = 0;
    sdp->byis = 0;
    sdp->byas = 0;
    /* account for bytes before the stripe */
    for (ii = 0; ii < sdp->zero && ii < sdp->numb; ii++) {
        ith = sdp->stripe[ii].sfrag;
        sdp->bybs += ith->bybb;
        sdp->bybs += ith->byib;
        if (ith->byab > 0) {
            fprintf(stderr, "Error on %d < \"zero\"\n", ii);
            vdifuse_trace(VDT("Error on %d < \"zero\"\n"), ii);
        }
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
    if (vdifuse_debug>3) fprintf(vdflog,
        "  stripe_boff: %lu+%lu+%lu = %lu B (%lu B)\n",
        sdp->bybs, sdp->byis, sdp->byas, sdp->bygt, sdp->vs->u.vfuse.st_size);
    if (sdp->bygt != sdp->vs->u.vfuse.st_size)
        vdifuse_trace(VDT("Inconsistent sizing: %lu != %lu\n"),
            sdp->bygt, sdp->vs->u.vfuse.st_size);
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
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_init(%s)\n", sdp->vs->fuse);
    if (vdifuse_debug>3) fprintf(vdflog,
        "   init min sh block size %d (%.8lf) fcmx %u\n",
        sdp->msbs, sdp->msbs*PKTDT, fcmx);
    sdp->zero = 0;
    sdp->sgap = sdp->numb;
    sdp->fcmx = fcmx;
    /* hearafter, zero managed by walk, sgap by dgap */
    if (vdifuse_debug>4) stripe_show(sdp, "init", 2);
    stripe_sort(sdp);
    stripe_dgap(sdp);
    stripe_boff(sdp);
    if (vdifuse_debug>2) stripe_show(sdp, "init", -1);
    return(stripe_schk(sdp));
}

/*
 * Write helper: check that the bytes are coming from legal places
 * and that they are going to legal places.
 */
static int memory_check(char *buf, void *addr, size_t todo,
    SGV2sdata *sdp, SGV2sfrag *ith)
{
    SGMMInfo *smi = &ith->sgi->smi;
    if (addr < smi->start || addr + todo > smi->eomem) {
        fprintf(stderr, "Reading outside fragment: %p<%p | %p>%p\n",
            addr, smi->start, addr + todo, smi->eomem);
        vdifuse_trace(VDT("read outside fragment: %p<%p | %p>%p\n"),
            addr, smi->start, addr + todo, smi->eomem);
        return(1);
    }
    if (buf < sdp->sowb || buf + todo > sdp->eowb) {
        fprintf(stderr, "Writing outside buffer: %p<%p | %p>%p\n",
            buf, sdp->sowb, buf + todo, sdp->eowb);
        vdifuse_trace(VDT("write outside buffer:  %p<%p | %p>%p\n"),
            buf, sdp->sowb, buf + todo, sdp->eowb);
        return(1);
    }
    return(0);
}

/*
 * In the dir < 0 case, we move some members forward so that we end
 * up with the stripe immediately before (or including the edge).
 * We get called once per fragment, which should be adequate if
 * we are going to succeed.
 *
 * TODO: we could move more than one fragment, up to the gap.
 *
 * Returns 1 if we are satisfied, otherwise 0 to try again.
 */
static int stripe_vern(SGV2sdata *sdp, off_t bydg)
{
    int nblk, ii;
    off_t obybs = sdp->bybs;
    sdp->diag[SGV2_DIAG_VERN] ++;
    SGV2sfrag *ith;
    if (sdp->bybs + sdp->byis >= bydg) return(1);
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_vern(%s)\n", sdp->vs->fuse);
    vdifuse_trace(VDT("vern: %lu < %lu\n"), sdp->bybs + sdp->byis, bydg);
    /* else (sdp->bybs + sdp->byis < bydg) and we have to shift stripe */
    ith = sdp->stripe[0].sfrag;
    nblk = ith->cblk + 1;
    if (nblk >= ith->sgi->sg_total_blks) nblk = ith->sgi->sg_total_blks-1;
    if (ith->cblk == nblk) {
        /* don't want to use this */
        sdp->stripe[0].reads = -1;
    } else {
        /* move it forward */
        member_move(ith, nblk, 1);
    }
    /* reset reads so that dgap and zero can be set normally */
    for (ii = 0; ii < sdp->numb; ii++) sdp->stripe[0].reads = 0;
    stripe_sort(sdp);
    stripe_dgap(sdp);
    stripe_boff(sdp);
    stripe_bread(sdp);
    /* the requested motion should always move us forward */
    if (sdp->bybs <= obybs) {
        vdifuse_trace(VDT("vern: %lu <= %lu, bailing\n"), sdp->bybs, obybs);
        return(1);
    }
    return(0);
}

/*
 * Shift the (active) stripe later (dir>0) or earlier (dir<0)
 * If there is a serious issue, return nonzero.
 *
 * if dir > 0 we want the stripe after  > pten(sgap-1) = t(bybs+byis)
 * if dir < 0 we want the stripe before < ptbe(zero)   = t(bybs)
 *
 * We mark the reads 0 if the new block looks like it should be part of
 * the new stripe, and -1 if not.  (Reads is otherwise just a diagnostic.)
 *
 * We know here that the current stripe blocks are too late or too soon,
 * so we can just select predecessors or successors and follow the steps
 * to make the stripe usable similar to what stripe_init() does.
 *
 * The tricky bit is that we want to make sure that the new stripe abuts
 * the existing one so that reads at the edge [read_rdfw() and read_rdbw()]
 * will be able to succeed.  The stripe_dgap() logic appears to be sound
 * against dir > 0, but if dir < 0, we might wind up with a gap before
 * the bytes we're looking for.  stripe_vern() attemps to address this.
 */
static int stripe_walk(SGV2sdata *sdp, int dir)
{
    int ii, nblk, cblk, ord, nrd; // , vcnt = 0;
    double edge, ptbe, pten, part1, part2;
    SGV2sfrag *ith;
    struct timeval now;
    off_t bydg = 0;
    (void)secs_since(&now);
    sdp->diag[SGV2_DIAG_WALK] ++;
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_walk(%s)\n", sdp->vs->fuse);
    if (vdifuse_debug>3) stripe_show(sdp, "prewalk", 0);

    if (dir == 0) {
        fprintf(stderr, "Illegal stripe shift of 0 requested!!!\n");
        vdifuse_trace(VDT("illegal stripe shift of 0\n"));
        return(1);
    } else if (dir > 0) {
        edge = sdp->stripe[sdp->sgap - 1].sfrag->pten;
        /* bydg would be sdp->bybs + sdp->byis; but it is unused for dir > 0 */
    } else if (dir < 0) {
        edge = sdp->stripe[sdp->zero].sfrag->ptbe;
        bydg = sdp->bybs;
    }

    /* move stripe to predecessors/successors */
    for (ii = 0; ii < sdp->numb; ii++) {
        ith = sdp->stripe[ii].sfrag;

        /* work out the new blk # which must be in the legal range */
        cblk = ith->cblk;
        ptbe = ith->ptbe;
        pten = ith->pten;
        nblk = ith->cblk + dir;
        if (nblk < 0) nblk = 0;
        if (nblk >= ith->sgi->sg_total_blks) nblk = ith->sgi->sg_total_blks-1;
        ord = sdp->stripe[ii].reads;    /* just for diagnostics below */

        /* do something based on block placement relative to edge */
        if (ith->cblk == nblk) {
            /* this can only happen at the extremes */
            if (dir > 0) nrd = (pten > edge) ? 0 : -1;
            else         nrd = (ptbe < edge) ? 0 : -1;
            if (vdifuse_debug>3) fprintf(vdflog,
                "  stripe%02d@%-2d #%u %.8lf (%d) == same\n",
                ii, ith->sgi->smi.mmfd, cblk, ptbe, ord);
        } else if (dir > 0 && pten > edge) {
            nrd = 0;
            if (vdifuse_debug>3) fprintf(vdflog,
                "  stripe%02d@%-2d #%u %.8lf (%d) == greater (%.8lf > %.8lf)\n",
                ii, ith->sgi->smi.mmfd, cblk, ptbe, ord, pten, edge);
        } else if (dir < 0 && ptbe < edge) {
            nrd = 0;
            if (vdifuse_debug>3) fprintf(vdflog,
                "  stripe%02d@%-2d #%u %.8lf (%d) == lesser (%.8lf < %.8lf)\n",
                ii, ith->sgi->smi.mmfd, cblk, ptbe, ord, ptbe, edge);
        } else {
            nrd = 0;
            member_move(ith, nblk, dir);
            if (vdifuse_debug>3) fprintf(vdflog,
                "  stripe%02d@%-2d #%u %.8lf (%d) -> #%u %.8lf %lu\n",
                ii, ith->sgi->smi.mmfd, cblk, ptbe, ord,
                nblk, ith->ptbe, ith->bybb);
        }
        sdp->stripe[ii].reads = nrd;
    }
    part1 = secs_since(&now);

    /* then always make sure we have a sane stripe */
    stripe_sort(sdp);
    stripe_dgap(sdp);
    stripe_boff(sdp);
    if (vdifuse_debug>2) stripe_show(sdp, "walkend", 0);
    part2 = secs_since(&now);

    /* give control to stripe_vern for eventually quitting */
    while (dir < 0) if (stripe_vern(sdp, bydg)) break;

    if (vdifuse_debug>1) fprintf(vdflog,
        "stripe_walk(%s) took (%f+%f) %f\n",
        sdp->vs->fuse, part1, part2, part1 + part2);
    return(stripe_schk(sdp));
}

/*
 * Place a stripe at an absolute position determined by a set of
 * block numbers.  This is a stripped-down version of stripe_walk()
 * since we don't have as much thinking to do about block positions.
 */
static void stripe_spot(SGV2sdata *sdp, off_t cblks[])
{
    int ii, dir;
    struct timeval now;
    double part1, part2;
    off_t oblks = 0, nblks = 0;
    (void)secs_since(&now);
    for (ii = 0; ii < sdp->numb; ii++) {
        dir = (cblks[ii] >= sdp->stripe[ii].sfrag->cblk) ? 1 : -1;
        oblks += sdp->stripe[ii].sfrag->cblk;
        nblks += cblks[ii];
        member_move(sdp->stripe[ii].sfrag, cblks[ii], dir);
        sdp->stripe[ii].reads = 0;
    }
    part1 = secs_since(&now);
    stripe_sort(sdp);
    stripe_sqze(sdp);
    stripe_dgap(sdp);
    stripe_boff(sdp);
    part2 = secs_since(&now);
    vdifuse_trace(VDT("stripe_spot took (%f+%f) %f <%lu> -> <%lu>\n"),
        part1, part2, part1 + part2, oblks/sdp->numb, nblks/sdp->numb);
    ii = stripe_schk(sdp);
    if (ii) {
        fprintf(vdflog, "stripe_spot fail %d\n", ii);
        vdifuse_trace(VDT("stripe_spot fail %d\n"), ii);
    }
}

/*
 * Make a large change in the stripe position, either to later blocks
 * (dir>1) or earlier (dir<-1) ones.  We can use stripe_walk() to
 * reposition the fragments, but need to do a bit more work to see if
 * we have found the appropriate part of the file.
 *
 * Should probably use a binary search strategy, but a one-shot plan
 * might work:  assume a uniform distribution, and compute new blocks
 * based on that.  Then:
 * a) move each member to this offset using stripe_spot
 * b) walk members +/- until before < read-range < after
 * which happens naturally once we return to caller!
 *
 * Returns nonzero if it has adjusted the stripe and not succeeded.
 * Returns zero if it did nothing or succeeded.
 */
static int stripe_find(SGV2sdata *sdp, int dir)
{
    off_t nblks[VDIFUSE_MAX_SEQI];
    int ii;
    double ffrac;
    SGInfo *sgi;
    sdp->diag[SGV2_DIAG_FIND] ++;
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_find(%s)\n", sdp->vs->fuse);
    ffrac = (double)(sdp->roff + sdp->size/2) / (double)sdp->bygt;
    vdifuse_trace(VDT("stripe_find to %lf within fragments\n"), ffrac);
    for (ii = 0; ii < sdp->numb; ii++) {
        sgi = sdp->stripe[ii].sfrag->sgi;
        nblks[ii] = rint((double)sgi->sg_total_blks * ffrac);
        /* just to be safe: keep it legal */
        if (nblks[ii] >= sgi->sg_total_blks)
            nblks[ii] = sgi->sg_total_blks - 1;
        else if (nblks[ii] <= 0) nblks[ii] = 0;
    }
    stripe_spot(sdp, nblks);
    return(0);
}

/*
 * Consider a large shift of the stripe position via stripe_find().
 * The distance is to the middle of the read request.
 *
 * Prior to the jump, we save position so that on failure of stripe_find()
 * we are in a position to restore it (and in fact do so).
 */
static int stripe_jump(SGV2sdata *sdp)
{
    off_t cblks[VDIFUSE_MAX_SEQI];
    int dir = 0, ii;
    off_t distance = 0;
    if (sdp->roff + sdp->size < sdp->bybs) {                /* before */
        dir = -1;
        distance = sdp->bybs - (sdp->roff + sdp->size) - sdp->size/2;
    } else if (sdp->roff > sdp->bybs + sdp->byis) {         /* after */
        dir = 1;
        distance = sdp->roff - (sdp->bybs + sdp->byis) + sdp->size/2;
    }
    if (dir && (distance > STRIPE_JUMP_TRIGGER)) {
        /* save stripe position in case of stripe_find() failure */
        stripe_bread(sdp);
        for (ii = 0; ii < sdp->numb; ii++)
            cblks[ii] = sdp->stripe[ii].sfrag->cblk;
        vdifuse_trace(VDT("stripe_jump %s: %lu>trigger %lu\n"),
            (dir > 0) ? "forward" : "backward", distance, STRIPE_JUMP_TRIGGER);
        vdifuse_flush_trace();
        ii = stripe_find(sdp, dir);
        if (ii) stripe_spot(sdp, cblks);
        stripe_bread(sdp);
        vdifuse_trace(VDT("stripe covers %lu..%lu (result %d)\n"),
            sdp->bybs, sdp->bybs + sdp->byis, ii);
        vdifuse_flush_bread();
        return(1);
    }
    return(0);
}

/*
 * Check where the "offset" lies when the relative to the current stripe.
 * For nearby requests, we walk forward or backwards until we overlap.
 * For distant requests, we go hunt for it.
 *
 * Return values: 0 = no change, 1 = ok change, -1 = death.
 */
static int stripe_chck(SGV2sdata *sdp)
{
    int rv = -1, sj = 0, strikes = 0;
    off_t distance, newdist;
    struct timeval now;
    double psecs, fsecs;

    sdp->diag[SGV2_DIAG_CHECK] ++;
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_chck(%s)\n", sdp->vs->fuse);
#if STRIPE_JUMP_ENABLED
    sj = stripe_jump(sdp);
#endif /* STRIPE_JUMP_ENABLED */
    (void)secs_since(&now);

    distance = sdp->bybs - (sdp->roff + sdp->size);
    /* sdp->roff + sdp->size <= sdp->bybs */
    while (distance >= 0) {
        if (vdifuse_debug>1) fprintf(vdflog,
            "  Req. %lu:%lu fully precedes stripe by %lu\n",
            sdp->roff, sdp->size, distance);
        if (stripe_walk(sdp, -1)) break;
        newdist = sdp->bybs - (sdp->roff + sdp->size);
        if (newdist > distance) {
            if (vdifuse_debug>1) fprintf(vdflog,
                "  Distance increased to %lu (%d)\n", newdist, strikes);
            vdifuse_trace(VDT("stripe_chck A: %lu>%lu (%d)"),
                newdist, distance, strikes);
            if (++strikes > 5) break;
        }
        distance = newdist;
        rv = 1;
    }
    psecs = secs_since(&now);
    strikes = 0;

    distance = sdp->roff - (sdp->bybs + sdp->byis);
    /* sdp->roff >= sdp->bybs + sdp->byis */
    while (distance >= 0) {
        if (vdifuse_debug>1) fprintf(vdflog,
            "  Req. %lu:%lu fully follows stripe by %lu\n",
            sdp->roff, sdp->size, distance);
        if (stripe_walk(sdp,  1)) break;
        newdist = sdp->roff - (sdp->bybs + sdp->byis);
        if (newdist > distance) {
            if (vdifuse_debug>1) fprintf(vdflog,
                "  Distance increased to %lu (%d)\n", newdist, strikes);
            vdifuse_trace(VDT("stripe_chck B: %lu>%lu (%d)\n"),
                newdist, distance, strikes);
            if (++strikes > 5) break;
        }
        distance = newdist;
        rv = 1;
    }
    fsecs = secs_since(&now);

    /* the cases we can handle are consistent with this */
    if (sdp->roff < sdp->bybs + sdp->byis &&
        sdp->bybs < sdp->roff + sdp->size) {
        if (vdifuse_debug>4) fprintf(vdflog,
            "  Req. start %lu < %lu is before stripe end\n"
            "  Req. end   %lu < %lu is after stripe start\n",
            sdp->roff, sdp->bybs + sdp->byis,
            sdp->roff + sdp->size, sdp->bybs);
        rv = 0;
        if (sj) vdifuse_trace(VDT("Req in stripe: %lu<%lu..%lu<%lu\n"),
            sdp->bybs, sdp->roff, sdp->roff+sdp->size, sdp->bybs+sdp->byis);
    } else {
      if (vdifuse_debug>1) fprintf(vdflog,
            "  Req. start %lu > %lu is after stripe end, OR\n"
            "  Req. end   %lu > %lu is before stripe start\n",
            sdp->roff, sdp->bybs + sdp->byis,
            sdp->roff + sdp->size, sdp->bybs);
      vdifuse_trace(
        VDT("stripe_chck req %lu > %lu or req %lu > %lu, rv %d\n"),
        sdp->roff, sdp->bybs + sdp->byis,
        sdp->roff + sdp->size, sdp->bybs, rv);
    }

    if (vdifuse_debug>2) fprintf(vdflog,
        "stripe_chck(%s) took (%f+%f) %fs\n",
        sdp->vs->fuse, psecs, fsecs, psecs + fsecs);
    return(rv);
}

/*
 * Check me again, I'm paranoid.
 */
static int stripe_anal(SGV2sdata *sdp)
{
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_anal(%s)\n", sdp->vs->fuse);
    if (sdp->roff >= sdp->bybs &&
               sdp->roff + sdp->size <= sdp->bybs + sdp->byis) {
        if (vdifuse_debug>4) fprintf(vdflog,
            "Request %lu:%lu fully in stripe\n", sdp->roff, sdp->size);
    } else if (sdp->roff >= sdp->bybs &&
               sdp->roff < sdp->bybs + sdp->byis) {
        if (vdifuse_debug>1) fprintf(vdflog,
            "Request %lu:%lu begins in stripe, runs after\n",
            sdp->roff, sdp->size);
    } else if (sdp->roff < sdp->bybs &&
               sdp->roff + sdp->size <= sdp->bybs + sdp->byis) {
        if (vdifuse_debug>1) fprintf(vdflog,
            "Request %lu:%lu ends in stripe, starts before\n",
            sdp->roff, sdp->size);
    } else {
        if (vdifuse_debug>0) fprintf(stderr,
            "Request %lu:%lu outside of stripe %lu:%lu\n",
            sdp->roff, sdp->size, sdp->bybs, sdp->bybs + sdp->byis);
        vdifuse_trace(VDT("EIO request %lu:%lu stripe %lu:%lu\n"),
            sdp->roff, sdp->size, sdp->bybs, sdp->bybs + sdp->byis);
        return(1);
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
 *
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
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_read(%s)\n", sdp->vs->fuse);
    for (ii = sdp->zero; ii < sdp->sgap && size > 0; ii++) {
        ith = sdp->stripe[ii].sfrag;
        if (soff <= roff && roff < soff + ith->byib) {
            sdp->stripe[ii].reads ++;
            addr = (void*)ith->addr + (roff - soff);
            todo = soff + ith->byib - roff;
            if (todo > size) todo = size;
            /* this is the only place bytes get moved */
            if (vdifuse_debug>4) fprintf(vdflog,
                "Reading %lu from %d-th frag of stripe (%lu:%lu) %s\n",
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
        stripe_bread(sdp);
        fprintf(stderr, "###Unable to read at %lu for %lu (read %lu)\n",
            sdp->roff, sdp->size, rb);
        fprintf(stderr, "   todo:%lu in %d-th frag of stripe (%lu:%lu) %s\n",
            todo, ii-1, roff-todo, size+todo, lab); 
        vdifuse_trace(VDT("incomplete read  %lu for %lu (read %lu)"
            " todo:%lu after %d-th frag of stripe (%lu:%lu) %s\n"),
            sdp->roff, sdp->size, rb,
            todo, ii-1, sdp->bybs, sdp->bybs + sdp->byis, lab);
        stripe_show(sdp, lab, -1);
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
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_rdfw(%s)\n", sdp->vs->fuse);
    if (vdifuse_debug>1) fprintf(vdflog,
        "stripe_rdfw need %lu:%lu\n", roff, size);
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
        if (vdifuse_debug>4) fprintf(vdflog, "stripe_rdfw calling...\n");
        if (stripe_walk(sdp, 1)) break;
        stripe_bread(sdp);
    }
    if (done != size) {
        stripe_bread(sdp);
        fprintf(stderr,
            "###Short read in stripe_rdfw(%lu != %lu) at %lu trying %lu\n",
            done, size, roff, todo);
        vdifuse_trace(VDT("stripe_rdfw(%lu != %lu) at %lu trying %lu\n"),
            done, size, roff, todo);
        /* stripe_show(sdp, "rdfw-ef", -1); */
        return(-EIO);   /* short read in stripe rdfw */
    }
    if (vdifuse_debug>1) fprintf(vdflog,
        "stripe_rdfw read %lu:%lu[%lu+%lu%s]\n", roff, done, td0, td1, tdx);
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
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_rdbw(%s)\n", sdp->vs->fuse);
    if (vdifuse_debug>1) fprintf(vdflog,
        "stripe_rdbw need %lu:%lu\n", roff, size);
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
        if (vdifuse_debug>4) fprintf(vdflog, "stripe_rdbw calling...\n");
        if (stripe_walk(sdp, -1)) break;
        stripe_bread(sdp);
        /* after the first all reads try to do entire stripe */
        sdp->size = sdp->bybs - roff;
    }
    if (done != size) {
        stripe_bread(sdp);
        fprintf(stderr,
            "###Short read in stripe_rdbw(%lu != %lu) at %lu trying %lu\n",
            done, size, roff, todo);
        vdifuse_trace(VDT("short stripe_rdbw(%lu != %lu) at %lu trying %lu\n"),
            done, size, roff, todo);
        /* stripe_show(sdp, "rdbw-ef", -1); */
        return(-EIO);   /* short read in stripe rdbw */
    }
    if (vdifuse_debug>1) fprintf(vdflog,
        "stripe_rdbw read %lu:%lu[%lu+%lu%s]\n", roff, done, td0, td1, tdx);
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
    if (vdifuse_debug>1) fprintf(vdflog,
        "Generated FH %d (%s) which is not %d or %d\n",
        fh, tempname, realfd, vorrfd);
    unlink(tempname);
    free(tempname);
    return(fh >= 0 ? fh : vorrfd);
}
static void release_fh(int fh)
{
    if (vdifuse_debug>1) fprintf(vdflog,
        "Releasing FH %d\n", fh);
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
 * with the value specified in the parameters area.
 */
int open_sgv2_seq(VDIFUSEntry *vs, FFInfo *ffi)
{
    VDIFUSEntry *vdccs = current_cache_start();
    VDIFUSEntry *vsanc = vdccs + vs->cindex, *vfrag, *vfanc;
    SGV2sdata *sdp;
    SGV2sfrag *sfp;
    int num, ccount = 0, errors = 0, msbs;
    uint32_t fcmx = 0, pgfcmx = vdccs->u.vpars.pkts_per_sec - 1;

    if (vdifuse_debug>5) fprintf(vdflog, "open_sgv2_seq(%s)\n", vs->fuse);

    ffi->fh = vorrfd;
    ffi->numb = vs->u.vfuse.st_nlink;
    ffi->stype = vs->stype;

    sdp = (SGV2sdata *)(ffi->sdata = calloc(1, sizeof(SGV2sdata)));
    if (!ffi->sdata) return(perror("open_sgv2_seq:calloc(sdata)"),vorrfd);
    if (vdifuse_debug>3) fprintf(vdflog,
        "sdp at %p:%lu\n", sdp, sizeof(SGV2sdata));
    sdp->vs = vs;
    sdp->numb = ffi->numb;
    sdp->bygt = vs->u.vfuse.st_size;
    sdp->msbs = 0x7FFFFFFF;	/* big enough */
    // sdp->scnt = 0;
    sdp->page = 32 * sysconf(_SC_PAGESIZE);
    // for (num = 0; num < SGV2_DIAG_COUNT; num++) sdp->diag[num] = 0;

    sfp = (SGV2sfrag *)(ffi->sfrag = calloc(ffi->numb, sizeof(SGV2sfrag)));
    if (!ffi->sfrag) return(perror("open_sgv2_seq:calloc(sfrag)"),vorrfd);
    if (vdifuse_debug>3) fprintf(vdflog,
        "sfp at %p:%lu\n", sfp, ffi->numb * sizeof(SGV2sdata));

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

        /* the next few lines are uncessary if per-member cached fcmx is ok */
        if (sfp->sgi->frame_cnt_max > fcmx) fcmx = sfp->sgi->frame_cnt_max;
        if (pgfcmx > 1 && pgfcmx < SG_FR_CNT_MAX)
            sfp->sgi->frame_cnt_max = pgfcmx;

        sdp->stripe[num].index = num;
        sdp->stripe[num].reads = 0;
        sdp->stripe[num].sfrag = sfp;
        errors |= sfp->err;
        if (vdifuse_debug>1 || sfp->err) fprintf(vdflog,
            "open_sgv2_seq(%s)@%d:%x msbs %d err %x with %d pps -> %d pps\n",
            sfp->sgi->name, sfp->sgi->smi.mmfd, sfp->err, sdp->msbs,
            errors, fcmx, pgfcmx);
        if (sfp->err) vdifuse_trace(
            VDT("Corrupt fragment %s [%X]\n"), sfp->sgi->name, sfp->err);
    }
    errors |= stripe_init(sdp, pgfcmx);
    if (errors) {
        fprintf(stderr, "Corrupt file %s [error %X]\n", vs->fuse, errors);
        vdifuse_trace(VDT("Corrupt file %s [error %X]\n"), vs->fuse, errors);
    }
    ffi->fh = errors ? vorrfd : generate_fh();
    return(ffi->fh);
}

/*
 * Dump the diagnostics to the trace.
 */
static void diag_counts(off_t *diag)
{
    vdifuse_trace(VDT("Diag-Move: %lu %lu %lu %lu %lu %lu\n"),
        diag[SGV2_DIAG_BREAD], diag[SGV2_DIAG_TRACE], diag[SGV2_DIAG_SHOWN], 
        diag[SGV2_DIAG_VERN], diag[SGV2_DIAG_WALK], diag[SGV2_DIAG_FIND]); 
    vdifuse_trace(VDT("Diag-Read: %lu %lu %lu %lu %lu %lu\n"),
        diag[SGV2_DIAG_CHECK], diag[SGV2_DIAG_READ], diag[SGV2_DIAG_RDFW], 
        diag[SGV2_DIAG_RDBW], diag[SGV2_DIAG_DOIT], diag[SGV2_DIAG_SPARE]); 
}

/*
 * Close the open fragments
 */
void release_sgv2_seq(FFInfo *ffi)
{
    int num;
    SGV2sdata *sdp = (SGV2sdata *)ffi->sdata;
    SGV2sfrag *sfp = (SGV2sfrag *)ffi->sfrag;
    if (vdifuse_debug>5) fprintf(vdflog,
        "release_sgv2_seq(%s)\n", sdp->vs->fuse);
    for (num = 0; num < ffi->numb; num++, sfp++) {
        if (vdifuse_debug>1) fprintf(vdflog,
            "release_sgv2_seq(%s)@%d:%x\n",
            sfp->sgi->name, sfp->sgi->smi.mmfd, sfp->err);
        if ((sfp->err & SGV2_ERR_REOPEN) == 0)
            sg_close(sfp->sgi);
    }
    diag_counts(sdp->diag);
    free(ffi->sfrag);
    free(ffi->sdata);
    release_fh(ffi->fh);
    return;
}

/*
 * For development/diagnostic/debugging use
 */
static void show_sgv2_state(FFInfo *ffi, int start, int adj)
{
    static unsigned long cnt = 0;
    SGV2sdata *sdp = (SGV2sdata*)ffi->sdata;
    if (start) {
        if (vdifuse_debug>4) fprintf(vdflog,
            ">>>READ[%lu] %lu at offset %lu\n",
            cnt++, sdp->size, sdp->roff);
        if (vdifuse_debug>5) fprintf(vdflog,
            "   read_sgv2_seq(%s)\n", sdp->vs->fuse);
    }
    if (vdifuse_debug>3) stripe_show(sdp, "READ", adj);
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
 * We use EFAULT for things we can't handle (normal).
 * We use EIO for things that we should stop and take a look at.
 */
static int do_read_sgv2_seq(char *buf, FFInfo *ffi)
{
    SGV2sdata *sdp = (SGV2sdata *)ffi->sdata;
    int rb, sf = 0;

    sdp->diag[SGV2_DIAG_DOIT] ++;

    /* working copies for convenience and simple checking */
    sdp->roff = ffi->offset;
    sdp->size = ffi->size;
    /* i.e. writes > sdp->sowb && < sdp->eowb are legal */
    sdp->sowb = buf;
    sdp->eowb = buf + sdp->size;

    /* the following would be silly */
    if (sdp->roff < 0) {
        vdifuse_trace(VDT("Read EFAULT: %lu:%lu\n"), ffi->offset, ffi->size);
        fprintf(stderr,
            "###Read starts before beginning of file\n"
            "### original request was %lu:%lu\n", ffi->offset, ffi->size);
        stripe_show(sdp, "sgv2-ro", -1);
        return(-EFAULT);    /* read before file */
    }
    /* as would this */
    if (sdp->roff >= sdp->bygt) {
        vdifuse_trace(VDT("Read after EOF %lu:%lu\n"), ffi->offset, ffi->size);
        if (vdifuse_debug>1) fprintf(vdflog,
            "Read beyond EOF %lu:%lu\n", ffi->offset, ffi->size);
        /* prevent further reads */
        ffi->errors |= VDIFUSE_FFIERROR_EOF;
        return(0);
    }
    /* truncate addresses past eof to the end of file */
    if (sdp->roff + sdp->size > sdp->bygt) {
        sdp->size -= (sdp->roff + sdp->size - sdp->bygt);
        if (vdifuse_debug>1) fprintf(vdflog,
            "Request trim (%lu:%lu->%lu)\n", sdp->roff, ffi->size, sdp->size);
        vdifuse_trace(VDT("Read trimmed to EOF: %lu:%lu->%lu\n"),
            ffi->offset, ffi->size, sdp->size);
        /* prevent further reads if the request was beyond the pale */
        if (sdp->size == 0) {
            ffi->errors |= VDIFUSE_FFIERROR_EOF;
            //return(-EFAULT);    /* EOF */
            return(0);
        }
    }

    if (vdifuse_debug>1) show_sgv2_state(ffi, 1, sf);

    /* reposition stripe so that one of the following methods works */
    stripe_bread(sdp);
    if ((sf = stripe_chck(sdp)) < 0) {
        stripe_bread(sdp);
        vdifuse_trace(VDT("Seek fail EIO %lu:%lu\n"), ffi->offset, ffi->size);
        fprintf(stderr,
            "###Unable to seek to requested location\n"
            "### original request was %lu:%lu\n", ffi->offset, ffi->size);
        stripe_show(sdp, "sgv2-ck", -1);
        return(-EIO);   /* Seek fail */
    }
    stripe_bread(sdp);
    if (vdifuse_debug>1) show_sgv2_state(ffi, 0, sf ? 1 : 2);

    if (stripe_anal(sdp)) return(-EIO); /* stripe_anal fail */

    /* data request is fully within the stripe */
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

    /* data request starts in stripe, runs after */
    if (sdp->roff >= sdp->bybs &&
        sdp->roff < sdp->bybs + sdp->byis) {
            rb = (stripe_rdfw(sdp, buf));
            vdifuse_bread(
                VDT("read %lu:%lu inside and after stripe %lu:%lu did %d\n"),
                sdp->roff, sdp->roff + sdp->size,
                sdp->bybs, sdp->bybs + sdp->byis, rb);
            return(rb);
    }

    /* data request starts before stripe, runs into */
    if (sdp->roff < sdp->bybs &&
        sdp->roff + sdp->size <= sdp->bybs + sdp->byis) {
            rb = (stripe_rdbw(sdp, buf));
            vdifuse_bread(
                VDT("read %lu:%lu before and inside stripe %lu:%lu did %d\n"),
                sdp->roff, sdp->roff + sdp->size,
                sdp->bybs, sdp->bybs + sdp->byis, rb);
            return(rb);
    }

    vdifuse_trace(VDT("Read logic error %lu:%lu\n"), sdp->roff, sdp->size);
    fprintf(stderr,
        "###Read logic error: %lu:%lu but %lu:%lu\n"
        "### original request was %lu:%lu\n",
        sdp->roff, sdp->size, sdp->bybs, sdp->byis,
        ffi->offset, ffi->size);
    stripe_show(sdp, "sgv2-nd", -1);
    return(-EIO);   /* Read logic error */
}

/*
 * The mutex here fixes things for multithreaded readers.
 * (The caller has a similar mutex, so this may be overkill.)
 * If we don't read as requested, flush the bread crumb trail.
 */
#include <pthread.h>

int read_sgv2_seq(char *buf, FFInfo *ffi)
{
    // static pthread_mutex_t vdifsg2_mutex = PTHREAD_MUTEX_INITIALIZER;
    int rb;
    // pthread_mutex_lock(&vdifsg2_mutex);
    rb = do_read_sgv2_seq(buf, ffi);
    if (rb != ffi->size) vdifuse_flush_bread();
    // pthread_mutex_unlock(&vdifsg2_mutex);
    return(rb);
}

/*
 * eof
 */
