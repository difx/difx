/*
 * $Id: vdifsg2.c 3481 2015-10-05 15:33:55Z gbc $
 *
 * This file provides support for the fuse interface.
 * This version is rather primitive in many respects.
 *
 * This file provides glue to the sg_access library.
 */

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "vdifsg2.h"

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
    VDIFUSEntry *vx = current_cache_start() + vx_index;
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
 * work out size of sgv2 sequences and update
 * vp->u.vfuse.st_size from the bogus starter value (0).
 * For each fragment we increment the total size by
 * total_pkts * pkt_size of that fragment using
 * the SGInfo data for that fragment.  And the
 * nlink is the total count of fragments, and we check
 * it and a few other things for consistency.
 */
int finalize_sgv2_sequence(VDIFUSEntry *vp)
{
    int vdcne = current_cache_entries();
    VDIFUSEntry *vdccs = current_cache_start(), *vsanc, *vfrag, *vfanc;
    SGInfo *sgi;
    nlink_t nlink = 0;
    int errs = 0, ccount = 0, psize = 0;

    if (vp->u.vfuse.st_size != 0) {
        fprintf(stderr, "Seq entry %d already has size %u\n",
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
        if (psize == 0) psize = sgi->pkt_size;
        else if (psize != sgi->pkt_size) break;
        nlink++;
    }

    if (nlink != vp->u.vfuse.st_nlink) {
        fprintf(stderr,
            "Missing seq entries (%u != %u; %u >= %u; %u != %u)\n",
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
 */
static void member_show(SGV2sfrag *sfn, int ith)
{
    if (vdifuse_debug>5) fprintf(vdflog, "member_show(%s)\n", sfn->sgi->name);
    fprintf(vdflog,
        "  [%d]%04x@%d %u/%u b/%luB i/%luB [0x%lx] a/%luB [%luB]\n"
        "    %.9f < %.9f < %.9f < %.9f\n",
        ith, sfn->err, sfn->sgi->smi.mmfd,
        sfn->cblk, sfn->sgi->sg_total_blks,
        sfn->bybb, sfn->byib,
        ((void*)sfn->addr - sfn->sgi->smi.start), sfn->byab,
        sfn->bybb + sfn->byib + sfn->byab,
        sfn->first, sfn->ptbe, sfn->pten, sfn->final); 
}

/*
 * Move a member to an absolute block position
 * TODO: packet times if we have invalid packets
 *       would need to be extrapolated to block edges
 */
static void member_move(SGV2sfrag *sfp, off_t blk, int dir)
{
    uint32_t *pp;
    if (vdifuse_debug>5) fprintf(vdflog,
        "member_move(%s) %s\n", sfp->sgi->name, dir>0 ? "fw" : "bw");
    if (blk < 0 || blk > sfp->sgi->sg_total_blks) {
        sfp->err |= SGV2_ERR_BLKERR;
        return;
    }
    /* first the blocks and bytes */
    sfp->cblk = blk;
    pp = sg_pkt_blkby(sfp->sgi, blk, &sfp->nblk, &sfp->bybb, &sfp->byab);
    if (!pp) {
        sfp->err |= SGV2_ERR_ACCESS;
        return;
    }
    sfp->byib = sfp->nblk * sfp->sgi->pkt_size;
    sfp->addr = pp;
    /* now work out the times */
    sfp->ptbe = packet_time((VDIFHeader *)pp);
    /* pkt offset within read size already accounted for */
    pp += (sfp->sgi->read_size/sizeof(uint32_t)) * (sfp->nblk-1);
    sfp->pten = packet_time((VDIFHeader *)pp);
    if (sfp->pten < sfp->ptbe) sfp->err |= SGV2_ERR_TIME_B;
    /* advise mmap() machinery of our intentions */
    sg_advice(sfp->sgi, pp, dir);
}

/*
 * Open the frag and initialize it to point to the first block
 * As a complication, we identify the size of the smallest short
 * block, since we'll need it later.
 */
static int member_init(SGV2sfrag *sfp)
{
    VDIFHeader pkt;
    int msbs = sfp->sgi->sg_wr_pkts;
    if (vdifuse_debug>5) fprintf(vdflog, "member_init(%s)\n", sfp->sgi->name);
    sfp->err = (sg_reopen(sfp->sgi)) ? SGV2_ERR_VIRGIN : SGV2_ERR_REOPEN;
    pkt.w1.secs_inre = sfp->sgi->first_secs;
    pkt.w2.df_num_insec = sfp->sgi->first_frame;
    sfp->first = packet_time(&pkt);
    pkt.w1.secs_inre = sfp->sgi->final_secs;
    pkt.w2.df_num_insec = sfp->sgi->final_frame;
    sfp->final = packet_time(&pkt);
    if (sfp->final < sfp->first) sfp->err |= SGV2_ERR_TIME_F;
    if (0 < sfp->sgi->sg_sh_pkts && sfp->sgi->sg_sh_pkts < msbs)
        msbs = sfp->sgi->sg_sh_pkts;
    if (0 < sfp->sgi->sg_se_pkts && sfp->sgi->sg_se_pkts < msbs)
        msbs = sfp->sgi->sg_se_pkts;
    member_move(sfp, 0, 1);         /* start at the beginning */
    if (vdifuse_debug>1) fprintf(vdflog,
        "member_init %d#%d %.8f..%.8f %u pps\n",
            sfp->sgi->smi.mmfd, sfp->cblk, sfp->first, sfp->final,
            sfp->sgi->frame_cnt_max);
    return(msbs);
}

/*
 * A diagnostic function that shows what we know about the stripe.
 * TODO: revise verbosity
 */
static void stripe_show(SGV2sdata *sdp, char *where, int adj)
{
    int ss, tre = 3, two = 0, one = 0, zer = 0;
    int z = sdp->zero, n = sdp->sgap-1, m = z==0?0:z-1;
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
    return(0);
}

/*
 * Sort the members of the sequence by block start time.
 */
static void stripe_sort(SGV2sdata *sdp)
{
    SBlock *sbp = sdp->stripe;
    int ii;
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_sort(%s)\n", sdp->vs->fuse);
    qsort(sbp, sdp->numb, sizeof(SBlock), stripe_comp);
}

/*
 * Check that a stripe is sorted, which it must always be up through
 * the gap; ie. [0..sgap] must be sorted, [sgap+1..numb-1] need not be.
 *
 * However, there is no harm in sorting the 2nd portion.
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
                    "Sort check failed on %d v %d (%.9f v %.9f)\n", ii, ii+1,
                    sdp->stripe[ii].sfrag->ptbe,
                    sdp->stripe[ii+1].sfrag->ptbe);
                stripe_show(sdp, "sortchk", -1);
                for (ii = 0; ii < sdp->numb; ii++)
                    member_show(sdp->stripe[ii].sfrag, ii);
                return(1);
            } else {                /* sort last part */
                nn = sdp->numb - (ii + 1);
                if (vdifuse_debug>2) fprintf(vdflog,
                    "Sort check resort 0..%d..%d,%d; (at %d sort %d)\n",
                    sdp->zero, sdp->sgap-1, sdp->sgap, ii+1, nn);
                qsort(sdp->stripe + ii + 1, nn, sizeof(SBlock), stripe_comp);
            }
        }
    if (vdifuse_debug>1) fprintf(vdflog,
        "Stripe %lu..%lu covers %.9f..%.9f\n",
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
    SGV2sfrag *ith, *jth;
    sdp->sgap = sdp->numb;
    for (ii = 0; ii < sdp->numb; ii++)
        if (sdp->stripe[ii].reads == 0) break;
    sdp->zero = ii;
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
                "Gap%02d@%-2d: have %+.9f = (jth-be)%.9f - (ith-en)%.9f\n"
                "Gap%02d@%-2d: take %+.9f = (jm1-be)%.9f - (ith-en)%.9f **\n",
                jj, fd, odt, jth->ptbe, pten, jj, fd, dt, tmp.ptbe, pten);
            *jth = tmp;
        } else {
            if (vdifuse_debug>4) fprintf(vdflog,
                "Ngp%02d@%-2d: keep %+.9f = (jth-be)%.9f - (ith-en)%.9f **\n"
                "Ngp%02d@%-2d: skip %+.9f = (jm1-be)%.9f - (ith-en)%.9f\n",
                jj, fd, odt, jth->ptbe, pten, jj, fd, dt, tmp.ptbe, pten);
            dt = odt;
            break;
        }
    }
    if (vdifuse_debug>3 && (dt >= mint)) fprintf(vdflog,
        "predecessors_leave_gap() left gap %.9f > %.9f\n", dt, mint);
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
        if (tmp.cblk < tmp.sgi->sg_total_blks) {
            member_move(&tmp, tmp.cblk + 1, 1);
            dt = tmp.pten - ptbe;
            if (dt < 0.0) {
                /* found an earlier block */
                if (vdifuse_debug>3) fprintf(vdflog,
                    "successor_closes_gap() with gap (%+.9f = %.9f - %.9f)\n",
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
 */
static void stripe_dgap(SGV2sdata *sdp)
{
    int jj, ii;
    SGV2sfrag *ith;
    SBlock tmp;
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_dgap(%s)\n", sdp->vs->fuse);
    jj = stripe_zero(sdp) + 1;
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
    sdp->sgap = jj++;
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
            jj = sdp->sgap = ii+1;
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
 *
 * TODO: need to think about making this more robust.
 */
static void stripe_boff(SGV2sdata *sdp)
{
    int ii;
    SGV2sfrag *ith;
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_boff(%s)\n", sdp->vs->fuse);
    sdp->bybs = 0;
    sdp->byis = 0;
    sdp->byas = 0;
    for (ii = 0; ii < sdp->zero; ii++) {
        ith = sdp->stripe[ii].sfrag;
        sdp->bybs += ith->bybb;
        sdp->bybs += ith->byib;
        if (ith->byab > 0) fprintf(stderr, "Error on %d < \"zero\"\n", ii);
    }
    for ( ; ii < sdp->sgap; ii++) {
        ith = sdp->stripe[ii].sfrag;
        sdp->bybs += ith->bybb;
        sdp->byis += ith->byib;
        sdp->byas += ith->byab;
    }
    for ( ; ii < sdp->numb; ii++) {
        ith = sdp->stripe[ii].sfrag;
        sdp->bybs += ith->bybb;
        sdp->byas += ith->byib + ith->byab;
    }
    sdp->bygt = sdp->bybs + sdp->byis + sdp->byas;
    if (vdifuse_debug>3) fprintf(vdflog,
        "  stripe_boff: %lu+%lu+%lu = %lu B\n",
        sdp->bybs, sdp->byis, sdp->byas, sdp->bygt);
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
 *
 * TODO: consider (for debugging) initialization to arbitrary places
 */
static int stripe_init(SGV2sdata *sdp, uint32_t fcmx)
{
    int ii;
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_init(%s)\n", sdp->vs->fuse);
    if (vdifuse_debug>3) fprintf(vdflog,
        "   init min sh block size %d (%.9f) fcmx %u\n",
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
 *
 * TODO: can consider checking that the data to be copied is valid.
 */
static int memory_check(char *buf, void *addr, size_t todo,
    SGV2sdata *sdp, SGV2sfrag *ith)
{
    SGMMInfo *smi = &ith->sgi->smi;
    if (addr < smi->start || addr + todo > smi->eomem) {
        fprintf(stderr, "Reading outside fragment: %p<%p | %p>%p\n",
            addr, smi->start, addr + todo, smi->eomem);
        return(1);
    }
    if (buf < sdp->sowb || buf + todo > sdp->eowb) {
        fprintf(stderr, "Writing outside buffer: %p<%p | %p>%p\n",
            buf, sdp->sowb, buf + todo, sdp->eowb);
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
 */
static int stripe_walk(SGV2sdata *sdp, int dir)
{
    int ii, nblk, cblk, ord, nrd;
    double edge, ptbe, part1, part2;
    SGV2sfrag *ith;
    struct timeval now;
    (void)secs_since(&now);
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_walk(%s)\n", sdp->vs->fuse);
    if (vdifuse_debug>3) stripe_show(sdp, "prewalk", 0);

    if (dir == 0) {
        fprintf(stderr, "Illegal stripe shift of 0 requested!!!\n");
        return(1);
    } else if (dir > 0) {
        edge = sdp->stripe[sdp->sgap - 1].sfrag->pten;
    } else if (dir < 0) {
        edge = sdp->stripe[sdp->zero].sfrag->ptbe;
    }

    /* move stripe to predecessors/successors */
    for (ii = 0; ii < sdp->numb; ii++) {
        ith = sdp->stripe[ii].sfrag;

        /* work out the new blk # which must be in the legal range */
        nblk = ith->cblk + dir;
        if (nblk < 0) nblk = 0;
        if (nblk >= ith->sgi->sg_total_blks) nblk = ith->sgi->sg_total_blks-1;
        ord = sdp->stripe[ii].reads;

        /* do something based on block placement relative to edge */
        if (ith->cblk == nblk) {
            if (dir > 0) nrd = (ith->pten > edge) ? 0 : -1;
            else         nrd = (ith->ptbe < edge) ? 0 : -1;
            if (vdifuse_debug>3) fprintf(vdflog,
                "  stripe%02d@%-2d #%u %.9f (%d) == at same block (%d)\n",
                ii, ith->sgi->smi.mmfd, nblk, ith->ptbe, ord, nrd);
        } else if (dir > 0 && ith->pten > edge) {
            nrd = 0;
            if (vdifuse_debug>3) fprintf(vdflog,
                "  stripe%02d@%-2d #%u %.9f (%d) == already greater (%d)\n",
                ii, ith->sgi->smi.mmfd, nblk, ith->ptbe, ord, nrd);
        } else if (dir < 0 && ith->ptbe < edge) {
            nrd = 0;
            if (vdifuse_debug>3) fprintf(vdflog,
                "  stripe%02d@%-2d #%u %.9f (%d) == already lesser (%d)\n",
                ii, ith->sgi->smi.mmfd, nblk, ith->ptbe, ord, nrd);
        } else {
            nrd = 0;
            cblk = ith->cblk;
            ptbe = ith->ptbe;
            member_move(ith, nblk, dir);
            if (vdifuse_debug>3) fprintf(vdflog,
                "  stripe%02d@%-2d #%u %.9f (%d) -> #%u %.9f %lu\n",
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

    if (vdifuse_debug>1) fprintf(vdflog,
        "stripe_walk(%s) took (%f+%f) %f\n",
        sdp->vs->fuse, part1, part2, part1 + part2);
    return(stripe_schk(sdp));
}

/*
 * Make a large change in the stripe position, either to later blocks
 * (dir>1) or earlier (dir<-1) ones.  We can use stripe_walk() to
 * reposition the fragments, but need to do a bit more work to see if
 * we have found the appropriate part of the file.
 *
 * Should probably use a binary search strategy.
 */
static int stripe_find(SGV2sdata *sdp, int dir)
{
    if (vdifuse_debug>5) fprintf(vdflog, "stripe_find(%s)\n", sdp->vs->fuse);
    fprintf(vdflog, "Not able to provide random access yet\n"); // FIXME
    return(1);
}

/*
 * Check where the "offset" lies when the relative to the current stripe.
 * For nearby requests, we walk forward or backwards until we overlap.
 * For distant requests, we go hunt for it.
 *
 * Return values: 0 = no change, 1 = ok change, -1 = death.
 *
 * TODO: if stripe_find() is reliable, we should use it on large distances.
 */
static int stripe_chck(SGV2sdata *sdp)
{
    int rv = -1;
    off_t distance, newdist;
    static struct timeval entry, tdone;
    struct timeval now;
    double psecs, fsecs;
    (void)secs_since(&now);

    if (vdifuse_debug>5) fprintf(vdflog, "stripe_chck(%s)\n", sdp->vs->fuse);

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
                "  Distance increased to %lu\n", newdist);
            break;
        }
        distance = newdist;
        rv = 1;
    }
    psecs = secs_since(&now);

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
                "  Distance increased to %lu\n", newdist);
            break;
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
    }

    if (vdifuse_debug>1) fprintf(vdflog,
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
 * TODO: handle pkt offset > 0 case: the memcpy turns into a loop.
 * TODO: consider checking the data integrity as we write.
 */
static ssize_t stripe_read(SGV2sdata *sdp, char *buf, char *lab)
{
    int ii;
    SGV2sfrag *ith;
    off_t soff = sdp->bybs, roff = sdp->roff;
    size_t todo, size = sdp->size;
    ssize_t rb = 0;
    void *addr;
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
        fprintf(stderr, "###Unable to read at %lu for %lu (read %lu)\n",
            sdp->roff, sdp->size, rb);
        fprintf(stderr, "   todo:%lu in %d-th frag of stripe (%lu:%lu) %s\n",
            todo, ii-1, roff-todo, size+todo, lab); 
        stripe_show(sdp, lab, -1);
        return(-EFAULT);
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
 *
 * Returns a read error code (-EFAULT) if there is a problem.
 * TODO: check error return value.
 * TODO: test
 */
static int stripe_rdfw(SGV2sdata *sdp, char *buf)
{
    off_t roff = sdp->roff;
    size_t size = sdp->size, todo = size, done = 0, td0 = 0, td1 = 0;
    char *tdx = "";
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
    }
    if (done != size) {
        fprintf(stderr,
            "###Short read in stripe_rdfw(%lu != %lu) at %lu trying %lu\n",
            done, size, roff, todo);
        stripe_show(sdp, "rdfw-ef", -1);
        return(-EFAULT);
    }
    if (vdifuse_debug>1) fprintf(vdflog,
        "stripe_rdfw read %lu:%lu[%lu+%lu%s]\n", roff, done, td0, td1, tdx);
    return(done);
}

/*
 * Supply read requests moving the stripe backwards.  We follow the same
 * logic as stripe_rdfw() except we fill the buffer from the back end,
 * and walk forward.
 *
 * Returns a read error code (-EFAULT) if there is a problem.
 * TODO: check error return value.
 * TODO: test
 */
static int stripe_rdbw(SGV2sdata *sdp, char *buf)
{
    off_t roff = sdp->roff;
    size_t size = sdp->size, todo = size, done = 0, td0 = 0, td1 = 0;
    char *tdx = "";
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
        /* after the first all reads try to do entire stripe */
        sdp->size = sdp->bybs - roff;
    }
    if (done != size) {
        fprintf(stderr,
            "###Short read in stripe_rdbw(%lu != %lu) at %lu trying %lu\n",
            done, size, roff, todo);
        stripe_show(sdp, "rdbw-ef", -1);
        return(-EFAULT);
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
        "Generated FH %d (%s)\n", fh, tempname);
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
 */
int open_sgv2_seq(VDIFUSEntry *vs, FFInfo *ffi)
{
    VDIFUSEntry *vdccs = current_cache_start();
    VDIFUSEntry *vsanc = vdccs + vs->cindex, *vfrag, *vfanc;
    SGV2sdata *sdp;
    SGV2sfrag *sfp;
    int num, ccount = 0, errors = 0, msbs;
    uint32_t fcmx = 0;

    if (vdifuse_debug>5) fprintf(vdflog, "open_sgv2_seq(%s)\n", vs->fuse);

    ffi->fh = vorrfd;
    ffi->numb = vs->u.vfuse.st_nlink;
    ffi->stype = vs->stype;

    sdp = (SGV2sdata *)(ffi->sdata = calloc(1, sizeof(SGV2sdata)));
    if (!ffi->sdata) return(perror("open_sgv2_seq:calloc(sdata)"),vorrfd);
    if (vdifuse_debug>3) fprintf(vdflog,
        "sdp at %p:%u\n", sdp, sizeof(SGV2sdata));
    sdp->vs = vs;
    sdp->numb = ffi->numb;
    sdp->bygt = vs->u.vfuse.st_size;
    sdp->msbs = sdp->bygt;

    sfp = (SGV2sfrag *)(ffi->sfrag = calloc(ffi->numb, sizeof(SGV2sfrag)));
    if (!ffi->sfrag) return(perror("open_sgv2_seq:calloc(sfrag)"),vorrfd);
    if (vdifuse_debug>3) fprintf(vdflog,
        "sfp at %p:%u\n", sfp, ffi->numb * sizeof(SGV2sdata));

    for (num = 0; num < ffi->numb; num++, sfp++) {
        if (ccount == VDIFUSE_MAX_SEQI) {
            vsanc = vdccs + vsanc->cindex;
            ccount = 0;
        }
        vfrag = vdccs + vsanc->u.vseqi[ccount++];
        vfanc = vdccs + vfrag->cindex;
        sfp->sgi = (SGInfo *)vfanc->u.voids;
        sfp->sgi->name = vfanc->path;           /* attach sg access name */
        msbs = member_init(sfp);
        if (msbs < sdp->msbs) sdp->msbs = msbs;
        if (sfp->sgi->frame_cnt_max > fcmx) fcmx = sfp->sgi->frame_cnt_max;
        sdp->stripe[num].index = num;
        sdp->stripe[num].reads = 0;
        sdp->stripe[num].sfrag = sfp;
        if (vdifuse_debug>1 || sfp->err) fprintf(vdflog,
            "open_sgv2_seq(%s)@%d:%x\n",
            sfp->sgi->name, sfp->sgi->smi.mmfd, sfp->err);
        errors += sfp->err;
    }
    errors += stripe_init(sdp, fcmx);
    if (errors) fprintf(stderr, "Corrupt file %s\n", vs->fuse);
    ffi->fh = errors ? vorrfd : generate_fh();
    return(ffi->fh);
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
    SGV2sfrag *sfp = (SGV2sfrag*)ffi->sfrag;
    int index, nn;
    if (start) {
        if (vdifuse_debug>4) fprintf(vdflog,
            ">>>READ[%lu] %d at offset %lu\n",
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

    /* working copies for convenience and simple checking */
    sdp->roff = ffi->offset;
    sdp->size = ffi->size;
    /* i.e. writes > sdp->sowb && < sdp->eowb are legal */
    sdp->sowb = buf;
    sdp->eowb = buf + sdp->size;

    /* the following would be silly */
    if (sdp->roff < 0) {
        fprintf(stderr,
            "###Read starts before beginning of file\n"
            "### original request was %lu:%lu\n", ffi->offset, ffi->size);
        stripe_show(sdp, "sgv2-ro", -1);
        return(-EFAULT);
    }
    /* truncate addresses past eof to the end of file */
    if (sdp->roff + sdp->size > sdp->bygt) {
        if (vdifuse_debug>1) fprintf(vdflog,
            "Request exceeds (%lu:%lu) EOF\n", sdp->roff, sdp->size);
        sdp->size -= (sdp->roff + sdp->size - sdp->bygt);
        if (vdifuse_debug>1) fprintf(vdflog,
            "Request reduced (%lu:%lu)\n", sdp->roff, sdp->size);
    }

    if (vdifuse_debug>1) show_sgv2_state(ffi, 1, sf);

    /* reposition stripe so that one of the following methods works */
    if ((sf = stripe_chck(sdp)) < 0) {
        fprintf(stderr,
            "###Unable to seek to requested location\n"
            "### original request was %lu:%lu\n", ffi->offset, ffi->size);
        stripe_show(sdp, "sgv2-ck", -1);
        return(-EIO);
    }
    if (vdifuse_debug>1) show_sgv2_state(ffi, 0, sf ? 1 : 2);

    if (stripe_anal(sdp)) return(-EIO);

    /* data request is fully within the stripe */
    if (sdp->roff >= sdp->bybs &&
        sdp->roff + sdp->size <= sdp->bybs + sdp->byis)
            return(stripe_read(sdp, buf, "sgv2"));

    /* data request starts in stripe, runs after */
    if (sdp->roff >= sdp->bybs &&
        sdp->roff < sdp->bybs + sdp->byis)
            return(stripe_rdfw(sdp, buf));

    /* data request starts before stripe, runs into */
    if (sdp->roff < sdp->bybs &&
        sdp->roff + sdp->size <= sdp->bybs + sdp->byis)
            return(stripe_rdbw(sdp, buf));

    fprintf(stderr,
        "###Read logic error: %lu:%lu but %lu:%lu\n"
        "### original request was %lu:%lu\n",
        sdp->roff, sdp->size, sdp->bybs, sdp->byis,
        ffi->offset, ffi->size);
    stripe_show(sdp, "sgv2-nd", -1);
    return(-EIO);
}

/*
 * This fixes for multithreaded readers.
 * TODO: Implement proper mutex solution across entire fuse.
 */
#include <pthread.h>

int read_sgv2_seq(char *buf, FFInfo *ffi)
{
    static pthread_mutex_t vdifuse_mutex = PTHREAD_MUTEX_INITIALIZER;
    int rb;
    pthread_mutex_lock(&vdifuse_mutex);
    rb = do_read_sgv2_seq(buf, ffi);
    if (rb > 0) ffi->totrb += rb;
    pthread_mutex_unlock(&vdifuse_mutex);
    return(rb);
}

/*
 * eof
 */
