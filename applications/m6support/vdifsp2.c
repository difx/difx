/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdifsp2.c 5750 2023-03-26 14:49:25Z gbc $
 *
 * This file provides support for the fuse interface.
 * This version is rather primitive in many respects.
 *
 * This file provides glue to the basic sg_access library
 * to allow some of the complexity that vdifuse requires.
 *
 * vdifuse_trace is available in this file.
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
 * Developmental/check function
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
void attach_sgv2_ancillary(uint32_t vx_index)
{
    VDIFUSEntry *vc = create_cache_entry();
    VDIFUSEntry *vdccs = current_cache_start();
    VDIFUSEntry *vx = vdccs + vx_index;
    SGInfo *sgi;

    vdiflog(3, "    attach_sgv2_ancillary %p[%d] for %p[%d:%d]\n",
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

    vdiflog(4, "    sg_access(%s)...\n", vc->path);
    if (vdccs->u.vpars.pkts_per_sec > 1 &&
        vdccs->u.vpars.pkts_per_sec < SG_FR_CNT_MAX)
        sgi->frame_cnt_max = vdccs->u.vpars.pkts_per_sec - 1;
    sg_access(vc->path, sgi);
    /* check that it is ok */
    if (sgi->sg_version == SG_VERSION_OK_2) {
        vdiflog(2, "    SGv2 '%s'\n", vc->path);
    } else {
        vdiflog(0, "Problematic SGv2 file %s\n", vc->path);
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
    vdiflog(-1, "%s"
        "  (%s%s)\n"
        "  (%d=%d+%d+%d+%d pkts [%uB] /%u,%u,%u/)"
        "  (%02d@%d+%d..%02d@%d+%d %d=%d+%d+%d+%d {%d:%d})"
        "  (%d thr %d %d %d %d %d %d %d %d)"
        "  (%d sep %d %d %d %d %d %d %d %d)\n",
        (!sgi) ? "Null SGI pointer\n" : "",
        sgi->name == 0 ? "ok:" : "!!:", vc->path,
        sgi->total_pkts,
        sgi->sg_wr_pkts_bs, sgi->sg_sh_pkts,
        sgi->sg_wr_pkts_as, sgi->sg_se_pkts,
        sgi->pkt_size, sgi->sg_wr_block,
        sgi->sg_sh_block, sgi->sg_se_block,
        sgi->ref_epoch, sgi->first_secs, sgi->first_frame,
        sgi->ref_epoch, sgi->final_secs, sgi->final_frame,
        sgi->sg_total_blks,
        sgi->sg_wr_blks_bs, sgi->sg_sh_blk_off ? 1: 0,
        sgi->sg_wr_blks_as, sgi->sg_se_blk_off ? 1: 0,
        sgi->sg_first_bnum, sgi->sg_final_bnum,
        sgi->nvthreads, sgi->vthreads[0], sgi->vthreads[1], sgi->vthreads[2],
        sgi->vthreads[3], sgi->vthreads[4], sgi->vthreads[5], sgi->vthreads[6],
        sgi->vthreads[7], sgi->vthreadsep, sgi->vthreads[8], sgi->vthreads[9],
        sgi->vthreads[10], sgi->vthreads[11], sgi->vthreads[12],
        sgi->vthreads[13], sgi->vthreads[14], sgi->vthreads[15]);
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
 * eof
 */
