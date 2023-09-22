/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdiflat.c 5723 2023-03-13 20:53:47Z gbc $
 *
 * This file provides support for the fuse interface.
 * This version is rather primitive in many respects.
 *
 * This file provides support for flat vdif files.
 */

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "vdifuse.h"

/*
 * Work out the size of flat vdif sequences and update
 * vp->u.vfuse.st_size from the bogus starter value (0).
 * For each fragment we increment the total size by
 * total_pkts * pkt_size of that fragment.
 * nlink is the total count of fragments, and we check
 * it and a few other things for consistency.
 *
 * NOTION: update remaining vp->u.vfuse entries, if this is ever used.
 * + blksize_t st_blksize; // packet size
 * + blkcnt_t  st_blocks;  // number of packets
 * + time_t    st_mtime;   // data start time
 * + time_t    st_ctime;   // data end time (inc. last packet)
 * + time_t    st_atime;   // total scan duration
 */
int finalize_vdif_sequence(VDIFUSEntry *vp)
{
    int vdcne = current_cache_entries();
    VDIFUSEntry *vdccs = current_cache_start(), *vsanc, *vfrag;
    nlink_t nlink = 0;
    int errs = 0, ccount = 0;

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

    vsanc = vdccs + vp->cindex;
    while (!errs && (nlink < vp->u.vfuse.st_nlink)) {
        if (ccount == VDIFUSE_MAX_SEQI) {
            if (vsanc->cindex == 0 || vsanc->cindex >= vdcne) break;
            vsanc = vdccs + vsanc->cindex;
            ccount = 0;
        }
        vfrag = vdccs + vsanc->u.vseqi[ccount++];
        vp->u.vfuse.st_size += vfrag->u.vfuse.st_size;
        nlink++;
    }

    if (nlink != vp->u.vfuse.st_nlink) {
        fprintf(stderr, "Missing seq entries (%lu != %lu; %u >= %u)\n",
            nlink, vp->u.vfuse.st_nlink, vsanc->cindex, vdcne);
        errs ++;
    }
    return(errs);
}

/*
 * vorr support...
 *
 * ...for sequences of flat files...FIXME: NYI...
 */

int open_flat_seq(VDIFUSEntry *vs, FFInfo *ffi)
{
    /* open vproc directories for info about sdp */
    return(vorrfd);
}
void release_flat_seq(FFInfo *ffi)
{
    return;
}
int read_flat_seq(char *buf, FFInfo *ffi)
{
    return(-ENOENT);
}

/*
 * eof
 */
