/*
 * $Id: sg_access.c 2473 2014-09-15 19:43:25Z gbc $
 *
 * Code to understand and access sg files efficiently.
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif /* _GNU_SOURCE */
#include <fcntl.h>

#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif /* O_CLOEXEC */

#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include "vdif.h"
#include "sg_access.h"


/* macros to simplify updating sgi->frame_cnt_max */
#define update_fr_cnt_max_fr_frame(T,M) do {\
    if ((T)>(M)) (M) = (T); } while(0)
#define update_fr_cnt_max_fr_ptr32(P,M) do {\
    uint32_t frame = ((VDIFHeader *)(P))->w2.df_num_insec;\
    if (frame > (M)) (M) = frame; } while(0)

/* bring in vdif_epochs[] */
#include "vdif_epochs.h"

static FILE *sgalog = 0;

/*
 * For logging to something other than stdout
 */
void sg_logger(FILE *fp)
{
    sgalog = fp;
}

/*
 * To allow variable station id's within a thread
 */
static int sg_station_id_mask = SG_STATION_MASK;
void sg_set_station_id_mask(int mask)
{
    sg_station_id_mask = (mask & 0xFFFF);
}
int sg_get_station_id_mask(void)
{
    return(sg_station_id_mask);
}

/*
 * Compute and return vdif signature
 */
uint64_t sg_get_vsig(uint32_t *vhp, void *orig, int verb)
{
    VDIFsigu vdif_signature;
    VDIFHeader *vh = (VDIFHeader *)vhp;
    vdif_signature.word              = 0LL;
    vdif_signature.bits.df_len       = vh->w3.df_len;
    vdif_signature.bits.ref_epoch    = vh->w2.ref_epoch;
    vdif_signature.bits.UA           = vh->w2.UA;
    vdif_signature.bits.stationID    = vh->w4.stationID & sg_station_id_mask;
    vdif_signature.bits.num_channels = vh->w3.num_channels;
    vdif_signature.bits.ver          = vh->w3.ver;
    vdif_signature.bits.bps          = vh->w4.bps;
    vdif_signature.bits.dt           = vh->w4.dt;
    vdif_signature.bits.legacy       = vh->w1.legacy;
    vdif_signature.bits.unused       = 0x1;
    if (verb>0) fprintf(sgalog,
        "sgn:%18p %016lX at %lu\n"
        "sgn:  %08x%08x %08x%08x %08x%08x %08x%08x\n",
        vhp, vdif_signature.word, (void *)vhp - orig,
        vhp[1], vhp[0], vhp[3], vhp[2], vhp[5], vhp[4], vhp[7], vhp[6]);
    return(vdif_signature.word);
}

/*
 * Convenient time utility
 */
char *sg_vextime(int re, int secs)
{
    static char vext[80];
    time_t sunix = vdif_epochs[re];
    struct tm *stmp;
    sunix += secs;
    stmp = gmtime(&sunix);
    snprintf(vext, 78, "%4dy%03dd%02dh%02dm%02ds",
        1900 + stmp->tm_year, stmp->tm_yday + 1,
        stmp->tm_hour, stmp->tm_min, stmp->tm_sec);
    return(vext);
}

/*
 * Mostly out of curiousity, this captures the time in the
 * process for planning purposes, perhaps.
 */
static void capture_process_secs(int atend, double *saved)
{
    static struct timeval now;
    double dnow = (!gettimeofday(&now, 0))
                ? (double)now.tv_sec + 1e-6 * (double)now.tv_usec
                : 0.0;
    *saved = (atend)
           ? ((*saved > 0.0) ? dnow - *saved : 0.0)
           : dnow;
}

/*
 * Check to see if the existing smi info is consistent with a
 * previous user.  If so, bump the user count and return 0
 * so that the file isn't re-mapped.  Otherwise, return 0.
 * We assume here no one is opening and reusing an empty file.
 */
static int mm_uchk(SGMMInfo *smi, off_t st_size, int verb)
{
    /* prior user(s) */
    if (smi->size == st_size && smi->mmfd >= 0 &&
        smi->eomem == (smi->start + smi->size) && smi->users > 0) {
            if (verb > 1) fprintf(sgalog,
                "Prior User Init %d\n", smi->users + 1);
            return(++(smi->users));
    }
    /* first user */
    smi->mmfd = -1;
    smi->start = (void*)0;
    smi->eomem = (void*)0;
    smi->size = (off_t)0;
    smi->users = 1;
    if (verb > 1) fprintf(sgalog, "First User Init %d\n", smi->users);
    return(0);
}


/*
 * Open the file, memory map it and fill in some initial information
 * Before we start, we clear out stale information, in case of failure.
 */
static int mm_init(const char *file, SGMMInfo *smi, int verb)
{
    struct stat mm_stb;
    if (!sgalog) sgalog = stdout;
    if (stat(file, &mm_stb) || mm_stb.st_size <= 0)
        return(perror("mm_init:stat"),1);
    if (mm_uchk(smi, mm_stb.st_size, verb)) return(0);
    /* first user */
    smi->mmfd = open(file, O_RDONLY|O_CLOEXEC);
    if (smi->mmfd < 0)
        return(perror("mm_init:open"),2);
    smi->start = mmap(0, smi->size = mm_stb.st_size, 
        PROT_READ, MAP_SHARED, smi->mmfd, 0);
    if (smi->start == MAP_FAILED) {
        perror("mm_init:mmap");
        close(smi->mmfd);
        return(3);
    }
    smi->eomem = smi->start + smi->size;
    return(0);
}

/*
 * Close the file, discarding the map and copy of the name.
 * An msync() would be needed here if we were using RW maps.
 */
static void mm_term(SGMMInfo *smi, int verb)
{
    if (!smi) return;
    if (smi->users > 1 && verb > 1) fprintf(sgalog,
        "Other User Term %d\n", smi->users);
    if (--(smi->users) > 0) return;     /* other users */
    /* final user */
    if (verb > 1) fprintf(sgalog, "Final User Term %d\n", smi->users);
    if (smi->mmfd < 0) return;
    if (close(smi->mmfd)) perror("mm_term:close");
    smi->mmfd = -1;
    if (munmap(smi->start, smi->size)) perror("munmap");
    smi->start = (void*)0;
    smi->eomem = (void*)0;
    /* smi->size is left for reference */
}

/*
 * Examine the file header tag for verison 2.
 *
 * struct file_header_tag {
 *  unsigned int sync_word;
 *  int version;
 *  int block_size;
 *  int packet_format;
 *  int packet_size;
 * }
 * struct wb_header_tag {
 *  int blocknum;
 *  int wb_size;
 * }
 */
static void sg_file_header_tag_ok(SGInfo *sgi)
{
    uint32_t *words = (uint32_t *)(sgi->smi.start);

    if (words[0] != SG_VERSION_MAGIC) return;
    /* (words[3] != 0x0) it is mark5 fmt -> vdif; ok? */
    if (words[1] == 0x2) sgi->sg_version = SG_VERSION_OK_2;
    else return;

    sgi->read_size = words[4];
    sgi->sg_fht_size = 20;      /* sizeof(file_header_tag) */
    sgi->sg_wbht_size = 8;      /* sizeof(wb_header_tag) */
    sgi->sg_wr_block = words[2];
    sgi->frame_cnt_max = 0;

    /* need at least 4 packets */
    if (sgi->smi.size < 20 + 8 + 4 * sgi->read_size)
        sgi->sg_version = SG_VERSION_NOT_ENOUGH_DATA;
}

/*
 * Capture timestamp on first packet.
 * Work out the offset of the VDIF packet in the read packet.
 * This is common code that works on any consecutive runs of packets,
 * given an initial offset.  In the SG case, we know the read size;
 * in other cases, we assume it is the packet size plus the offset.
 */
static void sg_first_packet_common(SGInfo *sgi, off_t foff, off_t *rwdsp)
{
    uint32_t *vhp0, *vhp1;
    VDIFsigu vds0, vds1;
    off_t poff = 0, rwords = *rwdsp;

    /* vhp0 might point at a packet, vhp1 might point at the next */
    do {
        vhp0 = (uint32_t *)(sgi->smi.start + foff + poff);
        vds0.word = sg_get_vsig(vhp0, sgi->smi.start, sgi->verbose-2);
        if (*rwdsp == 0) {
            rwords = 2 * vds0.bits.df_len + poff / sizeof(uint32_t);
            if (rwords > SG_MAX_VDIF_BYTES/4) rwords = SG_MAX_VDIF_BYTES/4;
        }
        vhp1 = vhp0 + rwords;
        vds1.word = (vhp1 < (uint32_t *)(sgi->smi.eomem - sizeof(VDIFHeader)))
                  ? sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2) : 0LL;
    } while ((vds0.word != vds1.word) &&
             ((poff += sizeof(uint32_t)) < rwords));
    
    /* no word alignment produced equal signatures */
    if (vds0.word != vds1.word) {
        sgi->sg_version = SG_VERSION_SIG_FIRST_FAIL;
        return;
    }
    *rwdsp = rwords;
    
    /* yes, we are pointed at two packets */
    sgi->pkt_size = ( ((VDIFHeader *)vhp0)->w3.df_len ) * 8;
    sgi->pkt_offset = poff;
    sgi->first_secs = ((VDIFHeader *)vhp0)->w1.secs_inre;
    sgi->first_frame = ((VDIFHeader *)vhp0)->w2.df_num_insec;
    sgi->ref_epoch = ((VDIFHeader *)vhp0)->w2.ref_epoch;
    update_fr_cnt_max_fr_frame(sgi->first_frame, sgi->frame_cnt_max);
    sgi->vdif_signature.word = vds0.word;
}

/*
 * Call sg_first_packet_common() with the proper offset.
 */
static void sg_first_packet(SGInfo *sgi)
{
    off_t foff = sgi->sg_fht_size + sgi->sg_wbht_size;
    off_t rwds = sgi->read_size / sizeof(uint32_t);
    sg_first_packet_common(sgi, foff, &rwds);
}
static void flat_first_packet(SGInfo *sgi)
{
    off_t foff = 0, rwds = 0;
    if (sgi->smi.size < 4 * SG_MIN_VDIF_BYTES) {
        sgi->sg_version = SG_VERSION_NOT_ENOUGH_DATA;
        return;
    }
    sgi->frame_cnt_max = 0;
    sg_first_packet_common(sgi, foff, &rwds);
    if (sgi->sg_version != SG_VERSION_SIG_FIRST_FAIL)
        sgi->sg_version = SG_VERSION_FLAT;
    sgi->read_size = rwds * sizeof(uint32_t);
    sgi->sg_fht_size = sgi->sg_wbht_size = sgi->sg_wr_block = 0;
}

/*
 * Capture timestamp on final packet.
 * Check the offset of the VDIF packet in the read packet.
 * This is common code that works assuming the file ends with packets.
 */
static void sg_final_packet(SGInfo *sgi)
{
    uint32_t *vhp0, *vhp1;
    VDIFsigu vds0, vds1;

    vhp0 = (uint32_t *)(sgi->smi.eomem - 2 * sgi->read_size + sgi->pkt_offset);
    vhp1 = (uint32_t *)(sgi->smi.eomem - 1 * sgi->read_size + sgi->pkt_offset);
    vds0.word = sg_get_vsig(vhp0, sgi->smi.start, sgi->verbose-2);
    vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2);
    if (vds0.word != vds1.word || vds0.word != sgi->vdif_signature.word) {
        sgi->sg_version = SG_VERSION_SIG_FINAL_FAIL;
        return;
    }

    /* yes, we are pointed at two packets */
    sgi->final_secs = ((VDIFHeader *)vhp1)->w1.secs_inre;
    sgi->final_frame = ((VDIFHeader *)vhp1)->w2.df_num_insec;
    update_fr_cnt_max_fr_frame(sgi->final_frame, sgi->frame_cnt_max);
}

/*
 * The flat version requires the exact same girations
 */
static void flat_final_packet(SGInfo *sgi)
{
    sg_final_packet(sgi);
}

/*
 * Handle the two-short blocks at the end of the file.  When called,
 * we are somewhere in the last full block, and we need to find two
 * short blocks.  The first is sh, the second is se.  Part of the logic
 * is similar to the 2nd half sg_short_endfix(), but it's clearer to
 * code it here.
 *
 * And we need to skip sg_short_search() in sg_normal_block() later.
 */
static void sg_double_short(SGInfo *sgi, uint32_t *vhp0, uint32_t *vhp1)
{
    VDIFsigu vds0, vds1;

    vds0.word = sg_get_vsig(vhp0, sgi->smi.start, sgi->verbose-2);
    vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2);

    update_fr_cnt_max_fr_ptr32(vhp0, sgi->frame_cnt_max);
    while (vds0.word == vds1.word && vds0.word == sgi->vdif_signature.word) {
        update_fr_cnt_max_fr_ptr32(vhp1, sgi->frame_cnt_max);
        vhp0 = vhp1;
        vds0.word = vds1.word;
        vhp1 += (sgi->read_size / sizeof(uint32_t));
        if (vhp1 > (uint32_t *)(sgi->smi.eomem - sizeof(VDIFHeader))) {
            /* this would happen if the blocks were corrupt */
            sgi->sg_version = SG_VERSION_SIG_SDBLK_BAIL1;
            return;
        }
        vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-3);
    }

    /* ok, vhp1 points to the first short block */
    sgi->sg_sh_block = vhp1[1];
    sgi->sg_sh_blk_off = (void*)vhp1 - sgi->smi.start;
    sgi->sg_sh_pkts = (vhp1[1] - sgi->sg_wbht_size) / sgi->read_size;
    if (sgi->verbose>1) fprintf(sgalog,
        "Penultimate short block %d offset %ld size %d pkts %d\n",
        vhp1[0], sgi->sg_sh_blk_off, sgi->sg_sh_block, sgi->sg_sh_pkts);

    /* pass the number of normal write blocks back */
    sgi->sg_wr_blks_bs = sgi->sg_sh_blk_off / sgi->sg_wr_block;
    if (sgi->verbose>1) fprintf(sgalog,
        "Calculated %d blocks prior to this\n", sgi->sg_wr_blks_bs);

    /* move it to the final short block */
    vhp1 += vhp1[1] / sizeof(uint32_t);
    if (sgi->verbose>1) fprintf(sgalog,
        "Final (end) short block %d size %d pkts %d\n",
        vhp1[0], vhp1[1], (vhp1[1] - sgi->sg_wbht_size)/ sgi->read_size);

    /* capture result values */
    sgi->sg_se_block = vhp1[1];
    sgi->sg_se_blk_off = (void*)vhp1 - sgi->smi.start;
    sgi->sg_se_pkts = (vhp1[1] - sgi->sg_wbht_size) / sgi->read_size;

    /* check signature in new block */
    vhp1 += sgi->sg_wbht_size / sizeof(uint32_t);
    vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2);
    if (vds0.word != vds1.word || vds0.word != sgi->vdif_signature.word) {
        if (sgi->verbose>1) fprintf(sgalog,
            "But blocks appear to be corrupt\n");
        sgi->sg_sh_block = sgi->sg_sh_pkts = 0;
        sgi->sg_sh_blk_off = 0;
        sgi->sg_version = SG_VERSION_SIG_SDBLK_BAIL2;
        return;
    }
}

/*
 * First step, determine if there is or is not a short block at the end.
 */
static void sg_short_endfix(SGInfo *sgi)
{
    uint32_t *vhp0, *vhp1;
    VDIFsigu vds0, vds1;

    /* leave it for sg_normal_block() to declare short at start */
    if (sgi->smi.eomem - sgi->smi.start - sgi->sg_fht_size <sgi->sg_wr_block) {
        sgi->sg_se_block = 0;
        sgi->sg_se_blk_off = 0;
        sgi->sg_se_pkts = 0;
        return;
    }

    /* try to get two signature packets at the start of normal block at end */
    vhp0 = (uint32_t *)(sgi->smi.eomem -
        sgi->sg_wr_block + sgi->sg_wbht_size + sgi->pkt_offset);
    vhp1 = vhp0 + (sgi->read_size / sizeof(uint32_t));
    vds0.word = sg_get_vsig(vhp0, sgi->smi.start, sgi->verbose-2);
    vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2);

    /* usually, there is no short block at the end of the file */
    if (vds0.word == vds1.word && vds0.word == sgi->vdif_signature.word) {
        sgi->sg_se_block = 0;
        sgi->sg_se_blk_off = 0;
        sgi->sg_se_pkts = 0;
        update_fr_cnt_max_fr_ptr32(vhp0, sgi->frame_cnt_max);
        update_fr_cnt_max_fr_ptr32(vhp1, sgi->frame_cnt_max);
        return;
    }

    /* otherwise, shift to account for short block header tag */
    vhp0 -= sgi->sg_wbht_size / sizeof(uint32_t);
    vhp1 -= sgi->sg_wbht_size / sizeof(uint32_t);
    vds0.word = sg_get_vsig(vhp0, sgi->smi.start, sgi->verbose-2);
    vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2);
    if (vds0.word != vds1.word || vds0.word != sgi->vdif_signature.word) {
        /* this may happen if there are two short blocks at the end */
        vhp0 -= sgi->sg_wbht_size / sizeof(uint32_t);
        vhp1 -= sgi->sg_wbht_size / sizeof(uint32_t);
        vds0.word = sg_get_vsig(vhp0, sgi->smi.start, sgi->verbose-2);
        vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2);
        if (vds0.word != vds1.word || vds0.word != sgi->vdif_signature.word) {
            /* this would happen if the blocks were corrupt */
            sgi->sg_version = SG_VERSION_SIG_SEBLK_FAIL;
            if (sgi->verbose>1) fprintf(sgalog,
                "short_endfix: v0 v1 sig fail at offset %lu:%lu\n",
                (void*)vhp0 - sgi->smi.start, (void*)vhp1 - sgi->smi.start);
            return;
        } else {
            /* this happens when the final two blocks are short */
            if (sgi->verbose>1) fprintf(sgalog,
                "short_endfix: have 2 sb after offset %lu:%lu\n",
                (void*)vhp0 - sgi->smi.start, (void*)vhp1 - sgi->smi.start);
            sg_double_short(sgi, vhp0, vhp1);
            return;
        }
    }
    update_fr_cnt_max_fr_ptr32(vhp0, sgi->frame_cnt_max);

    /* and look for it somewhere between here and the end */
    while (vds0.word == vds1.word && vds0.word == sgi->vdif_signature.word) {
        update_fr_cnt_max_fr_ptr32(vhp1, sgi->frame_cnt_max);
        vhp0 = vhp1;
        vds0.word = vds1.word;
        vhp1 += (sgi->read_size / sizeof(uint32_t));
        if (vhp1 > (uint32_t *)(sgi->smi.eomem - sizeof(VDIFHeader))) {
            /* this would happen if the blocks were corrupt */
            sgi->sg_version = SG_VERSION_SIG_SEBLK_BAIL1;
            return;
        }
        vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-3);
    }

    /* ok, we didn't run off the end and the signatures are different */
    vhp1 += sgi->sg_wbht_size / sizeof(uint32_t);
    vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2);
    if (vds0.word != vds1.word || vds0.word != sgi->vdif_signature.word) {
        /* this would happen if the blocks were corrupt */
        sgi->sg_version = SG_VERSION_SIG_SEBLK_BAIL2;
        return;
    }
    update_fr_cnt_max_fr_ptr32(vhp1, sgi->frame_cnt_max);

    /* ok, vhp1 points to first packet of the short block at the end */
    vhp1 -= (sgi->sg_wbht_size + sgi->pkt_offset) / sizeof(uint32_t);
    if (sgi->smi.eomem - (void*)vhp1 != vhp1[1]) {
        /* would happen if the header tag data is incorrect */
        sgi->sg_version = SG_VERSION_SIG_SEBLK_BAIL3;
        return;
    }
    sgi->sg_se_block = vhp1[1];
    sgi->sg_se_blk_off = (void*)vhp1 - sgi->smi.start;
    sgi->sg_se_pkts = (vhp1[1] - sgi->sg_wbht_size) / sgi->read_size;
}

/*
 * Search for the runt (size sgi->sg_sh_block) within bp0 and bp1
 * where both point to valid packets, but only the first points
 * to a valid write block.  With each iteration, we test that we
 * are still sync'd with packets and test the size of the block.
 */
static uint32_t sg_binary_search(SGInfo *sgi,
    uint32_t lower, uint32_t upper, int offset)
{
    void *bpx, *start = sgi->smi.start;
    uint32_t *bhtx, xamen;
    VDIFsigu vdsx;
    while (upper > lower) {
        xamen = (upper + lower) / 2;
        bpx = start + sgi->sg_fht_size + xamen * (off_t)sgi->sg_wr_block;
        bhtx = (uint32_t *)bpx;
        vdsx.word = sg_get_vsig(bhtx + offset, start, sgi->verbose-2);
        if (sgi->verbose>1) fprintf(sgalog,
            "sgb: lower %u xamen %u upper %u size %u\n",
                lower, xamen, upper, bhtx[1]);

        if (vdsx.word != sgi->vdif_signature.word) {
            /* corruption */
            sgi->sg_version = SG_VERSION_SIG_SHBLK_BAIL;
            break;
        } else if (bhtx[1] == sgi->sg_sh_block) {
            /* found it */
            sgi->sg_sh_blk_off = (void*)bpx - start;
            update_fr_cnt_max_fr_ptr32(bhtx + offset, sgi->frame_cnt_max);
            break;
        } else if (bhtx[1] == sgi->sg_wr_block) {
            /* prevent an infinite loop */
            if (lower == xamen) {
                sgi->sg_version = SG_VERSION_SIG_SHBLK_BLOW;
                break;
            }
            /* new point is normal blocked, so runt is higher in mem */
            lower = xamen;
        } else {
            /* prevent an infinite loop */
            if (upper == xamen) {
                sgi->sg_version = SG_VERSION_SIG_SHBLK_BUPP;
                break;
            }
            /* new point is within a block, so runt is earlier in mem */
            upper = xamen;
        }
    }
    return(xamen);
}

/*
 * Search for the short block of size runt among blks normal blocks.
 * Since the block sizes are almost all the same, we check those in
 * addition to the signatures.  Use pointers to the blocks bh tags,
 * of which word 0 is the block num (ignore) and word 1 is the size.
 */
static uint32_t sg_short_search(SGInfo *sgi, uint32_t blks, off_t runt)
{
    void *bp0, *bp1, *start = sgi->smi.start;
    uint32_t *bht0, *bht1;
    VDIFsigu vds0, vds1;
    int offset = (sgi->sg_wbht_size + sgi->pkt_offset) / sizeof(uint32_t);

    sgi->sg_sh_block = runt;
    sgi->sg_sh_pkts = (runt - sgi->sg_wbht_size) / sgi->read_size;

    bp0 = start + sgi->sg_fht_size;
    bp1 = start + sgi->sg_fht_size + blks * (off_t)sgi->sg_wr_block;

    bht0 = (uint32_t *)bp0;
    bht1 = (uint32_t *)bp1;

    vds0.word = sg_get_vsig(bht0 + offset, start, sgi->verbose-2);
    vds1.word = sg_get_vsig(bht1 + offset, start, sgi->verbose-2);

    /* easy cases */
    if (vds0.word != sgi->vdif_signature.word ||
        vds1.word != sgi->vdif_signature.word) {
        /* blocking is corrupt */
        sgi->sg_version = (bht0[1] == sgi->sg_wr_block)
            ? SG_VERSION_SIG_SHBLK_FAIL1 : SG_VERSION_SIG_SHBLK_FAIL2;
        return(0);
    } else if (bht0[1] == runt && vds0.word == sgi->vdif_signature.word) {
        /* lower block is runt */
        sgi->sg_sh_blk_off = (void*)bp0 - start;
        update_fr_cnt_max_fr_ptr32(bht0 + offset, sgi->frame_cnt_max);
        return(0);
    } else if (bht1[1] == runt && vds1.word == sgi->vdif_signature.word) {
        /* upper block is runt */
        sgi->sg_sh_blk_off = (void*)bp1 - start;
        update_fr_cnt_max_fr_ptr32(bht1 + offset, sgi->frame_cnt_max);
        return(blks);
    }

    /* go look for it between lower and upper blocks */
    return(sg_binary_search(sgi, 0, blks, offset));
}

/*
 * After accounting for the runt, compute the normal block information
 */
static void sg_normal_block(SGInfo *sgi)
{
    off_t blockage, runt;
    uint32_t blks, bbsb;

    blockage = sgi->smi.size - sgi->sg_se_block - sgi->sg_fht_size;
    blks = blockage / sgi->sg_wr_block;
    runt = blockage - blks * (off_t)sgi->sg_wr_block;

    if (0 == blks) {        /* not likely, but possible */
        sgi->sg_sh_block = runt;
        sgi->sg_sh_blk_off = sgi->sg_fht_size;
        sgi->sg_sh_pkts = runt / sgi->read_size;
        bbsb = 0;
    } else if (runt > 0) {  /* we have to find it */
        if (sgi->sg_version == SG_VERSION_SIG_SDBLK_BAIL2) {
            if (sgi->verbose>1) fprintf(sgalog, "DBL sb prior fail\n");
            bbsb = 0;
        } else if (sgi->sg_sh_block == 0) {
            /* sgi->sg_sh_block normally zero, assigned by: */
            bbsb = sg_short_search(sgi, blks, runt);
        } else {
            /* double short block at end case already found it: */
            if (sgi->verbose>1) fprintf(sgalog, "DBL sb logic pass (%d)\n",
                sgi->sg_wr_blks_bs);
            bbsb = sgi->sg_wr_blks_bs;
            blks = bbsb;
        }
    } else {                /* no short block to find */
        bbsb = blks;
    }

    sgi->sg_wr_pkts = sgi->sg_wr_block / sgi->read_size;
    sgi->sg_wr_blks_bs = bbsb;
    sgi->sg_wr_pkts_bs = bbsb * sgi->sg_wr_pkts;
    sgi->sg_wr_blks_as = blks - bbsb;
    sgi->sg_wr_pkts_as = (blks - bbsb) * sgi->sg_wr_pkts;
    if (sgi->verbose>1) fprintf(sgalog, "Have %d normal blocks after\n",
        sgi->sg_wr_blks_as);
}

/*
 * Finalize a few bookkeeping parameters.
 */
static void sg_final_update(SGInfo *sgi)
{
    sgi->total_pkts = 
        sgi->sg_wr_pkts_bs + sgi->sg_sh_pkts +
        sgi->sg_wr_pkts_as + sgi->sg_se_pkts;
    sgi->sg_total_blks = sgi->sg_wr_blks_bs + sgi->sg_wr_blks_as +
        ((sgi->sg_sh_blk_off) ? 1 : 0) + ((sgi->sg_se_blk_off) ? 1 : 0);
    if (sgi->verbose>1) fprintf(sgalog, "Have %u total packets in %u blocks\n",
        sgi->total_pkts, sgi->sg_total_blks);
}
static void flat_final_update(SGInfo *sgi)
{
    sgi->total_pkts = sgi->smi.size / sgi->read_size;
    /* and zero things we don't use */
    sgi->sg_wr_pkts = sgi->sg_wr_pkts_bs = sgi->sg_wr_pkts_as = 0;
    sgi->sg_wr_blks_bs = sgi->sg_wr_blks_as = 0;
    sgi->sg_sh_block = sgi->sg_sh_pkts = 0;
    sgi->sg_se_block = sgi->sg_se_pkts = 0;
    sgi->sg_sh_blk_off = sgi->sg_se_blk_off = 0;
    sgi->sg_total_blks = 0;
    if (sgi->verbose>1) fprintf(sgalog, "Have %u total packets\n",
        sgi->sg_total_blks);
}

/*
 * Provide a string representation of the errors we might find
 */
char *sg_error_str(int err)
{
    switch (err) {
    case SG_VERSION_OK_2:
        return("Normal scatter-gather v2 file");
        break;
    case SG_VERSION_FLAT:
        return("Flat file of VDIF packets");
        break;
    case SG_VERSION_NOT:
        return("Unidentified file");
        break;
    case SG_VERSION_NOT_ENOUGH_DATA:
        return("File too small to hold useful packet data");
        break;
    case SG_VERSION_SIG_FIRST_FAIL:
        return("Unable to find two packets alike at the start of the file");
        break;
    case SG_VERSION_SIG_FINAL_FAIL:
        return("Unable to find two packets alike at the end of the file");
        break;
    case SG_VERSION_SIG_SEBLK_FAIL:
        return("Corruption searching for short block at end (fail1)");
        break;
    case SG_VERSION_SIG_SEBLK_BAIL1:
        return("Corruption searching for short block at end (bail1)");
        break;
    case SG_VERSION_SIG_SEBLK_BAIL2:
        return("Corruption searching for short block at end (bail2)");
        break;
    case SG_VERSION_SIG_SEBLK_BAIL3:
        return("Corruption searching for short block at end (bail3)");
        break;
    case SG_VERSION_SIG_SHBLK_FAIL1:
        return("Corruption searching for short block at start (fail1)");
        break;
    case SG_VERSION_SIG_SHBLK_FAIL2:
        return("Corruption searching for short block at start (fail2)");
        break;
    case SG_VERSION_SIG_SHBLK_BAIL:
        return("Corruption searching for short block at start (bail)");
        break;
    case SG_VERSION_SIG_SHBLK_BLOW:
        return("Corrupted starting short block search (lower)");
        break;
    case SG_VERSION_SIG_SHBLK_BUPP:
        return("Corrupted starting short block search (upper)");
        break;
    default:
        return("No idea what this means, sorry");
        break;
    }
}

/*
 * Methods to open a file and understand what is in it
 * The results are placed in the SGInfo structure.
 * All non-mm_* functions test sgi->sg_version before proceeding.
 * All unassigned data in the structure are zeroed at the start
 * and should be set to valid values at the end (if all is well).
 */
void sg_info(const char *file, SGInfo *sgi)
{
    double temptime;
    int verbcopy = sgi->verbose;
    SGMMInfo smicopy = sgi->smi;

    capture_process_secs(0, &temptime);
    if (sgi->name) free(sgi->name);
    memset(sgi, 0, sizeof(SGInfo));
    sgi->smi = smicopy;
    sgi->verbose = verbcopy;
    sgi->eval_time = temptime;
    sgi->sg_version = SG_VERSION_NOT;
    sgi->name = malloc(strlen(file)+4);
    strcpy(sgi->name, file);

    sg_file_header_tag_ok(sgi);
    if (sgi->sg_version == SG_VERSION_OK_2) {
        sg_first_packet(sgi);
        sg_final_packet(sgi);
        sg_short_endfix(sgi);
        sg_normal_block(sgi);
        sg_final_update(sgi);
    } else {
        flat_first_packet(sgi);
        if (sgi->sg_version == SG_VERSION_FLAT) {
            flat_final_packet(sgi);
            flat_final_update(sgi);
        }
    }

    capture_process_secs(1, &sgi->eval_time);
}

SGMMInfo *sg_open(const char *file, SGInfo *sgi)
{
    if (mm_init(file, &(sgi->smi), sgi->verbose)) return((SGMMInfo *)0);
    if (sgi) sg_info(file, sgi);
    return(&(sgi->smi));
}

SGMMInfo *sg_reopen(SGInfo *sgi)
{
    if (!sgi || !sgi->name) return((SGMMInfo *)0);
    if (mm_init(sgi->name, &(sgi->smi), sgi->verbose)) return((SGMMInfo *)0);
    /* sg_info would be redundant */
    return(&(sgi->smi));
}

void sg_close(SGInfo *sgi)
{
    mm_term(&sgi->smi, sgi->verbose);
}

void sg_access(const char *file, SGInfo *sgi)
{
    SGMMInfo *smi = sg_open(file, sgi);
    if (smi) sg_close(sgi);
}

/*
 * A diagnostic method to describe the file on stdout
 */
char *sg_repstr(SGInfo *sgi, char *label)
{
    static char buf[1024];
    char *lab = label ? label : "";
    switch (sgi->sg_version) {
    case SG_VERSION_OK_2:
        snprintf(buf, sizeof(buf),
            "%s%s (%s) %d@%d+%06d..%d+%06d\n"
            "%sSGv%d %luB %uB/Pkt %uP/b %ub %uP %.3lfms\n"
            "%swb:%u,%u,%uB %u(%u)+%ux%u+%u(%u)+%ux%u b(P)\n"
            "%ssg:%0lX >%dP/s :%lu:%lu %u|%u|%u:%u| s%u %u#\n",
            lab, sgi->name, sgi->smi.start ? "o" : "c",
                 sgi->ref_epoch,
                 sgi->first_secs, sgi->first_frame,
                 sgi->final_secs, sgi->final_frame,
            lab, sgi->sg_version, sgi->smi.size,
                 sgi->read_size, sgi->sg_wr_pkts,
                 sgi->sg_total_blks, sgi->total_pkts,
                 1.0e3 * sgi->eval_time,
            lab, sgi->sg_wr_block, sgi->sg_sh_block, sgi->sg_se_block,
                 sgi->sg_wr_blks_bs, sgi->sg_wr_pkts_bs,
                 sgi->sg_sh_blk_off?1:0, sgi->sg_sh_pkts,
                 sgi->sg_wr_blks_as, sgi->sg_wr_pkts_as,
                 sgi->sg_se_blk_off?1:0, sgi->sg_se_pkts,
            lab, sgi->vdif_signature.word, sgi->frame_cnt_max,
                 sgi->sg_sh_blk_off, sgi->sg_se_blk_off,
                 sgi->sg_fht_size, sgi->sg_wbht_size,
                 sgi->pkt_offset, sgi->pkt_size,
                 sgi->vdif_signature.bits.stationID,
                 sgi->vdif_signature.bits.num_channels
        );
        break;
    case SG_VERSION_FLAT:
        snprintf(buf, sizeof(buf),
            "%s%s (%s) %d@%d+%06d..%d+%06d\n"
            "%sFLAT %luB %uB/Pkt %uP %.3lfms\n"
            "%ssg:%0lX >%dP/s |%u:%u| s%u %u#\n",
            lab, sgi->name, sgi->smi.start ? "o" : "c",
                 sgi->ref_epoch,
                 sgi->first_secs, sgi->first_frame,
                 sgi->final_secs, sgi->final_frame,
            lab, sgi->smi.size,
                 sgi->read_size, sgi->total_pkts,
                 1.0e3 * sgi->eval_time,
            lab, sgi->vdif_signature.word, sgi->frame_cnt_max,
                 sgi->pkt_offset, sgi->pkt_size,
                 sgi->vdif_signature.bits.stationID,
                 sgi->vdif_signature.bits.num_channels
        );
        break;
    default:
        snprintf(buf, sizeof(buf),
            "%ssomething else (%d:%s)\n",
            lab, sgi->sg_version, sg_error_str(sgi->sg_version));
        break;
    }
    return(buf);
}
void sg_report(SGInfo *sgi, char *label)
{
    char *rep = sg_repstr(sgi, label);
    if (rep) fputs(rep, sgalog);
    else fprintf(sgalog, "%s: no report available\n", label);
}

/*
 * Random access methods: get the nn-th packet in the file.
 * On return, *end points to the end of the block, and *nl
 * to the number of packets left in the block.
 */
uint32_t *sg_pkt_by_num(SGInfo *sgi, off_t nn, int *nl, uint32_t **end)
{
    void *pktp, *endp;
    off_t nmpkts[4], ptrdel[4], pktcnt[4], bb, rr, ninp = nn;
    int ii, nlft;

    if (nl) *nl = 0;
    if (!sgi || !sgi->smi.start) return(NULL32P);
    if (nn < 0 || nn >= sgi->total_pkts) return(NULL32P);
    pktp = sgi->smi.start + sgi->sg_fht_size;
    endp = sgi->smi.start;
    nlft = 0;

    nmpkts[0] = sgi->sg_wr_pkts_bs;
    ptrdel[0] = (off_t)sgi->sg_wr_blks_bs * (off_t)sgi->sg_wr_block;
    pktcnt[0] = sgi->sg_wr_pkts;
    nmpkts[1] = sgi->sg_sh_pkts;
    ptrdel[1] = sgi->sg_sh_block;
    pktcnt[1] = sgi->sg_sh_pkts;
    nmpkts[2] = sgi->sg_wr_pkts_as;
    ptrdel[2] = (off_t)sgi->sg_wr_blks_as * (off_t)sgi->sg_wr_block;
    pktcnt[2] = sgi->sg_wr_pkts;
    nmpkts[3] = sgi->sg_se_pkts;
    ptrdel[3] = sgi->sg_se_block;
    pktcnt[3] = sgi->sg_se_pkts;

    for (ii = 0; ii < 4; ii++) {
        if (nn < nmpkts[ii]) {
            bb = nn / sgi->sg_wr_pkts;
            rr = nn % sgi->sg_wr_pkts;
            pktp += bb * sgi->sg_wr_block + rr * sgi->read_size;
            pktp += sgi->sg_wbht_size + sgi->pkt_offset;
            nlft = pktcnt[ii] - rr;
            endp = pktp + nlft * sgi->read_size;
            endp -= sgi->pkt_offset;
            break;
        } else {
            nn   -= nmpkts[ii];
            pktp += ptrdel[ii];
        }
    }
    if (ii == 4) pktp = NULL32P;
    if (nl) *nl = nlft;
    if (end) *end = (uint32_t *)endp;
    if (sgi->verbose>1) fprintf(sgalog, "sg_pkt_by_num(%lu<%lu<%lu)[%ld]\n",
        pktp - sgi->smi.start, endp - sgi->smi.start,
        sgi->smi.eomem - sgi->smi.start, ninp);
    if (pktp < sgi->smi.start || pktp >= sgi->smi.eomem) return(NULL32P);
    return((uint32_t *)pktp);
}

/*
 * Random access methods: get the 1st packet in the nn-th block.
 * On return, *end points to the end of the block, and *nl
 * to the number of packets left in the block.
 */
uint32_t *sg_pkt_by_blk(SGInfo *sgi, off_t nn, int *nl, uint32_t **end)
{
    void *pktp, *endp;
    off_t nmblks[4], ptrdel[4], pktcnt[4], ninp = nn;
    int ii, nlft;

    if (nl) *nl = 0;
    if (!sgi || !sgi->smi.start) return(NULL32P);
    if (nn < 0 || nn >= sgi->sg_total_blks) return(NULL32P);
    pktp = sgi->smi.start + sgi->sg_fht_size;
    endp = sgi->smi.start;
    nlft = 0;

    nmblks[0] = sgi->sg_wr_blks_bs;
    ptrdel[0] = (off_t)sgi->sg_wr_blks_bs * (off_t)sgi->sg_wr_block;
    pktcnt[0] = sgi->sg_wr_pkts;
    nmblks[1] = (sgi->sg_sh_blk_off) ? 1 : 0;
    ptrdel[1] = sgi->sg_sh_block;
    pktcnt[1] = sgi->sg_sh_pkts;
    nmblks[2] = sgi->sg_wr_blks_as;
    ptrdel[2] = (off_t)sgi->sg_wr_blks_as * (off_t)sgi->sg_wr_block;
    pktcnt[2] = sgi->sg_wr_pkts;
    nmblks[3] = (sgi->sg_se_blk_off) ? 1 : 0;
    ptrdel[3] = sgi->sg_se_block;
    pktcnt[3] = sgi->sg_se_pkts;

    for (ii = 0; ii < 4; ii++) {
        if (nn < nmblks[ii]) {
            pktp += nn * sgi->sg_wr_block;
            pktp += sgi->sg_wbht_size + sgi->pkt_offset;
            nlft = pktcnt[ii];
            endp = pktp + nlft * sgi->read_size;
            endp -= sgi->pkt_offset;
            break;
        } else {
            nn   -= nmblks[ii];
            pktp += ptrdel[ii];
        }
    }
    if (ii == 4) pktp = NULL32P;
    if (nl) *nl = nlft;
    if (end) *end = (uint32_t *)endp;
    if (sgi->verbose>1) fprintf(sgalog, "sg_pkt_by_blk(%lu<%lu<%lu)[%ld]\n",
        pktp - sgi->smi.start, endp - sgi->smi.start,
        sgi->smi.eomem - sgi->smi.start, ninp);
    if (pktp < sgi->smi.start || pktp >= sgi->smi.eomem) return(NULL32P);
    return((uint32_t *)pktp);
}

/*
 * A variant of the above that also returns the number of packet
 * bytes before the block in question and the bytes after it.
 * It differs in that we don't compute endp, and we compute after
 * we find the block in question.
 * 
 * (*nl * packet_size) + *pktbytesbefore + *pktbytesafter should
 * == total_packets * packet_size in a sane universe.
 */
uint32_t *sg_pkt_blkby(SGInfo *sgi, off_t nn, int *nl,
    off_t *pktbytesbefore, off_t *pktbytesafter)
{
    void *pktp;
    off_t nmblks[4], ptrdel[4], pktcnt[4], pbb, pba;
    int ii, nlft;

    if (nl) *nl = 0;
    if (pktbytesbefore) *pktbytesbefore = 0;
    if (pktbytesafter)  *pktbytesafter = 0;
    if (!sgi || !sgi->smi.start) return(NULL32P);
    if (nn < 0 || nn >= sgi->sg_total_blks) return(NULL32P);
    pktp = sgi->smi.start + sgi->sg_fht_size;
    nlft = 0;
    pbb = 0;    /* packets before */
    pba = 0;    /* packets after */

    nmblks[0] = sgi->sg_wr_blks_bs;
    ptrdel[0] = (off_t)sgi->sg_wr_blks_bs * (off_t)sgi->sg_wr_block;
    pktcnt[0] = sgi->sg_wr_pkts;
    nmblks[1] = (sgi->sg_sh_blk_off) ? 1 : 0;
    ptrdel[1] = sgi->sg_sh_block;
    pktcnt[1] = sgi->sg_sh_pkts;
    nmblks[2] = sgi->sg_wr_blks_as;
    ptrdel[2] = (off_t)sgi->sg_wr_blks_as * (off_t)sgi->sg_wr_block;
    pktcnt[2] = sgi->sg_wr_pkts;
    nmblks[3] = (sgi->sg_se_blk_off) ? 1 : 0;
    ptrdel[3] = sgi->sg_se_block;
    pktcnt[3] = sgi->sg_se_pkts;

    for (ii = 0; ii < 4; ii++) {
        if (nn < nmblks[ii]) {
            pktp += nn * sgi->sg_wr_block;
            pktp += sgi->sg_wbht_size + sgi->pkt_offset;
            nlft = pktcnt[ii];
            pbb  += nn * pktcnt[ii];         /* packets of this block type */
            pba  = (nmblks[ii] - nn - 1) * pktcnt[ii];
            break;
        } else {
            nn   -= nmblks[ii];
            pktp += ptrdel[ii];
            pbb  += nmblks[ii] * pktcnt[ii]; /* packets of this block type */
        }
    }
    if (ii == 4) pktp = NULL32P;
    for (ii++ ; ii < 4; ii++) {
        pba += nmblks[ii] * pktcnt[ii];      /* packets of this block type */
    }
    if (nl) *nl = nlft;
    *pktbytesbefore = pbb * sgi->pkt_size;
    *pktbytesafter  = pba * sgi->pkt_size;
    return((uint32_t *)pktp);
}


/*
 * Random access methods: get the packet at (or just before) offset nn
 * in the file.  On return, *end points to the end of the block, and *nl
 * to the number of packets left in the block.
 */
uint32_t *sg_pkt_by_off(SGInfo *sgi, off_t nn, int *nl, uint32_t **end)
{
    void *pktp, *endp;
    off_t ptrdel[4], pktcnt[4], bb, rr, pp, ninp = nn;
    int ii, nlft;

    if (nl) *nl = 0;
    if (!sgi || !sgi->smi.start) return(NULL32P);
    if (nn < 0 || nn >= sgi->smi.size) return(NULL32P);
    pktp = sgi->smi.start + sgi->sg_fht_size;
    endp = sgi->smi.start;
    nlft = 0;

    if (nn < sgi->sg_fht_size) nn = 0;
    else nn -= sgi->sg_fht_size;

    ptrdel[0] = (off_t)sgi->sg_wr_blks_bs * (off_t)sgi->sg_wr_block;
    pktcnt[0] = sgi->sg_wr_pkts;
    ptrdel[1] = sgi->sg_sh_block;
    pktcnt[1] = sgi->sg_sh_pkts;
    ptrdel[2] = (off_t)sgi->sg_wr_blks_as * (off_t)sgi->sg_wr_block;
    pktcnt[2] = sgi->sg_wr_pkts;
    ptrdel[3] = sgi->sg_se_block;
    pktcnt[3] = sgi->sg_se_pkts;

    for (ii = 0; ii < 4; ii++) {
        if (nn < ptrdel[ii]) {
            bb = nn / sgi->sg_wr_block;
            rr = nn % sgi->sg_wr_block;
            if (rr < sgi->sg_wbht_size) rr = sgi->sg_wbht_size;
            else rr -= sgi->sg_wbht_size;
            pp = rr / sgi->read_size;
            pktp += bb * sgi->sg_wr_block + pp * sgi->read_size;
            pktp += sgi->sg_wbht_size + sgi->pkt_offset;
            nlft = pktcnt[ii] - pp;
            endp = pktp + nlft * sgi->read_size;
            endp -= sgi->pkt_offset;
            break;
        } else {
            nn   -= ptrdel[ii];
            pktp += ptrdel[ii];
        }
    }
    if (ii == 4) pktp = NULL32P;
    if (nl) *nl = nlft;
    if (end) *end = (uint32_t *)endp;
    if (sgi->verbose>1) fprintf(sgalog, "sg_pkt_by_off(%lu<%lu<%lu)[%ld]\n",
        pktp - sgi->smi.start, endp - sgi->smi.start,
        sgi->smi.eomem - sgi->smi.start, ninp);
    if (pktp < sgi->smi.start || pktp >= sgi->smi.eomem) return(NULL32P);
    return((uint32_t *)pktp);
}

/*
 * Check the signatures on some number of packets starting with the
 * pkt pointer provided above.  It should return 0 if the number left
 * and end pointer are also as provided (or less restrictive).
 *
 * Despite the name, this method works as well for flat files.
 */
int sg_pkt_check(SGInfo *sgi, uint32_t *pkt, int nl, uint32_t *end)
{
    int nbad = 0;
    VDIFsigu vds;
    if (sgi->verbose>1) fprintf(sgalog, "sg_pkt_check(%lu<%lu)[%d]\n",
        (void*)pkt - sgi->smi.start, (void*)end - sgi->smi.start, nl);
    if (nl < 0) return(-nl);
    if (!sgi || (sgi->smi.start == 0)) return(nl);
    while (nl-- > 0) {
        vds.word = sg_get_vsig(pkt, sgi->smi.start, sgi->verbose-2);
        if ((pkt >= end) || vds.word != (sgi->vdif_signature.word)) nbad ++;
        update_fr_cnt_max_fr_ptr32(pkt, sgi->frame_cnt_max);
        pkt += sgi->read_size / sizeof(uint32_t);
    }
    return(nbad);
}

static void pr_skip(int ty, VDIFHeader *ah, VDIFHeader *bh, void *start)
{
    static char *desc[] = {
        "timing: same second, next frame",
        "timing: next second, frame -> 0",
        "timing: multi-second packet gap"
    };
    fprintf(sgalog, "%s, %u+%06u at %lu\n%s, %u+%06u at %lu\n",
        desc[ty], ah->w1.secs_inre, ah->w2.df_num_insec, (void*)ah - start,
        desc[ty], bh->w1.secs_inre, bh->w2.df_num_insec, (void*)bh - start);
}

/*
 * Check the timestamps on the packets.  It should return 0 if the
 * packets are sequential, otherwise nonzero if there are jumps.
 *
 * Despite the name, this method works as well for flat files.
 */
int sg_pkt_times(SGInfo *sgi, uint32_t *pkt, int nl, uint32_t *end)
{
    VDIFHeader *ah = (VDIFHeader *)pkt, *bh;
    int ds, skips = 0;
    if (sgi->verbose>1) fprintf(sgalog, "sg_pkt_times(%lu<%lu)[%d]\n",
        (void*)pkt - sgi->smi.start, (void*)end - sgi->smi.start, nl);
    while (--nl > 0) {
        pkt += sgi->read_size / sizeof(uint32_t);
        bh = (VDIFHeader *)pkt;
        if (ah->w1.secs_inre == bh->w1.secs_inre) {
            /* same second, next frame */
            ds = (ah->w2.df_num_insec + 1 != bh->w2.df_num_insec) ? 1 : 0;
            if (ds && sgi->verbose>1) pr_skip(0, ah, bh, sgi->smi.start);
        } else if (ah->w1.secs_inre + 1 ==  bh->w1.secs_inre) {
            /* next second, frame -> 0 */
            ds = (bh->w2.df_num_insec != 0) ? 1 : 0;
            if (ds && sgi->verbose>1) pr_skip(1, ah, bh, sgi->smi.start);
        } else {
            /* multi-second packet gap */
            ds = 1;
            if (ds && sgi->verbose>1) pr_skip(2, ah, bh, sgi->smi.start);
        }
        ah = bh;
        skips += ds;
    }
    return(skips);
}

/*
 * eof
 */
