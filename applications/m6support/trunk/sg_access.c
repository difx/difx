/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: sg_access.c 5776 2023-03-27 16:42:34Z gbc $
 *
 * Code to understand and access sg(2) files efficiently.
 *
 * The code is a bit contorted as it was developed for the original
 * sg version; issues with that resulted in the sg2 version which is
 * at this point the only one that needs to be supported.  Some of
 * the tools and code are adapted from the burst mode recorder and
 * earlier (non-scatter gather) Mark6 prototypes.
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif /* _GNU_SOURCE */
#include <fcntl.h>

#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif /* O_CLOEXEC */

#include <ctype.h>
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

/* bring in vdif_epochs[] */
#include "vdif_epochs.h"

/* macros to simplify updating sgi->frame_cnt_max */
#define update_fr_cnt_max_fr_frame(T,M) do {\
    if ((T)>(M)) (M) = (T); } while(0)
#define update_fr_cnt_max_fr_ptr32(P,M) do {\
    uint32_t frame = ((VDIFHeader *)(P))->w2.df_num_insec;\
    if (frame > (M)) (M) = frame; } while(0)

/* TODO: internalize sgalog and provide connection to trace */
static FILE *sgalog = 0;

/*
 * For logging to something other than stdout; however, logging
 * to stdout is legal and we should be careful not to close it.
 * If we are already logging to the same place, do nothing.
 *
 * A method to get the FILE handle is also provided.
 */
void sg_logger(FILE *fp)
{
    if (fp == sgalog || !fp) return;
    if (sgalog && (sgalog != stdout)) {
        fclose(sgalog);
        sgalog = NULL;
    }
    sgalog = fp;
}
FILE *sg_logfile(void)
{
    return(sgalog);
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
 * Compute and return a checksum from an open file; to be efficient,
 * we'll just use the first few bytes in the file.  Caller shouldn't
 * be calling without data, but might as well be safe and robust.
 */
uint32_t sg_checksum(SGInfo *sgi)
{
    uint32_t *ptr = (sgi ? (uint32_t*)(sgi->smi.start) : NULL), sum = 0;
    if (!ptr) return(0);
    int bytes = 23;
    while (bytes-- > 0) sum ^= *ptr++;
    return(sum);
}

/*
 * Compute and return vdif signature for the packet found at vh.
 *
 * VDIF packets should be uint64_t aligned, but the original
 * specification is by 4B word.  This function is a wrapper
 * around the more complicated method that follows.
 */
uint64_t sg_signature(uint32_t *vhp)
{
    VDIFsigu vdif_signature;
    VDIFHeader *vh = (VDIFHeader *)vhp;
    if (!vh) return(0x0);
    vdif_signature.word              = 0LL;
    vdif_signature.bits.df_len       = vh->w3.df_len;
    vdif_signature.bits.ref_epoch    = vh->w2.ref_epoch;
#if ROLLOVER < 2
    vdif_signature.bits.UA           = vh->w2.UA;   /* ROLLOVER < 2 */
#endif /* ROLLOVER < 2 uses UA */
    vdif_signature.bits.stationID    = vh->w4.stationID & sg_station_id_mask;
    vdif_signature.bits.num_channels = vh->w3.num_channels;
    vdif_signature.bits.ver          = vh->w3.ver;
    vdif_signature.bits.bps          = vh->w4.bps;
    vdif_signature.bits.dt           = vh->w4.dt;
    vdif_signature.bits.legacy       = vh->w1.legacy;
    vdif_signature.bits.unused       = 0x1;
    return(vdif_signature.word);
}

/*
 * More complicated version that does checking and updating.
 *
 * o is an origin for debugging when verb is nonzero.
 * vc (if not null) points to the expected signature.
 * If the packet is marked invalid, then if vc is not null, it will be returned
 * assuming the packet header is zero except for the invalid bit and length.
 * (I.e. fill packets are presumed to be filled correctly.)
 * When thp is not null, vdif thread info is captured and optionally reported.
 */
uint64_t sg_get_vsig(uint32_t *vhp, void *o, int verb, char *lab,
    VDIFsigu *vc, short *thp)
{
    VDIFsigu vdif_signature;
    VDIFHeader *vh = (VDIFHeader *)vhp;
    static char *labs[5] = { "??", "ok", "nv", "OK", "NV" };
    static char threp[VTHREP_BUFFER];
    int labi = 0, thup = 0;

    vdif_signature.word = sg_signature(vhp);
    if (vdif_signature.word == 0x0) {
        if (verb>0) fprintf(sgalog, "%s%18p %016lX (%s) at %lu\n",
            lab, vhp, vdif_signature.word, labs[labi], (void *)vhp - o);
        return(0x0);
    }

    if (vhp && vc) {       /* have a basis for checking/fixing */
        if (vh->w1.invalid) {
            labi = (vdif_signature.word == (1LLU<<63 | vh->w3.df_len)) ? 1 : 2;
            if (1 == labi) vdif_signature.word = vc->word;
        } else {    /* a valid packet had better agree */
            labi = (vdif_signature.word == vc->word) ? 3 : 4;
        }
    }
    /* update list of threads only on 'ok' or 'OK' */
    if (thp && (labi==1||labi==3)) {
        thup = sg_vthreads(thp, vh->w4.threadID, verb>0 ? threp : 0);
        if (verb>0) fputs(threp, sgalog);
    }
    if (verb>0) fprintf(sgalog,
        "%s%18p %016lX (%s) at %lu (%d%c)\n"
        "%s  %08x%08x %08x%08x %08x%08x %08x%08x\n",
        lab, vhp, vdif_signature.word, labs[labi],
        (void *)vhp - o, vh->w4.threadID, thup ? '+' : ' ',
        lab, vhp[1], vhp[0], vhp[3], vhp[2], vhp[5], vhp[4], vhp[7], vhp[6]);
    return(vdif_signature.word);
}

/* Set a bound on legality since lower thread ids are the norm */
static int max_legal_vthread_id = MAX_LEGAL_THREAD_ID;
void sg_set_max_legal_vthread_id(int max)
{
    if (max < 0 || max > MAX_LEGAL_THREAD_ID) return;
    max_legal_vthread_id = max;
}
int sg_get_max_legal_vthread_id(void)
{
    return(max_legal_vthread_id);
}

/*
 * Some management for threads; assumes thp[] entries are initialized
 * with invalid values (e.g. -1).  Report here is just for diagnostics.
 * Initialization to -1 is done in sg_info()
 * Returns 1 if a new thread was identified.
 */
int sg_vthreads(short *thp, short tid, char *threp)
{
    static int bads = 0;
    int ii = 0, rv = 0;
    if (tid < 0 || tid > max_legal_vthread_id) {
        bads++;
    } else for (ii = 0; ii < MAX_VDIF_THREADS; ii++) {
        if (tid == thp[ii]) {
            break;                      /* already have it */
        } else if (thp[ii] >= 0 && thp[ii] <= max_legal_vthread_id) {
            continue;                   /* skip legal entries */
        } else {
            rv = 1;                     /* for caller's benefit */
            thp[ii] = tid;
            break;                      /* legally added */
        }
    }
    if (threp) snprintf(threp, VTHREP_BUFFER,
        "vthreads: #%d %s[bd%d:rv%d(%s)] "
        "%d,%d,%d,%d,%d,%d,%d,%d,"
        "%d,%d,%d,%d,%d,%d,%d,%d\n",
        tid, ii < MAX_VDIF_THREADS ? "ok" : "er", bads, rv, rv?"new":"old",
        thp[0], thp[1], thp[2], thp[3], thp[4], thp[5], thp[6], thp[7],
        thp[8], thp[9], thp[10], thp[11], thp[12], thp[13], thp[14], thp[15]);
    return(rv);
}
char *sg_vthreads_rep(short *thp)
{
    static char threp[VTHREP_BUFFER];
    snprintf(threp, VTHREP_BUFFER,
        "vthreads: %d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d",
        thp[0], thp[1], thp[2], thp[3], thp[4], thp[5], thp[6], thp[7],
        thp[8], thp[9], thp[10], thp[11], thp[12], thp[13], thp[14], thp[15]);
    return(threp);
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
 * A commonly needed time utility.
 *   If a previous time is provided, return the elapsed time
 *   otherwise, just return the current time.
 * Either way, the value is seconds to us precision.
 */
double sg_current_or_elapsed_secs(double *previous)
{
    struct timeval now;
    double dnow = (!gettimeofday(&now, 0))
                ? (double)now.tv_sec + 1e-6 * (double)now.tv_usec
                : 0.0;
    if (previous) dnow -= *previous;
    return(dnow);
}
/*
 * Compute and return the time since *when, if a pointer
 * is provided; then store the current time in *when.
 *
 * using gprof is better...so this could go away.
 */
void sg_secs_since(struct timeval *when, double *secs)
{
    struct timeval now;
    gettimeofday(&now, 0);
    if (secs) {
        *secs  = (double)(now.tv_sec - when->tv_sec);
        *secs += (double)(now.tv_usec - when->tv_usec) * 1e-6;
    }
    *when = now;
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
 *
 * flags was originally MAP_SHARED, but MAP_POPULATE|MAP_NORESERVE
 * were added in an attempt to increase throughput.  MAP_POPULATE
 * proceeds to load the whole thing at open.  Not good.
 */
static int mm_init(const char *file, SGMMInfo *smi, int verb)
{
    struct stat mm_stb;
    if (!sgalog) sg_logger(stdout);
    if (stat(file, &mm_stb) || mm_stb.st_size <= 0)
        return(perror("mm_init:stat"),1);
    if (mm_uchk(smi, mm_stb.st_size, verb)) return(0);
    /* first user */
    smi->mmfd = open(file, O_RDONLY|O_CLOEXEC);
    if (smi->mmfd < 0)
        return(perror("mm_init:open"),2);
    smi->start = mmap(0, smi->size = mm_stb.st_size, 
        PROT_READ, MAP_SHARED|MAP_NORESERVE, smi->mmfd, 0);
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
    sg_advice_term(smi->mmfd);
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
 *
 * The read_size might include a per-packet offset,
 * e.g. in VTP, a sequence number might precede the
 * packet and it might have been recorded.
 */
static void sg_file_header_tag_ok(SGInfo *sgi)
{
    uint32_t *words = (uint32_t *)(sgi->smi.start);

    if (words[0] != SG_VERSION_MAGIC) return;
    if (words[1] == 0x2) sgi->sg_version = SG_VERSION_OK_2;
    else return;


    sgi->sg_fht_size = sizeof(SGV2Header);
    sgi->sg_wbht_size = sizeof(SGV2BlkNum);
    sgi->sg_wr_block = words[2];    /* block_size */
    /* (words[3] != 0x0) packet format: 0 for VDIF */
    sgi->read_size = words[4];      /* packet_size */
    sgi->frame_cnt_max = 0;

    /* need at least 4 packets */
    if (sgi->smi.size < 20 + 8 + 4 * sgi->read_size)
        sgi->sg_version = SG_VERSION_NOT_ENOUGH_DATA;
}

/*
 * Capture timestamp on first packet.
 *
 * Work out the offset of the VDIF packet in the read packet.
 * There could be a PSN preceding the VDIF packet, for example.
 *
 * This is common code that works on any consecutive runs of packets,
 * given an initial offset.  In the SG case, we know the read size;
 * in other cases, we assume it is the packet size plus the offset.
 */
static void sg_first_packet_common(SGInfo *sgi, off_t foff, off_t *rwdsp)
{
    uint32_t *vhp0, *vhp1;
    VDIFsigu vds0, vds1, *vdsp = (VDIFsigu *)0;
    off_t poff = 0, rwords = *rwdsp;

    if (sgi->verbose>2) fprintf(sgalog,
        "first_packet: bytes %lu offset %lu rwdsp %lu\n",
            sgi->smi.eomem - sgi->smi.start, foff, (*rwdsp)*sizeof(uint32_t));

    /* this may have been found by valid_first_signature() */
    if (sgi->vdif_signature.word) {
        vdsp = &sgi->vdif_signature;
        if (sgi->verbose>2) fprintf(sgalog, "signature: %lX\n", vdsp->word);
    }

    /* vhp0 might point at a packet, vhp1 might point at the next */
    do {
        vhp0 = (uint32_t *)(sgi->smi.start + foff + poff);
        vds0.word = sg_get_vsig(vhp0, sgi->smi.start, sgi->verbose-2,
            "fpc0:", vdsp, sgi->vthreads);
        if (*rwdsp == 0) {
            rwords = 2 * vds0.bits.df_len + poff / sizeof(uint32_t);
            if (rwords > SG_MAX_VDIF_BYTES/4) rwords = SG_MAX_VDIF_BYTES/4;
        }
        vhp1 = vhp0 + rwords;
        vds1.word = (vhp1 < (uint32_t *)(sgi->smi.eomem - sizeof(VDIFHeader)))
                  ? sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2,
                    "fpc1:", vdsp, sgi->vthreads) : 0LL;
    } while ((vds0.word != vds1.word) &&
             ((poff += sizeof(uint32_t)) < rwords));
    
    /* no word alignment produced equal signatures */
    if (vds0.word != vds1.word) {
        sgi->sg_version = SG_VERSION_SIG_FIRST_FAIL;
        if (sgi->verbose>2)
            fprintf(sgalog, "fail: %s\n", sg_error_str(sgi->sg_version));
        return;
    }
    *rwdsp = rwords;

    /* yes, we are pointed at two packets */
    sgi->pkt_size = ( ((VDIFHeader *)vhp0)->w3.df_len ) * 8;
    sgi->pkt_offset = poff;
    sgi->vdif_signature.word = vds0.word;

    /* mark first time as invalid and return to it later */
    if (((VDIFHeader *)vhp0)->w1.invalid) {
        sgi->ref_epoch = sgi->first_secs = sgi->first_frame = 0;
    } else {
        sgi->ref_epoch = ((VDIFHeader *)vhp0)->w2.ref_epoch;
        sgi->first_secs = ((VDIFHeader *)vhp0)->w1.secs_inre;
        sgi->first_frame = ((VDIFHeader *)vhp0)->w2.df_num_insec;
        update_fr_cnt_max_fr_frame(sgi->first_frame, sgi->frame_cnt_max);
    }
}

/*
 * Try harder to get a valid signature, then the remaining
 * logic can treat invalid packets as if they were valid.
 *
 * It might be necessary to have the user supply the read
 * size and offset:  sgi->read_size and sgi->pkt_offset
 *
 * valid_first_signature() returns either a valid signature found,
 * or zero, which adjusts the behavior in sg_first_packet_common()
 */
static off_t user_poff = 0;
static uint32_t user_size = 0, user_set = 0;
void sg_set_user_poff_and_size(const char *arg)
{
    int ns = sscanf(arg, "%lu:%u", &user_poff, &user_size);
    if (ns != 2) {
        fprintf(stderr, "Illegal '%s' (packet_offset, packet_size)\n", arg);
        user_poff = 0;
        user_size = 0;
        user_set = 0;
        return;
    }
    user_set = ns;
}
static uint64_t valid_first_signature(SGInfo *sgi, off_t foff, uint32_t rdsize)
{
    uint32_t *vhp0, *vhp1;
    VDIFHeader *vh0, *vh1;
    VDIFsigu vds0, vds1;
    int count = 0;

    if (user_set == 0 && rdsize == 0) return(0LL);    /* no hope */
    if (rdsize == 0 && user_size != 0) rdsize = user_size;
    if (sgi->verbose>1) fprintf(sgalog,
        "valid_first_signature(poff = %lu size = %u)\n", user_poff, user_size);

    /*
     * Step through 1200 packets until we find a pair of signatures
     * that agree with the read size and are not invalid packets.
     * The user needs to supply any nonzero packet offset.
     */
    vhp0 = (uint32_t *)(sgi->smi.start + foff + user_poff);
    do {
        vh0 = (VDIFHeader *)vhp0;
        vds0.word = sg_get_vsig(vhp0, sgi->smi.start, sgi->verbose-2,
            "vfs0:", (VDIFsigu *)0, sgi->vthreads);
        vhp1 = vhp0 + rdsize / sizeof(uint32_t);
        vh1 = (VDIFHeader *)vhp1;
        if (vhp1 > (uint32_t *)(sgi->smi.eomem - sizeof(VDIFHeader)))
            return(0LL);
        vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2,
                    "vfs1:", (VDIFsigu *)0, sgi->vthreads);
        if ((vds0.word == vds1.word) &&
            !(vh0->w1.invalid || vh1->w1.invalid)) {
                if (sgi->verbose>1) fprintf(sgalog,
                    "valid_first_signature after %d packets: "
                    "%016lX == %016lX\n", count, vds0.word, vds1.word);
                return(vds0.word);
        }
        vhp0 = vhp1;
        count ++;
    } while (vh0->w1.invalid || vh1->w1.invalid || count < 1200);
    return(0LL);
}

/*
 * Call sg_first_packet_common() with the proper offset.
 */
static void sg_first_packet(SGInfo *sgi)
{
    off_t foff = sgi->sg_fht_size + sgi->sg_wbht_size;
    off_t rwds = sgi->read_size / sizeof(uint32_t);
    sgi->vdif_signature.word =
        valid_first_signature(sgi, foff, sgi->read_size);
    sgi->sg_first_bnum = *(int*)(sgi->smi.start + sgi->sg_fht_size);
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
    sgi->vdif_signature.word =
        valid_first_signature(sgi, foff, 0);
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
 *
 * Note that sg_get_vsig() may have lied about the signature, so if the
 * end packet is invalid, we will want to try harder to get final time.
 */
static void sg_final_packet(SGInfo *sgi)
{
    uint32_t *vhp0, *vhp1;
    VDIFsigu vds0, vds1;

    vhp0 = (uint32_t *)(sgi->smi.eomem - 2 * sgi->read_size + sgi->pkt_offset);
    vhp1 = (uint32_t *)(sgi->smi.eomem - 1 * sgi->read_size + sgi->pkt_offset);
    vds0.word = sg_get_vsig(vhp0, sgi->smi.start, sgi->verbose-1,
        "fnp0:", &sgi->vdif_signature, sgi->vthreads);
    vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-1,
        "fnp1:", &sgi->vdif_signature, sgi->vthreads);
    if (vds0.word != vds1.word || vds0.word != sgi->vdif_signature.word) {
        sgi->sg_version = SG_VERSION_SIG_FINAL_FAIL;
        if (sgi->verbose>2)
            fprintf(sgalog, "fail: %s\n", sg_error_str(sgi->sg_version));
        return;
    }

    /* mark final time as invalid and return to it later */
    if (((VDIFHeader *)vhp1)->w1.invalid) {
        sgi->final_secs = sgi->final_frame = 0;
    } else {
        /* yes, we are pointed at a valid final packet */
        sgi->final_secs = ((VDIFHeader *)vhp1)->w1.secs_inre;
        sgi->final_frame = ((VDIFHeader *)vhp1)->w2.df_num_insec;
        update_fr_cnt_max_fr_frame(sgi->final_frame, sgi->frame_cnt_max);
    }
}

/*
 * The flat version requires the exact same girations
 */
static void flat_final_packet(SGInfo *sgi)
{
    sg_final_packet(sgi);
}

/*
 * Handle the two-short blocks at the end of the file case.  When called,
 * we are somewhere in the last full block, and we need to find two
 * short blocks.  The first is sh, the second is se.  Part of the logic
 * is similar to the 2nd half sg_short_endfix(), but it's clearer to
 * code it here.
 *
 * And we can to skip sg_short_search() in sg_normal_block() later.
 */
static void sg_double_short(SGInfo *sgi, uint32_t *vhp0, uint32_t *vhp1)
{
    VDIFsigu vds0, vds1;

    vds0.word = sg_get_vsig(vhp0, sgi->smi.start, sgi->verbose-2,
        "dsh0:", &sgi->vdif_signature, sgi->vthreads);
    vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2,
        "dsh1:", &sgi->vdif_signature, sgi->vthreads);

    update_fr_cnt_max_fr_ptr32(vhp0, sgi->frame_cnt_max);
    while (vds0.word == vds1.word && vds0.word == sgi->vdif_signature.word) {
        update_fr_cnt_max_fr_ptr32(vhp1, sgi->frame_cnt_max);
        vhp0 = vhp1;
        vds0.word = vds1.word;
        vhp1 += (sgi->read_size / sizeof(uint32_t));
        if (vhp1 > (uint32_t *)(sgi->smi.eomem - sizeof(VDIFHeader))) {
            /* this would happen if the blocks were corrupt */
            sgi->sg_version = SG_VERSION_SIG_SDBLK_BAIL1;
            if (sgi->verbose>2)
                fprintf(sgalog, "fail: %s\n", sg_error_str(sgi->sg_version));
            return;
        }
        vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-3,
            "dswh:", &sgi->vdif_signature, sgi->vthreads);
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
    vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2,
        "dsnw:", &sgi->vdif_signature, sgi->vthreads);
    if (vds0.word != vds1.word || vds0.word != sgi->vdif_signature.word) {
        if (sgi->verbose>1) fprintf(sgalog,
            "But blocks appear to be corrupt\n");
        sgi->sg_sh_block = sgi->sg_sh_pkts = 0;
        sgi->sg_sh_blk_off = 0;
        sgi->sg_version = SG_VERSION_SIG_SDBLK_BAIL2;
        if (sgi->verbose>2)
            fprintf(sgalog, "fail: %s\n", sg_error_str(sgi->sg_version));
        return;
    }
    if (sgi->verbose>2) fprintf(sgalog,
        "double_short: have both short blocks\n");
}

/*
 * First step, after the first and final packets is to determine if there
 * is or is not a short block at the end.
 */
static void sg_short_endfix(SGInfo *sgi)
{
    uint32_t *vhp0, *vhp1;
    VDIFsigu vds0, vds1;

    if (sgi->verbose>1) fprintf(sgalog,
        "short_endfix: %lu bytes %u header %u wb\n",
            sgi->smi.eomem - sgi->smi.start,
            sgi->sg_fht_size, sgi->sg_wr_block);

    /* leave it for sg_normal_block() to declare short at start */
    if ((sgi->smi.eomem - sgi->smi.start - sgi->sg_fht_size) <
        sgi->sg_wr_block) {
        sgi->sg_se_block = 0;
        sgi->sg_se_blk_off = 0;
        sgi->sg_se_pkts = 0;
        if (sgi->verbose>1) fprintf(sgalog,
            "short_endfix: we have only one short write block\n");
        return;
    }

    /* try to get two signature packets at the start of normal block at end */
    vhp0 = (uint32_t *)(sgi->smi.eomem -
        sgi->sg_wr_block + sgi->sg_wbht_size + sgi->pkt_offset);
    vhp1 = vhp0 + (sgi->read_size / sizeof(uint32_t));
    vds0.word = sg_get_vsig(vhp0, sgi->smi.start, sgi->verbose-2,
        "sef0:", &sgi->vdif_signature, sgi->vthreads);
    vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2,
        "sef1:", &sgi->vdif_signature, sgi->vthreads);

    /* usually, there is no short block at the end of the file */
    if (vds0.word == vds1.word && vds0.word == sgi->vdif_signature.word) {
        sgi->sg_se_block = 0;
        sgi->sg_se_blk_off = 0;
        sgi->sg_se_pkts = 0;
        update_fr_cnt_max_fr_ptr32(vhp0, sgi->frame_cnt_max);
        update_fr_cnt_max_fr_ptr32(vhp1, sgi->frame_cnt_max);
        if (sgi->verbose>1) fprintf(sgalog,
            "short_endfix: no short block at end of file\n");
        return;
    }
    if (sgi->verbose>2) fprintf(sgalog,
        "short_endfix: pinning down short block at end of file\n");

    /* otherwise, shift to account for short block header tag */
    vhp0 -= sgi->sg_wbht_size / sizeof(uint32_t);
    vhp1 -= sgi->sg_wbht_size / sizeof(uint32_t);
    vds0.word = sg_get_vsig(vhp0, sgi->smi.start, sgi->verbose-2,
        "sef2:", &sgi->vdif_signature, sgi->vthreads);
    vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2,
        "sef3:", &sgi->vdif_signature, sgi->vthreads);
    if (vds0.word != vds1.word || vds0.word != sgi->vdif_signature.word) {
        /* this may happen if there are two short blocks at the end */
        vhp0 -= sgi->sg_wbht_size / sizeof(uint32_t);
        vhp1 -= sgi->sg_wbht_size / sizeof(uint32_t);
        vds0.word = sg_get_vsig(vhp0, sgi->smi.start, sgi->verbose-2,
            "sef4:", &sgi->vdif_signature, sgi->vthreads);
        vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2,
            "sef5:", &sgi->vdif_signature, sgi->vthreads);
        if (vds0.word != vds1.word || vds0.word != sgi->vdif_signature.word) {
            /* this would happen if the blocks were corrupt */
            sgi->sg_version = SG_VERSION_SIG_SEBLK_FAIL;
            if (sgi->verbose>2)
                fprintf(sgalog, "fail: %s\n", sg_error_str(sgi->sg_version));
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
            if (sgi->verbose>2)
                fprintf(sgalog, "fail: %s\n", sg_error_str(sgi->sg_version));
            return;
        }
        vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-3,
            "sef6:", &sgi->vdif_signature, sgi->vthreads);
    }

    /* ok, we didn't run off the end and the signatures are different */
    vhp1 += sgi->sg_wbht_size / sizeof(uint32_t);
    vds1.word = sg_get_vsig(vhp1, sgi->smi.start, sgi->verbose-2,
        "sef7:", &sgi->vdif_signature, sgi->vthreads);
    if (vds0.word != vds1.word || vds0.word != sgi->vdif_signature.word) {
        /* this would happen if the blocks were corrupt */
        sgi->sg_version = SG_VERSION_SIG_SEBLK_BAIL2;
        if (sgi->verbose>2)
            fprintf(sgalog, "fail: %s\n", sg_error_str(sgi->sg_version));
        return;
    }
    update_fr_cnt_max_fr_ptr32(vhp1, sgi->frame_cnt_max);

    /* ok, vhp1 points to first packet of the short block at the end */
    vhp1 -= (sgi->sg_wbht_size + sgi->pkt_offset) / sizeof(uint32_t);
    if (sgi->smi.eomem - (void*)vhp1 != vhp1[1]) {
        /* would happen if the header tag data is incorrect */
        sgi->sg_version = SG_VERSION_SIG_SEBLK_BAIL3;
        if (sgi->verbose>2)
            fprintf(sgalog, "fail: %s\n", sg_error_str(sgi->sg_version));
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
        vdsx.word = sg_get_vsig(bhtx + offset, start, sgi->verbose-2,
            "bshx:", &sgi->vdif_signature, sgi->vthreads);
        if (sgi->verbose>1) fprintf(sgalog,
            "sgb: lower %u xamen %u upper %u size %u\n",
                lower, xamen, upper, bhtx[1]);

        if (vdsx.word != sgi->vdif_signature.word) {
            /* corruption */
            sgi->sg_version = SG_VERSION_SIG_SHBLK_BAIL;
            if (sgi->verbose>2)
                fprintf(sgalog, "fail: %s\n", sg_error_str(sgi->sg_version));
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
                if (sgi->verbose>2) fprintf(sgalog,
                    "fail: %s\n", sg_error_str(sgi->sg_version));
                break;
            }
            /* new point is normal blocked, so runt is higher in mem */
            lower = xamen;
        } else {
            /* prevent an infinite loop */
            if (upper == xamen) {
                sgi->sg_version = SG_VERSION_SIG_SHBLK_BUPP;
                if (sgi->verbose>2) fprintf(sgalog,
                    "fail: %s\n", sg_error_str(sgi->sg_version));
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

    vds0.word = sg_get_vsig(bht0 + offset, start, sgi->verbose-2,
        "ssh0:", &sgi->vdif_signature, sgi->vthreads);
    vds1.word = sg_get_vsig(bht1 + offset, start, sgi->verbose-2,
        "ssh1:", &sgi->vdif_signature, sgi->vthreads);

    /* easy cases */
    if (vds0.word != sgi->vdif_signature.word ||
        vds1.word != sgi->vdif_signature.word) {
        /* blocking is corrupt */
        sgi->sg_version = (bht0[1] == sgi->sg_wr_block)
            ? SG_VERSION_SIG_SHBLK_FAIL1 : SG_VERSION_SIG_SHBLK_FAIL2;
        if (sgi->verbose>2)
            fprintf(sgalog, "fail: %s\n", sg_error_str(sgi->sg_version));
        return(0);
    } else if (bht0[1] == runt && vds0.word == sgi->vdif_signature.word) {
        /* lower block is runt */
        if (sgi->verbose>2)
            fprintf(sgalog, "lower block is runt\n");
        sgi->sg_sh_blk_off = (void*)bp0 - start;
        update_fr_cnt_max_fr_ptr32(bht0 + offset, sgi->frame_cnt_max);
        return(0);
    } else if (bht1[1] == runt && vds1.word == sgi->vdif_signature.word) {
        /* upper block is runt */
        if (sgi->verbose>2)
            fprintf(sgalog, "upper block is runt\n");
        sgi->sg_sh_blk_off = (void*)bp1 - start;
        update_fr_cnt_max_fr_ptr32(bht1 + offset, sgi->frame_cnt_max);
        return(blks);
    } else {
        /* somewhere in between */
        if (sgi->verbose>2) fprintf(sgalog,
            "ss normal %d runt %ld blks %u bht0[1] %u bht1[1] %u\n",
            sgi->sg_wr_block, runt, blks, bht0[1], bht1[1]);
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
    if (sgi->verbose>1) fprintf(sgalog,
        "normal_block: blockage %lu blocks %u runt %lu wb %u\n",
            blockage, blks, runt, sgi->sg_wr_block); 

    if (0 == blks) {        /* not likely, but possible */
        sgi->sg_sh_block = runt;
        sgi->sg_sh_blk_off = sgi->sg_fht_size;
        sgi->sg_sh_pkts = runt / sgi->read_size;
        bbsb = 0;
        if (sgi->verbose>1) fprintf(sgalog,
            "normal_block: we only have a runt\n");
    } else if (runt > 0) {  /* we have to find it */
        if (sgi->sg_version == SG_VERSION_SIG_SDBLK_BAIL2) {
            if (sgi->verbose>1) fprintf(sgalog, "DBL sb prior fail\n");
            bbsb = 0;
            if (sgi->verbose>2)
                fprintf(sgalog, "fail: %s\n", sg_error_str(sgi->sg_version));
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
 * Get the number of the final block which should be directly
 * calculable more directly than is done in sg_pkt_by_blk().
 *
 * If the last block is short, go direct to the answer.
 * Otherwise compute the location and read out the answer.
 *
 * Note that the block numbers depend on the total collection of
 * fragments, so we cannot do any decent checking at this point.
 */
static int sg_get_final_bnum(SGInfo *sgi)
{
    void *lb;
    int final_bnum = SG_BLOCKNUM_INIT;
    uint32_t *bp;
    if (sgi->sg_se_blk_off) {
        /* the short end blk offset is to the final short block */
        lb = sgi->smi.start + sgi->sg_se_blk_off;
        final_bnum = ((SGV2BlkNum*)lb)->blocknum;
        if (sgi->verbose>1) {
            bp = (uint32_t*)lb;
            fprintf(sgalog, "final_bnum shend %lu %d %04X %04X\n",
                lb - (void*)sgi->smi.start, final_bnum, bp[0], bp[1]);
        }
    } else {
        /* back up from the end by a block of the correct size */
        lb = sgi->smi.eomem - sgi->sg_wr_block;
        final_bnum = ((SGV2BlkNum*)lb)->blocknum;
        if (sgi->verbose>1) {
            bp = (uint32_t*)lb;
            fprintf(sgalog, "final_bnum else  %lu %d %04X %04X\n",
                lb - (void*)sgi->smi.start, final_bnum, bp[0], bp[1]);
        }
    }
    return(final_bnum);
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
    sgi->sg_final_bnum = sg_get_final_bnum(sgi);
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
 * Try harder to find all the threads if we've found at least 2 threads.
 * We try sequentially for twice as many threads as we can support and
 * also by blocks for the same number.
 */
static void sg_vthreadsxtra(SGInfo *sgi)
{
    VDIFHeader *pkt;
    int vv;
    if (sgi->vthreads[1] < 0) return;   /* only one thread seen*/
    for (vv = 0; vv < 2*MAX_VDIF_THREADS; vv++) {
        pkt = (VDIFHeader*)sg_pkt_by_num(sgi, vv + MAX_VDIF_THREADS, 0, 0);
        (void)sg_get_vsig((uint32_t *)pkt, sgi->smi.start, sgi->verbose-2,
            "vthn:", &sgi->vdif_signature, sgi->vthreads);
    }
    for (vv = 0; vv < 2*MAX_VDIF_THREADS; vv++) {
        pkt = (VDIFHeader*)sg_pkt_by_blk(sgi, vv + MAX_VDIF_THREADS, 0, 0);
        (void)sg_get_vsig((uint32_t *)pkt, sgi->smi.start, sgi->verbose-2,
            "vthb:", &sgi->vdif_signature, sgi->vthreads);
    }
}

/*
 * Support routine for sg_vthreadsdone().
 * Returns the count of nonsequential fails.
 */
static int check_vthread_seq(int nvthreads, VDIFHeader *pkt[],
    char *lab, int verbose)
{
    VDIFHeader *ah, *bh;
    int vv, notseq = 0;
    for (vv = 0; vv < nvthreads - 1; vv++) {
        ah = pkt[vv];
        bh = pkt[vv+1];
        if (ah->w1.invalid || bh->w1.invalid) continue;     /* no comment */
        if (ah->w1.secs_inre == bh->w1.secs_inre &&
            ah->w4.threadID != bh->w4.threadID) continue;   /* sequential */
        if (ah->w1.secs_inre + 1 == bh->w1.secs_inre) {
            if (bh->w2.df_num_insec == 0 &&
                ah->w4.threadID != bh->w4.threadID) continue;   /* ditto */
        }
        if (verbose>1)
            fprintf(sgalog, "vthreadsdone %s %d..%d notset\n", lab, vv, vv+1);
        notseq ++;  /* not sequential in time, no comment */
    }
    return(notseq);
}

/*
 * Some work that vdifuse requires.  nvthreads gets set to a
 * count of the number of threads seen.  vthreadsep gets set
 * to 0 for sequential threads and sg_wr_pkts if the blocks
 * are homogeneous.  We can use the sg_pkt_by_num and
 * sg_pkt_by_blk methods since the block geometry has been
 * determined by the time this function gets called.
 */
static void sg_vthreadsdone(SGInfo *sgi)
{
    int vv, notseq;
    VDIFHeader *pkt[MAX_VDIF_THREADS];
    /* first set sgi->vthreads */
    sgi->nvthreads = 0;     /* should be zero anyway */
    if (sgi->verbose>1) fprintf(sgalog,
        "sg_vthreadsdone found %s\n", sg_vthreads_rep(sgi->vthreads));
    for (vv = 0; vv < MAX_VDIF_THREADS; vv++) {
        if (sgi->vthreads[vv] > 0) {
            if (sgi->verbose>1) fprintf(sgalog,
                "sg_vthreadsdone thread[%d] is %d\n", vv, sgi->vthreads[vv]);
            sgi->nvthreads++;
        }
        pkt[vv] = (VDIFHeader*)sg_pkt_by_num(sgi, vv, 0, 0);
    }
    if (sgi->verbose>1)
        fprintf(sgalog, "sg_vthreadsdone X %d vthreads\n", sgi->nvthreads);
    /* Cf seq_pkt_times() */
    notseq = check_vthread_seq(sgi->nvthreads, pkt, "by-num", sgi->verbose);

    /* allow some hiccups */
    if (notseq < sgi->nvthreads/2) {
        sgi->vthreadsep = 0;
        if (sgi->verbose>1)
            fprintf(sgalog, "vthreadsdone A vthreadsep = 0, %d\n", notseq);
    } else {
        /* verify a few blocks */
        for (vv = 0; vv < MAX_VDIF_THREADS; vv++)
            pkt[vv] = (VDIFHeader*)sg_pkt_by_blk(sgi, vv, 0, 0);
        notseq = check_vthread_seq(sgi->nvthreads, pkt, "by_blk", sgi->verbose);
        if (notseq < sgi->nvthreads/2) sgi->vthreadsep = sgi->sg_wr_pkts;
        else sgi->vthreadsep = 0;
        if (sgi->verbose>1) fprintf(sgalog,
            "vthreadsdone B vthreadsep = %d, %d\n", sgi->vthreadsep, notseq);
    }
    if (sgi->verbose>1) fprintf(sgalog,
        "vthreadsdone finally %d threads %d sep\n",
        sgi->nvthreads, sgi->vthreadsep);
}

/*
 * If we didn't find perfectly valid packets at the extremes,
 * these routines use the (now available) sg_pkt_by_num() method
 * to locate the packets and get their times.  There is an attempt
 * to extrapolate to the extremes of the files for the invalid
 * packets that are probably more right than wrong.
 */
void sg_first_time_check(SGInfo *sgi)
{
    int cnt = 0, nl, first_secs = 0, first_frame = 0;
    VDIFHeader *vx;
    uint32_t *endp;
    if (sgi->sg_version != SG_VERSION_OK_2 &&
        sgi->sg_version != SG_VERSION_FLAT) return;
    if (sgi->verbose>2) fprintf(sgalog, "First time %u@%u+%u\n",
        sgi->ref_epoch, sgi->first_secs, sgi->first_frame);
    if (sgi->ref_epoch != 0 ||
        sgi->first_secs != 0 ||
        sgi->first_frame != 0) return;
    do {
        vx = (VDIFHeader *)sg_pkt_by_num(sgi, cnt, &nl, &endp);
        if (!vx->w1.invalid) {
            sgi->ref_epoch = vx->w2.ref_epoch;
            first_secs = vx->w1.secs_inre;
            first_frame = vx->w2.df_num_insec - cnt;
            update_fr_cnt_max_fr_frame(vx->w2.df_num_insec,sgi->frame_cnt_max);
            if (first_frame < 0) {
                first_secs --;
                first_frame += (sgi->frame_cnt_max + 1);
            }
            sgi->first_secs = first_secs;
            sgi->first_frame = first_frame;
            if (sgi->verbose>2) fprintf(sgalog, "First time %u@%u+%u\n",
                sgi->ref_epoch, sgi->first_secs, sgi->first_frame);
            return;
        }
    } while (++cnt < 1200);
    if (sgi->verbose>1) fprintf(sgalog,
        "Unable to find first valid packet time\n");
}
void sg_final_time_check(SGInfo *sgi)
{
    int cnt = 1, nl, final_secs = 0, final_frame = 0;
    VDIFHeader *vx;
    uint32_t *endp;
    if (sgi->sg_version != SG_VERSION_OK_2 &&
        sgi->sg_version != SG_VERSION_FLAT) return;
    if (sgi->verbose>2) fprintf(sgalog, "Final time %u@%u+%u\n",
        sgi->ref_epoch, sgi->final_secs, sgi->final_frame);
    if (sgi->final_secs != 0 ||
        sgi->final_frame != 0) return;
    do {
        vx = (VDIFHeader *)sg_pkt_by_num(sgi,
            sgi->total_pkts - cnt, &nl, &endp);
        if (!vx->w1.invalid) {
            final_secs = vx->w1.secs_inre;
            final_frame = vx->w2.df_num_insec + cnt;
            update_fr_cnt_max_fr_frame(vx->w2.df_num_insec,sgi->frame_cnt_max);
            if (final_frame > (int)sgi->frame_cnt_max) {
                final_frame -= (sgi->frame_cnt_max + 1);
                final_secs ++;
            }
            sgi->final_secs = final_secs;
            sgi->final_frame = final_frame;
            if (sgi->verbose>2) fprintf(sgalog, "Final time %u@%u+%u\n",
                sgi->ref_epoch, sgi->final_secs, sgi->final_frame);
            return;
        }
    } while (++cnt < 1200);
    if (sgi->verbose>1) fprintf(sgalog,
        "Unable to find final valid packet time\n");
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
 * A few things from the sgi object are retained, but most are rebuilt.
 * The checksum here refers to the checksum of the data just loaded.
 * The caller may want to check that against a reference value, if known.
 */
void sg_info(const char *file, SGInfo *sgi)
{
    int verbcopy = sgi->verbose;
    int fcmxcopy = sgi->frame_cnt_max;
    int ii;
    SGMMInfo smicopy = sgi->smi;

    if (sgi->name) free(sgi->name);
    memset(sgi, 0, sizeof(SGInfo));
    /* set thread ids to all be invalid */
    for (ii = 0; ii < MAX_VDIF_THREADS; ii++) sgi->vthreads[ii] = -1;
    sgi->smi = smicopy;
    sgi->verbose = verbcopy;
    sgi->eval_time = sg_current_or_elapsed_secs((double *)0);
    sgi->sg_version = SG_VERSION_NOT;
    sgi->name = malloc(strlen(file)+4);
    strcpy(sgi->name, file);

    /* if the caller has supplied a plausible frame count max, use it */
    if (fcmxcopy > 1 && fcmxcopy < SG_FR_CNT_MAX)
        sgi->frame_cnt_max = fcmxcopy;

    /* initialize these to a bogus value to avoid confusion */
    sgi->sg_first_bnum = sgi->sg_final_bnum = SG_BLOCKNUM_BOGUS;

    sg_file_header_tag_ok(sgi);
    if (sgi->sg_version == SG_VERSION_OK_2) {
        sg_first_packet(sgi);
        sg_final_packet(sgi);
        sg_short_endfix(sgi);
        sg_normal_block(sgi);
        sg_final_update(sgi);
        sg_vthreadsxtra(sgi);
        sg_vthreadsdone(sgi);
    } else {
        flat_first_packet(sgi);
        if (sgi->sg_version == SG_VERSION_FLAT) {
            flat_final_packet(sgi);
            flat_final_update(sgi);
        }
    }
    sg_first_time_check(sgi);
    sg_final_time_check(sgi);
    sgi->checksum = sg_checksum(sgi);
}

/* The preceding makes an 'open' function simple... */
SGMMInfo *sg_open(const char *file, SGInfo *sgi)
{
    if (mm_init(file, &(sgi->smi), sgi->verbose)) return((SGMMInfo *)0);
    if (sgi) sg_info(file, sgi);
    return(&(sgi->smi));
}

/* ... which may be closed ... */
void sg_close(SGInfo *sgi)
{
    mm_term(&sgi->smi, sgi->verbose);
    sgi->eval_time = sg_current_or_elapsed_secs(&sgi->eval_time);
}

/* ... and also re-opened .... */
SGMMInfo *sg_reopen(SGInfo *sgi)
{
    if (!sgi || !sgi->name) return((SGMMInfo *)0);
    if (mm_init(sgi->name, &(sgi->smi), sgi->verbose)) return((SGMMInfo *)0);
    /* sg_info would be redundant, but eval time is new since we opened it */
    if (sgi) sgi->eval_time = sg_current_or_elapsed_secs((double *)0);
    /* since only a pointer is returned, any checking is up to the caller */
    return(&(sgi->smi));
}

/* ... and also open+close to gather information. */
void sg_access(const char *file, SGInfo *sgi)
{
    SGMMInfo *smi = sg_open(file, sgi);
    if (smi) sg_close(sgi);
}

/* dig out the station 2-letter code */
char *sg_scode(int station)
{
    static char buf[20];
    int lsb = (station & 0xff);
    int msb = (station & 0xff00) >> 8;
    if (isprint(lsb) && isprint(msb)) {
        buf[0] = 'S';
        buf[1] = msb;
        buf[2] = lsb;
        buf[3] = 0;
    } else {
        snprintf(buf, 20, "s%1X%1X", msb, lsb);
    }
    return(buf);
}

/*
 * A diagnostic method to describe the file on stdout
 */
char *sg_repstr(SGInfo *sgi, char *label)
{
    static char buf[1024];
    char *lab = label ? label : "";
    double eval_time = 0.0;
    switch (sgi->sg_version) {
    case SG_VERSION_OK_2:
        eval_time = (sgi->smi.start)
                  ? sg_current_or_elapsed_secs(&sgi->eval_time) /* open */
                  : sgi->eval_time;                             /* closed */
        snprintf(buf, sizeof(buf),
            "%s%s %d@%d+%06d..%d+%06d\n"
            "%sSGv%d(%s) %luB %uB/Pkt %uP/b %ub %uP %.3lfms\n"
            "%swb:%u,%u,%uB %u(%u)+%ux%u+%u(%u)+%ux%u b(P)\n"
            "%ssg:%0lX >%dP/s :%lu:%lu %u|%u|%u:%u| %s %u#\n"
            "%sck:%08X=%08X bs %5d be %5d v:%d th:%d end: %s\n",
            lab, sgi->name, sgi->ref_epoch,
                 sgi->first_secs, sgi->first_frame,
                 sgi->final_secs, sgi->final_frame,
            lab, sgi->sg_version,
                 sgi->smi.start ? "o" : "c", sgi->smi.size,
                 sgi->read_size, sgi->sg_wr_pkts,
                 sgi->sg_total_blks, sgi->total_pkts,
                 1.0e3 * eval_time,
            lab, sgi->sg_wr_block, sgi->sg_sh_block, sgi->sg_se_block,
                 sgi->sg_wr_blks_bs, sgi->sg_wr_pkts_bs,
                 sgi->sg_sh_blk_off?1:0, sgi->sg_sh_pkts,
                 sgi->sg_wr_blks_as, sgi->sg_wr_pkts_as,
                 sgi->sg_se_blk_off?1:0, sgi->sg_se_pkts,
            lab, sgi->vdif_signature.word, sgi->frame_cnt_max,
                 sgi->sg_sh_blk_off, sgi->sg_se_blk_off,
                 sgi->sg_fht_size, sgi->sg_wbht_size,
                 sgi->pkt_offset, sgi->pkt_size,
                 sg_scode(sgi->vdif_signature.bits.stationID),
                 sgi->vdif_signature.bits.num_channels,
            lab, sgi->checksum, sg_checksum(sgi),
                 sgi->sg_first_bnum, sgi->sg_final_bnum, sgi->verbose,
                 sgi->nvthreads, sg_vextime(sgi->ref_epoch, sgi->final_secs +
                 (int)(0.5 + (
                 (double)sgi->final_frame / (double)sgi->frame_cnt_max)))
        );
        break;
    case SG_VERSION_FLAT:
        eval_time = (sgi->smi.start)
                  ? sg_current_or_elapsed_secs(&sgi->eval_time) /* open */
                  : sgi->eval_time;                             /* closed */
        snprintf(buf, sizeof(buf),
            "%s%s %d@%d+%06d..%d+%06d\n"
            "%sFLAT(%s) %luB %uB/Pkt %uP %.3lfms\n"
            "%ssg:%0lX >%dP/s |%u:%u| %s %u#\n"
            "%sck:%08X=%08X v:%d th:%d end: %s\n",
            lab, sgi->name, sgi->ref_epoch,
                 sgi->first_secs, sgi->first_frame,
                 sgi->final_secs, sgi->final_frame,
            lab, sgi->smi.start ? "o" : "c",
                 sgi->smi.size,
                 sgi->read_size, sgi->total_pkts,
                 1.0e3 * eval_time,
            lab, sgi->vdif_signature.word, sgi->frame_cnt_max,
                 sgi->pkt_offset, sgi->pkt_size,
                 sg_scode(sgi->vdif_signature.bits.stationID),
                 sgi->vdif_signature.bits.num_channels,
            lab, sgi->checksum, sg_checksum(sgi),
                 sgi->verbose, sgi->nvthreads,
                 sg_vextime(sgi->ref_epoch, sgi->final_secs +
                 (int)(0.5 + (
                 (double)sgi->final_frame / (double)sgi->frame_cnt_max)))
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
 * Collecting common code for the sg2 file structure.
 */
static inline void load_ptr_pkt_values(SGInfo *sgi,
    off_t ptrdel[4], off_t pktcnt[4])
{
    ptrdel[0] = (off_t)sgi->sg_wr_blks_bs * (off_t)sgi->sg_wr_block;
    pktcnt[0] = sgi->sg_wr_pkts;
    ptrdel[1] = sgi->sg_sh_block;
    pktcnt[1] = sgi->sg_sh_pkts;
    ptrdel[2] = (off_t)sgi->sg_wr_blks_as * (off_t)sgi->sg_wr_block;
    pktcnt[2] = sgi->sg_wr_pkts;
    ptrdel[3] = sgi->sg_se_block;
    pktcnt[3] = sgi->sg_se_pkts;
}

/*
 * Random access methods: get the nn-th packet in the file.
 * On return, *end points to the end of the block, and *nl
 * to the number of packets left in the block.
 */
uint32_t *sg_pkt_by_num(SGInfo *sgi, off_t nn, int *nl, uint32_t **end)
{
    void *pktp, *endp, *pptr;
    off_t nmpkts[4], ptrdel[4], pktcnt[4], bb, rr, ninp = nn;
    int ii, nlft, this = SG_BLOCKNUM_INIT, next = SG_BLOCKNUM_INIT;
    int prev = SG_BLOCKNUM_INIT;

    if (nl) *nl = 0;
    if (!sgi || !sgi->smi.start) return(NULL32P);
    if (nn < 0 || nn >= sgi->total_pkts) return(NULL32P);
    pktp = sgi->smi.start + sgi->sg_fht_size;
    endp = sgi->smi.start;
    nlft = 0;

    nmpkts[0] = sgi->sg_wr_pkts_bs;
    nmpkts[1] = sgi->sg_sh_pkts;
    nmpkts[2] = sgi->sg_wr_pkts_as;
    nmpkts[3] = sgi->sg_se_pkts;
    load_ptr_pkt_values(sgi, ptrdel, pktcnt);

    for (ii = 0; ii < 4; ii++) {
        if (nn < nmpkts[ii]) {
            bb = nn / sgi->sg_wr_pkts;              /* how many blocks */
            pktp += bb * sgi->sg_wr_block;
            this = ((SGV2BlkNum*)pktp)->blocknum;   /* w/packet */
            /* the first block after the short block is special for prev: */
            if (ii == 2 && nn == 0) /* go back a short block */
                pptr = (void*)pktp - sgi->sg_sh_block;
            else /* (most of the time) go back a full block */
                pptr = (void*)pktp - sgi->sg_wr_block;
            prev = (pptr >= sgi->smi.start)
                ? ((SGV2BlkNum*)pptr)->blocknum : SG_BLOCKNUM_NOPREV;
            rr = nn % sgi->sg_wr_pkts;              /* how many packets */
            pktp += rr * sgi->read_size;
            pktp += sgi->sg_wbht_size + sgi->pkt_offset;
            nlft = pktcnt[ii] - rr;
            endp = pktp + nlft * sgi->read_size;
            next = (endp < sgi->smi.eomem)          /* following */
                 ? ((SGV2BlkNum*)endp)->blocknum : SG_BLOCKNUM_NONEXT;
            endp -= sgi->pkt_offset;
            break;
        } else {
            nn   -= nmpkts[ii];
            pktp += ptrdel[ii];
        }
    }
    if (ii == 4) { pktp = NULL32P; this = next = SG_BLOCKNUM_BADPKT; }
    if (nl) *nl = nlft;
    if (end) *end = (uint32_t *)endp;
    if (sgi->verbose>1) fprintf(sgalog,
        "sg_pkt_by_num[%ld](%lu<%lu<%lu)blk{%d:%d:%d}\n",
        ninp, pktp - sgi->smi.start, endp - sgi->smi.start,
        sgi->smi.eomem - sgi->smi.start, prev, this, next);
    if (pktp < sgi->smi.start || pktp >= sgi->smi.eomem) return(NULL32P);
    return((uint32_t *)pktp);
}

/*
 * Random access methods: get the 1st packet in the nn-th block.
 * On return, *end points to the end of the block, and *nl
 * to the number of packets left in the block.
 */

/*
 * A variant of the above that also returns the number of packet
 * bytes before the block in question and the bytes after it.
 * This version did not originally compute endp, but adding the
 * blocknum extraction was easier with endp computed.
 * 
 * (*nl * packet_size) + *pktbytesbefore + *pktbytesafter should
 * be equal to total_packets * packet_size in a sane universe.
 */
static inline char *whence_sg_pkt(
    off_t *pktbytesbefore, off_t *pktbytesafter,
    int *prevp, int *thisp, int *nextp, size_t bsz)
{
    char *whence = "sg_pkt_by_blkall";
    if (!prevp && !thisp && !nextp) {
        if (!pktbytesbefore && !pktbytesafter)
            whence = "sg_pkt_by_blk";
        else
            whence = "sg_pkt_blkby";
    } else {
        if (!pktbytesbefore && !pktbytesafter)
            whence = "sg_pkt_by_blknm";
        else if (bsz)
            whence = "sg_pkt_by_blkall_dbg";
        else
            whence = "sg_pkt_by_blkall";
    }
    return(whence);
}
uint32_t *sg_pkt_by_blkall_dbg(SGInfo *sgi, off_t nn, int *nl,
    uint32_t **end, off_t *pktbytesbefore, off_t *pktbytesafter,
    int *prevp, int *thisp, int *nextp, char *buff, size_t bsz)
{
    void *pktp, *endp = NULL32P, *pptr;
    off_t nmblks[4], ptrdel[4], pktcnt[4], pbb, pba, ninp = nn;
    off_t pbbzero = 0, *pbbp = &pbbzero, pbazero = 0, *pbap = &pbazero;
    int ii, iibrk = -1, nlft, this = SG_BLOCKNUM_INIT, next = SG_BLOCKNUM_INIT;
    int prev = SG_BLOCKNUM_INIT, nlzero = 0, *nlp = &nlzero;
    uint32_t endz = 0, *endx = &endz;
    char *whence = whence_sg_pkt(
        pktbytesbefore, pktbytesafter, prevp, thisp, nextp, bsz);

    /* ensure safe return values */
    if (nl) *nl = 0; else nl = nlp;
    if (pktbytesbefore) *pktbytesbefore = 0; else pktbytesbefore = pbbp;
    if (pktbytesafter)  *pktbytesafter = 0; else pktbytesafter = pbap;
    if (prevp) *prevp = prev; else prevp = &prev;
    if (thisp) *thisp = this; else thisp = &this;
    if (nextp) *nextp = next; else nextp = &next;
    if (end) *end = NULL32P; else end = &endx;

    /* now initial checks and preparations */
    if (!sgi || !sgi->smi.start) return(NULL32P);
    if (nn < 0 || nn >= sgi->sg_total_blks) return(NULL32P);
    pktp = sgi->smi.start + sgi->sg_fht_size;
    nlft = 0;
    pbb = 0;    /* packets bytes before this block */
    pba = 0;    /* packets bytes after this block */

    /* static tables of layout */
    nmblks[0] = sgi->sg_wr_blks_bs;
    nmblks[1] = (sgi->sg_sh_blk_off) ? 1 : 0;
    nmblks[2] = sgi->sg_wr_blks_as;
    nmblks[3] = (sgi->sg_se_blk_off) ? 1 : 0;
    load_ptr_pkt_values(sgi, ptrdel, pktcnt);

    for (ii = 0; ii < 4; ii++) {
        if (nn < nmblks[ii]) {
            /* this block is nn blocks from pktp */
            pktp += nn * sgi->sg_wr_block;
            this = ((SGV2BlkNum*)pktp)->blocknum;   /* w/packet */
            /* the first block after the short block is special for prev: */
            if (ii == 2 && nn == 0) /* go back a short block */
                pptr = (void*)pktp - sgi->sg_sh_block;
            else /* (most of the time) go back a full block */
                pptr = (void*)pktp - sgi->sg_wr_block;
            prev = (pptr >= sgi->smi.start)
                ? ((SGV2BlkNum*)pptr)->blocknum : SG_BLOCKNUM_NOPREV;
            pktp += sgi->sg_wbht_size + sgi->pkt_offset;
            nlft = pktcnt[ii];
            /* finally endp requires and end of memory check */
            endp = pktp + nlft * sgi->read_size;
            next = (endp < sgi->smi.eomem)          /* following */
                 ? ((SGV2BlkNum*)endp)->blocknum : SG_BLOCKNUM_NONEXT;
            pbb  += nn * pktcnt[ii];         /* packets of this block type */
            pba  = (nmblks[ii] - nn - 1) * pktcnt[ii];
            iibrk = ii;
            break;
        } else {
            nn   -= nmblks[ii];
            pktp += ptrdel[ii];
            pbb  += nmblks[ii] * pktcnt[ii]; /* packets of this block type */
        }
    }
    if (ii == 4) { pktp = NULL32P; prev = this = SG_BLOCKNUM_BADPKT; }
    for (ii++ ; ii < 4; ii++) {
        pba += nmblks[ii] * pktcnt[ii];      /* packets of this block type */
    }

    /* convert packets to packet bytes */
    pbb *= sgi->pkt_size;
    pba *= sgi->pkt_size;
    /* update output values */
    *nl = nlft;
    *pktbytesbefore = pbb;
    *pktbytesafter  = pba;
    *prevp = prev;
    *thisp = this;
    *nextp = next;
    *end = endp;
    /* provide some information */
    if (buff && bsz) snprintf(buff, bsz,
        "%s[%ld](%ld<%ld#%ld<%ld)%d-pkts-blk{%d:%d:%d}/%d",
        whence, ninp, *pktbytesbefore, pktp - sgi->smi.start,
        *pktbytesafter, sgi->smi.eomem - pktp,
        *nl, prev, this, next, iibrk);
    else if (sgi->verbose>1) fprintf(sgalog,
        "%s[%ld](%ld<%ld#%ld<%ld)%d-pkts-blk{%d:%d:%d}/%d\n",
        whence, ninp, *pktbytesbefore, pktp - sgi->smi.start,
        *pktbytesafter, sgi->smi.eomem - pktp,
        *nl, prev, this, next, iibrk);
    return((uint32_t *)pktp);
}

/*
 * The variant that skips the debugging
 */
uint32_t *sg_pkt_by_blkall(SGInfo *sgi, off_t nn, int *nl,
    uint32_t **end, off_t *pktbytesbefore, off_t *pktbytesafter,
    int *prevp, int *thisp, int *nextp)
{
    uint32_t *pkt = sg_pkt_by_blkall_dbg(sgi, nn, nl, end,
        pktbytesbefore, pktbytesafter, prevp, thisp, nextp, NULL, 0);
    return(pkt);
}
/*
 * This is the original variant
 */
uint32_t *sg_pkt_by_blknm(SGInfo *sgi, off_t nn, int *nl,
    uint32_t **end, int *prevp, int *thisp, int *nextp)
{
    uint32_t *pkt = sg_pkt_by_blkall(sgi, nn, nl, end, NULL, NULL,
        prevp, thisp, nextp);
    return(pkt);
}
/*
 * The simplest version of the above, using more complicated routine.
 */
uint32_t *sg_pkt_by_blk(SGInfo *sgi, off_t nn, int *nl, uint32_t **end)
{
    uint32_t *pkt = sg_pkt_by_blkall(sgi, nn, nl, end, NULL, NULL,
        NULL, NULL, NULL);
    return(pkt);
}
/*
 * The variant that only provides the bytes before and after
 */
uint32_t *sg_pkt_blkby(SGInfo *sgi, off_t nn, int *nl,
    off_t *pktbytesbefore, off_t *pktbytesafter)
{
    uint32_t *end;
    uint32_t *pkt = sg_pkt_by_blkall(sgi, nn, nl, &end,
        pktbytesbefore, pktbytesafter, NULL, NULL, NULL);
    return(pkt);
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
    int ii, nlft, this, next;

    if (nl) *nl = 0;
    if (!sgi || !sgi->smi.start) return(NULL32P);
    if (nn < 0 || nn >= sgi->smi.size) return(NULL32P);
    pktp = sgi->smi.start + sgi->sg_fht_size;
    endp = sgi->smi.start;
    nlft = 0;

    if (nn < sgi->sg_fht_size) nn = 0;
    else nn -= sgi->sg_fht_size;
    load_ptr_pkt_values(sgi, ptrdel, pktcnt);

    for (ii = 0; ii < 4; ii++) {
        if (nn < ptrdel[ii]) {
            this = ((SGV2BlkNum*)pktp)->blocknum;   /* w/packet */
            bb = nn / sgi->sg_wr_block;
            rr = nn % sgi->sg_wr_block;
            if (rr < sgi->sg_wbht_size) rr = sgi->sg_wbht_size;
            else rr -= sgi->sg_wbht_size;
            pp = rr / sgi->read_size;
            pktp += bb * sgi->sg_wr_block + pp * sgi->read_size;
            pktp += sgi->sg_wbht_size + sgi->pkt_offset;
            nlft = pktcnt[ii] - pp;
            endp = pktp + nlft * sgi->read_size;
            next = (endp < sgi->smi.eomem)          /* following */
                 ? ((SGV2BlkNum*)endp)->blocknum : SG_BLOCKNUM_NONEXT;
            endp -= sgi->pkt_offset;
            break;
        } else {
            nn   -= ptrdel[ii];
            pktp += ptrdel[ii];
        }
    }
    if (ii == 4) { pktp = NULL32P; this = next = SG_BLOCKNUM_BADPKT; }
    if (nl) *nl = nlft;
    if (end) *end = (uint32_t *)endp;
    if (sgi->verbose>1) fprintf(sgalog,
        "sg_pkt_by_off[%ld](%lu<%lu<%lu)blk{%d:%d}\n",
        ninp, pktp - sgi->smi.start, endp - sgi->smi.start,
        sgi->smi.eomem - sgi->smi.start, this, next);
    if (pktp < sgi->smi.start || pktp >= sgi->smi.eomem) return(NULL32P);
    return((uint32_t *)pktp);
}

/*
 * Check the signatures on some number of packets starting with the
 * pkt pointer provided above.  It should return 0 if the number left
 * and end pointer are also as provided (or less restrictive).  If
 * nv is a valid pointer, it is incremented by the number of fill
 * (invalid) packets seen.
 */
int seq_pkt_check(SGInfo *sgi, uint32_t *pkt, int nl, uint32_t *end, int *nv)
{
    int nbad = 0, fill = 0;
    VDIFsigu vds;
    if (nv) *nv = fill;
    if (sgi->verbose>2) fprintf(sgalog, "seq_pkt_check(%lu<%lu)[%d]\n",
        (void*)pkt - sgi->smi.start, (void*)end - sgi->smi.start, nl);
    if (nl < 0) return(-nl);
    if (!sgi || (sgi->smi.start == 0)) return(nl);
    while (nl-- > 0) {
        vds.word = sg_get_vsig(pkt, sgi->smi.start, sgi->verbose-3,
            "pchk:", &sgi->vdif_signature, sgi->vthreads);
        if ((pkt >= end) || vds.word != (sgi->vdif_signature.word)) nbad ++;
        update_fr_cnt_max_fr_ptr32(pkt, sgi->frame_cnt_max);
        pkt += sgi->read_size / sizeof(uint32_t);
        if (((VDIFHeader *)pkt)->w1.invalid) fill++;
        if (sgi->verbose>4) fprintf(sgalog,
            "pchk[%d] nbad %d fill %d end %s (%p>=%p by %ld)\n"
            "pchk[%d] %16lX\npchk[%d] %16lX\n",
            nl, nbad, fill, ((pkt >= end)?"ERR":"OK "), pkt, end, pkt - end,
            nl, vds.word, nl, sgi->vdif_signature.word );
    }
    if (nv) *nv = fill;
    return(nbad);
}

static void pr_skip(int ty, VDIFHeader *ah, VDIFHeader *bh, void *start)
{
    static char ta[20], tb[20];
    static char *desc[] = {
        "timing: same second in next frame",
        "timing: next second, frame -> 0",
        "timing: multi-second packet gap",
        "timing: new thread at same frame",
    };
    if (ty < 3) {
        tb[0] = ta[0] = 0;
    } else {
        snprintf(ta, 20, " th-%05hd", ah->w4.threadID);
        snprintf(tb, 20, " th-%05hd", bh->w4.threadID);
    }
    fprintf(sgalog, "%s, %u+%06u at %lu%s\n%s, %u+%06u at %lu%s\n",
        desc[ty], ah->w1.secs_inre, ah->w2.df_num_insec, (void*)ah - start,ta,
        desc[ty], bh->w1.secs_inre, bh->w2.df_num_insec, (void*)bh - start,tb);
}

/*
 * Check the timestamps on the packets.  It should return 0 if the
 * (nl) packets are sequential, otherwise nonzero if there are jumps.
 *
 * In the event of comingled threads, it is hard to say what to expect.
 * However, we can consider sequential the case where the threads are
 * collected by time and consider that to be a non-jump case.
 *
 * In code below, ah(eader) is immediately followed by bh(eader).
 */
int seq_pkt_times(SGInfo *sgi, uint32_t *pkt, int nl, uint32_t *end)
{
    VDIFHeader *ah = (VDIFHeader *)pkt, *bh;
    int ds, skips = 0;
    if (sgi->verbose>2) fprintf(sgalog, "seq_pkt_times(%lu<%lu)[%d]\n",
        (void*)pkt - sgi->smi.start, (void*)end - sgi->smi.start, nl);
    while (--nl > 0) {
        pkt += sgi->read_size / sizeof(uint32_t);
        bh = (VDIFHeader *)pkt;
        if (ah->w1.invalid || bh->w1.invalid) {
            /* nothing to test -- one or both packets are invalid */
            ds = 0;
        } else if (ah->w1.secs_inre == bh->w1.secs_inre) {
            /* same second in next frame; frame should inc by 1 */
            ds = (ah->w2.df_num_insec + 1 != bh->w2.df_num_insec) ? 1 : 0;
            /* are we dealing with multiple threads */
            if (ah->w4.threadID != bh->w4.threadID) {
                if (sgi->verbose>3) pr_skip(3, ah, bh, sgi->smi.start);
                ds = 0;     /* different threads are allowed same second */
            }
            if (ds && sgi->verbose>2) pr_skip(0, ah, bh, sgi->smi.start);
        } else if (ah->w1.secs_inre + 1 ==  bh->w1.secs_inre) {
            /* next second, frame -> 0; frame counter to 0 expected */
            ds = (bh->w2.df_num_insec != 0) ? 1 : 0;
            if (ds && sgi->verbose>2) pr_skip(1, ah, bh, sgi->smi.start);
        } else {
            /* multi-second packet gap */
            ds = 1;
            if (ds && sgi->verbose>2) pr_skip(2, ah, bh, sgi->smi.start);
        }
        ah = bh;
        skips += ds;
    }
    return(skips);
}

/*
 * eof
 */
