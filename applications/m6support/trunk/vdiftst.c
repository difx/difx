/*
 * $Id: vdiftst.c 4248 2017-02-28 22:37:57Z gbc $
 *
 * This file provides support for the fuse interface.
 * This file contains methods to label a file as valid VDIF.
 * They return 1 if the file is accepted at the level of rigor.
 *
 * TODO: this version requires first/other offsets to be same for all frags
 * TODO: make better use of sg_* methods
 * TODO: cope gracefully with initially unknown rate
 */

#include <stdio.h>
#include <string.h>
#include <time.h>

#include "vdif.h"
#include "vdif_epochs.h"
#include "vdifuse.h"

/* VDIFUSE_SEARCH_MAX is at least long enough for a full VDIF packet */
#define BUF_SIZ (2*VDIFUSE_SEARCH_MAX + 1)

/* Capture the invariant part of a vdif stream */
static uint64_t vdif_signature;
/* try to work out the packet rate from frame counters */
static uint32_t maxfrcounter_seen;

/*
 * Tests return the test type if they pass, otherwise 0 for a fail.
 */

/*
 * This accepts anything.
 */
static int vdif_rigor_by_nocheck(char *path, VDIFUSEpars *pars)
{
    (void)path;
    (void)pars;
    return(VDIFUSE_RIGOR_NOCHECK);
}

/*
 * This is relatively trivial...just looks at the suffix.
 */
static int vdif_rigor_by_suffix(char *path, VDIFUSEpars *pars)
{
    int len;
    if ((pars->how_rigorous & VDIFUSE_RIGOR_SUFFIX) == 0)
        return(VDIFUSE_RIGOR_NOCHECK);
    len = strlen(path);
    if (strncmp(path + len - 5, ".vdif", 5)) return(0);
    return(VDIFUSE_RIGOR_SUFFIX);
}

/*
 * This requires <exp>_<sc>_<scan-name>[...don't care...].vdif
 */
static int vdif_rigor_by_ename(char *path, VDIFUSEpars *pars)
{
    int len;
    char *us;
    if ((pars->how_rigorous & VDIFUSE_RIGOR_ENAME) == 0)
        return(VDIFUSE_RIGOR_NOCHECK);
    len = strlen(path);
    if (strncmp(path + len - 5, ".vdif", 5)) return(0);
    us = strchr(path, '_');
    if (!us) return(0);
    us = strchr(us, '_');
    if (!us) return(0);
    return(VDIFUSE_RIGOR_ENAME);
}

/*
 * This looks into the file for the (SGv2) magic number
 */
static int vdif_rigor_by_magic(char *path, VDIFUSEpars *pars)
{
    uint32_t words[6];
    FILE *fp;
    if ((pars->how_rigorous & VDIFUSE_RIGOR_MAGIC) == 0)
        return(VDIFUSE_RIGOR_NOCHECK);
    fp = fopen(path, "r");
    if (!fp) return(0);
    if ((5 == fread(words, 4, 5, fp)) && (!fclose(fp)) &&
        (words[0] == 0xfeed6666) && (words[1] == 0x2))
            return(VDIFUSE_RIGOR_MAGIC);
    return(0);
}

/*
 * Insist on a minimum file size.
 */
static int vdif_rigor_by_minsize(char *path, VDIFUSEpars *pars)
{
    struct stat sb;
    if ((pars->how_rigorous & VDIFUSE_RIGOR_MINSIZE) == 0)
        return(VDIFUSE_RIGOR_NOCHECK);
    if (stat(path, &sb)) return(perror("stat"), 0);
    if (sb.st_size > VDIFUSE_MIN_FILE_SIZE)
        return(VDIFUSE_RIGOR_MINSIZE);
    return(0);
}

/*
 * Check the file against any regex patterns supplied
 * The real work is in vdifrex.c
 */
static int vdif_rigor_by_regex(char *path, VDIFUSEpars *pars)
{
    if ((pars->how_rigorous & VDIFUSE_RIGOR_REGEX) == 0)
        return(VDIFUSE_RIGOR_NOCHECK);
    return(regexcheck(path) ? 0 : VDIFUSE_RIGOR_REGEX);
}

/*
 * Other tests could be added here, culminating with the test director.
 */
int vdif_rigor_frag(char *path, VDIFUSEpars *pars)
{
    int rigor = 0;
    rigor |= vdif_rigor_by_nocheck(path, pars);
    rigor |= vdif_rigor_by_suffix(path, pars);
    rigor |= vdif_rigor_by_ename(path, pars);
    rigor |= vdif_rigor_by_magic(path, pars);
    rigor |= vdif_rigor_by_minsize(path, pars);
    rigor |= vdif_rigor_by_regex(path, pars);
    if (vdifuse_debug>3) fprintf(vdflog,
        "    passed tests " VDIFUSE_RIGOR_PRINTF "\n", rigor);
    return(rigor);
}

static void notice_maxfrcounter(VDIFHeader *vh)
{
    if (vh->w2.df_num_insec > maxfrcounter_seen)
        maxfrcounter_seen = vh->w2.df_num_insec;
}

/*
 * Compare two headers; between is the number of intervening packets.
 * (We assume a difference of much less than one second here.)
 *
 * TODO:
 *  stationID could be masked with params.station_mask
 *  threadID is a completely different problem
 */
static int headers_differ(VDIFHeader *v0, VDIFHeader *v1, int between)
{
    int delta, secs;
    if (v0->w1.legacy       != v1->w1.legacy      ) return(1);
    if (v0->w2.UA           != v1->w2.UA          ) return(2);
    if (v0->w2.ref_epoch    != v1->w2.ref_epoch   ) return(3);
    if (v0->w3.ver          != v1->w3.ver         ) return(4);
    if (v0->w3.df_len       != v1->w3.df_len      ) return(5);
    if (v0->w3.num_channels != v1->w3.num_channels) return(6);
//  if (v0->w4.stationID    != v1->w4.stationID   ) return(7);
//  if (v0->w4.threadID     != v1->w4.threadID    ) return(8);
    if (v0->w4.bps          != v1->w4.bps         ) return(9);
    if (between == 0) {
        secs = v1->w1.secs_inre - v0->w1.secs_inre;
        if (secs == 0) {
            delta = v1->w2.df_num_insec - v0->w2.df_num_insec - 1;
            if (delta != 0)                         return(10);
        } else if (secs == 1) {
            delta = v0->w2.df_num_insec - v1->w2.df_num_insec;
            if (delta <= 0)                         return(11);
        } else {
                                                    return(12);
        }
    } else {
        /* TODO: -- code this case if needed */
                                                    return(0 /* 13 */);
    }
    return(0);
}

/*
 * Based on the first and final headers, size and number, we find the
 * edge between two seconds and grab the frame counter.  That++ && return.
 *
 * pkt_rate on entry is the current best default if we have to bail.
 */
static size_t find_the_damn_rate_vdif(FILE *fp, size_t read_len,
    VDIFHeader *vo, VDIFHeader *vf, size_t ofo, size_t off, size_t pkt_rate)
{
    static uint64_t u_mid[BUF_SIZ];
    char *mid = (char *)u_mid;
    size_t offset, nb, rv;
    VDIFHeader *vm, *vn;

    /* less than 1 sec of data, no math here */
    if (vo->w1.secs_inre == vf->w1.secs_inre) return(pkt_rate);
    if (vdifuse_debug>3) fprintf(vdflog,
        "      Finding the damn rate: "
        "Pkt rate %u+%06u | %u+%06u [x]\n",
        vo->w1.secs_inre, vo->w2.df_num_insec,
        vf->w1.secs_inre, vf->w2.df_num_insec);

    /* back up to what should be the start of the frame - 1 */
    offset = off - read_len * (vf->w2.df_num_insec + 1);
    if (offset < ofo) return(fprintf(stderr,
        "%lu < %lu\n", offset, ofo), pkt_rate);
    if (fseeko(fp, offset, SEEK_SET)) return(perror("fseeko"), pkt_rate);
    if ((nb = fread(mid, read_len, 2, fp)) < 2)
        return(perror("fread"), pkt_rate);
    vm = (VDIFHeader *)mid;
    vn = (VDIFHeader *)(mid + read_len);
    rv = headers_differ(vm, vn, 0);
    if (vdifuse_debug>3) fprintf(vdflog,
        "      Finding the damn rate: "
        "Pkt rate %u+%06u | %u+%06u [%lu]\n",
        vm->w1.secs_inre, vm->w2.df_num_insec,
        vn->w1.secs_inre, vn->w2.df_num_insec, rv);
    if (rv) return(fprintf(vdflog, "Pkts differ\n"), pkt_rate);

    notice_maxfrcounter(vm);
    notice_maxfrcounter(vn);
    if ((vn->w1.secs_inre == (vm->w1.secs_inre + 1)) &&
        (vn->w2.df_num_insec == 0)) {
            pkt_rate = vm->w2.df_num_insec + 1;
            if (vdifuse_debug>3) fprintf(vdflog,
            "      Finding the damn rate: got %lu pps\n", pkt_rate);
            return(pkt_rate);
    } else {
        if (vdifuse_debug>3) fprintf(vdflog,
            "      Finding the damn rate: no luck on edge\n");
    }
    return(pkt_rate);
}

/*
 * We can poke at it, but with the existing blocking size,
 * there is no certainly we'll find a block with an edge in it...
 */
static size_t find_the_damn_rate_sgv2(FILE *fp, size_t read_len,
    VDIFHeader *vo, VDIFHeader *vf, size_t ofo, size_t off, size_t pkt_rate)
{
    if (vdifuse_debug>3) fprintf(vdflog,
        "SGV2 rate finding not implemented\n");
    return(pkt_rate);
}

/*
 * This routine is trusting that 0 <= nsec <= 999999999
 */
static void compute_duration(const struct timespec first, 
    const struct timespec final, struct timespec *dur)
{
    dur->tv_sec = final.tv_sec - first.tv_sec;
    dur->tv_nsec = final.tv_nsec - first.tv_nsec;
    if (dur->tv_nsec < 0) {
        dur->tv_nsec += VDIFUSE_ONE_SEC_NS;
        dur->tv_sec -= 1;
    }
}

/*
 * Compute the time extent of the fragment, updating the vfuse structure:
 * + dev_t     st_dev;     // prefix_bytes (to first packet)
 * + dev_t     st_rdev;    // offset_bytes (VDIF hdr in packet)
 * + ino_t     st_ino;     // pkts_per_sec
 * + blksize_t st_blksize; // packet size
 * + blkcnt_t  st_blocks;  // number of packets
 * + time_t    st_mtime;   // data start time
 * + time_t    st_ctime;   // data end time (inc. last packet)
 * + time_t    st_atime;   // total scan duration
 *
 * Note: pkt_rate might just be an estimate at this point.
 */
void update_vfuse(struct stat *vfuse, VDIFHeader *v0, VDIFHeader *vf,
    size_t prefix, size_t offset, size_t read_len,
    size_t pkt_rate, size_t num_pkts)
{
    static struct timespec first, final, dur;

    vdif_signature = sg_signature((uint32_t *)v0);

    vfuse->st_dev = prefix;
    vfuse->st_rdev = offset;
    vfuse->st_ino = pkt_rate;
    vfuse->st_blksize = read_len;
    vfuse->st_blocks = num_pkts;

    first.tv_sec = v0->w1.secs_inre;
    first.tv_sec += vdif_epochs[v0->w2.ref_epoch];
    first.tv_nsec = (v0->w2.df_num_insec * VDIFUSE_ONE_SEC_NS / pkt_rate);
    vfuse->st_mtime = first.tv_sec;
    vfuse->st_mtim.tv_nsec = first.tv_nsec;

    final.tv_sec = vf->w1.secs_inre;
    final.tv_sec += vdif_epochs[vf->w2.ref_epoch];
    final.tv_nsec = ((vf->w2.df_num_insec+1) * VDIFUSE_ONE_SEC_NS / pkt_rate);
    if (final.tv_nsec >= VDIFUSE_ONE_SEC_NS) {
        final.tv_sec += 1;
        final.tv_nsec = 0;
    }
    vfuse->st_ctime = final.tv_sec;
    vfuse->st_ctim.tv_nsec = final.tv_nsec;

    compute_duration(first, final, &dur);
    vfuse->st_atime = dur.tv_sec;
    vfuse->st_atim.tv_nsec = dur.tv_nsec;
}

/*
 * Analyze a fragment composed of VDIF packets with no special arrangment,
 * other than possibly a prefix at the start of the file.
 */
static int analyze_fragment_vdif(const char *path, struct stat *vfuse,
    VDIFUSEpars *pars)
{
    static uint64_t u_buf[BUF_SIZ];
    static uint64_t u_end[BUF_SIZ];
    char *buf = (char*)u_buf, *end = (char *)u_end;
    size_t nb, rv, pkt_len, read_len, head_off, tail_off, num_pkts;
    size_t pkt_rate;
    size_t prefix = pars->prefix_bytes;
    size_t offset = pars->offset_bytes;
    VDIFHeader *v0, *v1, *vp, *vf;
    FILE *fp = fopen(path, "r");

    if (!fp) return(fprintf(vdflog,"%s: ",path),perror("fopen"),1);

    /* get the first two headers, and the length */
    head_off = prefix + offset;
    if (fseeko(fp, head_off, SEEK_SET))
        return(perror("fseeko"), 2);
    /* don't know the packet length, so we have a weak test here */
    if ((nb = fread(buf, 1, BUF_SIZ-1, fp)) < BUF_SIZ-1)
        return(perror("fread"), 3 + fclose(fp));
    v0 = (VDIFHeader *)buf;
    pkt_len = 8 * v0->w3.df_len;
    if ((pkt_len < 32) || (pkt_len > (BUF_SIZ - 32)))
        return(fprintf(vdflog,
            "Error: unusual frame length of %lu(>%d) bytes in %s\n",
            pkt_len, BUF_SIZ-32, path), 4);

    read_len = pkt_len + offset;
    v1 = (VDIFHeader *)(buf + read_len);
    if ((rv = headers_differ(v0, v1, 0))) return(fprintf(stderr,
        "%s: VDIF header mismatch\n",path), 100 + rv + fclose(fp));
    notice_maxfrcounter(v0);
    notice_maxfrcounter(v1);

    /* estimate the number of packets */
    num_pkts = (vfuse->st_size - prefix) / read_len;

    /* get the final two headers, dropping any partial packet at the end */
    tail_off = head_off + (num_pkts - 2) * read_len;
    if (fseeko(fp, tail_off, SEEK_SET))
        return(perror("fseeko"), 12 + fclose(fp));
    /* we know the packet length, so can insist on both headers here */
    if ((nb = fread(end, read_len, 2, fp)) < 2)
        return(perror("fread"), 13 + fclose(fp));
    vp = (VDIFHeader *)end;
    vf = (VDIFHeader *)(end + read_len);
    if ((rv = headers_differ(v0, vp, 1))) return(fprintf(stderr,
        "%s: VDIF header mismatch\n",path), 200 + rv + fclose(fp));
    if ((rv = headers_differ(vp, vf, 0))) return(fprintf(stderr,
        "%s: VDIF header mismatch\n",path), 300 + rv + fclose(fp));
    notice_maxfrcounter(vp);
    notice_maxfrcounter(vf);

    /* see if we can improve on the packet rate estimate */
    pkt_rate = find_the_damn_rate_vdif(fp, read_len, v0, vf,
        head_off, tail_off + read_len, pars->est_pkt_rate);
    if (pars->pkts_per_sec > 0 && pkt_rate > pars->pkts_per_sec)
        return(400 + fprintf(vdflog, "Corrupt pkt rate %lu\n", pkt_rate));

    if (fclose(fp)) return(perror("fread"),500);
    update_vfuse(vfuse, v0, vf, prefix, offset, read_len, pkt_rate, num_pkts);
    return(0);
}

/*
 * Analyze an SG vers 2 fragment.  There will be a 5-word file prefix,
 * and then some number of blocks of VDIF packets preceded by a 2-word
 * header (blocknum and size).  The packets might still have an offset.
 */
static int analyze_fragment_sgv2(const char *path, struct stat *vfuse,
    VDIFUSEpars *pars)
{
    static uint64_t u_buf[BUF_SIZ];
    static uint64_t u_end[BUF_SIZ];
    static uint32_t words[10];
    char *buf = (char*)u_buf, *end = (char *)u_end;
    size_t nb, rv, pkt_len, read_len, head_off, tail_off, num_pkts;
    size_t header_pkt_len, block_size, nblks, runt;
    size_t offset = pars->offset_bytes;
    size_t pkt_rate;
    FILE *fp = fopen(path, "r");
    VDIFHeader *v0, *v1, *vp, *vf;

    if (!fp) return(fprintf(vdflog,"%s: ",path),perror("fopen"),1);

    /* get the file and block headers and some checks */
    if (7 != fread(words, 4, 7, fp)) return(2 + fclose(fp));
    if (words[0] != 0xfeed6666) return(3 + fclose(fp));
    if (words[1] != 0x2) return(4 + fclose(fp));
    block_size = words[2];
    if (block_size > pars->writeblocker) return(fprintf(vdflog,
        "Error: blocksize %lu>%u\n", block_size, pars->writeblocker),
        5 + fclose(fp));
    if (block_size > pars->writeblocker) return(5 + fclose(fp));
    if (words[3] != 0x0) return(6 + fclose(fp));     /* VDIF */
    header_pkt_len = words[4];
    if ((header_pkt_len < 32) || (header_pkt_len > (BUF_SIZ - 32)))
        return(fprintf(vdflog, "Error: unusual frame length of %lu bytes"
            "(<32|>%d) in %s\n", header_pkt_len, BUF_SIZ-32, path), 7);

    /* get the first two headers, and the length */
    if ((nb = fread(buf, 1, BUF_SIZ-1, fp)) < BUF_SIZ-1)
        return(perror("fread"),8 + fclose(fp));
    v0 = (VDIFHeader *)buf;
    pkt_len = 8 * v0->w3.df_len;
    read_len = pkt_len + offset;
    v1 = (VDIFHeader *)(buf + read_len);
    if (read_len != header_pkt_len) return(9 + fclose(fp));
    if ((rv = headers_differ(v0, v1, 0))) return(100 + rv + fclose(fp));
    notice_maxfrcounter(v0);
    notice_maxfrcounter(v1);

    /* estimate the number of packets */
    nblks = (vfuse->st_size - 5) / block_size;
    runt = (vfuse->st_size - 5) - nblks * block_size;
    num_pkts = block_size / read_len;   /* pkts/block */
    num_pkts *= nblks;                  /* total pkts */
    if (runt) num_pkts += (runt - 8) / read_len;

    /* get the final two headers, expecting two complete packets */
    tail_off = vfuse->st_size - 2 * read_len;
    if (fseeko(fp, tail_off, SEEK_SET))
        return(perror("fseeko"), 12 + fclose(fp));
    /* we know the packet length, so can insist on both headers here */
    if ((nb = fread(end, read_len, 2, fp)) < 2) return(perror("fread"), 
        13 + fclose(fp));
    vp = (VDIFHeader *)end;
    vf = (VDIFHeader *)(end + read_len);
    if ((rv = headers_differ(v0, vp, 1))) return(200 + rv + fclose(fp));
    if ((rv = headers_differ(vp, vf, 0))) return(300 + rv + fclose(fp));
    notice_maxfrcounter(vp);
    notice_maxfrcounter(vf);

    /* see if we can improve on the packet rate estimate */
    pkt_rate = find_the_damn_rate_sgv2(fp, read_len, v0, vf,
        head_off, tail_off + read_len, pars->pkts_per_sec);
        /* TODO: pars->est_pkt_rate    ^^^^^^^^^^^^^^^^^^  later */
    if (pars->pkts_per_sec > 0 && pkt_rate > pars->pkts_per_sec)
        return(400 + fprintf(vdflog, "Corrupt pkt rate %lu\n", pkt_rate));

    if (fclose(fp)) return(perror("fread"),500);
    update_vfuse(vfuse, v0, vf, 0, offset, read_len, pkt_rate, num_pkts);
    return(0);
}

/*
 * Support routines for analyze_fragment_sgv2_harder() follows in an
 * optimistic effort to make this more readable.  All return nonzero
 * if there are insurmountable issues.
 */
static int basic_checks_sgv2_harder(FILE *fp, VDIFUSEpars *pars,
    size_t *header_pkt_len, size_t *block_size)
{
    static uint32_t words[10];
    /* same tests as previously, so use simple error code returns */
    if (7 != fread(words, 4, 7, fp)) return(2);
    if (words[0] != 0xfeed6666) return(3);
    if (words[1] != 0x2) return(4);
    *block_size = words[2];
    if (*block_size > pars->writeblocker) return(5);
    if (words[3] != 0x0) return(6);     /* marked as VDIF */
    *header_pkt_len = words[4];
    if ((*header_pkt_len < 32) || (*header_pkt_len > (BUF_SIZ-32))) return(7);
    return(0);
}

/*
 * Extrapolate over invalid packets to generate a pseudo first
 * or final packet.  The first routine sets the pkt and read lengths.
 * We've already read the file header and the first block, so we
 * are positioned at the first packet.  8k packets are about 1200
 * per Mark6 block, and fill is typically < 1000, so if we haven't
 * found a good packet by a cnt of 1200, we might as well bail.
 */
static VDIFHeader *first_sgv2_harder(FILE *fp, size_t offset,
    size_t pkt_rate, size_t read_len, size_t *pkt_len)
{
    static VDIFHeader v0;
    static char pkt[BUF_SIZ];
    int nb, cnt = 0;
    size_t chklen;
    do {
        v0.w1.invalid = 1;
        if ((nb = fread(pkt, read_len, 1, fp)) < 1) break;
        v0 = *(VDIFHeader *)(pkt + offset);
        if (!v0.w1.invalid) {
            *pkt_len = 8 * v0.w3.df_len;
            chklen = *pkt_len + offset;
            if (chklen == read_len) break;  /* we have a valid packet */
        }
        cnt ++;
    } while (cnt < 1200);
    if (nb < 1) { perror("first_sgv2_harder:read"); return(0); }
    if (v0.w1.invalid) return(0);           /* no valid packets */
    notice_maxfrcounter(&v0);
    if (cnt > 0) {                          /* extrapolated first */
        nb = v0.w2.df_num_insec - cnt;
        if (nb < 0) {
            v0.w1.secs_inre --;
            v0.w2.df_num_insec = nb + pkt_rate;
        }
    }
    return(&v0);
}
static VDIFHeader *final_sgv2_harder(FILE *fp, size_t offset,
    size_t pkt_rate, off_t end, size_t read_len, size_t pkt_len)
{
    static VDIFHeader vf;
    static char pkt[BUF_SIZ];
    size_t tail_off, chklen;
    int nb, cnt = 1;
    do {
        tail_off = end - cnt * read_len;
        vf.w1.invalid = 1;
        if (fseeko(fp, tail_off, SEEK_SET)) {
            perror("final_sgv2_harder:fseek");
            break;
        }
        if ((nb = fread(pkt, read_len, 1, fp)) < 1) break;
        vf = *(VDIFHeader *)(pkt + offset);
        if (!vf.w1.invalid) {
            chklen = 8 * vf.w3.df_len + offset;
            if (chklen == read_len) break;  /* we have a valid packet */
        }
        cnt ++;
    } while (cnt < 1200);
    if (nb < 1) { perror("final_sgv2_harder:read"); return(0); }
    if (vf.w1.invalid) return(0);           /* no valid packets */
    notice_maxfrcounter(&vf);
    if (cnt > 0) {                          /* extrapolated final */
        nb = vf.w2.df_num_insec + cnt;
        if (nb >= pkt_rate) {
            vf.w1.secs_inre ++;
            vf.w2.df_num_insec -= pkt_rate;
        }
    }
    return(&vf);
}

/*
 * Try harder, which means we're willing to read more bytes and to
 * step around invalid packets.
 */
static int analyze_fragment_sgv2_harder(const char *path, struct stat *vfuse,
    VDIFUSEpars *pars, int failcode)
{
    int err;
    size_t block_size = 0, nblks, runt;
    size_t offset = pars->offset_bytes;
    size_t pkt_rate, pkt_len, read_len = 0, num_pkts;
    FILE *fp = fopen(path, "r");
    VDIFHeader *v0 = 0, *vf = 0;

    if (!fp) return(fprintf(vdflog,"%s: ",path),perror("fopen"),1);
    /* get the file and block headers and some checks */
    err = basic_checks_sgv2_harder(fp, pars, &read_len, &block_size);
    if (err) err *= 1000;

    /* TODO: this logic requires knowing the rate! */
    pkt_rate = pars->pkts_per_sec;

    /* get an initial header, and the length */
    if (!err) v0 = first_sgv2_harder(fp, offset, pkt_rate,
        read_len, &pkt_len);
    if (!v0) err += 10000;
    
    /* estimate the number of packets */
    if (!err && block_size > 0 && read_len > 0) {
        nblks = (vfuse->st_size - 5) / block_size;
        runt = (vfuse->st_size - 5) - nblks * block_size;
        num_pkts = block_size / read_len;   /* pkts/block */
        num_pkts *= nblks;                  /* total pkts */
        if (runt) num_pkts += (runt - 8) / read_len;
    } else {
        /* totally screwed, this is just for reportage */
        err += 30000;
        num_pkts = 0;
    }
    
    /* get a final header */
    if (!err) vf = final_sgv2_harder(fp, offset, pkt_rate, 
        vfuse->st_size, read_len, pkt_len);
    if (!vf) err += 20000;

    /* TODO: call find_the_damn_rate_sgv2() when it isn't a no-op */
    // pkt_rate = pars->pkts_per_sec;

    if ((vdifuse_debug>0) && (failcode && !err)) fprintf(vdflog,
        "Problematic file %s [%d -> %d] saved\n", path, failcode, err);
    fclose(fp);
    if (err) return(err);
    update_vfuse(vfuse, v0, vf, 0, offset, read_len, pkt_rate, num_pkts);
    return(0);
}

/*
 * Analyze the fragment and put what we find in the vfuse area
 * which already contains the results of a stat call on the file.
 * Note that st_size still retains the size of the frag in bytes.
 * The fragment is assumed to have at least 4 packets.
 *
 * Based on the rigor value, select the analyzer to complete the analysis.
 * if there are no issues in doing so, set the stype of the fragment
 *
 * We could allow per-file prefix and offset adjustments; this requires
 *   the function calls here would need to be adjusted
 *   and additional storage in the cache entry as well
 * this would be needed if the fragments weren't broken on packet edges.
 */
static int analyze_fragment(const char *path, struct stat *vfuse,
    VDIFUSEpars *pars, int rigor, uint32_t *stype)
{
    int rv, rvh = 0;
    if (rigor & VDIFUSE_RIGOR_MAGIC) {
        rv = analyze_fragment_sgv2(path, vfuse, pars);
        if (rv) rvh = analyze_fragment_sgv2_harder(path, vfuse, pars, rv);
        if (rvh) rv += rvh;
        else rv = rvh;
        /*
         * Note that the next calls are to attach_sgv2_anc() so
         * if we make arrangements to update the fuse entry for the
         * fragment, we can simplify the work here.
         */
        *stype = (rv) ? VDIFUSE_STYPE_NULL : VDIFUSE_STYPE_SGV2;
    } else {
        rv = analyze_fragment_vdif(path, vfuse, pars);
        *stype = (rv) ? VDIFUSE_STYPE_NULL : VDIFUSE_STYPE_VDIF;
    }
    return(rv);
}

/*
 * If we didn't know the packet rate, we do now, and so we can
 * set a bunch of dependent parameters from the input options.
 */
static void update_rate_params(VDIFUSEpars *pars)
{
    size_t pkt_rate = pars->est_pkt_rate;
    if (vdifuse_debug>0) fprintf(vdflog,
        "Default rate was revised to %lu packets per second\n", pkt_rate);
    if (pars->max_pkts_gap == 0 && pars->max_secs_gap > 0) {
        pars->max_pkts_gap = pars->max_secs_gap * pkt_rate;
        if (vdifuse_debug>0) fprintf(vdflog,
            "Max gap now set to %u packets\n", pars->max_pkts_gap);
    }
    if (pars->max_secs_gap == 0 && pars->max_pkts_gap > 0) {
        pars->max_secs_gap = (float)pars->max_pkts_gap / (float)pkt_rate;
        if (vdifuse_debug>0) fprintf(vdflog,
            "Max gap(s) now set to %g seconds\n", pars->max_secs_gap);
    }
}

/*
 * Populate the vfuse entry.  We start with a stat() call
 * to pick up values for entries that have no special meaning.
 *
 * The information from the rigor tests is passed along in rigor.
 */
int create_fragment_vfuse(VDIFUSEntry *vc, int index,
    VDIFUSEpars *pars, int rigor)
{
    static char fusename[VDIFUSE_MAX_PATH];
    char *xf;
    int rv;

    vdif_signature = 0LL;
    maxfrcounter_seen = 0;
    if (stat(vc->path, &vc->u.vfuse)) return(perror("stat"),1);
    if ((rv = analyze_fragment(vc->path,
        &vc->u.vfuse, pars, rigor, &vc->stype))) {
        if (vdifuse_debug>1) fprintf(vdflog,
            "      analyze_fragment tossed it by reason %d\n", rv);
        return(rv);
    }

    /* if there was a problem constructing a signature, we are toast */
    if (vdif_signature == 0LL) return(fprintf(vdflog, "!Signature\n"));
    vc->vsig = vdif_signature;

    /* if the packet rate of this file was larger than we had */
    vc->entry_pps = vc->u.vfuse.st_ino;
    if (vc->entry_pps > pars->est_pkt_rate) {
        pars->est_pkt_rate = vc->entry_pps;
        update_rate_params(pars);
    }

    /* generate a path for the fuse name */
    vc->u.vfuse.st_mode = S_IFREG | 0444;
    vc->u.vfuse.st_nlink = 1;
    xf = strrchr(vc->path, '/');
    if (!xf) return(fprintf(stderr, "No / in path %s\n", vc->path));

    /* ok:  go on and create the fusename for it */
    snprintf(fusename, VDIFUSE_MAX_PATH, xf+1);
    /* if names are not unique, construct unique name using index */
    if (!pars->noduplicates) {
        xf = strstr(fusename, ".vdif");
        if (!xf) xf = fusename + strlen(fusename);
        snprintf(xf, VDIFUSE_MAX_PATH-strlen(fusename)+4,
            "_vf%08X.vdif", index);
    }
    snprintf(vc->fuse, VDIFUSE_MAX_PATH, "%s/%s", fragments_topdir, fusename);
    if (vdifuse_debug>1) fprintf(vdflog,
        "    real '%s'\n"
        "    fuse '%s' [%016lX]\n", vc->path, vc->fuse, vc->vsig);

    if (maxfrcounter_seen > pars->maxfrcounter)
        pars->maxfrcounter = maxfrcounter_seen;
    if (vdifuse_debug>3) fprintf(vdflog,
        "FC %u %u\n", maxfrcounter_seen, pars->maxfrcounter);

    vc->hier[0] = '-';  /* overwritten later */
    vc->hier[1] = 0;
    vc->cindex = 0;     /* updated later for sgv2 */
    vc->ccount = 0;     /* updated later for sgv2 */

    if (vdifuse_debug>3) describe_fragment(vc);

    return(0);
}

/*
 * eof
 */
