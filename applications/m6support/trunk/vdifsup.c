/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdifsup.c 5755 2023-03-26 16:47:18Z gbc $
 *
 * This file provides support for the fuse interface.
 * This version is rather primitive in many respects.
 * Creation and maintenance of the cache is in vdifpars.c
 *
 * vdiflog should be used for this file for the non-fuse work,
 * however some of the activities are performed from fuse and
 * thus should be split into a separate file.
 *
 * NOTION: split file into prep and fuse type work.
 */

#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "vdifuse.h"
#include "sg_access.h"

/*
 * Should modify for m(first) c(final) a(delta) to agree with frag usage.
 */
void set_creation_time(struct stat *stbuf)
{
    VDIFUSEntry *vd_cache = current_cache_start();
    stbuf->st_mtime =
    stbuf->st_ctime =
    stbuf->st_atime = vd_cache[0].u.vpars.creation.tv_sec;
    stbuf->st_mtim.tv_nsec =
    stbuf->st_ctim.tv_nsec =
    stbuf->st_atim.tv_nsec = vd_cache[0].u.vpars.creation.tv_usec * 1000;
}

/*
 * For the following three methods, mp == NULL in initial cache
 * creation.  If it is not null, we actually have a mount point
 * and we want to inherit almost all of the stbuf info from it.
 */
int create_root_entry(struct stat *mp)
{
    VDIFUSEntry *vd_cache = current_cache_start();
    int ee = VDIFUSE_TOPDIR_ROOT_DIR;
    if (mp) {   /* update directory with mountpoint stat data */
        struct stat *stbuf = &vd_cache[ee].u.vfuse;
        *stbuf = *mp;
        stbuf->st_mode = S_IFDIR | 0555;
        stbuf->st_nlink = 4;
    } else {
        vd_cache[ee].index = ee;
        vd_cache[ee].etype = VDIFUSE_ENTRY_DIRECTORY;
        vd_cache[ee].cindex = vd_cache[ee].ccount = 0;
        strcpy(vd_cache[ee].fuse, "/");
        increment_cache_modified();
    }
    return(0);
}
int create_fragments_entry(struct stat *mp)
{
    VDIFUSEntry *vd_cache = current_cache_start();
    int ee = VDIFUSE_TOPDIR_FRAGMENT;
    if (mp) {   /* update directory with mountpoint stat data */
        struct stat *stbuf = &vd_cache[ee].u.vfuse;
        *stbuf = *mp;
        stbuf->st_mode = S_IFDIR | 0555;
        stbuf->st_nlink = 2;
    } else {
        vd_cache[ee].index = ee;
        vd_cache[ee].etype = VDIFUSE_ENTRY_DIRECTORY;
        vd_cache[ee].cindex = vd_cache[ee].ccount = 0;
        strcpy(vd_cache[ee].fuse, fragments_topdir);
        increment_cache_modified();
    }
    return(0);
}
int create_sequences_entry(struct stat *mp)
{
    VDIFUSEntry *vd_cache = current_cache_start();
    int ee = VDIFUSE_TOPDIR_SEQUENCE;
    if (mp) {   /* update directory with mountpoint stat data */
        struct stat *stbuf = &vd_cache[ee].u.vfuse;
        *stbuf = *mp;
        stbuf->st_mode = S_IFDIR | 0555;
        stbuf->st_nlink = 2;
    } else {
        vd_cache[ee].index = ee;
        vd_cache[ee].etype = VDIFUSE_ENTRY_DIRECTORY;
        vd_cache[ee].cindex = vd_cache[ee].ccount = 0;
        strcpy(vd_cache[ee].hier, sequences_topdir);
        strcpy(vd_cache[ee].fuse, sequences_topdir);
        increment_cache_modified();
    }
    return(0);
}
int create_vprocdir_entry(struct stat *mp)
{
    VDIFUSEntry *vd_cache = current_cache_start();
    int ee = VDIFUSE_TOPDIR_VPROCDIR;
    if (mp) {   /* update directory with mountpoint stat data */
        struct stat *stbuf = &vd_cache[ee].u.vfuse;
        *stbuf = *mp;
        stbuf->st_mode = S_IFDIR | 0555;
        stbuf->st_nlink = 2;
    } else {
        vd_cache[ee].index = ee;
        vd_cache[ee].etype = VDIFUSE_ENTRY_DIRECTORY;
        vd_cache[ee].cindex = vd_cache[ee].ccount = 0;
        strcpy(vd_cache[ee].hier, vdifproc_topdir);
        strcpy(vd_cache[ee].fuse, vdifproc_topdir);
        increment_cache_modified();
    }
    return(0);
}
int create_vthreads_entry(struct stat *mp)
{
    VDIFUSEntry *vd_cache = current_cache_start();
    int ee = VDIFUSE_TOPDIR_VTHREADS;
    if (!vthreads_dir) return(0);
    if (mp) {   /* update directory with mountpoint stat data */
        struct stat *stbuf = &vd_cache[ee].u.vfuse;
        *stbuf = *mp;
        stbuf->st_mode = S_IFDIR | 0555;
        stbuf->st_nlink = 2;
    } else {
        vd_cache[ee].index = ee;
        vd_cache[ee].etype = VDIFUSE_ENTRY_DIRECTORY;
        vd_cache[ee].cindex = vd_cache[ee].ccount = 0;
        strcpy(vd_cache[ee].hier, vthreads_topdir);
        strcpy(vd_cache[ee].fuse, vthreads_topdir);
        increment_cache_modified();
    }
    return(0);
}

/*
 * Sequences and Directories inherit ownership from the mount point
 * Also true of vthreads and vprocdir
 */
static int update_other_entries(struct stat *mp)
{
    VDIFUSEntry *vd_cache = current_cache_start();
    uint32_t vd_num_entries = current_cache_entries();
    int ee;
    for (ee = VDIFUSE_TOPDIR_VARIABLE; ee < vd_num_entries; ee++) {
        if (vd_cache[ee].etype != VDIFUSE_ENTRY_SEQUENCE &&
            vd_cache[ee].etype != VDIFUSE_ENTRY_VPROCDIR &&
            vd_cache[ee].etype != VDIFUSE_ENTRY_VTHREADS &&
            vd_cache[ee].etype != VDIFUSE_ENTRY_DIRECTORY) continue;
        vdiflog(2,
            "%s: uid %d -> %d gid %d -> %d &c.\n", vd_cache[ee].fuse,
            vd_cache[ee].u.vfuse.st_uid, mp->st_uid,
            vd_cache[ee].u.vfuse.st_gid, mp->st_gid);
        vd_cache[ee].u.vfuse.st_uid = mp->st_uid;
        vd_cache[ee].u.vfuse.st_gid = mp->st_gid;
    }
    increment_cache_modified();
    return(0);
}

/*
 * Create the VDIF meta-data cache.  Return 0 if the cache
 * created is ready for use, otherwise return nonzero.
 */
int vdifuse_create_metadata(char *cache, int ndirs, char **dirs)
{
    int err;
    vdiflog(0, "Creating Meta Data cache for fuse in %s\n", cache);
    if ((err = create_cache(cache))) return(fprintf(stderr,
        "Unable to create cache %s (%d)\n", cache, err));
    if ((err = populate_cache(ndirs, dirs))) return(fprintf(stderr,
        "Unable to build cache with the %d directories provided (%d)\n",
            ndirs, err));
    /* at this point the cache is created, and in memory */
    return(0);
}

/*
 *  stat buf usage for fragments:
 *  (normal usage except for + items)
 * + dev_t     st_dev;     // prefix_bytes (to first packet)
 * + ino_t     st_ino;     // pkts_per_sec
 *   mode_t    st_mode;    // protection
 *   nlink_t   st_nlink;   // number of hard links
 *   uid_t     st_uid;     // user ID of owner
 *   gid_t     st_gid;     // group ID of owner
 * + dev_t     st_rdev;    // offset_bytes (VDIF hdr in packet)
 *   off_t     st_size;    // total size, in bytes
 * + blksize_t st_blksize; // packet size
 * + blkcnt_t  st_blocks;  // number of packets
 * + time_t    st_mtime;   // data start time
 * + time_t    st_ctime;   // data end time (inc. last packet)
 * + time_t    st_atime;   // total scan duration
 */
int describe_fragment(VDIFUSEntry *vp)
{   
    vdiflog(1,
        "[%04d]Frg %s\n", vp->index, vp->path);
    vdiflog(2,
        "  (index=%u vsig=%016lX)\n"
        "  (size=%lu pktsize=%ld pkts=%ld prefix=%lu offset=%lu rate=%lu)\n"
        "  (from=%lu.%09lu to=%lu.%09lu dur=%lu.%09lu)\n"
        "  (hier %s)\n  (frag %s)\n",
        vp->index, vp->vsig,
        vp->u.vfuse.st_size, vp->u.vfuse.st_blksize, vp->u.vfuse.st_blocks,
        vp->u.vfuse.st_dev, vp->u.vfuse.st_rdev, vp->u.vfuse.st_ino,
        vp->u.vfuse.st_mtime, vp->u.vfuse.st_mtim.tv_nsec,
        vp->u.vfuse.st_ctime, vp->u.vfuse.st_ctim.tv_nsec,
        vp->u.vfuse.st_atime, vp->u.vfuse.st_atim.tv_nsec,
        vp->hier, vp->fuse
    );
    return(0);
}
static int describe_vprocdir(VDIFUSEntry *vp)
{
    vdiflog(-1, "vprocdir developer error\n");
    return(1);
}

static void cascade_time(struct tm *tm)
{
    int din = (tm->tm_year / 4) ? 365 : 366;    /* broken for 2000 */
    if (tm->tm_sec == 60) { tm->tm_sec = 0; tm->tm_min ++; }
    if (tm->tm_min == 60) { tm->tm_min = 0; tm->tm_hour ++; }
    if (tm->tm_hour == 24) { tm->tm_hour = 0; tm->tm_yday ++; }
    if (tm->tm_yday == din) { tm->tm_yday = 0 ; tm->tm_year ++; }
}

int report_sequence(VDIFUSEntry *vp)
{
    struct tm start;
    struct tm stop;
    /* round out to nearest second for this */
    gmtime_r(&vp->u.vfuse.st_mtime, &start);
    if (vp->u.vfuse.st_mtim.tv_nsec > VDIFUSE_ONE_SEC_NS/2) start.tm_sec ++;
    cascade_time(&start);
    gmtime_r(&vp->u.vfuse.st_ctime, &stop);
    if (vp->u.vfuse.st_ctim.tv_nsec > VDIFUSE_ONE_SEC_NS/2) stop.tm_sec ++;
    cascade_time(&stop);
    vdiflog(-1,
        "%s%-38s %04dy%03dd%02dh%02dm%02ds %04dy%03dd%02dh%02dm%02ds\n",
        filelistprefix, vp->fuse,
        start.tm_year + 1900, start.tm_yday + 1,
        start.tm_hour, start.tm_min, start.tm_sec,
        stop.tm_year + 1900, stop.tm_yday + 1,
        stop.tm_hour, stop.tm_min, stop.tm_sec);
    return(0);
}

int describe_seq_or_vthr(VDIFUSEntry *vp, char *sot)
{
    vdiflog(1,
        "[%04d]%s %s anc count [%d] %lu links %luB\n",
        vp->index, sot, vp->fuse, vp->cindex,
        vp->u.vfuse.st_nlink, vp->u.vfuse.st_size);
    vdiflog(2,
        "  pktsize=%lu pkts=%lu %lu.%09lu %lu.%09lu %lu.%09lu\n", 
        vp->u.vfuse.st_blksize, vp->u.vfuse.st_blocks,
        vp->u.vfuse.st_mtime, vp->u.vfuse.st_mtim.tv_nsec,
        vp->u.vfuse.st_ctime, vp->u.vfuse.st_ctim.tv_nsec,
        vp->u.vfuse.st_atime, vp->u.vfuse.st_atim.tv_nsec
        );
    return(0);
}
int describe_sequence(VDIFUSEntry *vp)
{
    return(describe_seq_or_vthr(vp, "Seq"));
}
int describe_vthreads(VDIFUSEntry *vp)
{
    return(describe_seq_or_vthr(vp, "Vth"));
}
int describe_directory(VDIFUSEntry *vp)
{
    vdiflog(1, "[%04d]Dir %s anc count [%d]\n", vp->index,
            *(vp->fuse) ? vp->fuse : " (root)", vp->cindex);
    vdiflog(0, "[%04d]Dir %s\n", vp->index,
            *(vp->path) ? vp->path : " (empty)");
    return(0);
}

int describe_struct(void)
{
    vdiflog(-1,
        "Size: The cache consists of entries of size %ld including a\n"
        "Size: union of size %ld (vfuse=%ld vpars=%ld vseqi=%ld voids=%ld),\n"
        "Size: paths of no more than %d bytes and sgv2 data (size=%d)\n"
        "Size: 6*4 + 8 + %ld + 3*%d = %ld\n",
        sizeof(VDIFUSEntry), sizeof(union vdifuse_union),
        sizeof(struct stat), sizeof(VDIFUSEpars),
        sizeof(uint32_t)*VDIFUSE_MAX_SEQI,
        sizeof(void *)*VDIFUSE_NUM_VOIDS,
        VDIFUSE_MAX_PATH, sg_info_size(),
        sizeof(union vdifuse_union), VDIFUSE_MAX_PATH,
        6*4 + 8 + sizeof(union vdifuse_union) + 3*VDIFUSE_MAX_PATH);
    return(0);
}

char *describe_stype(int stype)
{
    char *anctype;
    switch (stype) {
    case VDIFUSE_STYPE_NULL: anctype = "Empty fragment"; break;
    case VDIFUSE_STYPE_VDIF: anctype = "VDIF fragment"; break;
    case VDIFUSE_STYPE_SGV2: anctype = "SGv2 fragment"; break;
    case VDIFUSE_STYPE_INFO: anctype = "SGv2 access"; break;
    case VDIFUSE_STYPE_SDIR: anctype = "Dir entries"; break;
    case VDIFUSE_STYPE_PART: anctype = "Part of seq"; break;
    default:
        fprintf(stderr, "Illegal stype %d\n", stype);
        anctype = "ERROR";
        break;
    }
    return(anctype);
}

static inline int describe_ancillary_detail(VDIFUSEntry *vp)
{
    char *prefix, *suffix;
    int ee;
    if (vp->stype == VDIFUSE_STYPE_INFO)
        return(describe_ancillary_sgv2(vp));
    for (ee = 0; ee < vp->ccount; ee++) {
        prefix = (ee%8 == 0) ? "  (" : "";
        suffix = (ee%8 == 7) ? ")\n" :
            (ee == vp->ccount-1) ? ")\n" : " ";
        vdiflog(-1, "%s[%04d]%s", prefix, vp->u.vseqi[ee], suffix);
    }
    return(0);
}

int describe_ancillary(VDIFUSEntry *vp)
{
    char *anctype;
    int rv = 0;
    anctype = describe_stype(vp->stype);
    vdiflog(1,
        "[%04d]Anc %s (%s) with %d\n",
            vp->index, vp->fuse, anctype, vp->ccount);
    if (vdifuse_debug>2) rv = describe_ancillary_detail(vp);
    return(rv);
}

/*
 * A debugging version of the guts of the following.
 */
void describe_cache_entry(VDIFUSEntry *vp)
{
    VDIFUSEntry * vd_cache = current_cache_start();
    switch (vp->etype) {
    case VDIFUSE_ENTRY_FRAGMENT:
        (void)describe_fragment(vp);
        break;
    case VDIFUSE_ENTRY_SEQUENCE:
        (void)describe_sequence(vp);
        break;
    case VDIFUSE_ENTRY_VTHREADS:
        (void)describe_vthreads(vp);
        break;
    case VDIFUSE_ENTRY_VPROCDIR:
        (void)describe_vprocdir(vp);
        break;
    case VDIFUSE_ENTRY_DIRECTORY:
        (void)describe_directory(vp);
        break;
    case VDIFUSE_ENTRY_PARAMS:
        (void)describe_params(vp);
        break;
    case VDIFUSE_ENTRY_ANCILLARY:
        (void)describe_ancillary(vp);
        break;
    default:
    case VDIFUSE_ENTRY_INVALID:
        vdiflog(1, "Entry [%04ld] (%p) marked invalid\n",
            (vp - vd_cache) / sizeof(VDIFUSEntry), vp);
        break;
    }
}

/*
 * shortcut method
 */
void describe_cache_params(void)
{
    (void)describe_params(current_cache_start());
}

/*
 * Walk through the list, check and comment as required.
 */
static int report_on_cache(void)
{
    VDIFUSEntry * vd_cache = current_cache_start();
    uint32_t vd_num_entries = current_cache_entries();
    long ee, vthrs = 0, vpcds = 0;
    long frags = 0, seqs = 0, dirs = 0, params = 0, anc = 0, invalids = 0;
    VDIFUSEntry *vp;
    for (ee = 0, vp = vd_cache; ee < vd_num_entries; ee++, vp++) {
        switch (vp->etype) {
        case VDIFUSE_ENTRY_FRAGMENT:
            frags++;
            if (describe_fragment(vp)) return(1);
            break;
        case VDIFUSE_ENTRY_SEQUENCE:
            seqs++;
            if (describe_sequence(vp)) return(2);
            break;
        case VDIFUSE_ENTRY_VTHREADS:
            vthrs++;
            if (describe_vthreads(vp)) return(6);
            break;
        case VDIFUSE_ENTRY_VPROCDIR:
            vpcds++;
            if (describe_vprocdir(vp)) return(6);
            break;
        case VDIFUSE_ENTRY_DIRECTORY:
            dirs++;
            if (describe_directory(vp)) return(3);
            break;
        case VDIFUSE_ENTRY_PARAMS:
            params++;
            if (describe_params(vp)) return(4);
            break;
        case VDIFUSE_ENTRY_ANCILLARY:
            anc++;
            if (describe_ancillary(vp)) return(5);
            break;
        default:
        case VDIFUSE_ENTRY_INVALID:
            invalids++;
            vdiflog(1, "[%04ld]Entry (%s) marked invalid/unused\n",
                ee, vp->path);
            break;
        }
    }

    vdiflog(0, "Have "
        "%lu frags %lu seqs %lu dirs %lu params %lu "
        "anc %lu prc %lu vthr %lu inv\n",
        frags, seqs, dirs, params, anc, vpcds, vthrs, invalids);
    return(0);
}

/*
 * A slightly different reporting function
 */
static int filelist_from_cache(void)
{
    VDIFUSEntry * vd_cache = current_cache_start();
    uint32_t vd_num_entries = current_cache_entries();
    long ee;
    VDIFUSEntry *vp;
    for (ee = 0, vp = vd_cache; ee < vd_num_entries; ee++, vp++) {
        /* we only care about this one */
        if (((vp->etype == VDIFUSE_ENTRY_SEQUENCE) ||
             (vp->etype == VDIFUSE_ENTRY_VTHREADS)) &&
            report_sequence(vp)) return(2);
    }
    return(0);
}

static int check_topdirs(void)
{
    VDIFUSEntry * vd_cache = current_cache_start();
    char *x;
    x = vd_cache[VDIFUSE_TOPDIR_PARAMS].u.vpars.frag_top_dir;
    if (strcmp(x, fragments_topdir))
        return(fprintf(stderr, "%s != %s\n", x, fragments_topdir));
    x = vd_cache[VDIFUSE_TOPDIR_PARAMS].u.vpars.seqs_top_dir;
    if (strcmp(x, sequences_topdir))
        return(fprintf(stderr, "%s != %s\n", x, sequences_topdir));
    if (vthreads_dir) {
        x = vd_cache[VDIFUSE_TOPDIR_PARAMS].u.vpars.vthr_top_dir;
        if (strcmp(x, vthreads_topdir))
            return(fprintf(stderr, "%s != %s\n", x, vthreads_topdir));
    }
    x = vd_cache[VDIFUSE_TOPDIR_PARAMS].u.vpars.proc_top_dir;
    if (strcmp(x, vdifproc_topdir))
        return(fprintf(stderr, "%s != %s\n", x, vdifproc_topdir));
    return(0);
}

/*
 * Backward compatibility on the cache is difficult, and it is
 * safer to just require the user to regenerate it.
 */
static int check_cache_version(void)
{
    VDIFUSEntry * vd_cache = current_cache_start();
    if (check_topdirs()) return(2);
    if (vd_cache[0].u.vpars.vdifuse_vers == (float)VDIFUSE_VERSION) return(0);
    fprintf(stderr, "File version %f != code version %f (%e)\n",
        vd_cache[0].u.vpars.vdifuse_vers, (float)VDIFUSE_VERSION,
        vd_cache[0].u.vpars.vdifuse_vers - (float)VDIFUSE_VERSION);
    fprintf(stderr, "You need to regenerate the file cache (-c)\n");
    return(1);
}
/*
 * Check the cache file which is (at this point) known to exist
 * and contain at least one entry.  Return 0 if the cache
 * passes some sanity checks.  Return nonzero otherwise.
 *
 * If vdifuse_debug is nonzero, provide a verbose report.
 */
int vdifuse_report_metadata(char *cache, struct stat *sb)
{
    VDIFUSEntry * vd_cache;
    uint32_t vd_num_entries;
    int err;
    vdiflog(0,
        "Reporting Meta Data cache '%s' for use with fuse.\n", cache);
    vdiflog(0,
        "This cache contains %ld entries of size %ld\n",
            sb->st_size / sizeof(VDIFUSEntry), sizeof(VDIFUSEntry));
    if ((err = load_cache(cache))) return(fprintf(stderr,
        "Unable to open cache '%s' (%d)\n", cache, err));
    vd_cache = current_cache_start();
    vd_num_entries = current_cache_entries();
    if (check_cache_version()) return(fprintf(stderr,
        "Incompatible cache '%s' -- cannot continue.\n", cache));
    /* at this point the cache is in memory */
    if (vd_num_entries && vd_cache) return(report_on_cache());
    return (fprintf(stderr, "Cache wasn't loaded properly\n"));
}

/*
 * A variant of the previous, which generates filelists suitable
 * for use as filelists in v2d files.  It is relatively terse.
 */
int vdifuse_report_filelist(char *cache, struct stat *sb)
{
    VDIFUSEntry * vd_cache;
    uint32_t vd_num_entries;
    int err;
    if ((err = load_cache(cache))) return(fprintf(stderr,
        "Unable to open cache '%s' (%d)\n", cache, err));
    vd_cache = current_cache_start();
    vd_num_entries = current_cache_entries();
    if (check_cache_version()) return(fprintf(stderr,
        "Incompatible cache '%s' -- cannot continue.\n", cache));
    /* at this point the cache is in memory */
    if (vd_num_entries && vd_cache) return(filelist_from_cache());
    return (fprintf(stderr, "Cache wasn't loaded properly\n"));
}

/*
 * Update the cache with data from the mount point
 */
static int update_mount_point(struct stat *mp)
{
    set_creation_time(mp);
    if (create_root_entry(mp) ||
        create_fragments_entry(mp) ||
        create_sequences_entry(mp) ||
        create_vprocdir_entry(mp) ||
        create_vthreads_entry(mp) ||
        update_other_entries(mp))
            return(fprintf(stderr, "Issues with mount point\n"));
    return(0);
}

/*
 * If not creating or reporting, we're using
 */
int vdifuse_access_metadata(char *cache, struct stat *sb, struct stat *mp)
{
    VDIFUSEntry * vd_cache = current_cache_start();
    uint32_t vd_num_entries = current_cache_entries();
    int err;
    /* check if already loaded */
    if (vd_num_entries && vd_cache) return(update_mount_point(mp));
    vdiflog(0,
        "Accessing Meta Data cache for fuse in %s\n", cache);
    vdiflog(0,
        "Cache contains %ld entries of size %ld\n",
            sb->st_size / sizeof(VDIFUSEntry), sizeof(VDIFUSEntry));
    if ((err = load_cache(cache))) return(fprintf(stderr,
        "Unable to open cache %s (%d)\n", cache, err));
    vd_cache = current_cache_start();
    vd_num_entries = current_cache_entries();
    if (check_cache_version()) return(fprintf(stderr,
        "Incompatible cache %s -- cannot continue.\n", cache));
    /* at this point the cache is in memory */
    if (vd_num_entries && vd_cache) return(update_mount_point(mp));
    return (fprintf(stderr, "Cache wasn't loaded properly\n"));
}

/*
 * Top-level directories are somewhat special.
 */
void vdifuse_topdir(int which, struct stat *stbuf)
{
    VDIFUSEntry * vd_cache = current_cache_start();
    switch (which) {
    case VDIFUSE_TOPDIR_ROOT_DIR:
        *stbuf = vd_cache[VDIFUSE_TOPDIR_ROOT_DIR].u.vfuse;
        break;
    case VDIFUSE_TOPDIR_FRAGMENT:
        *stbuf = vd_cache[VDIFUSE_TOPDIR_FRAGMENT].u.vfuse;
        break;
    case VDIFUSE_TOPDIR_SEQUENCE:
        *stbuf = vd_cache[VDIFUSE_TOPDIR_SEQUENCE].u.vfuse;
        break;
    case VDIFUSE_TOPDIR_VPROCDIR:
        *stbuf = vd_cache[VDIFUSE_TOPDIR_VPROCDIR].u.vfuse;
        break;
    case VDIFUSE_TOPDIR_VTHREADS:
        *stbuf = vd_cache[VDIFUSE_TOPDIR_VTHREADS].u.vfuse;
        break;
    default:
        fprintf(stderr, "Developer Error\n");
        break;
    }
}

/*
 * Support for attributes; these return 0 if the path is found.
 * This lookup isn't terribly efficient, but computers are fast
 */
int vdifuse_fragment(const char *path, struct stat *stbuf)
{
    VDIFUSEntry * vd_cache = current_cache_start();
    uint32_t vd_num_entries = current_cache_entries();
    int ii;
    for (ii = 0; ii < vd_num_entries; ii++) {
        /* this directory tree contains only fragments */
        if (vd_cache[ii].etype != VDIFUSE_ENTRY_FRAGMENT) continue;
        if (!strcmp(path, vd_cache[ii].fuse)) {
            vdiflog(1, "Found %s\n", path);
            memcpy(stbuf, &vd_cache[ii].u.vfuse, sizeof(struct stat));
            return(0);
        }
    }
    vdiflog(1, "Did not find %s\n", path);
    return(1);
}
int vdifuse_sequence(const char *path, struct stat *stbuf)
{
    VDIFUSEntry * vd_cache = current_cache_start();
    uint32_t vd_num_entries = current_cache_entries();
    int ii;
    for (ii = 0; ii < vd_num_entries; ii++) {
        /* this directory tree contains subdirs and sequences */
        if (vd_cache[ii].etype != VDIFUSE_ENTRY_DIRECTORY &&
            vd_cache[ii].etype != VDIFUSE_ENTRY_SEQUENCE) continue;
        if (!strcmp(path, vd_cache[ii].fuse)) {
            vdiflog(1, "Found %s\n", path);
            memcpy(stbuf, &vd_cache[ii].u.vfuse, sizeof(struct stat));
            return(0);
        }
    }
    vdiflog(1, "Did not find %s\n", path);
    return(1);
}
int vdifuse_vthreads(const char *path, struct stat *stbuf)
{
    VDIFUSEntry * vd_cache = current_cache_start();
    uint32_t vd_num_entries = current_cache_entries();
    int ii;
    for (ii = 0; ii < vd_num_entries; ii++) {
        /* this directory tree contains subdirs and sequences */
        if (vd_cache[ii].etype != VDIFUSE_ENTRY_DIRECTORY &&
            vd_cache[ii].etype != VDIFUSE_ENTRY_VTHREADS) continue;
        if (!strcmp(path, vd_cache[ii].fuse)) {
            vdiflog(1, "Found %s\n", path);
            memcpy(stbuf, &vd_cache[ii].u.vfuse, sizeof(struct stat));
            return(0);
        }
    }
    vdiflog(1, "Did not find %s\n", path);
    return(1);
}

/*
 * Support for readdir.
 * Starting with index, provide the fuse name and stbuf data.
 * If no more, return 0.
 */
int get_vdifuse_fragment(int ii, char **name, struct stat **stbuf)
{
    VDIFUSEntry * vd_cache = current_cache_start();
    uint32_t vd_num_entries = current_cache_entries();
    for (ii++ ; ii < vd_num_entries; ii++) {
        if (vd_cache[ii].etype != VDIFUSE_ENTRY_FRAGMENT) continue;
        *name = strrchr(vd_cache[ii].fuse, '/');
        if (!*name) *name = "/.damaged-fragment";
        (*name) ++ ;
        *stbuf = &vd_cache[ii].u.vfuse;
        return(ii);
    }
    return(0);  /* no more */
}
/*
 * Support for readdir.
 * Provide the top-level sequences directory listing.
 * There should be an ANCILLARY file with the info for the entries.
 * if seqhierarchy is 0, we're actually providing a list of sequences.
 * if seqhierarchy > 0, we're providing exp subdirs
 * Returns of 0 indicate we are done, otherwise return the
 * nonzero index of the subdirectory or sequence entry.
 */
static int get_vdifuse_seq_or_vthr(
    int ii, char **name, struct stat **stbuf, int ee)
{
    VDIFUSEntry * vd_cache = current_cache_start();
    static uint32_t sndx, sdir;
    static VDIFUSEntry *vseq;
    VDIFUSEntry *vc;

    if (ii == 0) {                  /* first time through */
        vseq = &vd_cache[vd_cache[ee].cindex];
        sdir = vseq->u.vseqi[sndx = 0];
    } else {                        /* pick up where we left off */
        if (++sndx == VDIFUSE_MAX_SEQI) {
            if (vseq->cindex == 0) return(0);
            vseq = &vd_cache[vseq->cindex];
            sndx = 0;
        }
        sdir = vseq->u.vseqi[sndx];
    }

    if (sdir == 0) {
        ii = 0;                     /* no more entries */
    } else {
        vc = &vd_cache[sdir];
        *name = strrchr(vc->fuse, '/');
        if (!*name) *name = "/.damaged-sequence";
        (*name) ++;
        *stbuf = &vc->u.vfuse;
        ii = vseq - vd_cache;       /* index of current entry */
    }
    return(ii);
}
int get_vdifuse_sequence(int ii, char **name, struct stat **stbuf)
{
    return(get_vdifuse_seq_or_vthr(ii, name, stbuf, VDIFUSE_TOPDIR_SEQUENCE));
}
int get_vdifuse_vthreads(int ii, char **name, struct stat **stbuf)
{
    return(get_vdifuse_seq_or_vthr(ii, name, stbuf, VDIFUSE_TOPDIR_VTHREADS));
}

/*
 * Return the index of the sequence subdirectory matching path, if found.
 * If found, subsequence calls to get_vdifuse_subdir() fill in the entries.
 * This pair is similar to get_vdifuse_sequence(), except that the first
 * is needed to locate the parent subdirectory so that the second can
 * unload starting there.
 */
static int get_seq_or_vthr_subdir(const char *path, int ee)
{
    VDIFUSEntry * vd_cache = current_cache_start();
    uint32_t vd_num_entries = current_cache_entries();
    int ii;
    for (ii = 0 ; ii < vd_num_entries; ii++) {
        if (vd_cache[ii].etype != ee &&
            vd_cache[ii].etype != VDIFUSE_ENTRY_DIRECTORY) continue;
        if (strcmp(path, vd_cache[ii].fuse)) continue;
        vdiflog(1, "Found %s entry %d\n", path, ii);
        return(ii);
    }
    return(0);
}
int get_sequence_subdir(const char *path)
{
    return(get_seq_or_vthr_subdir(path, VDIFUSE_ENTRY_SEQUENCE));
}
int get_vthreads_subdir(const char *path)
{
    return(get_seq_or_vthr_subdir(path, VDIFUSE_ENTRY_VTHREADS));
}
int get_vdifuse_subdir(int ii, char **name, struct stat **stbuf)
{
    VDIFUSEntry * vd_cache = current_cache_start();
    static uint32_t sndx, sdir;
    static VDIFUSEntry *vseq;
    VDIFUSEntry *vc;

    // if (vd_cache[ii].etype == VDIFUSE_ENTRY_VPROCDIR) return(0);

    if (vd_cache[ii].etype == VDIFUSE_ENTRY_SEQUENCE ||
        vd_cache[ii].etype == VDIFUSE_ENTRY_VTHREADS ||
        vd_cache[ii].etype == VDIFUSE_ENTRY_DIRECTORY) {    /* first time */
        if (vd_cache[ii].cindex == 0) return(0);
        vseq = &vd_cache[vd_cache[ii].cindex];
        sdir = vseq->u.vseqi[sndx = 0];
    } else {
        if (++sndx == VDIFUSE_MAX_SEQI) {   /* pick up where we left off */
            if (vseq->cindex == 0) return(0);
            vseq = &vd_cache[vseq->cindex];
            sndx = 0;
        }
        sdir = vseq->u.vseqi[sndx];
    }

    if (sdir == 0) {
        ii = 0;                     /* no more entries */
    } else {
        vc = &vd_cache[sdir];
        *name = strrchr(vc->fuse, '/');
        if (!*name) *name = "/.damaged-seq-or-vthr";
        (*name) ++;
        *stbuf = &vc->u.vfuse;
        ii = vseq - vd_cache;       /* index of current entry */
    }
    return(ii);
}

/*
 * eof
 */
