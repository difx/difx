/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: vdiforr.c 5760 2023-03-26 17:43:20Z gbc $
 *
 * This file provides support for the fuse interface.
 * This version is rather primitive in many respects.
 * This file support open, read and release operations.
 *
 * vdifuse_trace is available in this file.
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>

/* for pread() on some configurations */
#define _XOPEN_SOURCE 500
#include <unistd.h>
/* just to be sure */
ssize_t pread(int fd, void *buf, size_t count, off_t offset);

#include "vdifuse.h"
#include "vdifsig.h"

/*
 * In order to maintain information across oper/read/release
 * operations, we maintain our own table of FFInfo data, indexed
 * by one of the open file descriptors (ffi->fh).  Typically the
 * caller is passingus an FFInfo structure of which only fh and flags
 * might be real.
 *
 * realfd is a special value to flag realpath (fragments)
 * vorrfd is a special value to flag erroneous file descriptors.
 * Neither of these slots in the FFInfo cache will be used.
 */
static FFInfo *FFIcache;
int realfd;
int vorrfd;

/*
 * The number of available file descriptors isn't known until runtime,
 * so we here allocate and initialize a table to hold open fds.
 *
 * This routine is called when vdifuse starts, so stderr is viable.
 */
int vorr_init(void)
{
    long open_max = sysconf(_SC_OPEN_MAX);
    if (open_max < 0) return(fprintf(stderr, "OPEN_MAX not available\n"));
    vdiftrace(3, "OPEN_MAX = %ld\n", open_max);
    FFIcache = (FFInfo *)calloc(open_max, sizeof(FFInfo));
    if (!FFIcache) return(perror("vorr_init"),2);
    realfd = getvdiflogfileno();    /* stdout or as redirected */
    vorrfd = fileno(stderr);
    FFIcache[realfd].vffers = VDIFUSE_FFIERROR_RPATH;
    FFIcache[vorrfd].vffers = VDIFUSE_FFIERROR_RPATH;
    if (realfd < 0 || vorrfd < 0 || realfd == vorrfd)
        return(fprintf(stderr, "issues with fd for realpaths or errors\n"));
    return(0);
}

/*
 * Lookup a real path by fuse path.  This is appropriate for
 * fragments, where there is a real path to the fragment.
 *
 * The last path translation is remembered for efficiency.
 * The integer vpee is set to reflect the index of the file
 * if it is found in the vprocdir cache.
 */
static char *fusepath_to_realpath(const char *fusepath, FFInfo *ffi)
{
    static uint32_t last = VDIFUSE_TOPDIR_PARAMS;
    uint32_t ii, vd_num_entries = current_cache_entries();
    char *vpath;
    VDIFUSEntry *vd_cache = current_cache_start();
    if (!strcmp(fusepath, vd_cache[last].path))
        return(vd_cache[last].path);
    if ((vpath = vproc_realpath(fusepath, ffi))) return(vpath);
    for (ii = 0; ii < vd_num_entries; ii++) {
        if (vd_cache[ii].etype != VDIFUSE_ENTRY_FRAGMENT) continue;
        if (!strcmp(fusepath, vd_cache[ii].fuse))
            return(vd_cache[last = ii].path);
    }
    return(NULL);
}

/*
 * Lookup a fuse path.  Note that a sequence will necessarily be
 * at location in the cache greater than VDIFUSE_TOPDIR_PARAMS(0),
 * so that is a useful error return value.
 */
static uint32_t fusepath_to_index(const char *fusepath)
{
    static uint32_t last = VDIFUSE_TOPDIR_PARAMS;
    uint32_t ii, vd_num_entries = current_cache_entries();
    VDIFUSEntry *vd_cache = current_cache_start();
    if (!strcmp(fusepath, vd_cache[last].fuse))
        return(last);
    for (ii = 0; ii < vd_num_entries; ii++) {
        if (vd_cache[ii].etype != VDIFUSE_ENTRY_SEQUENCE) continue;
        if (!strcmp(fusepath, vd_cache[ii].fuse))
            return(last = ii);
    }
    return(VDIFUSE_TOPDIR_PARAMS);
}

/*
 * Main entry points from vdifuse.c.
 * All return 0 on success, 1 on error, except the read operation
 * which might have a richer set of errors.
 *
 * FFInfo *ffi points to temporary stack space from caller:
 *   fh is the file handle
 *   flags are open flags
 *   size and offset are for read operations
 *   cntxt is used to (try to) recover from bus errors on reads
 *      to mmap'd data that is not available.
 *
 * For fragments, the fusepath has a corresponding realpath, so
 * we use pread to just provide the required bytes; there is no
 * history, so we use ffi->fh == 0 for this.  For sequences,
 * we call the appropriate methods to open/release the fragments.
 *
 * ffi->fh should be copied out to the fuse kernel.
 * ffi->flags are along for the ride.
 * ffi->vpendx is explained in vproc_realpath()
 */
int do_vorr_open(const char *fusepath, FFInfo *ffi)
{
    VDIFUSEntry *vs;
    vdiftrace(-1,VDT("Opening %s\n"), fusepath);
    vdifuse_marker(VDT("open"));
    ffi->vpendx = 0;
    if(fusepath_to_realpath(fusepath, ffi)) {
        ffi->fh = realfd;
        vdiftrace(0, VDT("Realpath open %s at %lu\n"), fusepath, ffi->fh);
        return(0);
    }
    ffi->sindex = fusepath_to_index(fusepath);
    if (ffi->sindex == VDIFUSE_TOPDIR_PARAMS) {
        vdiftrace(0, "No fuse path for %s\n", fusepath);
        return(-ENOENT);    /* do_vorr_open: no path */
    }
    gettimeofday(&(ffi->topen), 0);
    ffi->totrb = 0UL;
    ffi->flags = O_RDONLY;  /* unclear if this is respected */
    ffi->vffers = VDIFUSE_FFIERROR_NONE;
    vs = current_cache_start() + ffi->sindex;
    /* any vproc directories will be handled by open_*seq() */
    switch (vs->stype) {
    case VDIFUSE_STYPE_VDIF: ffi->fh = open_flat_seq(vs, ffi); break;
    case VDIFUSE_STYPE_SGV2: ffi->fh = open_sgv2_seq(vs, ffi); break;
    default:                 ffi->fh = vorrfd;                 break;
    }
    if (ffi->fh != vorrfd) {
        vdiftrace(0, VDT("Storing data for open %s at ffi[%ld]\n"),
            fusepath, ffi->fh);
        FFIcache[ffi->fh] = *ffi;
        update_sgv2_seq(&FFIcache[ffi->fh]);
        vproc_update_file(ffi, "vdif-file", fusepath, VPROC_TRUNCATE);
        return(0);
    } /* else */
    vdifuse_flush_trace();
    return(-ENOENT);    /* do_vorr_open: invalid path */
}

/*
 * For a global diagnostic
 */
static void report_access(const FFInfo *ffi, const char *path)
{
    struct timeval now;
    double dt;
    char rate[80], bulk[80];
    gettimeofday(&now, 0);
    dt = (double)(now.tv_sec - ffi->topen.tv_sec) +
         1e-6 * (double)(now.tv_usec - ffi->topen.tv_usec);

    vdiftrace(-1, VD3(
        "Accessed %lu bytes in %.3f s, rate = %.f MB/s\n",
        "Clearing stored data for %s at fh %lu\n"),
        ffi->totrb, dt, 1e-06 * (double)ffi->totrb / dt,
        (current_cache_start() + ffi->sindex)->fuse, ffi->fh);

    snprintf(bulk, sizeof(bulk), "%lu B in %.3f s\n", ffi->totrb, dt);
    vdiftrace(-1, VDT("Clos[%d] %s, %s"), ffi->fh, path, bulk);
    vproc_update_file(ffi, "read-stats", bulk, VPROC_TRUNCATE);

    snprintf(rate, sizeof(rate), "%.3f MB/s %.0f Mbps\n",
        1e-06 * (double)ffi->totrb / dt, 8e-06 * (double)ffi->totrb / dt);
    vdiftrace(-1, VDT("clos[%d] %s"), ffi->fh, rate);
    vproc_update_file(ffi, "read-rate", rate, VPROC_TRUNCATE);

    vdifuse_marker(VDT("clos"));
    vdifuse_flush_bread("clos");
}

/*
 * Release fragments opened; the details depend on stype.
 */
int do_vorr_release(const char *fusepath, FFInfo *ffi)
{
    FFInfo *ffp;
    if (ffi->fh == vorrfd) {
        vdiftrace(-1, VDT("illegal fuse release\n"));
        return(1);
    }
    if (ffi->fh == realfd) {
        vdiftrace(0, VDT("Realpath close %s at %lu\n"), fusepath, ffi->fh);
        ffi->fh = vorrfd;
        ffi->vpendx = 0;
        return(0);
    }
    ffp = &FFIcache[ffi->fh];
    switch(ffp->stype) {
    case VDIFUSE_STYPE_VDIF: release_flat_seq(ffp); break;
    case VDIFUSE_STYPE_SGV2: release_sgv2_seq(ffp); break;
    default:                               return(-EBADF); /* bad file type */
    }
    report_access(ffp, fusepath);
    /* nuke it and destroy caller reference to be really safe */
    memset(ffp, 0, sizeof(FFInfo));
    ffi->fh = vorrfd;
    return(0);
}

/*
 * This is the realpath case where we just use pread() to get
 * the bytes, and don't bother with open/close activities.
 *
 * pread() returns the number of bytes read.
 *
 * nonzero ffi->vpendx flags something in the vproc directory
 * area that will be handled by vproc_read().
 */
static int read_realpath(const char *fusepath, char *buf, FFInfo *ffi)
{
    char *real_path = fusepath_to_realpath(fusepath, ffi);
    int res, fd;
    if (ffi->vpendx) {
        res = vproc_read(buf, ffi);
        if (res == -1) res = -ENOENT;
    } else if (real_path) {
        fd = open(real_path, O_RDONLY);
        if (fd == -1) {
            perror("read_realpath:open");
            return -errno;
        }
        res = pread(fd, buf, ffi->size, ffi->offset);
        if (res == -1) {
            perror("read_realpath:pread");
            res = -errno;
        }
        close(fd);
    } else {
        res = -ENOENT;
    }
    return(res);
}

/*
 * Switch in to the sequence handlers.
 */
int do_vorr_read(const char *fpath, char *buf, FFInfo *ffi)
{
    static unsigned long cnt = 0;
    static long max_read = 0, hlf_read = 0;
    int res;
    FFInfo *ffp;
    if (ffi->fh == vorrfd) return(-EBADF);  /* should not happen */
    if (ffi->fh == realfd) return(read_realpath(fpath, buf, ffi));
    ffp = &FFIcache[ffi->fh];
    if (ffp->vffers) return(-ESPIPE);   /* read denied */
    ffp->offset = ffi->offset;
    ffp->size = ffi->size;
    memset(buf, 0xE, ffp->size);
    switch(ffp->stype) {
    case VDIFUSE_STYPE_VDIF: res = read_flat_seq(buf, ffp); break;
    case VDIFUSE_STYPE_SGV2: res = read_sgv2_seq(buf, ffp); break;
    default:                 res = -ENOENT;                 break;
    }
    /* not sure if we get here */
    if (vdsig_trap_rv) vdiftrace(-1, VDT("vdsig: WTF %d\n"), vdsig_trap_rv);
    /* empirically 32*4096 = 131072 is the kernel max_read on current Mark6 */
    if (res > 0) ffp->totrb += res;
    if (max_read == 0) max_read = 32 * sysconf(_SC_PAGESIZE);
    if (hlf_read == 0) hlf_read = 16 * sysconf(_SC_PAGESIZE);
    /* provide a trace only on a-typical reads */
    if (!( (ffp->size == res) || (res == max_read) || (res == hlf_read) ))
        vdiftrace(-1, VDT("Read[%d] %lu:%lu %d, %lu %lu\n"),
            ffp->fh, ffp->offset, ffp->size, res, cnt, ffp->totrb);
    if (res < 0) {
        /* prevent further access */
        ffp->vffers |= VDIFUSE_FFIERROR_READ;
        vdiferror(ffp,
            VDT(">>>Read FAULT, sys error: %d (%x) %lu:%lu\n"),
            res, ffp->vffers, ffp->offset, ffp->size);
    } else if (res < ffp->size) {
        vdiftrace(3, VDT(">>>Read %d < %lu\n"), res, ffp->size);
    } else {
        vdiftrace(3, VDT(">>>Read[%lu] %d from %lu\n"), cnt, res, ffp->fh);
    }
    cnt++;
    return(res);
}

/*
 * The following functions put mutexes on certain operations.
 * ffi is a temporary structure for passing fh and flags.
 */
#include <pthread.h>

/*
 * The vdsig_*() routines trap SIGBUS for mmap errors on read to
 * bad disk areas.  The volatile vdsig_trap_rv will be set to
 * an error condition (for return), and we do not then call the
 * work routines as we already did (see getcontext/setcontext).
 */

int vorr_open(const char *fusepath, FFInfo *ffi)
{
    static pthread_mutex_t vdifuse_mutex = PTHREAD_MUTEX_INITIALIZER;
    int res;
    pthread_mutex_lock(&vdifuse_mutex);
    vdsig_open_prep(ffi);
    res = (vdsig_trap_rv) ? vdsig_trap_rv : do_vorr_open(fusepath, ffi);
    vdsig_open_done(ffi, res);
    pthread_mutex_unlock(&vdifuse_mutex);
    return(res);
}

int vorr_read(const char *fpath, char *buf, FFInfo *ffi)
{
    static pthread_mutex_t vdifuse_mutex = PTHREAD_MUTEX_INITIALIZER;
    int res;
    pthread_mutex_lock(&vdifuse_mutex);
    vdsig_read_prep(&FFIcache[ffi->fh]);
    res = (vdsig_trap_rv) ? vdsig_trap_rv : do_vorr_read(fpath, buf, ffi);
    vdsig_read_done(&FFIcache[ffi->fh], res);
    pthread_mutex_unlock(&vdifuse_mutex);
    return(res);
}

int vorr_release(const char *fusepath, FFInfo *ffi)
{
    static pthread_mutex_t vdifuse_mutex = PTHREAD_MUTEX_INITIALIZER;
    int res;
    pthread_mutex_lock(&vdifuse_mutex);
    vdsig_release_prep(&FFIcache[ffi->fh]);
    res = (vdsig_trap_rv) ? vdsig_trap_rv : do_vorr_release(fusepath, ffi);
    vdsig_release_done(&FFIcache[ffi->fh], res);
    pthread_mutex_unlock(&vdifuse_mutex);
    return(res);
}

/*
 * eof
 */
