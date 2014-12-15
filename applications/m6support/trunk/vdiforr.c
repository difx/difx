/*
 * $Id: vdiforr.c 2392 2014-08-19 20:09:07Z gbc $
 *
 * This file provides support for the fuse interface.
 * This version is rather primitive in many respects.
 * This file support open, read and release operations.
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>

#include "vdifuse.h"

/*
 * In order to maintain information across oper/read/release
 * operations, we maintain our own table of FFInfo data, indexed
 * by one of the open file descriptors (ffi->fh).
 *
 * realfd is a special value to flag realpath (fragments)
 * vorrfd is a special value to flag errors
 */
static FFInfo *FFIcache;
static int realfd;
int vorrfd;

/*
 * The number of available file descriptors isn't known until runtime,
 * so we here allocate and initialize a table to hold open fds.
 *
 * TODO: there are alternatives to vdflog and stderr but this is adequate.
 */
int vorr_init(void)
{
    long open_max = sysconf(_SC_OPEN_MAX);
    if (open_max < 0) return(fprintf(stderr, "OPEN_MAX not available\n"));
    if (vdifuse_debug>3) fprintf(vdflog, "OPEN_MAX = %d\n", open_max);
    FFIcache = (FFInfo *)calloc(open_max, sizeof(FFInfo));
    if (!FFIcache) return(perror("vorr_init"),2);
    realfd = fileno(vdflog);
    vorrfd = fileno(stderr);
    if (realfd < 0 || vorrfd < 0 || realfd == vorrfd)
        return(fprintf(stderr, "issues with fd for realpaths or errors\n"));
    return(0);
}

/*
 * Lookup a real path by fuse path.  This is appropriate for
 * fragments, where there is a real path to the fragment.
 *
 * The last path translation is remembered for efficiency.
 */
static char *fusepath_to_realpath(const char *fusepath)
{
    static uint32_t last = VDIFUSE_TOPDIR_PARAMS;
    uint32_t ii, vd_num_entries = current_cache_entries();
    VDIFUSEntry *vd_cache = current_cache_start();
    if (!strcmp(fusepath, vd_cache[last].path))
        return(vd_cache[last].path);
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
 *
 * For fragments, the fusepath has a corresponding realpath, so
 * we use pread to just provide the required bytes; there is no
 * history, so we use ffi->fh == 0 for this.  For sequences,
 * we call the appropriate methods to open/release the fragments.
 *
 * ffi->fh should be copied out to the fuse kernel.
 *
 * TODO: restrict ffi->flags to, e.g. read-only
 */
int do_vorr_open(const char *fusepath, FFInfo *ffi)
{
    VDIFUSEntry *vs;
    if(fusepath_to_realpath(fusepath)) {
        ffi->fh = realfd;
        if (vdifuse_debug>0) fprintf(vdflog,
            "Realpath open %s at %d\n", fusepath, ffi->fh);
        return(0);
    }
    ffi->sindex = fusepath_to_index(fusepath);
    if (ffi->sindex == VDIFUSE_TOPDIR_PARAMS) {
        if (vdifuse_debug>0) fprintf(vdflog,
            "No fuse path for %s\n", fusepath);
        return(-ENOENT);
    }
    gettimeofday(&(ffi->topen), 0);
    ffi->totrb = 0UL;
    vs = current_cache_start() + ffi->sindex;
    switch (vs->stype) {
    case VDIFUSE_STYPE_VDIF: ffi->fh = open_flat_seq(vs, ffi); break;
    case VDIFUSE_STYPE_SGV2: ffi->fh = open_sgv2_seq(vs, ffi); break;
    default:                 ffi->fh = vorrfd;                 break;
    }
    if (ffi->fh != vorrfd) {
        if (vdifuse_debug>0) fprintf(vdflog,
            "Storing data for open %s at ffi[%d]\n", fusepath, ffi->fh);
        FFIcache[ffi->fh] = *ffi;
        return(0);
    }
    return(-ENOENT);
}

/*
 * For a global diagnostic
 */
static void report_access(FFInfo *ffi)
{
    struct timeval now;
    double dt;
    gettimeofday(&now, 0);
    dt = (double)(now.tv_sec - ffi->topen.tv_sec) +
         1e-6 * (double)(now.tv_usec - ffi->topen.tv_usec);
    if (vdifuse_debug>0) fprintf(vdflog,
        "Accessed %lu bytes in %.3f s, rate = %.f MB/s\n",
            ffi->totrb, dt, 1e-06 * (double)ffi->totrb / dt);
    if (vdifuse_debug>1) fprintf(vdflog,
        "Clearing stored data for %s at fh %d\n",
            (current_cache_start() + ffi->sindex)->fuse, ffi->fh);
}

/*
 * Release fragments opened; the details depend on stype.
 */
int do_vorr_release(const char *fusepath, FFInfo *ffi)
{
    if (ffi->fh == vorrfd) return(fprintf(stderr, "illegal fuse release\n"));
    if (ffi->fh == realfd) {
        if (vdifuse_debug>0) fprintf(vdflog,
            "Realpath close %s at %d\n", fusepath, ffi->fh);
        ffi->fh = vorrfd;
        return(0);
    }
    switch(FFIcache[ffi->fh].stype) {
    case VDIFUSE_STYPE_VDIF: release_flat_seq(&FFIcache[ffi->fh]); break;
    case VDIFUSE_STYPE_SGV2: release_sgv2_seq(&FFIcache[ffi->fh]); break;
    default:                                              return(-EBADF);
    }
    report_access(&FFIcache[ffi->fh]);
    /* nuke it just to be safe for next time */
    memset(&FFIcache[ffi->fh], 0, sizeof(FFInfo));
    ffi->fh = vorrfd;
    return(0);
}

/*
 * This is the realpath case where we just use pread() to get
 * the bytes, and don't bother with open/close activities.
 *
 * pread() returns the number of bytes read.
 */
static int read_realpath(const char *fusepath, char *buf, FFInfo *ffi)
{
    char *realpath = fusepath_to_realpath(fusepath);
    int res, fd;
    if (realpath) {
        fd = open(realpath, O_RDONLY);
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
static int do_vorr_read(const char *fpath, char *buf, FFInfo *ffi)
{
    static unsigned long cnt = 0;
    static uint64_t bogus = 0xEEEEEEEEEEEEEEEEUL;
    int res;
    FFInfo *ffp;
    if (ffi->fh == vorrfd) return(fprintf(stderr, "illegal fuse read\n"));
    if (ffi->fh == realfd) return(read_realpath(fpath, buf, ffi));
    ffp = &FFIcache[ffi->fh];
    ffp->offset = ffi->offset;
    ffp->size = ffi->size;
    memset(buf, 0xE, ffp->size);
    switch(ffp->stype) {
    case VDIFUSE_STYPE_VDIF: res = read_flat_seq(buf, ffp); break;
    case VDIFUSE_STYPE_SGV2: res = read_sgv2_seq(buf, ffp); break;
    default:                 res = -ENOENT;                 break;
    }
    if (res == -EIO) {
        fprintf(stderr, ">>>Read EIO\n");
        exit(1);
    }
    if (res < 0) fprintf(stderr, ">>>Read FAULT\n");
    if (vdifuse_debug>2 && res < ffi->size) fprintf(vdflog,
        ">>>Read %d < %d\n", res, ffi->size);
    if (*(uint64_t*)buf == bogus) fprintf(stderr,
        ">>>Read bogus at %lu:%lu\n", ffi->offset, ffi->size);
    if (vdifuse_debug>3) fprintf(vdflog,
        ">>>Read[%lu] %d into buffer from %d\n", cnt, res, ffi->fh);
    cnt++;
    return(res);
}

/*
 * Put mutexes on all of these.
 */
#include <pthread.h>

int vorr_open(const char *fusepath, FFInfo *ffi)
{
    static pthread_mutex_t vdifuse_mutex = PTHREAD_MUTEX_INITIALIZER;
    int res;
    pthread_mutex_lock(&vdifuse_mutex);
    do_vorr_open(fusepath, ffi);
    pthread_mutex_unlock(&vdifuse_mutex);
    return(res);
}

int vorr_read(const char *fpath, char *buf, FFInfo *ffi)
{
    static pthread_mutex_t vdifuse_mutex = PTHREAD_MUTEX_INITIALIZER;
    int res;
    pthread_mutex_lock(&vdifuse_mutex);
    res = do_vorr_read(fpath, buf, ffi);
    pthread_mutex_unlock(&vdifuse_mutex);
    return(res);
}

int vorr_release(const char *fusepath, FFInfo *ffi)
{
    static pthread_mutex_t vdifuse_mutex = PTHREAD_MUTEX_INITIALIZER;
    int res;
    pthread_mutex_lock(&vdifuse_mutex);
    do_vorr_release(fusepath, ffi);
    pthread_mutex_unlock(&vdifuse_mutex);
    return(res);
}

/*
 * eof
 */
