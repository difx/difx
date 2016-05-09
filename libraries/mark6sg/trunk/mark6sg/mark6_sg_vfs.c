/***************************************************************************
 *   Copyright (C) 2014 by Jan Wagner                                      *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#define _GNU_SOURCE
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <glob.h>
#include <linux/limits.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "mark6_sg_vfs.h"

// Prefetch (via mmap() MAP_POPULATE-like threaded forced kernel page faults)
#define USE_MMAP_POPULATE_LIKE_PREFETCH 1  // 1 to enable, 0 to disable
#define PREFETCH_NUM_BLOCKS_PER_FILE    4

// Internal structs
typedef struct touch_blocks_ctx_tt {
    pthread_t tid;
    void*     vfd;
    int       file_id;
} touch_blocks_ctx_t;

typedef struct m6sg_virt_filedescr {
    int       valid;
    int       eof;
    size_t    len;
    off_t     rdoffset;
    size_t    rdblock;
    int       nfiles;
    char**    filepathlist;
    char**    filenamelist; // (unused, although populated)
    int       fds[MARK6_SG_MAXFILES];
    void*     fmmap[MARK6_SG_MAXFILES];
    off_t     fsize[MARK6_SG_MAXFILES];
    size_t    nblocks;
    m6sg_blockmeta_t* blks;
    touch_blocks_ctx_t touch_ctxs[MARK6_SG_MAXFILES];
    volatile int       touch_terminate;
} m6sg_virt_filedescr_t;

typedef struct m6sg_virt_open_files {
    int nopen;
    m6sg_virt_filedescr_t** fd_list;
} m6sg_virt_open_files_t;

static pthread_mutex_t vfs_lock = PTHREAD_MUTEX_INITIALIZER;

static m6sg_virt_open_files_t m6sg_open_files_list = { 0, NULL };

void* touch_next_blocks_thread(void* p_ctx);

//////////////////////////////////////////////////////////////////////////////////////
////// Library Functions /////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////

/**
 * Open a scatter-gather recording for reading.
 * Similar behaviour as 'man 2 open'.
 */
int mark6_sg_open(const char *scanname, int flags)
{
    m6sg_virt_filedescr_t* vfd;
    struct stat st;
    int fd, i;

    // Ignore prefixed path
    if (strrchr(scanname, '/') != NULL)
    {
        scanname = strrchr(scanname, '/') + 1;
    }

    // Catch some error conditions
    if ((NULL == scanname) || (strlen(scanname)<=2))
    {
        errno = EACCES;
        return -1;
    }
    if ((flags & O_TRUNC) || (flags & O_RDWR) || (flags & O_WRONLY))
    {
        errno = EROFS;
        return -1;
    }

    // Grab global lock
    pthread_mutex_lock(&vfs_lock);

    // Allocate memory for full set of 'fd's if not allocated yet
    if (NULL == m6sg_open_files_list.fd_list)
    {
        m6sg_open_files_list.fd_list = (m6sg_virt_filedescr_t**) malloc(MARK6_SG_VFS_MAX_OPEN_FILES*sizeof(m6sg_virt_filedescr_t*));
        for (fd=0; fd<MARK6_SG_VFS_MAX_OPEN_FILES; fd++)
        {
            m6sg_open_files_list.fd_list[fd] = (m6sg_virt_filedescr_t*) malloc(sizeof(m6sg_virt_filedescr_t));
            memset(m6sg_open_files_list.fd_list[fd], 0x00, sizeof(m6sg_virt_filedescr_t));
        }
    }

    // Find next free 'fd'
    if (m6sg_open_files_list.nopen >= MARK6_SG_VFS_MAX_OPEN_FILES)
    {
        errno = ENOMEM;
        pthread_mutex_unlock(&vfs_lock);
        return -1;
    }
    for (fd=0; fd<MARK6_SG_VFS_MAX_OPEN_FILES; fd++)
    {
        if (m6sg_open_files_list.fd_list[fd]->valid == 0)
        {
            break;
        }
    }
    assert(fd < MARK6_SG_VFS_MAX_OPEN_FILES);
    m6sg_open_files_list.nopen++;

    // Prepare file descriptor
    vfd = m6sg_open_files_list.fd_list[fd];
    memset((void*)vfd, 0, sizeof(m6sg_virt_filedescr_t));
    vfd->valid = 1;
    pthread_mutex_unlock(&vfs_lock);

    // Gather all metadata about the scan's files
    vfd->nfiles  = mark6_sg_filelist_from_name(scanname, &(vfd->filepathlist), &(vfd->filenamelist));
    vfd->nblocks = mark6_sg_blocklist(vfd->nfiles, (const char**)(vfd->filepathlist), &(vfd->blks));
    if (vfd->nfiles <= 0)
    {
        mark6_sg_close(fd);
        errno = ENOENT;
        return -1;
    }

    for (i=0; i< vfd->nfiles; i++)
    {
        int flags;

        stat(vfd->filepathlist[i], &st);
        vfd->fsize[i] = st.st_size;

        vfd->fds[i] = open(vfd->filepathlist[i], O_RDONLY|O_LARGEFILE);
        if (vfd->fds[i] < 0)
        {
            perror(vfd->filepathlist[i]);
            vfd->fmmap[i] = NULL; // segfault later
            continue;
        }

        flags = MAP_PRIVATE;
        flags |= MAP_NORESERVE;
        // flags |= MAP_POPULATE; // terribly slow since entire(?) file is read first, blocking
        // flags |= MAP_HUGETLB;  // not for mmap() of normal files
        vfd->fmmap[i] = mmap(NULL, vfd->fsize[i], PROT_READ, flags, vfd->fds[i], 0);
        if (vfd->fmmap[i] == MAP_FAILED)
        {
            printf("mmap error for %s (fd=%d) of size %zu : ", vfd->filepathlist[i], vfd->fds[i], vfd->fsize[i]);
            perror("mmap");
        }

        if (m_m6sg_dbglevel>1) { printf("mmap() ufd=%d file %2d to virtual addr %p\n", fd, i, vfd->fmmap[i]); }

#if (USE_MMAP_POPULATE_LIKE_PREFETCH == 0)
        flags = 0;
        flags |= MADV_SEQUENTIAL;
        // flags |= MADV_WILLNEED; // very slow yet somewhat faster than MAP_POPULATE
        madvise(vfd->fmmap[i], vfd->fsize[i], flags);
        if (m_m6sg_dbglevel>1) { printf("madvise() file %2d\n", i); }
#endif

    }

    // Total scan length
    if (vfd->nblocks > 0)
    {
        vfd->len = vfd->blks[vfd->nblocks-1].virtual_offset + vfd->blks[vfd->nblocks-1].datalen;
    }

    // Trigger a preload of future mmap()'ed data in the background
#if (USE_MMAP_POPULATE_LIKE_PREFETCH != 0)
    vfd->touch_terminate = 0;
    for (i=0; i < vfd->nfiles; i++)
    {
        vfd->touch_ctxs[i].vfd = vfd;
        vfd->touch_ctxs[i].file_id = i;
        pthread_create(&(vfd->touch_ctxs[i].tid), NULL, touch_next_blocks_thread, &(vfd->touch_ctxs[i]));
    }
#endif

    return fd;
}


/**
 * Close a scatter-gather file.
 * Similar behaviour as 'man 2 close'.
 */
int mark6_sg_close(int fd)
{
    m6sg_virt_filedescr_t* vfd;
    int i;

    // Catch some error conditions
    if ((fd < 0) || (fd >= MARK6_SG_VFS_MAX_OPEN_FILES))
    {
        errno = EBADF;
        return -1;
    }

    pthread_mutex_lock(&vfs_lock);
    vfd = m6sg_open_files_list.fd_list[fd];
    if (!vfd->valid)
    {
        pthread_mutex_unlock(&vfs_lock);
        errno = EBADF;
        return -1;
    }

#if (USE_MMAP_POPULATE_LIKE_PREFETCH != 0)
    vfd->touch_terminate = 1;
    for (i=0; i < vfd->nfiles; i++)
    {
        if (m_m6sg_dbglevel>1) { printf("join() on prefetch thread %d\n", i); }
        pthread_join(vfd->touch_ctxs[i].tid, NULL);
    }
#endif

    for (i=0; i < vfd->nfiles; i++)
    {
        free(vfd->filepathlist[i]);
        free(vfd->filenamelist[i]);
        if (vfd->fmmap[i] != NULL)
        {
            if (m_m6sg_dbglevel>1) { printf("munmap() ufd=%d file %2d virtual addr %p\n", fd, i, vfd->fmmap[i]); }
            munmap(vfd->fmmap[i], vfd->fsize[i]);
            vfd->fmmap[i] = NULL;
        }
        close(vfd->fds[i]);
    }
    free(vfd->filepathlist);
    free(vfd->filenamelist);
    free(vfd->blks);
    memset((void*)vfd, 0, sizeof(m6sg_virt_filedescr_t));

    m6sg_open_files_list.nopen--;
    pthread_mutex_unlock(&vfs_lock);

    return 0;
}


/**
 * Read a piece of data from a scattered file.
 * Similar behaviour as 'man 2 read'.
 */
ssize_t mark6_sg_read(int fd, void* buf, size_t count)
{
    m6sg_virt_filedescr_t* vfd;
    ssize_t nrd;

    // Catch some error conditions
    if ((fd < 0) || (fd >= MARK6_SG_VFS_MAX_OPEN_FILES))
    {
        errno = EBADF;
        return -1;
    }

    vfd = m6sg_open_files_list.fd_list[fd];
    if (!vfd->valid)
    {
        errno = EBADF;
        return -1;
    }

    // Start read from offset via pread()
    nrd = mark6_sg_pread(fd, buf, count, vfd->rdoffset);
    if (vfd->rdoffset >= vfd->len)
    {
        vfd->eof = 1;
    }

    return nrd;
}

/**
 * Read a piece of data from a scattered file.
 * Similar behaviour as 'man 2 pread'.
 * Nearly thread safe but updates vfd->rdblock
 * and vfd->rdoffset at end of the read.
 */
ssize_t mark6_sg_pread(int fd, void* buf, size_t count, off_t rdoffset)
{
    m6sg_virt_filedescr_t* vfd;
    size_t nread = 0;
    size_t nremain = count;
    size_t blk, prevblk;

    // Catch some error conditions
    if ((fd < 0) || (fd >= MARK6_SG_VFS_MAX_OPEN_FILES))
    {
        errno = EBADF;
        return -1;
    }

    vfd = m6sg_open_files_list.fd_list[fd];
    if (!vfd->valid)
    {
        errno = EBADF;
        return -1;
    }
    if (rdoffset >= vfd->len)
    {
        return 0;
    }

    // Perform read
    blk = vfd->rdblock;
    prevblk = blk;
    while ((nremain > 0) && (rdoffset < vfd->len))
    {
        size_t navail, nskip, nwanted;
        int    fid;
        off_t  foff;
        off_t  blkstart = vfd->blks[blk].virtual_offset;
        off_t  blkstop  = blkstart + vfd->blks[blk].datalen;

        // If current block is wrong, try others until finding correct one
        if (rdoffset < blkstart)
        {
            blk = (blk + (vfd->nblocks - 1)) % vfd->nblocks; // "blk--" with wrap
            continue;
        }
        if (rdoffset >= blkstop)
        {
            blk = (blk + 1) % vfd->nblocks;
            continue;
        }

        nskip   = rdoffset - blkstart;
        navail  = blkstop - rdoffset;
        nwanted = (nremain < navail) ? nremain : navail;

        // Read the data from file on disk
        fid  = vfd->blks[blk].file_id;
        foff = vfd->blks[blk].file_offset + nskip;

        if (m_m6sg_dbglevel>2) { printf("memcpy() file %2d offset %ld : virtual offset %ld : req %ld\n", fid, foff, rdoffset, nwanted); }
        memcpy(buf+nread, ((char*)vfd->fmmap[fid]) + foff, nwanted);

        rdoffset += nwanted;
        nread    += nwanted;
        nremain  -= nwanted;
    }

    // Remember current block nr (not exactly thread safe, but non-critical)
    vfd->rdblock = blk;
    vfd->rdoffset = rdoffset;

    return nread;
}


/**
 * Seek to new read offset.
 * Similar behaviour as 'man 2 lseek'.
 */
off_t mark6_sg_lseek(int fd, off_t offset, int whence)
{
    m6sg_virt_filedescr_t* vfd;

    // Catch some error conditions
    if ((fd < 0) || (fd >= MARK6_SG_VFS_MAX_OPEN_FILES))
    {
        errno = EBADF;
        return -1;
    }

    vfd = m6sg_open_files_list.fd_list[fd];
    if (!vfd->valid)
    {
        errno = EBADF;
        return -1;
    }

    // "Seek"
    switch (whence)
    {
        case SEEK_CUR:
            vfd->rdoffset += offset;
            break;
        case SEEK_END:
            vfd->rdoffset = vfd->len - offset;
            break;
        case SEEK_SET:
        default:
            vfd->rdoffset = offset;
            break;
    }

    // Clip negative offsets
    vfd->rdoffset = (vfd->rdoffset >= 0) ? vfd->rdoffset : 0;

    return vfd->rdoffset;
}


/**
 * Get scan file information.
 * Date and time are taken from the first file of the scatter-gather fileset,
 * without decoding VDIF/Mark5B/other headers.
 * File size is the virtual size .
 *
 * Similar to 'man 2 fstat'.
 */
int mark6_sg_fstat(int fd, struct stat *buf)
{
    m6sg_virt_filedescr_t* vfd;

    // Catch some error conditions
    if ((fd < 0) || (fd >= MARK6_SG_VFS_MAX_OPEN_FILES) || (buf == NULL))
    {
        errno = EBADF;
        return -1;
    }

    vfd = m6sg_open_files_list.fd_list[fd];
    if (!vfd->valid)
    {
        errno = EBADF;
        return -1;
    }

    // Take basic 'stat' infos from first file
    fstat(vfd->fds[0], buf); // TODO: check return value?

    // Do some adjustments
    buf->st_size = vfd->len;
    buf->st_blksize = (vfd->nblocks > 0) ? vfd->blks[0].packetsize : 0;

    return 0;
}


/**
 * Return the packet size.
 */
int mark6_sg_packetsize(int fd)
{
    m6sg_virt_filedescr_t* vfd;

    // Catch some error conditions
    if ((fd < 0) || (fd >= MARK6_SG_VFS_MAX_OPEN_FILES))
    {
        errno = EBADF;
        return -1;
    }

    vfd = m6sg_open_files_list.fd_list[fd];
    if (!vfd->valid)
    {
        errno = EBADF;
        return -1;
    }

    if (vfd->nblocks > 0)
    {
        return vfd->blks[0].packetsize;
    }
    return 0;
}


/**
 * Return the stripe size (without metadata length).
 */
ssize_t mark6_sg_stripesize(int fd)
{
    m6sg_virt_filedescr_t* vfd;

    // Catch some error conditions
    if ((fd < 0) || (fd >= MARK6_SG_VFS_MAX_OPEN_FILES))
    {
        errno = EBADF;
        return -1;
    }

    vfd = m6sg_open_files_list.fd_list[fd];
    if (!vfd->valid)
    {
        errno = EBADF;
        return -1;
    }

    if (vfd->nblocks > 0)
    {
        return (ssize_t)(vfd->blks[0].datalen);
    }

    return 0;
}

//////////////////////////////////////////////////////////////////////////////////////
////// Local Functions ///////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////

void* touch_next_blocks_thread(void* p_ctx)
{
    size_t blk = 0, blk_prev = -1;
    off_t  off, off_stop;
    int    pagesz = getpagesize();

    touch_blocks_ctx_t*    ctx = (touch_blocks_ctx_t*)p_ctx;
    m6sg_virt_filedescr_t* vfd = (m6sg_virt_filedescr_t*)(ctx->vfd);

    char* fdata = (char*)(vfd->fmmap[ctx->file_id]);

    if (m_m6sg_dbglevel>2) { fprintf(stderr, "START thread for file %2d inc %d\n", ctx->file_id, pagesz); }

    // Run until terminated
    while (!vfd->touch_terminate)
    {

        // Thread is dedicated to one file of the scatter-gather fileset,
        // and touches a certain number of blocks that follow the current
        // read position (FUSE virtual -> file offset) in the file.

        for (blk = vfd->rdblock; blk < vfd->nblocks; blk++)
        {
            if ((vfd->blks[blk].file_id == ctx->file_id)) { break; }
        }
        if (blk >= vfd->nblocks)
        {
            usleep(1000);
            continue;
        }
        assert(vfd->blks[blk].file_id == ctx->file_id);

        // Check if current read position (block) has changed.
        // Note: checking if blk falls inside already pre-fetched window is for
        //       some reason slower than just re-reading almost all blocks
        if (blk == blk_prev)
        {
            usleep(10);
            continue;
        }
        blk_prev = blk;

        // Do not care about block numbers; just touch the next blocks in current file
        off      = vfd->blks[blk].file_offset;
        off     &= ~((off64_t)(pagesz - 1));
        off_stop = off + vfd->blks[blk].datalen * PREFETCH_NUM_BLOCKS_PER_FILE;
        off_stop = (off_stop > vfd->fsize[ctx->file_id]) ? vfd->fsize[ctx->file_id] : off_stop;
        while (off < off_stop)
        {
            // Reference the data to cause page fault and a kernel
            // fetch of missing page from underlying mmap()'ed file.
            // Data is not actually used here. The pages cached by
            // kernel will however be available in future read() calls.
            char dummy = fdata[off];
            off += pagesz;
            if (m_m6sg_dbglevel>99) { printf("(printf to prevent optimizing away 'dummy') %c", dummy); }
        }

    }

    if (m_m6sg_dbglevel>2) { fprintf(stderr, "EXIT thread for file %2d\n", ctx->file_id); }

    return NULL;
}
