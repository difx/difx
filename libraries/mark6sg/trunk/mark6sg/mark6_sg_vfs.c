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
#include "mark6sg.h"
#include "mark6_sg_vfs_impl.h"
#include "mark6_sg_format.h"

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h> // clock_gettime(), TODO need alternative for OS X
#include <unistd.h>
#include <sys/mman.h>
#include <sys/time.h>

#if HAVE_CONFIG_H
    #include "config.h"
#endif

#if HAVE_DIFXMESSAGE
    #include <difxmessage.h>
    #include <unistd.h>
#endif

#if HAVE_MMSG && __linux__
    #include <netinet/ip.h>
    #include <sys/socket.h>
#endif

// Prefetch (via mmap() MAP_POPULATE-like threaded forced kernel page faults)
#define USE_MMAP_POPULATE_LIKE_PREFETCH 1  // 1 to enable, 0 to disable
#define PREFETCH_NUM_BLOCKS_PER_FILE    4
#define WRITER_FRAMES_PER_BLOCK         5000

// When disks have unrelocatable bad sectors that cause I/O errors, mmap()'ed regions cause SIGBUS, whereas fread() returns a handleable error
#define AVOID_MMAP                      0  // 1 to use fread() file access instead of mapped memory access to read file data

static pthread_mutex_t vfs_lock = PTHREAD_MUTEX_INITIALIZER;

static m6sg_virt_open_files_t m6sg_open_files_list = { 0, NULL };

static int vdif_packetsize_hint = 10000 + 32;

static void* touch_next_blocks_thread(void* p_ioctx);
static void* writer_thread(void* p_ioctx);

void ioerror_noop_handler(int sig)
{
    // catch SIGBUS but do nothing
}

//////////////////////////////////////////////////////////////////////////////////////
////// Library Functions /////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////

/** Init on library load */
__attribute__((constructor)) void init(void)
{
#if HAVE_DIFXMESSAGE
    char hostname[DIFX_MESSAGE_LENGTH];
    gethostname(hostname, sizeof(hostname)-1);
    difxMessageInitFull(-1, "libmark6sg", hostname);
    difxMessageSendDifxAlert("libmark6sg started", DIFX_ALERT_LEVEL_INFO);
    //difxMessagePrint();
#endif
}

/**
 * Open a scatter-gather recording for reading.
 * Similar behaviour as 'man 2 open'.
 */
int mark6_sg_open(const char *scanname, int flags)
{
    m6sg_virt_filedescr_t* vfd;
    struct stat st;
    int fd, i;

    // Fragment files are mmap()'ed so read errors (disk errors) result in SIGBUS, catch it.
    signal(SIGBUS, ioerror_noop_handler);

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
    vfd->mode = O_RDONLY;
    snprintf(vfd->scanname, sizeof(vfd->scanname)-1, "%s", scanname);
    pthread_mutex_init(&vfd->lock, NULL);
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

    for (i = 0; i< vfd->nfiles; i++)
    {
        int flags;

        stat(vfd->filepathlist[i], &st);
        vfd->fsize[i] = st.st_size;

        vfd->fds[i] = open(vfd->filepathlist[i], O_RDONLY|O_LARGEFILE);
        if (vfd->fds[i] < 0)
        {
            perror(vfd->filepathlist[i]);
            vfd->fmmap[i] = MAP_FAILED; // segfault later
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
    for (i = 0; i < vfd->nfiles; i++)
    {
        vfd->touch_ctxs[i].vfd = vfd;
        vfd->touch_ctxs[i].file_id = i;
        pthread_create(&(vfd->touch_ctxs[i].tid), NULL, touch_next_blocks_thread, &(vfd->touch_ctxs[i]));
    }
#endif

#if HAVE_DIFXMESSAGE
    if (1) {
        DifxMessageMark6Activity m6act;
        memset(&m6act, 0x00, sizeof(m6act));
        m6act.state = MARK6_STATE_OPEN;
        snprintf(m6act.scanName, sizeof(m6act.scanName)-1, "%s", vfd->scanname);
        difxMessageSendMark6Activity(&m6act);
    }
#endif

    return fd;
}

/**
 * Create a scatter-gather file.
 * Similar behaviour as 'man 2 creat', except that an
 * error EEXIST is returned if the file already exists.
 */
int mark6_sg_creat(const char *scanname, mode_t ignored)
{
    const size_t default_framesize = vdif_packetsize_hint;
    m6sg_virt_filedescr_t* vfd;
    int fd, i;

    // Ignore any prefixed path
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
    vfd->valid     = 1;
    vfd->mode      = O_WRONLY;
    vfd->nblocks   = 0;
    vfd->len       = 0;
    vfd->framesize = default_framesize;
    pthread_mutex_init(&vfd->lock, NULL);
    pthread_mutex_unlock(&vfs_lock);

    // Name of fragment files
    vfd->nfiles = mark6_sg_filelist_for_new(scanname, &(vfd->filepathlist), &(vfd->filenamelist));
    if (vfd->nfiles <= 0)
    {
        mark6_sg_close(fd);
        errno = EEXIST; // TODO: if rootpattern matched no directories (then too nfiles=0!) return ENOENT
        return -1;
    }

    // Prepare writer pool infos
    vfd->writer_pool.inputareasize = vfd->framesize*WRITER_FRAMES_PER_BLOCK;
    pthread_cond_init(&vfd->writer_pool.any_inputarea_done, NULL);

    // Create fragment files
    for (i = 0; i < vfd->nfiles; i++)
    {
        struct file_header_tag hdr;

        // Defaults
        vfd->fsize[i] = 0;
        vfd->fmmap[i] = MAP_FAILED;

        // Create
        vfd->fds[i] = open(vfd->filepathlist[i], O_RDWR|O_LARGEFILE|O_CREAT|O_EXCL, S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH);
        if (vfd->fds[i] < 0)
        {
            int e = errno;
            perror(vfd->filepathlist[i]);
            return -e;
            //continue;
        }

        // Prepare the input buffer and associated disk-writer for this file
        vfd->writer_ctxs[i].vfd = vfd;
        vfd->writer_ctxs[i].file_id = i;
        vfd->writer_pool.inputarea_writeout_complete[i] = 1;
        posix_memalign((void**)&vfd->writer_pool.inputareas[i], getpagesize(), vfd->writer_pool.inputareasize);
        pthread_mutex_init(&vfd->writer_pool.inputarea_mutex[i], NULL);
        pthread_cond_init(&vfd->writer_pool.inputarea_newdata_cond[i], NULL);
        pthread_create(&vfd->writer_ctxs[i].tid, NULL, writer_thread, &(vfd->writer_ctxs[i]));

        // Write primary header
        hdr.sync_word     = MARK6_SG_SYNC_WORD;
        hdr.packet_size   = vfd->framesize; // use default block_size and packet_size in file header, actual ones in each block's header
        hdr.block_size    = vfd->framesize*WRITER_FRAMES_PER_BLOCK;
        hdr.block_size   += sizeof(wb_header_tag_v2_t);
        hdr.version       = 2;
        hdr.packet_format = 0;  // 0:vdif, 1:mark5, 2:unknown
        write(vfd->fds[i], &hdr, sizeof(hdr));

    }

    return fd;
}


/**
 * Close a scatter-gather file.
 * Similar behaviour as 'man 2 close'.
 *
 * In write mode any remaining data (written data that
 * does not fill a complete Mark6 sg block) are discarded.
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

#if HAVE_DIFXMESSAGE
    if (1) {
        DifxMessageMark6Activity m6act;
        memset(&m6act, 0x00, sizeof(m6act));
        m6act.state = MARK6_STATE_CLOSE;
        snprintf(m6act.scanName, sizeof(m6act.scanName)-1, "%s", vfd->scanname);
        difxMessageSendMark6Activity(&m6act);
    }
#endif

    if (vfd->mode == O_RDONLY)
    {
#if (USE_MMAP_POPULATE_LIKE_PREFETCH != 0)
        vfd->touch_terminate = 1;
        for (i = 0; i < vfd->nfiles; i++)
        {
            if (m_m6sg_dbglevel>1) { printf("join() on prefetch thread %d\n", i); }
            pthread_join(vfd->touch_ctxs[i].tid, NULL);
        }
#endif
    } 
    else if (vfd->mode == O_WRONLY)
    {
        vfd->writers_terminate = 1;
        for (i = 0; i < vfd->nfiles; i++)
        {
            // Wake the thread up to write out any pending/partial blocks
            pthread_mutex_lock(&vfd->writer_pool.inputarea_mutex[i]);
            vfd->writer_pool.inputarea_newdata_pushed[i] = 1;
            pthread_mutex_unlock(&vfd->writer_pool.inputarea_mutex[i]);
            pthread_cond_signal(&vfd->writer_pool.inputarea_newdata_cond[i]);

            // Wait for that thread to die
            pthread_join(vfd->writer_ctxs[i].tid, NULL);
            if (vfd->writer_pool.inputareas[i] != NULL)
            {
                free(vfd->writer_pool.inputareas[i]);
                vfd->writer_pool.inputareas[i] = NULL;
            }
            pthread_cond_destroy(&vfd->writer_pool.inputarea_newdata_cond[i]);
            pthread_mutex_destroy(&vfd->writer_pool.inputarea_mutex[i]);
        }
        pthread_cond_destroy(&vfd->writer_pool.any_inputarea_done);
    }

    for (i = 0; i < vfd->nfiles; i++)
    {
        free(vfd->filepathlist[i]);
        free(vfd->filenamelist[i]);
        if ((vfd->fmmap[i] != NULL) && (vfd->fmmap[i] != MAP_FAILED))
        {
            if (m_m6sg_dbglevel>1) { printf("munmap() ufd=%d file %2d virtual addr %p\n", fd, i, vfd->fmmap[i]); }
            munmap(vfd->fmmap[i], vfd->fsize[i]);
            vfd->fmmap[i] = MAP_FAILED;
        }
        close(vfd->fds[i]);
    }
    pthread_mutex_destroy(&vfd->lock);
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
    if (!vfd->valid || (vfd->mode != O_RDONLY))
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
    if (!vfd->valid || (vfd->mode != O_RDONLY))
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

#if !AVOID_MMAP
        if (m_m6sg_dbglevel>2) { printf("memcpy() file %2d offset %ld : virtual offset %ld : req %ld\n", fid, foff, rdoffset, nwanted); }
        memcpy(buf+nread, ((char*)vfd->fmmap[fid]) + foff, nwanted);
#else
        if (m_m6sg_dbglevel>2) { printf("pread() file %2d offset %ld : virtual offset %ld : req %ld\n", fid, foff, rdoffset, nwanted); }
        size_t pread_ngot = 0;
        while (pread_ngot < nwanted) {
            ssize_t nrd = pread(vfd->fds[fid], buf+nread+pread_ngot, nwanted-pread_ngot, foff+pread_ngot);
            if (nrd <= 0) {
                printf("pread() file %2d <%s> offset %ld : underlying file returned error %d (%s)\n", fid, vfd->filepathlist[fid], foff+pread_ngot, errno, strerror(errno));
                break;
            }
            pread_ngot += nrd;
        }
#endif

        rdoffset += nwanted;
        nread    += nwanted;
        nremain  -= nwanted;
    }

    // Remember current block nr (not exactly thread safe, but non-critical)
    vfd->rdblock = blk;
    vfd->rdoffset = rdoffset;

    // Report the current read position via multicast
#if HAVE_DIFXMESSAGE
    if (1) {
        static off_t prev_reported_offset = 0;
        static struct timeval prev_time;
        int do_report = 0;
        off_t delta_read;

        if (rdoffset < prev_reported_offset) {
            prev_reported_offset = rdoffset;
        }

        delta_read = rdoffset - prev_reported_offset;
        if (delta_read > 1024e6) {
            do_report = 1;
        }

        if (do_report) {
            DifxMessageMark6Activity m6act;
            struct timeval now;
            double dt;

            gettimeofday(&now, NULL);
            dt = (now.tv_sec - prev_time.tv_sec) + 1e-6*(now.tv_usec - prev_time.tv_usec);

            memset(&m6act, 0x00, sizeof(m6act));
            m6act.state = MARK6_STATE_PLAY;
            m6act.position = rdoffset;
            m6act.rate = (delta_read*8e-6)/dt;
            snprintf(m6act.scanName, sizeof(m6act.scanName)-1, "%s", vfd->scanname);
            difxMessageSendMark6Activity(&m6act);

            gettimeofday(&prev_time, NULL);
            prev_reported_offset = rdoffset;
            //prev_time = now;
            //printf("difxmessage PLAY %s rate %.3fMbps off=%zu dn=%zu dt=%.3f\n", vfd->scanname, m6act.rate, rdoffset, delta_read, dt);
        }
    }
#endif

    return nread;
}

/**
 * Write a piece of data to a scattered file set.
 * Similar behaviour as 'man 2 write'.
 */
ssize_t mark6_sg_write(int fd, const void *buf, size_t count)
{
    m6sg_virt_filedescr_t* vfd;
    writer_pool_t* pool;
    ssize_t nwr = 0;
    int idx, rc, n;

    // Catch some error conditions
    if ((fd < 0) || (fd >= MARK6_SG_VFS_MAX_OPEN_FILES))
    {
        errno = EBADF;
        return -1;
    }

    vfd = m6sg_open_files_list.fd_list[fd];
    if (!vfd->valid || (vfd->mode != O_WRONLY))
    {
        errno = EBADF;
        return -1;
    }

    pool = &vfd->writer_pool;

    while (nwr < count)
    {
        // Try the current area first
        idx = pool->active_area;
        assert(pool->inputarea_writeout_complete[idx] == 1);
        assert(pool->inputarea_newdata_pushed[idx] == 0);

        // Detect if write-out is necessary
        if ((pool->inputarea_append_offset[idx] + count) >= pool->inputareasize)
        {
            // Signal the area for a background write-out
            pthread_mutex_lock(&pool->inputarea_mutex[idx]);
            pool->inputarea_newdata_pushed[idx] = 1;
            pool->inputarea_writeout_complete[idx] = 0;
            pthread_mutex_unlock(&pool->inputarea_mutex[idx]);
            pthread_cond_signal(&pool->inputarea_newdata_cond[idx]);

            // Wait for an idle area
            while (1)
            {
                // First, wait if all are busy
                pthread_mutex_lock(&vfd->lock);
                while (pool->inputareas_busy_count >= vfd->nfiles)
                {
                    struct timespec abstimeout;
                    clock_gettime(0, &abstimeout);
                    abstimeout.tv_sec += 1;
                    rc = pthread_cond_timedwait(&pool->any_inputarea_done, &vfd->lock, &abstimeout);
                    if (rc == ETIMEDOUT)
                    {
                        if (m_m6sg_dbglevel>2) { printf("writer timed out waiting for any free io area, trying again\n"); }
                    }
                }
                pthread_mutex_unlock(&vfd->lock);

                // Next, locate an idle area
                for (n = 0; n <= vfd->nfiles; n++)
                {
                    idx = (idx + 1) % vfd->nfiles;
                    pthread_mutex_lock(&pool->inputarea_mutex[idx]);
                    if (pool->inputarea_writeout_complete[idx] == 1)
                    {
                        pool->active_area = idx;
                        idx = pool->active_area;
                        if (m_m6sg_dbglevel>2) { printf("writer switching to next free area %d\n", idx); }
                        pthread_mutex_unlock(&pool->inputarea_mutex[idx]);
                        break;
                    }
                    pthread_mutex_unlock(&pool->inputarea_mutex[idx]);
                }

                // Success?
                if (n < vfd->nfiles)
                {
                    break;
                }
                if (m_m6sg_dbglevel>2) { printf("writer switching missed free area, thought %d\n", idx); }
            }
        }

        // Append data
        memcpy(pool->inputareas[idx] + pool->inputarea_append_offset[idx], buf, count);
        pool->inputarea_append_offset[idx] += count;
        nwr += count;
    }

    return nwr;
}

/**
 * Move data from a socket into a scattered fileset
 */
#if HAVE_MMSG && __linux__
ssize_t mark6_sg_recvfile(int fd, int sd, size_t nitems, size_t itemlen)
{
    m6sg_virt_filedescr_t* vfd;
    writer_pool_t* pool;
    ssize_t nwr = 0, nread;
    size_t  count = nitems * itemlen;
    int idx, rc, n;

    struct mmsghdr *msgs;
    struct iovec *iovecs;

    // Catch some error conditions
    if ((fd < 0) || (fd >= MARK6_SG_VFS_MAX_OPEN_FILES))
    {
        errno = EBADF;
        return -1;
    }

    vfd = m6sg_open_files_list.fd_list[fd];
    if (!vfd->valid || (vfd->mode != O_WRONLY))
    {
        errno = EBADF;
        return -1;
    }

    pool = &vfd->writer_pool;

    msgs = (struct mmsghdr*)malloc(nitems * sizeof(struct mmsghdr));
    iovecs = (struct iovec*)malloc(nitems * sizeof(struct iovec));
    memset(msgs, 0, sizeof(nitems * sizeof(struct mmsghdr)));

    while (nwr < count)
    {
        // Try the current area first
        idx = pool->active_area;
        assert(pool->inputarea_writeout_complete[idx] == 1);
        assert(pool->inputarea_newdata_pushed[idx] == 0);

        // Detect if write-out is necessary
        if ((pool->inputarea_append_offset[idx] + count) >= pool->inputareasize)
        {
            // Signal the area for a background write-out
            pthread_mutex_lock(&pool->inputarea_mutex[idx]);
            pool->inputarea_newdata_pushed[idx] = 1;
            pool->inputarea_writeout_complete[idx] = 0;
            pthread_mutex_unlock(&pool->inputarea_mutex[idx]);
            pthread_cond_signal(&pool->inputarea_newdata_cond[idx]);

            // Wait for an idle area
            while (1)
            {
                // First, wait if all are busy
                pthread_mutex_lock(&vfd->lock);
                while (pool->inputareas_busy_count >= vfd->nfiles)
                {
                    struct timespec abstimeout;
                    clock_gettime(0, &abstimeout);
                    abstimeout.tv_sec += 1;
                    rc = pthread_cond_timedwait(&pool->any_inputarea_done, &vfd->lock, &abstimeout);
                    if (rc == ETIMEDOUT)
                    {
                        if (m_m6sg_dbglevel>2) { printf("writer timed out waiting for a free io area, trying again\n"); }
                    }
                }
                pthread_mutex_unlock(&vfd->lock);

                // Next, locate an idle area
                for (n = 0; n < vfd->nfiles; n++)
                {
                    idx = (idx + 1) % vfd->nfiles;
                    if (pool->inputarea_writeout_complete[idx] == 1)
                    {
                        pthread_mutex_lock(&vfd->lock);
                        pool->active_area = idx;
                        pthread_mutex_unlock(&vfd->lock);
                        idx = pool->active_area;
                        if (m_m6sg_dbglevel>2) { printf("writer switching to next free area %d\n", idx); }
                        break;
                    }
                }

                // Success?
                if (pool->inputarea_writeout_complete[idx] == 1)
                {
                     break;
                }
                if (m_m6sg_dbglevel>2) { printf("writer switching missed free area, thought %d\n", idx); }
            }
            assert(pool->inputarea_writeout_complete[idx] == 1);
            assert(pool->inputarea_newdata_pushed[idx] == 0);
        }

        // Determine where to append data
        for (n = 0; n < nitems; n++) {
            char *base = pool->inputareas[idx] + pool->inputarea_append_offset[idx];
            iovecs[n].iov_base         = base + n*itemlen;
            iovecs[n].iov_len          = itemlen;
            msgs[n].msg_hdr.msg_iov    = &iovecs[n];
            msgs[n].msg_hdr.msg_iovlen = 1;
       }

        // Append data via socket read
        nread = recvmmsg(sd, msgs, nitems, MSG_WAITALL, NULL);
        if (nread < 0) {
            perror("recvmmsg");
            free(msgs);
            free(iovecs);
            return 0;
        }
        pool->inputarea_append_offset[idx] += (nread*itemlen);
        nwr += (nread*itemlen);
        nitems -= nread;
    }

    free(msgs);
    free(iovecs);

    return nwr;
}
#else
ssize_t mark6_sg_recvfile(int fd, int sd, size_t nitems, size_t itemlen)
{
    errno = ENOTSUP;
    return -1;
}
#endif

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
 * Set hint of packetsize for creat()
 */
int mark6_sg_packetsize_hint(int size_hint)
{
    vdif_packetsize_hint = size_hint;
    return size_hint;
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

void* touch_next_blocks_thread(void* p_ioctx)
{
    size_t blk = 0, blk_prev = -1;
    off_t  off, off_stop;
    int    pagesz = getpagesize();

    io_thread_ctx_t*       ctx = (io_thread_ctx_t*)p_ioctx;
    m6sg_virt_filedescr_t* vfd = (m6sg_virt_filedescr_t*)(ctx->vfd);

#if !AVOID_MMAP
    char* fdata = (char*)(vfd->fmmap[ctx->file_id]);
#endif

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
            char dummy;
#if !AVOID_MMAP
            dummy = fdata[off];
#else
            (void)pread(vfd->fds[ctx->file_id], &dummy, 1, off);
#endif
            off += pagesz;
            if (m_m6sg_dbglevel>99) { printf("(printf to prevent optimizing away 'dummy') %c", dummy); }

            // Cancel if we're running late relative to user
            if ((blk + PREFETCH_NUM_BLOCKS_PER_FILE) < vfd->rdblock) break;
        }

    }

    if (m_m6sg_dbglevel>2) { fprintf(stderr, "EXIT thread for file %2d\n", ctx->file_id); }

    return NULL;
}

void* writer_thread(void* p_ioctx)
{
    io_thread_ctx_t*       ctx = (io_thread_ctx_t*)p_ioctx;
    m6sg_virt_filedescr_t* vfd = (m6sg_virt_filedescr_t*)(ctx->vfd);
    writer_pool_t*         pool = &vfd->writer_pool;
    const int id = ctx->file_id;

    const unsigned char* area = (const unsigned char*)pool->inputareas[id];
    int n;

    while (!vfd->writers_terminate)
    {
        wb_header_tag_v2_t m6blk;
        size_t blocklen;

        // Wait for wakeup
        pthread_mutex_lock(&pool->inputarea_mutex[id]);
        while (!pool->inputarea_newdata_pushed[id])
        {
            int rc;
            struct timespec abstimeout;
            clock_gettime(0, &abstimeout);
            abstimeout.tv_sec += 1;
            rc = pthread_cond_timedwait(&pool->inputarea_newdata_cond[id], &pool->inputarea_mutex[id], &abstimeout);
            if (rc == ETIMEDOUT)
            {
                if (m_m6sg_dbglevel>2) { printf("io %d timed out waiting for user data, trying again\n", id); }
            }
        }
        blocklen = pool->inputarea_append_offset[id];
        pool->inputarea_writeout_complete[id] = 0;
        pthread_mutex_unlock(&pool->inputarea_mutex[id]);

        // Ignore too small blocks
        if (pool->inputarea_append_offset[id] <= 32)
        {
            pool->inputarea_writeout_complete[id] = 1;
            continue;
        }

        // Increment global datablock# and currently busy files
        pthread_mutex_lock(&vfd->lock);
        m6blk.blocknum = vfd->nblocks;
        m6blk.wb_size  = blocklen;
        m6blk.wb_size += sizeof(m6blk);
        vfd->nblocks++;
        // TODO: also update vfd->blks[] by appending a new block list entry?
        pool->inputareas_busy_count++;
        pthread_mutex_unlock(&vfd->lock);

        // Determine VDIF frame size once
        if (m6blk.blocknum == 0)
        {
            struct file_header_tag m6hdr;
            size_t vdifsize;

            // Get size from (presumed) VDIF header
            vdifsize = ((size_t)area[8]) + (((size_t)area[9])<<8) + (((size_t)area[10])<<16); // VDIF spec: "in units of 8 bytes"
            vdifsize = 8 * vdifsize;
            assert(vdifsize <= vfd->framesize); // because buffer allocations are upto N*default_framesize only...

            // Remember it
            pthread_mutex_lock(&vfd->lock);
            vfd->framesize = vdifsize;
            pthread_mutex_unlock(&vfd->lock);

            // Update metadata in scatter-gather files
            for (n = 0; n < vfd->nfiles; n++)
            {
                // maybe not the tidiest method... alternative is delayed creat() to launch threads etc only after the first write
                lseek(vfd->fds[n], 0, SEEK_SET);
                read(vfd->fds[n], &m6hdr, sizeof(m6hdr));
                m6hdr.packet_size = vfd->framesize;
                m6hdr.block_size  = m6blk.wb_size;
                lseek(vfd->fds[n], 0, SEEK_SET);
                write(vfd->fds[n], &m6hdr, sizeof(m6hdr));
            }
            if (m_m6sg_dbglevel>2) { printf("io %d with block#%zu found VDIF frame size %zu, and blocken %zd\n", id, (size_t)m6blk.blocknum, (size_t)m6hdr.packet_size, (size_t)m6blk.wb_size); }
        }

        // Write block header
        write(vfd->fds[id], &m6blk, sizeof(m6blk));

        // Write data
        write(vfd->fds[id], area, blocklen);

        // Mark the area as available for new data
        pthread_mutex_lock(&pool->inputarea_mutex[id]);
        pool->inputarea_append_offset[id] = 0;
        pool->inputarea_newdata_pushed[id] = 0;
        pool->inputarea_writeout_complete[id] = 1;
        pthread_mutex_unlock(&pool->inputarea_mutex[id]);

        // Decrement the busy files count
        pthread_mutex_lock(&vfd->lock);
        assert(pool->inputareas_busy_count > 0);
        pool->inputareas_busy_count--;
        if (m_m6sg_dbglevel>2) { printf("io %d with block#%zu done writing\n", id, (size_t)m6blk.blocknum); }
        pthread_mutex_unlock(&vfd->lock);

        pthread_cond_signal(&pool->any_inputarea_done);
    }

    return NULL;
}

