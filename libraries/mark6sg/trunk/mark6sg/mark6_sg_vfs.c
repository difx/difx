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
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <glob.h>
#include <linux/limits.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "mark6_sg_vfs.h"

// Internal structs
typedef struct m6sg_virt_filedescr {
    int       valid;
    int       eof;
    size_t    len;
    off_t     rdoffset;
    int       rdblock;
    int       nfiles;
    char**    filepathlist;
    char**    filenamelist; // (unused, although populated)
    FILE*     fds[MARK6_SG_MAXFILES];
    int       nblocks;
    m6sg_blockmeta_t* blks;
} m6sg_virt_filedescr_t;

typedef struct m6sg_virt_open_files {
    int nopen;
    m6sg_virt_filedescr_t* fd_list;
} m6sg_virt_open_files_t;

static m6sg_virt_open_files_t m6sg_open_files_list = { 0, NULL };


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
    if (m6sg_open_files_list.nopen >= MARK6_SG_VFS_MAX_OPEN_FILES)
    {
        errno = ENOMEM;
        return -1;
    }

    // Find next free 'fd'
    if (NULL == m6sg_open_files_list.fd_list)
    {
        m6sg_open_files_list.fd_list = (m6sg_virt_filedescr_t*) malloc(MARK6_SG_VFS_MAX_OPEN_FILES*sizeof(m6sg_virt_filedescr_t*));
    }
    for (fd=0; fd<MARK6_SG_VFS_MAX_OPEN_FILES; fd++)
    {
        if (m6sg_open_files_list.fd_list[fd].valid == 0)
        {
            break;
        }
    }
    assert(fd < MARK6_SG_VFS_MAX_OPEN_FILES);

    // Prepare file descriptor
    vfd = &m6sg_open_files_list.fd_list[fd];
    memset((void*)vfd, 0, sizeof(m6sg_virt_filedescr_t));
    vfd->valid = 1;

    // Gather all metadata about the scan's files
    vfd->nfiles  = mark6_sg_filelist_from_name(scanname, &(vfd->filepathlist), &(vfd->filenamelist));
    vfd->nblocks = mark6_sg_blocklist(vfd->nfiles, (const char**)(vfd->filepathlist), &(vfd->blks));
    for (i=0; i<(vfd->nfiles); i++)
    {
        vfd->fds[i] = fopen(vfd->filepathlist[i], "r");
    }
    if (vfd->nfiles <= 0)
    {
        mark6_sg_close(fd);
        errno = ENOENT;
        return -1;
    }

    // Total scan length
    if (vfd->nblocks > 0)
    {
        vfd->len = vfd->blks[vfd->nblocks-1].virtual_offset + vfd->blks[vfd->nblocks-1].datalen;
    }

    m6sg_open_files_list.nopen++;

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

    vfd = &m6sg_open_files_list.fd_list[fd];
    if (!vfd->valid)
    {
        errno = EBADF;
        return -1;
    }

    for (i=0; i < vfd->nfiles; i++)
    {
        free(vfd->filepathlist[i]);
        free(vfd->filenamelist[i]);
        fclose(vfd->fds[i]);
    }
    free(vfd->filepathlist);
    free(vfd->filenamelist);
    free(vfd->blks);
    memset((void*)vfd, 0, sizeof(m6sg_virt_filedescr_t));

    m6sg_open_files_list.nopen--;
    return 0;
}


/**
 * Read a piece of data from a scattered file.
 * Similar behaviour as 'man 2 read'.
 */
ssize_t mark6_sg_read(int fd, void* buf, size_t count)
{
    m6sg_virt_filedescr_t* vfd;
    size_t nread = 0;
    size_t nremain = count;
    FILE* infile;
    int   blk;
    off_t blkstart, blkstop;


    // Catch some error conditions
    if ((fd < 0) || (fd >= MARK6_SG_VFS_MAX_OPEN_FILES))
    {
        errno = EBADF;
        return -1;
    }

    vfd = &m6sg_open_files_list.fd_list[fd];
    if (!vfd->valid)
    {
        errno = EBADF;
        return -1;
    }
    if (vfd->rdoffset > vfd->len)
    {
        vfd->eof = 1;
        return 0;
    }

    // Perform read
    while ((nremain > 0) && (vfd->rdoffset < vfd->len))
    {
        size_t navail, nskip, nwanted, nrd;
        off_t  foff;
        int    fid;
        blk      = vfd->rdblock;
        blkstart = vfd->blks[blk].virtual_offset;
        blkstop  = vfd->blks[blk].datalen + blkstart;
        fid      = vfd->blks[blk].file_id;
        infile   = vfd->fds[fid];
        if ((vfd->rdoffset < blkstart) || (vfd->rdoffset >= blkstop))
        {
            vfd->rdblock = (vfd->rdblock + 1) % vfd->nblocks;
            continue; // try other blocks until finding correct one
        }

        nskip   = vfd->rdoffset - blkstart;
        navail  = blkstop - vfd->rdoffset;
        nwanted = (nremain < navail) ? nremain : navail;

        // Read the data from file on disk
        foff = vfd->blks[blk].file_offset + nskip;
        fseek(infile, foff, SEEK_SET);
        nrd = fread(buf+nread, 1, nwanted, infile); // TODO: what to do when nrd != wnanted

        if (m_m6sg_dbglevel>0) { printf("read() file %2d offset %ld : virtual offset %ld : req %ld read %ld\n", fid, foff, vfd->rdoffset, nwanted, nrd); }

        vfd->rdoffset += nwanted;
        nread         += nwanted;
        nremain       -= nwanted;
    }

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

    vfd = &m6sg_open_files_list.fd_list[fd];
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

    vfd = &m6sg_open_files_list.fd_list[fd];
    if (!vfd->valid)
    {
        errno = EBADF;
        return -1;
    }

    // Take basic 'stat' infos from first file
    fstat(fileno(vfd->fds[0]), buf); // TODO: check return value?

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

    vfd = &m6sg_open_files_list.fd_list[fd];
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
