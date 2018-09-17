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
#ifndef MARK6_SG_VFS_IMPL__H
#define MARK6_SG_VFS_IMPL__H

#include "mark6sg.h" // MARK6_SG_MAXFILES

#include <pthread.h>
#include <sys/types.h>

typedef struct m6sg_blockmeta_tt m6sg_blockmeta_t;

// Structs for internal use in the library

typedef struct io_thread_ctx_tt {
    pthread_t tid;
    void*     vfd;
    int       file_id;
} io_thread_ctx_t;

typedef struct writer_pool_tt {
    int       active_area;
    size_t    inputareasize;
    char*     inputareas[MARK6_SG_MAXFILES];                  // char[framesize*WRITER_FRAMES_PER_BLOCK]
    off_t     inputarea_append_offset[MARK6_SG_MAXFILES];     // 0..framesize*WRITER_FRAMES_PER_BLOCK-2
    int       inputarea_writeout_complete[MARK6_SG_MAXFILES]; // [n]='1' if inputarea[n] is available for appending data
    pthread_mutex_t inputarea_mutex[MARK6_SG_MAXFILES];
    pthread_cond_t  inputarea_newdata_cond[MARK6_SG_MAXFILES];
    int             inputarea_newdata_pushed[MARK6_SG_MAXFILES];
    int             inputareas_busy_count;
    pthread_cond_t  any_inputarea_done;
} writer_pool_t;

typedef struct m6sg_virt_filedescr {
    int       valid;
    int       eof;
    int       mode;
    size_t    len;
    off_t     rdoffset;
    size_t    rdblock;
    int       nfiles;
    char      scanname[1024];
    char**    filepathlist;
    char**    filenamelist; // (unused, although populated)
    int       fds[MARK6_SG_MAXFILES];
    void*     fmmap[MARK6_SG_MAXFILES];
    off_t     fsize[MARK6_SG_MAXFILES];
    size_t    nblocks;
    size_t    framesize;
    pthread_mutex_t    lock;
    m6sg_blockmeta_t*  blks;
    io_thread_ctx_t    touch_ctxs[MARK6_SG_MAXFILES];
    volatile int       touch_terminate;
    io_thread_ctx_t    writer_ctxs[MARK6_SG_MAXFILES];
    writer_pool_t      writer_pool;
    volatile int       writers_terminate;
} m6sg_virt_filedescr_t;

typedef struct m6sg_virt_open_files {
    int nopen;
    m6sg_virt_filedescr_t** fd_list;
} m6sg_virt_open_files_t;

#endif
