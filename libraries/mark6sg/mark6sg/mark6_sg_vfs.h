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
// $Id: mark6_sg_vfs.h 10547 2022-07-26 09:07:43Z JanWagner $
// $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/libraries/mark6sg/mark6sg/mark6_sg_vfs.h $
// $LastChangedRevision: 10547 $
// $Author: JanWagner $
// $LastChangedDate: 2022-07-26 17:07:43 +0800 (二, 2022-07-26) $
//
//============================================================================
#ifndef MARK6_SG_VFS__H
#define MARK6_SG_VFS__H

// These VFS-like functions open and read Mark6 scatter-gather files,
// and have the same interface as 'man 2 open', 'man 2 read', etc.

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <sys/types.h>
#include <sys/stat.h>

#ifdef __cplusplus
extern "C" {
#endif

// Library functions
extern int     mark6_sg_open (const char *scanname, int flags);
extern int     mark6_sg_creat(const char *scanname, mode_t ignored);
extern int     mark6_sg_close(int fd);
extern ssize_t mark6_sg_write(int fd, const void* buf, size_t count);
extern ssize_t mark6_sg_read (int fd, void* buf, size_t count);
extern ssize_t mark6_sg_pread(int fd, void* buf, size_t count, off_t offset);
extern off_t   mark6_sg_lseek(int fd, off_t offset, int whence);
extern int     mark6_sg_fstat(int fd, struct stat *buf);
extern ssize_t mark6_sg_recvfile(int fd, int sd, size_t nitems, size_t itemlen);

extern int     mark6_sg_packetsize(int fd);
extern int     mark6_sg_packetsize_hint(int size_hint);
extern ssize_t mark6_sg_stripesize(int fd);

extern const char* mark6_sg_active_msn(int fd, const char* new_label);

#ifdef __cplusplus
}
#endif

#endif
