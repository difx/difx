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
#ifndef MARK6_SG_VFS__H
#define MARK6_SG_VFS__H

// These VFS-like functions open and read Mark6 scatter-gather files,
// and have the same interface as 'man 2 open', 'man 2 read', etc.

#include <sys/types.h>
#include <sys/stat.h>

#include "mark6_sg_utils.h"

// Library functions
extern int     mark6_sg_open (const char *scanname, int flags);
extern int     mark6_sg_close(int fd);
extern ssize_t mark6_sg_read (int fd, void* buf, size_t count);
extern off_t   mark6_sg_lseek(int fd, off_t offset, int whence);
extern int     mark6_sg_fstat(int fd, struct stat *buf);

extern int     mark6_sg_packetsize(int fd);

#endif
