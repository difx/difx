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
#ifndef MARK6_SG_UTILS__H
#define MARK6_SG_UTILS__H

#include <sys/types.h>
#include <sys/stat.h>

// Defines
#define MARK6_SG_SYNC_WORD          0xfeed6666
#define MARK6_SG_MAXFILES           32
#define MARK6_SG_ROOT_PATTERN       "/mnt/disks/[1-4]/[0-7]/"
#define MARK6_SG_VFS_MAX_OPEN_FILES 8192

// Debug level
extern int m_m6sg_dbglevel;

// Struct used by library to describe a group of files and their block content
typedef struct m6sg_blockmeta {
    int      file_id;        // index into the filename list (same list as passed to mark6_sg_blocklist())
    int      blocknum;       // block number
    int      packetsize;     // size of packets that Mark6 software thought were recorded
    off_t    file_offset;    // byte offset to the VLBI data in the file specified by file_id
    size_t   datalen;        // length in bytes of the VLBI data stored in the block
    off_t    virtual_offset; // virtual byte offset in the "gathered" output sans all headers/metadata
} m6sg_blockmeta_t;

// Library functions
extern int mark6_sg_filelist_from_name(const char* scanname, char*** filepathlist, char*** filenamelist);
extern int mark6_sg_filelist_uniques(int nfiles, const char** filenamelist, char*** uniquenamelist);
extern int mark6_sg_list_all_scans(char*** uniquenamelist);
extern int mark6_sg_blocklist(int nfiles, const char** filenamelist, m6sg_blockmeta_t** blocklist);
extern int mark6_sg_verbositylevel(int);
#ifdef __cplusplus
// TODO: allow alternate compilation with C++ class def
#endif

#endif
