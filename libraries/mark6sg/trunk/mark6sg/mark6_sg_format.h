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
#ifndef MARK6_SG_FORMAT__H
#define MARK6_SG_FORMAT__H

// 
// Internal data types and structures for Mark6 scatter-gather
//
// There is apparently no documentation on the Mark6 non-RAID recording format(?).
// From Mark6 source code one can see that the several files associated with a
// single scan all look like this:
//
//  [file header]
//  [block a header] [block a data (~10MB)]
//  [block b header] [block b data (~10MB)]
//  [block c header] [block c data (~10MB)]
//  ...
//
// Block numbers are increasing. They will have gaps (e.g, a=0, b=16, c=35, ...).
// The 'missing' blocks (1,2,3..,15,17,18,..,34,...) of one file are found in one of
// the other files. Blocks can have different data sizes.
//
// Because 10GbE recording in Mark6 software is not done Round-Robin across files,
// the order of blocks between files is somewhat random.
//
// There is no provision for zero padding of the structures, this makes page alignment
// impossible, would have been nice for zero-copy mmap() <-- recvmmsg().
//
// The below structures are adopted from the source code of the
// Mark6 program 'd-plane' version 1.12:

#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/stat.h>
#include <stdint.h>

#define MARK6_SG_SYNC_WORD      0xfeed6666

#ifdef __cplusplus
extern "C" {
#endif

// File header - one per file
struct file_header_tag
{
    uint32_t sync_word;         // MARK6_SG_SYNC_WORD
    int32_t  version;           // defines format of file
    int32_t  block_size;        // length of blocks including header (bytes)
    int32_t  packet_format;     // format of data packets, enumerated below
    int32_t  packet_size;       // length of packets (bytes)
};

// Write block header - version 2
typedef struct wb_header_tag_v2
{
    int32_t blocknum;           // block number, starting at 0
    int32_t wb_size;            // same as block_size, or occasionally shorter
} wb_header_tag_v2_t;

// Write block header - version 1
typedef struct wb_header_tag_v1
{
    int32_t blocknum;           // block number, starting at 0
} wb_header_tag_v1_t;

#ifdef __cplusplus
}
#endif

#endif // MARK6_SG_FORMAT__H
