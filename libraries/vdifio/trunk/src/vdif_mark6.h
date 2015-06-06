/***************************************************************************
 *  Copyright (C) 2015 by Walter Brisken                                   *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/vdifio/trunk/src/vdifio.c $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#ifndef __VDIF_MARK6_H__
#define __VDIF_MARK6_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define MARK6_SYNC	0xfeed6666

typedef struct
{
	uint32_t sync_word;	/* nominally the number above */
	int32_t version;
	int block_size;
	int packet_format;
	int packet_size;
} Mark6Header;

typedef struct
{
	int32_t blocknum;
	int32_t wb_size;
} Mark6BlockHeader_ver2;

typedef struct
{
	int32_t blocknum;
} Mark6BlockHeader_ver1;

const char *mark6PacketFormat(int formatId);

int mark6BlockHeaderSize(int version);

void printMark6Header(const Mark6Header *header);

#ifdef __cplusplus
}
#endif

#endif
