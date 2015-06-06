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

#include <stdio.h>
#include "vdif_mark6.h"

#define str(s) #s

const char *mark6PacketFormat(int formatId)
{
	if(formatId == 0)
	{
		return "VDIF";
	}
	else if(formatId == 1)
	{
		return "Mark5b";
	}
	else if(formatId == 2)
	{
		return "Unknown";
	}
	else
	{
		return "Illegal value!";
	}
}

void printMark6Header(const Mark6Header *header)
{
	printf("Mark6 header\n");
	printf("  sync_word = 0x%8x%s\n", header->sync_word, (header->sync_word == MARK6_SYNC) ? "" : " which is weird; it should be " str(MARK6_SYNC) );
	printf("  version = %d\n", header->version);
	printf("  block_size = %d\n", header->block_size);
	printf("  packet_format = %d = %s\n", header->packet_format, mark6PacketFormat(header->packet_format));
	printf("  packet_size = %d\n", header->packet_size);
}
