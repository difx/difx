/***************************************************************************
 *   Copyright (C) 2015 Walter Brisken                                     *
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
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/vdifio/trunk/src/vdifio.h $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include "vdifmark6.h"

struct vdif_mark6_mux *configurevdifmark6mux(const char *templateFilename, const char *fileParameter)
{
	struct vdif_mark6_mux *vm;

	return vm;
}

void deletevdifmark6mux(struct vdif_mark6_mux *vm)
{
}

void printvdifmark6mux(const struct vdif_mark6_mux *vm)
{
}

int vdifmark6mux(unsigned char *dest, int destSize, const unsigned char **src, const int *srcSize, const struct vdif_mark6_mux *vm, int64_t startOutputFrameNumber, struct vdif_mark6_mux_statistics *stats)
{

	return 0;
}

struct vdif_mark6_mux_statistics *newvdifmark6muxstatistics(const struct vdif_mark6_mux *vm)
{
	struct vdif_mark6_mux_statistics *stats;

	return stats;
}

void deletevdifmark6muxstatistics(struct vdif_mark6_mux_statistics *stats)
{
}

void printvdifmark6muxstatistics(const struct vdif_mark6_mux_statistics *stats)
{
}

void resetvdifmark6muxstatistics(struct vdif_mark6_mux_statistics *stats)
{
}
