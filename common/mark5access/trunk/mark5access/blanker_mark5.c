/***************************************************************************
 *   Copyright (C) 2007 by Walter Brisken                                  *
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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "config.h"
#include "mark5access/mark5_stream.h"

#ifdef WORDS_BIGENDIAN
#define MARK5_FILL_WORD32 0x44332211UL
#define MARK5_FILL_WORD64 0x4433221144332211ULL
#else
#define MARK5_FILL_WORD32 0x11223344UL
#define MARK5_FILL_WORD64 0x1122334411223344ULL
#endif

/* This module is used to find and mark bad data (ie, data replaced with
 * mark5 fill pattern.  This populates the "zones" data fields for
 * identification within the unpacking steps
 */

static int findfirstinvalid(const uint64_t *data, int start, int end)
{
	int n, i, r, s;
	uint32_t *data32;

	data32 = (uint32_t *)data;
	n = end-start;
	data += start;
	i = n/2;
	r = n-1;

	for(s = n/4; s > 0; s >>= 1)
	{
		if(data[i] == MARK5_FILL_WORD64)
		{
			r = i;
			i -= s;
		}
		else
		{
			i += s;
		}
	}

	/* return in bytes, not 64 or 32 bit words */
	r = 8*(start+r);

	/* look for 32 bit aligned straggler */
	if(data32[r/4 - 1] == MARK5_FILL_WORD32)
	{
		r -= 4;
	}

	return r;
}

static int findfirstvalid(const uint64_t *data, int start, int end)
{
	int n, i, r, s;
	uint32_t *data32;

	data32 = (uint32_t *)data;
	n = end-start;
	data += start;
	i = n/2;
	r = n-1;

	for(s = n/4; s > 0; s >>= 1)
	{
		if(data[i] != MARK5_FILL_WORD64)
		{
			r = i;
			i -= s;
		}
		else
		{
			i += s;
		}
	}

	/* return in bytes, not 64 or 32 bit words */
	r = 8*(start+r);

	/* look for 32 bit aligned straggler */
	if(data32[r/4 - 2] == MARK5_FILL_WORD32)
	{
		r += 4;
	}

	return r;
}

int blanker_mark5(struct mark5_stream *ms)
{
	int b, e, zonesize, nword;
	uint64_t *data;
	int startOK, endOK, zone=0;
	int nblanked = 0;
	int off;

	off = ms->payloadoffset;

	/* To be compatible with Mark5B and VLBA/Mark4 formats, the
	 * following must be either 14 or 15.
	 */
	ms->log2blankzonesize = 15;	/* 32768 bytes in size */
	zonesize = 1 << (ms->log2blankzonesize-3);
	nword = ms->framebytes/8;

	data = (uint64_t *)ms->frame;

	for(b = 0; b < nword; b += zonesize)
	{
		e = b + zonesize;
		if(e > nword)
		{
			e = nword;
		}

		startOK = data[b] != MARK5_FILL_WORD64;
		endOK   = data[e-1] != MARK5_FILL_WORD64;

		if(startOK && endOK)
		{
			ms->blankzonestartvalid[zone] = 0;
			ms->blankzoneendvalid[zone] = 1<<30;
		}
		else if(!startOK && !endOK)
		{
			ms->blankzonestartvalid[zone] = 1<<30;
			ms->blankzoneendvalid[zone] = 0;
			nblanked += (e-b)*8;
		}
		else if(startOK)
		{
			ms->blankzonestartvalid[zone] = 0;
			ms->blankzoneendvalid[zone] = 
				findfirstinvalid(data, b, e) - off;
			nblanked += (e*8 - (ms->blankzoneendvalid[zone] + off));
		}
		else
		{
			ms->blankzonestartvalid[zone] =
				findfirstvalid(data, b, e) - off;
			ms->blankzoneendvalid[zone] = 1<<30;
			if(ms->blankzonestartvalid[zone] > b*8)
			{
				nblanked += (ms->blankzonestartvalid[zone] 
					+ off - b*8);
			}
		}

		zone++;
	}


	return nblanked;
}
