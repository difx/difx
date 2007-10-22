/***************************************************************************
 *   Copyright (C) 2006, 2007 by Walter Brisken                            *
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

#include <stdio.h>
#include <stdlib.h>
#include "mark5access/mark5_stream.h"

static int mark5_stream_unpacker_next(struct mark5_stream *ms)
{
	ms->payload += ms->framebytes;

	return 0;
}

static int mark5_stream_unpacker_next_noheaders(struct mark5_stream *ms)
{
	ms->payload += ms->databytes;

	return 0;
}

static int mark5_stream_unpacker_init(struct mark5_stream *ms)
{
	ms->frame = 0;
	ms->datawindow = 0;
	ms->datawindowsize = 0;
	ms->mjd = -1;
	if(ms->next == mark5_stream_unpacker_next_noheaders)
	{
		sprintf(ms->streamname, "Unpacker-no-headers");
	}
	else
	{
		sprintf(ms->streamname, "Unpacker-with-headers");
	}
	
	return 0;
}

struct mark5_stream_generic *new_mark5_stream_unpacker(int noheaders)
{
	struct mark5_stream_generic *V;

	V = (struct mark5_stream_generic *)malloc(
		sizeof(struct mark5_stream_generic));

	V->init_stream = mark5_stream_unpacker_init;
	if(noheaders)
	{
		V->next = mark5_stream_unpacker_next_noheaders;
	}
	else
	{
		V->next = mark5_stream_unpacker_next;
	}
	V->seek = 0;
	V->final_stream = 0;
	V->inputdata = 0;

	return V;
}

int mark5_unpack(struct mark5_stream *ms, void *packed, float **unpacked, 
	int nsamp)
{
	if(ms->next == mark5_stream_unpacker_next_noheaders)
	{
		ms->payload = (uint8_t *)packed;
	}
	else
	{
		ms->payload = (uint8_t *)packed + ms->payloadoffset;
	}
	ms->readposition = 0;
	
	return ms->decode(ms, nsamp, unpacked);
}
