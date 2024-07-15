/***************************************************************************
 *   Copyright (C) 2006-2024 by Walter Brisken, Chris Phillips             *
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
#include <string.h>
#include "mark5access/mark5_stream.h"

static int mark5_stream_unpacker_next(struct mark5_stream *ms)
{
	ms->payload += ms->framebytes;
	if(ms->frame)
	{
		ms->frame += ms->framebytes;
	}

	/* successfully got new frame, so increment it */
	ms->framenum++;
	ms->readposition = 0;

	return 0;
}

static int mark5_stream_unpacker_next_noheaders(struct mark5_stream *ms)
{
	ms->payload += ms->databytes;
	if(ms->frame)
	{
		ms->frame += ms->framebytes;
	}

	/* successfully got new frame, so increment it */
	ms->framenum++;
	ms->readposition = 0;

	return 0;
}

static int mark5_stream_unpacker_init(struct mark5_stream *ms)
{
	ms->frame = 0;
	ms->payload = 0;
	ms->datawindow = 0;
	ms->datawindowsize = 0;
	ms->blanker = blanker_none;
	ms->log2blankzonesize = 30;
	ms->mjd = -1;
	if(ms->next == mark5_stream_unpacker_next_noheaders)
	{
		snprintf(ms->streamname, MARK5_STREAM_ID_LENGTH,
			"Unpacker-no-headers");
	}
	else
	{
		snprintf(ms->streamname, MARK5_STREAM_ID_LENGTH,
			"Unpacker-with-headers");
	}
	
	return 0;
}

struct mark5_stream_generic *new_mark5_stream_unpacker(int noheaders)
{
	struct mark5_stream_generic *V;

	V = (struct mark5_stream_generic *)calloc(1, sizeof(struct mark5_stream_generic));

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
	V->inputdatasize = 0;

	return V;
}

int mark5_unpack(struct mark5_stream *ms, const void *packed, float **unpacked, int nsamp)
{
	if(ms->next == mark5_stream_unpacker_next_noheaders)
	{
		ms->payload = (const unsigned char *)packed;
		ms->blanker(ms);
	}
	else
	{
		//go back to previous frame so we can make use of next() and its validation
		ms->frame = (const unsigned char *)packed - ms->framebytes;
		mark5_stream_next_frame(ms); //this also sets ms->payload()
	}
	ms->readposition = 0;

	return ms->decode(ms, nsamp, unpacked);
}

int mark5_unpack_with_offset(struct mark5_stream *ms, const void *packed, int offsetsamples, float **unpacked, int nsamp)
{
	if(ms->next == mark5_stream_unpacker_next_noheaders)
	{
		ms->payload = (const unsigned char *)packed;
#warning	/* FIXME: I think this should really be incremented by (offsetsamples/ms->framesamples)*ms->framebytes, but Walter is worried this might break fake mode (which is the main(only?) use case for this function */
		ms->blanker(ms);
	}
	else
	{
		//go back to previous frame so we can make use of next() and its validation
		ms->frame = (const unsigned char *)packed + (offsetsamples/ms->framesamples)*ms->framebytes - ms->framebytes;
		mark5_stream_next_frame(ms); //this also sets ms->payload()
	}

	/* set readposition to first desired sample */
	ms->readposition = (offsetsamples % ms->framesamples)*ms->nchan*ms->nbit*ms->decimation/8;

	return ms->decode(ms, nsamp, unpacked);
}



int mark5_unpack_complex(struct mark5_stream *ms, const void *packed, mark5_float_complex **unpacked, int nsamp)
{
	if(ms->next == mark5_stream_unpacker_next_noheaders)
	{
		ms->payload = (const unsigned char *)packed;
		ms->blanker(ms);
	}
	else
	{
		//go back to previous frame so we can make use of next() and its validation
		ms->frame = (const unsigned char *)packed - ms->framebytes;
		mark5_stream_next_frame(ms); //this also sets ms->payload()
	}

	ms->readposition = 0;

	return ms->complex_decode(ms, nsamp, unpacked);
}

int mark5_unpack_complex_with_offset(struct mark5_stream *ms, const void *packed, int offsetsamples, mark5_float_complex **unpacked, int nsamp)
{
	if(ms->next == mark5_stream_unpacker_next_noheaders)
	{
		ms->payload = (const unsigned char *)packed;
#warning	/* FIXME: I think this should really be incremented by (offsetsamples/ms->framesamples)*ms->framebytes, but Walter is worried this might break fake mode (which is the main(only?) use case for this function */
		ms->blanker(ms);
	}
	else
	{
                //go back to previous frame so we can make use of next() and its validation
                ms->frame = (const unsigned char *)packed + (offsetsamples/ms->framesamples)*ms->framebytes - ms->framebytes;
                mark5_stream_next_frame(ms); //this also sets ms->payload()
	}

	/* set readposition to first desired sample */
	ms->readposition = (offsetsamples % ms->framesamples)*ms->nchan*ms->nbit*2*ms->decimation/8;

	ms->blanker(ms);

	return ms->complex_decode(ms, nsamp, unpacked);
}

