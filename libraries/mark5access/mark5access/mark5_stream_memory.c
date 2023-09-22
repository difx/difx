/***************************************************************************
 *   Copyright (C) 2006-2015 by Walter Brisken                             *
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

#include <stdio.h>
#include <stdlib.h>
#include "mark5access/mark5_stream.h"

struct mark5_stream_memory
{
	const unsigned char *start;
	const unsigned char *end;			/* derived by init() */
	unsigned int nbytes;
};

static int mark5_stream_memory_init(struct mark5_stream *ms)
{
	const unsigned char *start;
	unsigned int nbytes;

	snprintf(ms->streamname, MARK5_STREAM_ID_LENGTH, "Memory");

	start = ((struct mark5_stream_memory *)(ms->inputdata))->start;
	nbytes = ((struct mark5_stream_memory *)(ms->inputdata))->nbytes;

	ms->datawindow = start;
	ms->datawindowsize = nbytes;

	((struct mark5_stream_memory *)(ms->inputdata))->end = start + nbytes;
	
	return 0;
}

static int mark5_stream_memory_next(struct mark5_stream *ms)
{
	const unsigned char *end;

	end = ((struct mark5_stream_memory *)(ms->inputdata))->end;
	
	ms->frame += ms->framebytes;
	if(ms->frame + ms->framebytes > end)
	{
		ms->readposition = -1;

		return -1;
	}

	/* successfully got new frame, so increment it */
	++ms->framenum;
	ms->readposition = 0;

	return ms->framebytes;
}

static int mark5_stream_memory_seek(struct mark5_stream *ms, long long framenum)
{
	const unsigned char *start, *end;
	
	start = ((struct mark5_stream_memory *)(ms->inputdata))->start;
	end = ((struct mark5_stream_memory *)(ms->inputdata))->end;

	ms->frame = start + ms->frameoffset + framenum*ms->framebytes;
	if(framenum < 0 || ms->frame + ms->framebytes > end)
	{
		ms->readposition = -1;

		return -1;
	}

	return 0;
}

static int mark5_stream_memory_final(struct mark5_stream *ms)
{
	free(ms->inputdata);
	
	return 0;
}

struct mark5_stream_generic *new_mark5_stream_memory(const void *data, unsigned int nbytes)
{
	struct mark5_stream_generic *s;
	struct mark5_stream_memory *M;

	s = (struct mark5_stream_generic *)calloc(1, sizeof(struct mark5_stream_generic));
	M = (struct mark5_stream_memory *)calloc(1, sizeof(struct mark5_stream_memory));
	M->start = (const unsigned char *)data;
	M->nbytes = nbytes;

	s->init_stream = mark5_stream_memory_init;
	s->next = mark5_stream_memory_next;
	s->seek = mark5_stream_memory_seek;
	s->final_stream = mark5_stream_memory_final;
	s->inputdata = M;
	s->inputdatasize = sizeof(struct mark5_stream_memory);

	return s;
}

