/***************************************************************************
 *   Copyright (C) 2006, 2007 by Walter Brisken                            *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
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
#include "vlba_stream.h"

struct VLBA_stream_memory
{
	uint32_t *start;
	uint32_t *end;			/* derived by init() */
	unsigned long length;
};

static int VLBA_stream_memory_init(struct VLBA_stream *vs)
{
	uint32_t *start;
	unsigned long length;

	start = ((struct VLBA_stream_memory *)(vs->inputdata))->start;
	length = ((struct VLBA_stream_memory *)(vs->inputdata))->length;

	((struct VLBA_stream_memory *)(vs->inputdata))->end = start + length/4;
	
	vs->frame = start;
	
	return 0;
};

static int VLBA_stream_memory_next(struct VLBA_stream *vs)
{
	uint32_t *end;

	end = ((struct VLBA_stream_memory *)(vs->inputdata))->end;
	
	vs->frame += vs->gulpsize/4;
	if(vs->frame + vs->gulpsize/4 > end)
	{
		return -1;
	}

	return vs->gulpsize;
};

static int VLBA_stream_memory_prev(struct VLBA_stream *vs)
{
	uint32_t *start;

	start = ((struct VLBA_stream_memory *)(vs->inputdata))->start;
	vs->frame -= vs->gulpsize/4;
	if(vs->frame < start)
	{
		vs->frame += vs->gulpsize/4;
		return -1;
	}
	
	return 0;
};

static int VLBA_stream_memory_final(struct VLBA_stream *vs)
{
	free(vs->inputdata);
	
	return 0;
};


struct VLBA_stream *VLBA_stream_memory_open(void *data, unsigned long length,
	int bits, int fanout)
{
	struct VLBA_stream_generic V;
	struct VLBA_stream_memory *M;

	M = (struct VLBA_stream_memory *)malloc(
		sizeof(struct VLBA_stream_memory));
	M->start = (uint32_t *)data;
	M->length = length;

	V.init = VLBA_stream_memory_init;
	V.next = VLBA_stream_memory_next;
	V.prev = VLBA_stream_memory_prev;
	V.final = VLBA_stream_memory_final;
	V.inputdata = M;

	return VLBA_stream_generic_open(&V, bits, fanout);
}

