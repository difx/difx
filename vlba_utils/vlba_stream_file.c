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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "vlba_stream.h"

#define MAX_VLBA_STREAM_FILES	20	/* probably way too small */

struct VLBA_stream_file
{
	long long offset;
	char files[MAX_VLBA_STREAM_FILES][256];
	int nfiles;
	int buffersize;

	int curfile;
	int fetchsize;
	int in;
	uint32_t *buffer;
	uint32_t *start;
	uint32_t *end;
	uint32_t *last;
};

static int VLBA_stream_file_init(struct VLBA_stream *vs)
{
	struct VLBA_stream_file *F;
	int n;

	F = (struct VLBA_stream_file *)(vs->inputdata);

	F->curfile = 0;
	F->buffer = 0;
	F->last = 0;
	F->start = 0;
	F->end = 0;
	F->fetchsize = 0;
	F->in = open64(F->files[0], O_RDONLY);
	if(!F->in)
	{
		fprintf(stderr, "VLBA_stream_file_init: "
			"File not found : %s\n",
			F->files[0]);
		return -1;
	}
	lseek(F->in, F->offset, SEEK_SET);
	F->buffer = (uint32_t *)malloc(F->buffersize);
	F->start = F->buffer;

	n = read(F->in, F->buffer, F->buffersize);
	if(n < F->buffersize)
	{
		fprintf(stderr, "VLBA_stream_file_init: "
			"File too short : %s\n",
			F->files[0]);
		return -1;
	}

	vs->frame = F->buffer;

	return 0;
}

static int VLBA_stream_file_next(struct VLBA_stream *vs)
{
	struct VLBA_stream_file *F;
	int nframes, n;

	F = (struct VLBA_stream_file *)(vs->inputdata);

	if(F->fetchsize == 0)	/* finish some initialization */
	{
		F->start = (uint32_t *)((char *)(F->buffer) +
			vs->frameoffset);
		nframes = (F->buffersize - vs->frameoffset)/vs->gulpsize;
		F->fetchsize = nframes*vs->gulpsize;
		F->end = F->start + F->fetchsize/4;
		F->last = F->end;

		/* back up stream a bit to load whole frames */
		lseek(F->in, F->offset + vs->frameoffset + F->fetchsize, 
			SEEK_SET);
	}

	/* usually this is all that needs to be done */
	vs->frame += vs->gulpsize/4;
	
	if(vs->frame + vs->gulpsize/4 > F->end)
	{
		vs->frame = F->start;
		n = read(F->in, F->start, F->fetchsize);
		
		while(n < F->fetchsize)
		{
			close(F->in);
			F->in = 0;
			F->curfile++;
			if(F->curfile >= F->nfiles)
			{
				break;
			}
			F->in = open64(F->files[F->curfile], O_RDONLY);
			if(!F->in)
			{
				break;
			}
			n += read(F->in, F->start+n, F->fetchsize-n);
		}
		
		if(n < vs->gulpsize)
		{
			return -1;
		}
		
		if(n < F->fetchsize)
		{
			F->last = F->start + n;
		}
		
		return vs->gulpsize;
	}

	if(vs->frame + vs->gulpsize/4 > F->last)
	{
		return -1;
	}

	return vs->gulpsize;
}

static int VLBA_stream_file_prev(struct VLBA_stream *vs)
{
	struct VLBA_stream_file *F;

	F = (struct VLBA_stream_file *)(vs->inputdata);

	vs->frame -= vs->gulpsize/4;
	if(vs->frame < F->start)
	{
		vs->frame += vs->gulpsize/4;
		return -1;
	}

	return 0;
}

static int VLBA_stream_file_final(struct VLBA_stream *vs)
{
	struct VLBA_stream_file *F;

	F = (struct VLBA_stream_file *)(vs->inputdata);

	if(F->in)
	{
		close(F->in);
	}
	if(F->buffer)
	{
		free(F->buffer);
	}

	free(F);

	return 0;
}

struct VLBA_stream *VLBA_stream_file_open(const char *filename,
	long long offset, int bits, int fanout)
{
	struct VLBA_stream_generic V;
	struct VLBA_stream_file *F;

	F = (struct VLBA_stream_file *)malloc(
		sizeof(struct VLBA_stream_file));
	strcpy(F->files[0], filename);
	F->nfiles = 1;
	F->buffersize = 1<<20;	/* 1MB seems like a reasonable default */
	F->offset = offset;

	V.init = VLBA_stream_file_init;
	V.next = VLBA_stream_file_next;
	V.prev = VLBA_stream_file_prev;
	V.final = VLBA_stream_file_final;
	V.inputdata = F;

	return VLBA_stream_generic_open(&V, bits, fanout);
}

void VLBA_stream_file_add_infile(struct VLBA_stream *vs, const char *filename)
{
	struct VLBA_stream_file *F;

	if(vs->init != VLBA_stream_file_init)
	{
		fprintf(stderr, "VLBA_stream_add_infile: "
			"Wrong stream type!\n");
		return;
	}

	F = (struct VLBA_stream_file *)(vs->inputdata);

	if(F->nfiles < MAX_VLBA_STREAM_FILES)
	{
		strcpy(F->files[F->nfiles], filename);
		F->nfiles++;
	}
}


/* for compatibility */

struct VLBA_stream *VLBA_stream_open(const char *filename,
	int bits, int fanout, long long offset)
{
	return VLBA_stream_file_open(filename, offset, bits, fanout);
}

void VLBA_stream_add_infile(struct VLBA_stream *vs, const char *filename)
{
	VLBA_stream_file_add_infile(vs, filename);
}
