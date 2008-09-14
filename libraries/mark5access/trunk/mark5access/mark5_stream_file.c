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

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#define _LARGEFILE64_SOURCE
#define __USE_FILE_OFFSET64
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include "mark5access/mark5_stream.h"

#define MAX_MARK5_STREAM_FILES	32	/* probably way too small */

struct mark5_stream_file
{
	int64_t offset;
	int64_t filesize;
	char files[MAX_MARK5_STREAM_FILES][256];
	int nfiles;
	int buffersize;

	int curfile;
	int fetchsize;
	int in;
	uint8_t *buffer;
	uint8_t *end;
	uint8_t *last;
};

/* loads fetchsize bytes into memory */
static int mark5_stream_file_fill(struct mark5_stream *ms)
{
	struct mark5_stream_file *F;
	struct stat64 fileStatus;
	int n;
	int err;
	char fn[64];

	F = (struct mark5_stream_file *)(ms->inputdata);

	n = read(F->in, F->buffer, F->fetchsize);
	
	while(n < F->fetchsize)
	{
		close(F->in);
		F->in = 0;
		F->curfile++;
		strncpy(fn, F->files[F->curfile], 64);
		fn[63] = 0;
		
		sprintf(ms->streamname, "File-%d/%d=%s", F->curfile,
			F->nfiles, fn);
		if(F->curfile >= F->nfiles)
		{
			break;
		}
		F->in = open64(F->files[F->curfile], O_RDONLY);
		if(F->in < 0)
		{
			fprintf(stderr, "File cannot be opened (2) : <%s> : "
				"in = %d\n",
				F->files[F->curfile], F->in);
			perror(0);
			return -1;
		}
		err = fstat64(F->in, &fileStatus);
		if(err < 0)
		{
			fprintf(stderr, "Error looking at file (2) : "
				"<%s> : err = %d\n",
				F->files[F->curfile], err);
			perror(0);
			return -1;
		}

		F->filesize = fileStatus.st_size;
		n += read(F->in, F->buffer+n, F->fetchsize-n);
	}
	
	if(n < ms->framebytes)
	{
		ms->readposition = -1;
		return -1;
	}
	
	if(n < F->fetchsize)
	{
		F->last = F->buffer + n;
	}

	return n;
}

static int mark5_stream_file_init(struct mark5_stream *ms)
{
	struct mark5_stream_file *F;
	char fn[64];

	F = (struct mark5_stream_file *)(ms->inputdata);

	strncpy(fn, F->files[0], 64);
	fn[63] = 0;
	sprintf(ms->streamname, "File-1/1=%s", fn);

	F->curfile = 0;
	F->buffer = 0;
	F->last = 0;
	F->buffer = 0;
	F->end = 0;
	F->fetchsize = 0;
	lseek64(F->in, F->offset, SEEK_SET);
	F->buffer = (uint8_t *)calloc(1, F->buffersize);
	ms->datawindow = F->buffer;
	ms->datawindowsize = F->buffersize;

	read(F->in, F->buffer, F->buffersize);

	return 0;
}

static int mark5_stream_file_next(struct mark5_stream *ms)
{
	struct mark5_stream_file *F;
	int nframes, status, nf;

	F = (struct mark5_stream_file *)(ms->inputdata);

	if(F->fetchsize == 0)	/* finish some initialization */
	{
		nframes = (F->buffersize)/ms->framebytes; 
		F->fetchsize = nframes*ms->framebytes;
		nf = (F->fetchsize-ms->frameoffset)/ms->framebytes;
		F->end = F->buffer + F->fetchsize;
		F->last = F->end;

		/* back up stream a bit to load whole frames */

		lseek64(F->in, 
			F->offset + ms->frameoffset + nf*ms->framebytes, 
			SEEK_SET);
	}

	/* usually this is all that needs to be done */
	ms->frame += ms->framebytes;
	
	if(ms->frame + ms->framebytes > F->end)
	{
		ms->frame = F->buffer;

		status = mark5_stream_file_fill(ms);
		if(status < 0)
		{
			return -1;
		}
	}

	if(ms->frame + ms->framebytes > F->last)
	{
		ms->readposition = -1;
		return -1;
	}

	/* successfully got new frame, so increment it */
	ms->framenum++;
	ms->readposition = 0;

	return ms->framebytes;
}

/* Work in progress */
static int mark5_stream_file_next_subframe(struct mark5_stream *ms)
{
	struct mark5_stream_file *F;
	/* int nframes, status; */

	F = (struct mark5_stream_file *)(ms->inputdata);
	
	if(F->fetchsize == 0)	/* finish some initialization */
	{
		
	}

	return 0;
}

static int mark5_stream_file_seek(struct mark5_stream *ms, int64_t framenum)
{
	struct mark5_stream_file *F;
	int64_t pos;
	int nframes, status;

	F = (struct mark5_stream_file *)(ms->inputdata);
	
	pos = framenum*ms->framebytes + ms->frameoffset;

	/* FIXME -- look into other files? */
	if(framenum < 0 || pos > F->filesize)	
	{
		return -1;
	}

	nframes = (F->buffersize)/ms->framebytes;
	F->fetchsize = nframes*ms->framebytes;
	F->end = F->buffer + F->fetchsize;
	F->last = F->end;
	ms->frame = F->buffer;

	lseek64(F->in, pos, SEEK_SET);

	status = mark5_stream_file_fill(ms);
	if(status < 0)
	{
		return -1;
	}

	return 0;
}

static int mark5_stream_file_final(struct mark5_stream *ms)
{
	struct mark5_stream_file *F;

	F = (struct mark5_stream_file *)(ms->inputdata);

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

struct mark5_stream_generic *new_mark5_stream_file(const char *filename,
	int64_t offset)
{
	struct mark5_stream_generic *V;
	struct mark5_stream_file *F;
	struct stat64 fileStatus;
	int in;
	int err;

	in = open64(filename, O_RDONLY);

	if(in < 0)
	{
		fprintf(stderr, "File cannot be opened (1) : <%s> : in = %d\n", 
			filename, in);
		perror(0);
		return 0;
	}
	err = fstat64(in, &fileStatus);
	if(err < 0)
	{
		fprintf(stderr, "Error looking at file (1) : <%s> : "
			"err = %d\n", filename, err);
		perror(0);
		close(in);
		return 0;
	}

	V = (struct mark5_stream_generic *)calloc(1,
		sizeof(struct mark5_stream_generic));
	F = (struct mark5_stream_file *)calloc(1,
		sizeof(struct mark5_stream_file));
	strcpy(F->files[0], filename);

	F->in = in;
	F->filesize = fileStatus.st_size;
	F->nfiles = 1;
	F->buffersize = F->filesize > 1<<20 ? 1<<20 : F->filesize;
	F->offset = offset;

	V->init_stream = mark5_stream_file_init;
	V->next = mark5_stream_file_next;
	V->seek = mark5_stream_file_seek;
	V->final_stream = mark5_stream_file_final;
	V->inputdata = F;

	return V;
}

int mark5_stream_file_add_infile(struct mark5_stream *ms, const char *filename)
{
	struct mark5_stream_file *F;
	char fn[64];

	if(ms->init_stream != mark5_stream_file_init)
	{
		fprintf(stderr, "mark5_stream_add_infile: "
			"Wrong stream type!\n");
		return -1;
	}

	F = (struct mark5_stream_file *)(ms->inputdata);

	if(F->nfiles < MAX_MARK5_STREAM_FILES)
	{
		strcpy(F->files[F->nfiles], filename);
		F->nfiles++;
		strncpy(fn, filename, 64);
		fn[64] = 0;
		sprintf(ms->streamname, "File-%d/%d=%-64s", F->curfile,
			F->nfiles, fn);
	
		return F->nfiles;
	}
	else
	{
		return -1;
	}
}
