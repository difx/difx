/***************************************************************************
 *   Copyright (C) 2006-2014 by Walter Brisken                             *
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

#include "config.h"

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
	long long offset;
	long long filesize;
	char files[MAX_MARK5_STREAM_FILES][MARK5_STREAM_ID_LENGTH];
	int nfiles;
	int buffersize;

	int curfile;
	int fetchsize;
	int in;
	unsigned char *buffer;
	unsigned char *end;
	unsigned char *last;
};

/* loads fetchsize bytes into memory */
static int mark5_stream_file_fill(struct mark5_stream *ms, int offset, int length)
{
	struct mark5_stream_file *F;
	struct stat fileStatus;
	int n;
	int err;
	unsigned char *buffer;
	
	F = (struct mark5_stream_file *)(ms->inputdata);

	buffer = F->buffer + offset;

	n = read(F->in, buffer, length);
	
	if(F->in == 0)
	{
		/* handle short reads specially for stdin */
		while(n < length)
		{
			int p;

			p = read(F->in, buffer+n, length-n);

			if(p <= 0)
			{
				return -1;
			}

			n += p;
		}
	}
	else
	{
		while(n < length)
		{
			if(F->in >= 0)
			{
				close(F->in);
			}
			F->in = -1;
			F->curfile++;
			
			snprintf(ms->streamname, MARK5_STREAM_ID_LENGTH, "File-%d/%d=%s", F->curfile, F->nfiles, F->files[F->curfile]);
			if(F->curfile >= F->nfiles)
			{
				break;
			}
			F->in = open(F->files[F->curfile], O_RDONLY);
			if(F->in < 0)
			{
				fprintf(m5stderr, "File cannot be opened (2) : <%s> : in = %d\n", F->files[F->curfile], F->in);
				perror(0);

				return -1;
			}
			err = fstat(F->in, &fileStatus);
			if(err < 0)
			{
				fprintf(m5stderr, "Error looking at file (2) : <%s> : err = %d\n", F->files[F->curfile], err);
				perror(0);

				return -1;
			}

			F->filesize = fileStatus.st_size;
			n += read(F->in, buffer+n, length-n);
		}
	}
	
	if(n < ms->framebytes)
	{
		ms->readposition = -1;

		return -1;
	}
	
	if(n < length)
	{
		F->last = buffer + n;
	}

	return n;
}

static int mark5_stream_file_init(struct mark5_stream *ms)
{
	struct mark5_stream_file *F;
	int r;

	F = (struct mark5_stream_file *)(ms->inputdata);

	snprintf(ms->streamname, MARK5_STREAM_ID_LENGTH, "File-1/1=%s", F->files[0]);

	F->curfile = 0;
	F->buffer = 0;
	F->last = 0;
	F->buffer = 0;
	F->end = 0;
	F->fetchsize = 0;
	F->buffer = (unsigned char *)calloc(1, F->buffersize);

	if(F->offset > 0)
	{
		/* Perform jump into data source */

		if(F->in == 0)
		{
			/* seek by reading if stdin */
			long long togo = F->offset;
			ssize_t nr;

			while(togo > 0)
			{
				nr = read(F->in, F->buffer, (togo >= F->buffersize) ? F->buffersize : togo);
				if(nr > 0)
				{
					togo -= nr;
				}
				else
				{
					fprintf(m5stderr, "mark5_stream_file_init: <stdin> seek reached EOF\n");

					return -1;
				}
			}
		}
		else
		{
			if(F->offset != lseek(F->in, F->offset, SEEK_SET))
			{
				fprintf(m5stderr, "mark5_stream_file_init: seek reached EOF\n");

				return -1;
			}
		}
	}


	ms->datawindow = F->buffer;
	ms->datawindowsize = F->buffersize;

	/* only load half a buffer-full to start with */
	r = read(F->in, F->buffer, F->buffersize/2);
	if(r < F->buffersize/2)
	{
		fprintf(m5stderr, "mark5_stream_file_init: Initial read of %d was short (%d bytes actually read).  Shortening datawindowsize\n", F->buffersize, r);
		ms->datawindowsize = F->buffersize = r;
	}

	return 0;
}

static int mark5_stream_file_next(struct mark5_stream *ms)
{
	struct mark5_stream_file *F;

	F = (struct mark5_stream_file *)(ms->inputdata);

	if(F->fetchsize == 0)	/* finish some initialization */
	{
		/* only F->buffersize/2 were loaded so far. */
		/* now load exactly enough to end read on the frame boundary just before or at end of buffer */

		int nload;
		int nframes;
		int status;
		int l;	/* amount needed to finish partial frame at end of first load */

		nframes = (F->buffersize)/ms->framebytes; 
		F->fetchsize = nframes*ms->framebytes;
		F->end = F->buffer + F->fetchsize;
		F->last = F->end;

		l = (F->buffersize/2 - ms->frameoffset) % ms->framebytes;
		if(l > 0)
		{
			l = ms->framebytes - l;
		}

		nload = l + ((F->buffersize/2 - l)/ms->framebytes)*ms->framebytes;

		status = mark5_stream_file_fill(ms, F->buffersize/2, nload);
		if(status < 0)
		{
			return -1;
		}
	}

	/* usually this is all that needs to be done */
	ms->frame += ms->framebytes;
	
	if(ms->frame + ms->framebytes > F->end)
	{
		int status;

		ms->frame = F->buffer;

		status = mark5_stream_file_fill(ms, 0, F->fetchsize);
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
	++ms->framenum;
	ms->readposition = 0;

	return ms->framebytes;
}

static int mark5_stream_file_seek(struct mark5_stream *ms, long long framenum)
{
	struct mark5_stream_file *F;
	off_t pos, sook;
	int nframes, status;

	F = (struct mark5_stream_file *)(ms->inputdata);
	
	pos = framenum*ms->framebytes + ms->frameoffset;

#warning "FIXME: look into other files?"
	if(framenum < 0 || pos > F->filesize)	
	{
		return -1;
	}

	nframes = (F->buffersize)/ms->framebytes;
	F->fetchsize = nframes*ms->framebytes;
	F->end = F->buffer + F->fetchsize;
	F->last = F->end;
	ms->frame = F->buffer;

	sook = lseek(F->in, pos, SEEK_SET);
	if(sook < 0)
	{
		fprintf(stderr, "Seek error: pos=%Ld\n", (long long int)pos);

		return -1;
	}

	status = mark5_stream_file_fill(ms, 0, F->fetchsize);
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

	if(F->in >= 0)
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

struct mark5_stream_generic *new_mark5_stream_file(const char *filename, long long offset)
{
	struct mark5_stream_generic *V;
	struct mark5_stream_file *F;
	struct stat fileStatus;
	int in;
	int err;

	if(strcmp(filename, "-") == 0)
	{
		in = 0; /* stdin */
	}
	else
	{
		in = open(filename, O_RDONLY);
	}

	if(in < 0)
	{
		fprintf(m5stderr, "File cannot be opened (1) : <%s> : in = %d\n", filename, in);
		perror(0);

		return 0;
	}
	err = fstat(in, &fileStatus);
	if(err < 0)
	{
		fprintf(m5stderr, "Error looking at file (1) : <%s> : " "err = %d\n", filename, err);
		perror(0);
		if(in > 0)
		{
			close(in);
		}

		return 0;
	}

	V = (struct mark5_stream_generic *)calloc(1, sizeof(struct mark5_stream_generic));
	F = (struct mark5_stream_file *)calloc(1, sizeof(struct mark5_stream_file));

	F->in = in;
	if(in == 0)
	{
		F->filesize = 1LL<<61;
		F->buffersize = 1<<19;
		snprintf(F->files[0], MARK5_STREAM_ID_LENGTH, "%s", "<stdin>");
	}
	else
	{
		F->filesize = fileStatus.st_size;
		F->buffersize = F->filesize > 1<<20 ? 1<<20 : F->filesize;
		snprintf(F->files[0], MARK5_STREAM_ID_LENGTH, "%s", filename);
	}
	F->nfiles = 1;
	F->offset = offset;

	V->init_stream = mark5_stream_file_init;
	V->next = mark5_stream_file_next;
	V->seek = mark5_stream_file_seek;
	V->final_stream = mark5_stream_file_final;
	V->inputdata = F;
	V->inputdatasize = sizeof(struct mark5_stream_file);

	return V;
}

int mark5_stream_file_add_infile(struct mark5_stream *ms, const char *filename)
{
	struct mark5_stream_file *F;

	if(ms->init_stream != mark5_stream_file_init)
	{
		fprintf(m5stderr, "mark5_stream_add_infile: " "Wrong stream type!\n");

		return -1;
	}

	F = (struct mark5_stream_file *)(ms->inputdata);

	if(F->nfiles < MAX_MARK5_STREAM_FILES)
	{
		snprintf(F->files[F->nfiles], MARK5_STREAM_ID_LENGTH, "%s", filename);
		F->nfiles++;
		snprintf(ms->streamname, MARK5_STREAM_ID_LENGTH, "File-%d/%d=%s", F->curfile, F->nfiles, filename);
	
		return F->nfiles;
	}
	else
	{
		return -1;
	}
}
