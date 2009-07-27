/***************************************************************************
 *   Copyright (C) 2006, 2007, 2008, 2009 by Walter Brisken                *
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

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "mark5access/mark5_stream.h"

static void mark5_stream_blank_frame(struct mark5_stream *ms)
{
	int z;

	for(z = 0; z < MAXBLANKZONES; z++)
	{
		ms->blankzonestartvalid[z] = 1<<30;
		ms->blankzoneendvalid[z] = 0;
	}
}

int mark5_stream_next_frame(struct mark5_stream *ms)
{
	int n;
	int v = 1;

	/* call specialized function to ready next frame */
	n = ms->next(ms);

	/* are we at end of file(s)? */
	if(n < 0)
	{
		ms->payload = 0;
		
		return -1;
	}
	
	if(ms->frame)
	{
		/* validate frame */
		v = ms->validate(ms);
		if(!v)
		{
			ms->nvalidatefail++;
			ms->consecutivefails++;
		}
		else
		{
			ms->nvalidatepass++;
			ms->consecutivefails = 0;
		}
	}
	
	/* blank bad data if any */
	if(v)
	{
		ms->blanker(ms);
	}
	else /* blank entire frame if validity check fails */
	{
		mark5_stream_blank_frame(ms);
	}

	/* set payload pointer to point to start of actual data */
	if(ms->frame)
	{
		ms->payload = ms->frame + ms->payloadoffset;
	}

	return 0;
}

static int set_stream(struct mark5_stream *ms, 
	const struct mark5_stream_generic *s)
{
	if(s && ms)
	{
		ms->init_stream = s->init_stream;
		ms->final_stream = s->final_stream;
		ms->next = s->next;
		ms->seek = s->seek;
		ms->inputdata = s->inputdata;

		if(!s->init_stream || !s->next)
		{
			return -1;
		}

		return 0;
	}

	return -1;
}

static int set_format(struct mark5_stream *ms, 
	const struct mark5_format_generic *f)
{
	if(f && ms)
	{
		ms->init_format = f->init_format;
		ms->final_format = f->final_format;
		ms->decode = f->decode;
		ms->validate = f->validate;
		ms->gettime = f->gettime;
		ms->fixmjd = f->fixmjd;
		ms->formatdata = f->formatdata;
		ms->Mbps = f->Mbps;
		ms->nchan = f->nchan;
		ms->nbit = f->nbit;
		ms->decimation = f->decimation;

		if(!f->init_format || !f->decode || !f->gettime)
		{
			return -1;
		}
		
		return 0;
	}

	return -1;
}

static int copy_format(const struct mark5_stream *ms,
	struct mark5_format *mf)
{
	mf->frameoffset = ms->frameoffset;
	mf->framebytes  = ms->framebytes;
	mf->databytes   = ms->databytes;
	mf->framens     = ms->framens;
	mf->mjd         = ms->mjd;
	mf->sec         = ms->sec;
	mf->ns          = ms->ns;
	mf->Mbps        = ms->Mbps;
	mf->nchan       = ms->nchan;
	mf->nbit        = ms->nbit;
	mf->decimation	= ms->decimation;

	return 0;
}

static int mark5_format_init(struct mark5_stream *ms)
{
	ms->framenum = 0;
	ms->readposition = 0;
	ms->frame = 0;
	ms->payload = 0;
	ms->framens = 0.0;
	ms->samprate = 0;
	ms->mjd = 0;
	ms->sec = 0;
	ms->ns = 0;

	return ms->init_format(ms);
}

/* Compatibility function */
struct mark5_stream *mark5_stream_open(const char *filename, 
	int nbit, int fanout, int64_t offset)
{
	struct mark5_stream_generic *s;
	struct mark5_format_generic *f;
	struct mark5_stream *ms;
	int status, ntrack;
	
	s = new_mark5_stream_file(filename, offset);
	if(!s)
	{
		return 0;
	}
	
	ms = (struct mark5_stream *)calloc(1, sizeof(struct mark5_stream));
	
	if(set_stream(ms, s) < 0)
	{
		fprintf(stderr, "mark5_stream_open: Incomplete stream.\n");
		
		return 0;
	}

	status = s->init_stream(ms);
	if(status < 0)
	{
		delete_mark5_stream(ms);
		fprintf(stderr, "mark5_open_stream: init_stream() failed\n");
		
		return 0;
	}

	/* Now go through known formats, looking for a match with the data */
	
	/* VLBA modes */
	for(ntrack = 8; ntrack <= 64; ntrack*=2)
	{
		f = new_mark5_format_vlba(0, ntrack/(nbit*fanout), 
			nbit, fanout, 1);
		set_format(ms, f);
		status = mark5_format_init(ms);
		if(status < 0)
		{
			if(f->final_format)
			{
				f->final_format(ms);
			}
			free(f);
		}
		else
		{
			strcat(ms->formatname, "-Auto");
			return ms;
		}
	}
	
	/* Mark4 modes */
	for(ntrack = 8; ntrack <= 64; ntrack*=2)
	{
		f = new_mark5_format_mark4(0, ntrack/(nbit*fanout), 
			nbit, fanout, 1);
		set_format(ms, f);
		status = mark5_format_init(ms);
		if(status < 0)
		{
			if(f->final_format)
			{
				f->final_format(ms);
			}
			free(f);
		}
		else
		{
			strcat(ms->formatname, "-Auto");
			return ms;
		}
	}
	
	/* No match found */
	delete_mark5_stream(ms);

	return 0;
}

struct mark5_format_generic *new_mark5_format_generic_from_string(
	const char *formatname)
{
	int a, b, c, d, e, r;

	if(strncasecmp(formatname, "VLBA1_", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
		if(r < 4)
		{
			return 0;
		}
		if(r < 5)
		{
			e = 1;
		}

		return new_mark5_format_vlba(b, c, d, a, e);
	}
	else if(strncasecmp(formatname, "MKIV1_", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
		if(r < 4)
		{
			return 0;
		}
		if(r < 5)
		{
			e = 1;
		}

		return new_mark5_format_mark4(b, c, d, a, e);
	}
	else if(strncasecmp(formatname, "Mark5B-", 7) == 0)
	{
		r = sscanf(formatname+7, "%d-%d-%d/%d", &a, &b, &c, &e);
		if(r < 3)
		{
			return 0;
		}
		if(r < 4)
		{
			e = 1;
		}

		return new_mark5_format_mark5b(a, b, c, e);
	}
	else if(strncasecmp(formatname, "K5_32-", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d/%d", &a, &b, &c, &e);
		if(r < 3)
		{
			return 0;
		}
		if(r < 4)
		{
			e = 1;
		}

		return new_mark5_format_k5(a, b, c, e);
	}
	else if(strncasecmp(formatname, "K5-", 3) == 0)
	{
		r = sscanf(formatname+3, "%d-%d-%d/%d", &a, &b, &c, &e);
		if(r < 3)
		{
			return 0;
		}
		if(r < 4)
		{
			e = 1;
		}

		return new_mark5_format_k5(a, b, c, e);
	}
	else
	{
		fprintf(stderr, "Unknown format : %s\n", formatname);

		return 0;
	}
}

/* a string containg a list of supported formats */
const char *mark5_stream_list_formats()
{
	return "VLBA1_*-*-*-*[/*], MKIV1_*-*-*-*[/*], MARK5B-*-*-*[/*]";
}
                                                                                /* given a format string, populate a structure with info about format */
struct mark5_format *new_mark5_format_from_name(const char *formatname)
{
	int a=1, b=0, c=0, d=0, e=0, ntrack=0;
	int databytes, framebytes;
	double framens;
	struct mark5_format *f;
	enum Mark5Format F;
	int decimation = 1;
	int r;

	if(strncasecmp(formatname, "VLBA1_", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
		if(r < 4)
		{
			return 0;
		}
		F = MK5_FORMAT_VLBA;
		databytes = 2500*a*c*d;
		framebytes = 2520*a*c*d;
		framens = 1000*((20000*a*c*d)/b);
		if(r > 4)
		{
			decimation = e;
		}
		ntrack = a*c*d;
	}
	else if(strncasecmp(formatname, "MKIV1_", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
		if(r < 4)
		{
			return 0;
		}
		F = MK5_FORMAT_MARK4;
		databytes = 2500*a*c*d;
		framebytes = databytes;
		framens = 1000*((20000*a*c*d)/b);
		if(r > 4)
		{
			decimation = e;
		}
		ntrack = a*c*d;
	}
	else if(strncasecmp(formatname, "Mark5B-", 7) == 0)
	{
		r = sscanf(formatname+7, "%d-%d-%d/%d", &b, &c, &d, &e);
		if(r < 3)
		{
			return 0;
		}
		F = MK5_FORMAT_MARK5B;
		databytes = 10000;
		framebytes = databytes+16;
		framens = 1000.0*(8.0*databytes/(double)b);
		if(r > 3)
		{
			decimation = e;
		}
	}
	else if(strncasecmp(formatname, "K5_32-", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d/%d", &b, &c, &d, &e);
		if(r < 3)
		{
			return 0;
		}
		F = MK5_FORMAT_K5;
		databytes = 1000000*b/8;
		framebytes = 32 + databytes;
		framens = 1000000000;
		if(r > 3)
		{
			decimation = e;
		}
	}
	else if(strncasecmp(formatname, "K5-", 3) == 0)
	{
		r = sscanf(formatname+3, "%d-%d-%d/%d", &b, &c, &d, &e);
		if(r < 3)
		{
			return 0;
		}
		F = MK5_FORMAT_K5;
		databytes = 1000000*b/8;
		framebytes = databytes + 8;
		framens = 1000000000;
		if(r > 3)
		{
			decimation = e;
		}
	}
	/* for VDIF, the datasize per packet must be supplied as first numeric element:
	 * e.g., VDIF_4000-2048-4-2
	 */
	else if(strncasecmp(formatname, "VDIF_", 5) == 0)
	{
		r = sscanf(formatname+5, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
		if(r < 4)
		{
			return 0;
		}
		F = MK5_FORMAT_VDIF;
		databytes = a;
		framebytes = databytes + 32;
		framens = 1000.0*(8.0*databytes/(double)b);
		if(r > 4)
		{
			decimation = e;
		}
	}
	/* for VDIF with legacy (16 byte) headers, a different name is used: */
	else if(strncasecmp(formatname, "VDIFL_", 6) == 0)
	{
		r = sscanf(formatname+6, "%d-%d-%d-%d/%d", &a, &b, &c, &d, &e);
		if(r < 4)
		{
			return 0;
		}
		F = MK5_FORMAT_VDIF;
		databytes = a;
		framebytes = databytes + 16;
		framens = 1000.0*(8.0*databytes/(double)b);
		if(r > 4)
		{
			decimation = e;
		}
	}
	else
	{
		return 0;
	}

	f = (struct mark5_format *)calloc(1, sizeof(struct mark5_format));
	f->format = F;
	f->fanout = a;
	f->Mbps = b;
	f->nchan = c;
	f->nbit = d;
	f->framebytes = framebytes;
	f->databytes = databytes;
	f->ntrack = ntrack;
	f->framens = framens;
	f->decimation = decimation;

	return f;
}

struct mark5_format *new_mark5_format_from_stream(
	struct mark5_stream_generic *s)
{
	struct mark5_stream *ms;
	struct mark5_format_generic *f;
	struct mark5_format *mf;
	int status, ntrack;
	
	if(!s)
	{
		return 0;
	}
	
	mf = (struct mark5_format *)calloc(1, sizeof(struct mark5_format));
	
	ms = (struct mark5_stream *)calloc(1, sizeof(struct mark5_stream));
	
	if(set_stream(ms, s) < 0)
	{
		fprintf(stderr, "new_mark5_format_from_stream: "
				"Incomplete stream.\n");
		
		return 0;
	}

	status = s->init_stream(ms);
	if(status < 0)
	{
		delete_mark5_stream(ms);
		fprintf(stderr, "new_mark5_format_from_stream: "
				"init_stream() failed\n");
		
		return 0;
	}

	/* Now go through known formats, looking for a match with the data */
	
	/* VLBA modes */
	for(ntrack = 8; ntrack <= 64; ntrack*=2)
	{
		f = new_mark5_format_vlba(0, ntrack, 1, 1, 1);
		set_format(ms, f);
		status = mark5_format_init(ms);
		if(status < 0)
		{
			if(f->final_format)
			{
				f->final_format(ms);
			}
			free(f);
		}
		else
		{
			copy_format(ms, mf);
			mf->format = MK5_FORMAT_VLBA;
			mf->ntrack = ntrack;
			mf->fanout = ntrack/(ms->nbit*ms->nchan);
			delete_mark5_stream(ms);
			
			return mf;
		}
	}
	
	/* Mark4 modes */
	for(ntrack = 8; ntrack <= 64; ntrack*=2)
	{
		f = new_mark5_format_mark4(0, ntrack, 1, 1, 1);
		set_format(ms, f);
		status = mark5_format_init(ms);
		if(status < 0)
		{
			if(f->final_format)
			{
				f->final_format(ms);
			}
			free(f);
		}
		else
		{
			copy_format(ms, mf);
			mf->format = MK5_FORMAT_MARK4;
			mf->ntrack = ntrack;
			mf->fanout = ntrack/(ms->nbit*ms->nchan);
			delete_mark5_stream(ms);
			
			return mf;
		}
	}

	/* Mark5b */
	f = new_mark5_format_mark5b(0, 16, 2, 1);
	set_format(ms, f);
	status = mark5_format_init(ms);
	if(status < 0)
	{
		if(f->final_format)
		{
			f->final_format(ms);
		}
		free(f);
	}
	else
	{
		copy_format(ms, mf);
		mf->format = MK5_FORMAT_MARK5B;
		mf->ntrack = 0;
		delete_mark5_stream(ms);
		
		return mf;
	}

	/* k5 */
	f = new_mark5_format_k5(0, 0, 0, -1);
	set_format(ms, f);
	status = mark5_format_init(ms);
	if(status < 0)
	{
		if(f->final_format)
		{
			f->final_format(ms);
		}
		free(f);
	}
	else
	{
		copy_format(ms, mf);
		mf->format = MK5_FORMAT_K5;
		mf->ntrack = 0;
		delete_mark5_stream(ms);
		
		return mf;
	}

	
	/* No match found */
	free(mf);
	free(ms);

	return 0;
}

void print_mark5_format(const struct mark5_format *mf)
{
	printf("mark5_format : %p\n", mf);
	if(!mf)
	{
		return;
	}
	printf("  Mbps = %d\n", mf->Mbps);
	printf("  nchan = %d\n", mf->nchan);
	printf("  nbit = %d\n", mf->nbit);
	printf("  frameoffset = %d\n", mf->frameoffset);
	printf("  framebytes = %d\n", mf->framebytes);
	printf("  framens = %f\n", mf->framens);
	printf("  mjd = %d sec = %d ns = %d\n", mf->mjd, mf->sec, mf->ns);
	printf("  ntrack = %d\n", mf->ntrack);
	printf("  fanout = %d\n", mf->fanout);
	printf("  decimation = %d\n", mf->decimation);
}

void delete_mark5_format(struct mark5_format *mf)
{
	if(mf)
	{
		free(mf);
	}
}

struct mark5_stream *new_mark5_stream(struct mark5_stream_generic *s, 
	struct mark5_format_generic *f)
{
	struct mark5_stream *ms;
	int status;
	int failed = 0;

	ms = (struct mark5_stream *)calloc(1, sizeof(struct mark5_stream));
       	if(!ms)
	{
		fprintf(stderr, "Error allocating memory for mark5_stream\n");
		
		return 0;
	}

	ms->format = MK5_FORMAT_UNKNOWN;
	
	if(set_stream(ms, s) < 0)
	{
		fprintf(stderr, "new_mark5_stream: Incomplete stream.\n");
		failed = 1;
	}
	if(set_format(ms, f) < 0)
	{
		fprintf(stderr, "new_mark5_stream: Incomplete format.\n");
		failed = 1;
	}

	ms->log2blankzonesize = 30;
	ms->blanker = blanker_none;

	if(failed)
	{
		if(f)
		{
			if(f->final_format)
			{
				f->final_format(ms);
			}
			free(f);
		}
		free(ms);
		
		return 0;
	}

	status = s->init_stream(ms);
	if(status < 0)
	{
		delete_mark5_stream(ms);
		fprintf(stderr, "new_mark5_format: init_stream(%s) failed\n",
			ms->formatname);
		
		return 0;
	}

	status = mark5_format_init(ms);
	if(status < 0)
	{
		fprintf(stderr, "new_mark5_stream: init_format(%s) failed\n",
			ms->formatname);
		delete_mark5_stream(ms);
		
		return 0;
	}

	ms->blanker(ms);

	return ms;
}

void delete_mark5_stream(struct mark5_stream *ms)
{
	if(ms)
	{
		if(ms->nvalidatefail > 0)
		{
			printf("Warning: %d validation failures on %s framenum=%Ld -> bytepos=%Ld\n",
				ms->nvalidatefail, ms->streamname, ms->framenum, ms->framenum*ms->framebytes);
		}
		if(ms->final_stream)
		{
			ms->final_stream(ms);
		}
		if(ms->final_format)
		{
			ms->final_format(ms);
		}
		free(ms);
	}
}

int mark5_stream_get_frame_time(struct mark5_stream *ms, 
	int *mjd, int *sec, double *ns)
{
	if(!ms)
	{
		return -1;
	}

	return ms->gettime(ms, mjd, sec, ns);
}

int mark5_stream_get_sample_time(struct mark5_stream *ms, 
	int *mjd, int *sec, double *ns)
{
	int status;

	if(!ms)
	{
		return -1;
	}
	status = ms->gettime(ms, mjd, sec, ns);

	if(status < 0)
	{
		return status;
	}

	if(ns)
	{
		*ns += (ms->framens/ms->databytes)*ms->readposition;
	}

	return 0;
}

int mark5_stream_print(const struct mark5_stream *ms)
{
	printf("Mark5 stream: %p\n", ms);
	if(!ms)
	{
		return -1;
	}
	printf("  stream = %s\n", ms->streamname);
	printf("  format = %s = %d\n", ms->formatname, ms->format);
	if(ms->mjd >= 0)
	{
		printf("  start mjd/sec = %d %05d.%09d\n", 
			ms->mjd, ms->sec, ms->ns);
		printf("  frame duration = %8.2f ns\n", ms->framens);
		printf("  framenum = %Ld\n", ms->framenum);
	}
	if(ms->samprate > 0)
	{
		printf("  sample rate = %d Hz\n", ms->samprate);
	}
	printf("  offset = %d\n", ms->frameoffset);
	printf("  framebytes = %d bytes\n", ms->framebytes);
	printf("  datasize = %d bytes\n", ms->databytes);
	printf("  sample granularity = %d\n", ms->samplegranularity);
	printf("  frame granularity = %d\n", ms->framegranularity);
	printf("  gframens = %d\n", ms->gframens);
	printf("  payload offset = %d\n", ms->payloadoffset);
	printf("  read position = %d\n", ms->readposition);
	if(ms->datawindow)
	{
		printf("  data window size = %Ld bytes\n", ms->datawindowsize);
	}

	return 0;
}

int mark5_stream_seek(struct mark5_stream *ms, int mjd, int sec, double ns)
{
	int status;
	double jumpns;
	int64_t n;

	if(!ms)
	{
		return -1;
	}
	if(ms->seek)
	{
		jumpns = 86400000000000LL*(mjd - ms->mjd) 
		       + 1000000000LL*(sec - ms->sec)
		       + (ns - ms->ns);

		if(jumpns < 0) /* before start of stream */
		{
			return -1;
		}
		n = jumpns / ms->framens;

		status = ms->seek(ms, n);

		if(status < 0)
		{
			return -1;
		}

		ms->framenum = n;

		/* FIXME -- validate here? */

		return 0;
	}
	else
	{
		return -1;
	}
}

int mark5_stream_copy(struct mark5_stream *ms, int nbytes, char *data)
{
	int nleft, q;

	if(!ms)
	{
		return -1;
	}
	if(ms->readposition < 0)
	{
		return -1;
	}

	q = ms->samplegranularity*ms->nchan*ms->nbit*ms->decimation/8;
	if(nbytes % q != 0)
	{
		return -1;
	}

	while(1)
	{
		nleft = ms->databytes - ms->readposition;
		if(nleft > nbytes)
		{
			memcpy(data, ms->payload+ms->readposition, nbytes);
			return 0;
		}
		memcpy(data, ms->payload+ms->readposition, nleft);
		if(ms->next(ms) < 0)
		{
			if(nbytes == nleft)
			{
				return 0;
			}
			else
			{
				return -1;
			}
		}
		nbytes -= nleft;
		data += nleft;
	}

	return 0;
}

int mark5_stream_set_blanker(struct mark5_stream *ms, 
	enum Mark5Blanker blanker)
{
	switch(blanker)
	{
	case MK5_BLANKER_NONE:
		ms->blanker = blanker_none;
		break;
	case MK5_BLANKER_MARK5:
		ms->blanker = blanker_mark5;
		break;
	default:
		return -1;
	}

	return ms->blanker(ms);
}

/*********************** data decode routines **********************/


int mark5_stream_decode(struct mark5_stream *ms, int nsamp, float **data)
{
	if(!ms)
	{
		return -1;
	}
	if(ms->readposition < 0)
	{
		return -1;
	}

	/* In order to ensure expected behavior, must unpack multiples of
	 * ms->samplegranularity
	 */
	if(nsamp % ms->samplegranularity != 0)
	{
		return -1;
	}
	return ms->decode(ms, nsamp, data);
}

int mark5_stream_decode_double(struct mark5_stream *ms, int nsamp, 
	double **data)
{
	double *d;
	float *f;
	int c;
	int i;
	int r;
	
	r = mark5_stream_decode(ms, nsamp, (float **)data);
	if(r < 0) 
	{
		return r;
	}

	/* convert in place */
	for(c = 0; c < ms->nchan; c++)
	{
		d = data[c]+nsamp;
		f = ((float *)(data[c]))+nsamp;
		for(i = 0; i < nsamp; i++)
		{
			d--;
			f--;
			*d = *f;
		}
	}

	return r;
}

int mark5_stream_decode_complex(struct mark5_stream *ms, int nsamp, 
	mark5_float_complex **data)
{
	mark5_float_complex *fc;
	float *f;
	int c;
	int i;
	int r;
	
	r = mark5_stream_decode(ms, nsamp, (float **)data);
	if(r < 0) 
	{
		return r;
	}

	/* convert in place */
	for(c = 0; c < ms->nchan; c++)
	{
		fc = data[c]+nsamp;
		f = ((float *)(data[c]))+nsamp;
		for(i = 0; i < nsamp; i++)
		{
			fc--;
			f--;
			fc->re = *f;
			fc->im = 0.0;
		}
	}

	return r;
}

int mark5_stream_decode_double_complex(struct mark5_stream *ms, int nsamp, 
	mark5_double_complex **data)
{
	mark5_double_complex *dc;
	float *f;
	int c;
	int i;
	int r;
	
	r = mark5_stream_decode(ms, nsamp, (float **)data);
	if(r < 0) 
	{
		return r;
	}

	/* convert in place */
	for(c = 0; c < ms->nchan; c++)
	{
		dc = data[c]+nsamp;
		f = ((float *)(data[c]))+nsamp;
		for(i = 0; i < nsamp; i++)
		{
			dc--;
			f--;
			dc->re = *f;
			dc->im = 0.0;
		}
	}

	return r;
}


/* Returns: -1 on error
 *           0 on success with no change
 *          >0 on success with change
 */
int mark5_stream_fix_mjd(struct mark5_stream *ms, int refmjd)
{
	if(!ms)
	{
		return -1;
	}
	if(ms->fixmjd)
	{
		return ms->fixmjd(ms, refmjd);
	}
	else
	{
		return 0;
	}
}

