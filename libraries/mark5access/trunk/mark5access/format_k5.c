/***************************************************************************
 *   Copyright (C) 2007-2011 by Walter Brisken                             *
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
#include "mark5access/mark5_stream.h"

#define NSUBMODE	2
#define MAXHEADERSIZE	32

const int k5headersize[NSUBMODE] = {8, 32};		/* bytes */
const int k5headersync[NSUBMODE] = {0x8B, 0x8C};
const char k5formatname[NSUBMODE][10] = {"K5", "K5_32"};
const int k5samprate[16] = {
	40000,
	100000,
	200000,
	500000,
	1000000,
	2000000,
	4000000,
	8000000,
	16000000,
	32000000,
	64000000,
	0,0,0,0,0
};

struct format_k5
{
	unsigned char header[MAXHEADERSIZE];
	int submode;
	int days;
};

static int findfirstframe(const unsigned char *data, int *submode)
{
	int  s;

	/* look for sync pattern */
	if(data[0] == 0xFF &&
	   data[1] == 0xFF &&
	   data[2] == 0xFF &&
	   data[3] == 0xFF)
	{
		for(s = 0; s < NSUBMODE; s++)
		{
			/* look for submode sync */
			if(data[7] == k5headersync[s])
			{
				if(submode)
				{
					*submode = s;
					return 0;
				}
			}
		}
	}

	return -1;
}

static int stream_frame_time_k5(const struct mark5_stream *ms,
	int *mjd, int *sec, double *ns)
{
	struct format_k5 *k;
	const unsigned char *buf;
	int day, year, y1;

	k = (struct format_k5 *)(ms->formatdata);
	
	if(ns)
	{
		*ns = 0.0;
	}
	if(sec)
	{
		*sec = k->header[4] + (k->header[5] << 8) +
			((k->header[6] & 0x01) << 16);
	}
	if(mjd)
	{
		if(k->submode == 1)
		{
			day = k->header[8] + ((k->header[9] & 0x01) << 8);
			year = 2000 + ((k->header[9] & 0x7e) >> 1);
			y1 = year-1;
			*mjd = day-678576+365*y1+y1/4-y1/100+y1/400;
		}
		else
		{
			*mjd = ms->mjd + k->days;
		}
	}

	return 0;
}

static int format_k5_fixmjd(struct mark5_stream *ms, int refmjd)
{
	if(ms)
	{
		ms->mjd = refmjd;
		return 1;
	}

	return -1;
}

/************************* decode routines **************************/

static int k5_decode_1bitstream_1bit(struct mark5_stream *ms, int nsamp,
	float **data)
{

	return 0;
}

static int k5_decode_1bitstream_2bit(struct mark5_stream *ms, int nsamp,
	float **data)
{

	return 0;
}

static int k5_decode_1bitstream_4bit(struct mark5_stream *ms, int nsamp,
	float **data)
{

	return 0;
}

static int k5_decode_1bitstream_8bit(struct mark5_stream *ms, int nsamp,
	float **data)
{

	return 0;
}

static int k5_decode_4bitstream_1bit(struct mark5_stream *ms, int nsamp,
	float **data)
{

	return 0;
}

static int k5_decode_4bitstream_2bit(struct mark5_stream *ms, int nsamp,
	float **data)
{

	return 0;
}

static int k5_decode_4bitstream_4bit(struct mark5_stream *ms, int nsamp,
	float **data)
{

	return 0;
}

static int k5_decode_4bitstream_8bit(struct mark5_stream *ms, int nsamp,
	float **data)
{

	return 0;
}

static int format_k5_make_formatname(struct mark5_stream *ms)
{
	struct format_k5 *k;

	k = (struct format_k5 *)(ms->formatdata);
	
	snprintf(ms->formatname, MARK5_STREAM_ID_LENGTH,
		"%s-%d-%d-%d", k5formatname[k->submode],
		ms->Mbps, ms->nchan, ms->nbit);

	return 0;
}

static int format_k5_init(struct mark5_stream *ms)
{
        struct format_k5 *k;
	int nbit, nchan, rate, Mbps, submode;
	int decoderindex = 0;
	double dns;

	if(!ms)
	{
		fprintf(m5stderr, "format_k5_init: ms = 0\n");
		return -1;
	}

	k = (struct format_k5 *)(ms->formatdata);

	ms->framegranularity = 1;

	if(ms->datawindow)
	{
		submode = -1;
		ms->frameoffset = findfirstframe(ms->datawindow, &submode);
		if(submode < 0 || submode >= NSUBMODE)
		{
			return -1;
		}

		if(k->submode >= 0)
		{
			if(k->submode != submode)
			{
				fprintf(m5stderr, "Warning: submode disagrees"
					" : %d != %d\n", submode, k->submode);
				return -1;
			}
		}
		else
		{
			k->submode = submode;
		}

		if(ms->frameoffset >= 0)
		{
			ms->format = MK5_FORMAT_K5;
		}
		else
		{
			return -1;
		}

		memcpy(k->header, ms->datawindow, k5headersize[k->submode]);

		ms->frame = ms->datawindow + ms->frameoffset;
		ms->payload = ms->frame + ms->payloadoffset;

		ms->gettime(ms, &ms->mjd, &ms->sec, &dns);
		ms->ns = (int)(dns + 0.5);

		nbit = 1 << (k->header[6] >> 6);
		nchan = (k->header[6] & 0x02) ? 4 : 1;
		rate = k5samprate[(k->header[6] >> 2) & 0x0F];
		Mbps = rate*nbit*nchan/1000000;

		if(ms->nbit <= 0)
		{
			ms->nbit = nbit;
		}
		if(ms->nchan <= 0)
		{
			ms->nchan = nchan;
		}
		if(ms->Mbps <= 0)
		{
			ms->Mbps = Mbps;
		}

		if(nbit != ms->nbit)
		{
			fprintf(m5stderr, "Warning: nbit disagrees : "
					"%d != %d\n", nbit, ms->nbit);
			return -1;
		}
		if(nchan != ms->nchan)
		{
			fprintf(m5stderr, "Warning: nchan disagrees : "
					"%d != %d\n", nchan, ms->nchan);
			return -1;
		}
		if(Mbps != ms->Mbps)
		{
			fprintf(m5stderr, "Warning: Mbps disagrees : "
					"%d != %d\n", Mbps, ms->Mbps);
			return -1;
		}

		if(nbit != ms->nbit || nchan != ms->nchan || Mbps != ms->Mbps)
		{
			return -1;
		}
	}

	ms->databytes = 1000000*ms->Mbps/8;
	ms->framebytes = ms->databytes + k5headersize[k->submode];
	ms->framesamples = 1000000*ms->Mbps/(ms->nbit*ms->nchan);
	ms->samprate = ms->framesamples;
	ms->framens = 1000000000.0;

	ms->samplegranularity = 8/(ms->nbit*ms->nchan);
	if(ms->samplegranularity < 1)
	{
		ms->samplegranularity = 1;
	}

	if(ms->nbit == 1)
	{
		decoderindex += 0;
	}
	else if(ms->nbit == 2)
	{
		decoderindex += 1;
	}
	else if(ms->nbit == 4)
	{
		decoderindex += 2;
	}
	else if(ms->nbit == 8)
	{
		decoderindex += 3;
	}
	else
	{
		fprintf(m5stderr, "format_k5_init : "
			"nbit needs to be 1, 2, 4, or 8\n");
		return 0;
	}

	if(ms->nchan == 1)
	{
		decoderindex += 0;
	}
	else if(ms->nchan == 4)
	{
		decoderindex += 4;
	}
	else
	{
		fprintf(m5stderr, "new_mark5_format_k5 : "
			"nchan needs to be 1 or 4\n");
		return 0;
	}

	ms->decode = 0;
	switch(decoderindex)
	{
		case 0 : ms->decode = k5_decode_1bitstream_1bit; break;
		case 1 : ms->decode = k5_decode_1bitstream_2bit; break;
		case 2 : ms->decode = k5_decode_1bitstream_4bit; break;
		case 3 : ms->decode = k5_decode_1bitstream_8bit; break;
		case 4 : ms->decode = k5_decode_4bitstream_1bit; break;
		case 5 : ms->decode = k5_decode_4bitstream_2bit; break;
		case 6 : ms->decode = k5_decode_4bitstream_4bit; break;
		case 7 : ms->decode = k5_decode_4bitstream_8bit; break;
	}
	ms->complex_decode = 0;

	ms->gframens = (int)(ms->framegranularity*ms->framens + 0.5);

	format_k5_make_formatname(ms);

	return 0;
}

static int format_k5_final(struct mark5_stream *ms)
{
	if(!ms)
	{
		return -1;
	}

	if(ms->formatdata)
	{
		free(ms->formatdata);
	}

	return 0;
}

static int one(const struct mark5_stream *ms)
{
	return 1;
}

struct mark5_format_generic *new_mark5_format_k5(int Mbps, int nchan, int nbit,
	int submode)
{
	struct format_k5 *k;
	struct mark5_format_generic *f;
	k = (struct format_k5 *)calloc(1, sizeof(struct format_k5));
	f = (struct mark5_format_generic *)calloc(1, 
	        sizeof(struct mark5_format_generic));

	k->days = 0;
	k->submode = submode;
	memset(k->header, 0, 32);

	f->Mbps = Mbps;
	f->nchan = nchan;
	f->nbit = nbit;
	f->formatdata = k;
	f->formatdatasize = sizeof(struct mark5_format_k5);
	f->gettime = stream_frame_time_k5;
	f->init_format = format_k5_init;
	f->final_format = format_k5_final;
	f->fixmjd = format_k5_fixmjd;
	f->validate = one;
	f->resync = one;
	f->decode = k5_decode_1bitstream_1bit;

	return f;
}
