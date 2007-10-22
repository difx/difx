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
#include "mark5access/mark5_stream.h"

const uint32_t mark5bSync       = 0xABADDEED;
const uint32_t mark5cCompatSync = 0xF00FF00F;	/* FIXME: replace with actual */

/* the high mag value for 2-bit reconstruction */
static const float HiMag = 3.3359;

struct mark5_format_mark5b
{
	int nbitstream;
	int kday;	/* kilo-mjd -- ie 51000, 52000, ... */
};

float lut1bit[256][8];
float lut2bit[256][4];


static void initluts()
{
	int b, i, s, m, l;
	const float lut2level[2] = {1.0, -1.0};
	const float lut4level[4] = {-HiMag, 1.0, -1.0, HiMag};

	for(b = 0; b < 256; b++)
	{
		/* lut1bit */
		for(i = 0; i < 8; i++)
		{
			l = (b>>i)&1;
			lut1bit[b][i] =  lut2level[l];
		}

		/* lut2bit */
		for(i = 0; i < 4; i++)
		{
			s = i*2;	/* 0, 2, 4, 6 */
			m = s+1;	/* 1, 3, 5, 7 */
			l = ((b>>s)&1) + (((b>>m)&1)<<1);
			lut2bit[b][i] =  lut4level[l];
		}
	}
}

static int findfirstframe(const uint8_t *data, int bytes, uint32_t syncword)
{
	int i;
	uint8_t sb0, sb1, sb2, sb3;

	sb0 = (syncword >>  0) & 0xFF;
	sb1 = (syncword >>  8) & 0xFF;
	sb2 = (syncword >> 16) & 0xFF;
	sb3 = (syncword >> 24) & 0xFF;

	bytes -= 10020; 
	if(bytes < 0)	/* enough bytes to decode? */
	{
		return -1;
	}
	
	/* look for two consecutive frame sync words */
	for(i = 0; i < bytes; i++)
	{
		if(data[0]     == sb0 &&
		   data[1]     == sb1 &&
		   data[2]     == sb2 &&
		   data[3]     == sb3 &&
		   data[10016] == sb0 &&
		   data[10017] == sb1 &&
		   data[10018] == sb2 &&
		   data[10019] == sb3    )
		{
			return i;
		}
		data++;
	}

	return -1;
}

static int mark5_stream_frame_time_mark5b(const struct mark5_stream *ms,
	int *mjd, int *sec, int *ns)
{
	struct mark5_format_mark5b *m;
	const uint8_t *buf;
	int i;
	uint8_t nibs[16];

	m = (struct mark5_format_mark5b *)(ms->formatdata);

	buf = ms->frame + 8;

	for(i = 0; i < 4; i++)
	{
		nibs[2*i+0] = buf[3-i] >> 4;
		nibs[2*i+1] = buf[3-i] & 0x0F;
		nibs[2*i+8] = buf[7-i] >> 4;
		nibs[2*i+9] = buf[7-i] & 0x0F;
	}

	if(mjd)
	{
		*mjd = m->kday + nibs[0]*100 + nibs[1]*10 + nibs[2];
	}
	if(sec) 
	{
		*sec = nibs[3]*10000 + nibs[4]*1000 + nibs[5]*100 + nibs[6]*10
	        	+ nibs[7];
	}
	if(ns)
	{
		*ns = nibs[8]*100000000 + nibs[9]*10000000 + nibs[10]*1000000
			+ nibs[11]*100000;

		/* "unround" the number */
		*ns = 156250*((*ns+156249)/156250);
	}

	return 0;
}

static int mark5_stream_frame_time_mark5cb(const struct mark5_stream *ms,
	int *mjd, int *sec, int *ns)
{
	struct mark5_format_mark5b *m;
	const uint8_t *buf;
	int n;	/* frame number within the 1 second period */
	int i;
	uint8_t nibs[8];

	m = (struct mark5_format_mark5b *)(ms->formatdata);

	buf = ms->frame + 8;
	
	/* 15 lowest bits of second 32-bit word */
	n = buf[4] + ((buf[5] & 0x7F) << 8);

	buf = ms->frame + 8;

	for(i = 0; i < 4; i++)
	{
		nibs[2*i+0] = buf[3-i] >> 4;
		nibs[2*i+1] = buf[3-i] & 0x0F;
	}

	if(mjd)
	{
		*mjd = m->kday + nibs[0]*100 + nibs[1]*10 + nibs[2];
	}
	if(sec) 
	{
		*sec = nibs[3]*10000 + nibs[4]*1000 + nibs[5]*100 + nibs[6]*10
	        	+ nibs[7];
	}
	if(ns)
	{
		*ns = ms->framens*n;
	}

	return 0;
}

static int mark5_format_mark5b_fixmjd(struct mark5_stream *ms, int refmjd)
{
	struct mark5_format_mark5b *m;
	int n;

	if(!ms)
	{
		return -1;
	}
	
	m = (struct mark5_format_mark5b *)(ms->formatdata);
	if(m->kday == 0)
	{
		n = (refmjd - ms->mjd + 500) / 1000;
		ms->mjd += n*1000;
		m->kday = n*1000;

		return 1;
	}

	return 0;
}

/************************* decode routines **************************/

static int mark5b_decode_1bitstream_1bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	uint8_t *buf;
	float *fp;
	int o, i;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		fp = lut1bit[buf[i]];
		i++;

		data[0][o] = fp[0];
		o++;
		data[0][o] = fp[1];
		o++;
		data[0][o] = fp[2];
		o++;
		data[0][o] = fp[3];
		o++;
		data[0][o] = fp[4];
		o++;
		data[0][o] = fp[5];
		o++;
		data[0][o] = fp[6];
		o++;
		data[0][o] = fp[7];

		if(i >= 10000)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = ms->payload;
			i = 0;
		}
	}

	ms->readposition = i;

	return 0;
}

static int mark5b_decode_2bitstream_1bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	uint8_t *buf;
	float *fp;
	int o, i;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		fp = lut1bit[buf[i]];
		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		o++;
		data[0][o] = fp[2];
		data[1][o] = fp[3];
		o++;
		data[0][o] = fp[4];
		data[1][o] = fp[5];
		o++;
		data[0][o] = fp[6];
		data[1][o] = fp[7];

		if(i >= 10000)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = ms->payload;
			i = 0;
		}
	}

	ms->readposition = i;

	return 0;
}

static int mark5b_decode_4bitstream_1bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	uint8_t *buf;
	float *fp;
	int o, i;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		fp = lut1bit[buf[i]];
		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];
		o++;
		data[0][o] = fp[4];
		data[1][o] = fp[5];
		data[2][o] = fp[6];
		data[3][o] = fp[7];

		if(i >= 10000)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = ms->payload;
			i = 0;
		}
	}

	ms->readposition = i;

	return 0;
}

static int mark5b_decode_8bitstream_1bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	uint8_t *buf;
	float *fp;
	int o, i;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		fp = lut1bit[buf[i]];
		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];
		data[4][o] = fp[4];
		data[5][o] = fp[5];
		data[6][o] = fp[6];
		data[7][o] = fp[7];

		if(i >= 10000)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = ms->payload;
			i = 0;
		}
	}

	ms->readposition = i;

	return 0;
}

static int mark5b_decode_16bitstream_1bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	uint8_t *buf;
	float *fp;
	int o, i;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		fp = lut1bit[buf[i]];
		i++;

		data[0][o]  = fp[0];
		data[1][o]  = fp[1];
		data[2][o]  = fp[2];
		data[3][o]  = fp[3];
		data[4][o]  = fp[4];
		data[5][o]  = fp[5];
		data[6][o]  = fp[6];
		data[7][o]  = fp[7];

		fp = lut1bit[buf[i]];
		i++;

		data[8][o]  = fp[0];
		data[9][o]  = fp[1];
		data[10][o] = fp[2];
		data[11][o] = fp[3];
		data[12][o] = fp[4];
		data[13][o] = fp[5];
		data[14][o] = fp[6];
		data[15][o] = fp[7];

		if(i >= 10000)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = ms->payload;
			i = 0;
		}
	}

	ms->readposition = i;

	return 0;
}

static int mark5b_decode_32bitstream_1bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	uint8_t *buf;
	float *fp;
	int o, i;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		fp = lut1bit[buf[i]];
		i++;

		data[0][o]  = fp[0];
		data[1][o]  = fp[1];
		data[2][o]  = fp[2];
		data[3][o]  = fp[3];
		data[4][o]  = fp[4];
		data[5][o]  = fp[5];
		data[6][o]  = fp[6];
		data[7][o]  = fp[7];

		fp = lut1bit[buf[i]];
		i++;

		data[8][o]  = fp[0];
		data[9][o]  = fp[1];
		data[10][o] = fp[2];
		data[11][o] = fp[3];
		data[12][o] = fp[4];
		data[13][o] = fp[5];
		data[14][o] = fp[6];
		data[15][o] = fp[7];

		fp = lut1bit[buf[i]];
		i++;

		data[16][o] = fp[0];
		data[17][o] = fp[1];
		data[18][o] = fp[2];
		data[19][o] = fp[3];
		data[20][o] = fp[4];
		data[21][o] = fp[5];
		data[22][o] = fp[6];
		data[23][o] = fp[7];

		fp = lut1bit[buf[i]];
		i++;

		data[24][o] = fp[0];
		data[25][o] = fp[1];
		data[26][o] = fp[2];
		data[27][o] = fp[3];
		data[28][o] = fp[4];
		data[29][o] = fp[5];
		data[30][o] = fp[6];
		data[31][o] = fp[7];

		if(i >= 10000)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = ms->payload;
			i = 0;
		}
	}

	ms->readposition = i;

	return 0;
}

static int mark5b_decode_2bitstream_2bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	uint8_t *buf;
	float *fp;
	int o, i;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		fp = lut2bit[buf[i]];
		i++;

		data[0][o] = fp[0];
		o++;
		data[0][o] = fp[1];
		o++;
		data[0][o] = fp[2];
		o++;
		data[0][o] = fp[3];

		if(i >= 10000)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = ms->payload;
			i = 0;
		}
	}

	ms->readposition = i;

	return 0;
}

static int mark5b_decode_4bitstream_2bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	uint8_t *buf;
	float *fp;
	int o, i;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		fp = lut2bit[buf[i]];
		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		o++;
		data[0][o] = fp[2];
		data[1][o] = fp[3];

		if(i >= 10000)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = ms->payload;
			i = 0;
		}
	}

	ms->readposition = i;

	return 0;
}

static int mark5b_decode_8bitstream_2bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	uint8_t *buf;
	float *fp;
	int o, i;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		fp = lut2bit[buf[i]];
		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];

		if(i >= 10000)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = ms->payload;
			i = 0;
		}
	}

	ms->readposition = i;

	return 0;
}

static int mark5b_decode_16bitstream_2bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	uint8_t *buf;
	float *fp;
	int o, i;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		fp = lut2bit[buf[i]];
		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];

		fp = lut2bit[buf[i]];
		i++;

		data[4][o] = fp[0];
		data[5][o] = fp[1];
		data[6][o] = fp[2];
		data[7][o] = fp[3];

		if(i >= 10000)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = ms->payload;
			i = 0;
		}
	}

	ms->readposition = i;

	return 0;
}

static int mark5b_decode_32bitstream_2bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	uint8_t *buf;
	float *fp;
	int o, i;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		fp = lut2bit[buf[i]];
		i++;

		data[0][o]  = fp[0];
		data[1][o]  = fp[1];
		data[2][o]  = fp[2];
		data[3][o]  = fp[3];

		fp = lut2bit[buf[i]];
		i++;

		data[4][o]  = fp[0];
		data[5][o]  = fp[1];
		data[6][o]  = fp[2];
		data[7][o]  = fp[3];

		fp = lut2bit[buf[i]];
		i++;

		data[8][o]  = fp[0];
		data[9][o]  = fp[1];
		data[10][o] = fp[2];
		data[11][o] = fp[3];

		fp = lut2bit[buf[i]];
		i++;

		data[12][o] = fp[0];
		data[13][o] = fp[1];
		data[14][o] = fp[2];
		data[15][o] = fp[3];

		if(i >= 10000)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = ms->payload;
			i = 0;
		}
	}

	ms->readposition = i;

	return 0;
}

/******************************************************************/

static int mark5_format_mark5b_make_formatname(struct mark5_stream *ms)
{
	if(ms->format == MK5_FORMAT_MARK5CB)
	{
		sprintf(ms->formatname, "Mark5CB-%d-%d-%d", ms->Mbps, 
			ms->nchan, ms->nbit);
	}
	else
	{
		sprintf(ms->formatname, "Mark5B-%d-%d-%d", ms->Mbps, 
			ms->nchan, ms->nbit);
	}

	return 0;
}

static int mark5_format_mark5b_init(struct mark5_stream *ms)
{
	struct mark5_format_mark5b *f;
	int mjd1, sec1, ns1;
	int datarate;
	int bytes;

	if(!ms)
	{
		fprintf(stderr, "mark5_format_mark5b_init: ms = 0\n");
		return -1;
	}

	bytes = ms->datawindowsize > (1<<20) ? (1<<20) : ms->datawindowsize;

	f = (struct mark5_format_mark5b *)(ms->formatdata);

	ms->samplegranularity = 32/f->nbitstream;
	ms->framebytes = 10016;
	ms->databytes = 10000;
	ms->payloadoffset = 16;
	ms->framesamples = 2500*32/f->nbitstream;
	if(ms->datawindow)
	{
		/* look for normal Mark5B sync word */
		ms->frameoffset = findfirstframe(ms->datawindow, bytes,
			mark5bSync);

		if(ms->frameoffset >= 0)
		{
			ms->format = MK5_FORMAT_MARK5B;
		}
		else if(ms->Mbps > 0)
		{
			/* look for Mark5C compatibility mode sync word */
			ms->frameoffset = findfirstframe(ms->datawindow, bytes,
				mark5cCompatSync);
			if(ms->frameoffset >= 0)
			{
				ms->format = MK5_FORMAT_MARK5CB;
				ms->gettime = mark5_stream_frame_time_mark5cb;
				ms->framens = 80000000/ms->Mbps;
			}
			else
			{
				return -1;
			}
		}
		else 
		{
			return -1;
		}

		ms->frame = ms->datawindow + ms->frameoffset;
		ms->payload = ms->frame + ms->payloadoffset;

		ms->gettime(ms, &ms->mjd, &ms->sec, &ms->ns);
		ms->frame += ms->framebytes;
		ms->gettime(ms, &mjd1, &sec1, &ns1);
		ms->frame -= ms->framebytes;

		/* assume frame time less than 1 second, integer number of
		 * frames per second
		 */
		if(ns1 != ms->ns)
		{
			ms->framens = ns1 - ms->ns;
			if(ms->framens <= 0)
			{
				ms->framens += 1000000000;
			}
			ms->samprate = ms->framesamples*
				(1000000000/ms->framens);
			datarate = ms->samprate*ms->nbit*ms->nchan/1000000;
			if(datarate != ms->Mbps)
			{
				if(ms->Mbps > 0)
				{
					fprintf(stderr, "Warning -- data rate "
						"disagrees : %d != %d\n",
						datarate, ms->Mbps);
				}
				ms->Mbps = datarate;
			}
		}
		else
		{
			fprintf(stderr, "Warning -- rate calc. suspect\n");
		}
	}

	mark5_format_mark5b_make_formatname(ms);

	return 0;
}

static int mark5_format_mark5b_final(struct mark5_stream *ms)
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

struct mark5_format_generic *new_mark5_format_mark5b(int Mbps,
	int nchan, int nbit)
{
	static int first = 1;
	struct mark5_format_generic *f;
	struct mark5_format_mark5b *m;
	int decoderindex=0;
	int nbitstream;

	nbitstream = nchan*nbit;

	if(first)
	{
		initluts();
		first = 0;
	}

	if(nbit == 1)
	{
		decoderindex += 0;
	}
	else if(nbit == 2)
	{
		decoderindex += 6;
	}
	else
	{
		fprintf(stderr, "new_mark5_format_mark5b : "
			"nbit needs to be 1 or 2\n");
		return 0;
	}
	
	if(nbitstream == 1)
	{
		decoderindex += 0;
	}
	else if(nbitstream == 2)
	{
		decoderindex += 1;
	}
	else if(nbitstream == 4)
	{
		decoderindex += 2;
	}
	else if(nbitstream == 8)
	{
		decoderindex += 3;
	}
	else if(nbitstream == 16)
	{
		decoderindex += 4;
	}
	else if(nbitstream == 32)
	{
		decoderindex += 5;
	}
	else
	{
		fprintf(stderr, "new_mark5_format_mark5b : "
			"nbitstream needs to be 1, 2, 4, 8, 16 or 32\n");
		return 0;
	}

	if(decoderindex == 6)
	{
		fprintf(stderr, "Illegal format\n");
		return 0;
	}

	m = (struct mark5_format_mark5b *)malloc(
		sizeof(struct mark5_format_mark5b));
	f = (struct mark5_format_generic *)malloc(
		sizeof(struct mark5_format_generic));

	m->nbitstream = nbitstream;
	m->kday = 0;

	f->Mbps = Mbps;
	f->nchan = nchan;
	f->nbit = nbit;
	f->formatdata = m;
	f->gettime = mark5_stream_frame_time_mark5b;
	f->init_format = mark5_format_mark5b_init;
	f->final_format = mark5_format_mark5b_final;
	f->fixmjd = mark5_format_mark5b_fixmjd;
	f->validate = one;
	switch(decoderindex)
	{
		case 0 : f->decode = mark5b_decode_1bitstream_1bit; break;
		case 1 : f->decode = mark5b_decode_2bitstream_1bit; break;
		case 2 : f->decode = mark5b_decode_4bitstream_1bit; break;
		case 3 : f->decode = mark5b_decode_8bitstream_1bit; break;
		case 4 : f->decode = mark5b_decode_16bitstream_1bit; break;
		case 5 : f->decode = mark5b_decode_32bitstream_1bit; break;
		case 7 : f->decode = mark5b_decode_2bitstream_2bit; break;
		case 8 : f->decode = mark5b_decode_4bitstream_2bit; break;
		case 9 : f->decode = mark5b_decode_8bitstream_2bit; break;
		case 10: f->decode = mark5b_decode_16bitstream_2bit; break;
		case 11: f->decode = mark5b_decode_32bitstream_2bit; break;
	}

	return f;
}
