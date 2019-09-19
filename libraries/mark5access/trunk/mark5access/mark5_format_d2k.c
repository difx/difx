/***************************************************************************
 *   Copyright (C) 2007-2015 by Walter Brisken                             *
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
// $Id: mark5_format_d2k.c -1   $
// $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_format_d2k.c $
// $LastChangedRevision: -1 $
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
#include <math.h>
#include "mark5access/mark5_stream.h"

#define MK5B_PAYLOADSIZE 10000

const unsigned int d2kSync       = 0xABADDEED;

/* the high mag value for 2-bit reconstruction */
static const float HiMag = OPTIMAL_2BIT_HIGH;

struct mark5_format_mark5b
{
	int nbitstream;
	int kday;	/* kilo-mjd: ie 51000, 52000, ... */
};

static float lut1bit[256][8];
static float lut2bit[256][4];
static unsigned char countlut2bit[256][4];
static float zeros[8];

static void d2k_initluts()
{
	int b, i, s, m, l;
	const float lut2level[2] = {1.0, -1.0};
// B5M order rather than M5B
	const float lut4level[4] = {-HiMag, -1.0, 1.0, HiMag};

	for(i = 0; i < 8; ++i)
	{
		zeros[i] = 0.0;
	}

	for(b = 0; b < 256; ++b)
	{
		/* lut1bit */
		for(i = 0; i < 8; ++i)
		{
			l = (b>>i)&1;
			lut1bit[b][i] =  lut2level[l];
		}

		/* lut2bit */
		for(i = 0; i < 4; ++i)
		{
			s = i*2;	/* 0, 2, 4, 6 */
			m = s+1;	/* 1, 3, 5, 7 */
			l = ((b>>s)&1) + (((b>>m)&1)<<1);
			lut2bit[b][i] =  lut4level[l];
			if(fabs(lut2bit[b][i]) > 1.1)
			{
				countlut2bit[b][i] = 1;
			}
			else
			{
				countlut2bit[b][i] = 0;
			}
		}
	}
}

static int mark5_format_d2k_make_formatname(struct mark5_stream *ms)
{
	snprintf(ms->formatname, MARK5_STREAM_ID_LENGTH, "D2K-%d-%d-%d", 
		ms->Mbps, ms->nchan, ms->nbit);

	return 0;
}

//#ifdef NONONO
// ---------------------Repeated Code ======

static int findfirstframe(const unsigned char *data, int bytes, unsigned int syncword)
{
	int i;
	unsigned char sb0, sb1, sb2, sb3;

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
	for(i = 0; i < bytes; ++i)
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
		++data;
	}

	return -1;
}

static int mark5_stream_frame_num_mark5b(const struct mark5_stream *ms)
{
	return ms->frame[4] + (ms->frame[5] & 0x7F)*256;
}

static int mark5_stream_frame_time_mark5b(const struct mark5_stream *ms, int *mjd, int *sec, double *ns)
{
	struct mark5_format_mark5b *m;
	const unsigned char *buf;
	int i;
	int framenum;
	unsigned char nibs[16];

	m = (struct mark5_format_mark5b *)(ms->formatdata);

	buf = ms->frame + 8;

	framenum = mark5_stream_frame_num_mark5b(ms);

	for(i = 0; i < 4; ++i)
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
		*sec = nibs[3]*10000 + nibs[4]*1000 + nibs[5]*100 + nibs[6]*10 + nibs[7];
	}
	if(ns)
	{
// 20120802: It seems frame num is not super reliable...
		if(ms->framens > 0)
		{
			*ns = ms->framens*framenum;
		}
		else
		{
			*ns = nibs[8]*100000000 + nibs[9]*10000000 + nibs[10]*1000000 + nibs[11]*100000;
//			if(*ns < 0.0 || *ns > 1.0e9)
//			{
//				printf("Weird! %d %d %d %d\n", nibs[8], nibs[9], nibs[10], nibs[11]);
//			}
			/* "unround" the number */
			*ns = 156250*(((int)(*ns)+156249)/156250);
		}
	}

	return 0;
}

static void mark5_format_mark5b_genheaders(const struct mark5_stream *ms, int n, unsigned char *where)
{
	int i;

	if(!ms)
	{
		fprintf(m5stdout, "mark5_format_mark5b_genheaders: ms=0\n");

		return;
	}

	for(i = 0; i < n; i += ms->framegranularity)
	{
		int f;

		for(f = 0; f < ms->framegranularity; ++f)
		{
			*((unsigned int *)where) = d2kSync;
		}
	}
}

static int mark5_format_mark5b_fixmjd(struct mark5_stream *ms, int refmjd)
{
	struct mark5_format_mark5b *m;

	if(!ms)
	{
		return -1;
	}
	
	m = (struct mark5_format_mark5b *)(ms->formatdata);
	if(m->kday == 0)
	{
		int n;
		
		n = (refmjd - ms->mjd + 500) / 1000;
		ms->mjd += n*1000;
		m->kday = n*1000;

		return 1;
	}

	return 0;
}

/************************* decode routines **************************/

static int mark5b_decode_1bitstream_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
		++i;

		data[0][o] = fp[0];
		++o;
		data[0][o] = fp[1];
		++o;
		data[0][o] = fp[2];
		++o;
		data[0][o] = fp[3];
		++o;
		data[0][o] = fp[4];
		++o;
		data[0][o] = fp[5];
		++o;
		data[0][o] = fp[6];
		++o;
		data[0][o] = fp[7];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp-8*nblank;
}

static int mark5b_decode_1bitstream_1bit_decimation2(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
		++i;

		data[0][o] = fp[0];
		++o;
		data[0][o] = fp[2];
		++o;
		data[0][o] = fp[4];
		++o;
		data[0][o] = fp[6];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp-8*nblank;
}

static int mark5b_decode_1bitstream_1bit_decimation4(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
		++i;

		data[0][o] = fp[0];
		++o;
		data[0][o] = fp[4];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp-8*nblank;
}

static int mark5b_decode_1bitstream_1bit_decimation8(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation/8;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
		i += df;

		data[0][o] = fp[0];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp-8*nblank;
}

static int mark5b_decode_2bitstream_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
		++i;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		++o;
		data[0][o] = fp[2];
		data[1][o] = fp[3];
		++o;
		data[0][o] = fp[4];
		data[1][o] = fp[5];
		++o;
		data[0][o] = fp[6];
		data[1][o] = fp[7];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 4*nblank;
}

static int mark5b_decode_2bitstream_1bit_decimation2(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
		++i;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		++o;
		data[0][o] = fp[4];
		data[1][o] = fp[5];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 4*nblank;
}

static int mark5b_decode_2bitstream_1bit_decimation4(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation/4;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
		i += df;

		data[0][o] = fp[0];
		data[1][o] = fp[1];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 4*nblank;
}

static int mark5b_decode_4bitstream_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
		++i;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];
		++o;
		data[0][o] = fp[4];
		data[1][o] = fp[5];
		data[2][o] = fp[6];
		data[3][o] = fp[7];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 2*nblank;
}

static int mark5b_decode_4bitstream_1bit_decimation2(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
		++i;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 2*nblank;
}

static int mark5b_decode_4bitstream_1bit_decimation4(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation/2;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
		i += df;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 2*nblank;
}

static int mark5b_decode_8bitstream_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
		++i;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];
		data[4][o] = fp[4];
		data[5][o] = fp[5];
		data[6][o] = fp[6];
		data[7][o] = fp[7];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_8bitstream_1bit_decimation2(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
		i += 2;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];
		data[4][o] = fp[4];
		data[5][o] = fp[5];
		data[6][o] = fp[6];
		data[7][o] = fp[7];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_8bitstream_1bit_decimation4(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
		i += df;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];
		data[4][o] = fp[4];
		data[5][o] = fp[5];
		data[6][o] = fp[6];
		data[7][o] = fp[7];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_16bitstream_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = zeros;
			i += 2;
			++nblank;
		}
		else
		{
			fp0 = lut1bit[buf[i]];
			++i;
			fp1 = lut1bit[buf[i]];
			++i;
		}

		data[0][o]  = fp0[0];
		data[1][o]  = fp0[1];
		data[2][o]  = fp0[2];
		data[3][o]  = fp0[3];
		data[4][o]  = fp0[4];
		data[5][o]  = fp0[5];
		data[6][o]  = fp0[6];
		data[7][o]  = fp0[7];
		data[8][o]  = fp1[0];
		data[9][o]  = fp1[1];
		data[10][o] = fp1[2];
		data[11][o] = fp1[3];
		data[12][o] = fp1[4];
		data[13][o] = fp1[5];
		data[14][o] = fp1[6];
		data[15][o] = fp1[7];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_16bitstream_1bit_decimation2(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = zeros;
			i += 4;
			++nblank;
		}
		else
		{
			fp0 = lut1bit[buf[i]];
			++i;
			fp1 = lut1bit[buf[i]];
			i += 3;
		}

		data[0][o]  = fp0[0];
		data[1][o]  = fp0[1];
		data[2][o]  = fp0[2];
		data[3][o]  = fp0[3];
		data[4][o]  = fp0[4];
		data[5][o]  = fp0[5];
		data[6][o]  = fp0[6];
		data[7][o]  = fp0[7];
		data[8][o]  = fp1[0];
		data[9][o]  = fp1[1];
		data[10][o] = fp1[2];
		data[11][o] = fp1[3];
		data[12][o] = fp1[4];
		data[13][o] = fp1[5];
		data[14][o] = fp1[6];
		data[15][o] = fp1[7];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_16bitstream_1bit_decimation4(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation*2 - 1;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = zeros;
			++i;
			fp1 = zeros;
			++nblank;
		}
		else
		{
			fp0 = lut1bit[buf[i]];
			++i;
			fp1 = lut1bit[buf[i]];
		}
		i += df;

		data[0][o]  = fp0[0];
		data[1][o]  = fp0[1];
		data[2][o]  = fp0[2];
		data[3][o]  = fp0[3];
		data[4][o]  = fp0[4];
		data[5][o]  = fp0[5];
		data[6][o]  = fp0[6];
		data[7][o]  = fp0[7];
		data[8][o]  = fp1[0];
		data[9][o]  = fp1[1];
		data[10][o] = fp1[2];
		data[11][o] = fp1[3];
		data[12][o] = fp1[4];
		data[13][o] = fp1[5];
		data[14][o] = fp1[6];
		data[15][o] = fp1[7];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_32bitstream_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1, *fp2, *fp3;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = fp2 = fp3 = zeros;
			i += 4;
			++nblank;
		}
		else
		{
			fp0 = lut1bit[buf[i]];
			++i;
			fp1 = lut1bit[buf[i]];
			++i;
			fp2 = lut1bit[buf[i]];
			++i;
			fp3 = lut1bit[buf[i]];
			++i;
		}

		data[0][o]  = fp0[0];
		data[1][o]  = fp0[1];
		data[2][o]  = fp0[2];
		data[3][o]  = fp0[3];
		data[4][o]  = fp0[4];
		data[5][o]  = fp0[5];
		data[6][o]  = fp0[6];
		data[7][o]  = fp0[7];
		data[8][o]  = fp1[0];
		data[9][o]  = fp1[1];
		data[10][o] = fp1[2];
		data[11][o] = fp1[3];
		data[12][o] = fp1[4];
		data[13][o] = fp1[5];
		data[14][o] = fp1[6];
		data[15][o] = fp1[7];
		data[16][o] = fp2[0];
		data[17][o] = fp2[1];
		data[18][o] = fp2[2];
		data[19][o] = fp2[3];
		data[20][o] = fp2[4];
		data[21][o] = fp2[5];
		data[22][o] = fp2[6];
		data[23][o] = fp2[7];
		data[24][o] = fp3[0];
		data[25][o] = fp3[1];
		data[26][o] = fp3[2];
		data[27][o] = fp3[3];
		data[28][o] = fp3[4];
		data[29][o] = fp3[5];
		data[30][o] = fp3[6];
		data[31][o] = fp3[7];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_32bitstream_1bit_decimation2(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1, *fp2, *fp3;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = fp2 = fp3 = zeros;
			i += 8;
			++nblank;
		}
		else
		{
			fp0 = lut1bit[buf[i]];
			++i;
			fp1 = lut1bit[buf[i]];
			++i;
			fp2 = lut1bit[buf[i]];
			++i;
			fp3 = lut1bit[buf[i]];
			i += 5;
		}

		data[0][o]  = fp0[0];
		data[1][o]  = fp0[1];
		data[2][o]  = fp0[2];
		data[3][o]  = fp0[3];
		data[4][o]  = fp0[4];
		data[5][o]  = fp0[5];
		data[6][o]  = fp0[6];
		data[7][o]  = fp0[7];
		data[8][o]  = fp1[0];
		data[9][o]  = fp1[1];
		data[10][o] = fp1[2];
		data[11][o] = fp1[3];
		data[12][o] = fp1[4];
		data[13][o] = fp1[5];
		data[14][o] = fp1[6];
		data[15][o] = fp1[7];
		data[16][o] = fp2[0];
		data[17][o] = fp2[1];
		data[18][o] = fp2[2];
		data[19][o] = fp2[3];
		data[20][o] = fp2[4];
		data[21][o] = fp2[5];
		data[22][o] = fp2[6];
		data[23][o] = fp2[7];
		data[24][o] = fp3[0];
		data[25][o] = fp3[1];
		data[26][o] = fp3[2];
		data[27][o] = fp3[3];
		data[28][o] = fp3[4];
		data[29][o] = fp3[5];
		data[30][o] = fp3[6];
		data[31][o] = fp3[7];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_32bitstream_1bit_decimation4(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1, *fp2, *fp3;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation*4 - 3;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = fp2 = fp3 = zeros;
			i += 3;
			++nblank;
		}
		else
		{
			fp0 = lut1bit[buf[i]];
			++i;
			fp1 = lut1bit[buf[i]];
			++i;
			fp2 = lut1bit[buf[i]];
			++i;
			fp3 = lut1bit[buf[i]];
		}
		i += df;

		data[0][o]  = fp0[0];
		data[1][o]  = fp0[1];
		data[2][o]  = fp0[2];
		data[3][o]  = fp0[3];
		data[4][o]  = fp0[4];
		data[5][o]  = fp0[5];
		data[6][o]  = fp0[6];
		data[7][o]  = fp0[7];
		data[8][o]  = fp1[0];
		data[9][o]  = fp1[1];
		data[10][o] = fp1[2];
		data[11][o] = fp1[3];
		data[12][o] = fp1[4];
		data[13][o] = fp1[5];
		data[14][o] = fp1[6];
		data[15][o] = fp1[7];
		data[16][o] = fp2[0];
		data[17][o] = fp2[1];
		data[18][o] = fp2[2];
		data[19][o] = fp2[3];
		data[20][o] = fp2[4];
		data[21][o] = fp2[5];
		data[22][o] = fp2[6];
		data[23][o] = fp2[7];
		data[24][o] = fp3[0];
		data[25][o] = fp3[1];
		data[26][o] = fp3[2];
		data[27][o] = fp3[3];
		data[28][o] = fp3[4];
		data[29][o] = fp3[5];
		data[30][o] = fp3[6];
		data[31][o] = fp3[7];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

/************************ 2-bit decoders *********************/

static int mark5b_decode_2bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut2bit[buf[i]];
		}
		++i;

		data[0][o] = fp[0];
		++o;
		data[0][o] = fp[1];
		++o;
		data[0][o] = fp[2];
		++o;
		data[0][o] = fp[3];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 4*nblank;
}

static int mark5b_decode_2bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut2bit[buf[i]];
		}
		++i;

		data[0][o] = fp[0];
		++o;
		data[0][o] = fp[2];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 4*nblank;
}

static int mark5b_decode_2bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation/4;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut2bit[buf[i]];
		}
		i += df;

		data[0][o] = fp[0];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 4*nblank;
}

static int mark5b_decode_4bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut2bit[buf[i]];
		}
		++i;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		++o;
		data[0][o] = fp[2];
		data[1][o] = fp[3];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 2*nblank;
}

static int mark5b_decode_4bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut2bit[buf[i]];
		}
		++i;

		data[0][o] = fp[0];
		data[1][o] = fp[1];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 2*nblank;
}

static int mark5b_decode_4bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation/2;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut2bit[buf[i]];
		}
		i += df;

		data[0][o] = fp[0];
		data[1][o] = fp[1];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 2*nblank;
}

static int mark5b_decode_8bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut2bit[buf[i]];
		}
		++i;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_8bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut2bit[buf[i]];
		}
		i += 2;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_8bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			++nblank;
		}
		else
		{
			fp = lut2bit[buf[i]];
		}
		i += df;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_16bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = zeros;
			i += 2;
			++nblank;
		}
		else
		{
			fp0 = lut2bit[buf[i]];
			++i;
			fp1 = lut2bit[buf[i]];
			++i;
		}

		data[0][o] = fp0[0];
		data[1][o] = fp0[1];
		data[2][o] = fp0[2];
		data[3][o] = fp0[3];
		data[4][o] = fp1[0];
		data[5][o] = fp1[1];
		data[6][o] = fp1[2];
		data[7][o] = fp1[3];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_16bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = zeros;
			i += 4;
			++nblank;
		}
		else
		{
			fp0 = lut2bit[buf[i]];
			++i;
			fp1 = lut2bit[buf[i]];
			i += 3;
		}

		data[0][o] = fp0[0];
		data[1][o] = fp0[1];
		data[2][o] = fp0[2];
		data[3][o] = fp0[3];
		data[4][o] = fp1[0];
		data[5][o] = fp1[1];
		data[6][o] = fp1[2];
		data[7][o] = fp1[3];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_16bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation*2 - 1;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = zeros;
			++i;
			++nblank;
		}
		else
		{
			fp0 = lut2bit[buf[i]];
			++i;
			fp1 = lut2bit[buf[i]];
		}
		i += df;

		data[0][o] = fp0[0];
		data[1][o] = fp0[1];
		data[2][o] = fp0[2];
		data[3][o] = fp0[3];
		data[4][o] = fp1[0];
		data[5][o] = fp1[1];
		data[6][o] = fp1[2];
		data[7][o] = fp1[3];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_32bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1, *fp2, *fp3;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = fp2 = fp3 = zeros;
			i += 4;
			++nblank;
		}
		else
		{
			fp0 = lut2bit[buf[i]];
			++i;
			fp1 = lut2bit[buf[i]];
			++i;
			fp2 = lut2bit[buf[i]];
			++i;
			fp3 = lut2bit[buf[i]];
			++i;
		}

		data[0][o]  = fp0[0];
		data[1][o]  = fp0[1];
		data[2][o]  = fp0[2];
		data[3][o]  = fp0[3];
		data[4][o]  = fp1[0];
		data[5][o]  = fp1[1];
		data[6][o]  = fp1[2];
		data[7][o]  = fp1[3];
		data[8][o]  = fp2[0];
		data[9][o]  = fp2[1];
		data[10][o] = fp2[2];
		data[11][o] = fp2[3];
		data[12][o] = fp3[0];
		data[13][o] = fp3[1];
		data[14][o] = fp3[2];
		data[15][o] = fp3[3];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_32bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1, *fp2, *fp3;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = fp2 = fp3 = zeros;
			i += 8;
			++nblank;
		}
		else
		{
			fp0 = lut2bit[buf[i]];
			++i;
			fp1 = lut2bit[buf[i]];
			++i;
			fp2 = lut2bit[buf[i]];
			++i;
			fp3 = lut2bit[buf[i]];
			i += 5;
		}

		data[0][o]  = fp0[0];
		data[1][o]  = fp0[1];
		data[2][o]  = fp0[2];
		data[3][o]  = fp0[3];
		data[4][o]  = fp1[0];
		data[5][o]  = fp1[1];
		data[6][o]  = fp1[2];
		data[7][o]  = fp1[3];
		data[8][o]  = fp2[0];
		data[9][o]  = fp2[1];
		data[10][o] = fp2[2];
		data[11][o] = fp2[3];
		data[12][o] = fp3[0];
		data[13][o] = fp3[1];
		data[14][o] = fp3[2];
		data[15][o] = fp3[3];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_decode_32bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1, *fp2, *fp3;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation*4 - 3;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = fp2 = fp3 = zeros;
			i += 3;
			++nblank;
		}
		else
		{
			fp0 = lut2bit[buf[i]];
			++i;
			fp1 = lut2bit[buf[i]];
			++i;
			fp2 = lut2bit[buf[i]];
			++i;
			fp3 = lut2bit[buf[i]];
		}
		i += df;

		data[0][o]  = fp0[0];
		data[1][o]  = fp0[1];
		data[2][o]  = fp0[2];
		data[3][o]  = fp0[3];
		data[4][o]  = fp1[0];
		data[5][o]  = fp1[1];
		data[6][o]  = fp1[2];
		data[7][o]  = fp1[3];
		data[8][o]  = fp2[0];
		data[9][o]  = fp2[1];
		data[10][o] = fp2[2];
		data[11][o] = fp2[3];
		data[12][o] = fp3[0];
		data[13][o] = fp3[1];
		data[14][o] = fp3[2];
		data[15][o] = fp3[3];

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

/************************ 2-bit counters *********************/

static int mark5b_count_2bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o+=3)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			++nblank;
		}
		else
		{
			fp = countlut2bit[buf[i]];
			highstates[0] += fp[0];
			highstates[0] += fp[1];
			highstates[0] += fp[2];
			highstates[0] += fp[3];
		}
		++i;

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 4*nblank;
}

static int mark5b_count_2bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o+=2)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			++nblank;
		}
		else
		{
			fp = countlut2bit[buf[i]];
			highstates[0] += fp[0];
			highstates[0] += fp[2];
		}
		++i;

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 4*nblank;
}

static int mark5b_count_2bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation/4;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			++nblank;
		}
		else
		{
			fp = countlut2bit[buf[i]];
			highstates[0] += fp[0];
		}
		i += df;

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 4*nblank;
}

static int mark5b_count_4bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o+=2)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			++nblank;
		}
		else
		{
			fp = countlut2bit[buf[i]];
			highstates[0] += fp[0];
			highstates[1] += fp[1];
			highstates[0] += fp[2];
			highstates[1] += fp[3];
		}
		++i;

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 2*nblank;
}

static int mark5b_count_4bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			++nblank;
		}
		else
		{
			fp = countlut2bit[buf[i]];
			highstates[0] += fp[0];
			highstates[1] += fp[1];
		}
		++i;

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 2*nblank;
}

static int mark5b_count_4bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation/2;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			++nblank;
		}
		else
		{
			fp = countlut2bit[buf[i]];
			highstates[0] += fp[0];
			highstates[1] += fp[1];
		}
		i += df;

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - 2*nblank;
}

static int mark5b_count_8bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			++nblank;
		}
		else
		{
			fp = countlut2bit[buf[i]];
			highstates[0] += fp[0];
			highstates[1] += fp[1];
			highstates[2] += fp[2];
			highstates[3] += fp[3];
		}
		++i;

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_count_8bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			++nblank;
		}
		else
		{
			fp = countlut2bit[buf[i]];
			highstates[0] += fp[0];
			highstates[1] += fp[1];
			highstates[2] += fp[2];
			highstates[3] += fp[3];
		}
		i += 2;

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_count_8bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			++nblank;
		}
		else
		{
			fp = countlut2bit[buf[i]];
			highstates[0] += fp[0];
			highstates[1] += fp[1];
			highstates[2] += fp[2];
			highstates[3] += fp[3];
		}
		i += df;

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_count_16bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp0, *fp1;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			i += 2;
			++nblank;
		}
		else
		{
			fp0 = countlut2bit[buf[i]];
			++i;
			fp1 = countlut2bit[buf[i]];
			++i;
			highstates[0] += fp0[0];
			highstates[1] += fp0[1];
			highstates[2] += fp0[2];
			highstates[3] += fp0[3];
			highstates[4] += fp1[0];
			highstates[5] += fp1[1];
			highstates[6] += fp1[2];
			highstates[7] += fp1[3];
		}

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_count_16bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp0, *fp1;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			i += 4;
			++nblank;
		}
		else
		{
			fp0 = countlut2bit[buf[i]];
			++i;
			fp1 = countlut2bit[buf[i]];
			i += 3;
			highstates[0] += fp0[0];
			highstates[1] += fp0[1];
			highstates[2] += fp0[2];
			highstates[3] += fp0[3];
			highstates[4] += fp1[0];
			highstates[5] += fp1[1];
			highstates[6] += fp1[2];
			highstates[7] += fp1[3];
		}

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_count_16bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp0, *fp1;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation*2 - 1;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			++i;
			++nblank;
		}
		else
		{
			fp0 = countlut2bit[buf[i]];
			++i;
			fp1 = countlut2bit[buf[i]];
			highstates[0] += fp0[0];
			highstates[1] += fp0[1];
			highstates[2] += fp0[2];
			highstates[3] += fp0[3];
			highstates[4] += fp1[0];
			highstates[5] += fp1[1];
			highstates[6] += fp1[2];
			highstates[7] += fp1[3];
		}
		i += df;

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_count_32bitstream_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp0, *fp1, *fp2, *fp3;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			i += 4;
			++nblank;
		}
		else
		{
			fp0 = countlut2bit[buf[i]];
			++i;
			fp1 = countlut2bit[buf[i]];
			++i;
			fp2 = countlut2bit[buf[i]];
			++i;
			fp3 = countlut2bit[buf[i]];
			++i;
			highstates[0] += fp0[0];
			highstates[1] += fp0[1];
			highstates[2] += fp0[2];
			highstates[3] += fp0[3];
			highstates[4] += fp1[0];
			highstates[5] += fp1[1];
			highstates[6] += fp1[2];
			highstates[7] += fp1[3];
			highstates[8] += fp2[0];
			highstates[9] += fp2[1];
			highstates[10] += fp2[2];
			highstates[11] += fp2[3];
			highstates[12] += fp3[0];
			highstates[13] += fp3[1];
			highstates[14] += fp3[2];
			highstates[15] += fp3[3];
		}

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_count_32bitstream_2bit_decimation2(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp0, *fp1, *fp2, *fp3;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			i += 8;
			++nblank;
		}
		else
		{
			fp0 = countlut2bit[buf[i]];
			++i;
			fp1 = countlut2bit[buf[i]];
			++i;
			fp2 = countlut2bit[buf[i]];
			++i;
			fp3 = countlut2bit[buf[i]];
			i += 5;
			highstates[0] += fp0[0];
			highstates[1] += fp0[1];
			highstates[2] += fp0[2];
			highstates[3] += fp0[3];
			highstates[4] += fp1[0];
			highstates[5] += fp1[1];
			highstates[6] += fp1[2];
			highstates[7] += fp1[3];
			highstates[8] += fp2[0];
			highstates[9] += fp2[1];
			highstates[10] += fp2[2];
			highstates[11] += fp2[3];
			highstates[12] += fp3[0];
			highstates[13] += fp3[1];
			highstates[14] += fp3[2];
			highstates[15] += fp3[3];
		}

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

static int mark5b_count_32bitstream_2bit_decimation4(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp0, *fp1, *fp2, *fp3;
	int o, i, df;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	df = ms->decimation*4 - 3;

	for(o = 0; o < nsamp; ++o)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			i += 3;
			++nblank;
		}
		else
		{
			fp0 = countlut2bit[buf[i]];
			++i;
			fp1 = countlut2bit[buf[i]];
			++i;
			fp2 = countlut2bit[buf[i]];
			++i;
			fp3 = countlut2bit[buf[i]];
			highstates[0] += fp0[0];
			highstates[1] += fp0[1];
			highstates[2] += fp0[2];
			highstates[3] += fp0[3];
			highstates[4] += fp1[0];
			highstates[5] += fp1[1];
			highstates[6] += fp1[2];
			highstates[7] += fp1[3];
			highstates[8] += fp2[0];
			highstates[9] += fp2[1];
			highstates[10] += fp2[2];
			highstates[11] += fp2[3];
			highstates[12] += fp3[0];
			highstates[13] += fp3[1];
			highstates[14] += fp3[2];
			highstates[15] += fp3[3];
		}
		i += df;

		if(i >= MK5B_PAYLOADSIZE)
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

	return nsamp - nblank;
}

/******************************************************************/


static int mark5_format_mark5b_final(struct mark5_stream *ms)
{
	if(!ms)
	{
		return -1;
	}

	if(ms->formatdata)
	{
		free(ms->formatdata);
		ms->formatdata = 0;
	}

	return 0;
}

static int one(const struct mark5_stream *ms)
{
	return 1;
}

static int onenc(struct mark5_stream *ms)
{
	return 1;
}

//---- End Repeated cdoe
//#endif

static int mark5_format_d2k_init(struct mark5_stream *ms)
{
	struct mark5_format_mark5b *f;
	int mjd1, sec1, ns1;
	double dns, dns1;
	int datarate;
	int k, df;

	if(!ms)
	{
		fprintf(m5stderr, "mark5_format_d2k_init: ms = 0\n");

		return -1;
	}

	f = (struct mark5_format_mark5b *)(ms->formatdata);

	ms->samplegranularity = 8/(f->nbitstream*ms->decimation);
	if(ms->samplegranularity <= 0)
	{
		ms->samplegranularity = 1;
	}
	ms->framebytes = 10016;
	ms->databytes = 10000;
	ms->payloadoffset = 16;
	ms->framesamples = ms->databytes*8/(f->nbitstream*ms->decimation);
	ms->blanker = blanker_mark5;
	if(ms->Mbps > 0)
	{
		ms->framens = 80000000.0/ms->Mbps;
	}

	if(ms->datawindow)
	{
		int bytes;

		if(ms->datawindowsize < ms->framebytes)
		{
			fprintf(m5stderr, "mark5_format_d2k_init: windowsize too small: %lld < %d\n", ms->datawindowsize, ms->framebytes);

			return -1;
		}

		/* look through entire data window, up to 1Mibytes */
		bytes = ms->datawindowsize < MARK5_STREAM_MAXBUFSIZE ?
			ms->datawindowsize : MARK5_STREAM_MAXBUFSIZE;

		/* first look for normal Mark5B sync word */
		ms->frameoffset = findfirstframe(ms->datawindow, bytes, d2kSync);

		if(ms->frameoffset < 0)
		{
			return -1;
		}

		ms->frame = ms->datawindow + ms->frameoffset;
		ms->payload = ms->frame + ms->payloadoffset;

		ms->gettime(ms, &ms->mjd, &ms->sec, &dns);
		ms->ns = (int)(dns + 0.5);

		if(ms->Mbps > 0)
		{
			ms->samprate = ms->framesamples*(1000000000.0/ms->framens);
		}
		else
		{
			k = 8;
			while((k+2)*ms->framebytes > ms->datawindowsize && k > 1)
			{
				k /= 2;
			}
			ms->frame += k*ms->framebytes;
			ms->gettime(ms, &mjd1, &sec1, &dns1);
			ns1 = (int)(dns1 + 0.5);
			ms->frame -= k*ms->framebytes;

			/* assume frame time less than 1 second, integer number
			 * of frames per second
			 */
			if(ns1 != ms->ns)
			{
				ms->framens = (ns1 - ms->ns)/k;

				/* get time again so ms->framens is used */
				ms->gettime(ms, &ms->mjd, &ms->sec, &dns);
				ms->ns = (int)(dns + 0.5);

				if(ms->framens <= 0)
				{
					ms->framens += 1000000000;
				}
				ms->samprate = ms->framesamples*(1000000000/ms->framens);
				datarate = ms->samprate*ms->nbit*ms->nchan/1000000;
				if(datarate != (int)(ms->Mbps + 0.5))
				{
					if(ms->Mbps > 0)
					{
						fprintf(m5stderr, "Warning: data rate disagrees : %d != %f\n", datarate, ms->Mbps);
					}
					ms->Mbps = (double)datarate;
				}
			}
			else
			{
				fprintf(m5stderr, "Warning: mark5_format_d2k_init: assuming 2048-16-2\n");

				ms->framens = 39062.5;
				ms->framesperperiod = 25600;
				ms->alignmentseconds = 1;
				ms->Mbps = 2048;
				ms->nchan = 16;
				ms->nbit = 2;
				ms->framesamples = 2500;
				ms->samprate = 64000000;
			}
		}
	}

	/* see if we need to advance a small number of frames to make the
	 * first one start at integer nanosec
	 */
	if(1000000000 % ms->framesperperiod > 0)
	{
		k = ms->framesperperiod / (1000000000 % ms->framesperperiod);
		if(ms->datawindow)
		{
			int framenum;

			framenum = mark5_stream_frame_num_mark5b(ms);
			df = k - framenum % k;
			if(df != k)
			{
				ms->frame += df*ms->framebytes;
				ms->frameoffset += df*ms->framebytes;
				ms->gettime(ms, &ms->mjd, &ms->sec, &dns);
				ms->ns = (int)(dns + 0.5);
			}
		}
		ms->framegranularity = k;
	}
	else
	{
		ms->framegranularity = 1;
	}

	ms->gframens = (int)(ms->framegranularity*ms->framens + 0.5);

	ms->format = MK5_FORMAT_D2K;
	mark5_format_d2k_make_formatname(ms);

	return 0;
}


struct mark5_format_generic *new_mark5_format_d2k(int Mbps, int nchan, int nbit, int decimation)
{
	static int first = 1;
	struct mark5_format_generic *f;
	struct mark5_format_mark5b *m;
	int decoderindex = 0;
	int nbitstream;

	nbitstream = nchan*nbit;

	if(first)
	{
		d2k_initluts();
		first = 0;
	}

	if(decimation == 1)
	{
		decoderindex += 0;
	}
	else if(decimation == 2)
	{
		decoderindex += 12;
	}
	else if(decimation % 4 == 0)  /* all mults of 4 */
	{
		decoderindex += 24;
	}
	else
	{
		fprintf(m5stderr, "decimation must be 1, 2 or a mult of 4\n");
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
		fprintf(m5stderr, "new_mark5_format_d2k: nbit needs to be 1 or 2\n");

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
		fprintf(m5stderr, "new_mark5_format_d2k : nbitstream needs to be 1, 2, 4, 8, 16 or 32\n");

		return 0;
	}

	if(decoderindex == 6)
	{
		fprintf(m5stderr, "Illegal format\n");
		return 0;
	}

	m = (struct mark5_format_mark5b *)calloc(1, sizeof(struct mark5_format_mark5b));
	f = (struct mark5_format_generic *)calloc(1, sizeof(struct mark5_format_generic));

	m->nbitstream = nbitstream;

	f->Mbps = (double)Mbps;
	f->framesperperiod = Mbps*100;
	f->alignmentseconds = 1;
	f->nchan = nchan;
	f->nbit = nbit;
	f->formatdata = m;
	f->formatdatasize = sizeof(struct mark5_format_mark5b);
	f->gettime = mark5_stream_frame_time_mark5b;
	f->init_format = mark5_format_d2k_init;
	f->final_format = mark5_format_mark5b_final;
	f->fixmjd = mark5_format_mark5b_fixmjd;
	f->validate = one;
	f->resync = onenc;
	f->genheaders = mark5_format_mark5b_genheaders;
	f->decimation = decimation;
	f->decode = 0;
	f->complex_decode = 0;
	f->iscomplex = 0;
	f->count = 0;
	switch(decoderindex)
	{
		case 0:
			f->decode = mark5b_decode_1bitstream_1bit_decimation1;
			break;
		case 1:
			f->decode = mark5b_decode_2bitstream_1bit_decimation1;
			break;
		case 2:
			f->decode = mark5b_decode_4bitstream_1bit_decimation1;
			break;
		case 3:
			f->decode = mark5b_decode_8bitstream_1bit_decimation1;
			break;
		case 4:
			f->decode = mark5b_decode_16bitstream_1bit_decimation1;
			break;
		case 5:
			f->decode = mark5b_decode_32bitstream_1bit_decimation1;
			break;
		case 7:
			f->decode = mark5b_decode_2bitstream_2bit_decimation1;
			f->count = mark5b_count_2bitstream_2bit_decimation1;
			break;
		case 8:
			f->decode = mark5b_decode_4bitstream_2bit_decimation1;
			f->count = mark5b_count_4bitstream_2bit_decimation1;
			break;
		case 9:
			f->decode = mark5b_decode_8bitstream_2bit_decimation1;
			f->count = mark5b_count_8bitstream_2bit_decimation1;
			break;
		case 10:
			f->decode = mark5b_decode_16bitstream_2bit_decimation1;
			f->count = mark5b_count_16bitstream_2bit_decimation1;
			break;
		case 11:
			f->decode = mark5b_decode_32bitstream_2bit_decimation1;
			f->count = mark5b_count_32bitstream_2bit_decimation1;
			break;
		case 12:
			f->decode = mark5b_decode_1bitstream_1bit_decimation2;
			break;
		case 13:
			f->decode = mark5b_decode_2bitstream_1bit_decimation2;
			break;
		case 14:
			f->decode = mark5b_decode_4bitstream_1bit_decimation2;
			break;
		case 15:
			f->decode = mark5b_decode_8bitstream_1bit_decimation2;
			break;
		case 16:
			f->decode = mark5b_decode_16bitstream_1bit_decimation2;
			break;
		case 17:
			f->decode = mark5b_decode_32bitstream_1bit_decimation2;
			break;
		case 19:
			f->decode = mark5b_decode_2bitstream_2bit_decimation2;
			f->count = mark5b_count_2bitstream_2bit_decimation2;
			break;
		case 20:
			f->decode = mark5b_decode_4bitstream_2bit_decimation2;
			f->count = mark5b_count_4bitstream_2bit_decimation2;
			break;
		case 21:
			f->decode = mark5b_decode_8bitstream_2bit_decimation2;
			f->count = mark5b_count_8bitstream_2bit_decimation2;
			break;
		case 22:
			f->decode = mark5b_decode_16bitstream_2bit_decimation2;
			f->count = mark5b_count_16bitstream_2bit_decimation2;
			break;
		case 23:
			f->decode = mark5b_decode_32bitstream_2bit_decimation2;
			f->count = mark5b_count_32bitstream_2bit_decimation2;
			break;
		case 24:
			/* special case needing explicit decimation4 case */
			if(decimation == 4)
			{
				f->decode = mark5b_decode_1bitstream_1bit_decimation4; 
			}
			else if(decimation % 8 == 0)
			{
				f->decode = mark5b_decode_1bitstream_1bit_decimation8; 
			}
			break;
		case 25:
			f->decode = mark5b_decode_2bitstream_1bit_decimation4;
			break;
		case 26:
			f->decode = mark5b_decode_4bitstream_1bit_decimation4;
			break;
		case 27:
			f->decode = mark5b_decode_8bitstream_1bit_decimation4;
			break;
		case 28:
			f->decode = mark5b_decode_16bitstream_1bit_decimation4;
			break;
		case 29:
			f->decode = mark5b_decode_32bitstream_1bit_decimation4;
			break;
		case 31:
			f->decode = mark5b_decode_2bitstream_2bit_decimation4;
			f->count = mark5b_count_2bitstream_2bit_decimation4;
			break;
		case 32:
			f->decode = mark5b_decode_4bitstream_2bit_decimation4;
			f->count = mark5b_count_4bitstream_2bit_decimation4;
			break;
		case 33:
			f->decode = mark5b_decode_8bitstream_2bit_decimation4;
			f->count = mark5b_count_8bitstream_2bit_decimation4;
			break;
		case 34:
			f->decode = mark5b_decode_16bitstream_2bit_decimation4;
			f->count = mark5b_count_16bitstream_2bit_decimation4;
			break;
		case 35:
			f->decode = mark5b_decode_32bitstream_2bit_decimation4;
			f->count = mark5b_count_32bitstream_2bit_decimation4;
			break;
	}

	if(f->decode == 0)
	{
		fprintf(m5stderr, "Illegal combination of decimation, bitstreams and bits\n");
		free(f);
		free(m);

		return 0;
	}

	return f;
}


