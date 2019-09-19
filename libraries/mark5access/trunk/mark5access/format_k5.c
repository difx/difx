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

//#define K5_PAYLOADSIZE 10000

#define NSUBMODE	2
#define MAXHEADERSIZE	32

// See ftp://ivscc.gsfc.nasa.gov/pub/TOW/tow2005/notebook/Koyama1.Sem.pdf

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
	128000000,
	256000000,
	512000000,
	1024000000,
	2048000000
};

const unsigned int k5Sync       = 0xFFFFFFFF;

/* the high mag value for 2-bit reconstruction */
static const float HiMag = OPTIMAL_2BIT_HIGH;
static const float FourBit1sigma = 2.95;

/* struct mark5_format_k5  // needs to be adjusted */
/* { */
/* 	int nbitstreams; */
/* 	int kday;	/\* kilo-mjd: ie 51000, 52000, ... *\/ */
/* }; */

static float lut1bit[256][8];
static float lut2bit[256][4];
static float lut4bit[256][2];
static float lut8bit[256];
static float zeros[8];

static void initluts()
{
	int b, i, s, m, l;
	const float lut2level[2] = {-1.0, 1.0};
	const float lut4level[4] = {-HiMag, 1.0, -1.0, HiMag};
	const float lut16level[16] = {-8/FourBit1sigma,-7/FourBit1sigma,-6/FourBit1sigma,-5/FourBit1sigma,-4/FourBit1sigma,
				      -3/FourBit1sigma,-2/FourBit1sigma,-1/FourBit1sigma,0,1/FourBit1sigma,2/FourBit1sigma,
				      3/FourBit1sigma,4/FourBit1sigma,5/FourBit1sigma,6/FourBit1sigma,7/FourBit1sigma};

	for(i = 0; i < 8; i++)
	{
		zeros[i] = 0.0;
	}

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
		/* lut4bit */
		for(i = 0; i < 2; i++)
		{
			l = (b >> (4*i)) & 0x0F;
			lut4bit[b][i] = lut16level[l];
		}

		/* lut8bit */
		lut8bit[b] = (b*2-255)/256.0;
	}
}

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

static int k5_decode_1channel_1bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	unsigned char *buf;
	float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])       //  Where defined?
		{
			fp = zeros;
			nblank++;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
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

		if(i >= ms->Mbps*(1000000/8))
		{
		  if(mark5_stream_next_frame(ms) < 0) // ??
			{
				return -1;
			}
			buf = ms->payload;
			i = 0;
		}
	}

	ms->readposition = i;

	return nsamp-8*nblank; // nblank * nticks per byte (= 8/nbit/nIF)
}

static int k5_decode_1channel_2bit(struct mark5_stream *ms, int nsamp,
	float **data)
{ // NIF = 1 NBIT = 2
	unsigned char *buf;
	float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			nblank++;
		}
		else
		{
			fp = lut2bit[buf[i]];
		}
		i++;

		data[0][o] = fp[0];
		o++;
		data[0][o] = fp[1];
		o++;
		data[0][o] = fp[2];
		o++;
		data[0][o] = fp[3];

		if(i >= ms->Mbps*(1000000/8))
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

static int k5_decode_1channel_4bit(struct mark5_stream *ms, int nsamp,
	float **data)
{	
	unsigned char *buf;
	float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			nblank++;
		}
		else
		{
			fp = lut4bit[buf[i]];
		}

		i++;

		data[0][o] = fp[0];
		o++;
		data[0][o] = fp[1];

		if(i >= ms->Mbps*(1000000/8))
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

static int k5_decode_1channel_8bit(struct mark5_stream *ms, int nsamp,
	float **data)
{	
	unsigned char *buf;
	float fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = 0;
			nblank++;
		}
		else
		{
			fp = lut8bit[buf[i]];
		}

		i++;

		data[0][o] = fp;

		if(i >= ms->Mbps*(1000000/8))
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

static int k5_decode_2channel_1bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	unsigned char *buf;
	float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			nblank++;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
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

		if(i >= ms->Mbps*(1000000/8))
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

static int k5_decode_2channel_2bit(struct mark5_stream *ms, int nsamp,
	float **data)
{ // NIF = 2 NBIT = 2
	unsigned char *buf;
	float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			nblank++;
		}
		else
		{
			fp = lut2bit[buf[i]];
		}
		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		o++;
		data[0][o] = fp[2];
		data[1][o] = fp[3];

		if(i >= ms->Mbps*(1000000/8))
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

static int k5_decode_2channel_4bit(struct mark5_stream *ms, int nsamp,
	float **data)
{ // NIF = 2 NBIT = 2
	unsigned char *buf;
	float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			nblank++;
		}
		else
		{
			fp = lut4bit[buf[i]];
		}
		i++;

		data[0][o] = fp[0];
		o++;
		data[0][o] = fp[1];
		o++;
		data[0][o] = fp[2];
		o++;
		data[0][o] = fp[3];

		if(i >= ms->Mbps*(1000000/8))
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

static int k5_decode_2channel_8bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	unsigned char *buf;
	float fp0, fp1;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
		        fp0 = fp1 = 0;
			i += 2;
			nblank++;
		}
		else
		{
			fp0 = lut8bit[buf[i]];
			i++;
			fp1 = lut8bit[buf[i]];
			i++;
		}

		data[0][o] = fp0;
		data[1][o] = fp1;

		if(i >= ms->Mbps*(1000000/8))
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

static int k5_decode_4channel_1bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	unsigned char *buf;
	float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			nblank++;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
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

		if(i >= ms->Mbps*(1000000/8))
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

static int k5_decode_4channel_2bit(struct mark5_stream *ms, int nsamp,
	float **data)
{ // NIF = 4 NBIT = 2
	unsigned char *buf;
	float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			nblank++;
		}
		else
		{
			fp = lut2bit[buf[i]];
		}
		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];

		if(i >= ms->Mbps*(1000000/8))
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

static int k5_decode_4channel_4bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	unsigned char *buf;
	float *fp0, *fp1;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = zeros;
			i += 2;
			nblank++;
		}
		else
		{
			fp0 = lut4bit[buf[i]];
			i++;
			fp1 = lut4bit[buf[i]];
			i++;
		}

		data[0][o] = fp0[0];
		data[1][o] = fp0[1];
		data[2][o] = fp1[0];
		data[3][o] = fp1[1];

		if(i >= ms->Mbps*(1000000/8))
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

static int k5_decode_4channel_8bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	unsigned char *buf;
	float fp0,fp1,fp2,fp3;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = fp2 = fp3 = 0;
			i += 4;
			nblank++;
		}
		else
		{
			fp0 = lut8bit[buf[i]];
			i++;
			fp1 = lut8bit[buf[i]];
			i++;
			fp2 = lut8bit[buf[i]];
			i++;
			fp3 = lut8bit[buf[i]];
			i++;
		}

		i++;

		data[0][o] = fp0;
		data[1][o] = fp1;
		data[2][o] = fp2;
		data[3][o] = fp3;

		if(i >= ms->Mbps*(1000000/8))
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


static int k5_decode_8channel_1bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	unsigned char *buf;
	float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			nblank++;
		}
		else
		{
			fp = lut1bit[buf[i]];
		}
		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];
		data[4][o] = fp[4];
		data[5][o] = fp[5];
		data[6][o] = fp[6];
		data[7][o] = fp[7];

		if(i >= ms->Mbps*(1000000/8))
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

static int k5_decode_8channel_2bit(struct mark5_stream *ms, int nsamp,
	float **data)
{ // NIF = 8 NBIT = 2
	unsigned char *buf;
	float *fp0, *fp1;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = zeros;
			i += 2;
			nblank++;
		}
		else
		{
			fp0 = lut2bit[buf[i]];
			i++;
			fp1 = lut2bit[buf[i]];
			i++;
		}

		data[0][o] = fp0[0];
		data[1][o] = fp0[1];
		data[2][o] = fp0[2];
		data[3][o] = fp0[3];
		data[4][o] = fp1[0];
		data[5][o] = fp1[1];
		data[6][o] = fp1[2];
		data[7][o] = fp1[3];

		if(i >= ms->Mbps*(1000000/8))
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

static int k5_decode_16channel_1bit(struct mark5_stream *ms, int nsamp,
	float **data)
{
	unsigned char *buf;
	float *fp1,*fp0;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = zeros;
			i += 2;
			nblank++;
		}
		else
		{
			fp0 = lut1bit[buf[i]];
			i++;
			fp1 = lut1bit[buf[i]];
			i++;
		}

		data[0][o] = fp0[0];
		data[1][o] = fp0[1];
		data[2][o] = fp0[2];
		data[3][o] = fp0[3];
		data[4][o] = fp0[4];
		data[5][o] = fp0[5];
		data[6][o] = fp0[6];
		data[7][o] = fp0[7];
		data[8][o] = fp1[0];
		data[9][o] = fp1[1];
		data[10][o]= fp1[2];
		data[11][o]= fp1[3];
		data[12][o]= fp1[4];
		data[13][o]= fp1[5];
		data[14][o]= fp1[6];
		data[15][o]= fp1[7];

		if(i >= ms->Mbps*(1000000/8))
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

static int k5_decode_16channel_2bit(struct mark5_stream *ms, int nsamp,
	float **data)
{ // NIF = 16 NBIT = 2
	unsigned char *buf;
	float *fp0, *fp1, *fp2, *fp3;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i <  ms->blankzonestartvalid[0] ||
		   i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = fp2 = fp3 = zeros;
			i += 4;
			nblank++;
		}
		else
		{
			fp0 = lut2bit[buf[i]];
			i++;
			fp1 = lut2bit[buf[i]];
			i++;
			fp2 = lut2bit[buf[i]];
			i++;
			fp3 = lut2bit[buf[i]];
			i++;
		}

		data[0][o] = fp0[0];
		data[1][o] = fp0[1];
		data[2][o] = fp0[2];
		data[3][o] = fp0[3];
		data[4][o] = fp1[0];
		data[5][o] = fp1[1];
		data[6][o] = fp1[2];
		data[7][o] = fp1[3];
		data[8][o] = fp2[0];
		data[9][o] = fp2[1];
		data[10][o]= fp2[2];
		data[11][o]= fp2[3];
		data[12][o]= fp3[0];
		data[13][o]= fp3[1];
		data[14][o]= fp3[2];
		data[15][o]= fp3[3];

		if(i >= ms->Mbps*(1000000/8))
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

static int k5_decode_8channel_4bit(struct mark5_stream *ms, int nsamp,
	float **data)
{	return 0; }

static int k5_decode_8channel_8bit(struct mark5_stream *ms, int nsamp,
	float **data)
{	return 0; }

static int k5_decode_16channel_4bit(struct mark5_stream *ms, int nsamp,
	float **data)
{	return 0; }

static int k5_decode_16channel_8bit(struct mark5_stream *ms, int nsamp,
	float **data)
{	return 0; }

static int format_k5_make_formatname(struct mark5_stream *ms)
{
	struct format_k5 *k;

	k = (struct format_k5 *)(ms->formatdata);
	
	snprintf(ms->formatname, MARK5_STREAM_ID_LENGTH,
		"%s-%d-%d-%d", k5formatname[k->submode],
		(int)(ms->Mbps + 0.5), ms->nchan, ms->nbit);

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

		nbit = 1 << (k->header[6] >> 6);  // 1, 2, 4 or 8
		nchan = (k->header[6] & 0x02) ? 4 : 1; // 1 or 4
		// But 16 channels seem to exist .. does this use 4 files?
		rate = k5samprate[(k->header[6] >> 2) & 0x0F]; // as in look up table
		Mbps = (int)((uint64_t)rate*nbit*nchan/1000000);

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
			ms->Mbps = (double)Mbps;
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

	// Don't we need to define the payloadoffset here?
	// And what does K5_PAYLOAD need to be?  
	/* From mark5_format_mark5b */
	ms->payloadoffset = k5headersize[k->submode]; // 8 B or 32B  
	ms->blanker = blanker_mark5; // Does not exist?
	

	ms->databytes = 1000000*ms->Mbps/8;
	ms->framebytes = ms->databytes + k5headersize[k->submode];
	ms->framesamples = (int)((uint64_t)1000000*ms->Mbps/(ms->nbit*ms->nchan));
	ms->samprate = ms->framesamples;
	ms->framens = 1000000000.0; // 1 sec

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
			"nbit needs to be 1, 2, 4 or 8\n");
		return 0;
	}

	if(ms->nchan == 1)
	{
		decoderindex += 0;
	}
	else if(ms->nchan == 2)
	{
		decoderindex += 4;
	}
	else if(ms->nchan == 4)
	{
		decoderindex += 8;
	}
	else if(ms->nchan == 8)
	{
		decoderindex += 16;
	}
	else if(ms->nchan == 16)
	{
		decoderindex += 32;
	}
	else
	{
		fprintf(m5stderr, "format_k5_init : "
			"nchan needs to be 1, 2, 4, 8 or 16\n");
		return 0;
	}

	ms->decode = 0;
	switch(decoderindex)
	{
		case 0 : ms->decode = k5_decode_1channel_1bit; break;
		case 1 : ms->decode = k5_decode_1channel_2bit; break;
		case 2 : ms->decode = k5_decode_1channel_4bit; break;
		case 3 : ms->decode = k5_decode_1channel_8bit; break;

		case 4 : ms->decode = k5_decode_2channel_1bit; break;
		case 5 : ms->decode = k5_decode_2channel_2bit; break;
		case 6 : ms->decode = k5_decode_2channel_4bit; break;
		case 7 : ms->decode = k5_decode_2channel_8bit; break;

		case 8 : ms->decode = k5_decode_4channel_1bit; break;
		case 9 : ms->decode = k5_decode_4channel_2bit; break;
		case 10: ms->decode = k5_decode_4channel_4bit; break;
		case 11: ms->decode = k5_decode_4channel_8bit; break;
		  // Space for 3 chan here
		case 16: ms->decode = k5_decode_8channel_1bit; break;
		case 17: ms->decode = k5_decode_8channel_2bit; break;
		case 18: ms->decode = k5_decode_8channel_4bit; break;
		case 19: ms->decode = k5_decode_8channel_8bit; break;
		  // Space for other chan here
		case 32: ms->decode = k5_decode_16channel_1bit; break;
		case 33: ms->decode = k5_decode_16channel_2bit; break;
		case 34: ms->decode = k5_decode_16channel_4bit; break;
		case 35: ms->decode = k5_decode_16channel_8bit; break;

	default: break;
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

struct mark5_format_generic *new_mark5_format_k5(int Mbps, int nchan, int nbit, int submode)
{
	static int first = 1;
	struct format_k5 *k;
	struct mark5_format_generic *f;
	k = (struct format_k5 *)calloc(1, sizeof(struct format_k5));
	f = (struct mark5_format_generic *)calloc(1, 
	        sizeof(struct mark5_format_generic));

	if(first)
	{
		initluts();
		first = 0;
	}

	k->days = 0;
	k->submode = submode;
	memset(k->header, 0, 32);

	f->Mbps = (double)Mbps;
	f->nchan = nchan;
	f->nbit = nbit;
	f->formatdata = k;
	f->formatdatasize = sizeof(struct format_k5);
	f->gettime = stream_frame_time_k5;
	f->init_format = format_k5_init;
	f->final_format = format_k5_final;
	f->fixmjd = format_k5_fixmjd;
	f->iscomplex = 0;	
	f->validate = one;
	//f->resync = one;
	f->decode = NULL;
	if (nbit==1) {
	  switch (nchan) {
	  case 1: f->decode = k5_decode_1channel_1bit;
	    break;
	  case 2: f->decode = k5_decode_2channel_1bit;
	    break;
	  case 4: f->decode = k5_decode_4channel_1bit;
	    break;
	  case 8: f->decode = k5_decode_8channel_1bit;
	    break;
	  case 16: f->decode = k5_decode_16channel_1bit;
	    break;
	  default: 
	    break;
	  }} else if (nbit==2) { 
	  switch (nchan) {
	  case 1: f->decode = k5_decode_1channel_2bit;
	    break;
	  case 2: f->decode = k5_decode_2channel_2bit;
	    break;
	  case 4: f->decode = k5_decode_4channel_2bit;
	    break;
	  case 8: f->decode = k5_decode_8channel_2bit;
	    break;
	  case 16: f->decode = k5_decode_16channel_2bit;
	    break;
	  default: 
	    break;
	  }}
	  if (!f->decode) {
		fprintf(m5stderr, "Warning: new_mark5_format_k5: format not set\n");
		return NULL;
	  }
	return f;
}
