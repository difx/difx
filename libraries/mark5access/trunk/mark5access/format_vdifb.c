/***************************************************************************
 *   Copyright (C) 2009-2013 by Walter Brisken, Adam Deller, Chris Phillips*
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
// $HeadURL: $
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

// This is NOT standard VDIF format.  VDIFB is a VDIF-like format that should not
// be used for intercommunication of VLBI data.  Instead it is an intermediate
// format that is convenient to use when interleaving a multi-thread VDIF stream 
// for DiFX consumption.  The B in VDIFB stands for "Byte".  In this pseudo format
// 1, 2, 4 or 8 samples from each channel in 8, 4, 2, 1 bits/sample respectively
// are stored in one byte.  Byte N of a sequence comes from channel N%nchan.
// It is possible to extend this thought to 16 bit granularity (VDIFS) and 32 bit
// granularity (VDIFW).

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdint.h>

#include "mark5access/mark5_stream.h"

static const float HiMag = OPTIMAL_2BIT_HIGH;
static const float FourBit1sigma = 2.95;

static float lut1bit[256][8];
static float lut2bit[256][4];
static float lut4bit[256][2];
static float lut8bit[256];
static float zeros[8];
static float complex complex_zeros[8];

static float complex complex_lut1bit[256][4];
static float complex complex_lut2bit[256][2];
static float complex complex_lut4bit[256];

/* for use in counting high states; 2-bit support only at this time */
static unsigned char countlut2bit[256];

/* internal data specific to VDIFB */
struct mark5_format_vdifb
{
	int databytesperpacket;		/* = packetsize - frameheadersize */
	int frameheadersize;		/* 16 (legacy) or 32 (normal) */
	int leapsecs;			/* relative to reference epoch of VDIF data */
	int completesamplesperword;	/* number of samples for each channel in one 32-bit word */
};

static void initluts()
{
	/* Warning: these are different than for VLBA/Mark4/Mark5B! */
	const float lut2level[2] = {-1.0, 1.0};
	const float lut4level[4] = {-HiMag, -1.0, 1.0, HiMag};
	const float lut16level[16] = {-8/FourBit1sigma,-7/FourBit1sigma,-6/FourBit1sigma,-5/FourBit1sigma,-4/FourBit1sigma,
				      -3/FourBit1sigma,-2/FourBit1sigma,-1/FourBit1sigma,0,1/FourBit1sigma,2/FourBit1sigma,
				      3/FourBit1sigma,4/FourBit1sigma,5/FourBit1sigma,6/FourBit1sigma,7/FourBit1sigma};
	int b, i, l, li;
	
	for(i = 0; i < 8; i++)
	{
		zeros[i] = 0.0;
		complex_zeros[i] = 0.0+0.0*I;
	}

	for(b = 0; b < 256; b++)
	{
		/* lut1bit */
		for(i = 0; i < 8; i++)
		{
			l = (b>>i) & 0x01;
			lut1bit[b][i] =  lut2level[l];
		}

		/* lut2bit */
		countlut2bit[b] = 0;
		for(i = 0; i < 4; i++)
		{
			l = (b >> (2*i)) & 0x03;
			lut2bit[b][i] = lut4level[l];
			if(fabs(lut2bit[b][i]) > 1.1)
			{
				++countlut2bit[b];
			}
		}

		/* lut4bit */
		for(i = 0; i < 2; i++)
		{
			l = (b >> (4*i)) & 0x0F;
			lut4bit[b][i] = lut16level[l];
		}

		/* lut8bit */
		lut8bit[b] = (b*2-255)/256.0;

		/* Complex lookups */

		/* 1 bit real, 1 bit imaginary */
		for(i = 0; i < 4; i++)
		{
		         l =  (b>> (2*i)) & 0x1;
			 li = (b>> (2*i+1)) & 0x1;
			 complex_lut1bit[b][i] =  lut2level[l] + lut2level[li]*I;
		}

		/* 2 bit real, 2 bit imaginary */
		for(i = 0; i < 2; i++)
		{
		         l =  (b>> (4*i)) & 0x3;
			 li = (b>> (4*i+2)) & 0x3;
			 complex_lut2bit[b][i] =  lut4level[l] + lut4level[li]*I;
		}

		/* 4 bit real, 4 bit imaginary */
		l =  b & 0xF;
		li = (b>>4) & 0xF;
		complex_lut4bit[b] =  lut16level[l] + lut16level[li]*I;

	}
}

/* inspect frame (packet) and return mjd and time */
static int mark5_stream_frame_time_vdifb(const struct mark5_stream *ms, int *mjd, int *sec, double *ns)
{
	struct mark5_format_vdifb *v;
	unsigned int word0, word1;
	int seconds, days;
	int refepoch;

	/* table below is valid for year 2000.0 to 2032.0 and contains mjd on Jan 1 and Jul 1
	 * for each year. */
	static int mjdepochs[64] = 
	{
		51544, 51726, 51910, 52091, 52275, 52456, 52640, 52821,  /* 2000-2003 */
		53005, 53187, 53371, 53552, 53736, 53917, 54101, 54282,  /* 2004-2007 */
		54466, 54648, 54832, 55013, 55197, 55378, 55562, 55743,  /* 2008-2011 */
		55927, 56109, 56293, 56474, 56658, 56839, 57023, 57204,  /* 2012-2015 */
		57388, 57570, 57754, 57935, 58119, 58300, 58484, 58665,  /* 2016-2019 */
		58849, 59031, 59215, 59396, 59580, 59761, 59945, 60126,  /* 2020-2023 */
		60310, 60492, 60676, 60857, 61041, 61222, 61406, 61587,  /* 2024-2027 */
		61771, 61953, 62137, 62318, 62502, 62683, 62867, 63048   /* 2028-2031 */
	};

	if(!ms)
	{
		return -1;
	}
	v = (struct mark5_format_vdifb *)(ms->formatdata);

#ifdef WORDS_BIGENDIAN
	{
		unsigned char *headerbytes;

		/* Motorola byte order requires some fiddling */
		headerbytes = ms->frame;
		word0 = (headerbytes[0] << 24) | (headerbytes[1] << 16) | (headerbytes[2] << 8) | headerbytes[3];
		headerbytes = ms->frame + 4;
		word1 = (headerbytes[0] << 24) | (headerbytes[1] << 16) | (headerbytes[2] << 8) | headerbytes[3];
	}
#else
	{
		unsigned int *headerwords = (unsigned int *)(ms->frame);

		/* Intel byte order does not */
		word0 = headerwords[0];
		word1 = headerwords[1];
	}
#endif

	seconds = word0 & 0x3FFFFFFF;	/* bits 0 to 29 */
	refepoch = (word1 >> 24) & 0x3F;

	seconds += v->leapsecs;
	days = seconds/86400;
	seconds -= days*86400;
	days += mjdepochs[refepoch];

	if(mjd)
	{
		*mjd = days;
	}
	if(sec)
	{
		*sec = seconds;
	}
	if(ns)
	{
		*ns = (word1 & 0x00FFFFFF)*ms->framens;
	}

	return 0;
}

/************************* decode routines **************************/

static int vdifb_decode_1channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 8)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			memcpy(data[0]+o, zeros, 8*sizeof(float));
			++nblank;
			++i;
		}
		else
		{
			memcpy(data[0]+o, lut1bit[buf[i++]], 8*sizeof(float));
		}

		if(i >= ms->databytes)
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

	return nsamp - 8*nblank;
}

static int vdifb_decode_2channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 8)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			memcpy(data[0]+o, zeros, 8*sizeof(float));
			memcpy(data[1]+o, zeros, 8*sizeof(float));
			++nblank;
			i += 2;
		}
		else
		{
			memcpy(data[0]+o, lut1bit[buf[i++]], 8*sizeof(float));
			memcpy(data[1]+o, lut1bit[buf[i++]], 8*sizeof(float));
		}

		if(i >= ms->databytes)
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

	return nsamp - 8*nblank;
}

static int vdifb_decode_4channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 8)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			memcpy(data[0]+o, zeros, 8*sizeof(float));
			memcpy(data[1]+o, zeros, 8*sizeof(float));
			memcpy(data[2]+o, zeros, 8*sizeof(float));
			memcpy(data[3]+o, zeros, 8*sizeof(float));
			++nblank;
			i += 4;
		}
		else
		{
			memcpy(data[0]+o, lut1bit[buf[i++]], 8*sizeof(float));
			memcpy(data[1]+o, lut1bit[buf[i++]], 8*sizeof(float));
			memcpy(data[2]+o, lut1bit[buf[i++]], 8*sizeof(float));
			memcpy(data[3]+o, lut1bit[buf[i++]], 8*sizeof(float));
		}

		if(i >= ms->databytes)
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

	return nsamp - 8*nblank;
}

static int vdifb_decode_Nchannel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	int o, i, c;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 8)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			for(c = 0; c < ms->nchan; ++c)
			{
				memcpy(data[c]+o, zeros, 8*sizeof(float));
			}
			++nblank;
			i += ms->nchan;
		}
		else
		{
			for(c = 0; c < ms->nchan; ++c)
			{
				memcpy(data[c]+o, lut1bit[buf[i++]], 8*sizeof(float));
			}
		}

		if(i >= ms->databytes)
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

	return nsamp - 8*nblank;
}

static int vdifb_decode_1channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 4)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			memcpy(data[0]+o, zeros, 4*sizeof(float));
			++nblank;
			++i;
		}
		else
		{
			memcpy(data[0]+o, lut2bit[buf[i++]], 4*sizeof(float));
		}

		if(i >= ms->databytes)
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

static int vdifb_decode_2channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 4)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			memcpy(data[0]+o, zeros, 4*sizeof(float));
			memcpy(data[1]+o, zeros, 4*sizeof(float));
			++nblank;
			i += 2;
		}
		else
		{
			memcpy(data[0]+o, lut2bit[buf[i++]], 4*sizeof(float));
			memcpy(data[1]+o, lut2bit[buf[i++]], 4*sizeof(float));
		}

		if(i >= ms->databytes)
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

static int vdifb_decode_4channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 4)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			memcpy(data[0]+o, zeros, 4*sizeof(float));
			memcpy(data[1]+o, zeros, 4*sizeof(float));
			memcpy(data[2]+o, zeros, 4*sizeof(float));
			memcpy(data[3]+o, zeros, 4*sizeof(float));
			++nblank;
			i += 4;
		}
		else
		{
			memcpy(data[0]+o, lut2bit[buf[i++]], 4*sizeof(float));
			memcpy(data[1]+o, lut2bit[buf[i++]], 4*sizeof(float));
			memcpy(data[2]+o, lut2bit[buf[i++]], 4*sizeof(float));
			memcpy(data[3]+o, lut2bit[buf[i++]], 4*sizeof(float));
		}

		if(i >= ms->databytes)
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

static int vdifb_decode_Nchannel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	int o, i, c;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 4)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			for(c = 0; c < ms->nchan; ++c)
			{
				memcpy(data[c]+o, zeros, 4*sizeof(float));
			}
			++nblank;
			i += ms->nchan;
		}
		else
		{
			for(c = 0; c < ms->nchan; ++c)
			{
				memcpy(data[c]+o, lut2bit[buf[i++]], 4*sizeof(float));
			}
		}

		if(i >= ms->databytes)
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


static int vdifb_decode_1channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 2)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			memcpy(data[0]+o, zeros, 2*sizeof(float));
			++nblank;
			++i;
		}
		else
		{
			memcpy(data[0]+o, lut4bit[buf[i++]], 2*sizeof(float));
		}

		if(i >= ms->databytes)
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

static int vdifb_decode_2channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 2)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			memcpy(data[0]+o, zeros, 2*sizeof(float));
			memcpy(data[1]+o, zeros, 2*sizeof(float));
			++nblank;
			i += 2;
		}
		else
		{
			memcpy(data[0]+o, lut4bit[buf[i++]], 2*sizeof(float));
			memcpy(data[1]+o, lut4bit[buf[i++]], 2*sizeof(float));
		}

		if(i >= ms->databytes)
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

static int vdifb_decode_4channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 2)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			memcpy(data[0]+o, zeros, 2*sizeof(float));
			memcpy(data[1]+o, zeros, 2*sizeof(float));
			memcpy(data[2]+o, zeros, 2*sizeof(float));
			memcpy(data[3]+o, zeros, 2*sizeof(float));
			++nblank;
			i += 4;
		}
		else
		{
			memcpy(data[0]+o, lut4bit[buf[i++]], 2*sizeof(float));
			memcpy(data[1]+o, lut4bit[buf[i++]], 2*sizeof(float));
			memcpy(data[2]+o, lut4bit[buf[i++]], 2*sizeof(float));
			memcpy(data[3]+o, lut4bit[buf[i++]], 2*sizeof(float));
		}

		if(i >= ms->databytes)
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

static int vdifb_decode_8channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 2)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			memcpy(data[0]+o, zeros, 2*sizeof(float));
			memcpy(data[1]+o, zeros, 2*sizeof(float));
			memcpy(data[2]+o, zeros, 2*sizeof(float));
			memcpy(data[3]+o, zeros, 2*sizeof(float));
			memcpy(data[4]+o, zeros, 2*sizeof(float));
			memcpy(data[5]+o, zeros, 2*sizeof(float));
			memcpy(data[6]+o, zeros, 2*sizeof(float));
			memcpy(data[7]+o, zeros, 2*sizeof(float));
			++nblank;
			i += 8;
		}
		else
		{
			memcpy(data[0]+o, lut4bit[buf[i++]], 2*sizeof(float));
			memcpy(data[1]+o, lut4bit[buf[i++]], 2*sizeof(float));
			memcpy(data[2]+o, lut4bit[buf[i++]], 2*sizeof(float));
			memcpy(data[3]+o, lut4bit[buf[i++]], 2*sizeof(float));
			memcpy(data[4]+o, lut4bit[buf[i++]], 2*sizeof(float));
			memcpy(data[5]+o, lut4bit[buf[i++]], 2*sizeof(float));
			memcpy(data[6]+o, lut4bit[buf[i++]], 2*sizeof(float));
			memcpy(data[7]+o, lut4bit[buf[i++]], 2*sizeof(float));
		}

		if(i >= ms->databytes)
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

static int vdifb_decode_1channel_8bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fp = zeros;
			nblank++;
		}
		else
		{
			fp = &lut8bit[buf[i]];
		}

		i++;

		data[0][o] = fp[0];

		if(i >= ms->databytes)
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

static int vdifb_decode_2channel_8bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = zeros;
			++nblank;
		}
		else
		{
			fp0 = &lut8bit[buf[i++]];
			fp1 = &lut8bit[buf[i++]];
		}

		data[0][o] = fp0[0];
		data[1][o] = fp1[0];

		if(i >= ms->databytes)
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

static int vdifb_decode_4channel_8bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1, *fp2, *fp3;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = fp2 = fp3 = zeros;
			++nblank;
		}
		else
		{
			fp0 = &lut8bit[buf[i++]];
			fp1 = &lut8bit[buf[i++]];
			fp2 = &lut8bit[buf[i++]];
			fp3 = &lut8bit[buf[i++]];
		}

		data[0][o] = fp0[0];
		data[1][o] = fp1[0];
		data[2][o] = fp2[0];
		data[3][o] = fp3[0];

		if(i >= ms->databytes)
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

// Complex

#if 0

static int vdif_complex_decode_1channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fcp = complex_zeros;
			nblank++;
		}
		else
		{
			fcp = complex_lut1bit[buf[i]];
		}

		i++;

		data[0][o] = fcp[0];
		o++;
		data[0][o] = fcp[1];
		o++;
		data[0][o] = fcp[2];
		o++;
		data[0][o] = fcp[3];
		o++;

		if(i >= ms->databytes)
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

static int vdif_complex_decode_2channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fcp = complex_zeros;
			nblank++;
		}
		else
		{
			fcp = complex_lut1bit[buf[i]];
		}

		i++;

		data[0][o] = fcp[0];
		data[1][o] = fcp[1];
		o++;
		data[0][o] = fcp[2];
		data[1][o] = fcp[3];
		o++;

		if(i >= ms->databytes)
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

static int vdif_complex_decode_4channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fcp = complex_zeros;
			nblank++;
		}
		else
		{
			fcp = complex_lut1bit[buf[i]];
		}

		i++;

		data[0][o] = fcp[0];
		data[1][o] = fcp[1];
		data[2][o] = fcp[2];
		data[3][o] = fcp[3];

		if(i >= ms->databytes)
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

static int vdif_complex_decode_8channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp0, *fcp1;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
		        fcp0 = fcp1 = complex_zeros;
			nblank++;
			i += 2;
		}
		else
		{
			fcp0 = complex_lut1bit[buf[i]];
			i++;
			fcp1 = complex_lut1bit[buf[i]];
			i++;
		}

		data[0][o] = fcp0[0];
		data[1][o] = fcp0[1];
		data[2][o] = fcp0[2];
		data[3][o] = fcp0[3];
		data[4][o] = fcp1[0];
		data[5][o] = fcp1[1];
		data[6][o] = fcp1[2];
		data[7][o] = fcp1[3];

		if(i >= ms->databytes)
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

	return nsamp - nblank;  // CHECK
}

static int vdif_complex_decode_16channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp0, *fcp1, *fcp2, *fcp3;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fcp0 = fcp1 = fcp2 = fcp3 = complex_zeros;
			nblank++;
			i += 4;
		}
		else
		{
			fcp0 = complex_lut1bit[buf[i]];
			i++;
			fcp1 = complex_lut1bit[buf[i]];
			i++;
			fcp2 = complex_lut1bit[buf[i]];
			i++;
			fcp3 = complex_lut1bit[buf[i]];
			i++;
		}

		data[0][o] = fcp0[0];
		data[1][o] = fcp0[1];
		data[2][o] = fcp0[2];
		data[3][o] = fcp0[3];
		data[4][o] = fcp1[0];
		data[5][o] = fcp1[1];
		data[6][o] = fcp1[2];
		data[7][o] = fcp1[3];
		data[8][o] = fcp2[0];
		data[9][o] = fcp2[1];
		data[10][o] = fcp2[2];
		data[11][o] = fcp2[3];
		data[12][o] = fcp3[0];
		data[13][o] = fcp3[1];
		data[14][o] = fcp3[2];
		data[15][o] = fcp3[3];

		if(i >= ms->databytes)
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

static int vdif_complex_decode_1channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fcp = complex_zeros;
			nblank++;
		}
		else
		{
			fcp = complex_lut2bit[buf[i]];
		}

		i++;

		data[0][o] = fcp[0];
		o++;
		data[0][o] = fcp[1];
		o++;

		if(i >= ms->databytes)
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

static int vdif_complex_decode_2channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fcp = complex_zeros;
			nblank++;
		}
		else
		{
			fcp = complex_lut2bit[buf[i]];
		}

		i++;

		data[0][o] = fcp[0];
		data[1][o] = fcp[1];

		if(i >= ms->databytes)
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

static int vdif_complex_decode_4channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp0, *fcp1;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fcp0 = fcp1 = complex_zeros;
			nblank++;
			i += 2;
		}
		else
		{
			fcp0 = complex_lut2bit[buf[i++]];
			fcp1 = complex_lut2bit[buf[i++]];
		}


		data[0][o] = fcp0[0];
		data[1][o] = fcp0[1];
		data[2][o] = fcp1[0];
		data[3][o] = fcp1[1];

		if(i >= ms->databytes)
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

static int vdif_complex_decode_8channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp0, *fcp1, *fcp2, *fcp3;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fcp0 = fcp1 = fcp2 = fcp3 = complex_zeros;
			nblank++;
			i+=4;
		}
		else
		{
			fcp0 = complex_lut2bit[buf[i++]];
			fcp1 = complex_lut2bit[buf[i++]];
			fcp2 = complex_lut2bit[buf[i++]];
			fcp3 = complex_lut2bit[buf[i++]];
		}


		data[0][o] = fcp0[0];
		data[1][o] = fcp0[1];
		data[2][o] = fcp1[0];
		data[3][o] = fcp1[1];
		data[4][o] = fcp2[0];
		data[5][o] = fcp2[1];
		data[6][o] = fcp3[0];
		data[7][o] = fcp3[1];

		if(i >= ms->databytes)
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
//             new vdif mode added by rjc  2012.10.25
static int vdif_complex_decode_16channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp0, *fcp1, *fcp2, *fcp3, *fcp4, *fcp5, *fcp6, *fcp7;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fcp0 = fcp1 = fcp2 = fcp3 = fcp4 = fcp5 = fcp6 = fcp7 = complex_zeros;
			nblank++;
			i+=8;
		}
		else
		{
			fcp0 = complex_lut2bit[buf[i]];
			i++;
			fcp1 = complex_lut2bit[buf[i]];
			i++;
			fcp2 = complex_lut2bit[buf[i]];
			i++;
			fcp3 = complex_lut2bit[buf[i]];
			i++;
			fcp4 = complex_lut2bit[buf[i]];
			i++;
			fcp5 = complex_lut2bit[buf[i]];
			i++;
			fcp6 = complex_lut2bit[buf[i]];
			i++;
			fcp7 = complex_lut2bit[buf[i]];
			i++;
		}


		data[0][o] = fcp0[0];
		data[1][o] = fcp0[1];
		data[2][o] = fcp1[0];
		data[3][o] = fcp1[1];
		data[4][o] = fcp2[0];
		data[5][o] = fcp2[1];
		data[6][o] = fcp3[0];
		data[7][o] = fcp3[1];
		data[8][o] = fcp4[0];
		data[9][o] = fcp4[1];
		data[10][o] = fcp5[0];
		data[11][o] = fcp5[1];
		data[12][o] = fcp6[0];
		data[13][o] = fcp6[1];
		data[14][o] = fcp7[0];
		data[15][o] = fcp7[1];

		if(i >= ms->databytes)
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

static int vdif_complex_decode_1channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fcp = complex_zeros;
			nblank++;
		}
		else
		{
			fcp = &complex_lut4bit[buf[i]];
		}

		i++;

		data[0][o] = fcp[0];

		if(i >= ms->databytes)
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

static int vdif_complex_decode_2channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp0, *fcp1;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fcp0 = fcp1 = complex_zeros;
			nblank++;
			i += 2;
		}
		else
		{
			fcp0 = &complex_lut4bit[buf[i]];
			i++;
			fcp1 = &complex_lut4bit[buf[i]];
			i++;
		}

		data[0][o] = fcp0[0];
		data[1][o] = fcp0[1];

		if(i >= ms->databytes)
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

static int vdif_complex_decode_4channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp0, *fcp1, *fcp2, *fcp3;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fcp0 = fcp1 = fcp2 = fcp3 = complex_zeros;
			nblank++;
			i += 4;
		}
		else
		{
			fcp0 = &complex_lut4bit[buf[i]];
			i++;
			fcp1 = &complex_lut4bit[buf[i]];
			i++;
			fcp2 = &complex_lut4bit[buf[i]];
			i++;
			fcp3 = &complex_lut4bit[buf[i]];
			i++;
		}

		data[0][o] = fcp0[0];
		data[1][o] = fcp0[1];
		data[2][o] = fcp1[0];
		data[3][o] = fcp1[1];

		if(i >= ms->databytes)
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

static int vdif_complex_decode_1channel_8bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			data[0][o] = complex_zeros[0];
			nblank++;
		}
		else
		{
		  data[0][o] = lut8bit[buf[i]] + lut8bit[buf[i+1]]*I; 
		  i+= 2;
		}


		if(i >= ms->databytes)
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

static int vdif_complex_decode_2channel_8bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			data[0][o] = complex_zeros[0];
			data[1][o] = complex_zeros[0];
			nblank++;
		}
		else
		{
			data[0][o] = lut8bit[buf[i]] + lut8bit[buf[i+1]]*I; 
			i+=2;
			data[1][o] = lut8bit[buf[i]] + lut8bit[buf[i+1]]*I; 
			i+=2;
		}

		if(i >= ms->databytes)
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

static int vdif_complex_decode_4channel_8bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			data[0][o] = complex_zeros[0];
			data[1][o] = complex_zeros[0];
			data[2][o] = complex_zeros[0];
			data[3][o] = complex_zeros[0];
			nblank++;
		}
		else
		{
			data[0][o] = lut8bit[buf[i]] + lut8bit[buf[i+1]]*I; 
			i+=2;
			data[1][o] = lut8bit[buf[i]] + lut8bit[buf[i+1]]*I; 
			i+=2;
			data[2][o] = lut8bit[buf[i]] + lut8bit[buf[i+1]]*I; 
			i+=2;
			data[3][o] = lut8bit[buf[i]] + lut8bit[buf[i+1]]*I; 
			i+=2;
		}

		if(i >= ms->databytes)
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


static int vdif_complex_decode_1channel_16bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const uint16_t *buf;
	int o, i;
	int nblank = 0;

	buf = (const uint16_t *)ms->payload;
	i = ms->readposition/2;

	for(o = 0; o < nsamp; o++)
	{
		if(i*2 >= ms->blankzoneendvalid[0])
		{
			data[0][o] = complex_zeros[0];
			nblank++;
		}
		else
		{
		  data[0][o] = ((int16_t)(buf[i]^0x8000) + (int16_t)(buf[i+1]^0x8000)*I)/8.0;  // Assume RMS==8
		  i+= 2;
		}


		if(i*2 >= ms->databytes)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = (const uint16_t *)ms->payload;
			i = 0;
		}
	}

	ms->readposition = i*2;

	return nsamp - nblank;
}

static int vdif_complex_decode_2channel_16bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const uint16_t *buf;
	int o, i;
	int nblank = 0;

	buf = (const uint16_t *)ms->payload;
	i = ms->readposition/2;

	for(o = 0; o < nsamp; o++)
	{
		if(i*2 >= ms->blankzoneendvalid[0])
		{
			data[0][o] = complex_zeros[0];
			data[1][o] = complex_zeros[0];
			nblank++;
		}
		else
		{
		        data[0][o] = ((int16_t)(buf[i]^0x8000) + (int16_t)(buf[i+1]^0x8000)*I)/8.0;  // Assume RMS==8
			i+=2;
		        data[1][o] = ((int16_t)(buf[i]^0x8000) + (int16_t)(buf[i+1]^0x8000)*I)/8.0;  // Assume RMS==8
			i+=2;
		}

		if(i*2 >= ms->databytes)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = (const uint16_t *)ms->payload;
			i = 0;
		}
	}

	ms->readposition = i*2;

	return nsamp - nblank;
}

static int vdif_complex_decode_4channel_16bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const uint16_t *buf;
	int o, i, j;
	int nblank = 0;

	buf = (const uint16_t *)ms->payload;
	i = ms->readposition/2;

	for(o = 0; o < nsamp; o++)
	{
		if(i*2 >= ms->blankzoneendvalid[0])
		{
		  for (j=0; j<4; j++) {
			data[j][o] = complex_zeros[0];
			nblank++;
		  }
		}
		else
		{
		  for (j=0; j<4; j++) {
		        data[j][o] = ((int16_t)(buf[i]^0x8000) + (int16_t)(buf[i+1]^0x8000)*I)/8.0;  // Assume RMS==8
			i+=2;
		  }
		}

		if(i*2 >= ms->databytes)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = (const uint16_t *)ms->payload;
			i = 0;
		}
	}

	ms->readposition = i*2;

	return nsamp - nblank;
}

static int vdif_complex_decode_8channel_16bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const uint16_t *buf;
	int o, i, j;
	int nblank = 0;

	buf = (const uint16_t*)ms->payload;
	i = ms->readposition/2;

	for(o = 0; o < nsamp; o++)
	{
		if(i*2 >= ms->blankzoneendvalid[0])
		{
		  for (j=0; j<8; j++) {
			data[j][o] = complex_zeros[0];
			nblank++;
		  }
		}
		else
		{
		  for (j=0; j<8; j++) {
		        data[j][o] = ((int16_t)(buf[i]^0x8000) + (int16_t)(buf[i+1]^0x8000)*I)/8.0;  // Assume RMS==8
			i+=2;
		  }
		}

		if(i*2 >= ms->databytes)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = (const uint16_t *)ms->payload;
			i = 0;
		}
	}

	ms->readposition = i*2;

	return nsamp - nblank;
}

static int vdif_complex_decode_16channel_16bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const uint16_t *buf;
	int o, i, j;
	int nblank = 0;

	buf = (const uint16_t *)ms->payload;
	i = ms->readposition/2;

	for(o = 0; o < nsamp; o++)
	{
		if(i*2 >= ms->blankzoneendvalid[0])
		{
		  for (j=0; j<16; j++) {
			data[j][o] = complex_zeros[0];
			nblank++;
		  }
		}
		else
		{
		  for (j=0; j<16; j++) {
		        data[j][o] = ((int16_t)(buf[i]^0x8000) + (int16_t)(buf[i+1]^0x8000)*I)/8.0;  // Assume RMS==8
			i+=2;
		  }
		}

		if(i*2 >= ms->databytes)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = (const uint16_t *)ms->payload;
			i = 0;
		}
	}

	ms->readposition = i*2;

	return nsamp - nblank;
}

/* Other decoders go here */

#endif

/************************ 2-bit state counters *********************/
/* Note: these only count high vs. low states, not full state counts */

static int vdifb_count_1channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 4)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			++nblank;
			++i;
		}
		else
		{
			highstates[0] += countlut2bit[buf[i++]];
		}

		if(i >= ms->databytes)
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

static int vdifb_count_2channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 4)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			++nblank;
			i += 4;
		}
		else
		{
			highstates[0] += countlut2bit[buf[i++]];
			highstates[1] += countlut2bit[buf[i++]];
		}

		if(i >= ms->databytes)
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

static int vdifb_count_4channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += 4)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			++nblank;
			i += 4;
		}
		else
		{
			highstates[0] += countlut2bit[buf[i++]];
			highstates[1] += countlut2bit[buf[i++]];
			highstates[2] += countlut2bit[buf[i++]];
			highstates[3] += countlut2bit[buf[i++]];
		}

		if(i >= ms->databytes)
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

static int vdifb_count_Nchannel_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	int o, i, c;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o += ms->nchan)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			++nblank;
			i += ms->nchan;
		}
		else
		{
			for(c = 0; c < ms->nchan; ++c)
			{
				highstates[c] += countlut2bit[buf[i++]];
			}
		}

		if(i >= ms->databytes)
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


/******************************************************************/

static int mark5_format_vdifb_make_formatname(struct mark5_stream *ms)
{
	if(ms->format == MK5_FORMAT_VDIFB)	/* True VDIF header, not legacy */
	{
		if (ms->complex_decode) 
		{
			sprintf(ms->formatname, "VDIFBC_%d-%d-%d-%d", ms->databytes, ms->Mbps, ms->nchan, ms->nbit);
		}
		else
		{
			sprintf(ms->formatname, "VDIFB_%d-%d-%d-%d", ms->databytes, ms->Mbps, ms->nchan, ms->nbit);
		}
	}
	else
	{
		sprintf(ms->formatname, "VDIFB?");
		fprintf(m5stderr, "Warning: mark5_format_vdif_make_formatname: format not set\n");
		
		return -1;
	}

	return 0;
}

static int mark5_format_vdifb_init(struct mark5_stream *ms)
{
	struct mark5_format_vdifb *f;
	unsigned int word2;
	unsigned char *headerbytes, bitspersample;
	int framensNum, framensDen, dataframelength;
	double dns;

	if(!ms)
	{
		fprintf(m5stderr, "mark5_format_vdifb_init: ms = 0\n");

		return -1;
	}

	f = (struct mark5_format_vdifb *)(ms->formatdata);

	bitspersample = ms->nbit;
	if(ms->complex_decode)
	{
		bitspersample *= 2;
	}

	ms->payloadoffset = f->frameheadersize;
	ms->databytes = f->databytesperpacket;
	ms->framebytes = f->databytesperpacket + f->frameheadersize;
	ms->blanker = blanker_vdif;

	/* FIXME: if nbit is not a power of 2, this formula breaks down! */
	ms->samplegranularity = 8/(bitspersample*ms->decimation);
	if(ms->samplegranularity <= 0)
	{
		ms->samplegranularity = 1;
	}
	
	ms->framesamples = ms->databytes*8/(ms->nchan*bitspersample*ms->decimation);

	// Don't think these are needed..... CJP
        f->completesamplesperword = 32/(bitspersample*ms->nchan);

        ms->framegranularity = 1;
        if(ms->Mbps > 0)
        {
		framensNum = ms->databytes*8*1000;     
		framensDen = ms->Mbps;

		ms->framens = (double)framensNum/(double)framensDen;

		for(ms->framegranularity = 1; ms->framegranularity < 128; ms->framegranularity *= 2)
		{
			if((ms->framegranularity*framensNum) % framensDen == 0)
			{
				break;
			}
		}

		if(ms->framegranularity >= 128)
		{
			fprintf(m5stderr, "VDIFB Warning: cannot calculate gframens %d/%d\n",
			framensNum, framensDen);
			ms->framegranularity = 1;
		}
		ms->samprate = ms->framesamples*(1000000000.0/ms->framens);
        }
        else
        {
                fprintf(m5stderr, "Error: you must specify the data rate (Mbps) for a VDIFB mode (was set to %d)!", ms->Mbps);

		return -1;
        }

	/* Aha: we have some data to look at to further refine the format... */
	if(ms->datawindow)
	{
		ms->frame = ms->datawindow + ms->frameoffset;
		ms->payload = ms->frame + ms->payloadoffset;

#ifdef WORDS_BIGENDIAN
		/* Motorola byte order requires some fiddling */
		headerbytes = ms->frame + 8;
		word2 = (headerbytes[0] << 24) | (headerbytes[1] << 16) | (headerbytes[2] << 8) | headerbytes[3];
#else
		{
			unsigned int *headerwords = (unsigned int *)(ms->frame);

			/* Intel byte order does not */
			word2 = headerwords[2];
		}
#endif
		headerbytes = ms->frame;
		
		if(headerbytes[3] & 0x40)	/* Legacy bit */
		{
			fprintf(m5stderr, "VDIFB: legacy headers not supported.\n");

			return -1;
		}
		else
		{
			if(f->frameheadersize == 0)
			{
				f->frameheadersize = 32;
			}
			else if(f->frameheadersize != 32)
			{
				fprintf(m5stderr, "VDIFB Warning: Changing frameheadersize from %d to 32\n", f->frameheadersize);
				f->frameheadersize = 32;
			}
		}

		dataframelength = (word2 & 0x00FFFFFF)*8;
		if(f->databytesperpacket == 0)
		{
			f->databytesperpacket = dataframelength - f->frameheadersize;
		}
		else if(f->databytesperpacket != dataframelength - f->frameheadersize)
		{
			fprintf(m5stderr, "VDIFB Warning: Changing databytesperpacket from %d to %d\n",
				f->databytesperpacket, dataframelength - f->frameheadersize);
			f->databytesperpacket = dataframelength - f->frameheadersize;
		}

		ms->payloadoffset = f->frameheadersize;
		ms->databytes = f->databytesperpacket;
		ms->framebytes = f->databytesperpacket + f->frameheadersize;
		ms->framesamples = ms->databytes*8/(ms->nchan*bitspersample*ms->decimation);
		
		/* get time again so ms->framens is used */
		ms->gettime(ms, &ms->mjd, &ms->sec, &dns);
		ms->ns = (int)(dns + 0.5);

		/* WRITEME */
	}

	ms->gframens = (int)(ms->framegranularity*ms->framens + 0.5);

	if(f->frameheadersize == 32)
	{
		ms->format = MK5_FORMAT_VDIFB;
	}
	else
	{
		fprintf(m5stderr, "Error: mark5_format_vdifb_init: unsupported frameheadersize=%d\n", f->frameheadersize);
		
		return -1;
	}
	mark5_format_vdifb_make_formatname(ms);

	return 0;
}

static int mark5_format_vdifb_final(struct mark5_stream *ms)
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

static int mark5_format_vdifb_validate(const struct mark5_stream *ms)
{
	const unsigned int *header;

	if(ms->mjd && ms->framenum % ms->framegranularity == 0)
	{
		int mjd_d, mjd_t, sec_d, sec_t;
		double ns_d;
		long long ns_t;
		
		mark5_stream_frame_time_vdifb(ms, &mjd_d, &sec_d, &ns_d);

		ns_t = (long long)(ms->framenum)*(long long)(ms->gframens/ms->framegranularity) + (long long)(ms->ns);
		sec_t = ns_t / 1000000000L;
		ns_t -= (long long)sec_t * 1000000000L;
		sec_t += ms->sec;
		mjd_t = sec_t / 86400;
		sec_t -= mjd_t * 86400;
		mjd_t += ms->mjd;

		if(mjd_t != mjd_d || sec_t != sec_d || fabs((double)ns_t - ns_d) > 0.000001)
		{
			fprintf(m5stdout, "VDIFB validate[%lld]: %d %d %f : %d %d %lld\n",
				ms->framenum, mjd_d, sec_d, ns_d,   mjd_t, sec_t, ns_t);
			
			return 0;
		}
	}

	/* Check the invalid bit */
	header = (const unsigned int *)ms->frame;
	if((header[0] >> 31) & 0x01)
	{
#ifdef DEBUG
		fprintf(m5stderr, "mark5_format_vdifb_validate: Skipping invalid frame\n");
#endif

		return 0;
	}

	return 1;
}

static int mark5_format_vdifb_resync(struct mark5_stream *ms)
{
	/* FIXME: not implemented yet */
	return mark5_format_vdifb_validate(ms);
}

void mark5_format_vdifb_set_leapsecs(struct mark5_stream *ms, int leapsecs)
{
	struct mark5_format_vdifb *f;
	
	f = (struct mark5_format_vdifb *)(ms->formatdata);

	f->leapsecs = leapsecs;
}

struct mark5_format_generic *new_mark5_format_vdifb(int Mbps, 
	int nchan, int nbit, int decimation, 
	int databytesperpacket, int frameheadersize, int usecomplex)
{
	static int first = 1;
	struct mark5_format_generic *f;
	struct mark5_format_vdifb *v;
	int decoderindex = 0;

	if(first)
	{
		initluts();
		first = 0;
	}

	if(decimation == 1) /* inc by 1024 for each successive value to allow full range of nchan and nbit */
	{
		decoderindex += 0;
	}
	else
	{
		fprintf(m5stderr, "VDIFB decimation must be 1 for now\n");
		
		return 0;
	}

	if(nbit == 1) /* inc by 32 for each successive value to allow full range of nchan */
	{
		decoderindex += 0;
	}
	else if(nbit == 2)
	{
		decoderindex += 32;
	}
	else if(nbit == 4)
	{
		decoderindex += 64;
	}
	else if(nbit == 8)
	{
		decoderindex += 96;
	}
	else if(nbit == 16)
	{
		decoderindex += 128;
	}
	else
	{
		fprintf(m5stderr, "VDIFB nbit must be 1, 2, 4, 8 or 16 for now\n");
		
		return 0;
	}

	if(nchan == 1) /* inc by 1 for each legal value.  Up to 2^31 legal in principle */
	{
		decoderindex += 0;
	}
	else if(nchan == 2)
	{
		decoderindex += 1;
	}
	else if(nchan == 4)
	{
		decoderindex += 2;
	}
	else if(nchan == 8)
	{
		decoderindex += 3;
	}
	else if(nchan == 16)
	{
		decoderindex += 4;
	}
	else if(nchan == 32)
	{
		decoderindex += 5;
	}
	else if(nchan == 64)
	{
		decoderindex += 6;
	}
	else
	{
		fprintf(m5stderr, "VDIFB nchan must be 1, 2, 4, 8, 16, 32 or 64 for now\n");

		return 0;
	}

	v = (struct mark5_format_vdifb *)calloc(1, sizeof(struct mark5_format_vdifb));
	f = (struct mark5_format_generic *)calloc(1, sizeof(struct mark5_format_generic));

	v->frameheadersize = frameheadersize;
	v->databytesperpacket = databytesperpacket;

	f->Mbps = Mbps;
	f->nchan = nchan;
	f->nbit = nbit;
	f->formatdata = v;
	f->formatdatasize = sizeof(struct mark5_format_vdifb);
	
	/* set some function pointers */
	f->gettime = mark5_stream_frame_time_vdifb;
	f->init_format = mark5_format_vdifb_init;
	f->final_format = mark5_format_vdifb_final;
	f->validate = mark5_format_vdifb_validate;
	f->resync = mark5_format_vdifb_resync;
	f->decimation = decimation;
	f->decode = 0;
	f->complex_decode = 0;
	f->count = 0;

	if(!usecomplex) 
	{
	    switch(decoderindex)
	    {
	        case 0:
			f->decode = vdifb_decode_1channel_1bit_decimation1;
			break;
		case 1:
			f->decode = vdifb_decode_2channel_1bit_decimation1;
			break;
		case 2:
			f->decode = vdifb_decode_4channel_1bit_decimation1;
			break;
		case 3:
		case 4:
	        case 5:
			f->decode = vdifb_decode_Nchannel_1bit_decimation1;
			break;

		case 32:
			f->decode = vdifb_decode_1channel_2bit_decimation1;
			f->count = vdifb_count_1channel_2bit_decimation1;
			break;
		case 33:
			f->decode = vdifb_decode_2channel_2bit_decimation1;
			f->count = vdifb_count_2channel_2bit_decimation1;
			break;
		case 34:
			f->decode = vdifb_decode_4channel_2bit_decimation1;
			f->count = vdifb_count_4channel_2bit_decimation1;
			break;
		case 35:
		case 36:
			f->decode = vdifb_decode_Nchannel_2bit_decimation1;
			f->count = vdifb_count_Nchannel_2bit_decimation1;
			break;

		case 64:
			f->decode = vdifb_decode_1channel_4bit_decimation1;
			break;
		case 65:
			f->decode = vdifb_decode_2channel_4bit_decimation1;
			break;
		case 66:
			f->decode = vdifb_decode_4channel_4bit_decimation1;
			break;
		case 67:
			f->decode = vdifb_decode_8channel_4bit_decimation1;
			break;

		case 96:
			f->decode = vdifb_decode_1channel_8bit_decimation1;
			break;
		case 97:
			f->decode = vdifb_decode_2channel_8bit_decimation1;
			break;
		case 98:
			f->decode = vdifb_decode_4channel_8bit_decimation1;
			break;
	    }

	    if(f->decode == 0)
	    {
		fprintf(m5stderr, "VDIFB: Illegal combination of decimation, channels and bits\n");
		free(v);
		free(f);
		
		return 0;
	    }
	}
	else
	{
	    fprintf(m5stderr, "VDIFB: complex not supported yet\n");
	    free(v);
	    free(f);

	    return 0;
#if 0
	    switch(decoderindex)
	    {
	        case 0:
			f->complex_decode = vdifb_complex_decode_1channel_1bit_decimation1;
			break;
		case 1:
			f->complex_decode = vdifb_complex_decode_2channel_1bit_decimation1;
			break;
		case 2:
			f->complex_decode = vdifb_complex_decode_4channel_1bit_decimation1;
			break;
		case 3:
			f->complex_decode = vdifb_complex_decode_8channel_1bit_decimation1;
			break;
		case 4:
			f->complex_decode = vdifb_complex_decode_16channel_1bit_decimation1;
			break;

		case 32:
			f->complex_decode = vdifb_complex_decode_1channel_2bit_decimation1;
			break;
		case 33:
			f->complex_decode = vdifb_complex_decode_2channel_2bit_decimation1;
			break;
		case 34:
			f->complex_decode = vdifb_complex_decode_4channel_2bit_decimation1;
			break;
		case 35:
			f->complex_decode = vdifb_complex_decode_8channel_2bit_decimation1;
			break;
		case 36:
			f->complex_decode = vdifb_complex_decode_16channel_2bit_decimation1;
			break;

		case 64:
			f->complex_decode = vdifb_complex_decode_1channel_4bit_decimation1;
			break;
		case 65:
			f->complex_decode = vdifb_complex_decode_2channel_4bit_decimation1;
			break;
		case 66:
			f->complex_decode = vdifb_complex_decode_4channel_4bit_decimation1;
			break;

		case 96:
			f->complex_decode = vdifb_complex_decode_1channel_8bit_decimation1;
			break;
		case 97:
			f->complex_decode = vdifb_complex_decode_2channel_8bit_decimation1;
			break;
		case 98:
			f->complex_decode = vdifb_complex_decode_4channel_8bit_decimation1;
			break;

		case 128:
			f->complex_decode = vdifb_complex_decode_1channel_16bit_decimation1;
			break;
		case 129:
			f->complex_decode = vdifb_complex_decode_2channel_16bit_decimation1;
			break;
		case 130:
			f->complex_decode = vdifb_complex_decode_4channel_16bit_decimation1;
			break;
		case 131:
			f->complex_decode = vdifb_complex_decode_8channel_16bit_decimation1;
			break;
		case 132:
			f->complex_decode = vdifb_complex_decode_16channel_16bit_decimation1;
			break;
	    }

	    if(f->complex_decode == 0)
	    {
		fprintf(m5stderr, "VDIF: Illegal combination of decimation, channels and bits\n");
		free(v);
		free(f);

		return 0;
	    }
#endif
	}

	return f;
}

/* here framesize includes the 32 byte header 
 *
 * return value: 1 if true, 0 if false
 *
 * Note: this only works for nbit = 2^n
 */
static int is_legal_vdifb_framesize(int framesize)
{
	framesize -= 32;

	if(framesize % 8 != 0)
	{
		return 0;
	}

	framesize /= 8;

	while(framesize % 2 == 0)
	{
		framesize /= 2;
	}
	while(framesize % 5 == 0)
	{
		framesize /= 5;
	}

	if(framesize != 1)
	{
		return 0;
	}
	else
	{
		return 1;
	}
}

/* if *framesize is set to 0, the frame size will be determined by this call
 * and returned into the same variable.
 *
 * return value is -1 on error (no VDIF found) or 0 if found.
 *
 * here framesize includes the 32 byte header
 */
int find_vdifb_frame(const unsigned char *data, size_t length, size_t *offset, int *framesize)
{
	int fs, fs0, fs1;

	if(framesize && *framesize)
	{
		fs0 = fs1 = *framesize;
	}
	else
	{
		fs0 = 40;
		fs1 = 8232;
	}

	for(fs = fs0; fs <= fs1; ++fs)
	{
		size_t maxOffset;

		if(!is_legal_vdifb_framesize(fs))
		{
			continue;
		}

		maxOffset = 5*fs;	/* check over a maximum of 5 frame lengths */
		if(maxOffset > length - fs - 32)
		{
			maxOffset = length - fs - 32;
		}

		for(*offset = 0; *offset < maxOffset; *offset += 8)
		{
			unsigned int secA, secB;
			unsigned int refEpochA, refEpochB;
			unsigned int fsA, fsB;
			unsigned int edvA, edvB;
			const unsigned int *frame;

			frame = ((unsigned int *)data) + *offset/4;
			secA      = frame[0] & 0x3FFFFFFF;
			refEpochA = (frame[1] >> 24) & 0x3F;
			fsA       = (frame[2] & 0x00FFFFFF) << 3;
			edvA      = frame[4] >> 24;
			frame += fs/4;
			secB      = frame[0] & 0x3FFFFFFF;
			refEpochB = (frame[1] >> 24) & 0x3F;
			fsB       = (frame[2] & 0x00FFFFFF) << 3;
			edvB      = frame[4] >> 24;

			/* does it look reasonable? */
			if(fsA == fs && fsB == fs && refEpochA == refEpochB && (secA == secB || secA+1 == secB) && edvA == edvB)
			{
				*framesize = fs;

				return 0;
			}

		}
	}

	return -1;
}

int get_vdifb_chans_per_thread(const unsigned char *data)
{
	return 1 << (data[11] & 0x1F);
}

int get_vdifb_quantization_bits(const unsigned char *data)
{
	return ((data[15] >> 2) & 0x1F) + 1;
}

int get_vdifb_complex(const unsigned char *data)
{
	return data[15] >> 7;
}

int get_vdifb_threads(const unsigned char *data, size_t length, int dataframesize)
{
	const int maxThreads = 128;
	size_t i;
	unsigned short int threads[maxThreads];
	unsigned int nThread = 0;

	for(i = 0; i < length-32; i += dataframesize)
	{
		const unsigned int *frame;
		unsigned short int thread;
		unsigned short int t; 
		
		frame = (unsigned int *)(data+i);

		// sanity check that we are still in sync
		if(dataframesize != (frame[2] & 0x00FFFFFF) << 3)
		{
			break;
		}
		thread = (frame[3] >> 16) & 0x03FF;
		if(nThread == 0)
		{
			threads[0] = thread;
			++nThread;
		}
		else
		{
			for(t = 0; t < nThread; ++t)
			{
				if(threads[t] == thread)
				{
					break;
				}
			}
			if(t == nThread)
			{
				if(nThread == maxThreads)
				{
					return -1;
				}
				threads[nThread] = thread;
				++nThread;
			}
		}
	}

	return nThread;
}
