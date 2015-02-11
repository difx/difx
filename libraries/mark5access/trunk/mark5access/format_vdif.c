/***************************************************************************
 *   Copyright (C) 2009-2014 by Walter Brisken, Adam Deller, Chris Phillips*
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
static unsigned char countlut2bit[256][4];

/* internal data specific to VDIF */
struct mark5_format_vdif
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
		for(i = 0; i < 4; i++)
		{
			l = (b >> (2*i)) & 0x03;
			lut2bit[b][i] = lut4level[l];
			if(fabs(lut2bit[b][i]) < 1.1)
			{
				countlut2bit[b][i] = 0;
			}
			else
			{
				countlut2bit[b][i] = 1;
			}
		}

		/* lut4bit */
		for(i = 0; i < 2; i++)
		{
			l = (b >> (4*i)) & 0x0F;
			lut4bit[b][i] = lut16level[l];
		}

		/* lut8bit */
		lut8bit[b] = (b*2-255)/71.0;	/* This scaling mimics 2-bit data reasonably well. */

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
static int mark5_stream_frame_time_vdif(const struct mark5_stream *ms, int *mjd, int *sec, double *ns)
{
	struct mark5_format_vdif *v;
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
	v = (struct mark5_format_vdif *)(ms->formatdata);

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

static int vdif_decode_1channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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

static int vdif_decode_2channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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

/* this is 3 channels packed into a 4-channel format.  The highest number channel is ignored */
static int vdif_decode_3channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			fp = lut1bit[buf[i]];
		}

		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		o++;
		data[0][o] = fp[4];
		data[1][o] = fp[5];
		data[2][o] = fp[6];

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

static int vdif_decode_4channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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

/* this is 5 channels packed into a 8-channel format.  The 3 highest channels are ignored */
static int vdif_decode_5channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			fp = lut1bit[buf[i]];
		}

		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];
		data[4][o] = fp[4];

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

/* this is 6 channels packed into a 8-channel format.  The 2 highest channels are ignored */
static int vdif_decode_6channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			fp = lut1bit[buf[i]];
		}

		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];
		data[4][o] = fp[4];
		data[5][o] = fp[5];

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

/* this is 7 channels packed into a 8-channel format.  The highest channel is ignored */
static int vdif_decode_7channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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

static int vdif_decode_8channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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

static int vdif_decode_16channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
			i += 2;
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
		data[10][o] = fp1[2];
		data[11][o] = fp1[3];
		data[12][o] = fp1[4];
		data[13][o] = fp1[5];
		data[14][o] = fp1[6];
		data[15][o] = fp1[7];

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

static int vdif_decode_32channel_1bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
			i += 4;
		}
		else
		{
			fp0 = lut1bit[buf[i]];
			i++;
			fp1 = lut1bit[buf[i]];
			i++;
			fp2 = lut1bit[buf[i]];
			i++;
			fp3 = lut1bit[buf[i]];
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

static int vdif_decode_1channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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

static int vdif_decode_2channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			fp = lut2bit[buf[i]];
		}

		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		o++;
		data[0][o] = fp[2];
		data[1][o] = fp[3];

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

static int vdif_decode_3channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			fp = lut2bit[buf[i]];
		}

		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];

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

static int vdif_decode_4channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			fp = lut2bit[buf[i]];
		}

		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];
		data[2][o] = fp[2];
		data[3][o] = fp[3];

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

static int vdif_decode_5channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
			i+=2;
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

static int vdif_decode_6channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
			i+=2;
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

static int vdif_decode_7channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
			i+=2;
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

static int vdif_decode_8channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
			i+=2;
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

static int vdif_decode_16channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
			i+=4;
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
		data[10][o] = fp2[2];
		data[11][o] = fp2[3];
		data[12][o] = fp3[0];
		data[13][o] = fp3[1];
		data[14][o] = fp3[2];
		data[15][o] = fp3[3];

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

static int vdif_decode_32channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1, *fp2, *fp3, *fp4, *fp5, *fp6, *fp7;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = fp2 = fp3 = fp4 = fp5 = fp6 = fp7 = zeros;
			nblank++;
			i+=8;
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
			fp4 = lut2bit[buf[i]];
			i++;
			fp5 = lut2bit[buf[i]];
			i++;
			fp6 = lut2bit[buf[i]];
			i++;
			fp7 = lut2bit[buf[i]];
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
		data[10][o] = fp2[2];
		data[11][o] = fp2[3];
		data[12][o] = fp3[0];
		data[13][o] = fp3[1];
		data[14][o] = fp3[2];
		data[15][o] = fp3[3];
		data[16][o] = fp4[0];
		data[17][o] = fp4[1];
		data[18][o] = fp4[2];
		data[19][o] = fp4[3];
		data[20][o] = fp5[0];
		data[21][o] = fp5[1];
		data[22][o] = fp5[2];
		data[23][o] = fp5[3];
		data[24][o] = fp6[0];
		data[25][o] = fp6[1];
		data[26][o] = fp6[2];
		data[27][o] = fp6[3];
		data[28][o] = fp7[0];
		data[29][o] = fp7[1];
		data[30][o] = fp7[2];
		data[31][o] = fp7[3];

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


static int vdif_decode_1channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			fp = lut4bit[buf[i]];
		}

		i++;

		data[0][o] = fp[0];
		o++;
		data[0][o] = fp[1];

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

static int vdif_decode_2channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			fp = lut4bit[buf[i]];
		}

		i++;

		data[0][o] = fp[0];
		data[1][o] = fp[1];

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

static int vdif_decode_3channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
			i += 2;
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

static int vdif_decode_4channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
			i += 2;
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

static int vdif_decode_5channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
			i += 4;
		}
		else
		{
			fp0 = lut4bit[buf[i]];
			i++;
			fp1 = lut4bit[buf[i]];
			i++;
			fp2 = lut4bit[buf[i]];
			i++;
			fp3 = lut4bit[buf[i]];
			i++;
		}

		data[0][o] = fp0[0];
		data[1][o] = fp0[1];
		data[2][o] = fp1[0];
		data[3][o] = fp1[1];
		data[4][o] = fp2[0];

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

static int vdif_decode_6channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
			i += 4;
		}
		else
		{
			fp0 = lut4bit[buf[i]];
			i++;
			fp1 = lut4bit[buf[i]];
			i++;
			fp2 = lut4bit[buf[i]];
			i++;
			fp3 = lut4bit[buf[i]];
			i++;
		}

		data[0][o] = fp0[0];
		data[1][o] = fp0[1];
		data[2][o] = fp1[0];
		data[3][o] = fp1[1];
		data[4][o] = fp2[0];
		data[5][o] = fp2[1];

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

static int vdif_decode_7channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
			i += 4;
		}
		else
		{
			fp0 = lut4bit[buf[i]];
			i++;
			fp1 = lut4bit[buf[i]];
			i++;
			fp2 = lut4bit[buf[i]];
			i++;
			fp3 = lut4bit[buf[i]];
			i++;
		}

		data[0][o] = fp0[0];
		data[1][o] = fp0[1];
		data[2][o] = fp1[0];
		data[3][o] = fp1[1];
		data[4][o] = fp2[0];
		data[5][o] = fp2[1];
		data[6][o] = fp3[0];

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

static int vdif_decode_8channel_4bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
			i += 4;
		}
		else
		{
			fp0 = lut4bit[buf[i]];
			i++;
			fp1 = lut4bit[buf[i]];
			i++;
			fp2 = lut4bit[buf[i]];
			i++;
			fp3 = lut4bit[buf[i]];
			i++;
		}

		data[0][o] = fp0[0];
		data[1][o] = fp0[1];
		data[2][o] = fp1[0];
		data[3][o] = fp1[1];
		data[4][o] = fp2[0];
		data[5][o] = fp2[1];
		data[6][o] = fp3[0];
		data[7][o] = fp3[1];

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

static int vdif_decode_1channel_8bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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

static int vdif_decode_2channel_8bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
		}
		else
		{
			fp0 = &lut8bit[buf[i]];
			i++;
			fp1 = &lut8bit[buf[i]];
			i++;
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

static int vdif_decode_3channel_8bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
		}
		else
		{
			fp0 = &lut8bit[buf[i]];
			i++;
			fp1 = &lut8bit[buf[i]];
			i++;
			fp2 = &lut8bit[buf[i]];
			i++;
			fp3 = &lut8bit[buf[i]];
			i++;
		}

		data[0][o] = fp0[0];
		data[1][o] = fp1[0];
		data[2][o] = fp2[0];

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

static int vdif_decode_4channel_8bit_decimation1(struct mark5_stream *ms, int nsamp, float **data)
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
			nblank++;
		}
		else
		{
			fp0 = &lut8bit[buf[i]];
			i++;
			fp1 = &lut8bit[buf[i]];
			i++;
			fp2 = &lut8bit[buf[i]];
			i++;
			fp3 = &lut8bit[buf[i]];
			i++;
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
			i+=2;
		}
		else
		{
			fcp0 = complex_lut2bit[buf[i]];
			i++;
			fcp1 = complex_lut2bit[buf[i]];
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
			fcp0 = complex_lut2bit[buf[i]];
			i++;
			fcp1 = complex_lut2bit[buf[i]];
			i++;
			fcp2 = complex_lut2bit[buf[i]];
			i++;
			fcp3 = complex_lut2bit[buf[i]];
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

static int vdif_complex_decode_32channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp[16];
	int o, i, j;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
  		        for (j=0; j<16; j++) fcp[j] = complex_zeros;
		        nblank++;
		        i+=16;
		}
		else
		{
		  for (j=0; j<16; j++) 
		  {
		        fcp[j] = complex_lut2bit[buf[i]]; 
			i++;
		  }
		}

		int k=0;
		for (j=0; j<16; j++) 
		{
		        data[k++][o] = fcp[j][0];
			data[k++][o] = fcp[j][1];
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

static int vdif_complex_decode_64channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp[32];
	int o, i, j;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
  		        for (j=0; j<32; j++) fcp[j] = complex_zeros;
		        nblank++;
		        i+=16;
		}
		else
		{
		  for (j=0; j<32; j++) 
		  {
		        fcp[j] = complex_lut2bit[buf[i]]; 
			i++;
		  }
		}

		int k=0;
		for (j=0; j<32; j++) 
		{
		        data[k++][o] = fcp[j][0];
			data[k++][o] = fcp[j][1];
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

/************************ 2-bit state counters *********************/
/* Note: these only count high vs. low states, not full state counts */

static int vdif_count_1channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o+=4)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			nblank++;
		}
		else
		{
			fp = countlut2bit[buf[i]];
			highstates[0] += fp[0];
			highstates[0] += fp[1];
			highstates[0] += fp[2];
			highstates[0] += fp[3];
		}

		i++;

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

static int vdif_count_2channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o+=2)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			nblank++;
		}
		else
		{
			fp = countlut2bit[buf[i]];
			highstates[0] += fp[0];
			highstates[1] += fp[1];
			highstates[0] += fp[2];
			highstates[1] += fp[3];
		}

		i++;

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

static int vdif_count_4channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			nblank++;
		}
		else
		{
			fp = countlut2bit[buf[i]];
			highstates[0] += fp[0];
			highstates[1] += fp[1];
			highstates[2] += fp[2];
			highstates[3] += fp[3];
		}

		i++;

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

static int vdif_count_8channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp0, *fp1;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			nblank++;
			i+=2;
		}
		else
		{
			fp0 = countlut2bit[buf[i]];
			i++;
			fp1 = countlut2bit[buf[i]];
			i++;

			highstates[0] += fp0[0];
			highstates[1] += fp0[1];
			highstates[2] += fp0[2];
			highstates[3] += fp0[3];
			highstates[4] += fp1[0];
			highstates[5] += fp1[1];
			highstates[6] += fp1[2];
			highstates[7] += fp1[3];
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

static int vdif_count_16channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp0, *fp1, *fp2, *fp3;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			nblank++;
			i+=4;
		}
		else
		{
			fp0 = countlut2bit[buf[i]];
			i++;
			fp1 = countlut2bit[buf[i]];
			i++;
			fp2 = countlut2bit[buf[i]];
			i++;
			fp3 = countlut2bit[buf[i]];
			i++;

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


static int vdif_count_32channel_2bit_decimation1(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp0, *fp1, *fp2, *fp3, *fp4, *fp5, *fp6, *fp7;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			nblank++;
			i+=8;
		}
		else
		{
			fp0 = countlut2bit[buf[i]];
			i++;
			fp1 = countlut2bit[buf[i]];
			i++;
			fp2 = countlut2bit[buf[i]];
			i++;
			fp3 = countlut2bit[buf[i]];
			i++;
			fp4 = countlut2bit[buf[i]];
			i++;
			fp5 = countlut2bit[buf[i]];
			i++;
			fp6 = countlut2bit[buf[i]];
			i++;
			fp7 = countlut2bit[buf[i]];
			i++;

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
			highstates[16] += fp4[0];
			highstates[17] += fp4[1];
			highstates[18] += fp4[2];
			highstates[19] += fp4[3];
			highstates[20] += fp5[0];
			highstates[21] += fp5[1];
			highstates[22] += fp5[2];
			highstates[23] += fp5[3];
			highstates[24] += fp6[0];
			highstates[25] += fp6[1];
			highstates[26] += fp6[2];
			highstates[27] += fp6[3];
			highstates[28] += fp7[0];
			highstates[29] += fp7[1];
			highstates[30] += fp7[2];
			highstates[31] += fp7[3];
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

/******************************************************************/

static int mark5_format_vdif_make_formatname(struct mark5_stream *ms)
{
	if(ms->format == MK5_FORMAT_VDIF)	/* True VDIF header, not legacy */
	{
		if (ms->complex_decode) 
		{
			sprintf(ms->formatname, "VDIFC_%d-%d-%d-%d", ms->databytes, ms->Mbps, ms->nchan, ms->nbit);
		}
		else
		{
			sprintf(ms->formatname, "VDIF_%d-%d-%d-%d", ms->databytes, ms->Mbps, ms->nchan, ms->nbit);
		}
	}
	else if(ms->format == MK5_FORMAT_VDIFL)	/* Must be legacy mode, so add an L to VDIF name */
	{
		sprintf(ms->formatname, "VDIFL_%d-%d-%d-%d", ms->databytes, ms->Mbps, ms->nchan, ms->nbit);
	}
	else
	{
		sprintf(ms->formatname, "VDIF?");
		fprintf(m5stderr, "Warning: mark5_format_vdif_make_formatname: format not set\n");
		
		return -1;
	}

	return 0;
}

static int mark5_format_vdif_init(struct mark5_stream *ms)
{
	struct mark5_format_vdif *f;
	unsigned int word2;
	unsigned char *headerbytes, bitspersample;
	int framensNum, framensDen, dataframelength;
	double dns;

	if(!ms)
	{
		fprintf(m5stderr, "mark5_format_vdif_init: ms = 0\n");

		return -1;
	}

	f = (struct mark5_format_vdif *)(ms->formatdata);

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
	ms->samplegranularity = 8/(ms->nchan*bitspersample*ms->decimation);
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
//		framensNum = 250*f->databytesperpacket*f->completesamplesperword*ms->nchan*bitspersample;
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
			fprintf(m5stderr, "VDIF Warning: cannot calculate gframens %d/%d\n",
			framensNum, framensDen);
			ms->framegranularity = 1;
		}
		ms->samprate = ms->framesamples*(1000000000.0/ms->framens);
        }
        else
        {
                fprintf(m5stderr, "Error: you must specify the data rate (Mbps) for a VDIF mode (was set to %d)!", ms->Mbps);

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
			if(f->frameheadersize == 0)
			{
				f->frameheadersize = 16;
			}
			else if(f->frameheadersize != 16)
			{
				fprintf(m5stderr, "VDIF Warning: Changing frameheadersize from %d to 16\n",
					f->frameheadersize);
				f->frameheadersize = 16;
			}
		}
		else
		{
			if(f->frameheadersize == 0)
			{
				f->frameheadersize = 32;
			}
			else if(f->frameheadersize != 32)
			{
				fprintf(m5stderr, "VDIF Warning: Changing frameheadersize from %d to 32\n",
					f->frameheadersize);
				f->frameheadersize = 32;
			}
		}

		dataframelength = (word2 & 0x00FFFFFF)*8;
		//fprintf(stdout, "Dataframelength as derived from the VDIF header is %d bytes\n", dataframelength);
		if(f->databytesperpacket == 0)
		{
			f->databytesperpacket = dataframelength - f->frameheadersize;
		}
		else if(f->databytesperpacket != dataframelength - f->frameheadersize)
		{
			fprintf(m5stderr, "VDIF Warning: Changing databytesperpacket from %d to %d\n",
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
		ms->format = MK5_FORMAT_VDIF;
	}
	else if(f->frameheadersize == 16)
	{
		ms->format = MK5_FORMAT_VDIFL;
	}
	else
	{
		fprintf(m5stderr, "Error: mark5_format_vdif_init: unsupported frameheadersize=%d\n", f->frameheadersize);
		
		return -1;
	}
	mark5_format_vdif_make_formatname(ms);

	return 0;
}

static int mark5_format_vdif_final(struct mark5_stream *ms)
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

static int mark5_format_vdif_validate(const struct mark5_stream *ms)
{
	const unsigned int *header;

	if(ms->mjd && ms->framenum % ms->framegranularity == 0)
	{
		int mjd_d, mjd_t, sec_d, sec_t;
		double ns_d;
		long long ns_t;
		
		mark5_stream_frame_time_vdif(ms, &mjd_d, &sec_d, &ns_d);

		ns_t = (long long)(ms->framenum/ms->framegranularity)*(long long)(ms->gframens) + (long long)(ms->ns);
		sec_t = ns_t / 1000000000L;
		ns_t -= (long long)sec_t * 1000000000L;
		sec_t += ms->sec;
		mjd_t = sec_t / 86400;
		sec_t -= mjd_t * 86400;
		mjd_t += ms->mjd;

		if(mjd_t != mjd_d || sec_t != sec_d || fabs((double)ns_t - ns_d) > 0.000001)
		{
			fprintf(m5stdout, "VDIF validate[%lld]: %d %d %f : %d %d %lld\n",
				ms->framenum,   mjd_d, sec_d, ns_d,   mjd_t, sec_t, ns_t);
			
			return 0;
		}
	}

	/* Check the invalid bit */
	header = (const unsigned int *)ms->frame;
	if((header[0] >> 31) & 0x01)
	{
#ifdef DEBUG
		fprintf(m5stderr, "mark5_format_vdif_validate: Skipping invalid frame\n");
#endif
		return 0;
	}

	return 1;
}

static int mark5_format_vdif_resync(struct mark5_stream *ms)
{
	/* FIXME: not implemented yet */
	return mark5_format_vdif_validate(ms);
}

void mark5_format_vdif_set_leapsecs(struct mark5_stream *ms, int leapsecs)
{
	struct mark5_format_vdif *f;
	
	f = (struct mark5_format_vdif *)(ms->formatdata);

	f->leapsecs = leapsecs;
}

struct mark5_format_generic *new_mark5_format_vdif(int Mbps, 
	int nchan, int nbit, int decimation, 
	int databytesperpacket, int frameheadersize, int usecomplex)
{
	static int first = 1;
	struct mark5_format_generic *f;
	struct mark5_format_vdif *v;
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
		fprintf(m5stderr, "VDIF decimation must be 1 for now\n");
		
		return 0;
	}

	if(nbit == 1) /* inc by 1000 for each successive value to allow full range of nchan */
	{
		decoderindex += 0;
	}
	else if(nbit == 2)
	{
		decoderindex += 1000;
	}
	else if(nbit == 4)
	{
		decoderindex += 2000;
	}
	else if(nbit == 8)
	{
		decoderindex += 3000;
	}
	else if(nbit == 16)
	{
		decoderindex += 4000;
	}
	else
	{
		fprintf(m5stderr, "VDIF nbit must be 1, 2, 4, 8 or 16 for now\n");
		
		return 0;
	}

	if(nchan < 1 || nchan > 512)
	{
		fprintf(m5stderr, "VDIF nchan must be <= 512 for now\n");

		return 0;
	}

	decoderindex += nchan;

	v = (struct mark5_format_vdif *)calloc(1, sizeof(struct mark5_format_vdif));
	f = (struct mark5_format_generic *)calloc(1, sizeof(struct mark5_format_generic));

	v->frameheadersize = frameheadersize;
	v->databytesperpacket = databytesperpacket;

	f->Mbps = Mbps;
	f->nchan = nchan;
	f->nbit = nbit;
	f->formatdata = v;
	f->formatdatasize = sizeof(struct mark5_format_vdif);
	
	/* set some function pointers */
	f->gettime = mark5_stream_frame_time_vdif;
	f->init_format = mark5_format_vdif_init;
	f->final_format = mark5_format_vdif_final;
	f->validate = mark5_format_vdif_validate;
	f->resync = mark5_format_vdif_resync;
	f->decimation = decimation;
	f->decode = 0;
	f->complex_decode = 0;
	f->count = 0;

	if(!usecomplex) 
	{
	    switch(decoderindex)
	    {
	        case 1:
			f->decode = vdif_decode_1channel_1bit_decimation1;
			break;
		case 2:
			f->decode = vdif_decode_2channel_1bit_decimation1;
			break;
		case 3:
			f->decode = vdif_decode_3channel_1bit_decimation1;
			break;
		case 4:
			f->decode = vdif_decode_4channel_1bit_decimation1;
			break;
		case 5:
			f->decode = vdif_decode_5channel_1bit_decimation1;
			break;
		case 6:
			f->decode = vdif_decode_6channel_1bit_decimation1;
			break;
		case 7:
			f->decode = vdif_decode_7channel_1bit_decimation1;
			break;
		case 8:
			f->decode = vdif_decode_8channel_1bit_decimation1;
			break;
		case 16:
			f->decode = vdif_decode_16channel_1bit_decimation1;
			break;
	        case 32:
			f->decode = vdif_decode_32channel_1bit_decimation1;
			break;

		case 1001:
			f->decode = vdif_decode_1channel_2bit_decimation1;
			f->count = vdif_count_1channel_2bit_decimation1;
			break;
		case 1002:
			f->decode = vdif_decode_2channel_2bit_decimation1;
			f->count = vdif_count_2channel_2bit_decimation1;
			break;
		case 1003:
			f->decode = vdif_decode_3channel_2bit_decimation1;
			break;
		case 1004:
			f->decode = vdif_decode_4channel_2bit_decimation1;
			f->count = vdif_count_4channel_2bit_decimation1;
			break;
		case 1005:
			f->decode = vdif_decode_5channel_2bit_decimation1;
			break;
		case 1006:
			f->decode = vdif_decode_6channel_2bit_decimation1;
			break;
		case 1007:
			f->decode = vdif_decode_7channel_2bit_decimation1;
			break;
		case 1008:
			f->decode = vdif_decode_8channel_2bit_decimation1;
			f->count = vdif_count_8channel_2bit_decimation1;
			break;
		case 1016:
			f->decode = vdif_decode_16channel_2bit_decimation1;
			f->count = vdif_count_16channel_2bit_decimation1;
			break;
		case 1032:
			f->decode = vdif_decode_32channel_2bit_decimation1;
			f->count = vdif_count_32channel_2bit_decimation1;
			break;

		case 2001:
			f->decode = vdif_decode_1channel_4bit_decimation1;
			break;
		case 2002:
			f->decode = vdif_decode_2channel_4bit_decimation1;
			break;
		case 2003:
			f->decode = vdif_decode_3channel_4bit_decimation1;
			break;
		case 2004:
			f->decode = vdif_decode_4channel_4bit_decimation1;
			break;
		case 2005:
			f->decode = vdif_decode_5channel_4bit_decimation1;
			break;
		case 2006:
			f->decode = vdif_decode_6channel_4bit_decimation1;
			break;
		case 2007:
			f->decode = vdif_decode_7channel_4bit_decimation1;
			break;
		case 2008:
			f->decode = vdif_decode_8channel_4bit_decimation1;
			break;

		case 3001:
			f->decode = vdif_decode_1channel_8bit_decimation1;
			break;
		case 3002:
			f->decode = vdif_decode_2channel_8bit_decimation1;
			break;
		case 3003:
			f->decode = vdif_decode_3channel_8bit_decimation1;
			break;
		case 3004:
			f->decode = vdif_decode_4channel_8bit_decimation1;
			break;
	    }

	    if(f->decode == 0)
	    {
		fprintf(m5stderr, "VDIF: Unsupported combination decimation=%d, channels=%d and bits=%d\n", decimation, nchan, nbit);
		free(v);
		free(f);
		
		return 0;
	    }
	}
	else
	{
	    switch(decoderindex)
	    {
	        case 1:
			f->complex_decode = vdif_complex_decode_1channel_1bit_decimation1;
			break;
		case 2:
			f->complex_decode = vdif_complex_decode_2channel_1bit_decimation1;
			break;
		case 4:
			f->complex_decode = vdif_complex_decode_4channel_1bit_decimation1;
			break;
		case 8:
			f->complex_decode = vdif_complex_decode_8channel_1bit_decimation1;
			break;
		case 16:
			f->complex_decode = vdif_complex_decode_16channel_1bit_decimation1;
			break;

		case 1001:
			f->complex_decode = vdif_complex_decode_1channel_2bit_decimation1;
			break;
		case 1002:
			f->complex_decode = vdif_complex_decode_2channel_2bit_decimation1;
			break;
		case 1004:
			f->complex_decode = vdif_complex_decode_4channel_2bit_decimation1;
			break;
		case 1008:
			f->complex_decode = vdif_complex_decode_8channel_2bit_decimation1;
			break;
		case 1016:
			f->complex_decode = vdif_complex_decode_16channel_2bit_decimation1;
			break;
		case 1032:
			f->complex_decode = vdif_complex_decode_32channel_2bit_decimation1;
			break;
		case 1064:
			f->complex_decode = vdif_complex_decode_64channel_2bit_decimation1;
			break;

		case 2001:
			f->complex_decode = vdif_complex_decode_1channel_4bit_decimation1;
			break;
		case 2002:
			f->complex_decode = vdif_complex_decode_2channel_4bit_decimation1;
			break;
		case 2004:
			f->complex_decode = vdif_complex_decode_4channel_4bit_decimation1;
			break;

		case 3001:
			f->complex_decode = vdif_complex_decode_1channel_8bit_decimation1;
			break;
		case 3002:
			f->complex_decode = vdif_complex_decode_2channel_8bit_decimation1;
			break;
		case 3004:
			f->complex_decode = vdif_complex_decode_4channel_8bit_decimation1;
			break;

		case 4001:
			f->complex_decode = vdif_complex_decode_1channel_16bit_decimation1;
			break;
		case 4002:
			f->complex_decode = vdif_complex_decode_2channel_16bit_decimation1;
			break;
		case 4004:
			f->complex_decode = vdif_complex_decode_4channel_16bit_decimation1;
			break;
		case 4008:
			f->complex_decode = vdif_complex_decode_8channel_16bit_decimation1;
			break;
		case 4016:
			f->complex_decode = vdif_complex_decode_16channel_16bit_decimation1;
			break;
	    }

	    if(f->complex_decode == 0)
	    {
		fprintf(m5stderr, "VDIF: Unsupported combination decimation=%d, channels=%d and bits=%d\n", decimation, nchan, nbit);
		free(v);
		free(f);

		return 0;
	    }

	}

	return f;
}

/* here framesize includes the 32 byte header 
 *
 * return value: 1 if true, 0 if false
 *
 * Note: this only works for nbit = 2^n
 */
static int is_legal_vdif_framesize(int framesize)
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

static int is_legal_vdifl_framesize(int framesize)
{
	framesize -= 16;

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
int find_vdif_frame(const unsigned char *data, size_t length, size_t *offset, int *framesize)
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

		if(!is_legal_vdif_framesize(fs))
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
			const unsigned int *frame;

			frame = ((unsigned int *)data) + *offset/4;
			secA      = frame[0] & 0x3FFFFFFF;
			refEpochA = (frame[1] >> 24) & 0x3F;
			fsA       = (frame[2] & 0x00FFFFFF) << 3;
			frame += fs/4;
			secB      = frame[0] & 0x3FFFFFFF;
			refEpochB = (frame[1] >> 24) & 0x3F;
			fsB       = (frame[2] & 0x00FFFFFF) << 3;

			/* does it look reasonable? */
			if(fsA == fs && fsB == fs && refEpochA == refEpochB && (secA == secB || secA+1 == secB))
			{
				*framesize = fs;

				return 0;
			}

		}
	}

	return -1;
}

/* same as above, but for legacy frames */
int find_vdifl_frame(const unsigned char *data, size_t length, size_t *offset, int *framesize)
{
	int fs, fs0, fs1;

	if(framesize && *framesize)
	{
		fs0 = fs1 = *framesize;
	}
	else
	{
		fs0 = 24;
		fs1 = 8216;
	}

	for(fs = fs0; fs <= fs1; ++fs)
	{
		size_t maxOffset;

		if(!is_legal_vdifl_framesize(fs))
		{
			continue;
		}

		maxOffset = 5*fs;	/* check over a maximum of 5 frame lengths */
		if(maxOffset > length - fs - 16)
		{
			maxOffset = length - fs - 16;
		}

		for(*offset = 0; *offset < maxOffset; *offset += 8)
		{
			unsigned int secA, secB;
			unsigned int refEpochA, refEpochB;
			unsigned int fsA, fsB;
			const unsigned int *frame;

			frame = ((unsigned int *)data) + *offset/4;
			secA      = frame[0] & 0x3FFFFFFF;
			refEpochA = (frame[1] >> 24) & 0x3F;
			fsA       = (frame[2] & 0x00FFFFFF) << 3;
			frame += fs/4;
			secB      = frame[0] & 0x3FFFFFFF;
			refEpochB = (frame[1] >> 24) & 0x3F;
			fsB       = (frame[2] & 0x00FFFFFF) << 3;

			/* does it look reasonable? */
			if(fsA == fs && fsB == fs && refEpochA == refEpochB && (secA == secB || secA+1 == secB))
			{
				*framesize = fs;

				return 0;
			}

		}
	}

	return -1;
}

int get_vdif_chans_per_thread(const unsigned char *data)
{
	return 1 << (data[11] & 0x1F);
}

int get_vdif_quantization_bits(const unsigned char *data)
{
	return ((data[15] >> 2) & 0x1F) + 1;
}

int get_vdif_complex(const unsigned char *data)
{
	return data[15] >> 7;
}

int get_vdif_threads(const unsigned char *data, size_t length, int dataframesize)
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
