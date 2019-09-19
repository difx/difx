/***************************************************************************
 *   Copyright (C) 2009-2017 by Walter Brisken, Adam Deller, Chris Phillips*
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
// $Id: format_vdif.c 7371 2016-07-14 01:27:03Z ChrisPhillips $
// $HeadURL: $
// $LastChangedRevision: 7371 $
// $Author: ChrisPhillips $
// $LastChangedDate: 2016-07-14 11:27:03 +1000 (Thu, 14 Jul 2016) $
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
#ifdef WORDS_BIGENDIAN
#include <endian.h>
#endif

#include "codifio.h"

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

/* internal data specific to CODIF */
struct mark5_format_codif
{
	int databytesperpacket;		/* = packetsize - frameheadersize */
	int frameheadersize;		/* 64 bytes */
	int leapsecs;			/* relative to reference epoch of CODIF data */
	int completesamplesperword;	/* number of samples for each channel in one 32-bit word */
};

#warning "********FIXME***** CODIF supports offset binary and 2s complement. Update luts"

int set_decoder(int nbit, int nchan, int usecomplex, decodeFunc *decode, complex_decodeFunc *complex_decode, countFunc *count);


static void initluts()
{
	/* Warning: these are different than for VLBA/Mark4/Mark5B! */
	const float lut2level[2] = {1.0, -1.0};
	const float lut4level[4] = {1.0, HiMag, -HiMag, -1.0};
	const float lut16level[16] = {0,1/FourBit1sigma,2/FourBit1sigma,3/FourBit1sigma,4/FourBit1sigma,5/FourBit1sigma,6/FourBit1sigma,7/FourBit1sigma,
				      -8/FourBit1sigma,-7/FourBit1sigma,-6/FourBit1sigma,-5/FourBit1sigma,-4/FourBit1sigma,-3/FourBit1sigma,-2/FourBit1sigma,-1/FourBit1sigma};
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
		lut8bit[b] = (float)((int8_t)((uint8_t)b))/71.0;

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
static int mark5_stream_frame_time_codif(const struct mark5_stream *ms, int *mjd, int *sec, double *ns)
{
	struct mark5_format_codif *v;
	int seconds, days;
	int refepoch;
	unsigned long long fullframens;
	codif_header *header;

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
	v = (struct mark5_format_codif *)(ms->formatdata);

#ifdef WORDS_BIGENDIAN
#error "bigendian not supported"
#endif
	header = (codif_header*)(ms->frame);


	seconds = getCODIFFrameEpochSecOffset(header);
	refepoch = getCODIFEpoch(header);

#warning "***** Where does leapseconds come from"
	seconds += v->leapsecs;
	days = seconds/86400;
	seconds -= days*86400;
	days += mjdepochs[refepoch];

	fullframens = getCODIFFrameNumber(header)*ms->framens;
	
	if(mjd)
	{
		*mjd = days;
	}
	if(sec)
	{
		*sec = seconds+fullframens/1000000000;
	}
	if(ns)
	{
	        *ns = fullframens % 1000000000;
	}

	return 0;
}

/************************* decode routines **************************/

static int codif_decode_1channel_1bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_2channel_1bit(struct mark5_stream *ms, int nsamp, float **data)
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
static int codif_decode_3channel_1bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_4channel_1bit(struct mark5_stream *ms, int nsamp, float **data)
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
static int codif_decode_5channel_1bit(struct mark5_stream *ms, int nsamp, float **data)
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
static int codif_decode_6channel_1bit(struct mark5_stream *ms, int nsamp, float **data)
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
static int codif_decode_7channel_1bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_8channel_1bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_16channel_1bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_32channel_1bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_1channel_2bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_2channel_2bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_3channel_2bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_4channel_2bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_5channel_2bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_6channel_2bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_7channel_2bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_8channel_2bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_16channel_2bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_32channel_2bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_64channel_2bit(struct mark5_stream *ms, int nsamp, float **data)
{
	const unsigned char *buf;
	const float *fp0, *fp1, *fp2, *fp3, *fp4, *fp5, *fp6, *fp7;
	const float *fp8, *fp9, *fpA, *fpB, *fpC, *fpD, *fpE, *fpF;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;
	
	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			fp0 = fp1 = fp2 = fp3 = fp4 = fp5 = fp6 = fp7 = zeros;
			fp8 = fp9 = fpA = fpB = fpC = fpD = fpE = fpF = zeros;
			nblank++;
			i+=16;
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
			fp8 = lut2bit[buf[i]];
			i++;
			fp9 = lut2bit[buf[i]];
			i++;
			fpA = lut2bit[buf[i]];
			i++;
			fpB = lut2bit[buf[i]];
			i++;
			fpC = lut2bit[buf[i]];
			i++;
			fpD = lut2bit[buf[i]];
			i++;
			fpE = lut2bit[buf[i]];
			i++;
			fpF = lut2bit[buf[i]];
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

		data[32][o] = fp8[0];
		data[33][o] = fp8[1];
		data[34][o] = fp8[2];
		data[35][o] = fp8[3];
		data[36][o] = fp9[0];
		data[37][o] = fp9[1];
		data[38][o] = fp9[2];
		data[39][o] = fp9[3];
		data[40][o] = fpA[0];
		data[41][o] = fpA[1];
		data[42][o] = fpA[2];
		data[43][o] = fpA[3];
		data[44][o] = fpB[0];
		data[45][o] = fpB[1];
		data[46][o] = fpB[2];
		data[47][o] = fpB[3];
		data[48][o] = fpC[0];
		data[49][o] = fpC[1];
		data[50][o] = fpC[2];
		data[51][o] = fpC[3];
		data[52][o] = fpD[0];
		data[53][o] = fpD[1];
		data[54][o] = fpD[2];
		data[55][o] = fpD[3];
		data[56][o] = fpE[0];
		data[57][o] = fpE[1];
		data[58][o] = fpE[2];
		data[59][o] = fpE[3];
		data[60][o] = fpF[0];
		data[61][o] = fpF[1];
		data[62][o] = fpF[2];
		data[63][o] = fpF[3];

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

static int codif_decode_1channel_4bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_2channel_4bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_3channel_4bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_4channel_4bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_5channel_4bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_6channel_4bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_7channel_4bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_8channel_4bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_1channel_8bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_2channel_8bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_3channel_8bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_4channel_8bit(struct mark5_stream *ms, int nsamp, float **data)
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

static int codif_decode_1channel_32bit(struct mark5_stream *ms, int nsamp, float **data)
{
	const float *buf;
	int o, i;
	int nblank = 0;

	buf = (const float*)ms->payload;
	i = ms->readposition/4;

	for(o = 0; o < nsamp; o++)
	{
		if(i*4 >= ms->blankzoneendvalid[0])
		{
			data[0][o] = zeros[0];
			nblank++;
		}
		else
		{
#ifdef WORDS_BIGENDIAN
			uint32_t v = *((const uint32_t*)buf + i);
			v = le32toh(v);
			data[0][o] = *((float*)&v);
#else
			data[0][o] = buf[i];
#endif
		}

		i++;


		if(i*4 >= ms->databytes)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = (const float*)ms->payload;
			i = 0;
		}
	}

	ms->readposition = i*4;

	return nsamp - nblank;
}

// Complex

static int codif_complex_decode_1channel_1bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_2channel_1bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_4channel_1bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_8channel_1bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_16channel_1bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_1channel_2bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_2channel_2bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_4channel_2bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_8channel_2bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_16channel_2bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_32channel_2bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_64channel_2bit(struct mark5_stream *ms, int nsamp, float complex **data)
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
		        i+=32;
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


static int codif_complex_decode_1channel_4bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_2channel_4bit(struct mark5_stream *ms, int nsamp, float complex **data)
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
		data[1][o] = fcp1[0];

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

static int codif_complex_decode_4channel_4bit(struct mark5_stream *ms, int nsamp, float complex **data)
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
		data[1][o] = fcp1[0];
		data[2][o] = fcp2[0];
		data[3][o] = fcp3[0];

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

static int codif_complex_decode_8channel_4bit(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	const float complex *fcp0, *fcp1, *fcp2, *fcp3;
	int o, i, j;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
		    for (j=0;j<8;j++) 
		        data[j][o] = complex_zeros[0];
		    nblank++;
		    i += 8;
		}
		else
		{
		  for(j=0;j<8;j++) {
		        data[j][o] = complex_lut4bit[buf[i]];
			i++;
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

	return nsamp - nblank;
}

static int codif_complex_decode_1channel_8bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_2channel_8bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_4channel_8bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_8channel_8bit(struct mark5_stream *ms, int nsamp, float complex **data)
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
			data[4][o] = complex_zeros[0];
			data[5][o] = complex_zeros[0];
			data[6][o] = complex_zeros[0];
			data[7][o] = complex_zeros[0];
			nblank++;
			i += 16;
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
			data[4][o] = lut8bit[buf[i]] + lut8bit[buf[i+1]]*I; 
			i+=2;
			data[5][o] = lut8bit[buf[i]] + lut8bit[buf[i+1]]*I; 
			i+=2;
			data[6][o] = lut8bit[buf[i]] + lut8bit[buf[i+1]]*I; 
			i+=2;
			data[7][o] = lut8bit[buf[i]] + lut8bit[buf[i+1]]*I; 
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

static int codif_complex_decode_16channel_8bit(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const unsigned char *buf;
	int o, i, j;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
		        for (j=0;j<16;j++) 
			  data[j][o] = complex_zeros[0];
			nblank++;
			i += 32;
		}
		else
		{
		  for (j=0; j<16;j++) {
		        data[j][o] = lut8bit[buf[i]] + lut8bit[buf[i+1]]*I;
			i +=2;
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

	return nsamp - nblank;
}

static int codif_complex_decode_1channel_16bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_2channel_16bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_4channel_16bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_8channel_16bit(struct mark5_stream *ms, int nsamp, float complex **data)
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
		    data[j][o] = ((int16_t)(buf[i]) + (int16_t)(buf[i+1])*I)/8.0;  // Assume RMS==8
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

static int codif_complex_decode_14channel_16bit(struct mark5_stream *ms, int nsamp, float complex **data)
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
		  for (j=0; j<14; j++) {
			data[j][o] = complex_zeros[0];
			nblank++;
		  }
		}
		else
		{
		  for (j=0; j<14; j++) {
		    //		        data[j][o] = ((int16_t)(buf[i]^0x8000) + (int16_t)(buf[i+1]^0x8000)*I)/8.0;  // Assume RMS==8
		        data[j][o] = ((int16_t)buf[i] + (int16_t)buf[i+1]*I)/8.0;  // Assume RMS==8
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

static int codif_complex_decode_16channel_16bit(struct mark5_stream *ms, int nsamp, float complex **data)
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

static int codif_complex_decode_1channel_32bit(struct mark5_stream *ms, int nsamp, float complex **data)
{
	const float *buf;
	int o, i;
	int nblank = 0;

	buf = (const float *)ms->payload;
	i = ms->readposition/4;

	for(o = 0; o < nsamp; o++)
	{
		if(i*4 >= ms->blankzoneendvalid[0])
		{
			data[0][o] = complex_zeros[0];
			nblank++;
		}
		else
		{
#ifdef WORDS_BIGENDIAN
			uint32_t re = *((const uint32_t*)buf + i);
			uint32_t im = *((const uint32_t*)buf + i + 1);
			re = le32toh(re);
			im = le32toh(im);
			data[0][o] = *((float*)&re) + *((float*)&im)*I;
#else
			data[0][o] = buf[i] + buf[i+1]*I;
#endif
		}

		i += 2;


		if(i*4 >= ms->databytes)
		{
			if(mark5_stream_next_frame(ms) < 0)
			{
				return -1;
			}
			buf = (const float *)ms->payload;
			i = 0;
		}
	}

	ms->readposition = i*4;

	return nsamp - nblank;
}

/* Other decoders go here */

/************************ 2-bit state counters *********************/
/* Note: these only count high vs. low states, not full state counts */

static int codif_count_1channel_2bit(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
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

static int codif_count_2channel_2bit(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
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

static int codif_count_4channel_2bit(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
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

static int codif_count_8channel_2bit(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
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

static int codif_count_16channel_2bit(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
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


static int codif_count_32channel_2bit(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
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

static int codif_count_64channel_2bit(struct mark5_stream *ms, int nsamp, unsigned int *highstates)
{
	const unsigned char *buf;
	const unsigned char *fp0, *fp1, *fp2, *fp3, *fp4, *fp5, *fp6, *fp7;
	const unsigned char *fp8, *fp9, *fpA, *fpB, *fpC, *fpD, *fpE, *fpF;
	int o, i;
	int nblank = 0;

	buf = ms->payload;
	i = ms->readposition;

	for(o = 0; o < nsamp; o++)
	{
		if(i >= ms->blankzoneendvalid[0])
		{
			nblank++;
			i += 16;
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
			fp8 = countlut2bit[buf[i]];
			i++;
			fp9 = countlut2bit[buf[i]];
			i++;
			fpA = countlut2bit[buf[i]];
			i++;
			fpB = countlut2bit[buf[i]];
			i++;
			fpC = countlut2bit[buf[i]];
			i++;
			fpD = countlut2bit[buf[i]];
			i++;
			fpE = countlut2bit[buf[i]];
			i++;
			fpF = countlut2bit[buf[i]];
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
			highstates[32] += fp8[0];
			highstates[33] += fp8[1];
			highstates[34] += fp8[2];
			highstates[35] += fp8[3];
			highstates[36] += fp9[0];
			highstates[37] += fp9[1];
			highstates[38] += fp9[2];
			highstates[39] += fp9[3];
			highstates[40] += fpA[0];
			highstates[41] += fpA[1];
			highstates[42] += fpA[2];
			highstates[43] += fpA[3];
			highstates[44] += fpB[0];
			highstates[45] += fpB[1];
			highstates[46] += fpB[2];
			highstates[47] += fpB[3];
			highstates[48] += fpC[0];
			highstates[49] += fpC[1];
			highstates[50] += fpC[2];
			highstates[51] += fpC[3];
			highstates[52] += fpD[0];
			highstates[53] += fpD[1];
			highstates[54] += fpD[2];
			highstates[55] += fpD[3];
			highstates[56] += fpE[0];
			highstates[57] += fpE[1];
			highstates[58] += fpE[2];
			highstates[59] += fpE[3];
			highstates[60] += fpF[0];
			highstates[61] += fpF[1];
			highstates[62] += fpF[2];
			highstates[63] += fpF[3];
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

static int mark5_format_codif_make_formatname(struct mark5_stream *ms)
{
	if(ms->format == MK5_FORMAT_CODIF)
	{
		if (ms->iscomplex) 
		{
			sprintf(ms->formatname, "CODIFC_%d-%dm%d-%d-%d", ms->databytes, ms->framesperperiod, ms->alignmentseconds, ms->nchan, ms->nbit);
		}
		else
		{
			sprintf(ms->formatname, "CODIF_%d-%dm%d-%d-%d", ms->databytes, ms->framesperperiod, ms->alignmentseconds, ms->nchan, ms->nbit);
		}
	}
	else
	{
		sprintf(ms->formatname, "CODIF?");
		fprintf(m5stderr, "Warning: mark5_format_codif_make_formatname: format not set\n");
		
		return -1;
	}

	return 0;
}

static int mark5_format_codif_init(struct mark5_stream *ms)
{

    
	struct mark5_format_codif *f;
	unsigned int word2;
	uint8_t bitspersample=0;
	int framensNum, framensDen, dataarraylength, status;
	double dns;
	codif_header *header;

#ifdef WORDS_BIGENDIAN
		#error "bigendian not supported"
#endif

	if(!ms)
	{
		fprintf(m5stderr, "mark5_format_codif_init: ms = 0\n");

		return -1;
	}

	f = (struct mark5_format_codif *)(ms->formatdata);

	ms->framesamples = 0;
	ms->framens = 0; // It gets set later
        ms->framegranularity = 1; // Should get set later

	ms->payloadoffset = f->frameheadersize;
	ms->databytes = f->databytesperpacket;
	ms->framebytes = f->databytesperpacket + f->frameheadersize;
	ms->blanker = blanker_codif;

	/* We have some data to look at to further refine the format... */
	if(ms->datawindow)
	{
		ms->frame = ms->datawindow + ms->frameoffset;
	
		header = (codif_header*)ms->frame;


		
		if(f->frameheadersize != 0 && f->frameheadersize != CODIF_HEADER_BYTES)
		{
			fprintf(m5stderr, "CODIF Warning: Changing frameheadersize from %d to 64\n",
				f->frameheadersize);
		}
		f->frameheadersize = CODIF_HEADER_BYTES;

		dataarraylength = getCODIFFrameBytes(header);

		if(f->databytesperpacket != 0 && f->databytesperpacket != dataarraylength)
		{
			fprintf(m5stderr, "CODIF Warning: Changing databytesperpacket from %d to %d\n",
				f->databytesperpacket, dataarraylength);
		}
		f->databytesperpacket = dataarraylength;

		if (ms->nchan != 0 && ms->nchan != getCODIFNumChannels(header))
		{
			fprintf(m5stderr, "CODIF Warning: Changing nchan from %d to %d\n",
				ms->nchan, getCODIFNumChannels(header));
		}
		ms->nchan =  getCODIFNumChannels(header);

		if (ms->nbit != 0 && ms->nbit != getCODIFBitsPerSample(header))
		{
			fprintf(m5stderr, "CODIF Warning: Changing nbit from %d to %d\n",
				ms->nbit, header->nbits);
		}
		ms->nbit = getCODIFBitsPerSample(header);
		bitspersample = ms->nbit;
		if (header->iscomplex) bitspersample *=2;

		ms->payload = ms->frame + ms->payloadoffset;
		ms->framegranularity = get_codif_framegranularity(header);
		ms->framesperperiod = get_codif_frames_per_period(header);
		ms->alignmentseconds = get_codif_alignment_seconds(header);
		ms->Mbps = ((double)ms->databytes * ms->framesperperiod * 8) / ms->alignmentseconds/1e6;
		
		ms->framens = get_codif_framens(header);
		
		/* get time again so ms->framens is used */
		ms->gettime(ms, &ms->mjd, &ms->sec, &dns);
		ms->ns = (int)(dns + 0.5);
		if (header->iscomplex)
		  ms->iscomplex = 1;
		else
		  ms->iscomplex = 0;

		status = set_decoder(ms->nbit, ms->nchan, header->iscomplex, &ms->decode, &ms->complex_decode, &ms->count);
	
		if (!status) {
		  fprintf(m5stderr, "CODIF: Unsupported combination channels=%d and bits=%d\n", ms->nchan, ms->nbit);
		  return 0;
		}
		
	} 
	else 
	{ 
	  //printf("DEBUG: No data to check\n");
	}
	
#warning "Need to set complex decode"

	if (bitspersample==0) {
          bitspersample = ms->nbit;
	  if (ms->iscomplex)
	  {
            bitspersample *= 2;
          }
        }
	ms->framesamples = ms->databytes*8/(ms->nchan*bitspersample*ms->decimation);

	/* FIXME: if nbit is not a power of 2, this formula breaks down! */
	ms->samplegranularity = 8/(ms->nchan*bitspersample*ms->decimation);
	if(ms->samplegranularity <= 0)
	{
		ms->samplegranularity = 1;
	}
	
	// Don't think these are needed..... CJP
        f->completesamplesperword = 32/(bitspersample*ms->nchan);

        if(ms->framesperperiod > 0 && ms->alignmentseconds > 0)
        {

		// This block sets framens, framegranularity and samprate - want to set framens above.
		if (ms->framens==0) { // Don't reset if calculated above
		  ms->framens = 1.0e9*(double)ms->alignmentseconds/(double)ms->framesperperiod;
		}

		for(ms->framegranularity = 1; ms->framegranularity < 128; ++ms->framegranularity)
		{
		        if((((uint64_t)1000000000)*ms->framegranularity*ms->alignmentseconds) % ((uint64_t)ms->framesperperiod) == 0)
		        {
		                break;
		        }
		}
		
		if(ms->framegranularity >= 128)
		{
		        fprintf(m5stderr, "CODIF Warning: cannot calculate gframens %d/%d\n",
		        ms->framesperperiod, ms->alignmentseconds);
		        ms->framegranularity = 1;
		}
		
		ms->samprate = ((int64_t)ms->framesamples)*(1000000000.0/ms->framens);
        }
        else
        {
                fprintf(m5stderr, "Error: you must specify the framesperperiod and alignmentseconds for a ");
		fprintf(m5stderr, "CODIF mode (was set to %d, %d)!\n", ms->framesperperiod, ms->alignmentseconds);

		return -1;
        }

#warning "Need to set decoder"

	ms->gframens = (int)(ms->framegranularity*ms->framens + 0.5);

	ms->format = MK5_FORMAT_CODIF;
	mark5_format_codif_make_formatname(ms);

	return 0;
}

static int mark5_format_codif_final(struct mark5_stream *ms)
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

static int mark5_format_codif_validate(const struct mark5_stream *ms)
{
	codif_header *header;
	
	/* Check for overly unusual header  */
	header = (codif_header *)ms->frame;

	if(getCODIFSync(header) != 0xADEADBEE)
	{
	        fprintf(m5stderr, "mark5_format_codif_validate: Skipping frame with wrong sync (0x%08X)\n", getCODIFSync(header));
	        return 0;
	}

	if(getCODIFFrameBytes(header) == 0)
	{
		fprintf(m5stderr, "mark5_format_codif_validate: Skipping frame with Data Frame Length of zero\n");
		return 0;
	}

	if(ms->mjd && ms->framenum % ms->framegranularity == 0)
	{
		int mjd_d, mjd_t, sec_d, sec_t;
		double ns_d;
		long long ns_t;
		
		mark5_stream_frame_time_codif(ms, &mjd_d, &sec_d, &ns_d);

		ns_t = (long long)(ms->framenum/ms->framegranularity)*(long long)(ms->gframens) + (long long)(ms->ns);
		sec_t = ns_t / 1000000000L;
		ns_t -= (long long)sec_t * 1000000000L;
		sec_t += ms->sec;
		mjd_t = sec_t / 86400;
		sec_t -= mjd_t * 86400;
		mjd_t += ms->mjd;

		if(mjd_t != mjd_d || sec_t != sec_d || fabs((double)ns_t - ns_d) > 0.000001)
		{
			fprintf(m5stdout, "CODIF validate[%lld]: %d %d %f : %d %d %lld\n",
				ms->framenum,   mjd_d, sec_d, ns_d,   mjd_t, sec_t, ns_t);
			return 0;
		}
	}

	/* Check the invalid bit */
	if(getCODIFFrameInvalid(header))
	{
		fprintf(m5stderr, "mark5_format_codif_validate: Skipping invalid frame\n");
		return 0;
	}

	return 1;
}

static int mark5_format_codif_resync(struct mark5_stream *ms)
{
	/* FIXME: not implemented yet */
	return mark5_format_codif_validate(ms);
}

void mark5_format_codif_set_leapsecs(struct mark5_stream *ms, int leapsecs)
{
	struct mark5_format_codif *f;
	
	f = (struct mark5_format_codif *)(ms->formatdata);

	f->leapsecs = leapsecs;
}

int set_decoder(int nbit, int nchan, int usecomplex, decodeFunc *decode, complex_decodeFunc *complex_decode, countFunc *count) {
    int decoderindex = 0;

    if(nbit == 1) /* inc by 1000 for each successive value to allow full range of nchan */
      {
	decoderindex += 0;
      }
    else if (nbit == 2)
      {
	decoderindex += 1000;
      }
    else if (nbit == 4)
      {
	decoderindex += 2000;
      }
    else if (nbit == 8)
      {
	decoderindex += 3000;
      }
    else if(nbit == 16)
      {
	decoderindex += 4000;
      }
    else if(nbit == 32)
      {
	decoderindex += 5000;
      }
    else
      {
	fprintf(m5stderr, "CODIF nbit must be 1, 2, 4, 8, 16 or 32 for now\n");
	return 0;
      }

    if(nchan < 1 || nchan > 512)
      {
	fprintf(m5stderr, "CODIF nchan must be <= 512 for now\n");
	return 0;
      }

    decoderindex += nchan;

    *decode = 0;
    *complex_decode = 0;
    *count = 0;

    if(!usecomplex) 
      {
	switch(decoderindex)
	  {
	  case 1:
	    *decode = codif_decode_1channel_1bit;
	    break;
	  case 2:
	    *decode = codif_decode_2channel_1bit;
	    break;
	  case 3:
	    *decode = codif_decode_3channel_1bit;
	    break;
	  case 4:
	    *decode = codif_decode_4channel_1bit;
	    break;
	  case 5:
	    *decode = codif_decode_5channel_1bit;
	    break;
	  case 6:
	    *decode = codif_decode_6channel_1bit;
	    break;
	  case 7:
	    *decode = codif_decode_7channel_1bit;
	    break;
	  case 8:
	    *decode = codif_decode_8channel_1bit;
	    break;
	  case 16:
	    *decode = codif_decode_16channel_1bit;
	    break;
	  case 32:
	    *decode = codif_decode_32channel_1bit;
	    break;
	    
	  case 1001:
	    *decode = codif_decode_1channel_2bit;
	    *count = codif_count_1channel_2bit;
	    break;
	  case 1002:
	    *decode = codif_decode_2channel_2bit;
	    *count = codif_count_2channel_2bit;
	    break;
	  case 1003:
	    *decode = codif_decode_3channel_2bit;
	    break;
	  case 1004:
	    *decode = codif_decode_4channel_2bit;
	    *count = codif_count_4channel_2bit;
	    break;
	  case 1005:
	    *decode = codif_decode_5channel_2bit;
	    break;
	  case 1006:
	    *decode = codif_decode_6channel_2bit;
	    break;
	  case 1007:
	    *decode = codif_decode_7channel_2bit;
	    break;
	  case 1008:
	    *decode = codif_decode_8channel_2bit;
	    *count = codif_count_8channel_2bit;
	    break;
	  case 1016:
	    *decode = codif_decode_16channel_2bit;
	    *count = codif_count_16channel_2bit;
	    break;
	  case 1032:
	    *decode = codif_decode_32channel_2bit;
	    *count = codif_count_32channel_2bit;
	    break;
	  case 1064:
	    *decode = codif_decode_64channel_2bit;
	    *count = codif_count_64channel_2bit;
	    break;
	    
	  case 2001:
	    *decode = codif_decode_1channel_4bit;
	    break;
	  case 2002:
	    *decode = codif_decode_2channel_4bit;
	    break;
	  case 2003:
	    *decode = codif_decode_3channel_4bit;
	    break;
	  case 2004:
	    *decode = codif_decode_4channel_4bit;
	    break;
	  case 2005:
	    *decode = codif_decode_5channel_4bit;
	    break;
	  case 2006:
	    *decode = codif_decode_6channel_4bit;
	    break;
	  case 2007:
	    *decode = codif_decode_7channel_4bit;
	    break;
	  case 2008:
	    *decode = codif_decode_8channel_4bit;
	    break;
	    
	  case 3001:
	    *decode = codif_decode_1channel_8bit;
	    break;
	  case 3002:
	    *decode = codif_decode_2channel_8bit;
	    break;
	  case 3003:
	    *decode = codif_decode_3channel_8bit;
	    break;
	  case 3004:
	    *decode = codif_decode_4channel_8bit;
	    break;
	    
	  case 5001:
	    *decode = codif_decode_1channel_32bit;
	    break;
	  }
	
	if(*decode == 0)
	  {
	    fprintf(m5stderr, "CODIF: Unsupported combination channels=%d and bits=%d\n", nchan, nbit);
	    return 0;
	  }
      }      
    else
      {
	switch(decoderindex)
	  {
	  case 1:
	    *complex_decode = codif_complex_decode_1channel_1bit;
	    break;
	  case 2:
	    *complex_decode = codif_complex_decode_2channel_1bit;
	    break;
	  case 4:
	    *complex_decode = codif_complex_decode_4channel_1bit;
	    break;
	  case 8:
	    *complex_decode = codif_complex_decode_8channel_1bit;
	    break;
	  case 16:
	    *complex_decode = codif_complex_decode_16channel_1bit;
	    break;
	    
	  case 1001:
	    *complex_decode = codif_complex_decode_1channel_2bit;
	    break;
	  case 1002:
	    *complex_decode = codif_complex_decode_2channel_2bit;
	    break;
	  case 1004:
	    *complex_decode = codif_complex_decode_4channel_2bit;
	    break;
	  case 1008:
	    *complex_decode = codif_complex_decode_8channel_2bit;
	    break;
	  case 1016:
	    *complex_decode = codif_complex_decode_16channel_2bit;
	    break;
	  case 1032:
	    *complex_decode = codif_complex_decode_32channel_2bit;
	    break;
	  case 1064:
	    *complex_decode = codif_complex_decode_64channel_2bit;
	    break;
	    
	  case 2001:
	    *complex_decode = codif_complex_decode_1channel_4bit;
	    break;
	  case 2002:
	    *complex_decode = codif_complex_decode_2channel_4bit;
	    break;
	  case 2004:
	    *complex_decode = codif_complex_decode_4channel_4bit;
	    break;
	  case 2008:
	    *complex_decode = codif_complex_decode_8channel_4bit;
	    break;
	    
	  case 3001:
	    *complex_decode = codif_complex_decode_1channel_8bit;
	    break;
	  case 3002:
	    *complex_decode = codif_complex_decode_2channel_8bit;
	    break;
	  case 3004:
	    *complex_decode = codif_complex_decode_4channel_8bit;
	    break;
	  case 3008:
	    *complex_decode = codif_complex_decode_8channel_8bit;
	    break;
	  case 3016:
	    *complex_decode = codif_complex_decode_16channel_8bit;
	    break;
	    
	  case 4001:
	    *complex_decode = codif_complex_decode_1channel_16bit;
	    break;
	  case 4002:
	    *complex_decode = codif_complex_decode_2channel_16bit;
	    break;
	  case 4004:
	    *complex_decode = codif_complex_decode_4channel_16bit;
	    break;
	  case 4008:
	    *complex_decode = codif_complex_decode_8channel_16bit;
	    break;
	  case 4014:
	    *complex_decode = codif_complex_decode_14channel_16bit;
	    break;
	  case 4016:
	    *complex_decode = codif_complex_decode_16channel_16bit;
	    break;
	    
	  case 5001:
	    *complex_decode = codif_complex_decode_1channel_32bit;
	    break;
	  }
	
	if(*complex_decode == 0)
	  {
	    fprintf(m5stderr, "CODIF: Unsupported combination, complex channels=%d and bits=%d\n", nchan, nbit);
	    return 0;
	  }
	
      }
    return(1);
}

struct mark5_format_generic *new_mark5_format_codif(int framesperperiod, 
	int alignmentseconds, int nchan, int nbit, int decimation, 
	int databytesperpacket, int frameheadersize, int usecomplex)
{
    int status;
    static int first = 1;
    struct mark5_format_generic *f;
    struct mark5_format_codif *v;
    int decoderindex = 0;

    if(first)
	{
		initluts();
		first = 0;
	}


    

	v = (struct mark5_format_codif *)calloc(1, sizeof(struct mark5_format_codif));
	f = (struct mark5_format_generic *)calloc(1, sizeof(struct mark5_format_generic));

	v->frameheadersize = frameheadersize;
	v->databytesperpacket = databytesperpacket;

	f->framesperperiod = framesperperiod;
	f->alignmentseconds = alignmentseconds;
	f->Mbps = ((double)framesperperiod*databytesperpacket*8)/alignmentseconds/1e6;
	f->nchan = nchan;
	f->nbit = nbit;
	f->formatdata = v;
	f->formatdatasize = sizeof(struct mark5_format_codif);
	
	/* set some function pointers */
	f->gettime = mark5_stream_frame_time_codif;
	f->init_format = mark5_format_codif_init;
	f->final_format = mark5_format_codif_final;
	f->validate = mark5_format_codif_validate;
	f->resync = mark5_format_codif_resync;
	f->decimation = decimation;
	f->decode = 0;
	f->complex_decode = 0;
	f->iscomplex = usecomplex;
	f->count = 0;

	status = set_decoder(nbit, nchan, usecomplex, &f->decode, &f->complex_decode, &f->count);
	
	if (!status) {
	  fprintf(m5stderr, "CODIF: Unsupported combination decimation=%d, channels=%d and bits=%d\n", decimation, nchan, nbit);
	  free(v);
	  free(f);

	  return 0;
	}

	return f;
}

/* here framesize includes the 64 byte header 
 *
 * return value: 1 if true, 0 if false
 *
 * Note: this only works for nbit = 2^n
 */
static int is_legal_codif_framesize(int framesize)
{
	framesize -= 64;

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
 * return value is -1 on error (no CODIF found) or 0 if found.
 *
 * here framesize includes the 64 byte header
 */
int find_codif_frame(const unsigned char *data, int length, size_t *offset, int *framesize, int *headersize)
{
    int refEpochA, refEpochB, bitsA, bitsB, versionA, versionB, chanA, chanB;
    uint32_t fsA, fsB, secA, secB;
    codif_header *header;
    
    for (*offset=0; *offset<length; (*offset)++) {
      header = (codif_header*)(data + *offset);
      
      if (header->sync != 0xABADDEED) continue; // Sync

      fsA = getCODIFFrameBytes(header);
      if (!is_legal_codif_framesize(fsA)) {
	//continue;
      }
      if (fsA+*offset*2*CODIF_HEADER_BYTES> length) continue;  // Need two frame headers plus offset

      secA = header->seconds;
      refEpochA = header->epoch;
      versionA = header->version;
      chanA = getCODIFNumChannels(header);

      header = (codif_header*)(data + *offset + fsA + CODIF_HEADER_BYTES);
      
      if (header->sync != 0xABADDEED) continue; // Sync

      fsB = getCODIFFrameBytes(header);
      secB = header->seconds;
      refEpochB = header->epoch;
      versionB = header->version;
      chanB = getCODIFNumChannels(header);
      
      /* does it look reasonable? */
      if (fsA==fsB && refEpochA==refEpochB && (secA==secB || secA+1==secB) && versionA==versionB && chanA==chanB) {
	*framesize = fsA+CODIF_HEADER_BYTES;
	*headersize = CODIF_HEADER_BYTES;
	return 0;
      }
    }
    return -1;
}

int get_codif_chans_per_thread(const codif_header *header)
{
    return getCODIFNumChannels(header);
}

uint64_t get_codif_samples_per_period(const codif_header *header)
{
    return header->totalsamples;
}

double get_codif_rate(const codif_header *header)
{
    double rate;
    rate = getCODIFTotalSamples(header) * getCODIFNumChannels(header) * header ->nbits / (double)header->period / 1e6;
    if (header->iscomplex) rate *= 2;
    return rate;
}

uint32_t get_codif_frames_per_period(const codif_header *header)
{
    uint64_t databytes = (uint64_t)getCODIFFrameBytes(header);
    uint64_t samplesperframe = (databytes*8) / (getCODIFNumChannels(header) * getCODIFBitsPerSample(header));
    if (getCODIFComplex(header))
    {
        samplesperframe /= 2;
    }
    return (uint32_t)(getCODIFTotalSamples(header) / samplesperframe);
}

uint32_t get_codif_period(const codif_header *header)
{
    return getCODIFPeriod(header);
}

uint32_t get_codif_alignment_seconds(const codif_header *header)
{
    return getCODIFPeriod(header);;
}

int get_codif_quantization_bits(const codif_header *header)
{
    return getCODIFPeriod(header);
}

int get_codif_complex(const codif_header *header)
{
    return getCODIFComplex(header);
}

double get_codif_framens(const codif_header *header)
{
    double framens;
    framens = getCODIFFrameBytes(header)*8*getCODIFPeriod(header)/(double)(header->totalsamples * getCODIFNumChannels(header) * header->nbits)*1e9;
    if (header->iscomplex) framens /= 2;
    return framens;
}

int get_codif_framegranularity(const codif_header *header)
{
	int bitspersample = header->nbits;
	if (header->iscomplex)
	{
		bitspersample *= 2;
	}
	uint64_t framesperperiod = (header->totalsamples * getCODIFNumChannels(header) * bitspersample) / (getCODIFFrameBytes(header)*8);
	int granularity;
	for (granularity=1; granularity<1024; ++granularity)
	{
		if (((uint64_t)1000000000*granularity*getCODIFPeriod(header)) % (framesperperiod) == 0)
		{
			break;
		}
    	}

	if (granularity>=1024)
	{
		granularity = -1;
	}
	return(granularity);
}

int get_codif_threads(const unsigned char *data, size_t length, int dataframesize)
{
        const int maxThreads = 1024; //  Actual max is 2^16, but unlikely that many on one data stream
	size_t i;
	unsigned short int threads[maxThreads];
	unsigned int nThread = 0;

	for(i = 0; i < length-32; i += dataframesize)
	{
		const uint32_t *frame;
		unsigned short int thread;
		unsigned short int t; 
		
		frame = (uint32_t *)(data+i);

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

