
/***************************************************************************
 *   Copyright (C) 2009 by Walter Brisken                                  *
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
// $Id: $
// $HeadURL: $
// $LastChangedRevision: 1310 $
// $Author: $
// $LastChangedDate: $
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


static const float HiMag = 3.3359;

static float lut1bit[256][8];
static float lut2bit[256][4];
static float zeros[8];

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
	int b, i, l;
	
	for(i = 0; i < 8; i++)
	{
		zeros[i] = 0.0;
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
			lut1bit[b][i] = lut4level[l];
		}
	}
}

/* inspect frame (packet) and return mjd and time */
static int mark5_stream_frame_time_vdif(const struct mark5_stream *ms,
	int *mjd, int *sec, double *ns)
{
	struct mark5_format_vdif *v;
	unsigned int *headerwords, word0, word1;
	unsigned char *headerbytes;
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

	headerwords = (unsigned int *)(ms->frame);

#ifdef WORDS_BIGENDIAN
	/* Motorola byte order requires some fiddling */
	headerbytes = ms->frame + 0;
	word0 = (headerbytes[0] << 24) | (headerbytes[1] << 16) | (headerbytes[2] << 8) | headerbytes[3];
	headerbytes = ms->frame + 4;
	word1 = (headerbytes[0] << 24) | (headerbytes[1] << 16) | (headerbytes[2] << 8) | headerbytes[3];
#else
	/* Intel byte order does not */
	word0 = headerwords[0];
	word1 = headerwords[1];
#endif

	headerbytes = ms->frame;

	seconds = word0 & 0x3FFFFFFF;	/* bits 0 to 29 */
	refepoch = headerbytes[7] & 0x3F;

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

static int vdif_decode_1channel_1bit_decimation1(struct mark5_stream *ms,
	int nsamp, float **data)
{
	unsigned char *buf;
	float *fp;
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

static int vdif_decode_2channel_1bit_decimation1(struct mark5_stream *ms,
	int nsamp, float **data)
{
	unsigned char *buf;
	float *fp;
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

static int vdif_decode_4channel_1bit_decimation1(struct mark5_stream *ms,
	int nsamp, float **data)
{
	unsigned char *buf;
	float *fp;
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

static int vdif_decode_8channel_1bit_decimation1(struct mark5_stream *ms,
	int nsamp, float **data)
{
	unsigned char *buf;
	float *fp;
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

static int vdif_decode_16channel_1bit_decimation1(struct mark5_stream *ms,
	int nsamp, float **data)
{
	unsigned char *buf;
	float *fp0, *fp1;
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

static int vdif_decode_32channel_1bit_decimation1(struct mark5_stream *ms,
	int nsamp, float **data)
{
	unsigned char *buf;
	float *fp0, *fp1, *fp2, *fp3;
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

static int vdif_decode_1channel_2bit_decimation1(struct mark5_stream *ms,
	int nsamp, float **data)
{
	unsigned char *buf;
	float *fp;
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

static int vdif_decode_2channel_2bit_decimation1(struct mark5_stream *ms,
	int nsamp, float **data)
{
	unsigned char *buf;
	float *fp;
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

static int vdif_decode_4channel_2bit_decimation1(struct mark5_stream *ms,
	int nsamp, float **data)
{
	unsigned char *buf;
	float *fp;
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

static int vdif_decode_8channel_2bit_decimation1(struct mark5_stream *ms,
	int nsamp, float **data)
{
	unsigned char *buf;
	float *fp0, *fp1;
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

static int vdif_decode_16channel_2bit_decimation1(struct mark5_stream *ms,
	int nsamp, float **data)
{
	unsigned char *buf;
	float *fp0, *fp1, *fp2, *fp3;
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


/* Other decoders go here */

/******************************************************************/

static int mark5_format_vdif_make_formatname(struct mark5_stream *ms)
{
	if(ms->format == MK5_FORMAT_VDIF)	/* True VDIF header, not legacy */
	{
		sprintf(ms->formatname, "VDIF-%d-%d-%d", ms->Mbps,
			ms->nchan, ms->nbit);
	}
	else if(ms->format == MK5_FORMAT_VDIFL)	/* Must be legacy mode, so add an L to VDIF name */
	{
		sprintf(ms->formatname, "VDIFL-%d-%d-%d", ms->Mbps,
			ms->nchan, ms->nbit);
	}
	else
	{
		sprintf(ms->formatname, "VDIF?");
		fprintf(stderr, "Warning: mark5_format_vdif_make_formatname: format not set\n");
		return -1;
	}

	return 0;
}

static int one(const struct mark5_stream *ms)
{
	return 1;
}

static int mark5_format_vdif_init(struct mark5_stream *ms)
{
	struct mark5_format_vdif *f;
	unsigned int *headerwords, word2;
	unsigned char *headerbytes;
	int framensNum, framensDen, dataframelength;
	double dns;

	if(!ms)
	{
		fprintf(stderr, "mark5_format_vdif_init: ms = 0\n");
		return -1;
	}

	f = (struct mark5_format_vdif *)(ms->formatdata);

	ms->payloadoffset = f->frameheadersize;
	ms->databytes = f->databytesperpacket;
	ms->framebytes = f->databytesperpacket + f->frameheadersize;
	ms->samplegranularity = 8/(ms->nchan*ms->nbit*ms->decimation);
	if(ms->samplegranularity <= 0)
	{
		ms->samplegranularity = 1;
	}
	ms->framesamples = ms->databytes*8/(ms->nchan*ms->nbit*ms->decimation);
	ms->blanker = blanker_vdif;
	if(ms->Mbps > 0)
	{
		ms->framens = 8000.0*f->databytesperpacket/ms->Mbps;
	}

	/* Aha -- we have some data to look at to further refine the format... */
	if(ms->datawindow)
	{
		ms->frame = ms->datawindow;
		ms->payload = ms->frame + ms->payloadoffset;

		headerwords = (unsigned int *)(ms->frame);

#ifdef WORDS_BIGENDIAN
		/* Motorola byte order requires some fiddling */
		headerbytes = ms->frame + 8;
		word2 = (headerbytes[0] << 24) | (headerbytes[1] << 16) | (headerbytes[2] << 8) | headerbytes[3];
#else
		/* Intel byte order does not */
		word2 = headerwords[2];
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
				fprintf(stderr, "VDIF Warning: Changing frameheadersize from %d to 16\n",
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
				fprintf(stderr, "VDIF Warning: Changing frameheadersize from %d to 32\n",
					f->frameheadersize);
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
			fprintf(stderr, "VDIF Warning: Changing databytesperpacket from %d to %d\n",
				f->databytesperpacket, dataframelength - f->frameheadersize);
			f->databytesperpacket = dataframelength - f->frameheadersize;
		}

		ms->payloadoffset = f->frameheadersize;
		ms->databytes = f->databytesperpacket - f->frameheadersize;
		ms->framebytes = f->databytesperpacket;
		ms->framesamples = ms->databytes*8/(ms->nchan*ms->nbit*ms->decimation);
		

		/* get time again so ms->framens is used */
		ms->gettime(ms, &ms->mjd, &ms->sec, &dns);
		ms->ns = (int)(dns + 0.5);

		/* WRITEME */
	}

	f->completesamplesperword = 32/(ms->nbit*ms->nchan);

	ms->framegranularity = 1;
	if(ms->Mbps > 0)
	{
		framensNum = 250*f->databytesperpacket*f->completesamplesperword*ms->nchan*ms->nbit;
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
			fprintf(stderr, "VDIF Warning: cannot calculate gframens %d/%d\n",
				framensNum, framensDen);
			ms->framegranularity = 1;
		}
	}
	else
	{
		ms->framens = 0;
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
		fprintf(stderr, "Error: mark5_format_vdif_init: unsupported frameheadersize=%d\n",
			f->frameheadersize);
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

void mark5_format_vdif_set_leapsecs(struct mark5_stream *ms, int leapsecs)
{
	struct mark5_format_vdif *f;
	
	f = (struct mark5_format_vdif *)(ms->formatdata);

	f->leapsecs = leapsecs;
}

struct mark5_format_generic *new_mark5_format_vdif(int Mbps, 
	int nchan, int nbit, int decimation, 
	int databytesperpacket, int frameheadersize)
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
	else if(decimation == 2)
	{
		decoderindex += 1024;
	}
	else
	{
		fprintf(stderr, "VDIF decimation must be 1 for now\n");
		return 0;
	}

	if(nbit == 1) /* inc by 32 for each successive value to allow full range of nchan */
	{
		decoderindex += 0;
	}
	if(nbit == 2)
	{
		decoderindex += 32;
	}
	else
	{
		fprintf(stderr, "VDIF nbit must be 1 or 2 for now\n");
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
	else
	{
		fprintf(stderr, "VDIF nchan must be 1, 2, 4, 8, 16 or 32 for now\n");
		return 0;
	}

	v = (struct mark5_format_vdif *)calloc(1,
		sizeof(struct mark5_format_vdif));
	f = (struct mark5_format_generic *)calloc(1, 
		sizeof(struct mark5_format_generic));

	v->frameheadersize = frameheadersize;
	v->databytesperpacket = databytesperpacket;

	f->Mbps = Mbps;
	f->nchan = nchan;
	f->nbit = nbit;
	f->formatdata = v;
	
	/* set some function pointers */
	f->gettime = mark5_stream_frame_time_vdif;
	f->init_format = mark5_format_vdif_init;
	f->final_format = mark5_format_vdif_final;
	f->validate = one;
	f->decimation = decimation;
	f->decode = 0;

	switch(decoderindex)
	{
		case    0 : f->decode = vdif_decode_1channel_1bit_decimation1; break;
		case    1 : f->decode = vdif_decode_2channel_1bit_decimation1; break;
		case    2 : f->decode = vdif_decode_4channel_1bit_decimation1; break;
		case    3 : f->decode = vdif_decode_8channel_1bit_decimation1; break;
		case    4 : f->decode = vdif_decode_16channel_1bit_decimation1; break;
		case    5 : f->decode = vdif_decode_32channel_1bit_decimation1; break;

		case   32 : f->decode = vdif_decode_1channel_2bit_decimation1; break;
		case   33 : f->decode = vdif_decode_2channel_2bit_decimation1; break;
		case   34 : f->decode = vdif_decode_4channel_2bit_decimation1; break;
		case   35 : f->decode = vdif_decode_8channel_2bit_decimation1; break;
		case   36 : f->decode = vdif_decode_16channel_2bit_decimation1; break;
	}

	if(f->decode == 0)
	{
		fprintf(stderr, "VDIF: Illegal combination of decimation, channels and bits\n");
		free(v);
		free(f);
		return 0;
	}

	return f;
}
