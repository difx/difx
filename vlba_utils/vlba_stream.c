/***************************************************************************
 *   Copyright (C) 2006, 2007 by Walter Brisken                            *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
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
#include <errno.h>
#include <fcntl.h>
#include "vlba_stream.h"

uint32_t *modbits=0;
uint64_t *modbits64=0;

void initmodbits()
{
	int i, n, k;
	unsigned int ff[16] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
	
	if(!modbits) 
	{
		modbits = (uint32_t *)malloc(PAYLOADSIZE*sizeof(uint32_t));
	}
	if(!modbits64) 
	{
		modbits64 = (uint64_t *)malloc(PAYLOADSIZE*sizeof(uint64_t));
	}
	
	for(i = 0; i < PAYLOADSIZE; i++)
	{
		k = ff[10] ^ ff[12] ^ ff[13] ^ ff[15];
		for(n = 15; n > 0; n--) ff[n] = ff[n-1];
		ff[0] = k;
		modbits[i] = k*0xFFFFFFFF;
		modbits64[i] = k*0xFFFFFFFFFFFFFFFFLL;
		if(i % 8 == 7) /* Skip the parity bit */
		{
			k = ff[10] ^ ff[12] ^ ff[13] ^ ff[15];
			for(n = 15; n > 0; n--) ff[n] = ff[n-1];
			ff[0] = k;
		}
	}
}

static int findfirstframe8(uint32_t *data, int samples)
{
	int i, j;
	uint8_t *dat;

	dat = (uint8_t *)data;

	for(i = 1; i < samples-32; i++)
	{
		if(dat[i-1])
		{
			continue;
		}
		for(j = 0; j < 32; j++)
		{
			if(dat[i+j] != 0xFF)
			{
				break;
			}
		}
		if(j == 32)
		{
			return i;
		}
	}

	return -1;
}

static int findfirstframe16(uint32_t *data, int samples)
{
	int i, j;
	uint16_t *dat;

	dat = (uint16_t *)data;

	for(i = 1; i < samples-32; i++)
	{
		if(dat[i-1]) 
		{
			continue;
		}
		for(j = 0; j < 32; j++)
		{
			if(dat[i+j] != 0xFFFF)
			{
				break;
			}
		}
		if(j == 32)
		{
			return i*2;
		}
	}

	return -1;
}

static int findfirstframe32(uint32_t *data, int samples)
{
	int i, j;

	for(i = 1; i < samples-32; i++)
	{
		if(data[i-1])
		{
			continue;
		}
		for(j = 0; j < 32; j++)
		{
			if(data[i+j] != 0xFFFFFFFF)
			{
				break;
			}
		}
		if(j == 32)
		{
			return i*4;
		}
	}

	return -1;
}

static int findfirstframe64(uint32_t *data, int samples)
{
	int i, j;

	for(i = 2; i < 2*samples-64; i++)
	{
		if(data[i-1] || data[i-2])
		{
			continue;
		}
		for(j = 0; j < 64; j++)
		{
			if(data[i+j] != 0xFFFFFFFF && data[i+j] != 0xFFEFFFFF) 
			{
				break;
			}
		}
		if(j == 64)
		{
			return i*4;
		}
	}

	return -1;
}


static void extractnibbles8(const unsigned int *data, int track,
	int numnibbles, char *nibbles)
{
	uint32_t mask;
	int i, b;
	const uint8_t *dat;

	dat = (uint8_t *)data;

	mask = 1 << track;

	for(i = 0; i < numnibbles; i++)
	{
		nibbles[i] = 0;
		for(b = 0; b < 4; b++)
		{
			nibbles[i] += (dat[4*i+3-b] & mask) ? (1<<b) : 0;
		}
	}
}

static void extractnibbles16(const unsigned int *data, int track,
	int numnibbles, char *nibbles)
{
	uint32_t mask;
	int i, b;
	const uint16_t *dat;

	dat = (uint16_t *)data;

	mask = 1 << track;

	for(i = 0; i < numnibbles; i++)
	{
		nibbles[i] = 0;
		for(b = 0; b < 4; b++)
		{
			nibbles[i] += (dat[4*i+3-b] & mask) ? (1<<b) : 0;
		}
	}
}

static void extractnibbles32(const unsigned int *data, int track,
	int numnibbles, char *nibbles)
{
	uint32_t mask;
	int i, b;

	mask = 1 << track;

	for(i = 0; i < numnibbles; i++)
	{
		nibbles[i] = 0;
		for(b = 0; b < 4; b++)
		{
			nibbles[i] += (data[4*i+3-b] & mask) ? (1<<b) : 0;
		}
	}
}

static void extractnibbles64(const unsigned int *data, int track,
	int numnibbles, char *nibbles)
{
	uint64_t mask;
	int i, b;
	const uint64_t *dat;

	dat = (uint64_t *)data;

	mask = 1LL << track;

	for(i = 0; i < numnibbles; i++)
	{
		nibbles[i] = 0;
		for(b = 0; b < 4; b++)
		{
			nibbles[i] += (dat[4*i+3-b] & mask) ? (1<<b) : 0;
		}
	}
}

static void VLBA_stream_frame_time_mark4(const struct VLBA_stream *vs, 
	double *mjd, double *sec)
{
	char nibs[13];
	const double lastdig[] = {0.0, 0.00125, 0.0025, 0.00375, 0.0, 0.005,
				  0.00625, 0.0075, 0.00875, 0.0};

	switch(vs->tracks)
	{
	case 8:
		extractnibbles8(vs->frame+vs->tracks, 0, 13, nibs);
		break;
	case 16:
		extractnibbles16(vs->frame+vs->tracks, 0, 13, nibs);
		break;
	case 32:
		extractnibbles32(vs->frame+vs->tracks, 0, 13, nibs);
		break;
	case 64:
		extractnibbles64(vs->frame+vs->tracks, 0, 13, nibs);
		break;
	default:
		return;
	}

	/* For now assume between years 2000 and 2010 */
	if(mjd) 
	{
		*mjd = 51544 + 365*nibs[0] + 100*nibs[1] + 10*nibs[2] +nibs[3]
			+ (int)(nibs[0]/4);
	}

	if(sec)
	{
		*sec = nibs[4]*36000 + nibs[5]*3600 + nibs[6]*600 + nibs[7]*60
			+ nibs[8]*10 + nibs[9] + nibs[10]/10.0 + nibs[11]/100.0
			+ lastdig[(unsigned int)(nibs[12])];
	}
}

static void VLBA_stream_frame_time_vlba(const struct VLBA_stream *vs, 
	double *mjd, double *sec)
{
	char nibs[12];

	switch(vs->tracks)
	{
	case 8:
		extractnibbles8(vs->frame+vs->tracks, 0, 12, nibs);
		break;
	case 16:
		extractnibbles16(vs->frame+vs->tracks, 0, 12, nibs);
		break;
	case 32:
		extractnibbles32(vs->frame+vs->tracks, 0, 12, nibs);
		break;
	case 64:
		extractnibbles64(vs->frame+vs->tracks, 0, 12, nibs);
		break;
	default:
		return;
	}

	if(mjd)
	{
		*mjd = nibs[0]*100 + nibs[1]*10 + nibs[2];
	}
	if(sec) 
	{
		*sec = nibs[3]*10000 + nibs[4]*1000 + nibs[5]*100 + nibs[6]*10
	        	+ nibs[7] + nibs[8]/10.0 + nibs[9]/100.0 
			+ nibs[10]/1000.0 + nibs[11]/10000.0;
	}
}

void VLBA_stream_frame_time(const struct VLBA_stream *vs, double *mjd, 
	double *sec)
{
	if(vs->format == FORMAT_VLBA)
	{
		VLBA_stream_frame_time_vlba(vs, mjd, sec);
	}
	else if(vs->format == FORMAT_MARK4)
	{
		VLBA_stream_frame_time_mark4(vs, mjd, sec);
	}
}

void VLBA_stream_time(const struct VLBA_stream *vs, double *mjd, double *sec)
{
	VLBA_stream_frame_time(vs, mjd, sec);
	
	*sec += (double)vs->read_position*(double)vs->frametime/PAYLOADSIZE;
	
	/* The first bit of data in a frame should be timetagged as if it
	 * were 160 samples into the stream.  Thus 64 samples extra need to be
	 * added.
	 */
	if(vs->format == FORMAT_MARK4)
	{
		*sec += 64.0*(double)vs->frametime/PAYLOADSIZE;
	}
}

int prev_frame(struct VLBA_stream *vs)
{
	int status;
	
	if(vs->prev)
	{
		status = vs->prev(vs);
		if(status >= 0)
		{
			vs->framenum--;
			vs->payload = vs->frame + vs->payload_offset;
		}
		return status;
	}
	else	/* rewinding was not possible */
	{
		return -1;
	}
}

int next_frame(struct VLBA_stream *vs)
{
	int i, n;

	/* call specialized function to ready next frame */
	n = vs->next(vs);

	/* are we at end of file(s)? */
	if(n != vs->gulpsize)
	{
		vs->payload = 0;
		return -1;
	}
	
	/* successfully got new frame, so increment it */
	vs->framenum++;

	/* check for sync -- just a minimal check */
	for(i = 0; i < vs->tracks; i++) 
	{
		if(vs->frame[i] != 0xFFFFFFFF && 
		   vs->frame[i] != 0xFFEFFFFF)
		{
			fprintf(stderr, "Sync not found in frame %d\n",
				vs->framenum);
			vs->payload = 0;
			return -1;
		}
	}
	
	vs->payload = vs->frame + vs->payload_offset;

	return 0;
}

void VLBA_stream_set_basebits(struct VLBA_stream *vs, int nchan, 
	const int *basebits)
{
	int maxchan, c, delta;

	maxchan = vs->tracks / (vs->bits*vs->fanout);

	if(nchan > maxchan)
	{
		fprintf(stderr, "VLBA_stream_set_basebits: "
			"nchan > maxchan (%d)\n", maxchan);
		exit(0);
	}

	if(vs->basebits)
	{
		free(vs->basebits);
	}
	
	if(nchan == 0) 
	{
		nchan = maxchan;
		basebits = 0;
	}

	if(!basebits)	/* set defaults */
	{
		vs->nchan = nchan;
		vs->basebits = (int *)malloc(nchan*sizeof(int));
		switch(vs->tracks)
		{
		case 8:
		case 16:
			delta = vs->bits*vs->fanout;
			for(c = 0; c < nchan; c++)
			{
				vs->basebits[c] = c*delta;
			}
			break;
		case 32:
			delta = 2*vs->bits*vs->fanout;
			for(c = 0; c < nchan; c++)
			{
				if(c < maxchan/2)	/* Evens */
				{
					vs->basebits[c] = c*delta;
				}
				else			/* Odds */
				{
					vs->basebits[c] = (c-maxchan/2)*delta+1;
				}
			}
			break;
		case 64:
			delta = 2*vs->bits*vs->fanout;
			for(c = 0; c < nchan; c++)
			{
				int q;

				q = 4*c / maxchan;

				switch(q)
				{
				case 0: /* boardset 1 evens */
					vs->basebits[c] = c*delta;
					break;
				case 1: /* boardset 1 odds */
					vs->basebits[c] = (c-maxchan/4)*delta+1;
					break;
				case 2: /* boardset 2 evens */
					vs->basebits[c] = (c-maxchan/4)*delta;
					break;
				case 3: /* boardset 2 odds */
					vs->basebits[c] = (c-maxchan/2)*delta+1;
					break;
				}
			}
			break;
		default:
			fprintf(stderr, "VLBA_stream_set_basebits: "
				"bad number of tracks\n");
			exit(0);
		}
	}
	else
	{
		vs->nchan = nchan;
		vs->basebits = (int *)malloc(nchan*sizeof(int));
		for(c = 0; c < nchan; c++)
		{
			vs->basebits[c] = basebits[c];
		}
	}

}

static int VLBA_stream_decode_tracks(struct VLBA_stream *vs)
{
	int bufsize;
	int i;

	bufsize = VLBA_FRAMESIZE+1024;

	i = findfirstframe64(vs->frame, 2*bufsize); 
	if(i >= 0) vs->tracks = 64;
	else
	{
		i = findfirstframe32(vs->frame, bufsize); 
		if(i >= 0) vs->tracks = 32;
		else
		{
			i = findfirstframe16(vs->frame, bufsize); 
			if(i >= 0) vs->tracks = 16;
			else 
			{
				i = findfirstframe8(vs->frame, bufsize); 
				if(i >= 0) vs->tracks = 8;
				else return 0;
			}
		}
	}
	
	vs->frameoffset = i;
	vs->frame = (uint32_t *)(((char *)(vs->frame)) + i);
	
	return vs->tracks;
}

static void VLBA_stream_decode_format(struct VLBA_stream *vs)
{
	int i;
	uint32_t *data;
	int sync;
	
	vs->gulpsize = VLBA_FRAMESIZE*vs->tracks/8 + 4*vs->tracks;

	data = vs->frame;
	
	sync = 1;
	for(i = 0; i < vs->tracks; i++)
	{
		if(data[i+VLBA_FRAMESIZE*vs->tracks/32] != 0xFFFFFFFF &&
		   data[i+VLBA_FRAMESIZE*vs->tracks/32] != 0xFFEFFFFF)
		{
			sync = 0;
		}
	}
	if(sync)
	{
		vs->format = FORMAT_VLBA;
		vs->gulpsize = VLBA_FRAMESIZE*vs->tracks/8;
		vs->payload = vs->frame + (3*vs->tracks);
		vs->firstvalid = 0;
		vs->lastvalid = PAYLOADSIZE-1;
	}
	else
	{
		sync = 1;
		for(i = 0; i < vs->tracks; i++)
		{
			if(data[i+PAYLOADSIZE*vs->tracks/32] != 0xFFFFFFFF)
			{
				sync = 0;
			}
		}
		if(sync)
		{
			vs->format = FORMAT_MARK4;
			vs->gulpsize = PAYLOADSIZE*vs->tracks/8;
			vs->payload = vs->frame;
			vs->firstvalid = 96;
			vs->lastvalid = PAYLOADSIZE-1-64;
		}
		else 
		{
			vs->format = FORMAT_UNKNOWN;
			vs->payload = vs->frame;
			vs->firstvalid = vs->lastvalid = 0;
		}
	}

	vs->payload_offset = vs->payload - vs->frame;
}

void VLBA_stream_clear_statecount(struct VLBA_stream *vs)
{
	int c, i;

	for(c = 0; c < 16; c++)
	{
		for(i = 0; i < 4; i++) 
		{
			vs->statecount[c][i] = 0;
		}
	}
}

void VLBA_stream_print_statecount(struct VLBA_stream *vs)
{
	int nchan, c, i;
	double total;

	nchan = vs->tracks/vs->fanout/vs->bits;

	printf("State counts:\n");

	for(c = 0; c < nchan; c++)
	{
		total = 0.0;
		if(vs->bits == 1)
		{
			for(i = 0; i < 2; i++) 
			{
				total += vs->statecount[c][i];
			}
			printf("  %2d : %f %f\n", c, 
				vs->statecount[c][1]/total,
				vs->statecount[c][0]/total);
		}
		else
		{
			for(i = 0; i < 4; i++) 
			{
				total += vs->statecount[c][i];
			}
			printf("  %2d : %f %f %f %f\n", c, 
				vs->statecount[c][0]/total,
				vs->statecount[c][2]/total,
				vs->statecount[c][1]/total,
				vs->statecount[c][3]/total);
		}
	}
}

struct VLBA_stream *VLBA_stream_generic_open(struct VLBA_stream_generic *V, 
	int bits, int fanout)
{
	struct VLBA_stream *vs;
	double dt, sec1, mjd1;
	int status;

	if(!V->init || !V->next)
	{
		fprintf(stderr, "Incomplete VLBA_stream_generic type\n");
		return 0;
	}

	if(!modbits)
	{
		initmodbits();
	}
	
	vs = (struct VLBA_stream *)malloc(sizeof(struct VLBA_stream));
	vs->frame = 0;
	vs->bits = bits;
	vs->fanout = fanout;
	vs->basebits = 0;
	vs->read_position = 0;
	vs->init = V->init;
	vs->next = V->next;
	vs->prev = V->prev;
	vs->final = V->final;
	vs->inputdata = V->inputdata;

	status = vs->init(vs);
	if(status < 0)
	{
		if(vs->final)
		{
			vs->final(vs);
		}
		free(vs);
		fprintf(stderr, "VLBA_stream_generic_open: init() failed\n");
		return 0;
	}
	
	if(VLBA_stream_decode_tracks(vs) < 8)
	{
		if(vs->final)
		{
			vs->final(vs);
		}
		free(vs);
		fprintf(stderr, "VLBA_stream_generic_open: "
			"Cannot find sync word\n");
		return 0;
	}

	VLBA_stream_decode_format(vs);
	if(vs->format == FORMAT_UNKNOWN)
	{
		if(vs->final)
		{
			vs->final(vs);
		}
		free(vs);
		fprintf(stderr, "VLBA_stream_generic_open: "
			"Unknown format or corrupt data\n");
		return 0;
	}
	
	/* try backing up one frame in case initial sync was missed */
	if(prev_frame(vs) >= 0)
	{
		vs->framenum = 0;
		vs->frameoffset -= vs->gulpsize;
	}

	/* Now determine times of two frames to get timing info */
	VLBA_stream_frame_time(vs, &vs->mjd, &vs->sec);

	status = next_frame(vs);
	if(status < 0)
	{
		if(vs->final)
		{
			vs->final(vs);
		}
		free(vs);
		fprintf(stderr, "VLBA_stream_generic_open: next() failed\n");
		return 0;
	}
	
	VLBA_stream_frame_time(vs, &mjd1, &sec1);

	dt = sec1 - vs->sec;
	if(dt < 0.0) 
	{
		dt += 86400.0;
	}
	
	vs->frametime = dt; 
	vs->samprate = (int)(fanout*PAYLOADSIZE/dt + 0.5);
	
	/* after getting time interval, back up to reset pointer, if possible.
	 * otherwise, call this frame 0 
	 */
	if(prev_frame(vs) < 0)
	{
		vs->sec = sec1;
		vs->mjd = mjd1;
		vs->framenum = 0;
	}

	VLBA_stream_clear_statecount(vs);
	
	VLBA_stream_set_basebits(vs, 0, 0);

	/* to populate the deprecated "fileoffset" member */
	vs->fileoffset = vs->frameoffset;

	return vs;
}

void VLBA_stream_close(struct VLBA_stream *vs)
{
	if(vs)
	{
		if(vs->final)
		{
			vs->final(vs);
		}
		if(vs->basebits)
		{
			free(vs->basebits);
		}
		free(vs);
	}
}

void VLBA_stream_print(const struct VLBA_stream *vs)
{
	printf("VLBA_stream : %p\n", vs);
	if(vs)
	{
		printf("  mjd = %d\n", (int)(vs->mjd));
		printf("  sec = %f\n", vs->sec);
		printf("  samprate = %f\n", vs->samprate);
		printf("  frameoffset = %d\n", vs->frameoffset);
		printf("  framesize = %d\n", vs->gulpsize);
		printf("  tracks = %d\n", vs->tracks);
		printf("  bits = %d\n", vs->bits);
		printf("  fanout = %d\n", vs->fanout);
		printf("  format = %d\n", vs->format);
		printf("  nchan = %d\n", vs->nchan);
		printf("  payload offset = %d\n", vs->payload_offset);
	}
	printf("\n");
}

/*********************** data unpack routines **********************/

static int VLBA_stream_get_data_2bit8(struct VLBA_stream *vs, int nsamp, 
	float **data)
{
	const float lut[4] = {-3.3359, 1.0, -1.0, 3.3359};
	uint8_t *buf, p;
	int i, o, s, c, m, f;
	int index;

	buf = (uint8_t *)(vs->payload);
	if(buf == 0)
	{
		return -1;
	}

	o = vs->read_position;

	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
			{
				for(c = 0; c < vs->nchan; c++)
				{
					data[c][i+f] = 0.0;
				}
			}
		}
		else
		{
			if(vs->format == FORMAT_VLBA)
			{
				p = buf[o] ^ modbits[o];
			}
			else
			{
				p = buf[o];
			}
			for(f = 0; f < vs->fanout; f++) 
		    	{
				for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+f;
					m = s + vs->fanout;
					index = ((p>>s)&1) + (((p>>m)&1)<<1);
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
			}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0)
			{
				return -1;
			}
			buf = (uint8_t *)(vs->payload);
			o = 0;
		}
	}

	vs->read_position = o;

	return 0;
}

static int VLBA_stream_get_data_1bit8(struct VLBA_stream *vs, int nsamp, 
	float **data)
{
	const float lut[2] = {1.0, -1.0};
	uint8_t *buf, p;
	int i, o, s, c, f;
	int index;

	buf = (uint8_t *)(vs->payload);
	if(buf == 0)
	{
		return -1;
	}

	o = vs->read_position;

	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    	{
				for(c = 0; c < vs->nchan; c++)
				{
					data[c][i+f] = 0.0;
				}
			}
		}
		else
		{
			if(vs->format == FORMAT_VLBA)
			{
				p = buf[o] ^ modbits[o];
			}
			else
			{
				p = buf[o];
			}
			for(f = 0; f < vs->fanout; f++) 
			{
				for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+f;
					index = (p>>s)&1;
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
			}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0)
			{
				return -1;
			}
			buf = (uint8_t *)(vs->payload);
			o = 0;
		}
	}

	vs->read_position = o;

	return 0;
}

static int VLBA_stream_get_data_2bit16(struct VLBA_stream *vs, int nsamp, 
	float **data)
{
	const float lut[4] = {-3.3359, 1.0, -1.0, 3.3359};
	uint16_t *buf, p;
	int i, o, s, c, m, f;
	int index;

	buf = (uint16_t *)(vs->payload);
	if(buf == 0)
	{
		return -1;
	}

	o = vs->read_position;

	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    	{
				for(c = 0; c < vs->nchan; c++)
				{
					data[c][i+f] = 0.0;
				}
			}
		}
		else
		{
			if(vs->format == FORMAT_VLBA)
			{
				p = buf[o] ^ modbits[o];
			}
			else
			{
				p = buf[o];
			}
			for(f = 0; f < vs->fanout; f++) 
		    	{
				for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+f;
					m = s + vs->fanout;
					index = ((p>>s)&1) + (((p>>m)&1)<<1);
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
			}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0)
			{
				return -1;
			}
			buf = (uint16_t *)(vs->payload);
			o = 0;
		}
	}

	vs->read_position = o;

	return 0;
}

static int VLBA_stream_get_data_1bit16(struct VLBA_stream *vs, int nsamp, 
	float **data)
{
	const float lut[2] = {1.0, -1.0};
	uint16_t *buf, p;
	int i, o, s, c, f;
	int index;

	buf = (uint16_t *)(vs->payload);
	if(buf == 0)
	{
		return -1;
	}

	o = vs->read_position;

	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    	{
				for(c = 0; c < vs->nchan; c++)
				{
					data[c][i+f] = 0.0;
				}
			}
		}
		else
		{
			if(vs->format == FORMAT_VLBA)
			{
				p = buf[o] ^ modbits[o];
			}
			else
			{
				p = buf[o];
			}
			for(f = 0; f < vs->fanout; f++) 
		    	{
				for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+f;
					index = (p>>s)&1;
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
			}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0)
			{
				return -1;
			}
			buf = (uint16_t *)(vs->payload);
			o = 0;
		}
	}

	vs->read_position = o;

	return 0;
}

static int VLBA_stream_get_data_2bit32(struct VLBA_stream *vs, int nsamp, 
	float **data)
{
	const float lut[4] = {-3.3359, 1.0, -1.0, 3.3359};
	uint32_t *buf, p;
	int i, o, s, c, m, f;
	int index;

	buf = vs->payload;
	if(buf == 0)
	{
		return -1;
	}

	o = vs->read_position;

	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    	{
				for(c = 0; c < vs->nchan; c++)
				{
					data[c][i+f] = 0.0;
				}
			}
		}
		else
		{
			if(vs->format == FORMAT_VLBA)
			{
				p = buf[o] ^ modbits[o];
			}
			else
			{
				p = buf[o];
			}
			for(f = 0; f < vs->fanout; f++) 
			{
				for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+2*f;
					m = s + 2*vs->fanout;
					index = ((p>>s)&1) + (((p>>m)&1)<<1);
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
			}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0)
			{
				return -1;
			}
			buf = vs->payload;
			o = 0;
		}
	}

	vs->read_position = o;

	return 0;
}

static int VLBA_stream_get_data_1bit32(struct VLBA_stream *vs, int nsamp, 
	float **data)
{
	const float lut[2] = {1.0, -1.0};
	uint32_t *buf, p;
	int i, o, s, c, f;
	int index;

	buf = vs->payload;
	if(buf == 0)
	{
		return -1;
	}

	o = vs->read_position;

	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    	{
				for(c = 0; c < vs->nchan; c++)
				{
					data[c][i+f] = 0.0;
				}
			}
		}
		else
		{
			if(vs->format == FORMAT_VLBA)
			{
				p = buf[o] ^ modbits[o];
			}
			else
			{
				p = buf[o];
			}
			for(f = 0; f < vs->fanout; f++) 
		    	{
				for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+2*f;
					index = (p>>s)&1;
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
			}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0)
			{
				return -1;
			}
			buf = vs->payload;
			o = 0;
		}
	}

	vs->read_position = o;

	return 0;
}

static int VLBA_stream_get_data_2bit64(struct VLBA_stream *vs, int nsamp, 
	float **data)
{
	const float lut[4] = {-3.3359, 1.0, -1.0, 3.3359};
	uint64_t *buf, p;
	int f, o, i, m, s, c;
	int index;

	buf = (uint64_t *)(vs->payload);
	if(buf == 0)
	{
		return -1;
	}
	
	o = vs->read_position;
	
	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    	{
				for(c = 0; c < vs->nchan; c++)
				{
					data[c][i+f] = 0.0;
				}
			}
		}
		else
		{
			if(vs->format == FORMAT_VLBA)
			{
				p = buf[o] ^ modbits64[o];
			}
			else
			{
				p = buf[o];
			}
			for(f = 0; f < vs->fanout; f++) 
			{
				for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+2*f;
					m = s + 2*vs->fanout;
					index = ((p>>s)&1) + (((p>>m)&1)<<1);
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
			}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0)
			{
				return -1;
			}
			buf = (uint64_t *)(vs->payload);
			o = 0;
		}
	}

	vs->read_position = o;

	return 0;
}

static int VLBA_stream_get_data_1bit64(struct VLBA_stream *vs, int nsamp, 
	float **data)
{
	const float lut[2] = {1.0, -1.0};
	uint64_t *buf, p;
	int f, o, i, s, c;
	int index;

	buf = (uint64_t *)(vs->payload);
	if(buf == 0)
	{
		return -1;
	}
	
	o = vs->read_position;
	
	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    	{
				for(c = 0; c < vs->nchan; c++)
				{
					data[c][i+f] = 0.0;
				}
			}
		}
		else
		{
			if(vs->format == FORMAT_VLBA)
			{
				p = buf[o] ^ modbits64[o];
			}
			else
			{
				p = buf[o];
			}
			for(f = 0; f < vs->fanout; f++) 
			{
				for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+2*f;
					index = (p>>s)&1;
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
			}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0)
			{
				return -1;
			}
			buf = (uint64_t *)(vs->payload);
			o = 0;
		}
	}

	vs->read_position = o;

	return 0;
}


int VLBA_stream_get_data(struct VLBA_stream *vs, int nsamp, float **data)
{
	if(vs->bits == 2) 
	{
		switch(vs->tracks)
		{
		case 8:
			return VLBA_stream_get_data_2bit8(vs, nsamp, data);
		case 16:
			return VLBA_stream_get_data_2bit16(vs, nsamp, data);
		case 32:
			return VLBA_stream_get_data_2bit32(vs, nsamp, data);
		case 64:
			return VLBA_stream_get_data_2bit64(vs, nsamp, data);
		default:
			return 0;
		}
	}
	else if(vs->bits == 1)
	{
		switch(vs->tracks)
		{
		case 8:
			return VLBA_stream_get_data_1bit8(vs, nsamp, data);
		case 16:
			return VLBA_stream_get_data_1bit16(vs, nsamp, data);
		case 32:
			return VLBA_stream_get_data_1bit32(vs, nsamp, data);
		case 64:
			return VLBA_stream_get_data_1bit64(vs, nsamp, data);
		default:
			return 0;
		}
	}
	else 
	{
		fprintf(stderr, "VLBA_stream_get_data: only 1 or 2 bits "
				"allowed now\n");
		return -1;
	}
}

int VLBA_stream_get_data_double(struct VLBA_stream *vs, int nsamp, 
	double **data)
{
	double *d;
	float *f;
	int c;
	int i;
	int r;
	
	r = VLBA_stream_get_data(vs, nsamp, (float **)data);
	if(r) 
	{
		return r;
	}

	/* convert in place */
	for(c = 0; c < vs->nchan; c++)
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

	return 0;
}

int VLBA_stream_get_data_complex(struct VLBA_stream *vs, int nsamp, 
	vlba_float_complex **data)
{
	vlba_float_complex *fc;
	float *f;
	int c;
	int i;
	int r;
	
	r = VLBA_stream_get_data(vs, nsamp, (float **)data);
	if(r) 
	{
		return r;
	}

	/* convert in place */
	for(c = 0; c < vs->nchan; c++)
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

	return 0;
}

int VLBA_stream_get_data_double_complex(struct VLBA_stream *vs, int nsamp, 
	vlba_double_complex **data)
{
	vlba_double_complex *dc;
	float *f;
	int c;
	int i;
	int r;
	
	r = VLBA_stream_get_data(vs, nsamp, (float **)data);
	if(r) 
	{
		return r;
	}

	/* convert in place */
	for(c = 0; c < vs->nchan; c++)
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

	return 0;
}
