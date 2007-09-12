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
			if(data[i+j] != 0xFFFFFFFF) 
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

static void VLBA_stream_frame_time_mark5b(const struct VLBA_stream *vs, 
	double *mjd, double *sec)
{
	char nibs[16];
	const unsigned char *buf;
	int framenum;
	int i;

	buf = ((unsigned char *)vs->frame) + 8;

	framenum = vs->frame[1] & 0x7fff;
	
	for(i = 0; i < 4; i++)
	{
		nibs[2*i+0] = buf[3-i] >> 4;
		nibs[2*i+1] = buf[3-i] & 0x0F;
		nibs[2*i+8] = buf[7-i] >> 4;
		nibs[2*i+9] = buf[7-i] & 0x0F;
	}

	if(mjd)
	{
		*mjd = nibs[0]*100 + nibs[1]*10 + nibs[2];
	}
	if(sec) 
	{
		*sec = nibs[3]*10000 + nibs[4]*1000 + nibs[5]*100 + nibs[6]*10
	        	+ nibs[7];
		if(vs->frametime <= 0)
		{
			*sec += nibs[8]/10.0 + nibs[9]/100.0 
				+ nibs[10]/1000.0 + nibs[11]/10000.0;
		}
		else
		{
			*sec += framenum*vs->frametime;
		}
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
	else if(vs->format == FORMAT_MARK5B)
	{
		VLBA_stream_frame_time_mark5b(vs, mjd, sec);
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
	if(vs->format != FORMAT_MARK5B)
	{
		for(i = 0; i < vs->tracks; i++) 
		{
			if(vs->frame[i] != 0xFFFFFFFF) 
			{
				fprintf(stderr, "Sync not found in frame %d\n",
					vs->framenum);
				vs->payload = 0;
				return -1;
			}
		}
	}
	
	vs->payload = vs->frame + vs->payload_offset;
	
	/* now do the demodulation */
	if(vs->format == FORMAT_VLBA) switch(vs->tracks)
	{
		case 8:
		{
			uint8_t *dat;
			dat = (uint8_t *)(vs->payload);
			for(i = vs->firstvalid; i <= vs->lastvalid; i++)
			{
				dat[i] ^= modbits[i];
			}
			break;
		}
		case 16:
		{
			uint16_t *dat;
			dat = (uint16_t *)(vs->payload);
			for(i = vs->firstvalid; i <= vs->lastvalid; i++)
			{
				dat[i] ^= modbits[i];
			}
			break;
		}
		case 32:
		{
			uint32_t *dat;
			dat = vs->payload;
			for(i = vs->firstvalid; i <= vs->lastvalid; i++)
			{
				dat[i] ^= modbits[i];
			}
			break;
		}
		case 64:
		{
			uint64_t *dat;
			dat = (uint64_t *)(vs->payload);
			for(i = vs->firstvalid; i <= vs->lastvalid; i++)
			{
				dat[i] ^= modbits64[i];
			}
			break;
		}
		default:
		{
			return 0;
		}
	}

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
		case 64:
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

	i = findfirstframe64(vs->frame, bufsize); 
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

static int decode_Mark5B(struct VLBA_stream *vs)
{
	int i;
	const unsigned char *buf;

	buf = (unsigned char *)vs->frame;
	
	/* look for sync word 0xABADDEED in two consecutive frames */
	for(i = 0; i < 10016; i++)
	{
		if(buf[3]     == 0xAB &&
		   buf[2]     ==   0xAD &&
		   buf[1]     ==     0xDE &&
		   buf[0]     ==       0xED &&
		   buf[10019] == 0xAB &&
		   buf[10018] ==   0xAD &&
		   buf[10017] ==     0xDE &&
		   buf[10016] ==       0xED)
		{
			vs->frameoffset = i;
			vs->frame = (uint32_t *)buf;
			vs->tracks = 32;
			vs->format = FORMAT_MARK5B;
			vs->gulpsize = 10016;
			vs->payload = vs->frame + 4;
			vs->firstvalid = 0;
			vs->lastvalid = 9999;
			vs->payload_offset = vs->payload - vs->frame;
			vs->nchan = vs->tracks/(vs->fanout*vs->bits);
			return vs->tracks;
		}
		buf++;
	}

	return 0;
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
		if(data[i+VLBA_FRAMESIZE*vs->tracks/32] != 0xFFFFFFFF)
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
	vs->frametime = 0.0;

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
		if(decode_Mark5B(vs) < 8)
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
	}

	if(vs->format != FORMAT_MARK5B)
	{
		VLBA_stream_decode_format(vs);
	}
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
	}
	
	VLBA_stream_frame_time(vs, &mjd1, &sec1);

	dt = sec1 - vs->sec;
	if(dt < 0.0) 
	{
		dt += 86400.0;
	}

	/* Note -- due to insufficient time resolution in 4 frames the 
	 * algorithm below fails for data rates higher than 4096 Mbps and
	 * half the time for 4096 Mbps
	 */
	if(vs->format == FORMAT_MARK5B)
	{
		int n;
	
		status = next_frame(vs);
		status = next_frame(vs);
		status = next_frame(vs);
		if(status < 0)
		{
			if(vs->final)
			{
				vs->final(vs);
			}
			free(vs);
			fprintf(stderr, "VLBA_stream_generic_open: next() failed\n");
		}
		
		VLBA_stream_frame_time(vs, &mjd1, &sec1);
		dt = sec1 - vs->sec;
	
		n = dt*10000.0 + 0.5;
		if(vs->samprate <= 0) switch(n)
		{
			case 0:
				dt = 0.000078125;	/* 4096? Mbps */
			case 1:
			case 2:
				dt = 0.00015625;	/* 2048 Mbps */
				break;
			case 3:
			case 4:
				dt = 0.0003125;		/* 1024 Mbps */
				break;
			case 6:
			case 7:
				dt = 0.000625;		/* 512 Mbps */
				break;
			case 12:
			case 13:
				dt = 0.00125;		/* 256 Mbps */
				break;
		}
		vs->frametime = 0.25*dt; 
		vs->samprate = (int)(320000/(vs->nchan*vs->bits*dt) + 0.5);

		/* back up a three extra frames */
		prev_frame(vs);
		prev_frame(vs);
		prev_frame(vs);
	}
	else
	{
		vs->frametime = dt; 
		vs->samprate = (int)(fanout*PAYLOADSIZE/dt + 0.5);
	}
	
	/* after getting time interval, back up to reset pointer, if possible.
	 * otherwise, call this frame 0 
	 */
	prev_frame(vs);
	vs->framenum = 0;
	VLBA_stream_frame_time(vs, &vs->mjd, &vs->sec);

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

static int Mark5B_get_data_2bit(struct VLBA_stream *vs, int nsamp, 
	vlbatype **data)
{
}

static int Mark5B_get_data_1bit(struct VLBA_stream *vs, int nsamp, 
	vlbatype **data)
{
}

static int VLBA_stream_get_data_2bit8(struct VLBA_stream *vs, int nsamp, 
	vlbatype **data)
{
	const double lut[4] = {-3.3359, 1.0, -1.0, 3.3359};
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
			p = buf[o];
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
	vlbatype **data)
{
	const double lut[2] = {1.0, -1.0};
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
			p = buf[o];
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
	vlbatype **data)
{
	const double lut[4] = {-3.3359, 1.0, -1.0, 3.3359};
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
			p = buf[o];
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
	vlbatype **data)
{
	const double lut[2] = {1.0, -1.0};
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
			p = buf[o];
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
	vlbatype **data)
{
	const double lut[4] = {-3.3359, 1.0, -1.0, 3.3359};
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
			p = buf[o];
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
	vlbatype **data)
{
	const double lut[2] = {1.0, -1.0};
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
			p = buf[o];
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
	vlbatype **data)
{
	const double lut[4] = {-3.3359, 1.0, -1.0, 3.3359};
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
			p = buf[o];
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
	vlbatype **data)
{
	const double lut[2] = {1.0, -1.0};
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
			p = buf[o];
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


int VLBA_stream_get_data(struct VLBA_stream *vs, int nsamp, vlbatype **data)
{
	if(vs->format == FORMAT_MARK5B)
	{
	    if(vs->bits == 2)
	    {
	    	return Mark5B_get_data_2bit(vs, nsamp, data);
	    }
	    else if(vs->bits == 1)
	    {
	    	return Mark5B_get_data_1bit(vs, nsamp, data);
	    }
	    else
	    {
		fprintf(stderr, "VLBA_stream_get_data: only 1 or 2 bits "
				"allowed now\n");
		return -1;
	    }
	}
	else
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
}

