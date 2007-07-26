/***************************************************************************
 *   Copyright (C) 2006 by Walter Brisken                                  *
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
		modbits = (uint32_t *)malloc(PAYLOADSIZE*sizeof(uint32_t));
	if(!modbits64) 
		modbits64 = (uint64_t *)malloc(PAYLOADSIZE*sizeof(uint64_t));

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
		if(dat[i-1]) continue;
		for(j = 0; j < 32; j++) if(dat[i+j] != 0xFF) break;
		if(j == 32) return i;
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
		if(dat[i-1]) continue;
		for(j = 0; j < 32; j++) if(dat[i+j] != 0xFFFF) break;
		if(j == 32) return i*2;
	}

	return -1;
}

static int findfirstframe32(uint32_t *data, int samples)
{
	int i, j;

	for(i = 1; i < samples-32; i++)
	{
		if(data[i-1]) continue;
		for(j = 0; j < 32; j++) if(data[i+j] != 0xFFFFFFFF) break;
		if(j == 32) return i*4;
	}

	return -1;
}

static int findfirstframe64(uint32_t *data, int samples)
{
	int i, j;

	for(i = 2; i < 2*samples-64; i++)
	{
		if(data[i-1] || data[i-2]) continue;
		for(j = 0; j < 64; j++) if(data[i+j] != 0xFFFFFFFF) break;
		if(j == 64) return i*4;
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
			nibbles[i] += (dat[4*i+3-b] & mask) ? (1<<b) : 0;
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
			nibbles[i] += (dat[4*i+3-b] & mask) ? (1<<b) : 0;
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
			nibbles[i] += (data[4*i+3-b] & mask) ? (1<<b) : 0;
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
			nibbles[i] += (dat[4*i+3-b] & mask) ? (1<<b) : 0;
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
	if(mjd) *mjd = 51544 + 365*nibs[0] + 100*nibs[1] + 10*nibs[2] +nibs[3]
		+ (int)(nibs[0]/4);

	if(sec) *sec = nibs[4]*36000 + nibs[5]*3600 + nibs[6]*600 + nibs[7]*60
		+ nibs[8]*10 + nibs[9] + nibs[10]/10.0 + nibs[11]/100.0
		+ lastdig[(unsigned int)(nibs[12])];
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

	if(mjd) *mjd = nibs[0]*100 + nibs[1]*10 + nibs[2];
	if(sec) *sec = nibs[3]*10000 + nibs[4]*1000 + nibs[5]*100 + nibs[6]*10
	        + nibs[7] + nibs[8]/10.0 + nibs[9]/100.0 + nibs[10]/1000.0
	        + nibs[11]/10000.0;
}

void VLBA_stream_frame_time(const struct VLBA_stream *vs, double *mjd, 
	double *sec)
{
	if(vs->format == FORMAT_VLBA)
		VLBA_stream_frame_time_vlba(vs, mjd, sec);
	else
		VLBA_stream_frame_time_mark4(vs, mjd, sec);
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
		*sec += 64.0*(double)vs->frametime/PAYLOADSIZE;
}

static int read_frame(struct VLBA_stream *vs, uint32_t *dest)
{
	int n;

	n = read(vs->infile, dest, vs->gulpsize);
	while(n < vs->gulpsize) 
	{
		fprintf(stderr, "End of file %s\n", vs->infiles[vs->curfile]);
		close(vs->infile);
		vs->curfile++;
		if(vs->curfile >= vs->nfiles) 
		{
			vs->infile = 0;
			return -1;
		}

		fprintf(stderr, "Continuing to read from file [%d/%d] %s\n", 
			vs->curfile, vs->nfiles,
			vs->infiles[vs->curfile]);
		vs->infile = open64(vs->infiles[vs->curfile], O_RDONLY);
		if(!vs->infile)
		{
			fprintf(stderr, "Cannot open <%s>\n", 
				vs->infiles[vs->curfile]);
			return -1;
		}
		n += read(vs->infile, (char *)(dest)+n, 
			vs->gulpsize-n);
	}

	return n;
}

static int next_frame(struct VLBA_stream *vs)
{
	int i, n;

#ifndef NOVLBATHREADS
	if(vs->cache)
	{
		/* don't bother waiting if all data is read from files */
		if(vs->curfile < vs->nfiles) 
			sem_wait(&vs->cache_rd_sem);

		/* if all data is read from files and cache is empty, then
		   we've reached end of stream */
		if(vs->curfile >= vs->nfiles && vs->cache_rd == vs->cache_wr)
		{
			printf("Already read final frame\n");
			return -1;
		}
		
		/* not at end of stream so allow previous frame to be over
		   written */
		sem_post(&vs->cache_wr_sem);

		vs->frame = vs->cache + vs->cache_rd*vs->gulpsize/4;
		vs->payload = vs->frame + vs->payload_offset;
		vs->cache_rd++;
		if(vs->cache_rd >= vs->cache_size) vs->cache_rd = 0;
		n = vs->gulpsize;

	}
	else n = read_frame(vs, vs->frame);
#else
	n = read_frame(vs, vs->frame);
#endif

	/* are we at end of file(s)? */
	if(n != vs->gulpsize) return -1;

	/* successfully got new frame, so increment it */
	vs->framenum++;

	/* check for sync */
	for(i = 0; i < vs->tracks; i++) 
		if(vs->frame[i] != 0xFFFFFFFF) 
		{
			fprintf(stderr, "Sync not found in frame %d\n",
				vs->framenum);
			return -1;
		}
	
	/* now do the demodulation */
	if(vs->format == FORMAT_VLBA) switch(vs->tracks)
	{
		case 8:
			{ 
				uint8_t *dat;
				dat = (uint8_t *)(vs->payload);
				for(i = vs->firstvalid; i <= vs->lastvalid; i++)
					dat[i] ^= modbits[i];
			}
			break;
		case 16:
			{ 
				uint16_t *dat;
				dat = (uint16_t *)(vs->payload);
				for(i = vs->firstvalid; i <= vs->lastvalid; i++)
					dat[i] ^= modbits[i];
			}
			break;
		case 32:
			{ 
				uint32_t *dat;
				dat = vs->payload;
				for(i = vs->firstvalid; i <= vs->lastvalid; i++)
					dat[i] ^= modbits[i];
			}
			break;
		case 64:
			{ 
				uint64_t *dat;
				dat = (uint64_t *)(vs->payload);
				for(i = vs->firstvalid; i <= vs->lastvalid; i++)
					dat[i] ^= modbits64[i];
			}
			break;
		default:
			return 0;
	}

	return 0;
}

	
static void first_frame(struct VLBA_stream *vs)
{
	lseek(vs->infile, vs->startoffset+vs->fileoffset, SEEK_SET);
	next_frame(vs);
	vs->framenum = 0;
	
#if 0
	/* Not yet complete -- rewind to catch bytes before first frame header */
	lseek(vs->infile, vs->startoffset, SEEK_SET)
	vs->read_position = vs->fileoffset-96;
	vs->framenum = 0;
	A = FRAMESIZE*vs->tracks/8;
	B = vs->fileoffset*vs->tracks/8;
	n = read(vs->infile, (char *)(vs->frame)+(A-B), B);
	if(vs->read_position >= PAYLOADSIZE)
		next_frame(vs);
#endif
}

void VLBA_stream_set_basebits(struct VLBA_stream *vs, int nchan, const int *basebits)
{
	int maxchan, c, delta;

	maxchan = vs->tracks / (vs->bits*vs->fanout);

	if(nchan > maxchan)
	{
		fprintf(stderr, "VLBA_stream_set_basebits : nchan > maxchan (%d)\n",
			maxchan);
		exit(0);
	}

	if(vs->basebits) free(vs->basebits);

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
				vs->basebits[c] = c*delta;
			break;
		case 32:
		case 64:
			delta = 2*vs->bits*vs->fanout;
			for(c = 0; c < nchan; c++)
			{
				if(c < maxchan/2)
					vs->basebits[c] = c*delta;	        /*Evens*/
				else
					vs->basebits[c] = (c-maxchan/2)*delta+1; /*Odds*/
			}
			break;
		default:
			fprintf(stderr, 
				"VLBA_stream_set_basebits : bad number of tracks\n");
			exit(0);
		}
	}
	else
	{
		vs->nchan = nchan;
		vs->basebits = (int *)malloc(nchan*sizeof(int));
		for(c = 0; c < nchan; c++)
			vs->basebits[c] = basebits[c];
	}

}

void VLBA_stream_add_infile(struct VLBA_stream *vs, const char *filename)
{
	strcpy(vs->infiles[vs->nfiles], filename);
	vs->nfiles++;
}

static int VLBA_stream_decode_tracks(struct VLBA_stream *vs)
{
	int bufsize;
	int i, n;

	bufsize = FRAMESIZE+1024;

	vs->frame = (uint32_t *)malloc(8*bufsize);
	lseek(vs->infile, vs->startoffset, SEEK_SET);
	
	n = read(vs->infile, vs->frame, 8*bufsize);
	if(n != 8*bufsize)
	{
		fprintf(stderr, "VLBA_stream_open : file too small\n");
		fprintf(stderr, "%d / %d\n", n, FRAMESIZE);
		fprintf(stderr, "ERRNO %d\n", errno);
		free(vs->frame);
		free(vs);
		return 0;
	}

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
	
	vs->fileoffset = i;
	
	free(vs->frame);

	return vs->tracks;
}

static void VLBA_stream_decode_format(struct VLBA_stream *vs)
{
	int i;
	uint32_t *data;
	int sync;
	
	vs->gulpsize = FRAMESIZE*vs->tracks/8 + 4*vs->tracks;
	vs->frame = (uint32_t *)malloc(vs->gulpsize);
	read(vs->infile, vs->frame, vs->gulpsize);

	data = vs->frame;
	
	sync = 1;
	for(i = 0; i < vs->tracks; i++)
		if(data[i+FRAMESIZE*vs->tracks/32] != 0xFFFFFFFF) sync = 0;
	if(sync)
	{
		vs->format = FORMAT_VLBA;
		vs->gulpsize = FRAMESIZE*vs->tracks/8;
		vs->payload = vs->frame + (3*vs->tracks);
		vs->firstvalid = 0;
		vs->lastvalid = PAYLOADSIZE-1;
	}
	else
	{
	    sync = 1;
	    for(i = 0; i < vs->tracks; i++)
		if(data[i+PAYLOADSIZE*vs->tracks/32] != 0xFFFFFFFF) sync = 0;
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
		
	free(vs->frame);
	vs->frame = (uint32_t *)malloc(vs->gulpsize);
	vs->payload = vs->frame + vs->payload_offset;
}

void VLBA_stream_clear_statecount(struct VLBA_stream *vs)
{
	int c, i;

	for(c = 0; c < 16; c++) for(i = 0; i < 4; i++) 
		vs->statecount[c][i] = 0;
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
			for(i = 0; i < 2; i++) total += vs->statecount[c][i];
			printf("  %2d : %f %f\n", c, 
				vs->statecount[c][1]/total,
				vs->statecount[c][0]/total);
		}
		else
		{
			for(i = 0; i < 4; i++) total += vs->statecount[c][i];
			printf("  %2d : %f %f %f %f\n", c, 
				vs->statecount[c][0]/total,
				vs->statecount[c][2]/total,
				vs->statecount[c][1]/total,
				vs->statecount[c][3]/total);
		}
	}
}

struct VLBA_stream *VLBA_stream_open(const char *filename, 
	int bits, int fanout, long long offset)
{
	struct VLBA_stream *vs;
	int in;
	double dt, sec1;

	if(!modbits) initmodbits();

	in = open64(filename, O_RDONLY);
	if(!in) 
	{
		fprintf(stderr, "Cannot open <%s>\n", filename);
		return 0;
	}

	vs = (struct VLBA_stream *)malloc(sizeof(struct VLBA_stream));
	vs->cache_size = 0;
	vs->cache = 0;
	vs->frame = 0;
	vs->infile = in;
	vs->bits = bits;
	vs->fanout = fanout;
	vs->basebits = 0;
	vs->startoffset = offset;
	
	strcpy(vs->infiles[0], filename);
	vs->curfile = 0;
	vs->nfiles = 1;
	
	if(VLBA_stream_decode_tracks(vs) < 8)
	{
		free(vs);
		fprintf(stderr, "Cannot find sync word\n");
		return 0;
	}

	vs->read_position = 0;

	lseek(in, offset+vs->fileoffset, SEEK_SET);
	VLBA_stream_decode_format(vs);
	if(vs->format == FORMAT_VLBA) ;
//		fprintf(stderr, "Looks like VLBA format\n");
	else if(vs->format == FORMAT_MARK4) ;
//		fprintf(stderr, "Looks like Mark4 format\n");
	else
	{
		free(vs);
		fprintf(stderr, "Unknown format or corrupt data\n");
		return 0;
	}
	
	lseek(in, offset+vs->fileoffset, SEEK_SET);
	read(in, vs->frame, vs->gulpsize);
	
	VLBA_stream_frame_time(vs, &vs->mjd, &vs->sec);
//	fprintf(stderr, "fileoffset = %d\n", vs->fileoffset);
//	fprintf(stderr, "1st frame mjd = %0.0f, sec = %f\n", vs->mjd, vs->sec);
	
	read(in, vs->frame, vs->gulpsize);
	VLBA_stream_frame_time(vs, 0, &sec1);
//	fprintf(stderr, "2nd frame mjd = %0.0f, sec = %f\n", vs->mjd, sec1);

	dt = sec1 - vs->sec;
	if(dt < 0.0) dt += 86400.0;
	vs->frametime = dt; 

	vs->samprate = fanout*PAYLOADSIZE/dt;
	vs->samprate = (int)(vs->samprate+0.5);
//	fprintf(stderr, "sample rate : %f\n", vs->samprate);
//	fprintf(stderr, "ntracks = %d\n", vs->tracks);

	VLBA_stream_clear_statecount(vs);
	
	first_frame(vs);

	VLBA_stream_set_basebits(vs, 0, 0);

	return vs;
}

void VLBA_stream_close(struct VLBA_stream *vs)
{
#ifndef NOVLBATHREADS
	void *status;
#endif

	if(vs)
	{
		if(vs->infile) close(vs->infile);
		if(vs->basebits) free(vs->basebits);
#ifndef NOVLBATHREADS
		if(vs->cache) 
		{
			vs->infile = 0;
			sem_post(&vs->cache_wr_sem);
			fprintf(stderr, "waiting for read thread to end...");
			fflush(stderr);
			pthread_join(vs->readthread, &status);
			fprintf(stderr, "   ...ended.\n");
			free(vs->cache);
			sem_destroy(&vs->cache_wr_sem);
			sem_destroy(&vs->cache_rd_sem);
		}
		else if(vs->frame) free(vs->frame);
#else
		if(vs->frame) free(vs->frame);
#endif
		free(vs);
	}
}

#ifndef NOVLBATHREADS
static int VLBA_stream_read(struct VLBA_stream *vs)
{
	int n;

	n = read_frame(vs, vs->cache + vs->cache_wr*vs->gulpsize/4);
	if(n < 0) 
	{
		sem_post(&vs->cache_rd_sem);
		return -1;
	}

	vs->cache_wr++;
	if(vs->cache_wr >= vs->cache_size)
		vs->cache_wr = 0;
	sem_post(&vs->cache_rd_sem);

	return 1;
}

static void *VLBA_stream_read_thread(void *_vs)
{
	struct VLBA_stream *vs;

	vs = (struct VLBA_stream *)_vs;

	for(;;)
	{
		sem_wait(&vs->cache_wr_sem);
		if(vs->infile == 0)
			pthread_exit(0);
		/* read a frame.  if end of data, close thread */
		if(VLBA_stream_read(vs) < 0) pthread_exit(0);
	}
}

int VLBA_stream_start_cache(struct VLBA_stream *vs, int nframe)
{
	if(vs->cache)
	{
		fprintf(stderr, "VLBA_stream_start_cache: cache already started\n");
		return -1;
	}
	if(!vs->frame)
	{
		fprintf(stderr, "VLBA_stream_start_cache: frame pointer is zero\n");
		return -1;
	}
	if(!vs->infile)
	{
		fprintf(stderr, "VLBA_stream_start_cache: infile pointer is zero\n");
		return -1;
	}
	vs->cache = (uint32_t *)malloc(nframe*vs->gulpsize);
	vs->cache_size = nframe;
	vs->cache_wr = 1;
	vs->cache_rd = 1;
	memcpy(vs->cache, vs->frame, vs->gulpsize);
	free(vs->frame);
	vs->frame = vs->cache;
	vs->payload = vs->frame + vs->payload_offset;

	/* give the cache writing thread headroom to fill cache */
	sem_init(&vs->cache_wr_sem, 0, vs->cache_size-1);

	/* tell cache reading that no new frames are ready yet */
	sem_init(&vs->cache_rd_sem, 0, 0);

	pthread_create(&vs->readthread, 0, VLBA_stream_read_thread, vs);

	return 0;
}
#endif

static int VLBA_stream_get_data_2bit8(struct VLBA_stream *vs, int nsamp, 
	vlbatype **data)
{
	const double lut[4] = {-3.3359, 1.0, -1.0, 3.3359};
	uint8_t *buf, p;
	int i, o, s, c, m, f;
	int index;

	buf = (uint8_t *)(vs->payload);

	o = vs->read_position;

	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
					data[c][i+f] = 0.0;
		}
		else
		{
			p = buf[o];
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+f;
					m = s + vs->fanout;
					index = ((p>>s)&1) + (((p>>m)&1)<<1);
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0) return -1;
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

	o = vs->read_position;

	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
					data[c][i+f] = 0.0;
		}
		else
		{
			p = buf[o];
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+f;
					index = (p>>s)&1;
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0) return -1;
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

	o = vs->read_position;

	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
					data[c][i+f] = 0.0;
		}
		else
		{
			p = buf[o];
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+f;
					m = s + vs->fanout;
					index = ((p>>s)&1) + (((p>>m)&1)<<1);
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0) return -1;
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

	o = vs->read_position;

	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
					data[c][i+f] = 0.0;
		}
		else
		{
			p = buf[o];
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+f;
					index = (p>>s)&1;
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0) return -1;
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

	o = vs->read_position;

	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
					data[c][i+f] = 0.0;
		}
		else
		{
			p = buf[o];
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+2*f;
					m = s + 2*vs->fanout;
					index = ((p>>s)&1) + (((p>>m)&1)<<1);
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0) return -1;
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

	o = vs->read_position;

	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
					data[c][i+f] = 0.0;
		}
		else
		{
			p = buf[o];
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+2*f;
					index = (p>>s)&1;
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0) return -1;
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
	
	o = vs->read_position;
	
	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
					data[c][i+f] = 0.0;
		}
		else
		{
			p = buf[o];
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+2*f;
					m = s + 2*vs->fanout;
					index = ((p>>s)&1) + (((p>>m)&1)<<1);
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0) return -1;
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
	
	o = vs->read_position;
	
	for(i = 0; i < nsamp; i+=vs->fanout)
	{
		if(o < vs->firstvalid || o > vs->lastvalid)
		{
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
					data[c][i+f] = 0.0;
		}
		else
		{
			p = buf[o];
			for(f = 0; f < vs->fanout; f++) 
		    		for(c = 0; c < vs->nchan; c++)
				{
					s = vs->basebits[c]+2*f;
					index = (p>>s)&1;
					data[c][i+f] = lut[index];
					vs->statecount[c][index]++;
				}
		}

		o++;
		if(o >= PAYLOADSIZE)
		{
			if(next_frame(vs) < 0) return -1;
			buf = (uint64_t *)(vs->payload);
			o = 0;
		}
	}

	vs->read_position = o;

	return 0;
}


int VLBA_stream_get_data(struct VLBA_stream *vs, int nsamp, vlbatype **data)
{
	if(vs->bits == 2) switch(vs->tracks)
	{
		case 8 : return VLBA_stream_get_data_2bit8(vs, nsamp, data);
		case 16: return VLBA_stream_get_data_2bit16(vs, nsamp, data);
		case 32: return VLBA_stream_get_data_2bit32(vs, nsamp, data);
		case 64: return VLBA_stream_get_data_2bit64(vs, nsamp, data);
		default: return 0;
	}
	else if(vs->bits == 1) switch(vs->tracks)
	{
		case 8 : return VLBA_stream_get_data_1bit8(vs, nsamp, data);
		case 16: return VLBA_stream_get_data_1bit16(vs, nsamp, data);
		case 32: return VLBA_stream_get_data_1bit32(vs, nsamp, data);
		case 64: return VLBA_stream_get_data_1bit64(vs, nsamp, data);
		default: return 0;
	}
	else 
	{
		fprintf(stderr, "VLBA_stream_get_data: only 1 or 2 bits "
				"allowed now\n");
		return -1;
	}
}

