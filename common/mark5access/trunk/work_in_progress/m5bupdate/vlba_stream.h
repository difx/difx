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

#ifndef __VLBA_STREAM_H__
#define __VLBA_STREAM_H__

/* determine type to unpack data into 
 *
 * compile with -DVLBAFLOAT for 32-bit float.  default otherwise is
 * 64-bit double.
 */
#ifdef VLBAFLOAT
typedef float vlbatype;
#else
typedef double vlbatype;
#endif

/* needed for open64 */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdio.h>
#include <inttypes.h>

#ifdef __cplusplus
extern "C" {
#endif

#define VLBA_FRAMESIZE	20160	/* words for entire VLBA frame of 32 tracks */
#define MARK4_FRAMESIZE	20000	/* words for entire MARK4 frame of 32 tracks */
#define FRAMESIZE	20160	/* deprecated -- use VLBA_FRAMESIZE */
#define PAYLOADSIZE	20000	/* words of payload per frame */

#define FORMAT_VLBA	0
#define FORMAT_MARK4	1
#define FORMAT_MARK5B	2
#define FORMAT_UNKNOWN	999

extern uint32_t *modbits;
extern uint64_t *modbits64;

struct VLBA_stream
{
	double samprate;	/* of de-fanned stream */
	double mjd;		/* of first found frame */
	double sec;		/* of first found frame */
	int frameoffset;
	int fileoffset;		/* deprecated -- use frameoffset */
	int gulpsize;		/* number of bytes in a frame */
	int framenum;
	int read_position;
	int tracks;		/* 8, 16, 32 or 64 */
	int bits;		/* 1 or 2 */
	int fanout;
	int nchan, *basebits;
	int format;
	int firstvalid, lastvalid;	/* window within payload that is ok */
	double frametime;
	uint32_t *frame;
	uint32_t *payload;
	int payload_offset;	/* == payload - frame */
	int statecount[16][4];

	int (*init)(struct VLBA_stream *vs);
	int (*next)(struct VLBA_stream *vs);
	int (*prev)(struct VLBA_stream *vs);
	int (*final)(struct VLBA_stream *vs);
	void *inputdata;
};

struct VLBA_stream_generic
{
	int (*init)(struct VLBA_stream *vs);
	int (*next)(struct VLBA_stream *vs);
	int (*prev)(struct VLBA_stream *vs);
	int (*final)(struct VLBA_stream *vs);
	void *inputdata;
};

struct VLBA_stream *VLBA_stream_generic_open(struct VLBA_stream_generic *V,
	int bits, int fanout);
	
void VLBA_stream_close(struct VLBA_stream *vs);

void VLBA_stream_print(const struct VLBA_stream *vs);

void VLBA_stream_time(const struct VLBA_stream *vs, double *mjd, double *sec);

void VLBA_stream_frame_time(const struct VLBA_stream *vs, double *mjd,
	double *sec);

void VLBA_stream_set_basebits(struct VLBA_stream *vs, int nchan, const int *basebits);

int VLBA_stream_get_data(struct VLBA_stream *vs, int nsamp, vlbatype **data);

void VLBA_stream_clear_statecount(struct VLBA_stream *vs);
void VLBA_stream_print_statecount(struct VLBA_stream *vs);



/* For compatibility, but please use new names (below) */

struct VLBA_stream *VLBA_stream_open(const char *filename, 
	int bits, int fanout, long long offset);

void VLBA_stream_add_infile(struct VLBA_stream *vs, const char *filename);



/* specific types */

struct VLBA_stream *VLBA_stream_memory_open(void *data, unsigned long length,
	int bits, int fanout);

struct VLBA_stream *VLBA_stream_file_open(const char *filename,
	long long offset, int bits, int fanout);

void VLBA_stream_file_add_infile(struct VLBA_stream *vs, const char *filename);


struct VLBA_format
{
	/* provided info */
	int nbit;	/* 1 or 2 */
	int nchan;	/* number of baseband channels */
	int fanout;	/* 1, 2, or 4 */
	int format;	/* 0 == VLBA, 1 = Mark4 */

	/* derived parameters */
	int firstvalid;	/* first valid sample number within payload */
	int lastvalid;	/* last valid sample number within payload */
	int framesize;	/* length of entire frame in bytes */
	int payloadoffset;	/* offset of payload from frame start */
	int tracks;		/* number of tracks in data stream */
	int basebits[32];	/* location of 1st sign bit for each channel */
};

struct VLBA_format *new_VLBA_format(int nbit, int nchan, int fanout,
        int format);

void delete_VLBA_format(struct VLBA_format *vf);	

int VLBA_unpack(const struct VLBA_format *vf, const char *indata, 
	float **data, int nsamp);

void print_VLBA_format(const struct VLBA_format *vf);

#ifdef __cplusplus
}
#endif

#endif
