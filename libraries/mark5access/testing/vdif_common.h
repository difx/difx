/***************************************************************************
 *   Common support for VDIF unpacker testing and benchmarking.            *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 ***************************************************************************/

#ifndef __VDIF_COMMON_H__
#define __VDIF_COMMON_H__

#include <stdint.h>
#include "mark5access/mark5_stream.h"

#define VDIF_HEADER_BYTES	32
#define MAX_VDIF_MODES		1024

/* One unpacker variant in format_vdif.c, identified by (complex, nbit, nchan) */
struct vdif_mode
{
	int iscomplex;
	int nbit;		/* bits per real or per imaginary component */
	int nchan;		/* channels as requested of the library */
	int npad;		/* channels as packed: nchan rounded up to a power of 2 */
	int bitspersample;	/* nbit, or 2*nbit for complex */
	int blockbits;		/* bits holding one time sample of all (padded) channels */
	int hascount;		/* library provides a high-state counter for this mode */
	char name[32];		/* e.g. "real-2bit-16ch" or "cplx-8bit-4ch" */
};

/* Fill in derived fields of a mode from iscomplex, nbit, nchan */
void vdif_mode_init(struct vdif_mode *m, int iscomplex, int nbit, int nchan);

/* Probe the library for every supported mode; returns number found.
 * Library error chatter is suppressed during probing. */
int vdif_enumerate_modes(struct vdif_mode *modes, int maxmodes);

/* Select modes whose name matches a comma separated list of fnmatch()
 * patterns.  Returns new count; modes array is compacted in place. */
int vdif_filter_modes(struct vdif_mode *modes, int nmodes, const char *patterns);

struct mark5_format_generic *vdif_mode_new_format(const struct vdif_mode *m, int databytes, int framesperperiod);

/* Number of samples per channel per frame */
int vdif_mode_framesamples(const struct vdif_mode *m, int databytes);

/* Number of samples the decoder produces per inner-loop iteration (one byte
 * or one sample block, whichever is larger in samples) */
int vdif_mode_granularity(const struct vdif_mode *m);

/* Smallest legal payload size for this mode (multiple of 8 bytes and of a sample block) */
int vdif_mode_min_databytes(const struct vdif_mode *m);

/* Random numbers: splitmix64 */
uint64_t rng_next(uint64_t *state);

enum frame_kind
{
	FRAME_GOOD = 0,
	FRAME_INVALID = 1,	/* invalid bit set in header */
	FRAME_FILL = 2		/* payload starts with Mark5 fill pattern */
};

/* Generate nframes consecutive VDIF frames (32 byte header + databytes payload)
 * with random payload. kinds may be 0 (all good). */
void vdif_generate_frames(unsigned char *buf, const struct vdif_mode *m, int databytes,
	int nframes, int framesperperiod, const enum frame_kind *kinds, uint64_t *rng);

/* Reference model: independent decode of one sample straight from the VDIF
 * bit layout.  Payload is treated as a little-endian bitstream; channel c of
 * time sample s starts at bit (s*npad + c)*bitspersample, real part first. */
void ref_init(void);
float ref_value(uint32_t code, int nbit);
uint32_t ref_extract(const unsigned char *payload, long long bitpos, int nbit);
void ref_sample(const struct vdif_mode *m, const unsigned char *payload, int s, int c, float *re, float *im);

/* 1 if this 2-bit code is a "high" state (magnitude > 1) */
int ref_is_high_2bit(uint32_t code);

double now_seconds(void);

#endif
