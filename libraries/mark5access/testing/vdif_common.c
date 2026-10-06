/***************************************************************************
 *   Common support for VDIF unpacker testing and benchmarking.            *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fnmatch.h>
#include <time.h>
#include "vdif_common.h"

/* Values the VDIF decoders are expected to produce.  These are written out
 * independently of format_vdif.c so that a change in the library's lookup
 * tables or arithmetic is detected. */
static float ref2bit[4];
static float ref4bit[16];
static float ref8bit[256];

static const int probe_nbits[] = { 1, 2, 4, 8, 16, 32 };
#define N_PROBE_NBITS ((int)(sizeof(probe_nbits)/sizeof(probe_nbits[0])))
#define MAX_PROBE_NCHAN 64

void ref_init(void)
{
	const float HiMag = OPTIMAL_2BIT_HIGH;
	const float FourBit1sigma = 2.95;
	double offset8bit = 128.0;
	int i;

	if(getenv("SYMMETRIC_8_BIT_VDIF") != 0)
	{
		offset8bit = 127.5;
	}

	ref2bit[0] = -HiMag;
	ref2bit[1] = -1.0;
	ref2bit[2] = 1.0;
	ref2bit[3] = HiMag;

	for(i = 0; i < 16; ++i)
	{
		ref4bit[i] = (i - 7.5)/FourBit1sigma;
	}

	for(i = 0; i < 256; ++i)
	{
		ref8bit[i] = (i - offset8bit)/3.3;
	}
}

float ref_value(uint32_t code, int nbit)
{
	switch(nbit)
	{
	case 1:
		return code ? 1.0f : -1.0f;
	case 2:
		return ref2bit[code];
	case 4:
		return ref4bit[code];
	case 8:
		return ref8bit[code];
	case 16:
		return ((int)code - 32768)/8.0;
	case 32:
		return code/8.0 - (1<<28);
	}

	return 0.0f;
}

int ref_is_high_2bit(uint32_t code)
{
	return code == 0 || code == 3;
}

uint32_t ref_extract(const unsigned char *payload, long long bitpos, int nbit)
{
	const unsigned char *p = payload + bitpos/8;

	switch(nbit)
	{
	case 1:
	case 2:
	case 4:
		return (p[0] >> (bitpos % 8)) & ((1u << nbit) - 1);
	case 8:
		return p[0];
	case 16:
		return p[0] | ((uint32_t)p[1] << 8);
	case 32:
		return p[0] | ((uint32_t)p[1] << 8) | ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
	}

	return 0;
}

void ref_sample(const struct vdif_mode *m, const unsigned char *payload, int s, int c, float *re, float *im)
{
	long long bitpos = ((long long)s*m->npad + c)*m->bitspersample;

	*re = ref_value(ref_extract(payload, bitpos, m->nbit), m->nbit);
	if(m->iscomplex)
	{
		*im = ref_value(ref_extract(payload, bitpos + m->nbit, m->nbit), m->nbit);
	}
	else
	{
		*im = 0.0f;
	}
}

void vdif_mode_init(struct vdif_mode *m, int iscomplex, int nbit, int nchan)
{
	memset(m, 0, sizeof(*m));
	m->iscomplex = iscomplex;
	m->nbit = nbit;
	m->nchan = nchan;
	for(m->npad = 1; m->npad < nchan; m->npad *= 2)
	{
	}
	m->bitspersample = iscomplex ? 2*nbit : nbit;
	m->blockbits = m->npad*m->bitspersample;
	snprintf(m->name, sizeof(m->name), "%s-%dbit-%dch", iscomplex ? "cplx" : "real", nbit, nchan);
}

int vdif_mode_framesamples(const struct vdif_mode *m, int databytes)
{
	return (int)((long long)databytes*8/m->blockbits);
}

int vdif_mode_granularity(const struct vdif_mode *m)
{
	return m->blockbits >= 8 ? 1 : 8/m->blockbits;
}

int vdif_mode_min_databytes(const struct vdif_mode *m)
{
	int blockbytes = (m->blockbits + 7)/8;

	return blockbytes > 8 ? blockbytes : 8;
}

struct mark5_format_generic *vdif_mode_new_format(const struct vdif_mode *m, int databytes, int framesperperiod)
{
	return new_mark5_format_generalized_vdif(framesperperiod, 1, m->nchan, m->nbit, 1,
		databytes, VDIF_HEADER_BYTES, m->iscomplex);
}

int vdif_enumerate_modes(struct vdif_mode *modes, int maxmodes)
{
	FILE *devnull, *olderr = 0;
	int n = 0;
	int cplx, b, nchan;

	/* the library complains about every unsupported mode; silence it */
	devnull = fopen("/dev/null", "w");
	if(devnull)
	{
		mark5_library_getoption(M5A_OPT_STDERRFD, &olderr);
		mark5_library_setoption(M5A_OPT_STDERRFD, devnull);
	}

	for(cplx = 0; cplx <= 1; ++cplx)
	{
		for(b = 0; b < N_PROBE_NBITS; ++b)
		{
			for(nchan = 1; nchan <= MAX_PROBE_NCHAN && n < maxmodes; ++nchan)
			{
				struct vdif_mode m;
				struct mark5_format_generic *f;

				vdif_mode_init(&m, cplx, probe_nbits[b], nchan);
				f = vdif_mode_new_format(&m, 8000, 1000);
				if(f)
				{
					m.hascount = (f->count != 0);
					modes[n++] = m;
					delete_mark5_format_generic(f);
				}
			}
		}
	}

	if(devnull)
	{
		mark5_library_setoption(M5A_OPT_STDERRFD, olderr);
		fclose(devnull);
	}

	return n;
}

static int match_any(const char *name, const char *patterns)
{
	char buf[1024];
	char *tok, *save = 0;

	snprintf(buf, sizeof(buf), "%s", patterns);
	for(tok = strtok_r(buf, ",", &save); tok; tok = strtok_r(0, ",", &save))
	{
		if(fnmatch(tok, name, 0) == 0)
		{
			return 1;
		}
	}

	return 0;
}

int vdif_filter_modes(struct vdif_mode *modes, int nmodes, const char *patterns)
{
	int i, n = 0;

	if(!patterns || !patterns[0])
	{
		return nmodes;
	}
	for(i = 0; i < nmodes; ++i)
	{
		if(match_any(modes[i].name, patterns))
		{
			modes[n++] = modes[i];
		}
	}

	return n;
}

uint64_t rng_next(uint64_t *state)
{
	uint64_t z = (*state += 0x9E3779B97F4A7C15ULL);

	z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
	z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;

	return z ^ (z >> 31);
}

static void put_le32(unsigned char *p, uint32_t v)
{
	p[0] = v & 0xFF;
	p[1] = (v >> 8) & 0xFF;
	p[2] = (v >> 16) & 0xFF;
	p[3] = (v >> 24) & 0xFF;
}

void vdif_generate_frames(unsigned char *buf, const struct vdif_mode *m, int databytes,
	int nframes, int framesperperiod, const enum frame_kind *kinds, uint64_t *rng)
{
	static const unsigned char fill[8] = { 0x44, 0x33, 0x22, 0x11, 0x44, 0x33, 0x22, 0x11 };
	const uint32_t refepoch = 48;		/* 2024 Jan 1 */
	const uint32_t startsec = 1234567;
	int framebytes = databytes + VDIF_HEADER_BYTES;
	int log2nchan = 0;
	int f, i;

	while((1 << log2nchan) < m->npad)
	{
		++log2nchan;
	}

	for(f = 0; f < nframes; ++f)
	{
		unsigned char *frame = buf + (size_t)f*framebytes;
		unsigned char *payload = frame + VDIF_HEADER_BYTES;
		enum frame_kind kind = kinds ? kinds[f] : FRAME_GOOD;
		uint32_t w0, w1, w2, w3;

		w0 = (startsec + f/framesperperiod) & 0x3FFFFFFF;
		if(kind == FRAME_INVALID)
		{
			w0 |= 1u << 31;
		}
		w1 = (refepoch << 24) | (f % framesperperiod);
		w2 = (1u << 29) | ((uint32_t)log2nchan << 24) | (framebytes/8);
		w3 = (m->iscomplex ? (1u << 31) : 0) | ((uint32_t)(m->nbit - 1) << 26);

		memset(frame, 0, VDIF_HEADER_BYTES);
		put_le32(frame + 0, w0);
		put_le32(frame + 4, w1);
		put_le32(frame + 8, w2);
		put_le32(frame + 12, w3);

		for(i = 0; i < databytes; i += 8)
		{
			uint64_t r = rng_next(rng);
			int k;

			for(k = 0; k < 8 && i + k < databytes; ++k)
			{
				payload[i + k] = (r >> (8*k)) & 0xFF;
			}
		}

		if(kind == FRAME_FILL)
		{
			memcpy(payload, fill, 8);
		}
		else
		{
			/* random data must not accidentally look like fill pattern */
			if(memcmp(payload, fill, 8) == 0)
			{
				payload[0] ^= 0x01;
			}
			if(memcmp(payload + databytes - 8, fill, 8) == 0)
			{
				payload[databytes - 8] ^= 0x01;
			}
		}
	}
}

double now_seconds(void)
{
	struct timespec ts;

	clock_gettime(CLOCK_MONOTONIC, &ts);

	return ts.tv_sec + 1.0e-9*ts.tv_nsec;
}
