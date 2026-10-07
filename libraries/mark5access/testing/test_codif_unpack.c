/***************************************************************************
 *   Correctness tests for the CODIF unpackers in format_codif.c           *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 ***************************************************************************/

/* Every (real/complex, nbit, nchan) combination that the library accepts is
 * tested against an independent reference decoder through the public
 * mark5access API, as test_vdif_unpack does for VDIF:
 *
 *   meta    framesamples and samplegranularity the library derives
 *   stream  memory stream + mark5_stream_decode[_complex] in random chunks
 *   blank   as stream, with frames flagged invalid or holding fill pattern
 *   unpack  unpacker stream + mark5_unpack[_complex]_with_offset, good and
 *           bad frames (the mpifxcorr code path)
 *   count   mark5_stream_count_high_states (where the library provides it)
 *
 * CODIF packs channels densely (no power of 2 requirement): channel c of time
 * sample s starts at bit (s*nchan + c)*bitspersample of the payload, read as
 * a little-endian bitstream, real part first for complex data.  Values are
 * two's complement (the only representation the library supports):
 *   1 bit: 0 -> +1, 1 -> -1          2 bit: {+1, +HiMag, -HiMag, -1}
 *   4 bit: k/2.95 for k = -8..7     8 bit: int8/71
 *   16 bit: int16/8                 32 bit: IEEE float
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <codifio.h>
#include "vdif_common.h"	/* rng_next(), ref_extract(), mode filtering helpers */

#define GUARD		64
#define FPS		10000

static const uint32_t GUARD_BITS = 0x7FC0DEADu;	/* a NaN */

struct codif_mode
{
	int iscomplex;
	int nbit;
	int nchan;
	int parts;		/* 1 real, 2 complex */
	int samplebits;		/* bits per time sample, all channels */
	int granularity;	/* samples per whole number of bytes */
	int hascount;
	char name[32];
};

struct options
{
	const char *patterns;
	uint64_t seed;
	int verbose;
	int maxreport;
};

struct testdata
{
	const struct codif_mode *m;
	int databytes;
	int framebytes;
	int framesamples;
	int nframes;
	int nalloc;
	unsigned char *buf;
	int *bad;		/* 0 good, 1 invalid flag, 2 fill pattern */
	long long nsamp;
	float **refre;
	float **refim;
};

struct result
{
	long long nchecked;
	long long nbad;
	char first[256];
};

static void usage(const char *pgm)
{
	printf("Usage: %s [options]\n\n", pgm);
	printf("Tests every CODIF unpacker in format_codif.c against a reference decoder.\n\n");
	printf("Options:\n");
	printf("  -m <patterns>  only test modes matching comma separated glob patterns\n");
	printf("  -l             list the supported modes and exit\n");
	printf("  -s <seed>      random seed [default 1]\n");
	printf("  -r <n>         report up to n mismatches per failing test [default 1]\n");
	printf("  -x             strict: known library bugs count as failures\n");
	printf("  -v             verbose\n");
	printf("  -h             this help\n");
}

static void result_fail(struct result *r, const struct options *o, const char *fmt, ...)
	__attribute__((format(printf, 3, 4)));

static void result_fail(struct result *r, const struct options *o, const char *fmt, ...)
{
	char msg[256];
	va_list ap;

	va_start(ap, fmt);
	vsnprintf(msg, sizeof(msg), fmt, ap);
	va_end(ap);

	if(r->nbad == 0)
	{
		snprintf(r->first, sizeof(r->first), "%s", msg);
	}
	if(r->nbad < o->maxreport && o->maxreport > 1)
	{
		printf("      %s\n", msg);
	}
	++r->nbad;
}

static float ref_codif_value(uint32_t code, int nbit)
{
	const float HiMag = OPTIMAL_2BIT_HIGH;
	const float FourBit1sigma = 2.95;
	const float lut4level[4] = {1.0, HiMag, -HiMag, -1.0};

	switch(nbit)
	{
	case 1:
		return code ? -1.0f : 1.0f;
	case 2:
		return lut4level[code];
	case 4:
		return (code < 8 ? (int)code : (int)code - 16)/FourBit1sigma;	/* float division */
	case 8:
		return (float)((int8_t)code)/71.0;
	case 16:
		return ((int16_t)code)/8.0;
	case 32:
		{
			float f;

			memcpy(&f, &code, 4);

			return f;
		}
	}

	return 0.0f;
}

static int gcd(int a, int b)
{
	while(b)
	{
		int t = a % b;

		a = b;
		b = t;
	}

	return a;
}

static void codif_mode_init(struct codif_mode *m, int iscomplex, int nbit, int nchan)
{
	memset(m, 0, sizeof(*m));
	m->iscomplex = iscomplex;
	m->nbit = nbit;
	m->nchan = nchan;
	m->parts = iscomplex ? 2 : 1;
	m->samplebits = nchan*nbit*m->parts;
	m->granularity = 8/gcd(8, m->samplebits);
	snprintf(m->name, sizeof(m->name), "%s-%dbit-%dch", iscomplex ? "cplx" : "real", nbit, nchan);
}

static int enumerate_modes(struct codif_mode *modes, int maxmodes)
{
	static const int nbits[] = { 1, 2, 4, 8, 16, 32 };
	FILE *devnull, *olderr = 0;
	int n = 0, cplx, b, nchan;

	devnull = fopen("/dev/null", "w");
	if(devnull)
	{
		mark5_library_getoption(M5A_OPT_STDERRFD, &olderr);
		mark5_library_setoption(M5A_OPT_STDERRFD, devnull);
	}
	for(cplx = 0; cplx <= 1; ++cplx)
	{
		for(b = 0; b < 6; ++b)
		{
			for(nchan = 1; nchan <= 64 && n < maxmodes; ++nchan)
			{
				struct codif_mode m;
				struct mark5_format_generic *f;

				codif_mode_init(&m, cplx, nbits[b], nchan);
				f = new_mark5_format_codif(FPS, 1, nchan, nbits[b], 1, 8000, 64, cplx);
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

static int filter_modes(struct codif_mode *modes, int nmodes, const char *patterns)
{
	struct vdif_mode tmp;
	int i, n = 0;

	if(!patterns || !patterns[0])
	{
		return nmodes;
	}
	for(i = 0; i < nmodes; ++i)
	{
		/* reuse the VDIF glob matcher on the name */
		memset(&tmp, 0, sizeof(tmp));
		snprintf(tmp.name, sizeof(tmp.name), "%s", modes[i].name);
		if(vdif_filter_modes(&tmp, 1, patterns) == 1)
		{
			modes[n++] = modes[i];
		}
	}

	return n;
}

/* Payload size near target: a multiple of 8 bytes holding whole samples */
static int legal_databytes(const struct codif_mode *m, int target)
{
	int d;

	for(d = target; ; d += 8)
	{
		if((d*8) % m->samplebits == 0)
		{
			return d;
		}
	}
}

static void free_testdata(struct testdata *td)
{
	int c;

	if(td->refre)
	{
		for(c = 0; c < td->m->nchan; ++c)
		{
			free(td->refre[c]);
			free(td->refim[c]);
		}
	}
	free(td->refre);
	free(td->refim);
	free(td->buf);
	free(td->bad);
	memset(td, 0, sizeof(*td));
}

static void make_testdata(struct testdata *td, const struct codif_mode *m, int databytes, int withbad, uint64_t *rng)
{
	static const unsigned char fill[8] = { 0x44, 0x33, 0x22, 0x11, 0x44, 0x33, 0x22, 0x11 };
	uint64_t totalsamples;
	int f, c, i, s;

	memset(td, 0, sizeof(*td));
	td->m = m;
	td->databytes = databytes;
	td->framebytes = databytes + CODIF_HEADER_BYTES;
	td->framesamples = databytes*8/m->samplebits;
	td->nframes = 6;
	while((long long)td->nframes*td->framesamples < 4096 && td->nframes < 4096)
	{
		++td->nframes;
	}
	td->nalloc = td->nframes + 1;
	td->nsamp = (long long)td->nframes*td->framesamples;
	totalsamples = (uint64_t)td->framesamples*FPS;

	td->bad = (int *)calloc(td->nalloc, sizeof(int));
	if(withbad)
	{
		for(f = 1; f < td->nframes; ++f)
		{
			td->bad[f] = (f % 5 == 2) ? 1 : (f % 5 == 4) ? 2 : 0;
		}
	}

	td->buf = (unsigned char *)malloc((size_t)td->nalloc*td->framebytes);
	for(f = 0; f < td->nalloc; ++f)
	{
		codif_header *h = (codif_header *)(td->buf + (size_t)f*td->framebytes);
		unsigned char *p = td->buf + (size_t)f*td->framebytes + CODIF_HEADER_BYTES;
		char station[3] = "Xx";

		createCODIFHeader(h, databytes, 0, 0, m->nbit, m->nchan, 1, 1, totalsamples, m->iscomplex, station, 0);
		h->epoch = 48;
		h->seconds = 1234567 + f/FPS;
		h->frame = f % FPS;
		h->invalid = (td->bad[f] == 1);
		for(i = 0; i < databytes; i += 8)
		{
			uint64_t r = rng_next(rng);

			memcpy(p + i, &r, databytes - i >= 8 ? 8 : databytes - i);
		}
		if(m->nbit == 32)
		{
			/* finite floats, so that comparisons are meaningful */
			for(i = 0; i + 4 <= databytes; i += 4)
			{
				float v = (float)((int32_t)(rng_next(rng) & 0xFFFFF) - 0x80000)/64.0f;

				memcpy(p + i, &v, 4);
			}
		}
		if(td->bad[f] == 2)
		{
			memcpy(p, fill, 8);
		}
		else
		{
			if(memcmp(p, fill, 8) == 0)
			{
				p[0] ^= 0x01;
			}
			if(memcmp(p + databytes - 8, fill, 8) == 0)
			{
				p[databytes - 8] ^= 0x01;
			}
		}
	}

	td->refre = (float **)calloc(m->nchan, sizeof(float *));
	td->refim = (float **)calloc(m->nchan, sizeof(float *));
	for(c = 0; c < m->nchan; ++c)
	{
		td->refre[c] = (float *)malloc(td->nsamp*sizeof(float));
		td->refim[c] = (float *)malloc(td->nsamp*sizeof(float));
	}
	for(f = 0; f < td->nframes; ++f)
	{
		const unsigned char *p = td->buf + (size_t)f*td->framebytes + CODIF_HEADER_BYTES;

		for(s = 0; s < td->framesamples; ++s)
		{
			long long k = (long long)f*td->framesamples + s;

			for(c = 0; c < m->nchan; ++c)
			{
				long long bit = ((long long)s*m->nchan + c)*m->nbit*m->parts;

				if(td->bad[f])
				{
					td->refre[c][k] = td->refim[c][k] = 0.0f;
					continue;
				}
				td->refre[c][k] = ref_codif_value(ref_extract(p, bit, m->nbit), m->nbit);
				td->refim[c][k] = m->iscomplex ? ref_codif_value(ref_extract(p, bit + m->nbit, m->nbit), m->nbit) : 0.0f;
			}
		}
	}
}

static long long good_samples(const struct testdata *td, long long start, long long n)
{
	long long s, good = 0;

	for(s = start; s < start + n; ++s)
	{
		good += (td->bad[s/td->framesamples] == 0);
	}

	return good;
}

static float **alloc_output(const struct codif_mode *m, long long nsamp)
{
	long long n = (nsamp + GUARD)*m->parts, i;
	float **out;
	int c;

	out = (float **)calloc(m->nchan, sizeof(float *));
	for(c = 0; c < m->nchan; ++c)
	{
		out[c] = (float *)malloc(n*sizeof(float));
		for(i = 0; i < n; ++i)
		{
			memcpy(out[c] + i, &GUARD_BITS, 4);
		}
	}

	return out;
}

static void free_output(const struct codif_mode *m, float **out)
{
	int c;

	for(c = 0; c < m->nchan; ++c)
	{
		free(out[c]);
	}
	free(out);
}

static void compare_output(const struct testdata *td, float **out, long long refstart, long long n,
	const char *label, const struct options *o, struct result *r)
{
	const struct codif_mode *m = td->m;
	long long i, g;
	int c;

	for(c = 0; c < m->nchan; ++c)
	{
		for(i = 0; i < n; ++i)
		{
			long long s = refstart + i;
			float re = out[c][m->parts*i];
			float im = m->iscomplex ? out[c][m->parts*i + 1] : 0.0f;

			++r->nchecked;
			if(memcmp(&re, &td->refre[c][s], 4) != 0 || (m->iscomplex && memcmp(&im, &td->refim[c][s], 4) != 0))
			{
				result_fail(r, o, "%s: sample %lld (frame %lld, +%lld) chan %d: got (%g,%g) expected (%g,%g)",
					label, s, s/td->framesamples, s%td->framesamples, c, re, im, td->refre[c][s], td->refim[c][s]);
			}
		}
		for(g = m->parts*n; g < m->parts*(n + GUARD); ++g)
		{
			if(memcmp(out[c] + g, &GUARD_BITS, 4) != 0)
			{
				result_fail(r, o, "%s: chan %d: output written beyond requested %lld samples", label, c, n);
				break;
			}
		}
	}
}

static long long chunk_length(const struct testdata *td, long long remaining, int gran, uint64_t *rng)
{
	long long n = gran*(1 + (long long)(rng_next(rng) % (3LL*td->framesamples/gran + 2)));

	return n > remaining ? remaining : n;
}

/* the library prints a line per bad frame; keep test output readable */
static FILE *quiet_on(void)
{
	static FILE *devnull = 0;
	FILE *olderr = 0;

	if(!devnull)
	{
		devnull = fopen("/dev/null", "w");
	}
	mark5_library_getoption(M5A_OPT_STDERRFD, &olderr);
	if(devnull)
	{
		mark5_library_setoption(M5A_OPT_STDERRFD, devnull);
	}

	return olderr;
}

static void quiet_off(FILE *olderr)
{
	mark5_library_setoption(M5A_OPT_STDERRFD, olderr);
}

static struct mark5_stream *open_memory_stream(const struct testdata *td)
{
	const struct codif_mode *m = td->m;
	struct mark5_stream *ms;
	FILE *olderr = quiet_on();

	ms = new_mark5_stream_absorb(
		new_mark5_stream_memory(td->buf, (unsigned int)((size_t)td->nalloc*td->framebytes)),
		new_mark5_format_codif(FPS, 1, m->nchan, m->nbit, 1, td->databytes, CODIF_HEADER_BYTES, m->iscomplex));
	quiet_off(olderr);

	return ms;
}

static void test_stream(const struct testdata *td, const char *label, const struct options *o,
	uint64_t *rng, struct result *r, struct result *rmeta)
{
	const struct codif_mode *m = td->m;
	struct mark5_stream *ms;
	float **out, **ptr;
	long long pos = 0;
	int gran = m->granularity, c, first = 1;

	ms = open_memory_stream(td);
	if(!ms)
	{
		result_fail(r, o, "%s: could not create memory stream", label);
		return;
	}
	++rmeta->nchecked;
	if(ms->framesamples != td->framesamples)
	{
		result_fail(rmeta, o, "%s: library framesamples = %d, expected %d", label, ms->framesamples, td->framesamples);
	}
	if(ms->samplegranularity != gran)
	{
		result_fail(rmeta, o, "%s: library samplegranularity = %d, expected %d", label, ms->samplegranularity, gran);
	}

	out = alloc_output(m, td->nsamp);
	ptr = (float **)calloc(m->nchan, sizeof(float *));
	while(pos < td->nsamp)
	{
		long long n = first ? gran : chunk_length(td, td->nsamp - pos, gran, rng);
		long long expect = good_samples(td, pos, n);
		FILE *olderr;
		int ret;

		first = 0;
		for(c = 0; c < m->nchan; ++c)
		{
			ptr[c] = out[c] + m->parts*pos;
		}
		olderr = quiet_on();
		ret = m->iscomplex ? mark5_stream_decode_complex(ms, (int)n, (mark5_float_complex **)ptr) :
			mark5_stream_decode(ms, (int)n, ptr);
		quiet_off(olderr);
		if(ret != expect)
		{
			result_fail(r, o, "%s: decode of %lld samples at %lld returned %d, expected %lld", label, n, pos, ret, expect);
			if(ret < 0)
			{
				break;
			}
		}
		pos += n;
	}
	if(pos == td->nsamp)
	{
		compare_output(td, out, 0, td->nsamp, label, o, r);
	}
	free(ptr);
	free_output(m, out);
	delete_mark5_stream(ms);
}

static void test_unpack(const struct testdata *td, const char *label, const struct options *o,
	uint64_t *rng, struct result *r)
{
	const struct codif_mode *m = td->m;
	struct mark5_stream *ms;
	long long fs = td->framesamples, cases[16][2];
	int gran = m->granularity, ncase = 0, k;

	ms = new_mark5_stream_absorb(new_mark5_stream_unpacker(0),
		new_mark5_format_codif(FPS, 1, m->nchan, m->nbit, 1, td->databytes, CODIF_HEADER_BYTES, m->iscomplex));
	if(!ms)
	{
		result_fail(r, o, "%s: could not create unpacker stream", label);
		return;
	}

	cases[ncase][0] = 0;            cases[ncase][1] = td->nsamp;  ++ncase;
	cases[ncase][0] = gran;         cases[ncase][1] = fs;         ++ncase;
	cases[ncase][0] = fs - gran;    cases[ncase][1] = 2*gran;     ++ncase;
	cases[ncase][0] = 2*fs;         cases[ncase][1] = fs;         ++ncase;
	cases[ncase][0] = 3*fs + gran;  cases[ncase][1] = 2*fs;       ++ncase;
	while(ncase < 16)
	{
		long long off = gran*(long long)(rng_next(rng) % (td->nsamp/gran));

		cases[ncase][0] = off;
		cases[ncase][1] = chunk_length(td, td->nsamp - off, gran, rng);
		++ncase;
	}

	for(k = 0; k < ncase; ++k)
	{
		long long off = cases[k][0], n = cases[k][1], expect;
		char sublabel[96];
		float **out;
		FILE *olderr;
		int ret;

		if(n <= 0 || off + n > td->nsamp)
		{
			continue;
		}
		snprintf(sublabel, sizeof(sublabel), "%s off=%lld n=%lld", label, off, n);
		out = alloc_output(m, td->nsamp);
		olderr = quiet_on();
		ret = m->iscomplex ? mark5_unpack_complex_with_offset(ms, td->buf, (int)off, (mark5_float_complex **)out, (int)n) :
			mark5_unpack_with_offset(ms, td->buf, (int)off, out, (int)n);
		quiet_off(olderr);
		expect = good_samples(td, off, n);
		if(ret != expect)
		{
			result_fail(r, o, "%s: returned %d, expected %lld", sublabel, ret, expect);
		}
		compare_output(td, out, off, n, sublabel, o, r);
		free_output(m, out);
	}
	delete_mark5_stream(ms);
}

static void test_count(const struct testdata *td, const char *label, const struct options *o,
	uint64_t *rng, struct result *r)
{
	const struct codif_mode *m = td->m;
	struct mark5_stream *ms;
	unsigned int *got;
	unsigned long long *expect;
	long long pos = 0, s;
	int c, f;

	ms = open_memory_stream(td);
	if(!ms)
	{
		result_fail(r, o, "%s: could not create memory stream", label);
		return;
	}
	got = (unsigned int *)calloc(m->nchan, sizeof(unsigned int));
	expect = (unsigned long long *)calloc(m->nchan, sizeof(unsigned long long));
	for(f = 0; f < td->nframes; ++f)
	{
		const unsigned char *p = td->buf + (size_t)f*td->framebytes + CODIF_HEADER_BYTES;

		if(td->bad[f])
		{
			continue;
		}
		for(s = 0; s < td->framesamples; ++s)
		{
			for(c = 0; c < m->nchan; ++c)
			{
				uint32_t code = ref_extract(p, (s*m->nchan + c)*m->nbit, 2);

				expect[c] += (fabsf(ref_codif_value(code, 2)) > 1.1f);
			}
		}
	}
	while(pos < td->nsamp)
	{
		long long n = chunk_length(td, td->nsamp - pos, m->granularity, rng);
		long long e = good_samples(td, pos, n);
		FILE *olderr = quiet_on();
		int ret = mark5_stream_count_high_states(ms, (int)n, got);

		quiet_off(olderr);
		if(ret != e)
		{
			result_fail(r, o, "%s: count of %lld samples at %lld returned %d, expected %lld", label, n, pos, ret, e);
			if(ret < 0)
			{
				break;
			}
		}
		pos += n;
	}
	for(c = 0; c < m->nchan; ++c)
	{
		++r->nchecked;
		if(got[c] != expect[c])
		{
			result_fail(r, o, "%s: chan %d high states %u, expected %llu", label, c, got[c], expect[c]);
		}
	}
	free(got);
	free(expect);
	delete_mark5_stream(ms);
}

/* Format detection from data, which uses find_codif_frame() and
 * get_codif_threads(): junk bytes, then frames from nthread interleaved threads.
 * Returns number of failures. */
static int test_detect(int iscomplex, int nbit, int nchan, int nthread, int junk, uint64_t *rng)
{
	struct codif_mode m;
	struct mark5_format *mf;
	unsigned char *buf;
	char station[3] = "Xx";
	int databytes, framebytes, nframes = 40, f, i, nfail = 0;
	size_t len;
	uint64_t totalsamples;
	FILE *olderr;

	codif_mode_init(&m, iscomplex, nbit, nchan);
	databytes = legal_databytes(&m, 8000);
	framebytes = databytes + CODIF_HEADER_BYTES;
	totalsamples = (uint64_t)(databytes*8/m.samplebits)*FPS;
	len = junk + (size_t)nframes*framebytes;
	buf = (unsigned char *)malloc(len);
	for(i = 0; i < junk; ++i)
	{
		buf[i] = rng_next(rng) & 0xFF;
	}
	for(f = 0; f < nframes; ++f)
	{
		codif_header *h = (codif_header *)(buf + junk + (size_t)f*framebytes);
		unsigned char *p = (unsigned char *)h + CODIF_HEADER_BYTES;

		createCODIFHeader(h, databytes, 100 + f % nthread, 0, nbit, nchan, 1, 1, totalsamples, iscomplex, station, 0);
		h->epoch = 48;
		h->seconds = 1234567;
		h->frame = f/nthread;
		for(i = 0; i < databytes; ++i)
		{
			p[i] = rng_next(rng) & 0xFF;
		}
	}

	{
		/* other formats' detection attempts print to stdout too */
		FILE *oldout = 0;
		FILE *devnull = fopen("/dev/null", "w");

		mark5_library_getoption(M5A_OPT_STDOUTFD, &oldout);
		if(devnull)
		{
			mark5_library_setoption(M5A_OPT_STDOUTFD, devnull);
		}
		olderr = quiet_on();
		mf = new_mark5_format_from_stream(new_mark5_stream_memory(buf, (unsigned int)len));
		quiet_off(olderr);
		mark5_library_setoption(M5A_OPT_STDOUTFD, oldout);
		if(devnull)
		{
			fclose(devnull);
		}
	}
	printf("detect %s, %d threads, %d junk bytes: ", m.name, nthread, junk);
	if(!mf)
	{
		printf("FAIL: not detected\n");
		++nfail;
	}
	else
	{
		if(mf->format != MK5_FORMAT_CODIF || mf->nchan != nchan || mf->nbit != nbit ||
		   mf->framebytes != framebytes || mf->frameoffset != junk || mf->ntrack != nthread)
		{
			printf("FAIL: format %d nchan %d nbit %d framebytes %d frameoffset %d threads %d; expected %d %d %d %d %d %d\n",
				mf->format, mf->nchan, mf->nbit, mf->framebytes, mf->frameoffset, mf->ntrack,
				MK5_FORMAT_CODIF, nchan, nbit, framebytes, junk, nthread);
			++nfail;
		}
		else
		{
			printf("pass\n");
		}
		delete_mark5_format(mf);
	}
	free(buf);

	return nfail;
}

enum testkind { TEST_META, TEST_STREAM, TEST_BLANK, TEST_UNPACK, TEST_COUNT, NUM_TESTS };
static const char *testnames[NUM_TESTS] = { "meta", "stream", "blank", "unpack", "count" };

/* Library bugs known but not yet fixed; reported as xfail (see test_vdif_unpack.c) */
static const char *known_failure(const struct codif_mode *m, enum testkind t)
{
	(void)m;
	(void)t;

	return 0;
}

struct summary
{
	int ntests, npass, nfail, nxfail, nxpass;
};

static void test_mode(const struct codif_mode *m, const struct options *o, int strict, struct summary *sum)
{
	struct result res[NUM_TESTS];
	int ran[NUM_TESTS];
	int sizes[2], k, t, bad;
	uint64_t rng = o->seed ^ ((uint64_t)m->iscomplex << 40) ^ ((uint64_t)m->nbit << 20) ^ (uint64_t)m->nchan;

	memset(res, 0, sizeof(res));
	memset(ran, 0, sizeof(ran));
	sizes[0] = legal_databytes(m, 8000);
	sizes[1] = legal_databytes(m, 8);

	for(k = 0; k < 2; ++k)
	{
		for(bad = 0; bad <= 1; ++bad)
		{
			struct testdata td;
			char label[64];
			const char *tag = bad ? "bad-" : "";

			make_testdata(&td, m, sizes[k], bad, &rng);

			snprintf(label, sizeof(label), "%s/%dB/%sstream", m->name, sizes[k], tag);
			if(o->verbose) printf("    %s\n", label);
			test_stream(&td, label, o, &rng, &res[bad ? TEST_BLANK : TEST_STREAM], &res[TEST_META]);
			ran[TEST_META] = ran[bad ? TEST_BLANK : TEST_STREAM] = 1;

			snprintf(label, sizeof(label), "%s/%dB/%sunpack", m->name, sizes[k], tag);
			if(o->verbose) printf("    %s\n", label);
			test_unpack(&td, label, o, &rng, &res[TEST_UNPACK]);
			ran[TEST_UNPACK] = 1;

			if(m->hascount)
			{
				snprintf(label, sizeof(label), "%s/%dB/%scount", m->name, sizes[k], tag);
				test_count(&td, label, o, &rng, &res[TEST_COUNT]);
				ran[TEST_COUNT] = 1;
			}
			free_testdata(&td);
		}
	}

	printf("%-17s", m->name);
	for(t = 0; t < NUM_TESTS; ++t)
	{
		const char *known = known_failure(m, (enum testkind)t);
		const char *status;

		if(!ran[t])
		{
			status = "  -  ";
		}
		else
		{
			++sum->ntests;
			if(res[t].nbad == 0)
			{
				status = known ? "XPASS" : "pass ";
				if(known) ++sum->nxpass; else ++sum->npass;
			}
			else if(known && !strict)
			{
				status = "xfail";
				++sum->nxfail;
			}
			else
			{
				status = "FAIL ";
				++sum->nfail;
			}
		}
		printf("  %s:%s", testnames[t], status);
	}
	printf("\n");
	for(t = 0; t < NUM_TESTS; ++t)
	{
		if(ran[t] && res[t].nbad > 0 && (!known_failure(m, (enum testkind)t) || strict || o->verbose))
		{
			printf("    %s: %lld failure(s); first: %s\n", testnames[t], res[t].nbad, res[t].first);
		}
	}
	fflush(stdout);
}

int main(int argc, char **argv)
{
	static struct codif_mode modes[1024];
	struct options o;
	struct summary sum;
	int nmodes, i, opt, listonly = 0, strict = 0;

	memset(&o, 0, sizeof(o));
	memset(&sum, 0, sizeof(sum));
	o.seed = 1;
	o.maxreport = 1;

	while((opt = getopt(argc, argv, "m:ls:r:xvh")) != -1)
	{
		switch(opt)
		{
		case 'm': o.patterns = optarg; break;
		case 'l': listonly = 1; break;
		case 's': o.seed = strtoull(optarg, 0, 0); break;
		case 'r': o.maxreport = atoi(optarg); break;
		case 'x': strict = 1; break;
		case 'v': o.verbose = 1; break;
		case 'h': usage(argv[0]); return EXIT_SUCCESS;
		default: usage(argv[0]); return EXIT_FAILURE;
		}
	}

	nmodes = enumerate_modes(modes, 1024);
	nmodes = filter_modes(modes, nmodes, o.patterns);
	if(nmodes == 0)
	{
		fprintf(stderr, "No modes selected\n");

		return EXIT_FAILURE;
	}
	if(listonly)
	{
		for(i = 0; i < nmodes; ++i)
		{
			printf("%-17s  %d bits/sample%s\n", modes[i].name, modes[i].samplebits, modes[i].hascount ? ", has counter" : "");
		}

		return EXIT_SUCCESS;
	}

	printf("Testing %d CODIF unpacker modes (seed %llu, bit exact)\n", nmodes, (unsigned long long)o.seed);
	for(i = 0; i < nmodes; ++i)
	{
		test_mode(&modes[i], &o, strict, &sum);
	}
	if(!o.patterns)
	{
		uint64_t rng = o.seed;
		int nd = 0;

		printf("\n");
		nd += test_detect(0, 2, 1, 4, 0, &rng);
		nd += test_detect(0, 2, 1, 4, 100, &rng);
		nd += test_detect(1, 8, 3, 3, 37, &rng);
		nd += test_detect(0, 16, 4, 1, 8, &rng);
		sum.ntests += 4;
		sum.npass += 4 - nd;
		sum.nfail += nd;
	}

	printf("\n%d modes, %d tests: %d passed, %d FAILED", nmodes, sum.ntests, sum.npass, sum.nfail);
	if(!strict)
	{
		printf(", %d known failures (xfail), %d XPASS", sum.nxfail, sum.nxpass);
	}
	printf("\n");

	return sum.nfail ? EXIT_FAILURE : EXIT_SUCCESS;
}
