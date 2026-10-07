/***************************************************************************
 *   Correctness tests for the VDIF unpackers in format_vdif.c             *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 ***************************************************************************/

/* Every (real/complex, nbit, nchan) combination that the library accepts is
 * tested against an independent reference decoder (vdif_common.c) through
 * the public mark5access API:
 *
 *   stream  : memory stream + mark5_stream_decode[_complex], decoded in
 *             randomly sized chunks so that frame boundaries fall at
 *             arbitrary points within a call
 *   bad     : as stream, but with some frames flagged invalid or filled with
 *             fill pattern; these must decode as zeros and be excluded from
 *             the returned good-sample count
 *   unpack  : unpacker stream with headers + mark5_unpack[_complex]_with_offset
 *             at various offsets; this is the path used by mpifxcorr
 *   count   : mark5_stream_count_high_states (2-bit real modes only)
 *
 * Each test is repeated for several frame sizes, including the smallest legal
 * one.  Output arrays are padded with guard values to detect overruns.
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include "vdif_common.h"

#define GUARD		64		/* guard floats after each output array */
#define FPS		10000		/* frames per second in generated headers */

static const uint32_t GUARD_BITS = 0x7FC0DEADu;	/* a NaN */

struct options
{
	const char *patterns;
	uint64_t seed;
	double tol;
	int verbose;
	int maxreport;
};

/* A buffer of generated frames plus the reference decode of it */
struct testdata
{
	const struct vdif_mode *m;
	int databytes;
	int framebytes;
	int framesamples;
	int nframes;		/* frames with data to decode */
	int nalloc;		/* nframes plus one trailing frame the decoders may step onto */
	unsigned char *buf;
	enum frame_kind *kinds;
	long long nsamp;	/* nframes*framesamples */
	float **refre;		/* [nchan][nsamp] */
	float **refim;
};

/* Result of one test variant */
struct result
{
	long long nchecked;
	long long nbad;
	char first[256];	/* description of first failure */
};

static void usage(const char *pgm)
{
	printf("Usage: %s [options]\n\n", pgm);
	printf("Tests every VDIF unpacker in format_vdif.c against a reference decoder.\n\n");
	printf("Options:\n");
	printf("  -m <patterns>  only test modes matching comma separated glob patterns,\n");
	printf("                 e.g. -m 'real-2bit-*,cplx-8bit-4ch'\n");
	printf("  -l             list the supported modes and exit\n");
	printf("  -s <seed>      random seed [default 1]\n");
	printf("  -t <tol>       allowed absolute difference [default 0: bit exact]\n");
	printf("  -r <n>         report up to n mismatches per failing test [default 1]\n");
	printf("  -x             strict: known library bugs count as failures\n");
	printf("  -v             verbose: print each test variant\n");
	printf("  -h             this help\n\n");
	printf("Set SYMMETRIC_8_BIT_VDIF=1 to test the alternate 8-bit lookup table.\n");
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

static int values_differ(float a, float b, double tol)
{
	if(tol <= 0.0)
	{
		uint32_t ua, ub;

		memcpy(&ua, &a, 4);
		memcpy(&ub, &b, 4);

		return ua != ub;
	}

	return !(fabs((double)a - (double)b) <= tol);
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
	free(td->kinds);
	memset(td, 0, sizeof(*td));
}

/* Build frames and reference values.  If withbad, some frames (never the
 * first, whose validity the library does not check) are made bad. */
static void make_testdata(struct testdata *td, const struct vdif_mode *m, int databytes, int withbad, uint64_t *rng)
{
	long long s;
	int f, c, i;

	memset(td, 0, sizeof(*td));
	td->m = m;
	td->databytes = databytes;
	td->framebytes = databytes + VDIF_HEADER_BYTES;
	td->framesamples = vdif_mode_framesamples(m, databytes);

	/* at least 6 frames and a few thousand samples, without huge buffers */
	td->nframes = 6;
	while((long long)td->nframes*td->framesamples < 4096 && td->nframes < 4096)
	{
		++td->nframes;
	}
	td->nalloc = td->nframes + 1;
	td->nsamp = (long long)td->nframes*td->framesamples;

	td->kinds = (enum frame_kind *)calloc(td->nalloc, sizeof(enum frame_kind));
	if(withbad)
	{
		for(f = 1; f < td->nframes; ++f)
		{
			if(f % 5 == 2)
			{
				td->kinds[f] = FRAME_INVALID;
			}
			else if(f % 5 == 4)
			{
				td->kinds[f] = FRAME_FILL;
			}
		}
	}

	td->buf = (unsigned char *)malloc((size_t)td->nalloc*td->framebytes);
	vdif_generate_frames(td->buf, m, databytes, td->nalloc, FPS, td->kinds, rng);

	td->refre = (float **)calloc(m->nchan, sizeof(float *));
	td->refim = (float **)calloc(m->nchan, sizeof(float *));
	for(c = 0; c < m->nchan; ++c)
	{
		td->refre[c] = (float *)malloc(td->nsamp*sizeof(float));
		td->refim[c] = (float *)malloc(td->nsamp*sizeof(float));
	}

	for(f = 0; f < td->nframes; ++f)
	{
		const unsigned char *payload = td->buf + (size_t)f*td->framebytes + VDIF_HEADER_BYTES;

		for(i = 0; i < td->framesamples; ++i)
		{
			s = (long long)f*td->framesamples + i;
			for(c = 0; c < m->nchan; ++c)
			{
				if(td->kinds[f] == FRAME_GOOD)
				{
					ref_sample(m, payload, i, c, &td->refre[c][s], &td->refim[c][s]);
				}
				else
				{
					td->refre[c][s] = 0.0f;
					td->refim[c][s] = 0.0f;
				}
			}
		}
	}
}

/* Number of samples in [start, start+n) that lie in good frames */
static long long good_samples(const struct testdata *td, long long start, long long n)
{
	long long s, good = 0;

	for(s = start; s < start + n; ++s)
	{
		if(td->kinds[s/td->framesamples] == FRAME_GOOD)
		{
			++good;
		}
	}

	return good;
}

/* Output arrays: per channel, (nsamp + GUARD) floats, or twice that for complex */
static float **alloc_output(const struct vdif_mode *m, long long nsamp)
{
	float **out;
	long long n = (nsamp + GUARD)*(m->iscomplex ? 2 : 1);
	long long i;
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

static void free_output(const struct vdif_mode *m, float **out)
{
	int c;

	for(c = 0; c < m->nchan; ++c)
	{
		free(out[c]);
	}
	free(out);
}

/* Compare out[c][0..n) against reference samples [refstart, refstart+n), and
 * verify guard area beyond n is untouched. */
static void compare_output(const struct testdata *td, float **out, long long refstart, long long n,
	const char *label, const struct options *o, struct result *r)
{
	const struct vdif_mode *m = td->m;
	int stride = m->iscomplex ? 2 : 1;
	long long i, g;
	int c;

	for(c = 0; c < m->nchan; ++c)
	{
		for(i = 0; i < n; ++i)
		{
			long long s = refstart + i;
			float re = out[c][stride*i];
			float im = m->iscomplex ? out[c][stride*i + 1] : 0.0f;
			float xre = td->refre[c][s];
			float xim = td->refim[c][s];

			++r->nchecked;
			if(values_differ(re, xre, o->tol) || (m->iscomplex && values_differ(im, xim, o->tol)))
			{
				if(m->iscomplex)
				{
					result_fail(r, o, "%s: sample %lld (frame %lld, +%lld) chan %d: got (%g,%g) expected (%g,%g)",
						label, s, s/td->framesamples, s%td->framesamples, c, re, im, xre, xim);
				}
				else
				{
					result_fail(r, o, "%s: sample %lld (frame %lld, +%lld) chan %d: got %g expected %g",
						label, s, s/td->framesamples, s%td->framesamples, c, re, xre);
				}
			}
		}
		for(g = stride*n; g < stride*(n + GUARD); ++g)
		{
			if(memcmp(out[c] + g, &GUARD_BITS, 4) != 0)
			{
				result_fail(r, o, "%s: chan %d: output written beyond requested %lld samples (float index %lld)",
					label, c, n, g);
				break;
			}
		}
	}
}

/* Random chunk length: a multiple of granularity, at most remaining */
static long long chunk_length(const struct testdata *td, long long remaining, int gran, uint64_t *rng)
{
	long long maxunits = 3LL*td->framesamples/gran + 2;
	long long n;

	n = gran*(1 + (long long)(rng_next(rng) % maxunits));
	if(n > remaining)
	{
		n = remaining;
	}

	return n;
}

static struct mark5_stream *open_memory_stream(const struct testdata *td)
{
	return new_mark5_stream_absorb(
		new_mark5_stream_memory(td->buf, (unsigned int)((size_t)td->nalloc*td->framebytes)),
		vdif_mode_new_format(td->m, td->databytes, FPS));
}

/* Sequential decode through a memory stream in random sized chunks */
static void test_stream(const struct testdata *td, const char *label, const struct options *o,
	uint64_t *rng, struct result *r, struct result *rmeta)
{
	const struct vdif_mode *m = td->m;
	struct mark5_stream *ms;
	float **out, **ptr;
	long long pos = 0;
	int gran = vdif_mode_granularity(m);
	int stride = m->iscomplex ? 2 : 1;
	int c, first = 1;

	ms = open_memory_stream(td);
	if(!ms)
	{
		result_fail(r, o, "%s: could not create memory stream", label);
		return;
	}
	/* decoders step frames by byte count, so carry on and check the data even if these are wrong */
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
		long long n, expect;
		int ret;

		/* start with the smallest legal chunk */
		n = first ? gran : chunk_length(td, td->nsamp - pos, gran, rng);
		first = 0;
		for(c = 0; c < m->nchan; ++c)
		{
			ptr[c] = out[c] + stride*pos;
		}
		if(m->iscomplex)
		{
			ret = mark5_stream_decode_complex(ms, (int)n, (mark5_float_complex **)ptr);
		}
		else
		{
			ret = mark5_stream_decode(ms, (int)n, ptr);
		}
		expect = good_samples(td, pos, n);
		if(ret != expect)
		{
			result_fail(r, o, "%s: decode of %lld samples at %lld returned %d, expected %lld",
				label, n, pos, ret, expect);
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

/* Random access unpack with offset, as done by mpifxcorr */
static void test_unpack(const struct testdata *td, const char *label, const struct options *o,
	uint64_t *rng, struct result *r)
{
	const struct vdif_mode *m = td->m;
	struct mark5_stream *ms;
	float **out;
	int gran = vdif_mode_granularity(m);
	long long fs = td->framesamples;
	long long cases[16][2];
	int ncase = 0;
	int k, t;

	ms = new_mark5_stream_absorb(new_mark5_stream_unpacker(0), vdif_mode_new_format(m, td->databytes, FPS));
	if(!ms)
	{
		result_fail(r, o, "%s: could not create unpacker stream", label);
		return;
	}

	/* (offset, nsamp) pairs: whole buffer, within a frame, across boundaries */
	cases[ncase][0] = 0;            cases[ncase][1] = td->nsamp;  ++ncase;
	cases[ncase][0] = gran;         cases[ncase][1] = fs;         ++ncase;
	cases[ncase][0] = fs - gran;    cases[ncase][1] = 2*gran;     ++ncase;
	cases[ncase][0] = 2*fs;         cases[ncase][1] = fs;         ++ncase;
	cases[ncase][0] = 3*fs + gran;  cases[ncase][1] = 2*fs;       ++ncase;
	while(ncase < 16)
	{
		long long off = gran*(long long)(rng_next(rng) % (td->nsamp/gran));
		long long n = chunk_length(td, td->nsamp - off, gran, rng);

		cases[ncase][0] = off;
		cases[ncase][1] = n;
		++ncase;
	}

	out = alloc_output(m, td->nsamp);

	for(k = 0; k < ncase; ++k)
	{
		long long off = cases[k][0];
		long long n = cases[k][1];
		long long expect;
		char sublabel[96];
		int ret;

		if(n <= 0 || off + n > td->nsamp)
		{
			continue;
		}
		snprintf(sublabel, sizeof(sublabel), "%s off=%lld n=%lld", label, off, n);

		/* reset guards that a previous longer case would have overwritten */
		for(t = 0; t < m->nchan; ++t)
		{
			long long i, nf = (td->nsamp + GUARD)*(m->iscomplex ? 2 : 1);

			for(i = 0; i < nf; ++i)
			{
				memcpy(out[t] + i, &GUARD_BITS, 4);
			}
		}

		if(m->iscomplex)
		{
			ret = mark5_unpack_complex_with_offset(ms, td->buf, (int)off, (mark5_float_complex **)out, (int)n);
		}
		else
		{
			ret = mark5_unpack_with_offset(ms, td->buf, (int)off, out, (int)n);
		}
		expect = good_samples(td, off, n);
		if(ret != expect)
		{
			result_fail(r, o, "%s: returned %d, expected %lld", sublabel, ret, expect);
		}
		compare_output(td, out, off, n, sublabel, o, r);
	}

	free_output(m, out);
	delete_mark5_stream(ms);
}

/* High state counting for 2-bit modes */
static void test_count(const struct testdata *td, const char *label, const struct options *o,
	uint64_t *rng, struct result *r)
{
	const struct vdif_mode *m = td->m;
	struct mark5_stream *ms;
	unsigned int *got;
	unsigned long long *expect;
	long long pos = 0, s;
	int gran = vdif_mode_granularity(m);
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
		const unsigned char *payload = td->buf + (size_t)f*td->framebytes + VDIF_HEADER_BYTES;

		if(td->kinds[f] != FRAME_GOOD)
		{
			continue;
		}
		for(s = 0; s < td->framesamples; ++s)
		{
			for(c = 0; c < m->nchan; ++c)
			{
				expect[c] += ref_is_high_2bit(ref_extract(payload, (s*m->npad + c)*m->bitspersample, 2));
			}
		}
	}

	while(pos < td->nsamp)
	{
		long long n = chunk_length(td, td->nsamp - pos, gran, rng);
		long long e = good_samples(td, pos, n);
		int ret;

		ret = mark5_stream_count_high_states(ms, (int)n, got);
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

enum testkind { TEST_META, TEST_STREAM, TEST_BLANK, TEST_UNPACK, TEST_COUNT, NUM_TESTS };
static const char *testnames[NUM_TESTS] = { "meta", "stream", "blank", "unpack", "count" };

/* Library bugs known at the time these tests were written.  Such failures
 * are reported as "xfail" and do not affect the exit status (unless -x is
 * given); if one starts passing it is reported as "XPASS" so this table
 * can be updated. */
static const char *known_failure(const struct vdif_mode *m, enum testkind t)
{
	/* No known failures at present.  Entries look like:
	 *
	 * if(!m->iscomplex && m->nbit == 4 && m->nchan == 16 && t == TEST_BLANK)
	 * {
	 *	return "description of the library bug";
	 * }
	 */
	(void)m;
	(void)t;

	return 0;
}

struct summary
{
	int ntests;
	int npass;
	int nfail;		/* unexpected failures */
	int nxfail;		/* known failures */
	int nxpass;		/* known failures that now pass */
	int nfailmodes;
};

static void test_mode(const struct vdif_mode *m, const struct options *o, int strict, struct summary *sum)
{
	struct result res[NUM_TESTS];
	int ran[NUM_TESTS];
	int sizes[3];
	int nsize = 0;
	int t, k, modefail = 0;
	uint64_t rng = o->seed ^ ((uint64_t)m->iscomplex << 40) ^ ((uint64_t)m->nbit << 20) ^ (uint64_t)m->nchan;

	memset(res, 0, sizeof(res));
	memset(ran, 0, sizeof(ran));

	sizes[nsize++] = 8000;
	sizes[nsize++] = 1024;
	sizes[nsize++] = vdif_mode_min_databytes(m);

	for(k = 0; k < nsize; ++k)
	{
		struct testdata td;
		char label[64];
		int bad;

		if(sizes[k] % vdif_mode_min_databytes(m) != 0)
		{
			continue;
		}

		/* first pass with all frames good, second with some bad frames */
		for(bad = 0; bad <= 1; ++bad)
		{
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
				if(o->verbose) printf("    %s\n", label);
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
				if(known)
				{
					++sum->nxpass;
				}
				else
				{
					++sum->npass;
				}
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
				modefail = 1;
			}
		}
		printf("  %s:%s", testnames[t], status);
	}
	printf("\n");

	for(t = 0; t < NUM_TESTS; ++t)
	{
		const char *known = known_failure(m, (enum testkind)t);

		if(!ran[t])
		{
			continue;
		}
		if(res[t].nbad > 0 && (!known || strict || o->verbose))
		{
			printf("    %s: %lld failure(s); first: %s\n", testnames[t], res[t].nbad, res[t].first);
			if(known)
			{
				printf("    %s: known bug: %s\n", testnames[t], known);
			}
		}
		else if(res[t].nbad == 0 && known)
		{
			printf("    %s: now passes; was known bug: %s\n", testnames[t], known);
		}
	}
	fflush(stdout);

	sum->nfailmodes += modefail;
}

int main(int argc, char **argv)
{
	struct vdif_mode modes[MAX_VDIF_MODES];
	struct options o;
	struct summary sum;
	int nmodes, i, opt;
	int listonly = 0, strict = 0;

	memset(&o, 0, sizeof(o));
	memset(&sum, 0, sizeof(sum));
	o.seed = 1;
	o.maxreport = 1;

	while((opt = getopt(argc, argv, "m:ls:t:r:xvh")) != -1)
	{
		switch(opt)
		{
		case 'm': o.patterns = optarg; break;
		case 'l': listonly = 1; break;
		case 's': o.seed = strtoull(optarg, 0, 0); break;
		case 't': o.tol = atof(optarg); break;
		case 'r': o.maxreport = atoi(optarg); break;
		case 'x': strict = 1; break;
		case 'v': o.verbose = 1; break;
		case 'h': usage(argv[0]); return EXIT_SUCCESS;
		default: usage(argv[0]); return EXIT_FAILURE;
		}
	}

	ref_init();

	nmodes = vdif_enumerate_modes(modes, MAX_VDIF_MODES);
	nmodes = vdif_filter_modes(modes, nmodes, o.patterns);
	if(nmodes == 0)
	{
		fprintf(stderr, "No modes selected\n");

		return EXIT_FAILURE;
	}

	if(listonly)
	{
		for(i = 0; i < nmodes; ++i)
		{
			printf("%-17s  packed as %d chan, %d bits/sample%s\n", modes[i].name, modes[i].npad,
				modes[i].blockbits, modes[i].hascount ? ", has counter" : "");
		}

		return EXIT_SUCCESS;
	}

	printf("Testing %d VDIF unpacker modes (seed %llu, %s)%s\n", nmodes,
		(unsigned long long)o.seed, o.tol > 0 ? "with tolerance" : "bit exact",
		getenv("SYMMETRIC_8_BIT_VDIF") ? " SYMMETRIC_8_BIT_VDIF" : "");

	for(i = 0; i < nmodes; ++i)
	{
		test_mode(&modes[i], &o, strict, &sum);
	}

	printf("\n%d modes, %d tests: %d passed, %d FAILED", nmodes, sum.ntests, sum.npass, sum.nfail);
	if(!strict)
	{
		printf(", %d known failures (xfail), %d XPASS", sum.nxfail, sum.nxpass);
	}
	printf("\n");
	if(sum.nxpass > 0)
	{
		printf("Some known bugs appear fixed: update known_failure() in %s\n", __FILE__);
	}

	return sum.nfail ? EXIT_FAILURE : EXIT_SUCCESS;
}
