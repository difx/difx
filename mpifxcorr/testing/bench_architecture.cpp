/***************************************************************************
 *   Benchmark for the vector routines of architecture.h                   *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 ***************************************************************************/

/* Times each vector* routine in each implementation built (generic; IPP
 * when available) at one or more vector lengths.  Buffers are allocated and
 * filled once per routine and length, then the routine is called repeatedly
 * for at least the minimum time.  Repeats are interleaved across routines
 * and the best is kept, so a burst of other activity spoils one repeat of
 * many routines rather than every repeat of one.
 *
 * Reported per routine, implementation and length: ns per call, ns per
 * element and million elements per second (for FFTs an element is one
 * input point).  With IPP, generic/IPP time ratios are shown.  Results can
 * be written as CSV (-o) and compared with an earlier run (-b). */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <fnmatch.h>
#include <time.h>
#include <unistd.h>
#include <functional>
#include <memory>
#include <random>
#ifdef __APPLE__
#include <pthread.h>
#include <sys/qos.h>
#endif
#include "arch_api.h"

typedef std::function<void(void)> Call;
/* returns a callable doing one call of the routine on prepared buffers, or
 * an empty function if the routine is not available */
typedef std::function<Call(const ArchImpl &, int)> Factory;

struct Bench
{
	const char *name;
	Factory make;
};

static std::vector<Bench> &benches()
{
	static std::vector<Bench> b;

	return b;
}

struct AddBench
{
	AddBench(const char *name, Factory f) { benches().push_back(Bench{name, f}); }
};

static std::mt19937_64 rng(1);

static void rfill(float *p, int n, double lo, double hi) { std::uniform_real_distribution<double> u(lo, hi); for(int i = 0; i < n; ++i) p[i] = u(rng); }
static void rfill(double *p, int n, double lo, double hi) { std::uniform_real_distribution<double> u(lo, hi); for(int i = 0; i < n; ++i) p[i] = u(rng); }
static void rfill(acf32 *p, int n, double lo, double hi) { rfill((float *)p, 2*n, lo, hi); }
static void rfill(acf64 *p, int n, double lo, double hi) { rfill((double *)p, 2*n, lo, hi); }
static void rfill(int16_t *p, int n, double lo, double hi) { std::uniform_real_distribution<double> u(lo, hi); for(int i = 0; i < n; ++i) p[i] = (int16_t)u(rng); }
static void rfill(int32_t *p, int n, double lo, double hi) { std::uniform_real_distribution<double> u(lo, hi); for(int i = 0; i < n; ++i) p[i] = (int32_t)u(rng); }
static void rfill(uint8_t *p, int n, double lo, double hi) { std::uniform_real_distribution<double> u(lo, hi); for(int i = 0; i < n; ++i) p[i] = (uint8_t)u(rng); }

/* buffers live as long as the callable that uses them */
template<class T> static std::shared_ptr<std::vector<T>> buf(int n, double lo = -100, double hi = 100)
{
	std::shared_ptr<std::vector<T>> b(new std::vector<T>(n));

	rfill(b->data(), n, lo, hi);

	return b;
}

/* For in-place operations the data would grow or shrink without bound when
 * called repeatedly (e.g. multiply by a constant); use values that keep the
 * work representative and finite: scale near 1, offsets near 0. */

#define BENCH(name) static Call bench_##name(const ArchImpl &impl, int n); \
	static AddBench add_##name(#name, bench_##name); \
	static Call bench_##name(const ArchImpl &impl, int n)

#define GETFN(type, var, name) type var = (type)impl.get(name); if(!var) return Call();

/* generic patterns */
template<class T> static Call bin_i(const ArchImpl &impl, int n, const char *name)
{
	typedef int (*FN)(const T *, T *, int);
	GETFN(FN, f, name);
	auto s = buf<T>(n, -1e-3, 1e-3), sd = buf<T>(n);

	return [=]() { f(s->data(), sd->data(), n); };
}

template<class T> static Call bin(const ArchImpl &impl, int n, const char *name, double lo = -100, double hi = 100)
{
	typedef int (*FN)(const T *, const T *, T *, int);
	GETFN(FN, f, name);
	auto s1 = buf<T>(n, lo, hi), s2 = buf<T>(n), d = buf<T>(n);

	return [=]() { f(s1->data(), s2->data(), d->data(), n); };
}

template<class TI, class TO> static Call un(const ArchImpl &impl, int n, const char *name, double lo = -100, double hi = 100)
{
	typedef int (*FN)(const TI *, TO *, int);
	GETFN(FN, f, name);
	auto s = buf<TI>(n, lo, hi);
	auto d = buf<TO>(n);

	return [=]() { f(s->data(), d->data(), n); };
}

template<class T> static Call un_i(const ArchImpl &impl, int n, const char *name, double lo = 0.999, double hi = 1.001)
{
	typedef int (*FN)(T *, int);
	GETFN(FN, f, name);
	auto sd = buf<T>(n, lo, hi);

	return [=]() { f(sd->data(), n); };
}

template<class T> static Call scal(const ArchImpl &impl, int n, const char *name)
{
	typedef int (*FN)(const T *, T, T *, int);
	GETFN(FN, f, name);
	auto s = buf<T>(n), d = buf<T>(n);

	return [=]() { f(s->data(), (T)1.0001, d->data(), n); };
}

template<class T> static Call scal_i(const ArchImpl &impl, int n, const char *name, T v)
{
	typedef int (*FN)(T, T *, int);
	GETFN(FN, f, name);
	auto sd = buf<T>(n);

	return [=]() { f(v, sd->data(), n); };
}

template<class T> static Call zero(const ArchImpl &impl, int n, const char *name)
{
	typedef int (*FN)(T *, int);
	GETFN(FN, f, name);
	auto d = buf<T>(n);

	return [=]() { f(d->data(), n); };
}

BENCH(Add_f32_I) { return bin_i<float>(impl, n, "Add_f32_I"); }
BENCH(Add_f64_I) { return bin_i<double>(impl, n, "Add_f64_I"); }
BENCH(Add_cf32_I) { return bin_i<acf32>(impl, n, "Add_cf32_I"); }
BENCH(Add_cf64_I) { return bin_i<acf64>(impl, n, "Add_cf64_I"); }
BENCH(Add_s16_I) { return bin_i<int16_t>(impl, n, "Add_s16_I"); }
BENCH(Add_s32_I) { return bin_i<int32_t>(impl, n, "Add_s32_I"); }
BENCH(Sub_f32_I) { return bin_i<float>(impl, n, "Sub_f32_I"); }
BENCH(Sub_cf32_I) { return bin_i<acf32>(impl, n, "Sub_cf32_I"); }
BENCH(Mul_f32_I) { return bin_i<float>(impl, n, "Mul_f32_I"); }
BENCH(Mul_cf32_I) { return bin_i<acf32>(impl, n, "Mul_cf32_I"); }
BENCH(Add_f32) { return bin<float>(impl, n, "Add_f32"); }
BENCH(Add_f64) { return bin<double>(impl, n, "Add_f64"); }
BENCH(Add_cf32) { return bin<acf32>(impl, n, "Add_cf32"); }
BENCH(Sub_s32) { return bin<int32_t>(impl, n, "Sub_s32", -1e6, 1e6); }
BENCH(Sub_cf32) { return bin<acf32>(impl, n, "Sub_cf32"); }
BENCH(Mul_f32) { return bin<float>(impl, n, "Mul_f32"); }
BENCH(Mul_cf32) { return bin<acf32>(impl, n, "Mul_cf32"); }
BENCH(Divide_f32) { return bin<float>(impl, n, "Divide_f32", 0.5, 100); }
BENCH(AddProduct_cf32) { return bin<acf32>(impl, n, "AddProduct_cf32", -1e-3, 1e-3); }
BENCH(Mul_f32cf32)
{
	typedef int (*FN)(const float *, const acf32 *, acf32 *, int);
	GETFN(FN, f, "Mul_f32cf32");
	auto s1 = buf<float>(n);
	auto s2 = buf<acf32>(n), d = buf<acf32>(n);

	return [=]() { f(s1->data(), s2->data(), d->data(), n); };
}
BENCH(AddC_f32) { return scal<float>(impl, n, "AddC_f32"); }
BENCH(AddC_f64) { return scal<double>(impl, n, "AddC_f64"); }
BENCH(MulC_f32) { return scal<float>(impl, n, "MulC_f32"); }
BENCH(MulC_f64) { return scal<double>(impl, n, "MulC_f64"); }
BENCH(AddC_f32_I) { return scal_i<float>(impl, n, "AddC_f32_I", 1e-6f); }
BENCH(AddC_f64_I) { return scal_i<double>(impl, n, "AddC_f64_I", 1e-9); }
BENCH(AddC_s16_I) { return scal_i<int16_t>(impl, n, "AddC_s16_I", 0); }
/* -1: values stay bounded, and IPP returns at once when multiplying by 1 */
BENCH(MulC_f32_I) { return scal_i<float>(impl, n, "MulC_f32_I", -1.0f); }
BENCH(MulC_f64_I) { return scal_i<double>(impl, n, "MulC_f64_I", -1.0); }
BENCH(DivC_f32_I) { return scal_i<float>(impl, n, "DivC_f32_I", -1.0f); }
BENCH(MulC_cf32_I)
{
	typedef int (*FN)(acf32, acf32 *, int);
	GETFN(FN, f, "MulC_cf32_I");
	auto sd = buf<acf32>(n);
	acf32 v = { 0.6f, 0.8f };	/* unit magnitude: data stays finite */

	return [=]() { f(v, sd->data(), n); };
}
BENCH(DivC_cf32_I)
{
	typedef int (*FN)(acf32, acf32 *, int);
	GETFN(FN, f, "DivC_cf32_I");
	auto sd = buf<acf32>(n);
	acf32 v = { 0.6f, 0.8f };

	return [=]() { f(v, sd->data(), n); };
}
BENCH(MulC_cf32)
{
	typedef int (*FN)(const acf32 *, acf32, acf32 *, int);
	GETFN(FN, f, "MulC_cf32");
	auto s = buf<acf32>(n), d = buf<acf32>(n);
	acf32 v = { 0.6f, 0.8f };

	return [=]() { f(s->data(), v, d->data(), n); };
}
BENCH(Copy_u8) { return un<uint8_t, uint8_t>(impl, n, "Copy_u8", 0, 255); }
BENCH(Copy_s16) { return un<int16_t, int16_t>(impl, n, "Copy_s16"); }
BENCH(Copy_s32) { return un<int32_t, int32_t>(impl, n, "Copy_s32"); }
BENCH(Copy_f32) { return un<float, float>(impl, n, "Copy_f32"); }
BENCH(Copy_f64) { return un<double, double>(impl, n, "Copy_f64"); }
BENCH(Copy_cf32) { return un<acf32, acf32>(impl, n, "Copy_cf32"); }
BENCH(Move_f32) { return un<float, float>(impl, n, "Move_f32"); }
BENCH(Move_cf32) { return un<acf32, acf32>(impl, n, "Move_cf32"); }
BENCH(Conj_cf32) { return un<acf32, acf32>(impl, n, "Conj_cf32"); }
BENCH(ConjFlip_cf32) { return un<acf32, acf32>(impl, n, "ConjFlip_cf32"); }
BENCH(Flip_cf32) { return un<acf32, acf32>(impl, n, "Flip_cf32"); }
BENCH(Magnitude_cf32) { return un<acf32, float>(impl, n, "Magnitude_cf32"); }
BENCH(Phase_cf32) { return un<acf32, float>(impl, n, "Phase_cf32"); }
BENCH(Real_cf32) { return un<acf32, float>(impl, n, "Real_cf32"); }
BENCH(Cos_f32) { return un<float, float>(impl, n, "Cos_f32", -50, 50); }
BENCH(Sin_f32) { return un<float, float>(impl, n, "Sin_f32", -50, 50); }
BENCH(Convert_s16f32) { return un<int16_t, float>(impl, n, "Convert_s16f32", -32768, 32767); }
BENCH(Convert_s32f32) { return un<int32_t, float>(impl, n, "Convert_s32f32", -1e9, 1e9); }
BENCH(Convert_f64f32) { return un<double, float>(impl, n, "Convert_f64f32"); }
BENCH(SinCos_f32)
{
	typedef int (*FN)(const float *, float *, float *, int);
	GETFN(FN, f, "SinCos_f32");
	auto s = buf<float>(n, -50, 50), sn = buf<float>(n), cs = buf<float>(n);

	return [=]() { f(s->data(), sn->data(), cs->data(), n); };
}
BENCH(Conj_cf32_I) { return un_i<acf32>(impl, n, "Conj_cf32_I"); }
BENCH(Flip_f64_I) { return un_i<double>(impl, n, "Flip_f64_I"); }
BENCH(Square_f32_I) { return un_i<float>(impl, n, "Square_f32_I"); }
BENCH(Square_f64_I) { return un_i<double>(impl, n, "Square_f64_I"); }
BENCH(Sqrt_f32_I) { return un_i<float>(impl, n, "Sqrt_f32_I"); }
BENCH(Abs_f32_I) { return un_i<float>(impl, n, "Abs_f32_I", -1, 1); }
BENCH(Zero_u8) { return zero<uint8_t>(impl, n, "Zero_u8"); }
BENCH(Zero_s16) { return zero<int16_t>(impl, n, "Zero_s16"); }
BENCH(Zero_s32) { return zero<int32_t>(impl, n, "Zero_s32"); }
BENCH(Zero_f32) { return zero<float>(impl, n, "Zero_f32"); }
BENCH(Zero_cf32) { return zero<acf32>(impl, n, "Zero_cf32"); }
BENCH(Zero_cf64) { return zero<acf64>(impl, n, "Zero_cf64"); }
BENCH(Set_f32)
{
	typedef int (*FN)(float, float *, int);
	GETFN(FN, f, "Set_f32");
	auto d = buf<float>(n);

	return [=]() { f(1.5f, d->data(), n); };
}
BENCH(RealToComplex_f32)
{
	typedef int (*FN)(const float *, const float *, acf32 *, int);
	GETFN(FN, f, "RealToComplex_f32");
	auto re = buf<float>(n), im = buf<float>(n);
	auto d = buf<acf32>(n);

	return [=]() { f(re->data(), im->data(), d->data(), n); };
}
BENCH(ConvertScaled_s16f32)
{
	typedef int (*FN)(const int16_t *, float *, int, int);
	GETFN(FN, f, "ConvertScaled_s16f32");
	auto s = buf<int16_t>(n, -32768, 32767);
	auto d = buf<float>(n);

	return [=]() { f(s->data(), d->data(), n, 2); };
}
BENCH(ConvertScaled_f32s16)
{
	typedef int (*FN)(const float *, int16_t *, int, int, int);
	GETFN(FN, f, "ConvertScaled_f32s16");
	auto s = buf<float>(n, -3e4, 3e4);
	auto d = buf<int16_t>(n);

	return [=]() { f(s->data(), d->data(), n, ARCH_RND_NEAR, 0); };
}
BENCH(ConvertScaled_f32u8)
{
	typedef int (*FN)(const float *, uint8_t *, int, int, int);
	GETFN(FN, f, "ConvertScaled_f32u8");
	auto s = buf<float>(n, 0, 255);
	auto d = buf<uint8_t>(n);

	return [=]() { f(s->data(), d->data(), n, ARCH_RND_NEAR, 0); };
}
BENCH(Convert_f32s32)
{
	typedef int (*FN)(const float *, int32_t *, int, int);
	GETFN(FN, f, "Convert_f32s32");
	auto s = buf<float>(n, -1e9, 1e9);
	auto d = buf<int32_t>(n);

	return [=]() { f(s->data(), d->data(), n, ARCH_RND_NEAR); };
}
BENCH(SplitScaled_s16f32)
{
	typedef int (*FN)(const int16_t *, float **, int, int);
	GETFN(FN, f, "SplitScaled_s16f32");
	const int nchan = 2;
	auto s = buf<int16_t>(n, -32768, 32767);
	auto d0 = buf<float>(n/nchan + 1), d1 = buf<float>(n/nchan + 1);
	std::shared_ptr<std::vector<float *>> d(new std::vector<float *>{d0->data(), d1->data()});

	return [=]() { f(s->data(), d->data(), nchan, n/nchan); (void)d0; (void)d1; };
}
BENCH(Sum_cf32)
{
	typedef int (*FN)(const acf32 *, int, acf32 *);
	GETFN(FN, f, "Sum_cf32");
	auto s = buf<acf32>(n);
	std::shared_ptr<acf32> r(new acf32);

	return [=]() { f(s->data(), n, r.get()); };
}
BENCH(Mean_cf32)
{
	typedef int (*FN)(const acf32 *, int, acf32 *);
	GETFN(FN, f, "Mean_cf32");
	auto s = buf<acf32>(n);
	std::shared_ptr<acf32> r(new acf32);

	return [=]() { f(s->data(), n, r.get()); };
}
BENCH(DotProduct_f64)
{
	typedef int (*FN)(const double *, const double *, int, double *);
	GETFN(FN, f, "DotProduct_f64");
	auto s1 = buf<double>(n), s2 = buf<double>(n);
	std::shared_ptr<double> r(new double);

	return [=]() { f(s1->data(), s2->data(), n, r.get()); };
}
BENCH(StdDev_f32)
{
	typedef int (*FN)(const float *, int, float *);
	GETFN(FN, f, "StdDev_f32");
	auto s = buf<float>(n);
	std::shared_ptr<float> r(new float);

	return [=]() { f(s->data(), n, r.get()); };
}
BENCH(Max_f32)
{
	typedef int (*FN)(const float *, int, float *);
	GETFN(FN, f, "Max_f32");
	auto s = buf<float>(n);
	std::shared_ptr<float> r(new float);

	return [=]() { f(s->data(), n, r.get()); };
}
BENCH(Min_f32)
{
	typedef int (*FN)(const float *, int, float *);
	GETFN(FN, f, "Min_f32");
	auto s = buf<float>(n);
	std::shared_ptr<float> r(new float);

	return [=]() { f(s->data(), n, r.get()); };
}
BENCH(MaxIndx_f32)
{
	typedef int (*FN)(const float *, int, float *, int *);
	GETFN(FN, f, "MaxIndx_f32");
	auto s = buf<float>(n);
	std::shared_ptr<float> r(new float);
	std::shared_ptr<int> idx(new int);

	return [=]() { f(s->data(), n, r.get(), idx.get()); };
}

/* FFTs: n is the transform length (rounded down to a power of 2 for FFTs) */
typedef int (*fn_fft_init)(int, int, void **, unsigned char **, int *);
typedef int (*fn_fft_free)(void *, unsigned char *);

template<class TI, class TO> static Call fft(const ArchImpl &impl, int n, const char *name, bool isorder)
{
	typedef int (*FN)(void *, unsigned char *, const TI *, TO *);
	fn_fft_init init = (fn_fft_init)impl.get(std::string(name) + "_init");
	FN fwd = (FN)impl.get(std::string(name) + "_fwd");
	fn_fft_free fr = (fn_fft_free)impl.get(std::string(name) + "_free");
	int order = 0, N = n;
	void *spec = 0;
	unsigned char *b = 0;
	int bufsize = 0;

	if(!init || !fwd || !fr)
	{
		return Call();
	}
	if(isorder)
	{
		while((2 << order) <= n)
		{
			++order;
		}
		N = 1 << order;
	}
	if(init(isorder ? order : N, ARCH_FFT_NORENORM, &spec, &b, &bufsize) != 0)
	{
		return Call();
	}
	if(b == ARCH_UNSET_PTR)
	{
		b = 0;		/* generic real init leaves it unset; the transforms ignore it */
	}
	auto x = buf<TI>(N, -1, 1);
	auto X = buf<TO>(sizeof(TO) == sizeof(float) && sizeof(TI) == sizeof(float) ? N + 2 : N);
	/* the spec is freed when the last copy of the callable goes */
	std::shared_ptr<void> owner((void *)1, [=](void *) { fr(spec, b); });

	return [=]() { fwd(spec, b, x->data(), X->data()); (void)owner; };
}

BENCH(FFTC_cf32) { return fft<acf32, acf32>(impl, n, "FFTC_cf32", true); }
BENCH(DFTC_cf32) { return fft<acf32, acf32>(impl, n, "DFTC_cf32", false); }
BENCH(FFTR_f32) { return fft<float, float>(impl, n, "FFTR_f32", true); }
BENCH(DFTR_f32) { return fft<float, float>(impl, n, "DFTR_f32", false); }

/* ---------------------------------------------------------------- main */

struct Options
{
	const char *patterns;
	const char *csvout;
	const char *baseline;
	std::vector<int> lengths;
	double mintime;
	int repeats;
};

struct Case
{
	std::string name;
	const ArchImpl *impl;
	int n;
	Call call;
	double best;	/* seconds per call */
};

static double now(void)
{
	struct timespec ts;

	clock_gettime(CLOCK_MONOTONIC, &ts);

	return ts.tv_sec + 1e-9*ts.tv_nsec;
}

static void time_case(Case &c, double mintime)
{
	long long calls = 0, batch = 1;
	double t0, t = 0;

	c.call();	/* warm up */
	t0 = now();
	do
	{
		for(long long k = 0; k < batch; ++k)
		{
			c.call();
		}
		calls += batch;
		t = now() - t0;
		if(batch < (1LL << 20))
		{
			batch *= 2;
		}
	}
	while(t < mintime);
	if(t/calls < c.best)
	{
		c.best = t/calls;
	}
}

static bool match(const char *name, const char *patterns)
{
	char b[1024];
	char *tok, *save = 0;

	if(!patterns)
	{
		return true;
	}
	snprintf(b, sizeof(b), "%s", patterns);
	for(tok = strtok_r(b, ",", &save); tok; tok = strtok_r(0, ",", &save))
	{
		if(fnmatch(tok, name, 0) == 0)
		{
			return true;
		}
	}

	return false;
}

static std::string key(const Case &c)
{
	return c.impl->name + "," + c.name + "," + std::to_string(c.n);
}

static void usage(const char *pgm)
{
	printf("Usage: %s [options]\n\n", pgm);
	printf("Benchmarks the vector routines of mpifxcorr's architecture.h in each\n");
	printf("implementation built (generic, its compiler-vectorized and SIMD builds where\n");
	printf("supported; IPP if available).\n\n");
	printf("Options:\n");
	printf("  -m <patterns>  only routines matching comma separated glob patterns\n");
	printf("  -n <list>      comma separated vector lengths [default 4096]\n");
	printf("  -t <seconds>   minimum time per repeat [default 0.05]\n");
	printf("  -r <n>         repeats, best is reported [default 5]\n");
	printf("  -o <file>      write results as CSV\n");
	printf("  -b <file>      compare against a CSV written earlier with -o\n");
	printf("  -h             this help\n");
}

int main(int argc, char **argv)
{
	Options o;
	std::vector<const ArchImpl *> impls;
	std::vector<Case> cases;
	std::map<std::string, double> base;
	int opt;
	double logsum = 0;
	int nratio = 0;
	FILE *csv = 0;
	std::string ref = "generic";

	o.patterns = 0;
	o.csvout = 0;
	o.baseline = 0;
	o.mintime = 0.05;
	o.repeats = 5;
	while((opt = getopt(argc, argv, "m:n:t:r:o:b:h")) != -1)
	{
		switch(opt)
		{
		case 'm': o.patterns = optarg; break;
		case 'n':
			{
				char b[1024], *tok, *save = 0;

				snprintf(b, sizeof(b), "%s", optarg);
				for(tok = strtok_r(b, ",", &save); tok; tok = strtok_r(0, ",", &save))
				{
					o.lengths.push_back(atoi(tok));
				}
			}
			break;
		case 't': o.mintime = atof(optarg); break;
		case 'r': o.repeats = atoi(optarg); break;
		case 'o': o.csvout = optarg; break;
		case 'b': o.baseline = optarg; break;
		case 'h': usage(argv[0]); return EXIT_SUCCESS;
		default: usage(argv[0]); return EXIT_FAILURE;
		}
	}
	if(o.lengths.empty())
	{
		o.lengths.push_back(4096);
	}

#ifdef __APPLE__
	pthread_set_qos_class_self_np(QOS_CLASS_USER_INTERACTIVE, 0);
#endif

	impls = arch_impls_sorted();
	/* times are also given relative to IPP, or without it to plain generic */
	for(const ArchImpl *impl : impls)
	{
		if(impl->name == "ipp")
		{
			ref = "ipp";
		}
	}

	if(o.baseline)
	{
		FILE *in = fopen(o.baseline, "r");
		char line[512];

		if(!in)
		{
			fprintf(stderr, "Cannot open %s\n", o.baseline);

			return EXIT_FAILURE;
		}
		while(fgets(line, sizeof(line), in))
		{
			char impl[64], name[128];
			int n;
			double ns;

			if(sscanf(line, "%63[^,],%127[^,],%d,%lf", impl, name, &n, &ns) == 4)
			{
				base[std::string(impl) + "," + name + "," + std::to_string(n)] = ns;
			}
		}
		fclose(in);
	}

	for(const Bench &b : benches())
	{
		if(!match(b.name, o.patterns))
		{
			continue;
		}
		for(int n : o.lengths)
		{
			for(const ArchImpl *impl : impls)
			{
				Case c;

				c.name = b.name;
				c.impl = impl;
				c.n = n;
				c.call = b.make(*impl, n);
				c.best = 1e30;
				if(c.call)
				{
					cases.push_back(c);
				}
			}
		}
	}

	printf("architecture.h benchmark: best of %d x %.3f s per routine\n\n", o.repeats, o.mintime);
	for(int rep = 0; rep < o.repeats; ++rep)
	{
		fprintf(stderr, "\rrepeat %d/%d", rep + 1, o.repeats);
		for(Case &c : cases)
		{
			time_case(c, o.mintime);
		}
	}
	fprintf(stderr, "\r              \r");

	if(o.csvout)
	{
		csv = fopen(o.csvout, "w");
		if(csv)
		{
			fprintf(csv, "impl,routine,length,ns_per_call,ns_per_element,Melements_per_s\n");
		}
	}

	printf("%-22s %-8s %7s %12s %10s %10s", "routine", "impl", "length", "ns/call", "ns/elem", "Melem/s");
	if(impls.size() > 1)
	{
		printf(" %10s", ("vs " + ref).c_str());
	}
	if(!base.empty())
	{
		printf(" %10s", "speedup");
	}
	printf("\n");
	for(const Case &c : cases)
	{
		double ns = 1e9*c.best;

		printf("%-22s %-8s %7d %12.1f %10.3f %10.1f", c.name.c_str(), c.impl->name.c_str(), c.n, ns, ns/c.n, c.n/ns*1e3);
		if(impls.size() > 1)
		{
			/* time relative to the reference for the same routine and length */
			double refns = 0;

			for(const Case &d : cases)
			{
				if(d.name == c.name && d.n == c.n && d.impl->name == ref)
				{
					refns = 1e9*d.best;
				}
			}
			if(refns > 0)
			{
				printf(" %9.2fx", ns/refns);
			}
			else
			{
				printf(" %10s", "-");
			}
		}
		if(!base.empty())
		{
			std::map<std::string, double>::const_iterator it = base.find(key(c));

			if(it != base.end() && ns > 0)
			{
				printf(" %9.2fx", it->second/ns);
				logsum += log(it->second/ns);
				++nratio;
			}
		}
		printf("\n");
		if(csv)
		{
			fprintf(csv, "%s,%s,%d,%.3f,%.4f,%.2f\n", c.impl->name.c_str(), c.name.c_str(), c.n, ns, ns/c.n, c.n/ns*1e3);
		}
	}
	if(nratio)
	{
		printf("\nGeometric mean speedup over %d cases: %.3fx\n", nratio, exp(logsum/nratio));
	}
	if(csv)
	{
		fclose(csv);
		printf("\nResults written to %s\n", o.csvout);
	}

	return EXIT_SUCCESS;
}
