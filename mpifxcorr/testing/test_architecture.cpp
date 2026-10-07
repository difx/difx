/***************************************************************************
 *   Correctness tests for the vector routines of architecture.h           *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 ***************************************************************************/

/* Every vector* routine of architecture.h is checked, in each implementation
 * that was built (the generic FFTW/C++ section always; the Intel IPP section
 * when IPP is available), against a reference model of what IPP does,
 * computed in double precision.  IPP is taken to be correct: where IPP is
 * available a failure of IPP itself points at the reference model.
 *
 * Semantics follow the IPP manual, notably:
 *   - out-of-place Sub and Div operate as src2 - src1 and src2 / src1
 *   - integer results saturate; scaled conversions multiply by 2^-scale
 *   - ippRndZero truncates, ippRndNear rounds half to even
 *   - FFTs are unnormalised forward; the real forward output is CCS
 *     (N/2+1 complex values); IPP_FFT_DIV_INV_BY_N scales inverse by 1/N
 *
 * Floating point results are compared with tolerances matching the
 * operation (rounding of each operation, reduction length, the A21 accuracy
 * of IPP's sin/cos, FFT error growth).  Outputs have guard elements after
 * the end, so writing past the requested length is a failure.  Routines that
 * do not compile in an implementation are reported as such. */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <fnmatch.h>
#include <signal.h>
#include <unistd.h>
#include <sys/wait.h>
#include <functional>
#include <random>
#include <set>
#include "arch_api.h"

#define GUARD 16

static const int lengths[] = { 1, 2, 3, 7, 16, 63, 64, 1000, 4096 };
#define NLENGTHS ((int)(sizeof(lengths)/sizeof(lengths[0])))

struct Options
{
	const char *patterns;
	unsigned long long seed;
	int verbose;
	int maxreport;
	int strict;
};

struct Result
{
	long long nchecked;
	long long nbad;
	std::string first;
};

/* State for running one test against one implementation */
struct Ctx
{
	const ArchImpl *impl;
	const Options *o;
	Result res;
	std::mt19937_64 rng;
	std::string label;	/* e.g. "Add_f32_I n=7" */

	template<class F> F fn(const char *name) const { return (F)impl->get(name); }

	void fail(const char *fmt, ...) __attribute__((format(printf, 2, 3)))
	{
		char msg[512];
		va_list ap;

		va_start(ap, fmt);
		vsnprintf(msg, sizeof(msg), fmt, ap);
		va_end(ap);
		if(res.nbad == 0)
		{
			res.first = label + ": " + msg;
		}
		if(res.nbad < o->maxreport && o->maxreport > 1)
		{
			printf("      %s: %s\n", label.c_str(), msg);
		}
		++res.nbad;
	}

	void status(int st, const char *what)
	{
		++res.nchecked;
		if(st != 0)
		{
			fail("%s returned status %d", what, st);
		}
	}

	/* |got - want| <= tol, NaN only where expected */
	void close(const char *what, long i, double got, double want, double tol)
	{
		++res.nchecked;
		if(isnan(want) ? !isnan(got) : !(fabs(got - want) <= tol))
		{
			fail("%s[%ld]: got %.9g expected %.9g (tolerance %.3g)", what, i, got, want, tol);
		}
	}

	void exact(const char *what, long i, double got, double want)
	{
		++res.nchecked;
		if(got != want)
		{
			fail("%s[%ld]: got %.9g expected %.9g", what, i, got, want);
		}
	}

	double uniform(double lo, double hi) { return std::uniform_real_distribution<double>(lo, hi)(rng); }
	long long irand(long long lo, long long hi) { return std::uniform_int_distribution<long long>(lo, hi)(rng); }
};

/* Buffer with guard elements after the end */
template<class T> struct GBuf
{
	std::vector<unsigned char> mem;
	int n;

	explicit GBuf(int n_) : mem((n_ + GUARD)*sizeof(T), 0xA5), n(n_) {}
	T *p() { return (T *)&mem[0]; }
	T &operator[](int i) { return p()[i]; }
	bool guard_ok() const
	{
		for(size_t i = n*sizeof(T); i < mem.size(); ++i)
		{
			if(mem[i] != 0xA5)
			{
				return false;
			}
		}

		return true;
	}
	void check(Ctx &c, const char *what) const
	{
		++c.res.nchecked;
		if(!guard_ok())
		{
			c.fail("%s: written beyond end (%d elements)", what, n);
		}
	}
};

static const double EPS = FLT_EPSILON;
static const double DEPS = DBL_EPSILON;

/* ---------------------------------------------------------------- data */

/* Random data; a NaN range means the default: [-100, 100) for floating
 * point, the full range for integers */
static void range(double &lo, double &hi, double dlo, double dhi) { if(isnan(lo)) { lo = dlo; hi = dhi; } }
static void fill(Ctx &c, float *p, int n, double lo = NAN, double hi = NAN) { range(lo, hi, -100, 100); for(int i = 0; i < n; ++i) p[i] = c.uniform(lo, hi); }
static void fill(Ctx &c, double *p, int n, double lo = NAN, double hi = NAN) { range(lo, hi, -100, 100); for(int i = 0; i < n; ++i) p[i] = c.uniform(lo, hi); }
static void fill(Ctx &c, acf32 *p, int n, double lo = NAN, double hi = NAN) { range(lo, hi, -100, 100); for(int i = 0; i < n; ++i) { p[i].re = c.uniform(lo, hi); p[i].im = c.uniform(lo, hi); } }
static void fill(Ctx &c, acf64 *p, int n, double lo = NAN, double hi = NAN) { range(lo, hi, -100, 100); for(int i = 0; i < n; ++i) { p[i].re = c.uniform(lo, hi); p[i].im = c.uniform(lo, hi); } }
static void fill(Ctx &c, int16_t *p, int n, double lo = NAN, double hi = NAN) { range(lo, hi, -32768, 32767); for(int i = 0; i < n; ++i) p[i] = c.irand(lo, hi); }
static void fill(Ctx &c, int32_t *p, int n, double lo = NAN, double hi = NAN) { range(lo, hi, INT32_MIN, INT32_MAX); for(int i = 0; i < n; ++i) p[i] = c.irand(lo, hi); }
static void fill(Ctx &c, uint8_t *p, int n, double lo = NAN, double hi = NAN) { range(lo, hi, 0, 255); for(int i = 0; i < n; ++i) p[i] = c.irand(lo, hi); }

static double sat(double v, double lo, double hi) { return v < lo ? lo : v > hi ? hi : v; }

/* IPP rounding of a scaled float to integer, then saturation */
static double ipp_round(double v, int rnd)
{
	if(rnd == ARCH_RND_ZERO)
	{
		return trunc(v);
	}

	return nearbyint(v);	/* default rounding mode: half to even */
}

/* ---------------------------------------------------------------- tests */

struct Test
{
	const char *name;
	std::vector<const char *> deps;		/* wrapper names required */
	std::function<void(Ctx &)> run;
};

static std::vector<Test> &tests()
{
	static std::vector<Test> t;

	return t;
}

struct AddTest
{
	AddTest(const char *name, std::vector<const char *> deps, std::function<void(Ctx &)> run)
	{
		tests().push_back(Test{name, deps, run});
	}
};

#define TEST(name, ...) static void test_##name(Ctx &c); \
	static AddTest add_##name(#name, std::vector<const char *>{__VA_ARGS__}, test_##name); \
	static void test_##name(Ctx &c)

#define FOR_LENGTHS for(int li = 0; li < NLENGTHS; ++li) for(int n = lengths[li], once = 1; once; once = 0)
#define SETLABEL(c, n) (c).label = std::string(__func__ + 5) + " n=" + std::to_string(n)

typedef int (*fn_bin_i_f32)(const float *, float *, int);
typedef int (*fn_bin_i_f64)(const double *, double *, int);
typedef int (*fn_bin_i_cf32)(const acf32 *, acf32 *, int);
typedef int (*fn_bin_i_cf64)(const acf64 *, acf64 *, int);
typedef int (*fn_bin_i_s16)(const int16_t *, int16_t *, int);
typedef int (*fn_bin_i_s32)(const int32_t *, int32_t *, int);

/* elementwise real binary in place: sd[i] = op(sd[i], s[i]) */
template<class T, class FN, class OP> static void real_bin_i(Ctx &c, const char *name, OP op, double eps)
{
	FN f = c.fn<FN>(name);

	FOR_LENGTHS
	{
		GBuf<T> s(n), sd(n);
		std::vector<T> sd0(n);

		c.label = std::string(name) + " n=" + std::to_string(n);
		fill(c, s.p(), n);
		fill(c, sd.p(), n);
		memcpy(sd0.data(), sd.p(), n*sizeof(T));
		c.status(f(s.p(), sd.p(), n), name);
		for(int i = 0; i < n; ++i)
		{
			double want = op((double)sd0[i], (double)s[i]);

			c.close("sd", i, sd[i], want, eps*fabs(want));
		}
		sd.check(c, "sd");
	}
}

template<class T, class FN> static void complex_add_i(Ctx &c, const char *name, double sign, double eps)
{
	FN f = c.fn<FN>(name);

	FOR_LENGTHS
	{
		GBuf<T> s(n), sd(n);
		std::vector<T> sd0(n);

		c.label = std::string(name) + " n=" + std::to_string(n);
		fill(c, s.p(), n);
		fill(c, sd.p(), n);
		memcpy(sd0.data(), sd.p(), n*sizeof(T));
		c.status(f(s.p(), sd.p(), n), name);
		for(int i = 0; i < n; ++i)
		{
			double wr = sd0[i].re + sign*s[i].re, wi = sd0[i].im + sign*s[i].im;

			c.close("sd.re", i, sd[i].re, wr, eps*fabs(wr));
			c.close("sd.im", i, sd[i].im, wi, eps*fabs(wi));
		}
		sd.check(c, "sd");
	}
}

template<class T, class FN> static void int_add_i(Ctx &c, const char *name, double lo, double hi, double sign)
{
	FN f = c.fn<FN>(name);

	FOR_LENGTHS
	{
		GBuf<T> s(n), sd(n);
		std::vector<T> sd0(n);

		c.label = std::string(name) + " n=" + std::to_string(n);
		fill(c, s.p(), n);
		fill(c, sd.p(), n);
		memcpy(sd0.data(), sd.p(), n*sizeof(T));
		c.status(f(s.p(), sd.p(), n), name);
		for(int i = 0; i < n; ++i)
		{
			c.exact("sd (saturating)", i, sd[i], sat((double)sd0[i] + sign*(double)s[i], lo, hi));
		}
		sd.check(c, "sd");
	}
}

TEST(Add_f32_I, "Add_f32_I") { real_bin_i<float, fn_bin_i_f32>(c, "Add_f32_I", [](double a, double b) { return a + b; }, EPS); }
TEST(Add_f64_I, "Add_f64_I") { real_bin_i<double, fn_bin_i_f64>(c, "Add_f64_I", [](double a, double b) { return a + b; }, DEPS); }
TEST(Add_cf32_I, "Add_cf32_I") { complex_add_i<acf32, fn_bin_i_cf32>(c, "Add_cf32_I", 1, EPS); }
TEST(Add_cf64_I, "Add_cf64_I") { complex_add_i<acf64, fn_bin_i_cf64>(c, "Add_cf64_I", 1, DEPS); }
TEST(Add_s16_I, "Add_s16_I") { int_add_i<int16_t, fn_bin_i_s16>(c, "Add_s16_I", -32768, 32767, 1); }
TEST(Add_s32_I, "Add_s32_I") { int_add_i<int32_t, fn_bin_i_s32>(c, "Add_s32_I", INT32_MIN, INT32_MAX, 1); }
TEST(Sub_f32_I, "Sub_f32_I") { real_bin_i<float, fn_bin_i_f32>(c, "Sub_f32_I", [](double a, double b) { return a - b; }, EPS); }
TEST(Sub_cf32_I, "Sub_cf32_I") { complex_add_i<acf32, fn_bin_i_cf32>(c, "Sub_cf32_I", -1, EPS); }
TEST(Mul_f32_I, "Mul_f32_I") { real_bin_i<float, fn_bin_i_f32>(c, "Mul_f32_I", [](double a, double b) { return a*b; }, EPS); }

/* out of place real binary: d = op(s1, s2) (note IPP: Sub/Div give s2 - s1, s2 / s1) */
template<class T, class OP> static void real_bin(Ctx &c, const char *name, OP op, double eps, double s1lo = NAN, double s1hi = NAN)
{
	typedef int (*FN)(const T *, const T *, T *, int);
	FN f = c.fn<FN>(name);

	FOR_LENGTHS
	{
		GBuf<T> s1(n), s2(n), d(n);

		c.label = std::string(name) + " n=" + std::to_string(n);
		fill(c, s1.p(), n, s1lo, s1hi);
		fill(c, s2.p(), n);
		c.status(f(s1.p(), s2.p(), d.p(), n), name);
		for(int i = 0; i < n; ++i)
		{
			double want = op((double)s1[i], (double)s2[i]);

			c.close("d", i, d[i], want, eps*fabs(want));
		}
		d.check(c, "d");
	}
}

template<class T> static void int_bin(Ctx &c, const char *name, double lo, double hi, double sign)
{
	typedef int (*FN)(const T *, const T *, T *, int);
	FN f = c.fn<FN>(name);

	FOR_LENGTHS
	{
		GBuf<T> s1(n), s2(n), d(n);

		c.label = std::string(name) + " n=" + std::to_string(n);
		fill(c, s1.p(), n);
		fill(c, s2.p(), n);
		c.status(f(s1.p(), s2.p(), d.p(), n), name);
		for(int i = 0; i < n; ++i)
		{
			/* sign +1: s1 + s2; sign -1: s2 - s1 (IPP Sub) */
			c.exact("d (saturating)", i, d[i], sat((double)s2[i] + sign*(double)s1[i], lo, hi));
		}
		d.check(c, "d");
	}
}

template<class T> static void complex_bin_add(Ctx &c, const char *name, double sign, double eps)
{
	typedef int (*FN)(const T *, const T *, T *, int);
	FN f = c.fn<FN>(name);

	FOR_LENGTHS
	{
		GBuf<T> s1(n), s2(n), d(n);

		c.label = std::string(name) + " n=" + std::to_string(n);
		fill(c, s1.p(), n);
		fill(c, s2.p(), n);
		c.status(f(s1.p(), s2.p(), d.p(), n), name);
		for(int i = 0; i < n; ++i)
		{
			double wr = s2[i].re + sign*s1[i].re, wi = s2[i].im + sign*s1[i].im;

			c.close("d.re", i, d[i].re, wr, eps*fabs(wr));
			c.close("d.im", i, d[i].im, wi, eps*fabs(wi));
		}
		d.check(c, "d");
	}
}

TEST(Add_f32, "Add_f32") { real_bin<float>(c, "Add_f32", [](double a, double b) { return a + b; }, EPS); }
TEST(Add_f64, "Add_f64") { real_bin<double>(c, "Add_f64", [](double a, double b) { return a + b; }, DEPS); }
TEST(Add_s16, "Add_s16") { int_bin<int16_t>(c, "Add_s16", -32768, 32767, 1); }
TEST(Add_s32, "Add_s32") { int_bin<int32_t>(c, "Add_s32", INT32_MIN, INT32_MAX, 1); }
TEST(Add_cf32, "Add_cf32") { complex_bin_add<acf32>(c, "Add_cf32", 1, EPS); }
TEST(Add_cf64, "Add_cf64") { complex_bin_add<acf64>(c, "Add_cf64", 1, DEPS); }
TEST(Sub_s32, "Sub_s32") { int_bin<int32_t>(c, "Sub_s32", INT32_MIN, INT32_MAX, -1); }
TEST(Sub_cf32, "Sub_cf32") { complex_bin_add<acf32>(c, "Sub_cf32", -1, EPS); }
TEST(Mul_f32, "Mul_f32") { real_bin<float>(c, "Mul_f32", [](double a, double b) { return a*b; }, EPS); }
TEST(Divide_f32, "Divide_f32") { real_bin<float>(c, "Divide_f32", [](double a, double b) { return b/a; }, EPS, 0.5, 100); }

/* complex products: d = s1*s2, sd = sd*s, acc += s1*s2 */
static double cmul_tol(acf32 a, acf32 b, double accmag) { return 3*EPS*(fabs(a.re*b.re) + fabs(a.im*b.im) + fabs(a.re*b.im) + fabs(a.im*b.re) + accmag); }

TEST(Mul_cf32, "Mul_cf32")
{
	typedef int (*FN)(const acf32 *, const acf32 *, acf32 *, int);
	FN f = c.fn<FN>("Mul_cf32");

	FOR_LENGTHS
	{
		GBuf<acf32> s1(n), s2(n), d(n);

		SETLABEL(c, n);
		fill(c, s1.p(), n);
		fill(c, s2.p(), n);
		c.status(f(s1.p(), s2.p(), d.p(), n), "Mul_cf32");
		for(int i = 0; i < n; ++i)
		{
			double tol = cmul_tol(s1[i], s2[i], 0);

			c.close("d.re", i, d[i].re, (double)s1[i].re*s2[i].re - (double)s1[i].im*s2[i].im, tol);
			c.close("d.im", i, d[i].im, (double)s1[i].re*s2[i].im + (double)s1[i].im*s2[i].re, tol);
		}
		d.check(c, "d");
	}
}

TEST(Mul_cf32_I, "Mul_cf32_I")
{
	fn_bin_i_cf32 f = c.fn<fn_bin_i_cf32>("Mul_cf32_I");

	FOR_LENGTHS
	{
		GBuf<acf32> s(n), sd(n);
		std::vector<acf32> sd0(n);

		SETLABEL(c, n);
		fill(c, s.p(), n);
		fill(c, sd.p(), n);
		memcpy(sd0.data(), sd.p(), n*sizeof(acf32));
		c.status(f(s.p(), sd.p(), n), "Mul_cf32_I");
		for(int i = 0; i < n; ++i)
		{
			double tol = cmul_tol(s[i], sd0[i], 0);

			c.close("sd.re", i, sd[i].re, (double)s[i].re*sd0[i].re - (double)s[i].im*sd0[i].im, tol);
			c.close("sd.im", i, sd[i].im, (double)s[i].re*sd0[i].im + (double)s[i].im*sd0[i].re, tol);
		}
		sd.check(c, "sd");
	}
}

TEST(AddProduct_cf32, "AddProduct_cf32")
{
	typedef int (*FN)(const acf32 *, const acf32 *, acf32 *, int);
	FN f = c.fn<FN>("AddProduct_cf32");

	FOR_LENGTHS
	{
		GBuf<acf32> s1(n), s2(n), acc(n);
		std::vector<acf32> acc0(n);

		SETLABEL(c, n);
		fill(c, s1.p(), n);
		fill(c, s2.p(), n);
		fill(c, acc.p(), n, -1e4, 1e4);
		memcpy(acc0.data(), acc.p(), n*sizeof(acf32));
		c.status(f(s1.p(), s2.p(), acc.p(), n), "AddProduct_cf32");
		for(int i = 0; i < n; ++i)
		{
			double tol = cmul_tol(s1[i], s2[i], fabs(acc0[i].re) + fabs(acc0[i].im));

			c.close("acc.re", i, acc[i].re, acc0[i].re + (double)s1[i].re*s2[i].re - (double)s1[i].im*s2[i].im, tol);
			c.close("acc.im", i, acc[i].im, acc0[i].im + (double)s1[i].re*s2[i].im + (double)s1[i].im*s2[i].re, tol);
		}
		acc.check(c, "acc");
	}
}

TEST(Mul_f32cf32, "Mul_f32cf32")
{
	typedef int (*FN)(const float *, const acf32 *, acf32 *, int);
	FN f = c.fn<FN>("Mul_f32cf32");

	FOR_LENGTHS
	{
		GBuf<float> s1(n);
		GBuf<acf32> s2(n), d(n);

		SETLABEL(c, n);
		fill(c, s1.p(), n);
		fill(c, s2.p(), n);
		c.status(f(s1.p(), s2.p(), d.p(), n), "Mul_f32cf32");
		for(int i = 0; i < n; ++i)
		{
			double wr = (double)s1[i]*s2[i].re, wi = (double)s1[i]*s2[i].im;

			c.close("d.re", i, d[i].re, wr, EPS*fabs(wr));
			c.close("d.im", i, d[i].im, wi, EPS*fabs(wi));
		}
		d.check(c, "d");
	}
}

/* scalar operations */
template<class T> static void real_scal(Ctx &c, const char *name, bool add, double eps)
{
	typedef int (*FN)(const T *, T, T *, int);
	FN f = c.fn<FN>(name);

	FOR_LENGTHS
	{
		GBuf<T> s(n), d(n);
		T v = c.uniform(-100, 100);

		c.label = std::string(name) + " n=" + std::to_string(n);
		fill(c, s.p(), n);
		c.status(f(s.p(), v, d.p(), n), name);
		for(int i = 0; i < n; ++i)
		{
			double want = add ? (double)s[i] + v : (double)s[i]*v;

			c.close("d", i, d[i], want, eps*fabs(want));
		}
		d.check(c, "d");
	}
}

template<class T> static void real_scal_i(Ctx &c, const char *name, int op, double eps)
{
	typedef int (*FN)(T, T *, int);
	FN f = c.fn<FN>(name);

	FOR_LENGTHS
	{
		GBuf<T> sd(n);
		std::vector<T> sd0(n);
		T v = c.uniform(0.5, 100)*(c.irand(0, 1) ? 1 : -1);

		c.label = std::string(name) + " n=" + std::to_string(n);
		fill(c, sd.p(), n);
		memcpy(sd0.data(), sd.p(), n*sizeof(T));
		c.status(f(v, sd.p(), n), name);
		for(int i = 0; i < n; ++i)
		{
			double want = op == 0 ? (double)sd0[i] + v : op == 1 ? (double)sd0[i]*v : (double)sd0[i]/v;

			c.close("sd", i, sd[i], want, eps*fabs(want));
		}
		sd.check(c, "sd");
	}
}

TEST(AddC_f32, "AddC_f32") { real_scal<float>(c, "AddC_f32", true, EPS); }
TEST(AddC_f64, "AddC_f64") { real_scal<double>(c, "AddC_f64", true, DEPS); }
TEST(MulC_f32, "MulC_f32") { real_scal<float>(c, "MulC_f32", false, EPS); }
TEST(MulC_f64, "MulC_f64") { real_scal<double>(c, "MulC_f64", false, DEPS); }
TEST(AddC_f32_I, "AddC_f32_I") { real_scal_i<float>(c, "AddC_f32_I", 0, EPS); }
TEST(AddC_f64_I, "AddC_f64_I") { real_scal_i<double>(c, "AddC_f64_I", 0, DEPS); }
TEST(MulC_f32_I, "MulC_f32_I") { real_scal_i<float>(c, "MulC_f32_I", 1, EPS); }
TEST(MulC_f64_I, "MulC_f64_I") { real_scal_i<double>(c, "MulC_f64_I", 1, DEPS); }
TEST(DivC_f32_I, "DivC_f32_I") { real_scal_i<float>(c, "DivC_f32_I", 2, 2*EPS); }

TEST(AddC_s16_I, "AddC_s16_I")
{
	typedef int (*FN)(int16_t, int16_t *, int);
	FN f = c.fn<FN>("AddC_s16_I");

	FOR_LENGTHS
	{
		GBuf<int16_t> sd(n);
		std::vector<int16_t> sd0(n);
		int16_t v = c.irand(-32768, 32767);

		SETLABEL(c, n);
		fill(c, sd.p(), n);
		memcpy(sd0.data(), sd.p(), n*sizeof(int16_t));
		c.status(f(v, sd.p(), n), "AddC_s16_I");
		for(int i = 0; i < n; ++i)
		{
			c.exact("sd (saturating)", i, sd[i], sat((double)sd0[i] + v, -32768, 32767));
		}
		sd.check(c, "sd");
	}
}

TEST(MulC_cf32, "MulC_cf32")
{
	typedef int (*FN)(const acf32 *, acf32, acf32 *, int);
	FN f = c.fn<FN>("MulC_cf32");

	FOR_LENGTHS
	{
		GBuf<acf32> s(n), d(n);
		acf32 v = { (float)c.uniform(-100, 100), (float)c.uniform(-100, 100) };

		SETLABEL(c, n);
		fill(c, s.p(), n);
		c.status(f(s.p(), v, d.p(), n), "MulC_cf32");
		for(int i = 0; i < n; ++i)
		{
			double tol = cmul_tol(s[i], v, 0);

			c.close("d.re", i, d[i].re, (double)s[i].re*v.re - (double)s[i].im*v.im, tol);
			c.close("d.im", i, d[i].im, (double)s[i].re*v.im + (double)s[i].im*v.re, tol);
		}
		d.check(c, "d");
	}
}

TEST(MulC_cf32_I, "MulC_cf32_I")
{
	typedef int (*FN)(acf32, acf32 *, int);
	FN f = c.fn<FN>("MulC_cf32_I");

	FOR_LENGTHS
	{
		GBuf<acf32> sd(n);
		std::vector<acf32> sd0(n);
		acf32 v = { (float)c.uniform(-100, 100), (float)c.uniform(-100, 100) };

		SETLABEL(c, n);
		fill(c, sd.p(), n);
		memcpy(sd0.data(), sd.p(), n*sizeof(acf32));
		c.status(f(v, sd.p(), n), "MulC_cf32_I");
		for(int i = 0; i < n; ++i)
		{
			double tol = cmul_tol(sd0[i], v, 0);

			c.close("sd.re", i, sd[i].re, (double)sd0[i].re*v.re - (double)sd0[i].im*v.im, tol);
			c.close("sd.im", i, sd[i].im, (double)sd0[i].re*v.im + (double)sd0[i].im*v.re, tol);
		}
		sd.check(c, "sd");
	}
}

TEST(DivC_cf32_I, "DivC_cf32_I")
{
	typedef int (*FN)(acf32, acf32 *, int);
	FN f = c.fn<FN>("DivC_cf32_I");

	FOR_LENGTHS
	{
		GBuf<acf32> sd(n);
		std::vector<acf32> sd0(n);
		acf32 v = { (float)c.uniform(-100, 100), (float)c.uniform(-100, 100) };
		double vv = (double)v.re*v.re + (double)v.im*v.im;

		SETLABEL(c, n);
		fill(c, sd.p(), n);
		memcpy(sd0.data(), sd.p(), n*sizeof(acf32));
		c.status(f(v, sd.p(), n), "DivC_cf32_I");
		for(int i = 0; i < n; ++i)
		{
			double wr = ((double)sd0[i].re*v.re + (double)sd0[i].im*v.im)/vv;
			double wi = ((double)sd0[i].im*v.re - (double)sd0[i].re*v.im)/vv;
			double tol = 4*EPS*sqrt(wr*wr + wi*wi);

			c.close("sd.re", i, sd[i].re, wr, tol);
			c.close("sd.im", i, sd[i].im, wi, tol);
		}
		sd.check(c, "sd");
	}
}

TEST(MulC_cs16_I, "MulC_cs16_I")
{
	typedef int (*FN)(acs16, acs16 *, int);
	FN f = c.fn<FN>("MulC_cs16_I");

	FOR_LENGTHS
	{
		GBuf<acs16> sd(n);
		std::vector<acs16> sd0(n);
		acs16 v = { (int16_t)c.irand(-300, 300), (int16_t)c.irand(-300, 300) };

		SETLABEL(c, n);
		for(int i = 0; i < n; ++i)
		{
			sd[i].re = c.irand(-32768, 32767);
			sd[i].im = c.irand(-32768, 32767);
		}
		memcpy(sd0.data(), sd.p(), n*sizeof(acs16));
		c.status(f(v, sd.p(), n), "MulC_cs16_I");
		for(int i = 0; i < n; ++i)
		{
			c.exact("sd.re (saturating)", i, sd[i].re, sat((double)sd0[i].re*v.re - (double)sd0[i].im*v.im, -32768, 32767));
			c.exact("sd.im (saturating)", i, sd[i].im, sat((double)sd0[i].re*v.im + (double)sd0[i].im*v.re, -32768, 32767));
		}
		sd.check(c, "sd");
	}
}

/* unary maps */
template<class TI, class TO, class REF> static void unary(Ctx &c, const char *name, REF ref, double lo = NAN, double hi = NAN)
{
	typedef int (*FN)(const TI *, TO *, int);
	FN f = c.fn<FN>(name);

	FOR_LENGTHS
	{
		GBuf<TI> s(n);
		GBuf<TO> d(n);

		c.label = std::string(name) + " n=" + std::to_string(n);
		fill(c, s.p(), n, lo, hi);
		c.status(f(s.p(), d.p(), n), name);
		for(int i = 0; i < n; ++i)
		{
			ref(c, s.p(), d.p(), n, i);
		}
		d.check(c, "d");
	}
}

template<class T> static void copy_test(Ctx &c, const char *name)
{
	typedef int (*FN)(const T *, T *, int);
	FN f = c.fn<FN>(name);

	FOR_LENGTHS
	{
		GBuf<T> s(n), d(n);

		c.label = std::string(name) + " n=" + std::to_string(n);
		fill(c, s.p(), n);
		c.status(f(s.p(), d.p(), n), name);
		++c.res.nchecked;
		if(memcmp(s.p(), d.p(), n*sizeof(T)) != 0)
		{
			c.fail("copy differs from source");
		}
		d.check(c, "d");
	}
}

TEST(Copy_u8, "Copy_u8") { copy_test<uint8_t>(c, "Copy_u8"); }
TEST(Copy_s16, "Copy_s16") { copy_test<int16_t>(c, "Copy_s16"); }
TEST(Copy_s32, "Copy_s32") { copy_test<int32_t>(c, "Copy_s32"); }
TEST(Copy_f32, "Copy_f32") { copy_test<float>(c, "Copy_f32"); }
TEST(Copy_f64, "Copy_f64") { copy_test<double>(c, "Copy_f64"); }
TEST(Copy_cf32, "Copy_cf32") { copy_test<acf32>(c, "Copy_cf32"); }

/* Move: overlapping source and destination allowed */
template<class T> static void move_test(Ctx &c, const char *name)
{
	typedef int (*FN)(const T *, T *, int);
	FN f = c.fn<FN>(name);

	FOR_LENGTHS
	{
		for(int shift = -3; shift <= 3; shift += 3)
		{
			GBuf<T> buf(n + 6);
			std::vector<T> orig(n + 6);
			T *src = buf.p() + 3, *dst = buf.p() + 3 + shift;

			c.label = std::string(name) + " n=" + std::to_string(n) + " overlap shift=" + std::to_string(shift);
			fill(c, buf.p(), n + 6);
			memcpy(orig.data(), buf.p(), (n + 6)*sizeof(T));
			c.status(f(src, dst, n), name);
			++c.res.nchecked;
			if(memcmp(dst, &orig[3], n*sizeof(T)) != 0)
			{
				c.fail("destination does not hold the original source");
			}
			buf.check(c, "buf");
		}
	}
}

TEST(Move_f32, "Move_f32") { move_test<float>(c, "Move_f32"); }
TEST(Move_cf32, "Move_cf32") { move_test<acf32>(c, "Move_cf32"); }

TEST(Conj_cf32, "Conj_cf32")
{
	unary<acf32, acf32>(c, "Conj_cf32", [](Ctx &c, const acf32 *s, const acf32 *d, int, int i) {
		c.exact("d.re", i, d[i].re, s[i].re); c.exact("d.im", i, d[i].im, -(double)s[i].im); });
}
TEST(ConjFlip_cf32, "ConjFlip_cf32")
{
	unary<acf32, acf32>(c, "ConjFlip_cf32", [](Ctx &c, const acf32 *s, const acf32 *d, int n, int i) {
		c.exact("d.re", i, d[i].re, s[n-1-i].re); c.exact("d.im", i, d[i].im, -(double)s[n-1-i].im); });
}
TEST(Flip_cf32, "Flip_cf32")
{
	unary<acf32, acf32>(c, "Flip_cf32", [](Ctx &c, const acf32 *s, const acf32 *d, int n, int i) {
		c.exact("d.re", i, d[i].re, s[n-1-i].re); c.exact("d.im", i, d[i].im, s[n-1-i].im); });
}
TEST(Magnitude_cf32, "Magnitude_cf32")
{
	unary<acf32, float>(c, "Magnitude_cf32", [](Ctx &c, const acf32 *s, const float *d, int, int i) {
		double w = hypot((double)s[i].re, (double)s[i].im); c.close("d", i, d[i], w, 2*EPS*w); });
}
TEST(Phase_cf32, "Phase_cf32")
{
	unary<acf32, float>(c, "Phase_cf32", [](Ctx &c, const acf32 *s, const float *d, int, int i) {
		double w = atan2((double)s[i].im, (double)s[i].re); c.close("d", i, d[i], w, 4*EPS*M_PI); });
}
TEST(Real_cf32, "Real_cf32")
{
	unary<acf32, float>(c, "Real_cf32", [](Ctx &c, const acf32 *s, const float *d, int, int i) { c.exact("d", i, d[i], s[i].re); });
}
/* IPP _A21: at least 21 correct bits */
TEST(Cos_f32, "Cos_f32")
{
	unary<float, float>(c, "Cos_f32", [](Ctx &c, const float *s, const float *d, int, int i) {
		double w = cos((double)s[i]); c.close("d", i, d[i], w, 1e-6 + 4*EPS*fabs(w)); }, -50, 50);
}
TEST(Sin_f32, "Sin_f32")
{
	unary<float, float>(c, "Sin_f32", [](Ctx &c, const float *s, const float *d, int, int i) {
		double w = sin((double)s[i]); c.close("d", i, d[i], w, 1e-6 + 4*EPS*fabs(w)); }, -50, 50);
}
TEST(Convert_s16f32, "Convert_s16f32")
{
	unary<int16_t, float>(c, "Convert_s16f32", [](Ctx &c, const int16_t *s, const float *d, int, int i) { c.exact("d", i, d[i], s[i]); });
}
TEST(Convert_s32f32, "Convert_s32f32")
{
	unary<int32_t, float>(c, "Convert_s32f32", [](Ctx &c, const int32_t *s, const float *d, int, int i) { c.exact("d", i, d[i], (float)s[i]); });
}
TEST(Convert_f64f32, "Convert_f64f32")
{
	unary<double, float>(c, "Convert_f64f32", [](Ctx &c, const double *s, const float *d, int, int i) { c.exact("d", i, d[i], (float)s[i]); }, -1e6, 1e6);
}

TEST(SinCos_f32, "SinCos_f32")
{
	typedef int (*FN)(const float *, float *, float *, int);
	FN f = c.fn<FN>("SinCos_f32");

	FOR_LENGTHS
	{
		GBuf<float> s(n), sn(n), cs(n);

		SETLABEL(c, n);
		fill(c, s.p(), n, -50, 50);
		c.status(f(s.p(), sn.p(), cs.p(), n), "SinCos_f32");
		for(int i = 0; i < n; ++i)
		{
			double ws = sin((double)s[i]), wc = cos((double)s[i]);

			c.close("sin", i, sn[i], ws, 1e-6 + 4*EPS*fabs(ws));
			c.close("cos", i, cs[i], wc, 1e-6 + 4*EPS*fabs(wc));
		}
		sn.check(c, "sin");
		cs.check(c, "cos");
	}
}

/* in place unary */
template<class T, class REF> static void unary_i(Ctx &c, const char *name, REF ref, double lo = NAN, double hi = NAN)
{
	typedef int (*FN)(T *, int);
	FN f = c.fn<FN>(name);

	FOR_LENGTHS
	{
		GBuf<T> sd(n);
		std::vector<T> s0(n);

		c.label = std::string(name) + " n=" + std::to_string(n);
		fill(c, sd.p(), n, lo, hi);
		memcpy(s0.data(), sd.p(), n*sizeof(T));
		c.status(f(sd.p(), n), name);
		for(int i = 0; i < n; ++i)
		{
			ref(c, s0.data(), sd.p(), n, i);
		}
		sd.check(c, "sd");
	}
}

TEST(Conj_cf32_I, "Conj_cf32_I")
{
	unary_i<acf32>(c, "Conj_cf32_I", [](Ctx &c, const acf32 *s, const acf32 *d, int, int i) {
		c.exact("sd.re", i, d[i].re, s[i].re); c.exact("sd.im", i, d[i].im, -(double)s[i].im); });
}
TEST(Flip_f64_I, "Flip_f64_I")
{
	unary_i<double>(c, "Flip_f64_I", [](Ctx &c, const double *s, const double *d, int n, int i) { c.exact("sd", i, d[i], s[n-1-i]); });
}
TEST(Square_f32_I, "Square_f32_I")
{
	unary_i<float>(c, "Square_f32_I", [](Ctx &c, const float *s, const float *d, int, int i) {
		double w = (double)s[i]*s[i]; c.close("sd", i, d[i], w, EPS*w); });
}
TEST(Square_f64_I, "Square_f64_I")
{
	unary_i<double>(c, "Square_f64_I", [](Ctx &c, const double *s, const double *d, int, int i) {
		double w = s[i]*s[i]; c.close("sd", i, d[i], w, DEPS*w); });
}
TEST(Sqrt_f32_I, "Sqrt_f32_I")
{
	unary_i<float>(c, "Sqrt_f32_I", [](Ctx &c, const float *s, const float *d, int, int i) {
		double w = sqrt((double)s[i]); c.close("sd", i, d[i], w, EPS*w); }, 0, 1e4);
}
TEST(Abs_f32_I, "Abs_f32_I")
{
	unary_i<float>(c, "Abs_f32_I", [](Ctx &c, const float *s, const float *d, int, int i) { c.exact("sd", i, d[i], fabs((double)s[i])); });
}

/* fill operations */
template<class T> static void zero_test(Ctx &c, const char *name)
{
	typedef int (*FN)(T *, int);
	FN f = c.fn<FN>(name);

	FOR_LENGTHS
	{
		GBuf<T> d(n);
		std::vector<unsigned char> zero(n*sizeof(T), 0);

		c.label = std::string(name) + " n=" + std::to_string(n);
		memset(d.p(), 0x5C, n*sizeof(T));
		c.status(f(d.p(), n), name);
		++c.res.nchecked;
		if(memcmp(d.p(), zero.data(), n*sizeof(T)) != 0)
		{
			c.fail("not all zero");
		}
		d.check(c, "d");
	}
}

TEST(Zero_u8, "Zero_u8") { zero_test<uint8_t>(c, "Zero_u8"); }
TEST(Zero_s16, "Zero_s16") { zero_test<int16_t>(c, "Zero_s16"); }
TEST(Zero_s32, "Zero_s32") { zero_test<int32_t>(c, "Zero_s32"); }
TEST(Zero_f32, "Zero_f32") { zero_test<float>(c, "Zero_f32"); }
TEST(Zero_cf32, "Zero_cf32") { zero_test<acf32>(c, "Zero_cf32"); }
TEST(Zero_cf64, "Zero_cf64") { zero_test<acf64>(c, "Zero_cf64"); }

TEST(Set_f32, "Set_f32")
{
	typedef int (*FN)(float, float *, int);
	FN f = c.fn<FN>("Set_f32");

	FOR_LENGTHS
	{
		GBuf<float> d(n);
		float v = c.uniform(-100, 100);

		SETLABEL(c, n);
		c.status(f(v, d.p(), n), "Set_f32");
		for(int i = 0; i < n; ++i)
		{
			c.exact("d", i, d[i], v);
		}
		d.check(c, "d");
	}
}

TEST(RealToComplex_f32, "RealToComplex_f32")
{
	typedef int (*FN)(const float *, const float *, acf32 *, int);
	FN f = c.fn<FN>("RealToComplex_f32");

	FOR_LENGTHS
	{
		for(int which = 0; which < 3; ++which)	/* both, real only (im NULL), imaginary only (re NULL) */
		{
			GBuf<float> re(n), im(n);
			GBuf<acf32> d(n);

			c.label = "RealToComplex_f32 n=" + std::to_string(n) + (which == 1 ? " im=NULL" : which == 2 ? " re=NULL" : "");
			fill(c, re.p(), n);
			fill(c, im.p(), n);
			c.status(f(which == 2 ? 0 : re.p(), which == 1 ? 0 : im.p(), d.p(), n), "RealToComplex_f32");
			for(int i = 0; i < n; ++i)
			{
				c.exact("d.re", i, d[i].re, which == 2 ? 0.0 : re[i]);
				c.exact("d.im", i, d[i].im, which == 1 ? 0.0 : im[i]);
			}
			d.check(c, "d");
		}
	}
}

/* conversions with scaling, rounding and saturation */
TEST(ConvertScaled_s16f32, "ConvertScaled_s16f32")
{
	typedef int (*FN)(const int16_t *, float *, int, int);
	FN f = c.fn<FN>("ConvertScaled_s16f32");

	FOR_LENGTHS
	{
		for(int sf = -3; sf <= 3; sf += 3)
		{
			GBuf<int16_t> s(n);
			GBuf<float> d(n);

			c.label = "ConvertScaled_s16f32 n=" + std::to_string(n) + " scale=" + std::to_string(sf);
			fill(c, s.p(), n);
			c.status(f(s.p(), d.p(), n, sf), "ConvertScaled_s16f32");
			for(int i = 0; i < n; ++i)
			{
				c.exact("d", i, d[i], ldexp((double)s[i], -sf));
			}
			d.check(c, "d");
		}
	}
}

template<class TO> static void convert_f32_int(Ctx &c, const char *name, double lo, double hi, bool scaled)
{
	FOR_LENGTHS
	{
		for(int rnd = ARCH_RND_ZERO; rnd <= ARCH_RND_NEAR; ++rnd)
		{
			for(int sf = scaled ? -2 : 0; sf <= (scaled ? 2 : 0); sf += 2)
			{
				GBuf<float> s(n);
				GBuf<TO> d(n);
				int st;

				c.label = std::string(name) + " n=" + std::to_string(n) + (rnd ? " RndNear" : " RndZero") + " scale=" + std::to_string(sf);
				/* in and beyond range, with exact halves to check rounding of ties */
				for(int i = 0; i < n; ++i)
				{
					double v = c.uniform(1.2*lo, 1.2*hi);

					s[i] = (i % 4 == 1) ? (float)(floor(v) + 0.5) : (float)v;
					s[i] = ldexp((double)s[i], sf);	/* so that after scaling the ties remain */
				}
				if(scaled)
				{
					typedef int (*FN)(const float *, TO *, int, int, int);
					st = c.fn<FN>(name)(s.p(), d.p(), n, rnd, sf);
				}
				else
				{
					typedef int (*FN)(const float *, TO *, int, int);
					st = c.fn<FN>(name)(s.p(), d.p(), n, rnd);
				}
				c.status(st, name);
				for(int i = 0; i < n; ++i)
				{
					c.exact("d (rounded, saturated)", i, d[i], sat(ipp_round(ldexp((double)s[i], -sf), rnd), lo, hi));
				}
				d.check(c, "d");
			}
		}
	}
}

TEST(ConvertScaled_f32s16, "ConvertScaled_f32s16") { convert_f32_int<int16_t>(c, "ConvertScaled_f32s16", -32768, 32767, true); }
TEST(ConvertScaled_f32u8, "ConvertScaled_f32u8") { convert_f32_int<uint8_t>(c, "ConvertScaled_f32u8", 0, 255, true); }
TEST(Convert_f32s32, "Convert_f32s32") { convert_f32_int<int32_t>(c, "Convert_f32s32", INT32_MIN, INT32_MAX, false); }

TEST(SplitScaled_s16f32, "SplitScaled_s16f32")
{
	typedef int (*FN)(const int16_t *, float **, int, int);
	FN f = c.fn<FN>("SplitScaled_s16f32");

	for(int nchan = 1; nchan <= 4; ++nchan)
	{
		FOR_LENGTHS
		{
			GBuf<int16_t> s(n*nchan);
			std::vector<GBuf<float> *> d;
			std::vector<float *> dp;

			c.label = "SplitScaled_s16f32 nchan=" + std::to_string(nchan) + " chanlen=" + std::to_string(n);
			fill(c, s.p(), n*nchan);
			for(int k = 0; k < nchan; ++k)
			{
				d.push_back(new GBuf<float>(n));
				dp.push_back(d.back()->p());
			}
			c.status(f(s.p(), dp.data(), nchan, n), "SplitScaled_s16f32");
			/* de-interleave and map [MIN_S16, MAX_S16] linearly to [-1, 1] */
			for(int k = 0; k < nchan; ++k)
			{
				for(int i = 0; i < n; ++i)
				{
					double w = -1.0 + 2.0*((double)s[i*nchan + k] + 32768)/65535.0;

					c.close("d", i, (*d[k])[i], w, 4*EPS);
				}
				d[k]->check(c, "d");
				delete d[k];
			}
		}
	}
}

/* reductions */
TEST(Sum_cf32, "Sum_cf32")
{
	typedef int (*FN)(const acf32 *, int, acf32 *);
	FN f = c.fn<FN>("Sum_cf32");

	FOR_LENGTHS
	{
		GBuf<acf32> s(n);
		acf32 sum;
		double wr = 0, wi = 0, mag = 0;

		SETLABEL(c, n);
		fill(c, s.p(), n);
		for(int i = 0; i < n; ++i)
		{
			wr += s[i].re;
			wi += s[i].im;
			mag += fabs(s[i].re) + fabs(s[i].im);
		}
		c.status(f(s.p(), n, &sum), "Sum_cf32");
		c.close("sum.re", 0, sum.re, wr, (n + 1)*EPS*mag);
		c.close("sum.im", 0, sum.im, wi, (n + 1)*EPS*mag);
	}
}

TEST(Mean_cf32, "Mean_cf32")
{
	typedef int (*FN)(const acf32 *, int, acf32 *);
	FN f = c.fn<FN>("Mean_cf32");

	FOR_LENGTHS
	{
		GBuf<acf32> s(n);
		acf32 m;
		double wr = 0, wi = 0, mag = 0;

		SETLABEL(c, n);
		fill(c, s.p(), n);
		for(int i = 0; i < n; ++i)
		{
			wr += s[i].re;
			wi += s[i].im;
			mag += fabs(s[i].re) + fabs(s[i].im);
		}
		c.status(f(s.p(), n, &m), "Mean_cf32");
		c.close("mean.re", 0, m.re, wr/n, (n + 2)*EPS*mag/n);
		c.close("mean.im", 0, m.im, wi/n, (n + 2)*EPS*mag/n);
	}
}

TEST(DotProduct_f64, "DotProduct_f64")
{
	typedef int (*FN)(const double *, const double *, int, double *);
	FN f = c.fn<FN>("DotProduct_f64");

	FOR_LENGTHS
	{
		GBuf<double> s1(n), s2(n);
		double out, w = 0, mag = 0;

		SETLABEL(c, n);
		fill(c, s1.p(), n);
		fill(c, s2.p(), n);
		for(int i = 0; i < n; ++i)
		{
			w += (long double)s1[i]*s2[i];
			mag += fabs(s1[i]*s2[i]);
		}
		c.status(f(s1.p(), s2.p(), n, &out), "DotProduct_f64");
		c.close("dot", 0, out, w, (n + 1)*DEPS*mag);
	}
}

TEST(StdDev_f32, "StdDev_f32")
{
	typedef int (*FN)(const float *, int, float *);
	FN f = c.fn<FN>("StdDev_f32");

	FOR_LENGTHS
	{
		GBuf<float> s(n);
		float sd;
		double mean = 0, ss = 0;

		if(n < 2)
		{
			continue;	/* sample standard deviation needs 2 or more */
		}
		SETLABEL(c, n);
		fill(c, s.p(), n, 10, 50);
		for(int i = 0; i < n; ++i)
		{
			mean += s[i];
		}
		mean /= n;
		for(int i = 0; i < n; ++i)
		{
			ss += (s[i] - mean)*(s[i] - mean);
		}
		c.status(f(s.p(), n, &sd), "StdDev_f32");
		c.close("stddev", 0, sd, sqrt(ss/(n - 1)), 1e-5*sqrt(ss/(n - 1)) + 1e-6);
	}
}

template<bool MAX> static void maxmin(Ctx &c, const char *name, double lo, double hi)
{
	typedef int (*FN)(const float *, int, float *);
	FN f = c.fn<FN>(name);

	FOR_LENGTHS
	{
		GBuf<float> s(n);
		float m;
		double w;

		c.label = std::string(name) + " n=" + std::to_string(n) + " range " + std::to_string(lo) + ".." + std::to_string(hi);
		fill(c, s.p(), n, lo, hi);
		w = s[0];
		for(int i = 1; i < n; ++i)
		{
			w = MAX ? std::max(w, (double)s[i]) : std::min(w, (double)s[i]);
		}
		c.status(f(s.p(), n, &m), name);
		c.exact(MAX ? "max" : "min", 0, m, w);
	}
}

TEST(Max_f32, "Max_f32") { maxmin<true>(c, "Max_f32", -100, 100); maxmin<true>(c, "Max_f32", -1e36, -1e34); }
TEST(Min_f32, "Min_f32") { maxmin<false>(c, "Min_f32", -100, 100); maxmin<false>(c, "Min_f32", 1e34, 1e36); }

TEST(MaxIndx_f32, "MaxIndx_f32")
{
	typedef int (*FN)(const float *, int, float *, int *);
	FN f = c.fn<FN>("MaxIndx_f32");

	for(int r = 0; r < 2; ++r)
	{
		FOR_LENGTHS
		{
			GBuf<float> s(n);
			float m;
			int idx = -99, widx = 0;

			c.label = "MaxIndx_f32 n=" + std::to_string(n) + (r ? " values < -1e30" : "");
			fill(c, s.p(), n, r ? -1e36 : -100, r ? -1e34 : 100);
			if(n > 4)
			{
				s[n/2] = s[n/4] = 1e3*(r ? -1e31 : 1);	/* tie: first index wins */
			}
			for(int i = 1; i < n; ++i)
			{
				if(s[i] > s[widx])
				{
					widx = i;
				}
			}
			c.status(f(s.p(), n, &m, &idx), "MaxIndx_f32");
			c.exact("max", 0, m, s[widx]);
			c.exact("index", 0, idx, widx);
		}
	}
}

/* allocation: usable memory of the requested size */
template<class T> static void alloc_test(Ctx &c, const char *t)
{
	typedef int (*FA)(int, void **);
	typedef int (*FF)(void *);
	FA fa = c.fn<FA>((std::string("Alloc_") + t).c_str());
	FF ff = c.fn<FF>((std::string("Free_") + t).c_str());

	FOR_LENGTHS
	{
		void *p = 0;

		c.label = std::string("Alloc_") + t + " n=" + std::to_string(n);
		c.status(fa(n, &p), "Alloc");
		++c.res.nchecked;
		if(!p)
		{
			c.fail("null pointer");
			continue;
		}
		memset(p, 0x3C, n*sizeof(T));
		c.status(ff(p), "Free");
	}
}

TEST(Alloc_u8, "Alloc_u8", "Free_u8") { alloc_test<uint8_t>(c, "u8"); }
TEST(Alloc_s16, "Alloc_s16", "Free_s16") { alloc_test<int16_t>(c, "s16"); }
TEST(Alloc_cs16, "Alloc_cs16", "Free_cs16") { alloc_test<acs16>(c, "cs16"); }
TEST(Alloc_s32, "Alloc_s32", "Free_s32") { alloc_test<int32_t>(c, "s32"); }
TEST(Alloc_f32, "Alloc_f32", "Free_f32") { alloc_test<float>(c, "f32"); }
TEST(Alloc_cf32, "Alloc_cf32", "Free_cf32") { alloc_test<acf32>(c, "cf32"); }
TEST(Alloc_f64, "Alloc_f64", "Free_f64") { alloc_test<double>(c, "f64"); }
TEST(Alloc_cf64, "Alloc_cf64", "Free_cf64") { alloc_test<acf64>(c, "cf64"); }

/* ---------------------------------------------------------------- FFTs */

/* reference DFT in double: X[k] = sum x[j] exp(-2 pi i jk/N) */
static void ref_dft(const std::vector<double> &xr, const std::vector<double> &xi, std::vector<double> &Xr, std::vector<double> &Xi, int sign = -1)
{
	int N = xr.size();
	std::vector<double> cs(N), sn(N);

	for(int k = 0; k < N; ++k)
	{
		cs[k] = cos(2*M_PI*k/N);
		sn[k] = sign*sin(2*M_PI*k/N);
	}
	Xr.assign(N, 0);
	Xi.assign(N, 0);
	for(int k = 0; k < N; ++k)
	{
		long double ar = 0, ai = 0;

		for(int j = 0; j < N; ++j)
		{
			int m = (int)(((long long)j*k) % N);

			ar += xr[j]*cs[m] - xi[j]*sn[m];
			ai += xr[j]*sn[m] + xi[j]*cs[m];
		}
		Xr[k] = ar;
		Xi[k] = ai;
	}
}

static double fft_tol(int N, double norm2)
{
	return 8*EPS*(log2((double)N) + 1)*norm2;
}

typedef int (*fn_fft_init)(int, int, void **, unsigned char **, int *);
typedef int (*fn_fft_free)(void *, unsigned char *);

/* init must set the work buffer pointer and size (to something, possibly 0) */
static void check_init_outputs(Ctx &c, unsigned char *buf, int bufsize)
{
	++c.res.nchecked;
	if(buf == ARCH_UNSET_PTR || bufsize == ARCH_UNSET_INT)
	{
		c.fail("init did not set the work buffer pointer/size (%s%s)", buf == ARCH_UNSET_PTR ? "pointer" : "", bufsize == ARCH_UNSET_INT ? " size" : "");
	}
}

static void complex_fft_test(Ctx &c, const char *name, bool isorder, std::vector<int> sizes)
{
	fn_fft_init init = c.fn<fn_fft_init>((std::string(name) + "_init").c_str());
	typedef int (*FN)(void *, unsigned char *, const acf32 *, acf32 *);
	FN fwd = c.fn<FN>((std::string(name) + "_fwd").c_str());
	fn_fft_free fr = c.fn<fn_fft_free>((std::string(name) + "_free").c_str());

	for(int sz : sizes)
	{
		int N = isorder ? 1 << sz : sz;

		/* shift: source offset by one element, so not aligned as FFTW's own
		 * arrays are (mpifxcorr's real FFTs start at arbitrary samples) */
		for(int flag = ARCH_FFT_NORENORM; flag <= ARCH_FFT_RENORM; ++flag)
		for(int shift = 0; shift <= 1; ++shift)
		{
			void *spec = 0;
			unsigned char *buf = 0;
			int bufsize = 0;
			GBuf<acf32> xmem(N + 1), X(N);
			acf32 *xp = xmem.p() + shift;
			std::vector<double> xr(N), xi(N), Xr, Xi;
			double norm2 = 0;

			c.label = std::string(name) + " N=" + std::to_string(N) + (flag ? " ReNorm" : " NoReNorm") + (shift ? " offset source" : "");
			c.status(init(sz, flag, &spec, &buf, &bufsize), "init");
			check_init_outputs(c, buf, bufsize);
			fill(c, xp, N, -1, 1);
			for(int j = 0; j < N; ++j)
			{
				xr[j] = xp[j].re;
				xi[j] = xp[j].im;
				norm2 += xr[j]*xr[j] + xi[j]*xi[j];
			}
			norm2 = sqrt(norm2);
			ref_dft(xr, xi, Xr, Xi);
			/* forward transform: unnormalised with either flag */
			c.status(fwd(spec, buf == ARCH_UNSET_PTR ? 0 : buf, xp, X.p()), "forward");
			for(int j = 0; j < N; ++j)
			{
				++c.res.nchecked;
				if(xp[j].re != xr[j] || xp[j].im != xi[j])
				{
					c.fail("forward transform changed its input at %d", j);
					break;
				}
			}
			for(int k = 0; k < N; ++k)
			{
				c.close("X.re", k, X[k].re, Xr[k], fft_tol(N, norm2));
				c.close("X.im", k, X[k].im, Xi[k], fft_tol(N, norm2));
			}
			X.check(c, "X");
			fr(spec, buf);
		}
	}
}

TEST(FFTC_cf32, "FFTC_cf32_init", "FFTC_cf32_fwd", "FFTC_cf32_free") { complex_fft_test(c, "FFTC_cf32", true, {1, 2, 3, 5, 8, 10, 12}); }
TEST(DFTC_cf32, "DFTC_cf32_init", "DFTC_cf32_fwd", "DFTC_cf32_free") { complex_fft_test(c, "DFTC_cf32", false, {1, 5, 12, 100, 231, 1000, 1024}); }

/* real forward transforms: CCS output, N/2+1 complex values */
static void real_fft_test(Ctx &c, const char *name, bool isorder, std::vector<int> sizes, bool inverse)
{
	fn_fft_init init = c.fn<fn_fft_init>((std::string(name) + "_init").c_str());
	typedef int (*FN)(void *, unsigned char *, const float *, float *);
	FN fwd = c.fn<FN>((std::string(name) + "_fwd").c_str());
	FN inv = inverse ? c.fn<FN>((std::string(name) + "_inv").c_str()) : 0;
	fn_fft_free fr = c.fn<fn_fft_free>((std::string(name) + "_free").c_str());

	for(int sz : sizes)
	{
		int N = isorder ? 1 << sz : sz;

		for(int flag = ARCH_FFT_NORENORM; flag <= ARCH_FFT_RENORM; ++flag)
		for(int shift = 0; shift <= 1; ++shift)
		{
			void *spec = 0;
			unsigned char *buf = 0;
			int bufsize = 0;
			GBuf<float> xmem(N + 1), X(N + 2), y(N);
			float *xp = xmem.p() + shift;
			std::vector<double> xr(N), xi(N, 0.0), Xr, Xi;
			double norm2 = 0;

			c.label = std::string(name) + " N=" + std::to_string(N) + (flag ? " ReNorm" : " NoReNorm") + (shift ? " offset source" : "");
			c.status(init(sz, flag, &spec, &buf, &bufsize), "init");
			check_init_outputs(c, buf, bufsize);
			fill(c, xp, N, -1, 1);
			for(int j = 0; j < N; ++j)
			{
				xr[j] = xp[j];
				norm2 += xr[j]*xr[j];
			}
			norm2 = sqrt(norm2);
			ref_dft(xr, xi, Xr, Xi);
			c.status(fwd(spec, buf == ARCH_UNSET_PTR ? 0 : buf, xp, X.p()), "forward");
			for(int j = 0; j < N; ++j)
			{
				++c.res.nchecked;
				if(xp[j] != xr[j])
				{
					c.fail("forward transform changed its input at %d", j);
					break;
				}
			}
			for(int k = 0; k <= N/2; ++k)
			{
				c.close("X.re (CCS)", k, X[2*k], Xr[k], fft_tol(N, norm2));
				c.close("X.im (CCS)", k, X[2*k + 1], Xi[k], fft_tol(N, norm2));
			}
			X.check(c, "X");

			if(inv)
			{
				/* inverse of the exact spectrum: N x unnormalised, x with DIV_INV_BY_N */
				GBuf<float> Xin(N + 2);
				double scale = flag ? 1.0 : N;

				c.label += " inverse";
				for(int k = 0; k <= N/2; ++k)
				{
					Xin[2*k] = Xr[k];
					Xin[2*k + 1] = Xi[k];
				}
				c.status(inv(spec, buf == ARCH_UNSET_PTR ? 0 : buf, Xin.p(), y.p()), "inverse");
				for(int j = 0; j < N; ++j)
				{
					c.close("y", j, y[j], scale*xr[j], scale*fft_tol(N, norm2));
				}
				y.check(c, "y");
			}
			fr(spec, buf);
		}
	}
}

TEST(FFTR_f32, "FFTR_f32_init", "FFTR_f32_fwd", "FFTR_f32_free") { real_fft_test(c, "FFTR_f32", true, {1, 2, 3, 6, 10, 12}, false); }
TEST(FFTR_f32_inverse, "FFTR_f32_init", "FFTR_f32_fwd", "FFTR_f32_inv", "FFTR_f32_free") { real_fft_test(c, "FFTR_f32", true, {2, 6, 10}, true); }
TEST(DFTR_f32, "DFTR_f32_init", "DFTR_f32_fwd", "DFTR_f32_free") { real_fft_test(c, "DFTR_f32", false, {2, 6, 15, 100, 1000, 1024}, false); }

/* generic only: inverse real DFT with its own spec */
TEST(DFTCR_f32, "DFTCR_f32_init", "DFTCR_f32_inv", "DFTCR_f32_free")
{
	fn_fft_init init = c.fn<fn_fft_init>("DFTCR_f32_init");
	typedef int (*FN)(void *, unsigned char *, const float *, float *);
	FN inv = c.fn<FN>("DFTCR_f32_inv");
	fn_fft_free fr = c.fn<fn_fft_free>("DFTCR_f32_free");

	for(int N : {4, 64, 1000})
	{
		void *spec = 0;
		unsigned char *buf = 0;
		int bufsize = 0;
		GBuf<float> x(N), Xin(N + 2), y(N);
		std::vector<double> xr(N), xi(N, 0.0), Xr, Xi;
		double norm2 = 0;

		c.label = "DFTCR_f32 N=" + std::to_string(N);
		c.status(init(N, ARCH_FFT_NORENORM, &spec, &buf, &bufsize), "init");
		fill(c, x.p(), N, -1, 1);
		for(int j = 0; j < N; ++j)
		{
			xr[j] = x[j];
			norm2 += xr[j]*xr[j];
		}
		norm2 = sqrt(norm2);
		ref_dft(xr, xi, Xr, Xi);
		for(int k = 0; k <= N/2; ++k)
		{
			Xin[2*k] = Xr[k];
			Xin[2*k + 1] = Xi[k];
		}
		c.status(inv(spec, buf, Xin.p(), y.p()), "inverse");
		for(int j = 0; j < N; ++j)
		{
			c.close("y", j, y[j], N*x[j], N*fft_tol(N, norm2));
		}
		y.check(c, "y");
		fr(spec, buf);
	}
}

TEST(FFT2D_cf32, "FFT2D_cf32_init", "FFT2D_cf32_fwd", "FFT2D_cf32_free")
{
	typedef int (*FI)(int, int, int, void **);
	typedef int (*FN)(void *, unsigned char *, const acf32 *, int, acf32 *, int);
	typedef int (*FF)(void *);
	FI init = c.fn<FI>("FFT2D_cf32_init");
	FN fwd = c.fn<FN>("FFT2D_cf32_fwd");
	FF fr = c.fn<FF>("FFT2D_cf32_free");
	int ox = 3, oy = 2, NX = 1 << ox, NY = 1 << oy;
	void *spec = 0;
	GBuf<acf32> x(NX*NY), X(NX*NY);
	double norm2 = 0;

	c.label = "FFT2D_cf32 " + std::to_string(NX) + "x" + std::to_string(NY);
	c.status(init(ox, oy, ARCH_FFT_NORENORM, &spec), "init");
	fill(c, x.p(), NX*NY, -1, 1);
	c.status(fwd(spec, 0, x.p(), NX*sizeof(acf32), X.p(), NX*sizeof(acf32)), "forward");
	/* reference: rows of length NX (x fastest), NY rows */
	for(int j = 0; j < NX*NY; ++j)
	{
		norm2 += x[j].re*x[j].re + x[j].im*x[j].im;
	}
	norm2 = sqrt(norm2);
	for(int ky = 0; ky < NY; ++ky)
	{
		for(int kx = 0; kx < NX; ++kx)
		{
			double ar = 0, ai = 0;

			for(int y = 0; y < NY; ++y)
			{
				for(int xx = 0; xx < NX; ++xx)
				{
					double ph = -2*M_PI*((double)kx*xx/NX + (double)ky*y/NY);
					acf32 v = x[y*NX + xx];

					ar += v.re*cos(ph) - v.im*sin(ph);
					ai += v.re*sin(ph) + v.im*cos(ph);
				}
			}
			c.close("X.re", ky*NX + kx, X[ky*NX + kx].re, ar, fft_tol(NX*NY, norm2));
			c.close("X.im", ky*NX + kx, X[ky*NX + kx].im, ai, fft_tol(NX*NY, norm2));
		}
	}
	X.check(c, "X");
	fr(spec);
}

/* ---------------------------------------------------------------- main */

/* Library bugs known but not yet fixed: reported as xfail, see README */
static const char *known_failure(const std::string &impl, const std::string &test)
{
	static const std::map<std::string, const char *> generic = {
		{ "Add_s16_I", "genericAdd_16s_I wraps on overflow; IPP saturates" },
		{ "AddC_s16_I", "genericAddC_16s_I wraps on overflow; IPP saturates" },
		{ "Sub_s32", "genericSub_32s wraps on overflow; IPP saturates" },
		{ "Flip_f64_I", "genericFlip_64f_I swaps every pair twice, so leaves the data unchanged" },
		{ "ConvertScaled_f32s16", "genericConvert_32f16s ignores the rounding mode (truncates) and does not saturate" },
		{ "ConvertScaled_f32u8", "genericConvert_32f8u ignores the rounding mode (truncates) and does not saturate" },
		{ "StdDev_f32", "genericStdDev_32f returns sqrt(sum)/(n-1) instead of sqrt(sum/(n-1))" },
		{ "Max_f32", "genericMax_32f starts from -1e30, wrong if all values are below that" },
		{ "Min_f32", "genericMin_32f starts from 1e30, wrong if all values are above that" },
		{ "MaxIndx_f32", "genericMaxIndx_32f starts from -1e30, wrong if all values are below that" },
	};

	if(impl == "generic")
	{
		std::map<std::string, const char *>::const_iterator it = generic.find(test);

		return it == generic.end() ? 0 : it->second;
	}

	return 0;
}

/* Run one test in a child process, so that a crash in the code under test
 * is reported rather than ending the run.  Returns the signal that killed
 * the child, or 0. */
static int run_isolated(const Test &t, Ctx &c)
{
	int fd[2];
	pid_t pid;
	int wstatus = 0;

	fflush(stdout);
	if(pipe(fd) != 0 || (pid = fork()) < 0)
	{
		t.run(c);	/* fall back to running in this process */

		return 0;
	}
	if(pid == 0)
	{
		char msg[1024];

		close(fd[0]);
		t.run(c);
		fflush(stdout);
		snprintf(msg, sizeof(msg), "%lld %lld %s", c.res.nchecked, c.res.nbad, c.res.first.c_str());
		if(write(fd[1], msg, strlen(msg)) < 0)
		{
			_exit(2);
		}
		close(fd[1]);
		_exit(0);
	}
	close(fd[1]);
	{
		std::string msg;
		char buf[1024];
		ssize_t k;

		while((k = read(fd[0], buf, sizeof(buf))) > 0)
		{
			msg.append(buf, k);
		}
		close(fd[0]);
		waitpid(pid, &wstatus, 0);
		if(WIFSIGNALED(wstatus))
		{
			return WTERMSIG(wstatus);
		}
		{
			long long nchecked = 0, nbad = 0;
			int pos = 0;

			if(sscanf(msg.c_str(), "%lld %lld %n", &nchecked, &nbad, &pos) >= 2)
			{
				c.res.nchecked = nchecked;
				c.res.nbad = nbad;
				c.res.first = msg.substr(pos);
			}
		}
	}

	return 0;
}

static bool match(const char *name, const char *patterns)
{
	char buf[1024];
	char *tok, *save = 0;

	if(!patterns)
	{
		return true;
	}
	snprintf(buf, sizeof(buf), "%s", patterns);
	for(tok = strtok_r(buf, ",", &save); tok; tok = strtok_r(0, ",", &save))
	{
		if(fnmatch(tok, name, 0) == 0)
		{
			return true;
		}
	}

	return false;
}

static void usage(const char *pgm)
{
	printf("Usage: %s [options]\n\n", pgm);
	printf("Tests the vector routines of mpifxcorr's architecture.h in each implementation\n");
	printf("built (generic; IPP if available) against a reference model of IPP behaviour.\n\n");
	printf("Options:\n");
	printf("  -m <patterns>  only routines matching comma separated glob patterns\n");
	printf("  -l             list routines and their availability, then exit\n");
	printf("  -s <seed>      random seed [default 1]\n");
	printf("  -r <n>         report up to n mismatches per failing test [default 1]\n");
	printf("  -x             strict: known failures count as failures\n");
	printf("  -h             this help\n");
}

int main(int argc, char **argv)
{
	Options o;
	std::vector<const ArchImpl *> impls;
	int opt, listonly = 0, nfail = 0, npass = 0, nxfail = 0, nxpass = 0, nnocompile = 0, nabsent = 0;

	memset(&o, 0, sizeof(o));
	o.seed = 1;
	o.maxreport = 1;
	while((opt = getopt(argc, argv, "m:ls:r:xh")) != -1)
	{
		switch(opt)
		{
		case 'm': o.patterns = optarg; break;
		case 'l': listonly = 1; break;
		case 's': o.seed = strtoull(optarg, 0, 0); break;
		case 'r': o.maxreport = atoi(optarg); break;
		case 'x': o.strict = 1; break;
		case 'h': usage(argv[0]); return EXIT_SUCCESS;
		default: usage(argv[0]); return EXIT_FAILURE;
		}
	}

	impls.push_back(&arch_generic_impl());
#ifdef HAVE_IPP
	impls.push_back(&arch_ipp_impl());
#endif

	printf("Testing architecture.h routines: %s", impls[0]->name.c_str());
	for(size_t k = 1; k < impls.size(); ++k)
	{
		printf(", %s", impls[k]->name.c_str());
	}
	printf("%s (seed %llu)\n\n", impls.size() == 1 ? " (IPP not available here)" : "", o.seed);

	printf("%-22s", "routine");
	for(const ArchImpl *impl : impls)
	{
		printf("  %-10s", impl->name.c_str());
	}
	printf("\n");

	for(const Test &t : tests())
	{
		std::vector<std::string> details;

		if(!match(t.name, o.patterns))
		{
			continue;
		}
		printf("%-22s", t.name);
		for(const ArchImpl *impl : impls)
		{
			std::string missing, status;
			bool broken = false;

			for(const char *d : t.deps)
			{
				if(!impl->get(d))
				{
					std::map<std::string, int>::const_iterator it = impl->skipped.find(d);

					missing = d;
					broken = (it != impl->skipped.end() && it->second == ARCH_SKIP_BROKEN);
					break;
				}
			}
			if(!missing.empty())
			{
				status = broken ? "NOCOMPILE" : "absent";
				broken ? ++nnocompile : ++nabsent;
				details.push_back(impl->name + ": " + missing + (broken ? ": vector* macro is defined but does not compile" : ": not provided by this implementation"));
			}
			else if(listonly)
			{
				status = "available";
			}
			else
			{
				Ctx c;
				const char *known = known_failure(impl->name, t.name);

				c.impl = impl;
				c.o = &o;
				c.res.nchecked = c.res.nbad = 0;
				c.rng.seed(o.seed*1000003ULL + std::hash<std::string>()(t.name));
				int sig = run_isolated(t, c);

				if(sig)
				{
					c.res.nbad = 1;
					c.res.first = std::string("crashed (") + strsignal(sig) + ")";
				}
				if(c.res.nbad == 0)
				{
					status = known ? "XPASS" : "pass";
					(known ? nxpass : npass)++;
				}
				else if(known && !o.strict)
				{
					status = "xfail";
					++nxfail;
				}
				else
				{
					status = sig ? "CRASH" : "FAIL";
					++nfail;
					details.push_back(impl->name + ": " + std::to_string(c.res.nbad) + " failure(s); first: " + c.res.first);
				}
				if(known && (c.res.nbad == 0 || o.strict))
				{
					details.push_back(impl->name + ": known failure: " + known);
				}
			}
			printf("  %-10s", status.c_str());
		}
		printf("\n");
		for(const std::string &d : details)
		{
			printf("    %s\n", d.c_str());
		}
		fflush(stdout);
	}

	if(!listonly)
	{
		printf("\n%d passed, %d FAILED, %d do not compile, %d not provided", npass, nfail, nnocompile, nabsent);
		if(!o.strict)
		{
			printf(", %d known failures (xfail), %d XPASS", nxfail, nxpass);
		}
		printf("\n");
	}

	return nfail ? EXIT_FAILURE : EXIT_SUCCESS;
}
