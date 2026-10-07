# architecture.h tests and benchmark

Correctness tests and a benchmark for the vector routines in
`mpifxcorr/src/architecture.h.in`: the generic section (FFTW and plain C++,
used when mpifxcorr is built without Intel IPP), with and without its
AVX2/FMA code, and, when IPP is available, the IPP section.  IPP is taken as
correct; the tests check every routine against a reference model of IPP's
documented behaviour.

| program              | purpose |
|----------------------|---------|
| `test_architecture`  | checks every `vector*` routine in each implementation built |
| `bench_architecture` | times every routine in each implementation built |

## Building and running

mpifxcorr must already be configured (`../Makefile` exists).  Then, after
sourcing `setup.bash` as usual:

    make            # builds test_architecture and bench_architecture
    make test       # correctness tests; should end "0 FAILED"
    make bench      # benchmark at length 4096

FFTW and IPP compile and link flags are taken from `../Makefile`
(`FFTW3F_CFLAGS`, `FFTW3F_LIBS`, `IPP_CFLAGS`, `IPP_LIBS`).  When mpifxcorr is
configured with IPP, configure does not look for FFTW, so if both FFTW
variables are empty they are taken from `pkg-config fftw3f`.  If `IPP_LIBS` is
empty only the generic section is built and tested (as on macOS/Arm, where
IPP is not available).  Any of these, and `CXX`/`CXXFLAGS`, can be overridden
on the make command line.  MPI is not needed.

The generic section is built as several implementations, which are tested
and timed side by side:

| implementation | code | flags |
|----------------|------|-------|
| `generic` | plain C++ | `CXXFLAGS` (default `-O2 -g`) |
| `gcc-O2`  | plain C++, vectorized by the compiler alone | `CXXFLAGS $(SIMDARCH)` |
| `gcc-O3`  | the same at `-O3` | `CXXFLAGS $(SIMDARCH) -O3` |
| `avx2`    | with the AVX2/FMA intrinsics | `CXXFLAGS $(SIMDARCH)` |
| `ipp`     | Intel IPP | `CXXFLAGS $(IPP_CFLAGS)` |

`SIMDARCH` is `-march=x86-64-v3` (AVX2 and FMA: Intel Haswell, AMD Zen and
later) by default.  The `gcc-*` and `avx2` builds are made only on x86_64
machines with AVX2 and FMA, since other code in them is compiled for AVX2
too; elsewhere only `generic` (and `ipp`) are built.  The plain builds are
compiled with `-DARCH_NO_SIMD`.

## How it works

The generic and IPP sections define the same `vector*` names with different
types, so they cannot share a compilation unit.  The Makefile makes one copy
of `architecture.h.in` for each (`@ipp_enabled@` set to 0 and 1:
`arch_generic.h`, `arch_ipp.h`), and each is compiled into its own file of
thin wrappers (`arch_generic.cpp`, `arch_ipp.cpp`).  The wrappers use neutral
types (`arch_api.h`) and register themselves by name; the test and benchmark
programs look routines up by name in each implementation (`arch_impls.cpp`).
`arch_generic.cpp` is compiled once per generic build, and makes the
header's inline functions static so that the linker cannot merge copies
compiled with different flags.

The wrappers are generated from a table in `gen_wrappers.py` into
`arch_wrappers.inc` (kept in git, so python is only needed to change the
table).  Some `vector*` macros do not compile in one implementation or the
other; `find_skips.sh` finds these at build time and leaves them out, so the
build always succeeds.  Each left-out routine is reported as:

- `absent`: the macro is not defined for that implementation at all
  (e.g. the out-of-place `vectorAdd_*` exist only in the IPP section);
- `NOCOMPILE`: the macro is defined but does not compile (e.g. it calls a
  function that does not exist), which is a bug if anything uses it.

gcc reports an error in a macro at the macro's definition in the header, not
where it is used; `find_skips.sh` adds `-ftrack-macro-expansion=0` when the
compiler accepts it, so that errors point into `arch_wrappers.inc` as they do
with clang.

## test_architecture

Each routine is run at lengths 1 to 4096 (FFTs at several sizes, including
non powers of 2 for the DFTs) on random data, and the result is compared with
a double precision reference following the IPP manual, notably:

- out-of-place subtract and divide compute `src2 - src1` and `src2 / src1`;
- integer results saturate; scaled conversions multiply by 2^-scale;
- `ippRndZero` truncates, `ippRndNear` rounds halves to even;
- FFTs are unnormalised forward transforms, the real forward output is CCS
  (N/2+1 complex values), and `IPP_FFT_DIV_INV_BY_N` scales the inverse by 1/N.

Floating point results are compared with tolerances matching each operation
(the rounding of single operations, the length of reductions, the A21 accuracy
of IPP's sin/cos, FFT error growth); copies, integer results and conversions
must match exactly.  Outputs have guard elements after the requested length,
so writing past the end is a failure.  FFT initialisation must set the work
buffer pointer and size.  Each test runs in a child process, so a crash in the
code under test is reported as `CRASH` rather than ending the run.

Options: `-m` selects routines by glob pattern (e.g. `-m 'Add*,FFT*'`), `-l`
lists them with their availability, `-r n` reports up to n mismatches per
failing test, `-s` sets the random seed, and `-x` makes known failures count
as failures.

### Known failures

Bugs in the generic section that are known but not yet fixed are listed in
`known_failure()` in `test_architecture.cpp`.  They are reported as `xfail`
and do not affect the exit status unless `-x` is given; when one is fixed it
is reported as `XPASS` and should be removed from the table.  At present:

| routine | problem in the generic section |
|---------|--------------------------------|
| `Sub_s32` | wraps on overflow; IPP saturates |
| `Flip_f64_I` | swaps each pair twice, leaving the data unchanged |
| `StdDev_f32` | returns `sqrt(sum)/(n-1)` instead of `sqrt(sum/(n-1))` |
| `ConvertScaled_f32s16`, `ConvertScaled_f32u8` | ignore the rounding mode (always truncate) and do not saturate |
| `Add_s16_I`, `AddC_s16_I` | wrap on overflow; IPP saturates |
| `Max_f32`, `Min_f32`, `MaxIndx_f32` | start from -1e30 / 1e30, so are wrong if every value is beyond that |

Routines that do not compile in the generic section: `vectorAdd_s32_I`,
`vectorMulC_cs16_I`, `vectorConvert_f32s32`, `vectorAlloc_cs16` (`cs16` is
defined as C99 `complex short`) and the 2-D FFT macros used by difx_monitor.
In the IPP section only the 2-D FFT macros do not compile (they use the
`ippiFFTInitAlloc` API, removed in IPP 9).

Other problems seen in `architecture.h.in` that the tests cannot show:

- in the generic section `MAX_S32` and `MAX_U32` are defined as the 16-bit
  limits (`fxmanager.cpp` uses `MAX_S32` as the starting minimum).

### IPP results

On Debian 13 (g++ 14.2, IPP 2026.0, x86_64) the IPP column passes every test,
so the reference models and tolerances agree with IPP.  That run found these
bugs in the IPP section, which have since been fixed:

- `vectorZero_s32` called `genericZero_32s`, which exists only in the generic
  section;
- `vectorAdd_cf32` and `vectorAdd_cf64` (out of place) mapped to the in-place
  IPP functions with the wrong number of arguments;
- `vectorGenerateFIRLowpass_f64` did not pass the work buffer that
  `ippsFIRGenLowpass_64f` needs since IPP 9;
- the IPP version check selected the IPP 9 API only for IPP 9 (`-DIPP9`) and
  2018-2021, so IPP 2017 or 2022 onwards without `-DIPP9` would have used the
  old, removed API.  It now selects it for any version from 9 on.

A failure in the IPP column points at the reference model or its tolerance.

## bench_architecture

Times each routine in each implementation built.  Buffers are prepared once
per routine and length, then the routine is called repeatedly for at least the
minimum time; repeats are interleaved across routines and the best is kept.
On macOS the process asks for a performance core.  Reported: ns per call, ns
per element and million elements per second (for FFTs, per input point), and
with IPP the generic time relative to IPP.

    ./bench_architecture -o before.csv           # baseline, length 4096
    ./bench_architecture -n 256,4096,65536       # several lengths
    ./bench_architecture -b before.csv           # compare with the baseline
    ./bench_architecture -m 'FFT*,DFT*'          # selected routines

Run-to-run variation is a few percent on an idle machine; the shortest
memory-bound routines vary most.

## AVX2/FMA code in the generic section

The end of the generic section of `architecture.h.in` has AVX2/FMA versions of
53 routines: those mpifxcorr calls (fringe rotation: `SinCos`,
`RealToComplex`, complex multiplies; cross multiplication: `AddProduct_cf32`;
sums, copies, conjugates and flips; the double precision model arithmetic)
and similar simple ones.  They are compiled in when the compiler targets a CPU
with AVX2 and FMA, and replace the plain versions through the `vector*`
macros; with `-DARCH_NO_SIMD`, or for any other CPU, the plain code is used
unchanged.  Routines with a known failure keep the plain code, so that both
builds give the same results.

To use it in a build of mpifxcorr without IPP, configure with e.g.
`CXXFLAGS="-g -O2 -march=x86-64-v3"` (any Intel CPU since Haswell, AMD Zen)
or `-march=native` for the build machine only.  It makes no difference to an
IPP build, where the generic section is not used.

The results are those of the plain code except for rounding: products and
sums may be fused (FMA), long sums add in a different order, and `Sin`,
`Cos`, `SinCos` and `Phase` use polynomials.  Measured against double
precision over 4 million random arguments per range, sin and cos are within
1.6 ulp for any |x| up to 10^6 (arguments of 65536 and over, infinities and
NaNs go to `sinf`/`cosf`), and `Phase` (atan2) within 3.1 ulp for magnitudes
from 1e-30 to 1e30, with the special values of signed zeros, infinities and
NaNs as in libm.  IPP's `_A21` functions guarantee about 4 ulp.

### Speed

Xeon Silver 4514Y (Sapphire Rapids), g++ 14.2, IPP 2026.0, one core.  The
geometric mean, over the 53 routines, of the time of each build divided by the
time of `avx2` (so above 1 means `avx2` is faster; for `ipp`, below 1 means
IPP is faster):

| length | generic | gcc-O2 | gcc-O3 | gcc-O3 AVX-512 | ipp |
|-------:|--------:|-------:|-------:|---------------:|----:|
| 16     | 2.5     | 2.5    | 1.39   |                | 1.85 * |
| 256    | 5.5     | 5.5    | 1.78   | 1.37           | 0.93 |
| 4096   | 4.6     | 4.6    | 1.63   | 1.48           | 0.86 |

\* at length 16 IPP's overhead per call dominates.
"gcc-O3 AVX-512" is the plain code built with `SIMDARCH=-march=x86-64-v4`.

- At mpifxcorr's usual `-O2`, targeting AVX2 alone changes nothing: gcc
  vectorizes almost none of the plain loops, as it cannot rule out
  overlapping arrays.
- At `-O3` gcc vectorizes the simple element-by-element loops about as well
  as the intrinsics, but not sin/cos (`SinCos_f32` 10.7 times slower at 4096),
  atan2 (12x), sums (`Sum`, `Mean`: 25-31x; `DotProduct_f64`: 3.8x), square
  roots, or the complex arithmetic that needs permutes (`AddProduct_cf32`
  1.85x, `Mul_cf32` 1.7x, `ConjFlip_cf32` 1.85x).
- IPP uses AVX-512 on this CPU, so it is up to about 2x faster than `avx2` on
  in-cache arithmetic (`SinCos_f32` 2.2x at 4096) and about the same where
  memory bandwidth limits both.

Selected routines (ns per call):

| routine | n | generic | gcc-O2 | gcc-O3 | avx2 | ipp |
|---------|--:|--------:|-------:|-------:|-----:|----:|
| `SinCos_f32`        | 256  | 1258 | 1268 | 1340 | 207  | 115  |
| `SinCos_f32`        | 4096 | 31348 | 30900 | 35886 | 3367 | 1556 |
| `AddProduct_cf32`   | 256  | 358  | 407  | 193  | 74   | 44   |
| `AddProduct_cf32`   | 4096 | 6020 | 6339 | 3179 | 1715 | 1333 |
| `Mul_cf32`          | 4096 | 4950 | 4770 | 2389 | 1439 | 2436 |
| `MulC_cf32`         | 4096 | 3976 | 3377 | 1605 | 1014 | 2201 |
| `RealToComplex_f32` | 4096 | 2130 | 2128 | 1127 | 1097 | 958  |
| `Add_cf32_I`        | 4096 | 2114 | 2114 | 823  | 826  | 1447 |
| `Mean_cf32`         | 4096 | 6453 | 6452 | 6451 | 206  | 220  |
| `DotProduct_f64`    | 4096 | 3161 | 3161 | 3162 | 833  | 755  |
| `Phase_cf32`        | 4096 | 52251 | 52251 | 52303 | 4386 | 4155 |

The FFTs are FFTW in every generic build, about 1.3-2.8 times slower than
IPP's; FFTW's own SIMD code is chosen at run time, so the compiler flags do
not affect it.
