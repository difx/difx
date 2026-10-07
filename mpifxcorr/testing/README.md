# architecture.h tests and benchmark

Correctness tests and a benchmark for the vector routines in
`mpifxcorr/src/architecture.h.in`: the generic section (FFTW and plain C++,
used when mpifxcorr is built without Intel IPP) and, when IPP is available,
the IPP section.  IPP is taken as correct; the tests check every routine
against a reference model of IPP's documented behaviour.

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

## How it works

The generic and IPP sections define the same `vector*` names with different
types, so they cannot share a compilation unit.  The Makefile makes one copy
of `architecture.h.in` for each (`@ipp_enabled@` set to 0 and 1:
`arch_generic.h`, `arch_ipp.h`), and each is compiled into its own file of
thin wrappers (`arch_generic.cpp`, `arch_ipp.cpp`).  The wrappers use neutral
types (`arch_api.h`) and register themselves by name; the test and benchmark
programs look routines up by name in each implementation.

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
