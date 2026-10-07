# VDIF and CODIF unpacker tests and benchmarks

Correctness tests and a benchmark for the unpackers in `mark5access/format_vdif.c`
and `mark5access/format_codif.c` (real and complex decoders, and the 2-bit
high-state counters).  They are a safety net and a yardstick for optimising the
unpackers.

| program             | purpose |
|---------------------|---------|
| `test_vdif_unpack`  | checks every VDIF mode against an independent reference decoder |
| `test_codif_unpack` | the same for CODIF, plus format detection from data |
| `bench_vdif_unpack` | times every VDIF mode, as mpifxcorr calls the unpackers |

## Building and running

The library must already be configured (`./configure` in the mark5access
directory).  Then:

    make            # builds ../mark5access first, then the programs here
    make test       # all correctness tests (see below)
    make bench      # benchmark all VDIF modes

The programs link the libtool static archive `../mark5access/.libs/libmark5access.a`,
so `make` here always picks up edits to the library sources.  Codifio compile and
link flags are taken from `../mark5access/Makefile`; override `M5LIB`,
`EXTRALIBS`, `CODIFIO_CFLAGS`, `CC` or `CFLAGS` on the make command line if needed.

`make test` runs `test_vdif_unpack` under each VDIF decoder path (see below),
again with `SYMMETRIC_8_BIT_VDIF=1` for the 8-bit modes, and `test_codif_unpack`.
Every run should end with `0 FAILED`.

## Modes

The set of modes is not hard coded.  Each program asks the library which
(real/complex, nbit, nchan) combinations it accepts (nbit 1..32, nchan 1..64),
so a newly supported mode is tested automatically.  Modes are named like
`real-2bit-16ch` or `cplx-8bit-4ch`; `-l` lists them and `-m` selects them with
comma-separated glob patterns, e.g. `-m 'real-2bit-*,cplx-*-16ch'`.

## VDIF decoder paths

`format_vdif.c` has a generic run-based decoder (`vdif_fast_decode`) that is used
instead of the original per-mode decoders wherever it is faster.  Blanking and
frame-end checks are made once per run of data rather than per byte or sample,
and each run is decoded by a kernel chosen per mode when the format is created:

- NEON kernels (Arm) for 1, 2, 4, 8, 16 and 32 bit data;
- plain C kernels on all platforms: a per-mode byte table copied with fixed-size
  `memcpy` for 1/2/4-bit modes with fewer than one byte per sample, and loops
  written so that compilers vectorize them for 8/16/32-bit data;
- the original per-mode decoders where neither is faster (without SIMD: 1/2/4-bit
  modes with a byte or more per sample).

All paths give bit-identical output.  Environment variables select a path at
run time, so one build can test and benchmark all of them:

| setting                | decoders used |
|------------------------|---------------|
| (none)                 | the fastest available: NEON where it wins, plain C or per-mode otherwise |
| `VDIF_NO_SIMD=1`       | as on a platform without SIMD kernels (e.g. Intel at present): plain C kernels where faster, per-mode decoders otherwise |
| `VDIF_LEGACY_DECODE=1` | the original per-mode decoders for every mode |

Compiling the library with `-DVDIF_NO_SIMD` leaves the SIMD kernels out.

## test_vdif_unpack

Every mode is checked against an independent reference decoder
(`vdif_common.c`), which reads the payload as a little-endian bitstream:
channel *c* of time sample *s* starts at bit `(s*npad + c)*bitspersample`,
where `npad` is nchan rounded up to a power of 2 (VDIF packs channels in powers
of 2; e.g. a 3-channel mode unpacks 3 of 4 packed channels).  For complex data
the real part comes first.  Values are compared bit-exactly (`-t` allows a
tolerance).

Columns in the report:

| test   | what it checks |
|--------|----------------|
| meta   | `framesamples` and `samplegranularity` the library derives for the stream |
| stream | memory stream + `mark5_stream_decode[_complex]` in randomly sized chunks, so frame boundaries fall at arbitrary points inside a call; return values |
| blank  | as stream, with some frames invalid (header bit) or holding fill pattern; these must decode as zeros and be excluded from the returned good-sample count |
| unpack | unpacker stream with headers + `mark5_unpack[_complex]_with_offset` at various offsets, with good and bad frames (the mpifxcorr code path) |
| count  | `mark5_stream_count_high_states` (2-bit real only), good and bad frames |

Each test runs for 8000 and 1024 byte payloads and the smallest legal payload
(8 bytes for most modes, which stresses frame stepping).  Output arrays have
guard words after the requested samples, so writing past `nsamp` is a failure.
The exit status is non-zero if anything fails.

Library bugs that are known but not yet fixed can be listed in
`known_failure()` (in each test program).  They are then reported as `xfail`
and don't affect the exit status unless `-x` is given; once fixed they are
reported as `XPASS` and should be removed from the table.  There are none at
present.

## test_codif_unpack

The same tests for `format_codif.c`, plus a check of format detection from data
(`new_mark5_format_from_stream()`, which uses `find_codif_frame()` and
`get_codif_threads()`), with junk bytes before the first frame and several
interleaved threads.

CODIF packs channels densely with no power of 2 requirement: channel *c* of
sample *s* starts at bit `(s*nchan + c)*bitspersample`.  Values are two's
complement, the only representation the library supports at present.  The
library has dedicated decoders for common power-of-2 channel counts and a
generic (slower) decoder for every other combination, so all channel counts
are accepted.

## bench_vdif_unpack

Times each VDIF mode as mpifxcorr uses it: an unpacker stream with headers,
calling `mark5_unpack[_complex]_with_offset` across a 4 MiB buffer of frames,
one frame per call by default (`-n` sets samples per call).  Library per-frame
overhead (validation, blanker) is included.  Repeats are interleaved across
modes and the best is kept, which gives roughly 1% mean run-to-run variation on
an idle machine.  On macOS the process asks for a performance core.

Reported: Msamp/s (all channels; complex counts once), input MB/s of payload,
output GB/s of floats, ns per time sample.  `-c` adds the 2-bit counters.

Optimisation workflow:

    ./bench_vdif_unpack -c -o before.csv      # baseline
    # ... edit ../mark5access/format_vdif.c ...
    make && make test                         # still correct?
    ./bench_vdif_unpack -c -b before.csv      # per-mode speedup and geometric mean

Comparing decoder paths from one build:

    VDIF_LEGACY_DECODE=1 ./bench_vdif_unpack -o legacy.csv
    VDIF_NO_SIMD=1 ./bench_vdif_unpack -b legacy.csv
    ./bench_vdif_unpack -b legacy.csv

### Output layout and cache aliasing

By default each channel's output array is a separate `calloc`, as in
mpifxcorr (`vectorAlloc_f32` per band).  When those arrays end up a power-of-2
distance apart, every channel maps to the same L1 cache set, and modes with
many channels slow down a lot whatever the decoder (on an Apple M3 Pro with
8000-byte frames, macOS spaces them 16 KiB apart: 16-32 channel 1-bit modes run
up to ~10x slower).  `-a stagger` puts the arrays in one block with an extra
cache line between channels, which isolates the speed of the unpacker code
itself.  Always compare runs that used the same layout and frame size (`-f`).
