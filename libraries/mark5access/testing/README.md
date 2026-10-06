# VDIF unpacker tests and benchmarks

Correctness tests and benchmarks for every unpacker in `mark5access/format_vdif.c`
(real, complex and 2-bit high-state counters).  They are intended as a safety net
and a yardstick when optimising the unpackers.

## Building

The library must already be configured (`./configure` in the mark5access
directory).  Then:

    make            # builds ../mark5access first, then the programs here
    make test       # correctness tests (also the SYMMETRIC_8_BIT_VDIF 8-bit table)
    make bench      # benchmark all modes

The programs link the libtool static archive `../mark5access/.libs/libmark5access.a`,
so `make` here always picks up edits to `format_vdif.c`.  Codifio link flags are
taken from `../mark5access/Makefile`; override `M5LIB`, `EXTRALIBS`, `CC` or
`CFLAGS` on the make command line if needed.

## Modes

The set of modes is not hard coded.  Both programs ask the library which
(real/complex, nbit, nchan) combinations it accepts (nbit 1..32, nchan 1..64),
so a newly added decoder is tested automatically.  Modes are named like
`real-2bit-16ch` or `cplx-8bit-4ch`; `-l` lists them and `-m` selects them with
comma-separated glob patterns, e.g. `-m 'real-2bit-*,cplx-*-16ch'`.

## test_vdif_unpack

Every mode is checked against an independent reference decoder
(`vdif_common.c`), which reads the payload as a little-endian bitstream:
channel *c* of time sample *s* starts at bit `(s*npad + c)*bitspersample`,
where `npad` is nchan rounded up to a power of 2 (VDIF requires a power of 2
channels; the "3 channel" decoders unpack 3 of 4 packed channels).  For complex
data the real part comes first.  Values are compared bit-exactly by default
(`-t` allows a tolerance).

Columns in the report:

| test   | what it checks |
|--------|----------------|
| meta   | `framesamples` and `samplegranularity` the library derives for the stream |
| stream | memory stream + `mark5_stream_decode[_complex]` in randomly sized chunks, so frame boundaries fall at arbitrary points inside a call; return values |
| blank  | as stream, with some frames invalid (header bit) or fill pattern; these must decode as zeros and be excluded from the returned good-sample count |
| unpack | unpacker stream with headers + `mark5_unpack[_complex]_with_offset` at various offsets, with good and bad frames (the mpifxcorr code path) |
| count  | `mark5_stream_count_high_states` (2-bit real only), good and bad frames |

Each test runs for 8000 and 1024 byte payloads and the smallest legal payload
(8 bytes for most modes, which stresses frame stepping).  Output arrays have
guard words after the requested samples, so writing past `nsamp` is a failure.
Exit status is non-zero if anything unexpected fails.

### Known library bugs (reported as `xfail`)

These were present when the tests were written.  They are listed in
`known_failure()` in `test_vdif_unpack.c`.  They don't affect the exit status
unless `-x` is given.  A known bug that starts passing is reported as `XPASS`;
remove it from the table at that point.

1. **Non-power-of-2 nchan** (3, 5–7, 9–15, 24 channels): `mark5_format_vdif_init()`
   computes `framesamples` (and so `samprate`) and `samplegranularity` from
   `nchan` rather than the packed channel count. `mark5_unpack*_with_offset()`
   computes the read position from unpadded nchan too.  The decoders themselves
   are correct for sequential decoding.

Fixed since the tests were written: `vdif_decode_1channel_32bit_decimation1`
read 16-bit words (`buf` was `const uint16_t *`), and
`vdif_decode_16channel_4bit_decimation1` advanced 4 rather than 8 bytes per
sample in blanked frames, shifting the data after any invalid or lost frame.

## bench_vdif_unpack

Times each mode as mpifxcorr uses it: an unpacker stream with headers, calling
`mark5_unpack[_complex]_with_offset` across a 4 MiB buffer of frames, one
frame per call by default (`-n` sets samples per call).  Library per-frame
overhead (validation, blanker) is included.  Repeats are interleaved across
modes and the best is kept, which gives roughly 1% mean run-to-run variation
on an idle machine.  On macOS the process asks for a performance core.

Reported: Msamp/s (all channels; complex counts once), input MB/s of payload,
output GB/s of floats, ns per time sample.

Optimisation workflow:

    ./bench_vdif_unpack -c -o before.csv      # baseline, incl. counters
    # ... edit ../mark5access/format_vdif.c ...
    make && ./test_vdif_unpack                # still correct?
    ./bench_vdif_unpack -c -b before.csv      # per-mode speedup and geometric mean

### Output layout and cache aliasing

By default each channel's output array is a separate `calloc`, as in
mpifxcorr (`vectorAlloc_f32` per band).  When those arrays end up a power-of-2
distance apart, every channel maps to the same L1 cache set.  On an M3 Pro with
8000-byte frames, macOS spaces them 16 KiB apart, and this makes the 16–32
channel 1-bit modes 8–14× slower, and several 2-bit wide modes 1.3–1.9× slower.
`-a stagger` puts the arrays in one block with an extra cache line between
channels, which isolates the speed of the unpacker code itself.  Always compare
runs that used the same layout.
