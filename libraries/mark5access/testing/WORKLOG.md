# VDIF unpacker work log

Handoff notes for picking this work up again, possibly on another machine.
How to build and use the tools is in [README.md](README.md).

## Goal

Speed up the VDIF unpackers in `../mark5access/format_vdif.c` (CODIF later),
keeping the output bit-exact.  Production target is Debian on Intel (SSE/AVX2).
Development so far has been on macOS, Apple M3 Pro (NEON), with clang.  The
non-VDIF/CODIF formats are legacy and out of scope.

## Done

2026-10-06 (branch `cjp_vdif_testing`, committed and pushed):

- Correctness tester and benchmark in this directory (see README).
- Fixed `vdif_decode_16channel_4bit_decimation1` (blank path advanced 4 rather
  than 8 bytes) and `vdif_decode_1channel_32bit_decimation1` (read through a
  `uint16_t *`).

2026-10-07 (not yet committed):

- NEON SIMD decoder for all 1, 2 and 4 bit modes, real and complex, every
  channel count the library supports (see "NEON design" below).  Bit-exact:
  `make test` passes on both the SIMD and scalar paths.  Also, on the
  known-bug cases (non-power-of-2 nchan with unpack offsets) the SIMD path
  fails identically to the scalar decoders, mismatch for mismatch.
- Runtime switch `VDIF_NO_SIMD=1` (environment) selects the original decoders;
  `make test` now runs both.
- Speed, SIMD vs original, one 8000-byte frame per call (geometric mean over
  78 modes): 2.2x with staggered output arrays, 2.4x with separate arrays.
  Every mode is faster (1.15x to 3.8x).

2026-10-07, later (not yet committed):

- Fixed the non-power-of-2 nchan bug: `format_vdif.c` init (framesamples,
  samplegranularity), `mark5_stream_unpacker.c` (read position for VDIF/VDIFL
  only; other formats unchanged), and `mpifxcorr/src/configuration.cpp`
  (framesamples for VDIF/VDIFL/INTERLACEDVDIF uses bands rounded up to a power
  of 2, so it still agrees with mark5access).  The known-failure table is now
  empty; `./test_vdif_unpack -x` passes all 479 tests with and without SIMD.
- Note: vex2difx (`fixDatastreamTable`) already pads recorded bands to a power
  of 2 with dummy "null" bands, so DiFX-generated configs never hit this bug.
  It affected direct mark5access users (m5spec etc.) and hand-written configs.
  The mpifxcorr change compiles but has not been run in a correlation.

2026-10-07, plain C (no SIMD) improvements (not yet committed):

- The generic run-based driver (renamed `vdif_fast_decode`, struct
  `vdif_fast_mode`, field `fast`) now also has plain C kernels and covers
  every mode, including 8/16/32 bit.  The kernel is chosen per mode at format
  creation, using whichever measured fastest:
  - 1/2/4 bit narrow (sample < 1 byte): 256-entry per-mode table of each
    byte value's floats laid out channel by channel, copied with one
    fixed-size `memcpy` per channel per byte (8, 16 or 32 byte moves).
    Beats NEON for 1-bit 1-channel (real and complex), so used there on
    NEON builds too.
  - 1/2/4 bit wide: plain C is no faster than the per-mode decoders (both
    store one float at a time, ~1 store/cycle), so without SIMD those modes
    keep the per-mode decoders.
  - 8/16/32 bit: channel-by-channel loops (sequential writes); 16 bit
    converted in float (exact, so identical).  Per-mode decoders kept for 16
    bit with >= 8 values per sample (slightly faster there).
- Env `VDIF_LEGACY_DECODE=1` selects the per-mode decoders everywhere;
  `VDIF_NO_SIMD=1` now means "as without SIMD" (plain C kernels where faster,
  per-mode decoders otherwise), i.e. what Intel builds get today.  `make
  test` runs all three paths.
- Lesson: the kernels must not be inlined into `vdif_fast_decode`.  When
  they were, clang ran out of registers and kept loop pointers on the stack,
  creating a loop-carried store/load dependency, which halved the narrow
  kernel's speed.  They are `noinline` now.  Check this on gcc/Intel too.
- Speed vs per-mode decoders, one 8000 byte frame per call, geometric mean
  over 115 modes: plain C path 1.16x (staggered and separate layouts), no
  mode slower beyond noise; default NEON+C path 1.96x staggered / 2.10x
  separate, no mode slower.
- Per-call overhead (frame validation, blanker, two divisions in
  `mark5_unpack_with_offset`) measured at ~11 ns per call.  Negligible for
  thousands of samples per call, but at 64 samples/call it limits NEON
  2-bit 1-channel to ~4400 Msamp/s.  Not changed.

| mode (Msamp/s, staggered) | legacy | plain C | default (NEON+C) |
|---|---|---|---|
| real-1bit-1ch  | 4750 | 20080 (4.2x) | 19840 (4.2x) |
| real-2bit-1ch  | 4520 | 13620 (3.0x) | 14820 (3.3x) |
| real-2bit-2ch  | 4230 | 7290 (1.7x)  | 13760 (3.3x) |
| real-2bit-16ch | 4550 | 4555 (1.0x)  | 13310 (2.9x) |
| real-4bit-1ch  | 3460 | 8990 (2.6x)  | 13030 (3.8x) |
| real-8bit-1ch  | 1860 | 4570 (2.5x)  | 4510 (2.4x) |
| real-16bit-1ch | 1810 | 3360 (1.9x)  | 3370 (1.9x) |
| real-32bit-1ch | 1810 | ~3450 (1.9x) | ~3450 (1.9x) |
| cplx-1bit-1ch  | 3390 | 10040 (3.0x) | 9990 (2.9x) |
| cplx-2bit-1ch  | 2930 | 6870 (2.3x)  | 7430 (2.5x) |
| cplx-2bit-16ch | 3280 | 3275 (1.0x)  | 6240 (1.9x) |
| cplx-8bit-1ch  | 1250 | 2270 (1.8x)  | 2240 (1.8x) |
| cplx-16bit-1ch | 1020 | 1680 (1.7x)  | 1680 (1.7x) |

2026-10-07, 8/16/32 bit (not yet committed):

- Plain C kernel specialized on (nbit, values per sample, real/complex) so
  strides are compile-time constants; clang auto-vectorizes 16 and 32 bit
  (interleaved loads).  Bit-exact forms: 16 bit `(v-32768)*0.125f`; 32 bit
  `(float)(int32)(v ^ 0x80000000)*0.125f` (= v/8 - 2^28 rounded once).
- 8 bit: no float formula matches the table; `(b-off)*(1/3.3)` in double
  does, but vectorizing through f64 lanes was 1.3-6x slower than the table,
  so plain C keeps the table.  For SIMD, `fma(x, rhi, x*rlo)` with
  x = b - offset and a two-part float reciprocal of 3.3 matches all 256
  values for both offsets (128, 127.5); checked at init, SIMD disabled for
  8 bit if it ever fails.  On Intel this needs FMA (AVX2/FMA3).
- NEON kernel for 8/16/32 bit: transpose N = 128/nbit samples into value
  planes (vld2/vld4 or uzp stages on 8/16/32-bit lanes), convert, store
  (vst2q_f32 interleaves re/im).  Used where it beats plain C: not for real
  16/32 bit with 1-2 values per sample (auto-vectorized C is faster) nor
  for 32 values per sample (transpose spills).
- Results vs per-mode decoders (Msamp/s staggered, default build):
  real-8bit-1ch 9900 (5.3x), real-16bit-1ch 17300 (9.6x),
  real-32bit-1ch 13800 (7.6x), cplx-8bit-1ch 4150 (3.3x),
  cplx-16bit-1ch 5480 (5.4x), cplx-8bit-16ch 2220 (1.1x).
- All 115 modes, geometric mean vs per-mode decoders: default 2.37x
  staggered / 2.54x separate (min 1.08x); no-SIMD 1.25x / 1.26x.

2026-10-07, CODIF review and fixes (not yet committed):

New `testing/test_codif_unpack` (dense channel layout, two's complement
values, detection test).  Bugs found and fixed in `format_codif.c`:

- Blank-path read position not advanced in real 8-bit 2/4ch, complex 8-bit
  1/2/4ch and complex 16-bit 1/2/4/8/16ch: after one bad frame the stream
  stayed stuck, blanking everything.  Complex 16-bit 4-16ch also counted
  blanked samples per channel (negative return values).
- Real 16-bit dispatch used case 4005/4006 for the 8/16 channel decoders
  (should be 4008/4016): 8/16ch unsupported, 5/6ch crashed.
- Real 16-bit 4/8/16ch set the word index from readposition without /2.
- Real 16-bit 2/3ch used integer division (buf[i]/8), truncating values.
- Complex 1-bit 1/2ch advanced the output one sample too far per byte.
- Odd channel counts: the 3/5/6/7 channel decoders (copied from VDIF)
  assumed padding to a power of 2; CODIF is dense.  Replaced, along with the
  14ch complex 16-bit decoder, by a generic dense decoder
  (`codif_decode_generic`), which is also used for any combination without
  a dedicated decoder, so every nchan 1..512 and nbit 1..32 is now accepted.
  samplegranularity is now 8/gcd(8, bits per sample) (e.g. 4 samples for
  3 channels x 2 bits).
- Init: returned success when no decoder could be set; did not recompute
  databytes/framebytes/payloadoffset after taking them from the header.
- `find_codif_frame()`: bounds check was mis-bracketed (could read past the
  buffer).  `get_codif_threads()`: read VDIF header fields; now reads CODIF
  thread ID and frame length.  `get_codif_framegranularity()`: guard
  division by zero.  `set_decoder()` made static (`codif_set_decoder`).
- Validation prints per bad frame only with -DDEBUG (as format_vdif.c).

Not changed: offset binary representation (known limitation); 4-bit levels
-8..7 (code 0 decodes to 0); the CODIF decoders have no fast paths yet
(the generic dense decoder is slow: one value at a time).

## Results on M3 Pro (Msamp/s, all channels, separate output arrays)

| mode           | original | SIMD  | speedup |
|----------------|----------|-------|---------|
| real-2bit-1ch  | 4450     | 14710 | 3.3x |
| real-2bit-2ch  | 4180     | 13330 | 3.2x |
| real-2bit-4ch  | 3960     | 13350 | 3.4x |
| real-2bit-16ch | 2360     | 11170 | 4.7x |
| real-2bit-64ch | 2900     | 11400 | 3.9x |
| cplx-2bit-1ch  | 2910     | 7410  | 2.5x |
| cplx-2bit-4ch  | 3000     | 6620  | 2.2x |
| cplx-2bit-16ch | 1590     | 5360  | 3.4x |
| cplx-2bit-64ch | 2110     | 3470  | 1.7x |
| real-1bit-1ch  | 4650     | 15350 | 3.3x |
| real-4bit-1ch  | 3470     | 13100 | 3.8x |
| cplx-4bit-16ch | 3210     | 5400  | 1.7x |

What limits it now:

- Up to about 8 channels it writes 50-60 GB/s of floats, which is about one
  16-byte store per cycle on this core.  It is store-bound: neither changing
  the interleaving method (`vst4q_u8` vs `zip` + `vst1q_x4`) nor calling with
  L1-sized output changed it.  More speed for these modes needs writing less
  (see suggestions below), not a faster kernel.
- Wide complex modes (cplx 2-bit 32/64ch, cplx 4-bit 9-16ch) reach only
  25-45 GB/s.  This is not memory (small calls don't help).  It is probably
  register spills in the 16/32-plane transpose.  Tried: transposing with
  `vld4q_u8` per group of planes; 20-35% slower (ld4 is costly on Apple
  cores), so reverted.
- With separate output arrays, cache-set aliasing still dominates some modes
  (real-1bit-16ch 1740 Msamp/s separate vs 14400 staggered).

## NEON design (for porting)

Code is in `format_vdif.c`, section "SIMD decoders".

- `struct vdif_simd_mode` (inside `struct mark5_format_vdif`) is filled by
  `vdif_simd_init_mode()` when the format is created.  It holds per-mode
  tables and the code-to-float table split into 4 byte planes
  (`lutplane[k][code]` = byte k of the float for that code).  Code values are
  copied from the existing scalar LUTs, so they are identical.
- Driver `vdif_simd_decode()`: works in "units" (smallest whole number of
  bytes holding whole samples; one byte, or one sample if a sample is >= 1
  byte).  Per call it splits the request into runs up to the end of the frame
  or blank zone, so blank and frame-end checks happen once per run rather
  than per byte.  Semantics match the old decoders exactly: whole units only;
  steps to the next frame as soon as one finishes; same return value; partial
  final units handled the same way.
- Runs are decoded 16 units at a time by a kernel; the remainder goes through
  `vdif_simd_scalar_units()` (portable, slow, at most 15 units per run).
- Last step of both kernels: 16 codes in output order become 16 floats with
  4 table lookups (`tbl(lutplane[k], codes)`), interleaved on store
  (`vst4q_u8`).
- Narrow kernel (a sample of all channels < 1 byte: 1-bit <= 4ch,
  2-bit <= 2ch, 4-bit 1ch, small complex): per channel and group of 16
  floats, `codes = (tbl(block16, sel) >> shift) & mask`, with per-mode
  `sel`/`shift` vectors (per-lane variable shift `vshlq_u8`).
- Wide kernel (a sample is P >= 1 bytes, P = 1..32): load 16 samples and
  transpose into P byte planes (plane k = byte k of each sample).  P = 2, 4
  come free from `vld2q`/`vld4q`.  P >= 8 use log2(P) "unshuffle" stages of
  `vuzp1q`/`vuzp2q`.  Each plane then gives each channel in it its 16 codes
  with a constant shift and mask.  Complex: re and im codes are zipped
  (`vzip1q`/`vzip2q`) into output order.  Padding channels are skipped.

## Porting to Intel (SSSE3 / AVX2 / AVX-512)

Same structure and tables.  Instruction mapping and differences:

- Table lookup `vqtbl1q_u8` -> `_mm_shuffle_epi8` (SSSE3) /
  `_mm256_shuffle_epi8` (AVX2, works per 128-bit lane, so broadcast the
  16-byte table to both lanes).  Codes are < 16, so the different
  out-of-range rule (high bit set -> 0 on x86) does not matter.
- No per-byte variable shift on x86 (no `vshlq_u8` equivalent).  For the
  narrow kernel, use the wide-kernel style instead: make 8/nbit copies of the
  block with constant shifts (`_mm_srli_epi16(v, s)` then AND mask; the mask
  removes bits shifted across bytes), then interleave them into time order
  with `unpacklo/hi_epi8` and `unpacklo/hi_epi16` (e.g. real 2-bit 1ch: zip
  c0,c1 and c2,c3, then zip16 the results).  Real 2-bit 2ch: ch0 = zip(c0,c2),
  ch1 = zip(c1,c3).  This also works on NEON and could replace the tbl/vshl
  version if simpler.
- No `vst4q_u8`: interleave the 4 byte planes with `unpacklo/hi_epi8`
  (planes 0,1 and 2,3) then `unpacklo/hi_epi16`, then 4 stores.  This is the
  `zip` variant that measured the same speed as `vst4q_u8` on NEON.
- No `vld2/vld4`/`vuzp`: transpose wide blocks with `_mm_shuffle_epi8`
  (gather bytes within each 16-byte load into byte-plane order) followed by
  `unpack` stages, or `_mm_packus_epi16` of masked/shifted 16-bit lanes for
  even/odd byte splits.
- AVX2 alternative for code-to-float with 1/2-bit data: widen codes with
  `_mm256_cvtepu8_epi32` and look up with `_mm256_permutevar8x32_ps` (8-entry
  float table in a register).  2 ops per 8 floats, no byte-plane interleave.
  4-bit (16 entries) needs two permutes and a blend.
- AVX-512 VBMI (Ice Lake and later), if production CPUs have it:
  `_mm512_multishift_epi64_epi8` extracts arbitrary bit fields per byte
  (all the code-extraction shifts in one op), and `_mm512_permutexvar_epi8`
  does 64-byte table lookups.
- Choose the path at compile time (`__SSSE3__`, `__AVX2__`) or with
  `__builtin_cpu_supports()` at format creation (function pointer), since
  Debian packages are built for baseline x86-64 (SSE2 only).
- Testing on the Mac: `clang -arch x86_64` builds run under Rosetta 2, which
  executes SSSE3 and AVX2 (AVX2 runs even though cpuid reports it as absent).
  Good for correctness only, not timing.  AVX-512 is not available.  The
  library would need building for x86_64 too (codifio is arm64 only here).

## Other suggestions (any platform)

- Run-based driver without SIMD: done (see 2026-10-07 plain C entry above).
  On Intel, re-measure the per-mode kernel choices (which kernel wins may
  differ from Apple cores) and check gcc does not inline the kernels into the
  driver.
- Output layout in mpifxcorr: allocate the unpack arrays as one block with a
  cache-line stagger per band (see README, "Output layout and cache
  aliasing").  Worth up to 8x for 16-32 channel 1-bit and about 1.3-1.9x for
  several 2-bit modes, independent of the decoder.
- Write less: the 2-bit decoders are now limited by writing 4-byte floats.
  Bigger gains would come from fusing unpacking with the next step in
  mpifxcorr (e.g. unpack straight into the FFT input / fringe rotation), or
  from a narrower output type where the consumer allows.
- Compiler flags: the library builds with plain `-O2`.  `-O3` and/or a
  `-march` for the production CPUs may help the scalar paths; measure.

## Open issues

- mpifxcorr INTERLACEDVDIF muxed frame size (`configuration.cpp`, search
  "multiplexedframebytes"): computed as payload * nummuxthreads, while vdifmux
  outputs payload * (thread count rounded up to a power of 2).  A padded
  `nbands` is computed just above but never used.  This looks like an
  unfinished fix; it matters only for a non-power-of-2 thread count.  Not
  changed; check with the mpifxcorr maintainers.
- Cache-set aliasing in mpifxcorr output arrays (above).  Worth checking on
  Intel.
- Wide complex modes are below the store limit (above).

## Next steps

1. Review and commit the plain C kernels (`format_vdif.c`, `testing/Makefile`).
2. Intel: write the SSSE3 (then AVX2) kernels following the notes above;
   check correctness under Rosetta here, then measure on Intel hardware.
3. Measure the scalar run-based driver for non-SIMD builds.
4. Wide complex modes with 32 values per sample (complex 8-bit 9-16ch,
   complex 16-bit 16ch, complex 2-bit 64ch) are the least improved; a
   transpose in two halves to avoid register spills might help.
5. Intel: SSE/AVX2 versions of the 8/16/32 bit kernels are straightforward
   (unpack/convert/multiply; 8 bit needs FMA).  First check how well gcc
   auto-vectorizes the plain C 16/32 bit kernels with -O2/-O3 on x86-64.

Note: the top-level `.gitignore` ignores every `Makefile`. `testing/Makefile`
is tracked with `git add -f`.
