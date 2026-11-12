# gpudifx: merge notes for reviewers

This file records how the CUDA GPU correlator developed in
[ADACS-Australia/ADeller2023B_difx](https://github.com/ADACS-Australia/ADeller2023B_difx)
(branch `adam-performance-gains`, "the GPU branch" below) was merged with
`difx/difx` `dev` to form the `gpudifx` branch: what was changed beyond the
mechanical merge, why, how it was tested, and what is still open. It is meant
as a map for in-depth review against both parents.

## 1. Commits and how to diff them

| What | Commit |
|---|---|
| Common ancestor of dev and the GPU branch | `3eb29e860` (2023-10-23) |
| GPU branch head that was merged | `b69f1a01d` |
| dev when `gpudifx` was created | `69c3e7506` |
| Merge of the GPU branch (all conflict resolution) | `1f630607b` |
| Bug fix pushed to dev (see section 6) | `67957d4bb`, merged to dev as `dc135ec49` |
| Merge of the updated dev into `gpudifx` | `eed819a3c` |

The divergence was about 3 years: 819 dev commits (1643 files, mostly HOPS,
difx2fits, vex2difx, sites) against 124 GPU-branch commits (92 files, mostly
`mpifxcorr/src`). Only 27 files were changed on both sides. 13 of those merged
automatically and 14 had conflicts.

Useful review commands:

```bash
# Everything gpudifx adds on top of dev (the GPU work plus the merge fix-ups)
git diff origin/dev...origin/gpudifx -- mpifxcorr

# What changed relative to the GPU branch as it stood in ADACS2023B
# (this also contains every dev change since 2023, so filter by path)
git diff b69f1a01d origin/gpudifx -- mpifxcorr/src

# Only the hand-made merge decisions: the committed result vs git's automatic
# merge with conflict markers (git >= 2.36). Ignore the deletions of docs/ and
# tests/FakeData/*.sbatch, which are deliberate exclusions (section 2).
git show --remerge-diff 1f630607b -- mpifxcorr install-difx genipppc applications
git show --remerge-diff eed819a3c
```

## 2. Scope: what was and was not merged

Merged: all committed GPU-branch content (GPUCore/GPUMode/gpudecode/
mk5mode_gpu, the `Mode` -> `CPUMode` split, configure/Makefile CUDA support,
`install-difx --withcuda`, `tests/Synthetic` CPU-vs-GPU harness
(`run-local.sh`, `run-slurm.sh`), `tests/FakeData` benchmark scripts,
`utilities/fft-profiling`, mark5access VDIF `genheaders`, `CLAUDE.md`).

Deliberately not merged (still in the GPU branch's history and in the ADACS
repository):
- `docs/` (GPU design notes, change logs, profiling notes, benchmark report).
- `tests/FakeData/*.sbatch` (site-specific SLURM scripts).

Several merged files still refer to these. Comments in `gpumode.cu`, `gpumode.cuh`,
`gpucore.cu`, `gpucore.cuh`, `gpudecode.cu`, `gpudecode.cuh` and `core.h`
point to `docs/gpu-*.md`. `tests/FakeData/README.md`, `BENCHMARKS*.md`,
`nsys-analyze.py`, `tests/frac-probes/RESULTS.md` and `utilities/fft-profiling/*`
mention the `.sbatch` scripts. See open items.

Never committed on the GPU branch, so not affected by the merge: `.claude/`,
`HANDOVER-desktop.md.deprecated`, and the synthetic VDIF test data.

## 3. Conflict resolutions

### Mechanical (one side or both taken)

| File | Resolution |
|---|---|
| `applications/difx2fits/src/time2str.c` | dev (same prototype) |
| `genipppc` | dev (the GPU side had a duplicated check) |
| `install-difx` | both: `--withspice` and `--withcuda` |
| `mpifxcorr/configure.ac` | dev's `LIBS` (MATH_LIBS moved to per-program `_LDADD`) plus `${CUDA_LIBS}` |
| `mpifxcorr/src/Makefile.am` | both: dev's per-program `mathlibs` and pcal test programs, plus the GPU branch's `cuda_files` and `.cu.o` rule |
| `mpifxcorr/src/configuration.cpp` | GPU side: same multiplexed-frame logic as dev, plus the `usegpu` choice between `Mk5_GPUMode` and `Mk5_CPUMode` (VDIF family only) |
| `mpifxcorr/src/core.cpp` | dev (whitespace only) |
| `mpifxcorr/src/mk5mode.cpp` | dev (both sides carried the same complex-erasure fix) |
| `mpifxcorr/src/mode.h` | dev (no `pcalnbins`) with the GPU branch's `= nullptr` initialisers |
| `mpifxcorr/src/vdiffile.cpp`, `visibility.cpp` | dev (the GPU side added only stale comments) |

### Hand ports: code the GPU branch moved, changed on dev in the meantime

- **`mode.cpp` / `cpumode.cpp`.** The GPU branch moved the body of
  `Mode::process` (~740 lines) into `CPUMode::process`, leaving an `abort()`
  stub in `Mode`. Dev's three changes to `Mode::process` were ported into
  `CPUMode::process`:
  1. PCal: `getDPhaseCalIntervalHz`, and
     `adjustSampleOffset(samplenrsincestart)` in place of
     `datasamples+nearestsample` (Jan Wagner's phase-drift fixes `4d80b7811`
     and `7d6936c3a`).
  2. The simplified `lofreq` for complex LSB/DSB data. It gives the same result
     as before in all four sideband cases.
  3. A comment about channel ordering.

  Dev's fraction-based `PCal::getNew` setup, the zero-initialised
  `pcalresults`/`extractor` and the removal of `pcalnbins` in `Mode::Mode` and
  `~Mode` merged automatically.
- **`pcal.cpp` / `pcal.h`.** Dev rewrote `pcal.cpp` (fractional tone spacing,
  `fraction.h`, renamed and new extractor classes, buffers freed in one
  central `pcal_config_pimpl` destructor). The resolution takes dev's file
  whole and re-applies the GPU branch's buffer injection (`setPcalReal`,
  `setPcalComplex`) to fit it (see 4.2).

## 4. Changes made during the merge beyond conflict resolution

Each of these was reviewed individually before being committed in `1f630607b`.

1. **`unpackedarrays` allocation (`mode.cpp`).** The GPU branch allocated all
   bands as one contiguous block, but `~CPUMode` called `vectorFree()` on
   every band pointer, freeing pointers into the middle of the block. Nothing
   depends on the block being contiguous (`GPUMode` uses its own device
   buffers), so the merge goes back to dev's per-band allocation, including
   dev's `+8` padding for unpackers that write in groups of samples
   (`db2e775e5`).
2. **PCal buffer ownership (`pcal.cpp`, `pcal.h`).** The GPU path accumulates
   pcal bins on the device, then hands the host copy of that buffer to the
   CPU extractor with `setPcal*` and calls `getFinalPCal`. The GPU branch used
   one `pcal_owned` flag and guarded only the real-valued buffer. The merge
   adds one ownership flag per buffer (`pcal_real_owned`,
   `pcal_complex_owned`). The central destructor never frees a borrowed
   buffer, and all six extractors' `clear()` skip zeroing it.

   This fixes two bugs on the GPU branch for complex-sampled data:
   (a) `GPUCore` calls `resetpcal()` (that is, `clear()`) just before
   `finalisepcal()`, so from the 2nd subint on the GPU's pcal was zeroed
   before it was read; (b) deleting the extractor freed the GPU's buffer.
   Both were reproduced with an AddressSanitizer test program and in real
   correlations (section 5).
3. **A leftover debug `printf` was removed.** `DEBUG_UNPACK_CPU_WINDOW` in
   `CPUMode::process` printed from the hot path when
   `subloopindex == 87 && datans == 36500000`.
4. **`gpumode.cu`: `gcd(fs_hz, (double)offset)`.** Dev added
   `int gcd(int,int)` to `mathutil.h`, which made the GPU's `gcd(double, int)`
   call ambiguous and broke the CUDA build. The cast matches the GPU branch's
   own line 599.
5. **`core.h` / `core.cpp`: the value of `RECEIVE_RING_LENGTH` is now in the
   class declaration.** `libmpifxcorr.a` contains the GPU objects, and
   `gpumode.cu` uses `Core::RECEIVE_RING_LENGTH`, but `core.o` is not in the
   library. vis2screen (which links the library) therefore failed to link on
   the GPU branch. The definition outside the class (without a value) is kept
   in `core.cpp` for any use that needs an address. All current uses take the
   value.
6. **`mk5mode.cpp`: guard against the NULL `invalid[]` array** in the
   `goodsamples < 0` path. This crashed CPU jobs that run past the end of
   their data (section 6).
7. Small cosmetic changes: blank lines in `Makefile.am` and `pcal.h`, a doc
   comment for `setPcalComplex`, and removal of stray whitespace and
   commented-out debug lines in the GPU additions to `pcal.cpp`.

The second merge (`eed819a3c`) brought in dev's static-analysis fixes,
`genvex2` and the dev bug fix. Its only conflict was adjacent lines in
`Makefile.am`, resolved by keeping both.

## 5. Testing

All on a desktop (8 cores, RTX 2070 SUPER, CUDA 13.2, Intel IPP), against a
clean build of `gpudifx` installed into a separate prefix.

- **Build.** `install-difx --withcuda --withpython` builds without errors,
  including vis2screen, which failed on the GPU branch.
- **Unit tests.** `make check` in `mpifxcorr/src` links (which needed the
  `_LDADD` fix from section 6). `pcaltest auto` under both IPP and the generic
  FFTW backend: 108 pass, plus the same 2 failures as dev, which the test
  marks "ought to fail". The output is byte-identical to dev's `pcal.cpp`.
- **PCal buffer injection.** A small AddressSanitizer test program repeats
  `GPUCore`'s call order (inject buffer, `clear()`, `getFinalPCal()`, delete)
  for trivial, implicit-shift and complex extractors. The merged code passes
  with no ASan errors or leaks. The GPU branch's `pcal.cpp` fails for complex
  data: the tones are zero after the first subint, and freeing the extractor
  causes a double free.
- **`tests/Synthetic/run-local.sh`, CPU vs GPU in both
  `DIFX_GPU_PIPELINE` modes.** usb, usb-complex, complex-complex, multi and
  dualpol all PASS, with mean differences of 3-4x10^-5 %, the same as the GPU
  branch's own runs.
- **PCal.** The Synthetic suite never exercised pcal (`phaseCalInt = 0`
  everywhere; the GPU branch's notes say GPU pcal was "validated by
  construction/review only"). Three variants with `phaseCalInt = 1`
  (usb-pcal, usb-complex-pcal, complex-complex-pcal) were added locally. They
  are not committed; see open items. The 1 MHz comb lands on the synthetic
  data's continuous test tones. Results:
  - Visibilities PASS for all three.
  - CPU vs GPU pcal agree to about 10^-4 in amplitude and 0.002 deg in phase
    for every station except usb-pcal T2. All stations have all 48 dumps.
  - usb-pcal T2 has a strong tone exactly on a pcal tone. There the GPU is
    0.07% low, and 3x10^-4 of the strong tone leaks into the weak tones. This
    is float32 rounding in the GPU's `atomicAdd` accumulation: two runs of the
    same GPU binary differ by 5x10^-4, and the GPU branch shows exactly the
    same CPU-GPU difference. So it is not caused by the merge.
  - On the GPU branch, every complex-sampled station wrote 1 pcal dump instead
    of 48, at half amplitude (mpifxcorr does not write all-zero pcal lines).
    This confirms bug 4.2 in a real correlation. The merge fixes it.
  - CPU pcal from the merged build and from the GPU branch agree to 10^-5,
    so these scenarios do not show the effect of dev's
    `adjustSampleOffset` change (see open items).

The local test set-up, with build, install, setup script, scenario copies,
outputs and helper tools (`pcal_compare.py`, `vizdiff.sh`,
`inject_test.cpp`, `resolve.py`), is in
`/home/adeller/packages/claude/gpudifx-merge-test/` on the merging machine.
Its `setup-merged.bash` removes an older DiFX install (`difxtrunk`) from the
search paths. Without that, a partial build quietly picked up that install's
libraries.

## 6. Dev issues found along the way

- **Fixed on dev (`67957d4bb`, merged to dev as `dc135ec49`):**
  - The NULL dereference of `invalid[]` in `Mk5Mode::unpack`. Unmodified dev
    crashed in 2 of 3 runs of the Synthetic usb job, at `mk5mode.cpp:195`.
    The job runs for 60 s over 50 s of data. With the fix, 4 of 4 runs
    completed and matched the merged CPU output.
  - `pcaltest` / `m5pcal_test` now link with `${mathlibs}`.
- **Fixed by Walter before the merge:** the `stder` typo in
  `libraries/difxio/tests/teststringarray.c` (`8ecf5579e`), which broke
  `install-difx`.
- **Fixed on dev after the merge (`458af882f`, merged to dev as `9bb152c58`,
  into gpudifx as `bac049fa2`):** why the unpacker returned a negative value.
  Since `db2e775e5`, `samplestounpack = unpacksamples + granularity - 1` was no
  longer a whole number of sample groups. The mark5access decoders work in
  whole groups and return `nsamp - G*nblank`, which came out at -1 when the
  whole window was blank (8 "Error trying to unpack" alerts per Synthetic usb
  job). `samplestounpack` is now rounded up to whole groups, which matches the
  `+ granularity` that `mk5mode_gpu.cu` uses.

## 7. Open items for review and follow-up

PCal and GPU/CPU parity:
1. **GPU pcal phase reference.** The GPU kernel bins samples at
   `(datasamples + global_sample) % N_bins` (old behaviour). The CPU now uses
   `samplenrsincestart`. The two agree in the Synthetic scenarios, but not in
   general. This needs a test case where they place samples in different
   bins, or real pcal data.
2. **GPU pcal bin count.** `gpumode.cu` works out `N_pcal_bins = fs/gcd(fs,offset)`
   itself, always with `fs = 2*bandwidth`. The extractor's `N_bins` comes from
   dev's `getNew`, which uses the tone-spacing denominator and `fs = bandwidth`
   for complex data. Since the GPU buffer is handed to the extractor, the two
   must agree. Suggested fix: take the bin count from the extractor, and reject
   fractional spacing (`getDPhaseCalDenominator() > 1`) and the shifting
   extractors on the GPU, since only the trivial and implicit-shift methods
   are implemented there.
3. **GPU pcal precision:** accumulate in double, or with per-block partial
   sums, to remove the float32 differences of about 10^-3 on strong tones.
4. **Pcal regression test.** Commit `phaseCalInt > 0` Synthetic scenarios and a
   CPU-vs-GPU pcal comparison to `run-local.sh` / `run-slurm.sh`.
5. The GPU code still calls the `float` wrapper `getDPhaseCalIntervalMHz`
   (8 calls). It works, but should use `getDPhaseCalIntervalHz`.

Cleanup:
6. **Leftover debug code:** a live `cout << threadid << endl;` in `Core`
   (prints `0 1 2 ...` in every log), the `static debug_dataweight_cpu_*`
   counters in `cpumode.cpp` (incremented without locking from several
   threads), the duplicated `PCAL_FINAL_DEBUG_*` `#define`s and
   `pcal_debug_target` in `mode.cpp`, debug blocks in `copyPCalTones`, and
   commented-out debug code across the GPU files.
7. `GPUMode` never frees the host `unpackedarrays` that `Mode::Mode`
   allocates (a small leak; the GPU path does not use them).

Build and configuration:
8. CUDA was enabled by default in `mpifxcorr/configure.ac` whenever `nvcc`
   was on PATH (`AC_CHECK_PROG`); `install-difx` always passed
   `--enable-cuda`/`--disable-cuda`, but running configure by hand did not.
   **Resolved after the merge:** CUDA is now opt-in (`--enable-cuda`, default
   no), and `--enable-cuda` without a CUDA compiler is a configure error
   rather than a silent CPU-only build.
9. `architecture.h.gpu.in` was a separate copy of `architecture.h.in` that
   `configure.ac` never used (it generates `src/architecture.h` only from
   `architecture.h.in`, for CUDA builds too), and it had fallen behind (no IPP
   2020 entry, `IPP9`, no `ippsMalloc_*_L`). **Resolved after the merge:** its
   CUDA-specific parts (`#define CUDA 4`, `<cufftw.h>` in the generic section,
   now behind `DIFX_USE_CUFFTW`) were folded into `architecture.h.in`, and the
   copy was deleted. Nothing sets or tests either yet (see `TODO.md`).
10. Source comments point to the `docs/` files that were not merged (section 2).

Not checked:
11. Whether `GPUCore` (which overrides `loopprocess` only) has any GPU-side
    copy of pulsar-bin accumulation that would need dev's change to ignore
    negative bin weights (`229414fda`). Dev's changes to `Core::processdata`,
    `copyPCalTones` and `uvshiftAndAverageBaselineFreq` are inherited.
12. LSB, complex double-sideband, and non-VDIF formats remain `NOT_SUPPORTED`
    on the GPU (as on the GPU branch) and were tested on the CPU only.
