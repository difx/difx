---
name: gpu-parity-test
description: Run the tests/Synthetic CPU-vs-GPU correctness suite on gpudifx (run-local.sh, both GPU pipeline modes, optional phase-cal comparison) and interpret the result. Use after any mpifxcorr change that could alter output, after merging dev into gpudifx, and before committing GPU or shared correlator code.
---

# GPU parity test (gpudifx)

The GPU correlator must produce the same visibilities as the CPU correlator, to
floating-point rounding (not bit for bit). `tests/Synthetic/run-local.sh` is the
single-machine check: it correlates each synthetic scenario three times (`cpu`,
`gpu0` = `--usegpu` with `DIFX_GPU_PIPELINE=0`, `gpu1` = `DIFX_GPU_PIPELINE=1`),
compares each GPU run with the CPU run using `diffDiFX.py`, and prints a
PASS/FAIL/SKIP table. It exits non-zero if anything fails. On a SLURM cluster,
`run-slurm.sh` does the same job.

## 1. Before running

Check each of these; a wrong answer here gives a meaningless result.

1. **The installed binary is the one you mean to test.** Source `setup.bash`, then
   `which mpifxcorr` must be `$DIFXROOT/bin/mpifxcorr`, and it must be newer than your
   last source change. If in doubt, run the `rebuild-and-verify` skill first. A stale
   install is the most common cause of a confusing result on this project.
2. **Test data exists.** The VDIF files in `tests/Synthetic/` are git-ignored (about
   3.6 GB). Generate them with `./createData.sh`, or symlink them from another checkout
   that already has them. `ls tests/Synthetic/*.vdif` should list them.
3. **`tests/Synthetic/threads` exists.** Every `.v2d` names `threadsFile = threads`, but the
   file is not committed yet (it is on the wiki to-do list, linked from `TODO.md`). If it
   is missing, create it with the two lines `NUMBER OF CORES:    1` and `10`.
4. **Scenario size.** Most scenarios are committed at full size (`nChan=4096`, long scans)
   and take a long time. Scaling `nChan` or the scan length down in your working copy is
   fine for a quick check, but **never commit those edits**.
5. **The GPU is free**: `nvidia-smi` shows no other `mpifxcorr`, `nsys` or `ncu`.

## 2. Run

```bash
cd tests/Synthetic
mkdir -p local-runs && ./run-local.sh 2>&1 | tee local-runs/run-local-$(date +%Y%m%d-%H%M).log  # all
./run-local.sh usb usb-complex multi dualpol                               # a subset
```

- Job directories, `.mpilog` files and diffDiFX output go under `local-runs/<scenario>/<mode>/`.
  Keep logs there, next to the jobs, not in a temporary directory, so someone else can
  look at them.
- mpifxcorr reports warnings and errors over difxmessage, not on stdout, so `run-local.sh`
  runs `difxlog` beside each job: the alerts are in `<jobdir>/test-<scenario>.difxlog`, and
  the run output flags any ERROR, SEVERE or FATAL lines. A PASS with logged errors still
  needs looking at.
- Overrides: `DIFF_THRESHOLD` (default `0.0005`), `WORKDIR`, `SETUP_SCRIPT`, `NTASKS`.
- Reference build: set `REFERENCE_SETUP_SCRIPT` to another DiFX build's `setup.bash`
  (typically a dev checkout, ideally a `git worktree` with its own `setup.local.bash` and
  install prefix) to add a CPU run made entirely with that build's tools and a
  `cpu:reference` column. It catches changes to CPU output relative to dev, which the
  CPU-vs-GPU columns cannot see.
- Scenarios in `GPU_UNSUPPORTED` (none at present) are SKIPped on the GPU legs.
  Remove a scenario from that list only in the commit that implements it on the GPU.
- The lower/double sideband scenarios (`lsb`, `lsb-complex`, `usb-dsb`, `lsb-dsb`) have
  one more leg, `gpucompat` (`DIFX_GPU_SIDEBAND_CPUCOMPAT=1`, a GPU test path that
  reproduces the CPU's post-FFT sideband handling). For those scenarios `cpu:gpucompat`
  is the CPU-vs-GPU check and must PASS. `cpu:gpu0` and `cpu:gpu1` are diffed with
  channel 0 excluded and reported as **INFO**, which never passes or fails the run: the
  production GPU path converts sidebands while decoding, so it differs from the CPU by
  around 0.03% (DSB) to 0.06% (LSB). This exception (`SIDEBAND_INFO_ONLY` in
  `run-local.sh`) goes once the CPU handles sidebands the same way.
- The production GPU sideband path is checked instead by the **sideband invariance**
  section: the long-baseline scenarios (`lb-*`, 3200 km, data simulated with delays by
  `createLongBaseline.sh`, which needs `vdifsim`) hold the same signal recorded as USB and
  as LSB, and `lb-lsb`/`lb-lsb-complex` on `gpu0` and `gpu1` must match their USB
  partner's CPU output to the threshold. `run-local.sh` includes them whenever
  `TESTLB1.vdif` exists; generate the data before a merge or a sideband change.
- Which scenario covers what: `multi` is the only multi-station, multi-band one; `dualpol`
  is the only one with cross-polarisation autocorrelations; `usb-complex` and
  `complex-complex` cover complex sampling. A change to one of those code paths needs its
  scenario.

Run the scenarios that cover the code you changed. Before a merge, or a commit to
shared code, run all of them.

## 3. Phase cal (until pcal scenarios are committed)

The committed scenarios have `phaseCalInt = 0`, so they do **not** test phase-cal
extraction. If your change touches pcal (`pcal.cpp`, the `DOPCAL` paths in
`gpudecode.cu`, pcal handling in `gpumode.cu`, `mode.cpp`), make local copies of a scenario
with `phaseCalInt = 1` in each ANTENNA block of the `.v2d` (usb, usb-complex and
complex-complex at least), run CPU and GPU, and compare the `PCAL_*` files tone by tone.
A comparison script exists in the merge-test area (`pcal_compare.py`; see
`mpifxcorr/GPU_MERGE_NOTES.md`); committing it with the scenarios is on the wiki to-do
list.

Expected: CPU and GPU agree closely, except on strong on-grid tones in real (non-complex)
data, where float32 `atomicAdd` on the GPU gives differences around 1e-3 that also vary
from run to run. That is known (wiki to-do list, GPU pcal precision), not a regression. An
all-zero GPU dump after the first subint, or a constant phase offset, is a real bug.

## 4. Reading the result

- **`diffDiFX.py` always exits 0.** Failures show only as printed `THRESHOLD EXCEEDED!`,
  `EPSILON EXCEEDED` or header-disagreement lines. `run-local.sh` searches for them; if
  you call `diffDiFX.py` by hand, search its output yourself.
- PASS means agreement within the threshold. Typical CPU-vs-GPU differences are around
  1e-5 to 1e-4 percent.
- Both `gpu0` and `gpu1` must PASS (INFO on the sideband scenarios, see above). If only one fails, suspect the pipelined
  transfer and host-tail overlap path (`DIFX_GPU_PIPELINE=1`) or a race.
- **Never check a GPU change by comparing two GPU runs byte for byte** (`cmp`, checksums).
  The XMAC accumulates with `atomicAdd`, so the same binary gives slightly different
  output each run. The test is always CPU vs GPU, within the threshold.
- **Never loosen the threshold to make a test pass** without the user agreeing and a
  written reason.

## 5. When something fails

1. `diffDiFX.py --diagnose -i <exp>.input -t 0.0005 <cpu.difx> <gpu.difx>` fits a complex
   gain per record. An amplitude-only difference points to weights; a phase rotation
   points to timing or delay (sample offset, fringe rotation); a large residual points to
   corrupt or misaligned data.
2. Localise it to a pipeline stage with the debug environment variables, which print
   matching lines from the CPU and GPU paths:
   - `DIFX_SPEC_DEBUG=<datasec>`: unpacked samples, FFT input and first spectral channels.
     Sort the matching lines from both logs and diff them.
   - `DIFX_WEIGHT_DEBUG=<datasec>`: per-window data weights and frame classes. **It must be
     run with `DIFX_GPU_PIPELINE=0`**; GPUCore refuses to start otherwise.
3. If a failure comes and goes between rebuilds, disappears under compute-sanitizer or
   ASan, or changes with tiny unrelated edits, **suspect a stale or mismatched build
   before suspecting a race**: rebuild cleanly and check the install (see
   `rebuild-and-verify`).
4. Look at the CPU twin of the code you changed (see the CPU/GPU map in the
   `difx-bug-review` skill). Most failures on this branch are one path being changed
   without the other.

## 6. Report

Give the table, the git HEAD it was run at, the `mpifxcorr` install time, which
scenarios were run or scaled down, and where the logs are. Say plainly if anything was
skipped. Do not report "tests pass" if only a subset was run.
