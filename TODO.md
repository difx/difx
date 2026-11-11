# gpudifx to-do

The shared backlog for the GPU correlator work on `gpudifx`. Keep it current: add items as
they come up, delete them when they land (the commit message and, for substantive changes,
the change log record what was done), and keep each entry short, with a pointer to where
the detail lives. Seeded on 2026-11-11 from the ADACS2023B plan (`docs/gpu-plan.md` on the
`adam-performance-gains` branch) and the open items in `mpifxcorr/GPU_MERGE_NOTES.md`.

## Agreed order (next up)

1. **Autocorrelations into Core/XMAC.** Removes the autocorrelation atomics, the cross-pol
   traffic and a whole host-tail data path. Built and gated off (`DIFX_GPU_XMAC_AUTOCORR`,
   default 0): correct, but 7% slower on the RTX 2070, so the A100 measurement below decides
   whether it lands. Design and increments: ADACS `docs/gpu-autocorr-design.md`.
2. **FP16 spectra**, in three stages: (a) standalone `fftbench` accuracy/speed probe;
   (b) opt-in gated implementation; (c) **science-level accuracy gate (fringe-fit S/N and
   image S/N) is a prerequisite, not a follow-up**. diffDiFX cannot validate FP16 at any
   threshold. Start (c) alongside (a).

## Pending measurements (cluster)

These use `tests/FakeData/*.sbatch`, which were not merged into gpudifx (run them from the
ADACS checkout, or bring site-neutral versions across first).
- A100 autocorrelation A/B: `DIFX_GPU_XMAC_AUTOCORR=0` vs `1` with `benchprof-profile-nsys-5s.sbatch`.
  Compare µs/call for `gpu_resultsrotatorMultiply` and `gpu_fuse_xmac_and_average`, and NVTX `host_finalize`.
- A 20 s wall-time run at the current commit (`benchprof-profile-nonsys-20s.sbatch`,
  `--cpus-per-task=5`). The published A100 figure (24.5 s) predates the last two speed-ups.

## Merge follow-ups (detail: `mpifxcorr/GPU_MERGE_NOTES.md` section 7)

- [ ] GPU pcal phase reference: move the GPU kernels to dev's `samplenrsincestart`, and build
      a test case where the old and new sample offsets give different bin positions (or use real pcal data).
- [ ] GPU pcal bin count: take `N_bins` from the extractor; reject fractional tone spacing and
      the shifting extractors on the GPU.
- [ ] **Do a real pcal test** (real baseband data with an injected pcal comb, CPU vs GPU) before
      changing the GPU pcal accumulation. The Synthetic data has a single CW tone, not a comb, so
      its other "tones" are noise and overstate the error.
- [ ] GPU pcal precision. The GPU sums each bin over a whole subint into one float (`atomicAdd`);
      the CPU does the same per Core thread (so its pcal also depends slightly on the thread
      count). Measured on Synthetic TEST2 (strong on-grid tone): ~1e-3 on the tone. The summed
      values are a few fixed quantisation levels, so the rounding is a bias that grows with the
      accumulator, worst for long subints. Options, decide after the real-data test:
      (a) partial bins per group of consecutive FFT windows, folded once per subint (patch kept
      at `gpudifx-merge-test/tools/pcal-window-group-partials.patch`; ~1e-5 on a modelled comb,
      but not reliably better on a single strong tone);
      (b) exact integer counts per bin (net counts of the +-1 and +-HiMag levels, or of the code
      for >=4-bit), converted to float once per subint: exact, no double precision, order
      independent, so bit-reproducible.
- [ ] Commit `phaseCalInt > 0` Synthetic scenarios and a CPU-vs-GPU pcal comparison to
      `run-local.sh` / `run-slurm.sh` (the scenarios and `pcal_compare.py` exist locally in the merge-test area).
- [ ] Switch the GPU code from `getDPhaseCalIntervalMHz` to `getDPhaseCalIntervalHz` (8 calls).
- [ ] Commit `tests/Synthetic/threads`: every `.v2d` there names `threadsFile = threads`, but the
      file only exists in local checkouts.
- [ ] Remove leftover debug code: `cout << threadid` in `Core`, the `debug_dataweight_cpu_*`
      counters, `PCAL_FINAL_DEBUG_*`, debug blocks in `copyPCalTones`, commented-out printf.
- [ ] `GPUMode` leaks the host `unpackedarrays` allocated by `Mode::Mode`.
- [ ] Check whether `GPUCore` needs dev's "ignore negative pulsar bin weights" change.
- [ ] Source comments point to `docs/gpu-*.md`, which was not merged. Decide where the GPU
      design notes and change log live now, then fix the references.

## Build and infrastructure

- [ ] **Make `install-difx` build out of tree by default, and clean it up.** Today an
      out-of-tree build means running it from a separate directory by path; run in the
      repository, it fills the tree with build products. Ideas: a `--builddir` option or
      `DIFX_BUILDDIR` (default outside the source tree) and a warning on in-tree builds;
      one component failing (e.g. a test program) should not stop that component's library
      from installing; `--noconf` should refuse components that have never been configured.
- [ ] CUDA should be opt-in in `mpifxcorr/configure.ac`. Today it is enabled whenever `nvcc`
      is on PATH; `install-difx` passes the flag explicitly, but running configure by hand does not.
- [ ] Continuous integration: a self-hosted runner on the GPU desktop running
      `tests/Synthetic/run-local.sh` (correctness) and `run-bench.sh` (benchmark ledger).
- [ ] Widen the test matrix beyond CPU vs GPU per scenario: (a) 1 thread vs N threads per
      Core (with 1 thread the CPU output is byte-identical run to run, so it can be checked
      exactly; N threads can expose ordering and race bugs); (b) several subintegration sizes;
      (c) other job parameters that change how data is split (e.g. integration time, guard
      samples). Wire them into `run-local.sh` / `run-slurm.sh` as optional axes.

## GPU feature queue

- [ ] `perbandweights` on the GPU path (interlaced VDIF, as in `Mk5_CPUMode`).
- [ ] LSB (unblocks the lsb, lsb-complex and dsb scenarios); complex double-sideband.
- [ ] CODIF: per-format frame-validity hook, widen the `getMode` format gate, add the
      `datalengthbytes <= getMaxDataBytes` clamp first.
- [ ] Multiple phase centres and pulsar binning: unsupported on the GPU without warning. Add a
      `NOT_SUPPORTED` guard at startup, then implement.
- [ ] STA dumps when autocorrelations move into the XMAC: refuse explicitly (decided 2026-08-28).
- [ ] Per-sample precision drop in fringe rotation (made moot if FP16 lands).

## Refactor (before gpudifx merges into dev)

- [ ] `Core` becomes a virtual base with `CPUCore`/`GPUCore`; `Mode` becomes a virtual base, with
      `Mk5Mode` as the frame-based branch and `CPUMk5Mode`/`GPUMk5Mode` under it.
- [ ] Rename and probably restructure `mk5mode` / `mk5mode_gpu`: at minimum `cpumk5mode` /
      `gpumk5mode`. More importantly, the GPU class is a misnomer: it does its own unpacking,
      does not link against mark5access, and can only handle frame-based formats, so its name
      and place in the hierarchy should say "GPU, frame-based" rather than "Mark5". Settle
      this together with the `Mode` hierarchy item above.
- [ ] Decide keep/remove for the WDEBUG/HDRDEBUG/SPECDEBUG and NVTX scaffolding.
- [ ] PCal: a first-class "ingest folded bins" interface and one reset/finalise path for CPU
      and GPU (today the GPU path half-uses the extractor via `config->get_use_gpu()`).
- [ ] Latent issues: XMAC plans assume all datastreams are configured alike; the CUDA stream
      is created and destroyed on every `processgpudata` call; the `set_weights` edge case
      `nearestSamples==0` reads a stale weight.

## Longer term

- [ ] Route the GPU code through the `architecture.h` abstraction (`ARCH == CUDA` and
      `DIFX_USE_CUFFTW` exist in `architecture.h.in` but nothing sets or tests them yet), so the
      GPU code path could also be built without CUDA.
- [ ] **Interlaced-VDIF demux on the DataStream: the real production bottleneck** (~1.7 Gbps/core).
      Reorder per-thread frames by memcpy, then de-interleave channels on the GPU.
- [ ] Subints larger than GPU memory: process a subint in chunks of FFT windows.
- [ ] Tensor cores for the XMAC (needs an accuracy assessment).
- [ ] cufftdx to fuse the FFT with the fractional-sample correction.
- [ ] Overlap input H2D with compute across datastreams (worth <1% today).
