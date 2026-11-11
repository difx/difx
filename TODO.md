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
- [ ] GPU pcal precision: accumulate in double or with per-block partial sums (float32 `atomicAdd`
      differs by about 1e-3 on strong tones).
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
- [ ] `architecture.h.gpu.in` has already fallen behind `architecture.h.in` (no IPP 2020 entry,
      uses `IPP9`): generate both from one template.
- [ ] Offer the `setup.local.bash` / `setup.local.csh` mechanism to dev (it is generic).
- [ ] Continuous integration: a self-hosted runner on the GPU desktop running
      `tests/Synthetic/run-local.sh` (correctness) and `run-bench.sh` (benchmark ledger).
- [ ] Upstream (dev): mark5access decoders return `nsamp - 4*nblank`, which comes out at -1 on
      fully blank data when `nsamp` is not a multiple of the sample granularity, as now
      happens after `db2e775e5`. `mk5mode_gpu.cu` still uses `+ granularity`.

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
- [ ] Decide keep/remove for the WDEBUG/HDRDEBUG/SPECDEBUG and NVTX scaffolding.
- [ ] PCal: a first-class "ingest folded bins" interface and one reset/finalise path for CPU
      and GPU (today the GPU path half-uses the extractor via `config->get_use_gpu()`).
- [ ] Latent issues: XMAC plans assume all datastreams are configured alike; the CUDA stream
      is created and destroyed on every `processgpudata` call; the `set_weights` edge case
      `nearestSamples==0` reads a stale weight.

## Longer term

- [ ] **Interlaced-VDIF demux on the DataStream: the real production bottleneck** (~1.7 Gbps/core).
      Reorder per-thread frames by memcpy, then de-interleave channels on the GPU.
- [ ] Subints larger than GPU memory: process a subint in chunks of FFT windows.
- [ ] Tensor cores for the XMAC (needs an accuracy assessment).
- [ ] cufftdx to fuse the FFT with the fractional-sample correction.
- [ ] Overlap input H2D with compute across datastreams (worth <1% today).
