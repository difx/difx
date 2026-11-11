# fftbench — results & remaining steps

> **RESOLVED (2026-07-22): cuFFT was NOT the source of the production
> per-station sync.** fftbench confirmed `cufftExecC2C` is async on both the
> 2070 and the A100 cluster (probe returns ~0 ms after a 50 ms backlog).
> The production per-station `cudaStreamSynchronize` (~10/subint) was then
> traced - by stubbing the FFT (sync count unchanged) and an nsys
> `--cudabacktrace=sync` stack - to `Mk5_GPUMode::unpack_all` ->
> `valid_frames->sync()`, a whole-stream drain only the host-weights
> fallback needs. Fixed with RING-deep host staging (docs/gpu-changes.md §9,
> docs/gpu-plan.md item 0). The cross-station batched-plan (strategy C) FFT-
> efficiency win remains a separate, smaller future idea (cufftdx / fusion).
> The rest of this file is the original investigation, kept for the record.


Standalone cuFFT microbenchmark (`fftbench.cu` + `Makefile`) built to pick
the fix for the per-station cuFFT sync seen in the 2026-07-21 A100 profile
(docs/gpu-profiling.md, gpu-plan.md work item 0). Reproduces the faithful
benchprof FFT config: n=128 (fftchannels), batch/station=160 (16 bands x
10 buffered FFTs), 10 plans on ONE shared stream, 400 subints, C2C fwd.
Three strategies: **A** default work-area alloc (current code), **B**
explicit per-plan work area, **C** single plan batching all 10 stations.

Build/run: `make run` (desktop, sm_75). `make GPU_ARCH=sm_80` for the A100.

## Results so far — DESKTOP only (RTX 2070 SUPER, CUDA 13.2, cuFFT 12.2.0.57)

```
probe (after ~50 ms backlog): cufftExecC2C host-return 0.00 ms => cuFFT is async
                         (no filler)              (+~0.9 ms/station filler)
  A default   host 8.9ms gpu 12.0ms @2.2us    host 2590ms gpu 2965ms
  B per-plan  host 7.3ms gpu  9.8ms @1.8us    host 2566ms gpu 2941ms   (work area = 0 bytes)
  C single    host 0.5ms gpu  2.6ms @1.3us    host 2254ms gpu 2941ms
```

**Findings (desktop):**
1. **`cufftExecC2C` is ASYNC on this cuFFT** — the single-op probe returns
   in 0.00 ms even with 50 ms of work already queued. No internal stream
   sync. So on the desktop there is *no* per-exec sync to remove.
2. **Per-plan work area (B) gives nothing** — identical to A, and cuFFT
   reports a **0-byte** work area for this transform. Candidate fix (b) is
   dead regardless of the sync question.
3. **Single batched plan (C) uses ~4x less GPU time** (2.6 vs 12 ms,
   no-filler) and one launch/subint instead of ten — tiny 128-pt FFTs are
   dominated by per-launch/setup cost, which batching amortises. Real
   efficiency win independent of the sync question (production vector_fft
   is 413 ms / 8.1%; batching could cut it to ~100 ms).
4. The `+filler` rows show all three throttling host~=gpu — that is
   **launch-queue back-pressure** (thousands of ~1 ms ops overflow the
   ~1024-deep async queue = GPU saturated), the WANTED state, NOT a sync.
   (An earlier version mislabelled this "sync"; the single-op probe is the
   authoritative test.)

**This contradicts the production A100 profile**, which showed ~3947
`cudaStreamSynchronize` on the process thread matching cuFFT's per-exec
footprint (`cuStreamIsCapturing`/`cuLaunchKernel` ~3948). Desktop cuFFT
does not sync => either the **A100 cluster cuFFT version syncs per exec**
(version-dependent behaviour) or production's syncs were misattributed.
Deciding this is the whole point of the remaining steps.

## Remaining steps to finalise (nsys)

1. **Local nsys cross-check (was mid-run when we stopped).** Profile the
   binary and count the CUDA API calls, same method as production:
   ```
   nsys profile --trace=cuda,nvtx --force-overwrite true -o fftbench-prof ./fftbench 400
   nsys export --type sqlite --force-overwrite true -o fftbench-prof.sqlite fftbench-prof.nsys-rep
   sqlite3 fftbench-prof.sqlite "SELECT s.value,COUNT(*) FROM CUPTI_ACTIVITY_KIND_RUNTIME r \
     JOIN StringIds s ON r.nameId=s.id GROUP BY s.value ORDER BY 2 DESC;" | grep -iE 'Synchronize|Capturing|LaunchKernel'
   ```
   Expect (if desktop cuFFT is async): `cuStreamIsCapturing` ~8800 (one per
   exec) but `cudaStreamSynchronize` only the handful my code calls
   explicitly. That confirms the count-match in production came from a
   *syncing* cluster cuFFT, not coincidence.
2. **Run fftbench on the A100 cluster** (`make GPU_ARCH=sm_80`, or via an
   sbatch like benchprof-profile.sbatch). Read the probe line: if it says
   **"SYNCS internally"**, the cluster cuFFT is the culprit and batching
   (C) is the fix direction (1 sync/subint instead of 10 — plus the 4x FFT
   efficiency). If it says **"is async"**, re-open the production sqlite and
   find the real source of the 3947 syncs (check which CUDA API precedes
   each sync; re-scan the device-path code).
3. **Identify the cluster cuFFT/CUDA version** (module list on the node, or
   TARGET_INFO in the production sqlite) and compare to desktop 12.2.0.57.
4. **Decide & implement the fix** in gpumode.cu (plan creation ~line 451):
   most likely the single batched plan (C) — bank the 4x FFT efficiency
   regardless, and it collapses 10 syncs/subint -> 1 if the cluster cuFFT
   syncs. Then re-run the Synthetic CPU-vs-GPU suite + benchprof-profile.sbatch.

Note: batching all stations into one plan means one shared big in/out FFT
buffer and one exec after all 10 stations' fringe rotation — a real
restructure of the per-station tofft loop, not a one-liner. Weigh against
just accepting 1 sync/subint if that already recovers most of the idle.
