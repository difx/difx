# FakeData benchmarks: archived numbers

Superseded measurements, split out of `BENCHMARKS.md` on 2026-08-28 so the
live ledger stays readable. Nothing here was edited on the way across. These
numbers are kept because they are the baselines the current ones are quoted
against - not because they still describe the code.

## A100 cluster profiling (benchprof-profile-nsys-5s.sbatch, 10 stations)

Note on windows: subints are 10 ms, so the profiling window scales linearly
(4 s = 400 subints/station, 5 s = 500, 20 s = 2000). Rows here span a mix of
4 s and (2026-07-23) 5 s windows, so absolute ms are NOT directly comparable
across rows - the **per-subint** figures in the fused-row note below, and idle
%, are the window-independent quantities. All default to single-thread VDIF. A
~20 s capture is NOT usable under nsys 2022.2.1 (its injection library
segfaults ~13 s in - see gpu-profiling.md); the 20 s soak run is nsys-free
(benchprof-profile-nonsys-20s.sbatch).

Clean GPU-bound profile: A100-SXM4-80GB, one rank/core (isolation via
`--cpus-per-task`; see the Rules above),
fake data, Core rank wrapped with nsys (`.nsys-rep` + sqlite kept under
the run dir). "GPU span" = first-kernel to last-kernel; "busy (kernels)"
= sum of kernel durations (single compute stream, so serial); "busy
(+copy)" additionally counts the H2D/D2H copies that share the compute
stream. This is a diagnostic ledger (idle %), not the T5-T1 metric.

| date | commit | GPU span (ms) | busy kernels (ms) | idle kernels | busy +copy | idle +copy | notes |
|---|---|---|---|---|---|---|---|
| 2026-07-21 | pre-overlap (80f6e291a) | 10497 | 5471 | 47.9% | — | — | reference (tooarrana clean profile, per gpu-plan notes) |
| 2026-07-21 | 7b8e31104 | 8798 | 5088 | 42.2% | 5695 | 35.3% | host-tail overlap; idle did NOT collapse |
| 2026-07-22 | unpack-drain fix, INTERLACED | 9858 | 5077 | 48.5% | 5731 | 41.9% | sync fix held (no `cudaStreamSynchronize` storm) but GPU still starved by the DataStream corner-turn — see §10 |
| 2026-07-22 | + single-thread VDIF (genheaders), 4 s | 6050 | 5058 | 16.4% | 5614 | **7.2%** | corner-turn removed; kernel busy unchanged (5058≈5077) so the win is pure idle removal; wall (CUDA-API span) 12169 → 7886 ms (**~35%**); GPU now compute-bound |
| 2026-07-23 | fused unpack+fringe (619c273be), 5 s | 7032 | 4863 | 30.9% | 6337 | 9.9% | fused kernel replaces unpack+fringe; kernel-only idle rises (faster kernels ⇒ fixed tail is a bigger fraction) but the H2D copies now fill it (idle+copy 9.9%). See the per-subint comparison below. |
| 2026-07-23 | fused sum_weights (d503b7708), 5 s | 5800 | 4500 | 22.4% | 5208 | 10.2% | `gpu_sum_weights` gone from the mix; kernel busy −7.4%, exactly its old share. Same 5 s window as the row above ⇒ directly comparable. Wall (CUDA-API span) 9064 → 7710 ms, but see the H2D caveat below — only the kernel-busy part is attributable. |
| 2026-08-26 | placement report (56eb3b4c3), 5 s | 5852 | 4545 | 22.3% | 5159 | 11.9% | SHARED node (1 core/task) — kept because it is the run that **disproved** the NUMA hypothesis: definitively mis-placed (GPU node 1, rank node 2) yet the fastest H2D of any capture, 13.01 GB / 588.6 ms = **22.1 GB/s**. Kernel busy matches the row above to <1%, as expected for a logging-only change. See gpu-changes.md §14. |

**Weight-kernel elimination, measured against the row above** (both captures
are 5 s / ~495 subints per station, so the totals compare directly). Capture +
analyser output: `tests/23072026-weightkerneliminated/benchprof/nsys/`.

| kernel | fused unpack+fringe | + fused sum_weights | Δ |
|---|---|---|---|
| `gpu_sum_weights` `<<<1,1>>>` | 373.2 ms (7.7%) | **absent** | −373.2 ms |
| `gpu_set_weights` | 22.5 ms (0.5%) | 40.3 ms (0.9%) | +17.8 ms (the atomicAdd) |
| **net kernel busy** | 4862.5 ms | 4500.5 ms | **−362 ms (−7.4%)** |

Every other kernel is stable to <0.5% across the two captures
(`gpu_resultsrotatorMultiply` 1894.9 → 1894.7 ms, `gpu_fused_fringe` 948.6 →
946.8, `vector_fft` 484.0 → 482.0), which is the cross-check that the two runs
are comparable and that nothing but the weight path moved.

**⚠ H2D caveat — do not read the whole wall gain as ours.** The same volume of
input moved far faster in the second capture: **12.90 GB / 1444.7 ms (8.9 GB/s)
→ 12.88 GB / 683.0 ms (18.9 GB/s)**, at an unchanged copy count (~29.7k). No
part of this change touches the transfer path. The first of those runs was
simply **contended** — it shared its node with another job, which stalls the
input copies (gpu-changes.md §14 has the per-copy distributions that show it).
Of the ~15% CUDA-API-span improvement, only the ~7.4% kernel-busy part is
attributable to the code. **This is why ledger runs must reserve most of the
node** (`sbatch --cpus-per-task=5 …`; tooarrana rejects `--exclusive`), and why
the two 2026-07-23 rows are comparable on kernel busy but not on wall.

New A100 kernel mix (% of kernel busy, 4500.5 ms): `gpu_resultsrotatorMultiply`
**42.1%** (the #1 target — memory-bound, the FP16 candidate),
`gpu_fuse_xmac_and_average` 22.7%, `gpu_fused_fringe` 21.0%, `vector_fft`
10.7%, `gpu_baseline_weights` 1.4%, `gpu_set_weights` 0.9%, precompute 0.7%,
`gpu_blank_frames` 0.6%. Busy-union (kernels+copies) is **89.8%** of the span,
and 74% of the residual idle is now in sub-20 µs inter-kernel gaps — launch
overhead, not host starvation. H2D at 683 ms is 11.8% of the span (the
remaining wall floor under the FP16 work).

**Fused unpack+fringe, per-subint (window-independent) vs the pre-fusion 4 s
profile above** (normalising each kernel's total by its launch count, since the
two captures cover 400 vs 500 subints/station):

- unpack + fringe (the fused portion): **0.509 → 0.197 ms/subint (−61%)**
- total GPU kernel busy: **1.28 → 0.98 ms/subint (−23%)**
- wall (CUDA-API span ÷ launches): **2.00 → 1.83 ms/subint (−8%)**

The GPU-busy win is large; wall gains less because the run is no longer
GPU-busy-bound. New A100 kernel mix (% of kernel busy): `gpu_resultsrotatorMultiply`
**39%** (now the #1 target - memory-bound, an FP16 candidate), `gpu_fuse_xmac_and_average`
21%, `gpu_fused_fringe` 19.5%, `vector_fft` 10%, **`gpu_sum_weights` `<<<1,1>>>` 7.7%**
(a single-thread reduction), weights/precompute/blank <2% each. H2D is now
~12.9 GB / 1444 ms on the compute stream (~20% of span) - a new wall floor the
fusion doesn't touch. **Follow-up: `gpu_sum_weights` deleted** - its
sum folded into `gpu_set_weights` (per-window atomicAdd), so that 7.7% should be
gone from the next A100 profile (2070 T5-T1 12.3→11.8).

The overlap shortened the GPU span ~16% (10.5 -> 8.8 s) and trimmed idle
~48% -> ~42% (kernels-only), but did **not** collapse the between-subints
gap. Root cause of the residual idle (2657 ms, 85.6% of it in >500 us
gaps): the process thread issues ~10 `cudaStreamSynchronize` per subint
(5.58 s total) - one per station, matching cuFFT's per-`cufftExecC2C`
driver footprint (`cuStreamIsCapturing`/`cuLaunchKernel`/`cudaStream-
Synchronize` all ~3948). cuFFT is synchronising the compute stream on
every FFT exec, serialising host and GPU at station granularity. The
host tail the overlap moved (~900 us/subint) was never the bottleneck.
See gpu-plan.md work queue.

**CORRECTION (2026-07-22): the sync is NOT cuFFT.** The
`cudaStreamSynchronize` count matched `cufftExecC2C` only because unpack
and the FFT sit in the same per-station tofft iteration. cuFFT is async
(the fftbench probe returns ~0 ms after a 50 ms backlog on this A100);
stubbing the FFT left the sync count unchanged; an nsys sync-backtrace
resolved the caller to `Mk5_GPUMode::unpack_all` -> `valid_frames->sync()`.
Removed on the device path (gated to the host-weights fallback) with
RING-deep host staging to keep the tail-overlap safe - see gpu-changes.md
§9 and gpu-plan.md work-queue item 0.

**Resolved (2026-07-22, §10 / last two table rows):** the drain removal
alone left the A100 idle at ~42% - the true limiter was the DataStream
INTERLACEDVDIF corner-turn, not any Core-side sync. Rebuilding fake data
as single-thread VDIF (mark5access `genheaders`, `191528ff9`) removed the
corner-turn and the idle collapsed **41.9% → 7.2%**, wall 12.2 → 7.9 s
(~35%). Kernel busy is unchanged (5058 ≈ 5077 ms), so the speed-up is
purely the GPU no longer starving. The residual 7.2% idle is now mostly
small (<20 us) inter-kernel gaps; the GPU is compute-bound, with
`gpu_resultsrotatorMultiply` (28.9%), `gpu_unpack` (24.5%) and the
`<<<1,1>>>` `gpu_sum_weights` (5.8%) the next kernel targets.

## Pre-protocol reference points (15 s of data, tInt 2 s, whole-run wall)

Measured 2026-07-18 during Lever A work; not comparable to the ledger
rows above, kept for the record:

| build | wall (15 s data) |
|---|---|
| pre-Lever-A (1bf496b16+build fixes) | 268.0 s |
| Lever A pinned (785bcaec0) | 258.9 s |
| Lever A, staging fallback (`DIFX_GPU_PIN_INPUT=0`) | 269.3 s |
