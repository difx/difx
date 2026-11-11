# GPU correlator benchmark ledger

## Protocol

Metric: **T5 − T1** — wall time of a 5 s FakeData correlation minus wall
time of a 1 s correlation (same config, `tInt = 1 s`, 20 ms subints,
10 stations). Differencing cancels the large constant start-up/shutdown
cost and measures the steady-state correlation rate: the metric is the
cost of 4 s of data.

Run it with `./run-bench.sh [repeats]` (default 3; report best-of-N to
suppress scheduling noise on the oversubscribed desktop). `USEGPU=0`
benchmarks the CPU path; `EXTRA_ENV="DIFX_GPU_PIN_INPUT=0"` etc. for A/B
legs. The script prints a ready-to-paste ledger row.

**Since 2026-07-22 the benchmark defaults to single-thread VDIF** (the fake
data is rewritten `INTERLACEDVDIF→VDIF` after vex2difx) so it measures the GPU
correlator, not the DataStream interlaced-VDIF corner-turn (the demux that
dominated the pipeline - see the A100 section §10). Set `SINGLE_THREAD_VDIF=0`
to keep the interlaced demux. Rows dated on/before 2026-07-21 are all
interlaced and are NOT directly comparable to single-thread rows.

Rules:
- Machine otherwise idle (no builds, no nsys, no browser). **On the cluster
  that means `sbatch --cpus-per-task=5 <script>`** — a co-tenant job stalls the
  input copies and costs ~10% of wall with no other symptom (gpu-changes.md
  §14). tooarrana rejects `--exclusive`, so isolation comes from reserving most
  of the node: 5 × 12 tasks = 60 of gina's 64 cores. That does **not** start
  more mpifxcorr ranks (the rank count is `--ntasks`). **Keep `--nodes=1`** —
  without it SLURM satisfies a wide `--cpus-per-task` by splitting the ranks
  across two nodes, which reserves a GPU per node and pushes some
  DataStream→Core traffic onto the fabric instead of intra-node CMA (seen
  2026-08-26). Each run prints a `node ownership: N/M cores` verdict and warns
  if the allocation spanned more than one node; anything under 90%, or
  multi-node, does not belong in this file.
- Record every change that lands: benchmark at the commit that follows it.
- Note the VDIF threading (single-thread default vs `SINGLE_THREAD_VDIF=0`).
- FakeData output is NOT bit-reproducible run to run (see README) - this
  ledger tracks time only, never correctness.

## Ledger (RTX 2070 desktop, ar313)

| date | commit | mode | T1 (s) | T5 (s) | T5−T1 (s) | notes |
|---|---|---|---|---|---|---|
| 2026-07-18 | 785bcaec0 | gpu | 24.0 | 90.7 | 66.7 | first ledger row; Lever A (pinned input) in |
| 2026-07-18 | f39cd5bc3 | gpu | 22.5 | 84.9 | 62.4 | set_weights on device (de-serialization Incr 1) |
| 2026-07-20 | 57de0ae03 | gpu | 16.4 | 48.8 | 32.4 | baseline weights on device (de-serialization Incr 2) |
| 2026-07-21 | 6f9e0dcef | gpu | 16.3 | 48.7 | 32.4 | AC weights on device, interim D2H dropped (Incr 2b; flat, a cleanup) |
| 2026-07-21 | 80f6e291a | gpu | 12.7 | 35.6 | 22.9 | fringe-rotation interpolator hoisted out of the per-sample loop (~29%) |
| 2026-07-21 | 7b8e31104 | gpu | 12.8 | 35.6 | 22.8 | host-tail overlap (intra-subint half-split); flat on the 2070 |
| 2026-07-21 | 7b8e31104 | gpu DIFX_GPU_PIPELINE=0 | 12.7 | 35.3 | 22.6 | synchronous, no overlap; ≈ pipeline=1 ⇒ no idle to hide on the 2070 (GPU-compute-bound). The overlap targets the ~48% idle measured on the A100, so the win is expected on the cluster re-profile, not here. |
| 2026-07-22 | (unpack-drain fix) | gpu DIFX_GPU_PIPELINE=1 | 11.8 | 35.5 | 23.7 | RING-deep host staging + drop per-station `unpack_all` drain; `cudaStreamSynchronize` 4021→31 (13.1 s→0.002 s). Flat on the 2070 (compute-bound + oversubscribed ⇒ no idle to convert). A100 wall win TBC on the cluster. |
| 2026-07-22 | (unpack-drain fix) | gpu DIFX_GPU_PIPELINE=0 | 12.6 | 35.5 | 22.9 | same build, no overlap; ≈ pipeline=1 on the 2070. |
| 2026-07-22 | f3046d081 | gpu, single-thread VDIF | 9.1 | 23.1 | **14.0** | benchmark now single-thread by default (no DataStream corner-turn); ~40% faster than the interlaced row below - the corner-turn cost the oversubscribed desktop too, not just the A100 |
| 2026-07-22 | f3046d081 | gpu, INTERLACEDVDIF (SINGLE_THREAD_VDIF=0) | 12.2 | 35.4 | 23.2 | interlaced comparison at the same commit |
| 2026-07-23 | (fused unpack+fringe) | gpu, single-thread VDIF | 8.4 | 20.7 | **12.3** | fused unpack into fringe rotation (gpu-plan.md item 1): decode straight from packed data, no unpacked-buffer round-trip; removes the largest kernel. 14.0→12.3 (~12%) on the 2070; larger A100 gain expected (unpack was a bigger fraction there). Both T1 and T5 down. |
| 2026-07-23 | (fused sum_weights) | gpu, single-thread VDIF | 8.5 | 20.3 | **11.8** | fold the total-weight reduction into gpu_set_weights (per-window atomicAdd), delete the `<<<1,1>>>` gpu_sum_weights. 12.3→11.8 (~4%) on the 2070; on the A100 it should be ~its 7.7% GPU-busy share. |
| 2026-08-26 | (conjugate array removed) | gpu, single-thread VDIF | 7.0 | 17.8 | **10.8** | delete `conj_fftd_gpu` and conjugate in the multiply instead (gpu-changes.md §15). 11.8→10.8 (~8.5%); `gpu_resultsrotatorMultiply` 536.3→396.9 us/call (−26%) on the 2070, and 82 MB/mode of VRAM freed. Larger A100 gain expected — that kernel is 42-44% of its kernel busy. |
| 2026-08-27 | (tiled fused fringe) | gpu, single-thread VDIF | 6.5 | 16.6 | **10.1** | shared-memory transpose in the fused decode+fringe kernel so its band-major write coalesces (gpu-changes.md §16, design in gpu-fringetile-design.md). Same-session A/B against the same binary with `DIFX_GPU_FRINGE_TILE=0`: **10.9 -> 10.1 s (7.3%)**; the kernel itself 948 -> 703 us/call (1.35x) and it is 46.5% of 2070 kernel time. Shape sweep (`fringetile-sweep.sh`, 13 shapes from 1x4096 to 128x128 to 16x4096): **1.30-1.54x, no shape slower**, output bit-identical to the untiled path everywhere. A100 A/B still to run. |
| 2026-08-28 | (autocorr incr 3, Mode path removed) | gpu DIFX_GPU_XMAC_AUTOCORR=1 | 6.7 | 17.5 | 10.8 | Mode-side autocorrelations gone when the gate is on (rotation kernel templated on DOAUTOCORR: no atomics, no cross-pol pass, no buffer, no drain, no host mirror). **Still +7%: on the 10-station job the XMAC grows 1826.9 -> 2334.8 us/call (+27.8%, +508 ms) while the rotator gives back only 12 ms (767.5 -> 755.7).** Kernel busy 2261.8 -> 2458.4 ms. The plan's estimate came from pre-§15 probes; §15 had already removed what made the rotator's autocorrelation work expensive, so it is now nearly free there. Gate stays off; the A100 is the deciding measurement. |
| 2026-08-28 | (autocorr incr 2, corrected plan) | gpu DIFX_GPU_XMAC_AUTOCORR=1 | 6.8 | 17.6 | 10.8 | autocorrelations as real baselines - 45 -> 55 per frequency, each carrying the frequency's pol products. `gpu_fuse_xmac_and_average` 34.0 -> 52.9 us/call, kernel busy 1360 -> 1420 ms (+4.4%) on the 2-station benchprof2, which is the WORST case (autocorrelations are 2/3 of its products; 22% on a 10-station job). Still +7% of wall because the rotator is also still computing them - increment 3 removes that. |
| 2026-08-28 | (autocorr incr 2, first plan - superseded) | gpu DIFX_GPU_XMAC_AUTOCORR=1 | 6.9 | 18.0 | 11.1 | autocorrelations computed in the XMAC as synthetic baselines - **10% SLOWER, and the reason the design is being revised** (gpu-autocorr-design.md). One synthetic baseline per output run re-reads the spectra: for benchprof2 the runs go 4 -> 68 and band-reads 8 -> 96, taking `gpu_fuse_xmac_and_average` 34.0 -> 104.5 us/call and kernel busy 1360 -> 1584 ms. The rotator only gives back 85 ms on this card. Correct (all scenarios PASS both gate states) but not landed on; gate stays off. |
| 2026-08-27 | (tiled fused fringe) | gpu DIFX_GPU_FRINGE_TILE=0 | 6.8 | 17.7 | 10.9 | the untiled path in the same binary and the same session - the A/B baseline for the row above, and it reproduces the 2026-08-26 10.8 s row, so there is no session drift in the comparison. |

## A100 kernel profile at `edad94b34` (landed 2026-08-28, job 15956983)

The profile that had been queued since 2026-08-26, finally through the queue.
`benchprof-profile-nsys-5s.sbatch`, gina6, `--cpus-per-task=5` (60/64 cores,
ledger-quality), 5 s window = 492 subints, single-thread VDIF. Capture and
analysis kept in `tests/benchprof-28082026/`. **This is the UNTILED build** -
the kernel is `gpu_fused_fringe`, not `_tiled` - so it is the A100 baseline for
the tiling work, not an A/B of it.

| quantity | value |
|---|---|
| kernel span | 5440 ms |
| kernel busy | 3924 ms (72.1% of span) |
| GPU busy (union with memcpy) | **4975 ms = 91.4%** |
| GPU idle | 466 ms = 8.6% |
| idle in gaps > 500 us | **0 ms (zero such gaps)** |
| idle in gaps < 20 us | 452 ms = 97% of idle |

| kernel | us/call | share | us/call before §15 |
|---|---|---|---|
| `gpu_resultsrotatorMultiply` | 277.9 | 34.9% | 396.5 (**−30%**) |
| `gpu_fuse_xmac_and_average` | 246.1 | 24.7% | 258 |
| `gpu_fused_fringe` | 191.9 | 24.1% | 192 (untouched) |
| `vector_fft` | 97.8 | 12.3% | 98 |

Two things follow.

**The conjugate-array deletion (§15) pays on the A100 as it did on the 2070:**
−30% on `gpu_resultsrotatorMultiply` there against −26% here, and every other
kernel is within a percent of its pre-§15 value, which is what makes the
attribution safe.

**The between-subints idle problem is finished.** The 2026-07-21 capture had
35.3% idle with 85.6% of it in gaps > 500 us - a host/data-delivery stall. This
one has 8.6% idle, **no gap over 500 us at all**, and 97% of what remains in
sub-20 us inter-kernel gaps. By the heuristic in `gpu-profiling.md` that is
launch-overhead bound, i.e. what is left for the host side is CUDA graphs, not
more overlap work.

**Still outstanding for the cost/benefit artifact:** this is a 5 s *profile*, not
a 20 s wall time, so the `gina4 (A100) 24.5 s` row below is still pre-§15 and
still conservative. Refreshing it needs one
`sbatch --cpus-per-task=5 benchprof-profile-nonsys-20s.sbatch` at the current
commit.

## A100 kernel-level sweep, 2026-08-28 (jobs 16004857 / 16004871) - RESOLVED

`fringetile-sweep.sh 2500 30` on an A100-SXM4-80GB (sm_80), one GPU, no MPI, so
neither node sharing nor srun overhead can reach it. This is the measurement the
wall-clock A/B below could not make.

| bands x chan | untiled us | tiled us | real | complex |
|---|---|---|---|---|
| 1 x 4096 | 147.5 | 124.9 | 1.18x | 1.18x |
| 1 x 16384 | 565.2 | 470.0 | 1.20x | 1.14x |
| 2 x 2048 | 150.5 | 138.2 | 1.09x | 1.02x |
| 3 x 1024 | 139.3 | 91.1 | **1.53x** | 1.40x |
| 4 x 512 | 78.8 | 73.7 | 1.07x | 1.02x |
| 6 x 512 | 147.5 | 102.4 | 1.44x | 1.32x |
| 8 x 256 | 85.0 | 74.8 | 1.14x | 1.10x |
| **16 x 256** (the benchmark shape) | 212.0 | 143.4 | **1.48x** | 1.38x |
| 16 x 128 | 107.5 | 69.6 | 1.54x | 1.42x |
| 32 x 256 | 616.4 | 287.7 | **2.14x** | 1.95x |
| 64 x 128 | 620.5 | 283.6 | **2.19x** | 1.92x |
| 128 x 128 | 1281.0 | 588.8 | **2.18x** | 1.92x |
| 16 x 4096 | 3413.0 | 2367.5 | 1.44x | 1.38x |
| 1 x 64 | 9.2 | 9.2 | 1.00x | 1.00x |
| 16 x 64 | 47.1 | 35.8 | 1.32x | 1.27x |

**No shape slower, and `dest` bit-identical at all 15 shapes in both sampling
modes** - which also re-verifies the kernel on a second architecture.

**The A100 gains more than the 2070, and the gain grows with band count.**
1.07-1.20x at 1-8 bands, 1.48x at the benchmark's 16, and **2.1-2.2x at 32-128
bands** (against 1.2-1.4x for the same shapes on the 2070). That is consistent
with the mechanism: the untiled lane mapping is `band + nbands*channel`, so a
warp's 32 lanes cover up to 32 *different* bands and the write scatters further
the more bands there are - and the A100 executes everything else fast enough
that the wasted sectors dominate more visibly. At 32 vs 64 bands the untiled
times are equal (616 vs 620 us for the same 8192 elements/window), so this is a
property of the access pattern, not of the work.

**This resolves the inconclusive wall A/B below.** At the benchmark shape the
kernel is 1.48x faster, and it is 24.1% of A100 kernel busy, so the expected
wall effect is 0.241 x (1 - 1/1.48) = 7.8% of kernel busy ~ **5-6% of span** -
which matches the ~5% the steady-state visibility dumps showed and contradicts
the -3% the biased wall A/B reported. Two independent measurements agree; the
wall A/B was the outlier, for the reasons in the next section.

**Decision: `DIFX_GPU_FRINGE_TILE` stays on by default on both architectures.**
The second acceptance condition (a win on more than one GPU) is met.

Why the wall-clock A/B (job 16004634) disagreed, and the rule that came out of
it, are in [`docs/gpu-profiling.md`](../../docs/gpu-profiling.md) under
"Measurement pitfall" - the short version is that its legs were ordered 1,0,1,0
so the tiled path paid the first-of-pair cost every time, on an 18%-owned node.

## CPU baselines (added 2026-08-27, for the cost/benefit costing)

These exist so the GPU numbers can be compared against something, and because
the ar313 CPU leg is easy to misread. **On ar313 the CPU path is core-starved,
not compute-limited**: the correlator competes with ten DataStream ranks for four
physical cores, and halving the cores available cost only 1.41x rather than 2x.
So the desktop CPU row says what that machine does and must NOT be scaled up to
describe a CPU server - which is what the cluster rows are for.

| machine | config | metric | result |
|---|---|---|---|
| ar313 (i7-6700, 4 cores) | CPU only, 2 physical cores | T5-T1 | 134.5 s |
| ar313 | CPU only, 4 physical cores | T5-T1 | **95.5 s** |
| ar313 | + RTX 2070 SUPER | T5-T1 | **10.8 s** (8.8x) |
| dave13 (2 x EPYC 7543, 64 cores) | CPU only, 10 threads | wall, 20 s window | 96.6 s |
| dave13 | CPU only, 20 threads | wall, 20 s window | 51.7 s (1.87x, 93% eff) |
| dave37 | CPU only, 40 threads | wall, 20 s window | **32.4 s** (1.60x, 80% eff) |
| gina4 (A100) | 12 cores + one A100 | wall, 20 s window | **24.5 s** |

Run with `benchprof-cpu-20s.sbatch` (`--export=ALL,NTHREAD=n`) and, for the GPU
row, `benchprof-profile-nonsys-20s.sbatch`. Note the two different metrics: the
desktop rows are T5-T1 (start-up differenced away), the cluster rows are raw wall
for a 20 s window (start-up included, so their steady-state rates are better than
shown). To compare across the two, ar313's fitted line gives 481 s (CPU) and
58.3 s (GPU) for a 20 s window.

**The 20->40 thread efficiency drop from 93% to 80% is a configuration artefact,
not a scaling limit:** each processor has 32 cores, so one Core process with 40
threads spans both sockets. Two processes of 20 threads each, bound one per
socket, would do better - so the 40-thread row is a worst case for a big CPU box.

**Derived equivalences** (interpolating the cluster CPU curve): the used
RTX 2070 SUPER is worth ~18 EPYC 7543 cores; one A100 is worth 50-60, i.e. most
of a dual-socket node, while occupying 12 cores rather than 40. The full
cost/benefit write-up, including August 2026 hardware prices and a VGOS-rate
sizing exercise, is the artifact recorded in `docs/gpu-payback.pdf`.

## Archived numbers

Superseded measurements - the 2026-07 A100 cluster profiling series and the
pre-protocol whole-run reference points - are in
[`BENCHMARKS-archive.md`](BENCHMARKS-archive.md). They are the baselines the
numbers above are quoted against.
