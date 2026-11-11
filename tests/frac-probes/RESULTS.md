# A100 probe results: what limits `gpu_resultsrotatorMultiply` (2026-08-26)

Run on **gina13**, single node (all four probes show ~24,900 `process_vm_readv`
calls in the Core rank = intra-node CMA, so the ranks were co-located; the
earlier two-node spill was cancelled and resubmitted). Branch
`probe-frac-limits`, `DIFX_GPU_FRAC_PROBE=0..3`, all four in one job.

| probe | us/call | delta | 2070 delta (for contrast) |
|---|---|---|---|
| 0 baseline | **396.5** | - | - |
| 1 `atomicAdd` -> plain store | 299.9 | **-24.4%** | +0.0% |
| 2 cross-pol phase off | 248.6 | **-37.3%** | -27% |
| 3 bands on `gridDim.z` | 909.7 | **+129%** | +139% |

Every other kernel is stable to <1% across all four captures
(`gpu_fuse_xmac_and_average` 258/259/259/258 us, `gpu_fused_fringe` 192 us in
all four, `vector_fft` 98 us in all four), so only the target kernel moved.

## The atomics DO matter on the A100 - the 2070 hid them completely

24.4% of the kernel, ~97 us/launch, ~484 ms over the 5 s window = **10.7% of
A100 kernel busy**. On the 2070 the identical probe measured 0.0%.

The two are consistent, not contradictory: on the 2070 the atomic latency sits
entirely behind another saturated unit, so removing it changes nothing. The A100
has 4.5x the bandwidth, runs the same traffic at 26% of peak, and the atomic
serialisation is therefore exposed.

> **Correction, 2026-08-27 (ncu):** this section originally explained the 2070
> result as "the 2070 is bandwidth-saturated (~85% of its 448 GB/s)". That is
> wrong. `ncu` measures the kernel at **46% of DRAM peak** on the 2070; the
> 83-85% figure is its *compute* (SM) throughput, which ncu attributes to the
> **FP64 pipe**. So the atomics are hidden behind FP64 issue, not behind memory
> stalls (Mem Pipes Busy is 17%). Every probe *result* in this file stands as
> measured - only the mechanism was misread. See `docs/gpu-profiling.md`,
> "ncu on the 2070".

**Lesson for this project: a "what is this kernel bound by?" answer does not
transfer between these two cards.** The 2070 probes were not wrong, they were
answering a different question. Anything that matters for the cluster has to be
measured on the cluster.

## Decomposition of the 396 us

- **atomics ~97 us (24%)** - probe 1 directly.
- **cross-pol traffic ~100 us (25%)** - probe 2 removes 148 us, of which ~48 us
  is its half of the atomics, leaving ~100 us of DRAM traffic.
- **band phase traffic + compute ~200 us (50%)** - the remainder.

Traffic sensitivity is therefore ~100 us per 82 MB removed, which is what makes
the FP16 case on this card: halving `fftd`/`conj_fftd` also removes ~82 MB, so
~100 us/launch, ~500 ms, ~11% of kernel busy. Comparable to the atomics prize,
and largely additive with it.

## Probe 3 kills the mapping the approved plan specified

The plan's Step 2 was "thread owns (band, channel, window-chunk)", i.e. bands on
`gridDim.z`. That is exactly probe 3, and it is **2.3x worse on the A100 and
2.4x worse on the 2070**. Whatever the per-thread work, moving bands out of the
in-thread loop destroys the access pattern: today a block streams one window's
16 bands as 16 contiguous ~1 KB reads, whereas with bands on the grid the
concurrently-scheduled blocks are consecutive *windows* of one band, 32 KB
apart. Consistent across both cards, so it is a locality effect, not a
card quirk.

Any atomics fix must therefore leave the thread count, the per-thread work and
the block->window mapping ALONE, and reduce the atomics some other way.
