# FakeData: 10-station GPU benchmark

`benchsimreal.v2d` + `bd247c3.vex.obs` define a 10-station VLBA benchmark
(16 x 32 MHz, 2 Gbps/station) using `source = fake` datastreams - no
baseband data files needed. This is the performance-timing counterpart to
the correctness tests in `tests/Synthetic`.

The v2d is desktop-adapted (RTX 2070, 8 GB): see the comments in it for
the antenna-window, threads-file, VDIF-format and subintNS choices.

## Run

> A standalone runbook covering both this directory and `tests/Synthetic`, local
> and on OzSTAR — including the runtime gates and the measurement pitfalls — is
> `docs/difx-gpu-benchmarking.pdf`.


```sh
source ../../setup.bash
vex2difx benchsimreal.v2d && difxcalc benchsimreal_1.calc
grep 'ACTIVE DATASTREAMS' benchsimreal_1.input   # must be 10!
difxlog benchsimreal_1 difxlog.txt 5 &           # difxmessage eats stdout INFO
/usr/bin/time -v mpirun --oversubscribe --mca mpi_yield_when_idle 1 \
    -machinefile machines -np 12 \
    mpifxcorr benchsimreal_1.input --nocommandthread --usegpu
```

Notes:
- 12 ranks = 1 FxManager + 10 datastreams + 1 (GPU)Core; on a small desktop
  this oversubscribes the cores, hence the yield option.
- mpirun exits nonzero even on success (fake datastreams exit without
  MPI_Finalize at teardown). Judge success by a non-empty
  `benchsimreal_1.difx/DIFX_*` file, not the exit code.
- Wall-time scaling on an RTX 2070: 15 s of data (the default window)
  takes ~260 s.
- **Benchmarking only - do not use for correctness testing.** The fake
  datastream writes valid VDIF headers over untouched (uninitialised)
  payload bytes, so every antenna delivers essentially the same
  pathological stream: correlation sums grow coherently (~N, not sqrt N),
  amplifying FP rounding differences, and the XMAC's atomic adds make the
  output non-bit-reproducible run to run (~1e-6 relative variation).
  Correctness lives in tests/Synthetic.

## Profiling (Nsight Systems)

Wrap mpirun itself - do NOT wrap individual ranks (the teardown above gets
per-rank nsys killed mid-report-write). Set `subintNS = 10000000` in the
v2d first: CUPTI needs VRAM on top of the job's own budget.

```sh
nsys profile --trace=cuda,nvtx,osrt -o bench --force-overwrite true \
    mpirun --oversubscribe --mca mpi_yield_when_idle 1 \
    -machinefile machines -np 12 \
    mpifxcorr benchsimreal_1.input --nocommandthread --usegpu
nsys stats --force-export=true --report nvtx_pushpop_sum \
    --report cuda_gpu_kern_sum --report cuda_gpu_mem_time_sum bench.nsys-rep
```

Only the GPU core rank emits CUDA/NVTX activity, so tracing all ranks is
cheap. If a profiling run is interrupted, orphaned `nsys --start-agent`
daemons may be left spinning at 100% CPU - find them with
`pgrep -af nsys` and kill them.

## Profiling (Nsight Compute) and the fringe-tile shape sweep

`ncu` works on ar313 since the 2026-08-27 reboot enabled unrestricted GPU
performance counters. Two harnesses live here:

```sh
./ncu-frac.sh <tag> [metrics|detailed|full] [kernel-regex]
```

Nsight Compute over the 2-station `benchprof2` job (same kernel dimensions as
the 10-station benchmark), `--target-processes all` over mpirun, warm-up skipped.
`metrics` is a cheap fixed metric list; `detailed` adds ncu's rule verdicts;
`full` adds Scheduler/Warp State, i.e. *why* a latency-bound kernel stalls -
keep `full` to 1-2 launches, it replays ~20 passes each. Results land in
`ncu-<tag>.ncu-rep` / `ncu-<tag>.log`.

```sh
./fringetile-sweep.sh 2500 30                    # real sampling
SWEEP_COMPLEX=1 ./fringetile-sweep.sh 2500 30    # complex twin
SWEEP_PCAL=1 ./fringetile-sweep.sh 2500 20       # + the DOPCAL path

# on OzSTAR (needs a GPU, so under srun):
srun --gres=gpu:1 --time=15 --mem=8000 --account=oz168 ./fringetile-sweep.sh 2500 30
```

Machine-specific bits are env-overridable, and the defaults do the right thing on
both machines: `SWEEP_SETUP` (the DiFX setup to source - `../../setup.bash` here,
`$HOME/setup_gpudifx.claude` where `sbatch` exists), `SWEEP_NVCC` (default:
whatever the setup puts on `PATH`; on OzSTAR that may need `module load cuda`),
`SWEEP_OUTDIR` (default: this directory here, a scratch dir under `/fred` on the
cluster, so a shared checkout is never written to) and `SWEEP_ARCH`. The `-arch`
is taken from the **GPU it is about to run on** rather than from `NVCCFLAGS`: a
cluster setup carrying the desktop's `-arch=sm_75` would otherwise build a cubin
the A100 cannot launch.

Sweeps the fused decode+fringe kernel over 13 (bands, channels) shapes, tiled
(`DIFX_GPU_FRINGE_TILE=1`) versus untiled, and prints per-shape time plus an
output hash. The two paths do identical arithmetic in identical order, so the
hashes must match bit-exactly - that is the all-shapes correctness check, and it
covers odd band counts and partial tiles that no DiFX scenario exercises. It
`#include`s `mpifxcorr/src/gpudecode.cu`, so it measures the shipped kernel.

**Pass `2500 30`, not the defaults.** At `numBufferedFFTs` = 10 the launch is
latency-dominated and every shape looks like 1.0x; below ~30 reps the largest
shape swings up to 18% on warm-up. Design and results:
`docs/gpu-fringetile-design.md`.
