#!/usr/bin/env bash
# ncu-frac.sh - Nsight Compute profile of the big GPU kernels on the 2-station
# benchprof2 job (same kernel dimensions as the 10-station benchprof: windows x
# 16 bands x 128 channels). Companion to nsys-frac.sh, which answers "how long
# does each kernel take"; this one answers "what is each kernel bound BY".
#
# Usable only since the 2026-08-27 reboot enabled unrestricted GPU performance
# counters (NVreg_RestrictProfilingToAdminUsers=0); before that ncu died with
# ERR_NVGPUCTRPERM - see ncu-frac-baseline.log for that failure.
#
# Two modes, because they cost very different amounts of replay:
#   metrics  (default) - a fixed metric list over the three big kernels; one
#                        pass group, cheap, gives DRAM/SM throughput, atomic
#                        sector counts, occupancy and launch dims.
#   detailed           - ncu's --set detailed on a few launches of one kernel,
#                        for the rule-based bottleneck verdict and tables.
#
# Only the Core rank issues kernels, so --target-processes all over mpirun (the
# desktop recipe) profiles what we want; a per-rank wrapper does not survive
# FakeData teardown.
#
# Usage: ./ncu-frac.sh <tag> [metrics|detailed|full] [kernel-regex]
set -u
TAG="${1:?usage: ncu-frac.sh <tag> [metrics|detailed|full] [kernel-regex]}"
MODE="${2:-metrics}"
KREGEX="${3:-gpu_resultsrotatorMultiply|gpu_fuse_xmac_and_average|gpu_fused_fringe}"
cd "$(dirname "$0")"
set +u; source ../../setup.bash >/dev/null 2>&1; set -u

NCU="${CUDAROOT:-/usr/local/cuda-13.2}/bin/ncu"
[ -x "$NCU" ] || { echo "no ncu at $NCU"; exit 1; }

# Skip the first launches: the first subints pay one-off allocation/JIT costs
# and are not representative of the steady state.
COMMON=(--target-processes all -k "regex:$KREGEX" --launch-skip 60 --force-overwrite
        -o "ncu-$TAG" --print-summary per-kernel)

case "$MODE" in
  metrics)
      ARGS=("${COMMON[@]}" --launch-count 30 --metrics
        gpu__time_duration.sum,\
sm__throughput.avg.pct_of_peak_sustained_elapsed,\
gpu__dram_throughput.avg.pct_of_peak_sustained_elapsed,\
dram__bytes_read.sum,dram__bytes_write.sum,\
l1tex__t_sectors_pipe_lsu_mem_global_op_atom.sum,\
l1tex__t_sectors_pipe_lsu_mem_global_op_red.sum,\
lts__t_sectors_op_read.sum,lts__t_sectors_op_write.sum,\
sm__warps_active.avg.pct_of_peak_sustained_active,\
launch__grid_size,launch__block_size,\
smsp__sass_thread_inst_executed_op_dfma_pred_on.sum,\
smsp__sass_thread_inst_executed_op_ffma_pred_on.sum )
      ;;
  detailed)
      ARGS=("${COMMON[@]}" --launch-count 3 --set detailed)
      ;;
  full)
      # ~20 replay passes per launch: keep the count tiny. Adds Scheduler and
      # Warp State Statistics, i.e. WHY a latency-bound kernel is stalling.
      ARGS=("${COMMON[@]}" --launch-count 2 --set full)
      ;;
  *)  echo "unknown mode '$MODE' (metrics|detailed|full)"; exit 1 ;;
esac

rm -rf benchprof2_1.difx
echo "[ncu-frac] $TAG mode=$MODE kernels=$KREGEX"
DIFX_GPU_PIPELINE=0 "$NCU" "${ARGS[@]}" \
    mpirun --oversubscribe --mca mpi_yield_when_idle 1 \
        -machinefile machines -np 4 \
        mpifxcorr benchprof2_1.input --nocommandthread --usegpu \
    > "ncu-$TAG.log" 2>&1
rc=$?
echo "[ncu-frac] ncu exit $rc; report ncu-$TAG.ncu-rep, log ncu-$TAG.log"
grep -q ERR_NVGPUCTRPERM "ncu-$TAG.log" && echo "[ncu-frac] COUNTERS BLOCKED - reboot/permission problem"
# The per-kernel summary is at the end of the log; show it.
sed -n '/Summary/,$p' "ncu-$TAG.log" | head -60
