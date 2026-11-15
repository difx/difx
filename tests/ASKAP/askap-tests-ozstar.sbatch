#!/bin/bash
#
#SBATCH --job-name=difx_askap_tests
#SBATCH --output=askap-tests.%j.log
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48        # one core per MPI process of the GPU job (see below)
#SBATCH --gres=gpu:1
#SBATCH --mem=16G                 # peak 8.3 GB
#SBATCH --time=0:20:00            # 9-10.5 min: tests 3.5, benchmark 6-7
#SBATCH --account=oz168
## add a partition if your site needs one:  #SBATCH --partition=<partition>
#
# ASKAP CODIF correctness and speed tests on one OzSTAR GPU node.  For each job:
#   cpu run; gpu run and gpucompat run, each compared with the cpu run (COMPARE_WITH:
#   gpucompat must PASS, production gpu is INFO); then bench-askap.sh (CPU vs GPU,
#   T(long) - T(short) for 1934, whole job for the short 0407 dump).
#
# run-askap.sh starts mpifxcorr with mpirun on this node (1 manager + one DataStream
# per file, 46 or 50 + 1 Core), so the allocation is one task with many CPUs, not one
# SLURM task per MPI process.  If your MPI refuses to place them, use --ntasks=52
# --cpus-per-task=1 instead.  The CPU path's Core uses NTHREAD threads; the DataStream
# processes need the rest of the cores.  Sweep NTHREAD for a scaling curve:
#     sbatch --export=ALL,NTHREAD=16 askap-tests-ozstar.sbatch
# Ask for at least 48 CPUs: with 32 the GPU job's 48 processes share cores and its timing
# varied by 60% between runs (tests/FakeData/BENCHMARKS.md, "ASKAP CODIF on an A100").
#
# Before submitting: a gpudifx clone that has this script (tests/ASKAP holds run-askap.sh,
# bench-askap.sh and the jobs), a DiFX build of that commit (SETUP_SCRIPT), and the CODIF
# data (see README.md); set the paths below and submit from any directory (the SLURM log
# askap-tests.<jobid>.log is written there).  bench-askap.sh reads the data into the page
# cache before each run (WARM=1), so the numbers measure the correlator.  At the end the
# results, without visibilities, are packed into $OUT.tgz: send that and the SLURM log back.

TESTDIR="SET-ME/difx/tests/ASKAP"                        # tests/ASKAP of the gpudifx clone
SETUP_SCRIPT="SET-ME/setup.bash"                        # sets up the DiFX build under test
DATA_1934="SET-ME/askap1934-lsb-1bit/codif"              # 46 files
DATA_0407="SET-ME/askap0407-lsb-4bit/codif"              # 50 files
for d in "$TESTDIR" "$DATA_1934" "$DATA_0407"; do
    [ -d "$d" ] || { echo "ERROR: $d is not a directory: set the paths at the top of this script" >&2; exit 1; }
done
[ -f "$SETUP_SCRIPT" ] || { echo "ERROR: SETUP_SCRIPT $SETUP_SCRIPT not found" >&2; exit 1; }
OUT=$TESTDIR/local-runs/askap-tests-${SLURM_JOB_ID:-manual}
: "${NTHREAD:=24}"
REPEATS=3
: "${DOBENCH:=1}"

export SETUP_SCRIPT NTHREAD
mkdir -p "$OUT"
{ echo "Node $(hostname), job ${SLURM_JOB_ID:-manual}, NTHREAD=$NTHREAD, $(env -u OMP_NUM_THREADS -u OMP_THREAD_LIMIT nproc) CPUs in affinity mask, OMP_NUM_THREADS=${OMP_NUM_THREADS:-unset}"
  echo "gpudifx commit: $(git -C "$TESTDIR" log -1 --format='%h %s' 2>/dev/null)"
  nvidia-smi --query-gpu=name,driver_version,memory.total,compute_cap --format=csv,noheader
  lscpu | grep -E "Model name|^CPU\(s\)|Thread|Socket"
  free -g | sed -n 1,2p
  df -h "$DATA_1934" "$DATA_0407" | tail -2
  ( set +u; . "$SETUP_SCRIPT" > /dev/null; which mpifxcorr; mpirun --version 2>&1 | head -1 )
} | tee "$OUT/system.txt"

summary=()
one() {   # one <label> <job> <data> [env assignments...]
    local label="$1" job="$2" data="$3"; shift 3
    env "$@" DATA="$data" WORKDIR="$OUT/$label" "$TESTDIR/run-askap.sh" "$job" > "$OUT/$label.log" 2>&1
    local rc=$?
    local cmp; cmp=$(grep -oE "^compare  : [A-Z]+" "$OUT/$label.log" | awk '{print $3}')
    summary+=("$(printf '%-34s %s%s' "$label" "$([ $rc = 0 ] && echo PASS || echo FAIL)" "${cmp:+ (compare: $cmp)}")")
}
for jd in "askap1934-lsb-1bit:$DATA_1934:2026y006d04h04m28s" "askap0407-lsb-4bit:$DATA_0407:-"; do
    IFS=: read -r job data short <<< "$jd"
    one "$job-cpu" "$job" "$data" USEGPU=0
    one "$job-gpu" "$job" "$data" USEGPU=1 COMPARE_WITH="$OUT/$job-cpu"
    one "$job-gpucompat" "$job" "$data" USEGPU=1 DIFX_GPU_SIDEBAND_CPUCOMPAT=1 COMPARE_WITH="$OUT/$job-cpu"
done
if [ "$DOBENCH" = 1 ]; then
    ( cd "$TESTDIR" && DATA=$DATA_1934 ./bench-askap.sh askap1934-lsb-1bit 2026y006d04h04m28s "$REPEATS"
      DATA=$DATA_0407 ./bench-askap.sh askap0407-lsb-4bit - "$REPEATS" ) > "$OUT/bench.log" 2>&1
fi
{ echo "Results in $OUT"
  printf '%s\n' "${summary[@]}"
  [ "$DOBENCH" = 1 ] && grep -E "^Job|median|FAILED" "$OUT/bench.log"
} | tee "$OUT/summary.txt"
# Pack everything except visibilities and data links for sending back (bench-askap.sh
# writes its runs to $TESTDIR/local-runs/bench-<job>-<stamp>, listed in bench.log)
benchdirs=$(grep -oE "output /\S+" "$OUT/bench.log" 2>/dev/null | awk '{print $2}')
tar czf "$OUT.tgz" --exclude='*.difx' --exclude='*.codif' \
    -C "$TESTDIR/local-runs" "$(basename "$OUT")" $(for d in $benchdirs; do basename "$d"; done)
echo "Packed: $OUT.tgz ($(du -h "$OUT.tgz" | cut -f1))"
