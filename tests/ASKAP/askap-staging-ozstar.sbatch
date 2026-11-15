#!/bin/bash
#
#SBATCH --job-name=difx_askap_staging
#SBATCH --output=askap-staging.%j.log
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48        # one core per MPI process of the GPU job
#SBATCH --gres=gpu:1
#SBATCH --mem=16G
#SBATCH --tmp=8G                  # node-local disk for the staged copies (2 x 1.4 GB)
#SBATCH --time=0:30:00            # estimate for PATHS=gpu; allow about an hour for "cpu gpu"
#SBATCH --account=oz168
## add a partition if your site needs one:  #SBATCH --partition=<partition>
#
# Does staging the ASKAP CODIF data to node-local disk speed up correlation?  Runs
# bench-askap-staging.sh on both ASKAP jobs: the copy to local disk is timed, then
# mpifxcorr reads the data cold from shared storage (Lustre), cold from the local copy,
# and warm from the page cache.  See bench-askap-staging.sh for the legs and the metric.
#
# STAGEDIR is the job's node-local disk: $JOBFS on OzSTAR (requested with --tmp above;
# /var/tmp/jobfs, NVMe, on gina302), removed by SLURM when the job ends, so the copies need
# no cleaning up.  Set STAGEDIR yourself on a system that names it differently.
#
# Before submitting: set the paths below as in askap-tests-ozstar.sbatch.  At the end the
# results, without visibilities, are packed into $OUT.tgz: send that and the SLURM log back.
#     sbatch askap-staging-ozstar.sbatch                            # GPU path
#     sbatch --export=ALL,PATHS="cpu gpu" askap-staging-ozstar.sbatch

TESTDIR="SET-ME/difx/tests/ASKAP"                        # tests/ASKAP of the gpudifx clone
SETUP_SCRIPT="SET-ME/setup.bash"                        # sets up the DiFX build under test
DATA_1934="SET-ME/askap1934-lsb-1bit/codif"              # 46 files
DATA_0407="SET-ME/askap0407-lsb-4bit/codif"              # 50 files
: "${STAGEDIR:=${JOBFS:-}}"
for d in "$TESTDIR" "$DATA_1934" "$DATA_0407"; do
    [ -d "$d" ] || { echo "ERROR: $d is not a directory: set the paths at the top of this script" >&2; exit 1; }
done
[ -f "$SETUP_SCRIPT" ] || { echo "ERROR: SETUP_SCRIPT $SETUP_SCRIPT not found" >&2; exit 1; }
[ -n "$STAGEDIR" ] && [ -d "$STAGEDIR" ] || { echo "ERROR: no node-local disk: JOBFS is not set; set STAGEDIR" >&2; exit 1; }
OUT=$TESTDIR/local-runs/askap-staging-${SLURM_JOB_ID:-manual}
: "${NTHREAD:=24}"
: "${PATHS:=gpu}"
REPEATS=3

export SETUP_SCRIPT NTHREAD PATHS STAGEDIR
mkdir -p "$OUT"
{ echo "Node $(hostname), job ${SLURM_JOB_ID:-manual}, NTHREAD=$NTHREAD, PATHS=$PATHS, $(env -u OMP_NUM_THREADS -u OMP_THREAD_LIMIT nproc) CPUs in affinity mask"
  echo "gpudifx commit: $(git -C "$TESTDIR" log -1 --format='%h %s' 2>/dev/null)"
  nvidia-smi --query-gpu=name,driver_version,memory.total,compute_cap --format=csv,noheader
  lscpu | grep -E "Model name|^CPU\(s\)|Thread|Socket"
  free -g | sed -n 1,2p
  df -hT "$DATA_1934" "$DATA_0407" "$STAGEDIR" | tail -n +2
  lsblk -d -o NAME,ROTA,TYPE,SIZE,MODEL 2>/dev/null
  ( set +u; . "$SETUP_SCRIPT" > /dev/null; which mpifxcorr; mpirun --version 2>&1 | head -1 )
} | tee "$OUT/system.txt"

( cd "$TESTDIR" && DATA=$DATA_1934 ./bench-askap-staging.sh askap1934-lsb-1bit 2026y006d04h04m28s "$REPEATS"
  DATA=$DATA_0407 ./bench-askap-staging.sh askap0407-lsb-4bit - "$REPEATS" ) > "$OUT/bench-staging.log" 2>&1
{ echo "Results in $OUT"
  grep -E "^Job|^Data|^  (shared|local):|^Copy to local disk:|median|FAILED|ERROR" "$OUT/bench-staging.log"
} | tee "$OUT/summary.txt"
# Pack everything except visibilities and data for sending back (the runs are in
# $TESTDIR/local-runs/bench-staging-<job>-<stamp>, listed in bench-staging.log)
benchdirs=$(grep -oE "output /\S+" "$OUT/bench-staging.log" 2>/dev/null | awk '{print $2}')
tar czf "$OUT.tgz" --exclude='*.difx' --exclude='*.codif' \
    -C "$TESTDIR/local-runs" "$(basename "$OUT")" $(for d in $benchdirs; do basename "$d"; done)
echo "Packed: $OUT.tgz ($(du -h "$OUT.tgz" | cut -f1))"
