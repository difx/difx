#!/bin/bash
#
#SBATCH --job-name=difx_benchprof_cpu
#SBATCH --output=benchprof-cpu.%j.mpilog
#SBATCH --nodes=1
#SBATCH --ntasks=12
#SBATCH --cpus-per-task=5
#SBATCH --mem-per-cpu=4000
#SBATCH --time=60:00
#SBATCH --account=oz168
## add a partition if your site needs one:  #SBATCH --partition=<partition>
#
# CPU-ONLY companion to benchprof-profile-nonsys-20s.sbatch: the SAME 10-antenna
# fake-data job, correlated without --usegpu, on a MODERN many-core node.
#
# WHY THIS EXISTS. The CPU-vs-GPU comparison on the ar313 desktop is confounded
# and flatters the GPU: that machine has four 2015-era cores, and in CPU mode
# the correlator threads contend with the 10 DataStream ranks for them. Measured
# there, going from 2 to 4 physical cores bought only 1.41x rather than 2x, i.e.
# the CPU leg is contention-limited rather than compute-limited. So the desktop
# number says what adding a GPU does to THAT machine - a fair question - but it
# cannot be extrapolated to a CPU server. This job supplies the number that can:
# the same correlation, on the same node type the A100 numbers come from, using
# CPUs that are a decade newer than the desktop's.
#
# NOTE no --gres: this asks for no GPU at all, so it should also queue faster.
#
# Deliberately 60 cores (12 x 5) so the node is effectively ours - a co-tenant
# job perturbs a wall-time measurement by ~10% (gpu-changes.md sec 14) and wall
# time is the entire output here. --cpu-bind=none lets the Core rank's threads
# use the whole allocation rather than being pinned into one task's 5 cores;
# with one Core rank and 10 DataStreams, pinning would starve exactly the part
# being measured.
#
# CORE SCALING. NTHREAD sets how many processing threads the single Core rank
# uses, which is the knob that says "how much CPU is the correlator actually
# getting". Sweep it to get a scaling curve rather than a single point - two
# runs are enough to show whether the CPU path is still scaling or has flattened:
#
#     sbatch --export=ALL,NTHREAD=32 benchprof-cpu-20s.sbatch
#     sbatch --export=ALL,NTHREAD=48 benchprof-cpu-20s.sbatch
#
# The allocation below (60 cores) is deliberately larger than the largest sweep
# point so the threads are never the constraint the allocation imposes, and
# --cpu-bind=none lets them spread over it. If 48 threads gives close to 1.5x
# the throughput of 32, the CPU path is still scaling and a bigger node would
# help; if it gives much less, it has hit its own limit and buying more cores
# will not fix it - which is exactly the question a CPU-vs-GPU costing needs
# answered.
#
# Submit from tests/FakeData of the cluster checkout:
#     cd <checkout>/tests/FakeData && sbatch --export=ALL,NTHREAD=32 benchprof-cpu-20s.sbatch
#
# Compare its "wall ...s for a ~20 s data window" against the GPU run's.

set -e

SETUP=/home/adeller/setup_gpudifx.claude
SRCDIR="${SLURM_SUBMIT_DIR:-$PWD}"
NTHREAD="${NTHREAD:-32}"        # processing threads PER Core rank; sweep this (see header)
NCORE="${NCORE:-1}"             # Core ranks; NCORE=2 needs --ntasks=13 (manager + 10 datastreams + 2 cores)
# One workdir per configuration, so sweep points can sit in the queue together.
WORKDIR=/fred/oz168/adeller/claude/testing/benchprof-cpu-${NCORE}x${NTHREAD}
want=$((11 + NCORE))
[ "${SLURM_NTASKS:-$want}" = "$want" ] || { echo "ERROR: NCORE=$NCORE needs --ntasks=$want, got $SLURM_NTASKS" >&2; exit 1; }

. "$SETUP"
export DIFX_MESSAGE_GROUP=$(hostname -i)
export DIFX_BINARY_GROUP=$(hostname -i)

rm -rf "$WORKDIR"; mkdir -p "$WORKDIR"
cp "$SRCDIR/benchprof.v2d" "$SRCDIR/bd247c3.vex.obs" "$WORKDIR/"
cd "$WORKDIR"

# Same ~20 s window as the GPU soak run, so the two wall times compare directly.
sed -i 's|^mjdStop .*|mjdStop  = 59675.09629650|' benchprof.v2d
# Give the single Core rank enough threads to use the node. The GPU run leaves
# this at 10 because the GPU path forces one thread regardless.
sed -i "s|^nThread = .*|nThread = ${NTHREAD}|" benchprof.v2d
sed -i "s|^nCore = .*|nCore = ${NCORE}|" benchprof.v2d

vex2difx benchprof.v2d
difxcalc benchprof_1.calc
nds=$(awk '/^ACTIVE DATASTREAMS/{print $3}' benchprof_1.input)
[ "$nds" = 10 ] || { echo "ERROR: expected 10 datastreams, got '$nds'" >&2; exit 1; }
# Single-thread VDIF, exactly as the GPU benchmarks do, so the DataStream
# corner-turn is out of BOTH sides of the comparison.
sed -i -E 's|(DATA FORMAT:[[:space:]]+)INTERLACEDVDIF/[0-9:]+|\1VDIF|' benchprof_1.input

echo "=== $(hostname): $(nproc --all) cores, $((${SLURM_CPUS_PER_TASK:-1} * ${SLURM_NTASKS:-1})) reserved, nCore=$NCORE x nThread=$NTHREAD ==="
lscpu | grep -E "^Model name|^CPU\(s\)|^Core\(s\)|^Socket" || true
difxlog benchprof_1 "$WORKDIR/benchprof_1.difxlog" 4 &
difxlogpid=$!

t0=$(date +%s.%N)
srun --ntasks="$SLURM_NTASKS" --cpu-bind=none \
     mpifxcorr benchprof_1.input --nocommandthread
t1=$(date +%s.%N)
kill "$difxlogpid" 2>/dev/null || true

echo "=== CPU-only wall $(echo "$t0 $t1" | awk '{printf "%.1f", $2-$1}')s for a ~20 s data window ==="
# Steady-state rate, start-up excluded: the manager logs one "STATUS Running" per
# 1 s integration, so (last - first) / (count - 1) is wall seconds per data second.
grep 'STATUS Running' "$WORKDIR/benchprof_1.difxlog" \
  | while read -r _ mon day hms yr _; do date -d "$mon $day $hms $yr" +%s; done \
  | awk 'NR==1{f=$1} {l=$1; n=NR} END{if (n>2) printf "=== steady state: %.2f s wall per 1 s of data = %.3fx real time (%d integrations) ===\n", (l-f)/(n-1), (n-1)/(l-f), n}'
out=$(ls -S "$WORKDIR"/benchprof_1.difx/DIFX_* 2>/dev/null | head -1)
[ -n "$out" ] && [ -s "$out" ] && echo "output OK: $(stat -c %s "$out") bytes" || { echo "ERROR: no output" >&2; exit 1; }
