#!/bin/bash
#
# run-bench.sh - timed GPU benchmark using the T(5s) - T(1s) metric.
#
# Runs the benchsimreal scenario twice per repeat: once with 1 s of data
# and once with 5 s, both generated from benchsimreal.v2d by rewriting
# mjdStop. The reported metric is the wall-time difference, which cancels
# the (large, constant) start-up and shutdown costs and measures the
# steady-state correlation rate. See BENCHMARKS.md for the results ledger.
#
# Usage:  ./run-bench.sh [repeats]        (default 3)
# Env:    USEGPU=0             benchmark the CPU path instead
#         SINGLE_THREAD_VDIF=0 keep interlaced VDIF (default: rewrite the fake
#                              data to single-thread VDIF, bypassing the
#                              DataStream corner-turn; needs mark5access genheaders)
#         EXTRA_ENV=...        e.g. EXTRA_ENV="DIFX_GPU_PIN_INPUT=0"

set -u
cd "$(dirname "$0")"

REPEATS=${1:-3}
: "${USEGPU:=1}"
: "${EXTRA_ENV:=}"

# 1s and 5s windows from the same mjdStart (see benchsimreal.v2d comments);
# stops are padded ~0.3ms high so 8-decimal MJD rounding cannot shorten the
# window below its nominal length (vex2difx skips jobs under minLength)
STOP1=59675.09607640
STOP5=59675.09612270

usegpuflag=""
[ "$USEGPU" = "1" ] && usegpuflag="--usegpu"

# shellcheck disable=SC1091
set +u; . ../../setup.bash > /dev/null 2>&1; set -u

gen() { # gen <name> <mjdstop>
    sed -e "s/^mjdStop .*/mjdStop  = $2/" benchsimreal.v2d > "$1.v2d"
    vex2difx "$1.v2d" > /dev/null
    # Benchmark the GPU without the DataStream interlaced-VDIF corner-turn:
    # rewrite the fake data to single-thread VDIF (needs mark5access genheaders).
    # Set SINGLE_THREAD_VDIF=0 to keep the interlaced demux for comparison.
    if [ "${SINGLE_THREAD_VDIF:-1}" != 0 ]; then
        sed -i -E 's|(DATA FORMAT:[[:space:]]+)INTERLACEDVDIF/[0-9:]+|\1VDIF|' "${1}_1.input"
    fi
    difxcalc "${1}_1.calc" > /dev/null
    local nds
    nds=$(awk '/^ACTIVE DATASTREAMS/{print $3}' "${1}_1.input")
    if [ "$nds" != 10 ]; then
        echo "ERROR: ${1}_1.input has $nds active datastreams (want 10)" >&2
        exit 1
    fi
}

run() { # run <name> -> prints wall seconds
    rm -rf "${1}_1.difx"
    local t0 t1
    t0=$(date +%s.%N)
    env $EXTRA_ENV mpirun --oversubscribe --mca mpi_yield_when_idle 1 \
        -machinefile machines -np 12 \
        mpifxcorr "${1}_1.input" --nocommandthread $usegpuflag \
        > /dev/null 2>&1
    t1=$(date +%s.%N)
    if [ ! -s "${1}_1.difx"/DIFX_* ]; then
        echo "ERROR: ${1} produced no visibilities" >&2
        exit 1
    fi
    echo "$t0 $t1" | awk '{printf "%.1f", $2-$1}'
}

gen bench1s "$STOP1"
gen bench5s "$STOP5"

echo "# repeats=$REPEATS usegpu=$USEGPU extra_env='$EXTRA_ENV'"
echo "# repeat  T1s  T5s  delta(=T5-T1)"
best1=""; best5=""
for r in $(seq "$REPEATS"); do
    t1=$(run bench1s)
    t5=$(run bench5s)
    d=$(echo "$t1 $t5" | awk '{printf "%.1f", $2-$1}')
    echo "  $r  ${t1}s  ${t5}s  ${d}s"
    best1=$(echo "${best1:-$t1} $t1" | awk '{print ($2<$1)?$2:$1}')
    best5=$(echo "${best5:-$t5} $t5" | awk '{print ($2<$1)?$2:$1}')
done
bestd=$(echo "$best1 $best5" | awk '{printf "%.1f", $2-$1}')
commit=$(git rev-parse --short HEAD 2>/dev/null || echo unknown)
echo
echo "best-of-$REPEATS metric (T5-T1): ${bestd}s   [T1=${best1}s T5=${best5}s]"
echo "ledger row: | $(date +%F) | $commit | $([ "$USEGPU" = 1 ] && echo gpu || echo cpu)${EXTRA_ENV:+ $EXTRA_ENV} | ${best1} | ${best5} | ${bestd} |  |"
