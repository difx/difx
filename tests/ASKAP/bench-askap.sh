#!/bin/bash
#
# bench-askap.sh - CPU vs GPU speed on an ASKAP CODIF job, without start-up costs.
#
# Usage:  DATA=<codif dir> ./bench-askap.sh <job> <short stop | -> [repeats]
#
# Runs run-askap.sh for the whole job ("long") and for the job cut at <short stop>
# (a v2d stop time, vex format or MJD), on the CPU and on the GPU, <repeats> times
# (default 3), alternating which length and path go first.  The metric is
# T(long) - T(short): mpifxcorr wall time for the data between the two stop times,
# with start-up and shut-down cancelled, as T5 - T1 in tests/FakeData.  Prints each
# run and the median per path.  The fringe check still runs but its thresholds are off
# (a short cut has too little data for them); each run must exit 0 with an error-free
# difxlog.
#
# The CODIF files are read into the page cache before every run (WARM=1, the default),
# so the numbers measure the correlator, not the disk: on a desktop with the data on a
# spinning disk a cold read of ~1.4 GB by ~50 datastreams at once takes longer than the
# GPU correlation itself.  WARM=0 measures cold runs instead.
#
# With "-" as the short stop only the whole job is timed (for data too short to cut:
# a cut must hold at least one integration to be checked); the result then includes
# start-up and shut-down.  Job directories go under local-runs/bench-<job>-<stamp>/.
#
# Env: NTHREAD (CPU correlator threads, default 8; the GPU uses 1), as in run-askap.sh.

set -u
cd "$(dirname "$0")"
JOB=${1:?job name}
SHORT=${2:?short stop time}
REPEATS=${3:-3}
: "${DATA:?set DATA}"
: "${WARM:=1}"
OUT="$(pwd)/local-runs/bench-$JOB-$(date +%Y%m%d-%H%M%S)"
mkdir -p "$OUT"

declare -A T
run() {   # run <path cpu|gpu> <length short|long> <repeat>
    local path="$1" len="$2" i="$3" usegpu=0 stop=""
    [ "$path" = gpu ] && usegpu=1
    [ "$len" = short ] && stop="$SHORT"
    local wd="$OUT/$path-$len-$i"
    [ "$WARM" = 1 ] && cat "$DATA"/*.codif > /dev/null
    # timing legs: no S/N or closure thresholds (the short cut has too little data for
    # them), but mpifxcorr must succeed and the difxlog must be free of errors
    USEGPU=$usegpu STOP="$stop" WORKDIR="$wd" MINSNR=0 MAXCLOSURE=360 ./run-askap.sh "$JOB" > "$wd.log" 2>&1
    local rc=$?
    local t
    t=$(grep -oE "^mpifxcorr: exit 0, [0-9.]+ s" "$wd.log" | awk '{print $4}')
    if [ "$rc" != 0 ] || [ -z "$t" ] || ! grep -q "^difxlog  : 0 ERROR" "$wd.log"; then
        echo "  $path $len #$i: FAILED (see $wd.log)"; exit 1
    fi
    T[$path-$len-$i]=$t
    echo "  $path $len #$i: $t s"
}

echo "Job $JOB, short stop $SHORT, $REPEATS repeats, WARM=$WARM; output $OUT"
for i in $(seq 1 "$REPEATS"); do
    if [ $((i % 2)) = 1 ]; then order="cpu gpu"; lens="long short"; else order="gpu cpu"; lens="short long"; fi
    [ "$SHORT" = - ] && lens="long"
    for path in $order; do for len in $lens; do run "$path" "$len" "$i"; done; done
done
if [ "$SHORT" = - ]; then
    echo "T(whole job) per repeat, and median (includes start-up and shut-down):"
else
    echo "T(long) - T(short) per repeat, and median:"
fi
for path in cpu gpu; do
    d=""
    for i in $(seq 1 "$REPEATS"); do
        if [ "$SHORT" = - ]; then d="$d ${T[$path-long-$i]}"
        else d="$d $(echo "${T[$path-long-$i]} ${T[$path-short-$i]}" | awk '{printf "%.1f", $1-$2}')"; fi
    done
    med=$(echo $d | tr ' ' '\n' | sort -g | awk '{a[NR]=$1} END {print (NR%2 ? a[(NR+1)/2] : (a[NR/2]+a[NR/2+1])/2)}')
    echo "  $path:$d  -> median $med s"
done
