#!/bin/bash
#
# bench-askap-staging.sh - does copying the CODIF data to node-local disk first make an
# ASKAP job faster?
#
# Usage:  DATA=<codif dir on shared storage> STAGEDIR=<dir on node-local disk> \
#             ./bench-askap-staging.sh <job> <short stop | -> [repeats]
#
# Copies the job's CODIF files from DATA into a new directory under STAGEDIR (timed: the
# cost staging adds), then times mpifxcorr (run-askap.sh) reading the data three ways:
#   shared-cold  from DATA, with its files dropped from this node's page cache first
#   local-cold   from the copy in STAGEDIR, dropped from the page cache first
#   warm         from DATA, read into the page cache first (no disk at all; the setting
#                bench-askap.sh uses by default)
# <repeats> times (default 3), rotating the order of the legs.  The metric is as in
# bench-askap.sh: T(long) - T(short) without start-up and shut-down, or the whole job when
# the short stop is "-".  The two cold legs differ only in where the data are read from,
# and the copy time is what staging costs in production, so staging pays if
#   copy time + local-cold < shared-cold     (for the whole job, i.e. with "-")
#
# Pages are dropped with posix_fadvise(DONTNEED) (pagecache.py, no root needed), and the
# share of the files still in the cache is checked before every run; a run whose drop or
# warm-up did not work is reported as FAILED.  "Cold" means cold on this node: the file
# servers may still hold the data in their own caches.
#
# Env:  PATHS    which correlator paths to time, default "gpu" ("cpu gpu" for both)
#       NTHREAD  CPU correlator threads, as in run-askap.sh
# Output: local-runs/bench-staging-<job>-<stamp>/.  The copy is left in
# STAGEDIR/askap-staging-<job>-<stamp>/: on a cluster's job-local disk it goes when the job
# ends; elsewhere remove it yourself.

set -u
cd "$(dirname "$0")"
TESTDIR=$(pwd)
JOB=${1:?job name}
SHORT=${2:?short stop time, or -}
REPEATS=${3:-3}
: "${DATA:?set DATA to the CODIF directory on shared storage}"
: "${STAGEDIR:?set STAGEDIR to a directory on node-local disk}"
: "${PATHS:=gpu}"
STAMP=$(date +%Y%m%d-%H%M%S)
OUT="$TESTDIR/local-runs/bench-staging-$JOB-$STAMP"
DATA=$(cd "$DATA" && pwd) || { echo "ERROR: DATA=$DATA is not a directory" >&2; exit 1; }
[ -d "$STAGEDIR" ] || { echo "ERROR: STAGEDIR=$STAGEDIR is not a directory" >&2; exit 1; }
LOCAL="$(cd "$STAGEDIR" && pwd)/askap-staging-$JOB-$STAMP"

# the files this job reads (run-askap.sh puts DATA in place of @DATA@ in the v2d)
mapfile -t names < <(grep -oE "@DATA@/[^ ]*\.codif" "$JOB.v2d" | sed 's#@DATA@/##' | sort -u)
[ "${#names[@]}" -gt 0 ] || { echo "ERROR: no @DATA@/*.codif files in $JOB.v2d" >&2; exit 1; }
for n in "${names[@]}"; do
    [ -f "$DATA/$n" ] || { echo "ERROR: $DATA/$n not found" >&2; exit 1; }
done
bytes=$(cd "$DATA" && du -cbL "${names[@]}" | tail -1 | cut -f1)
free=$(df -B1 --output=avail "$STAGEDIR" | tail -1)
[ "$free" -gt $((bytes + bytes / 10)) ] || { echo "ERROR: $STAGEDIR has $free bytes free, the data need $bytes" >&2; exit 1; }

shared_files=("${names[@]/#/$DATA/}")
local_files=("${names[@]/#/$LOCAL/}")
pc() { python3 -I "$TESTDIR/pagecache.py" "$@"; }
mkdir -p "$OUT" "$LOCAL"

echo "Job $JOB, short stop $SHORT, $REPEATS repeats, paths: $PATHS; output $OUT"
echo "Data: ${#names[@]} files, $((bytes / 1000000)) MB"
echo "  shared: $DATA ($(df --output=fstype "$DATA" | tail -1))"
echo "  local:  $LOCAL ($(df --output=fstype "$LOCAL" | tail -1))"

# the copy, from a cold cache, written all the way to disk
pc drop "${shared_files[@]}"
t0=$(date +%s.%N)
command cp "${shared_files[@]}" "$LOCAL/" && sync "${local_files[@]}"
rc=$?
t1=$(date +%s.%N)
[ $rc = 0 ] || { echo "ERROR: copy to $LOCAL failed" >&2; exit 1; }
copy=$(echo "$t0 $t1" | awk '{printf "%.1f", $2-$1}')
echo "Copy to local disk: $copy s ($(echo "$bytes $t0 $t1" | awk '{printf "%.0f", $1/1e6/($3-$2)}') MB/s)"

declare -A T
run() {   # run <path cpu|gpu> <leg shared-cold|local-cold|warm> <length short|long> <repeat>
    local path="$1" leg="$2" len="$3" i="$4" usegpu=0 stop="" data="$DATA" res ok
    [ "$path" = gpu ] && usegpu=1
    [ "$len" = short ] && stop="$SHORT"
    local wd="$OUT/$path-$leg-$len-$i"
    case "$leg" in
        shared-cold) pc drop "${shared_files[@]}"; res=$(pc resident "${shared_files[@]}") ;;
        local-cold)  data="$LOCAL"; pc drop "${local_files[@]}"; res=$(pc resident "${local_files[@]}") ;;
        warm)        cat "${shared_files[@]}" > /dev/null; res=$(pc resident "${shared_files[@]}") ;;
    esac
    if [ "$leg" = warm ]; then ok=$(echo "$res" | awk '{print ($1 >= 95)}'); else ok=$(echo "$res" | awk '{print ($1 <= 5)}'); fi
    if [ "$ok" != 1 ]; then
        echo "  $path $leg $len #$i: FAILED: $res% of the files in the page cache before the run"; exit 1
    fi
    # timing legs: no S/N or closure thresholds (as bench-askap.sh)
    USEGPU=$usegpu STOP="$stop" WORKDIR="$wd" DATA="$data" MINSNR=0 MAXCLOSURE=360 \
        ./run-askap.sh "$JOB" > "$wd.log" 2>&1
    local rc=$?
    local t
    t=$(grep -oE "^mpifxcorr: exit 0, [0-9.]+ s" "$wd.log" | awk '{print $4}')
    if [ "$rc" != 0 ] || [ -z "$t" ] || ! grep -q "^difxlog  : 0 ERROR" "$wd.log"; then
        echo "  $path $leg $len #$i: FAILED (see $wd.log)"; exit 1
    fi
    T[$path-$leg-$len-$i]=$t
    echo "  $path $leg $len #$i: $t s  ($res% cached before)"
}

legs=(shared-cold local-cold warm)
for i in $(seq 1 "$REPEATS"); do
    lens="long short"; [ $((i % 2)) = 0 ] && lens="short long"
    [ "$SHORT" = - ] && lens="long"
    for path in $PATHS; do
        for k in 0 1 2; do
            leg=${legs[$(((k + i - 1) % 3))]}
            for len in $lens; do run "$path" "$leg" "$len" "$i"; done
        done
    done
done

if [ "$SHORT" = - ]; then
    echo "T(whole job) per repeat, and median (includes start-up and shut-down):"
else
    echo "T(long) - T(short) per repeat, and median:"
fi
for path in $PATHS; do
    for leg in "${legs[@]}"; do
        d=""
        for i in $(seq 1 "$REPEATS"); do
            if [ "$SHORT" = - ]; then d="$d ${T[$path-$leg-long-$i]}"
            else d="$d $(echo "${T[$path-$leg-long-$i]} ${T[$path-$leg-short-$i]}" | awk '{printf "%.1f", $1-$2}')"; fi
        done
        med=$(echo $d | tr ' ' '\n' | sort -g | awk '{a[NR]=$1} END {print (NR%2 ? a[(NR+1)/2] : (a[NR/2]+a[NR/2+1])/2)}')
        printf '  %s %-11s:%s  -> median %s s\n' "$path" "$leg" "$d" "$med"
    done
done
echo "Copy to local disk (once): $copy s"
echo "Staged copy left in $LOCAL"
