#!/bin/bash
#
# run-askap.sh - correlate an ASKAP CRAFT (CODIF) test job and check for fringes.
#
# Usage:  DATA=<dir with the .codif files> ./run-askap.sh [job]      (job default: askap1934-lsb-1bit)
#
# Env:    DATA         directory holding <ant>.p0.codif / <ant>.p1.codif (required)
#         WORKDIR      where to run (default: local-runs/<job>[-gpu]); must not exist yet
#         USEGPU=1     run mpifxcorr with --usegpu
#         NTHREAD      correlator threads in the .threads file (default 8; GPU uses 1)
#         MINSNR       fail if any baseline's median parallel-hand S/N is below this (default 10)
#         MAXCLOSURE   fail if the rms parallel-hand closure phase exceeds this, deg (default 10)
#         SETUP_SCRIPT DiFX setup script to source (default: ../../setup.bash)
#
# The job's <job>.vex and <job>.v2d (written by make_askap_job.py, with @DATA@ in place of
# the CODIF directory) are copied into WORKDIR, @DATA@ is filled in, and vex2difx, difxcalc
# and mpifxcorr are run there, with difxlog capturing alerts in <job>.difxlog.  The result
# is checked with fringeCheckDiFX.py (utilities/vis2screen).  Exit status is non-zero if any
# step or the fringe check fails.

set -u
cd "$(dirname "$0")"
TESTDIR=$(pwd)

JOB=${1:-askap1934-lsb-1bit}
: "${DATA:?set DATA to the directory holding the .codif files (see README.md)}"
: "${USEGPU:=0}"
: "${NTHREAD:=8}"
: "${MINSNR:=10}"
: "${MAXCLOSURE:=10}"
: "${SETUP_SCRIPT:=$TESTDIR/../../setup.bash}"
suffix=""
[ "$USEGPU" = 1 ] && suffix="-gpu"
: "${WORKDIR:=$TESTDIR/local-runs/$JOB$suffix}"

for f in "$JOB.vex" "$JOB.v2d"; do
    [ -f "$f" ] || { echo "ERROR: $TESTDIR/$f not found" >&2; exit 1; }
done
DATA=$(cd "$DATA" && pwd) || { echo "ERROR: DATA=$DATA is not a directory" >&2; exit 1; }
if [ -e "$WORKDIR" ]; then
    echo "ERROR: $WORKDIR already exists; remove it or set WORKDIR" >&2
    exit 1
fi

# shellcheck disable=SC1090
set +u; . "$SETUP_SCRIPT" > /dev/null; set -u
for prog in vex2difx difxcalc mpifxcorr difxlog fringeCheckDiFX.py; do
    command -v "$prog" > /dev/null || { echo "ERROR: $prog not on PATH (after sourcing $SETUP_SCRIPT)" >&2; exit 1; }
done

mkdir -p "$WORKDIR"
cp "$JOB.vex" "$WORKDIR/"
sed "s#@DATA@#$DATA#g" "$JOB.v2d" > "$WORKDIR/$JOB.v2d"
missing=$(grep -o "$DATA/[^ ]*\.codif" "$WORKDIR/$JOB.v2d" | while read -r f; do [ -f "$f" ] || echo "$f"; done)
if [ -n "$missing" ]; then
    echo "ERROR: CODIF files missing from DATA:" >&2
    echo "$missing" | head -5 >&2
    exit 1
fi
cd "$WORKDIR" || exit 1

nthread=$NTHREAD
[ "$USEGPU" = 1 ] && nthread=1
printf 'NUMBER OF CORES:    1\n%d\n' "$nthread" > "$JOB.threads"

echo "Job      : $JOB  (DATA=$DATA)"
echo "Workdir  : $WORKDIR"
echo "mpifxcorr: $(command -v mpifxcorr)$([ "$USEGPU" = 1 ] && echo '  --usegpu')"

vex2difx "$JOB.v2d" > vex2difx.log 2>&1 || { echo "ERROR: vex2difx failed, see $WORKDIR/vex2difx.log" >&2; exit 1; }
[ -f "$JOB.input" ] || { echo "ERROR: vex2difx made no $JOB.input (see vex2difx.log)" >&2; exit 1; }
difxcalc "$JOB.calc" > difxcalc.log 2>&1 || { echo "ERROR: difxcalc failed, see $WORKDIR/difxcalc.log" >&2; exit 1; }

nds=$(awk '/^ACTIVE DATASTREAMS/{print $3}' "$JOB.input")
np=$((nds + 2))
echo "localhost slots=$np" > machines
usegpuflag=""
[ "$USEGPU" = 1 ] && usegpuflag="--usegpu"

# mpifxcorr reports alerts over difxmessage, not stdout: keep them with difxlog
difxlog "$JOB" "$JOB.difxlog" 4 & logpid=$!
sleep 1
t0=$(date +%s.%N)
mpirun --oversubscribe --mca mpi_yield_when_idle 1 -machinefile machines -np "$np" \
    mpifxcorr "$JOB.input" --nocommandthread $usegpuflag > mpifxcorr.log 2>&1
rc=$?
t1=$(date +%s.%N)
sleep 1
kill "$logpid" 2> /dev/null
wait "$logpid" 2> /dev/null
echo "mpifxcorr: exit $rc, $(echo "$t0 $t1" | awk '{printf "%.1f", $2-$1}') s, $nds datastreams"

nerr=$(grep -c -E ' (ERROR|SEVERE|FATAL) ' "$JOB.difxlog" 2> /dev/null)
echo "difxlog  : ${nerr:-?} ERROR/SEVERE/FATAL lines ($WORKDIR/$JOB.difxlog)"

difxfile=$(ls "$JOB.difx"/DIFX_* 2> /dev/null | head -1)
if [ "$rc" != 0 ] || [ -z "$difxfile" ]; then
    echo "FAIL: correlation did not produce output (see $WORKDIR/mpifxcorr.log)"
    exit 1
fi

# ASKAP coarse channels are oversampled by 32/27: ignore the outer 8% each side for phases
fringeCheckDiFX.py "$JOB.input" "$difxfile" --edgefrac 0.08 --minsnr "$MINSNR" --maxclosure "$MAXCLOSURE" \
    | tee fringecheck.txt
fcrc=${PIPESTATUS[0]}
if [ "$fcrc" != 0 ] || [ "${nerr:-1}" != 0 ]; then
    echo "FAIL ($JOB)"
    exit 1
fi
echo "PASS ($JOB)"
