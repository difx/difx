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
#         STOP         end the job early: a v2d "stop" time (vex time or MJD; used by bench-askap.sh)
#         COMPARE_WITH another run of the same job (its WORKDIR, normally a CPU run): diff this
#                      run's visibilities against it with diffDiFX.py at DIFF_THRESHOLD (percent,
#                      default 0.0005).  PASS/FAIL counts towards the exit status, except for
#                      the production GPU path (USEGPU=1 without DIFX_GPU_SIDEBAND_CPUCOMPAT=1)
#                      against a CPU run: the GPU handles lower sideband (all ASKAP data) at
#                      decode time and the CPU after the FFT, so that comparison is INFO only,
#                      with channel 0 excluded (as run-local.sh does on its sideband scenarios).
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
: "${STOP:=}"
: "${COMPARE_WITH:=}"
: "${DIFF_THRESHOLD:=0.0005}"
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
if [ -n "$COMPARE_WITH" ]; then
    COMPARE_WITH=$(cd "$COMPARE_WITH" 2>/dev/null && pwd) || { echo "ERROR: COMPARE_WITH is not a directory" >&2; exit 1; }
    ls "$COMPARE_WITH/$JOB.difx"/DIFX_* > /dev/null 2>&1 || { echo "ERROR: no $JOB.difx output in $COMPARE_WITH" >&2; exit 1; }
fi
for prog in vex2difx difxcalc mpifxcorr difxlog fringeCheckDiFX.py diffDiFX.py; do
    command -v "$prog" > /dev/null || { echo "ERROR: $prog not on PATH (after sourcing $SETUP_SCRIPT)" >&2; exit 1; }
done

mkdir -p "$WORKDIR"
cp "$JOB.vex" "$WORKDIR/"
sed "s#@DATA@#$DATA#g" "$JOB.v2d" > "$WORKDIR/$JOB.v2d"
[ -z "$STOP" ] || printf '\n# run-askap.sh STOP (minLength lowered so a short cut is not skipped)\nstop = %s\nminLength = 0.1\n' "$STOP" >> "$WORKDIR/$JOB.v2d"
missing=$(grep -o "$DATA/[^ ]*\.codif" "$WORKDIR/$JOB.v2d" | while read -r f; do [ -f "$f" ] || echo "$f"; done)
if [ -n "$missing" ]; then
    echo "ERROR: CODIF files missing from DATA:" >&2
    echo "$missing" | head -5 >&2
    exit 1
fi
cd "$WORKDIR" || exit 1

nthread=$NTHREAD
[ "$USEGPU" = 1 ] && nthread=1
runmode=cpu
[ "$USEGPU" = 1 ] && runmode=gpu
[ "$USEGPU" = 1 ] && [ "${DIFX_GPU_SIDEBAND_CPUCOMPAT:-0}" = 1 ] && runmode=gpucompat
echo "$runmode" > run-mode
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

# Visibility comparison with another run of the same job (COMPARE_WITH)
cmprc=0
if [ -n "$COMPARE_WITH" ]; then
    reffile=$(ls "$COMPARE_WITH/$JOB.difx"/DIFX_* | head -1)
    # The reference run's path: recorded by run-askap.sh, else from its difxlog
    if [ -f "$COMPARE_WITH/run-mode" ]; then
        refmode=$(cat "$COMPARE_WITH/run-mode")
    elif grep -q "GPU Core" "$COMPARE_WITH/$JOB.difxlog" 2>/dev/null; then
        refmode=gpu-unknown
    else
        refmode=cpu
    fi
    info=0; exclude=""
    # production GPU vs a CPU-style run (cpu, gpucompat): sideband handling differs
    case "$runmode:$refmode" in
        gpu:cpu|gpu:gpucompat|cpu:gpu|gpucompat:gpu) info=1; exclude="--excludechannels 0" ;;
    esac
    # shellcheck disable=SC2086
    diffDiFX.py --diagnose -i "$COMPARE_WITH/$JOB.input" -t "$DIFF_THRESHOLD" $exclude \
        "$reffile" "$difxfile" > compare.log 2>&1
    mean=$(grep -oE "After [0-9]+ records, the mean percentage absolute difference is +[0-9.]+" compare.log | tail -1 | awk '{print $NF}')
    hdr=$(grep -oE "At the end, [0-9]+ records disagreed" compare.log | grep -oE "[0-9]+" | head -1)
    if ! grep -q "At the end" compare.log || [ -z "$mean" ]; then
        verdict=ERROR
    elif grep -q "THRESHOLD EXCEEDED\|EPSILON EXCEEDED" compare.log || [ "${hdr:-0}" -gt 0 ]; then
        verdict=FAIL
    else
        verdict=PASS
    fi
    if [ "$info" = 1 ] && [ "$verdict" != ERROR ]; then
        echo "compare  : INFO (would be $verdict at $DIFF_THRESHOLD%; mean difference ${mean}%; CHANNEL 0 EXCLUDED;" \
             "production GPU vs CPU differ in sideband handling, not checked) vs $COMPARE_WITH"
    else
        echo "compare  : $verdict (mean difference ${mean:-?}%, threshold $DIFF_THRESHOLD%; $runmode vs $refmode) vs $COMPARE_WITH"
        [ "$verdict" = PASS ] || cmprc=1
    fi
    echo "           log: $WORKDIR/compare.log"
fi

if [ "$fcrc" != 0 ] || [ "${nerr:-1}" != 0 ] || [ "$cmprc" != 0 ]; then
    echo "FAIL ($JOB)"
    exit 1
fi
echo "PASS ($JOB)"
