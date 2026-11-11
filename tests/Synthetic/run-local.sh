#!/bin/bash
#
# run-local.sh - single-machine CPU-vs-GPU regression harness for the
# synthetic DiFX tests in this directory.
#
# A local (no SLURM) equivalent of run-slurm.sh: correlates each synthetic
# scenario via mpirun once per mode (cpu, gpu with DIFX_GPU_PIPELINE=0, gpu
# with DIFX_GPU_PIPELINE=1), then uses diffDiFX.py to compare the raw
# visibility output and reports PASS/FAIL.  Unlike run-all.sh, this DOES
# check correctness.  Jobs run sequentially (one GPU, single machine).
#
# Usage:
#   ./run-local.sh [scenario ...]
#
# With no arguments all scenarios are run.  Otherwise only the named
# scenarios run, e.g.:  ./run-local.sh usb usb-complex
#
# Environment overrides:
#   WORKDIR        job/output directory  (default: <this dir>/local-runs)
#   SETUP_SCRIPT   DiFX setup script     (default: <repo root>/setup.bash)
#   DIFF_THRESHOLD diffDiFX.py threshold (default: 0.0005)
#   NTASKS         MPI ranks per job     (default: ACTIVE DATASTREAMS + 2,
#                  read from each scenario's .input)
#
# Scenarios listed in GPU_UNSUPPORTED below are SKIPped on the GPU legs
# (GPUMode NOT_SUPPORTED("lower sideband")); remove entries as GPU support
# grows.

set -u

SYNTHDIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SYNTHDIR"

: "${WORKDIR:=$SYNTHDIR/local-runs}"
: "${SETUP_SCRIPT:=$SYNTHDIR/../../setup.bash}"
: "${DIFF_THRESHOLD:=0.0005}"
: "${NTASKS:=}"

# Per-run state as files (mirrors run-slurm.sh; keys use '~' separators).
STATE="$(mktemp -d "${TMPDIR:-/tmp}/difx-local-test.XXXXXX")"
trap 'rm -rf "$STATE"' EXIT
state_set() { printf '%s' "$2" > "$STATE/$1"; }
state_get() { cat "$STATE/$1" 2>/dev/null; }
state_has() { [ -f "$STATE/$1" ]; }

######## Scenarios and modes #################################################

ALL_SCENARIOS=(usb lsb usb-complex lsb-complex usb-dsb lsb-dsb complex-complex multi dualpol)

# Scenarios the GPU path rejects by design (lower sideband not implemented).
GPU_UNSUPPORTED=(lsb lsb-complex usb-dsb lsb-dsb)

if [ "$#" -gt 0 ]; then
    SCENARIOS=("$@")
else
    SCENARIOS=("${ALL_SCENARIOS[@]}")
fi

# gpu0/gpu1 = --usegpu with DIFX_GPU_PIPELINE=0/1; both must PASS vs cpu.
MODES=(cpu gpu0 gpu1)
COMPARISONS=("cpu:gpu0" "cpu:gpu1")

gpu_unsupported() {
    local s
    for s in "${GPU_UNSUPPORTED[@]}"; do [ "$s" = "$1" ] && return 0; done
    return 1
}

echo "Scenarios : ${SCENARIOS[*]}"
echo "Modes     : ${MODES[*]}"
echo "Workdir   : $WORKDIR"
echo

######## Ensure synthetic VDIF data exists ###################################

if [ ! -f "$SYNTHDIR/TEST1.vdif" ] || [ ! -f "$SYNTHDIR/TEST2-dsb-lsb.vdif" ]; then
    echo "Synthetic VDIF data not found - generating with createData.sh ..."
    ( cd "$SYNTHDIR" && ./createData.sh )
fi

######## Per-scenario, per-mode preparation ##################################

prep_mode() {
    local scen="$1" mode="$2"
    local expname="test-$scen"
    local v2d="${expname}.v2d"
    local jobdir="$WORKDIR/$scen/$mode"

    if [ ! -f "$SYNTHDIR/$v2d" ]; then
        echo "  [$scen/$mode] ERROR: $v2d not found" >&2
        return 1
    fi

    rm -rf "$jobdir"
    mkdir -p "$jobdir"

    cp "$SYNTHDIR/$v2d" "$jobdir/"
    local vex
    vex="$(awk -F= '/^[[:space:]]*vex[[:space:]]*=/{gsub(/[[:space:]]/,"",$2);print $2}' "$SYNTHDIR/$v2d")"
    cp "$SYNTHDIR/$vex" "$jobdir/"
    local vdif
    for vdif in $(awk -F= '/^[[:space:]]*file[[:space:]]*=/{gsub(/[[:space:]]/,"",$2);print $2}' "$SYNTHDIR/$v2d"); do
        ln -sf "$SYNTHDIR/$vdif" "$jobdir/$vdif"
    done

    if ( set -e; set +u; . "$SETUP_SCRIPT"; set -u; cd "$jobdir"; \
         vex2difx "$v2d"; difxcalc "${expname}.calc" ) \
         > "$jobdir/prep.log" 2>&1; then
        state_set "prep~$scen~$mode" 1
        echo "  [$scen/$mode] prepared"
    else
        echo "  [$scen/$mode] ERROR: vex2difx/difxcalc failed (see $jobdir/prep.log)" >&2
    fi
}

echo "== Preparing job directories =="
for scen in "${SCENARIOS[@]}"; do
    for mode in "${MODES[@]}"; do
        if [ "$mode" != cpu ] && gpu_unsupported "$scen"; then
            state_set "rc~$scen~$mode" SKIP
            continue
        fi
        prep_mode "$scen" "$mode"
    done
done
echo

######## Run all jobs sequentially ###########################################

run_mode() {
    local scen="$1" mode="$2"
    local expname="test-$scen"
    local jobdir="$WORKDIR/$scen/$mode"
    local usegpu="" pipeline=""

    case "$mode" in
        gpu0) usegpu="--usegpu"; pipeline=0 ;;
        gpu1) usegpu="--usegpu"; pipeline=1 ;;
    esac

    # 1 manager + N datastreams + 1 core, unless NTASKS overrides
    local np="$NTASKS"
    if [ -z "$np" ]; then
        np=$(awk '/^ACTIVE DATASTREAMS/{print $3+2}' "$jobdir/${expname}.input")
    fi

    rm -rf "$jobdir/${expname}.difx"
    ( set +u; . "$SETUP_SCRIPT"; set -u; cd "$jobdir"; \
      [ -n "$pipeline" ] && export DIFX_GPU_PIPELINE=$pipeline; \
      mpirun --oversubscribe --mca mpi_yield_when_idle 1 \
          -machinefile "$SYNTHDIR/machines" -np "$np" \
          mpifxcorr "${expname}.input" --nocommandthread $usegpu ) \
      > "$jobdir/${mode}.mpilog" 2>&1
}

echo "== Running correlations (sequential) =="
for scen in "${SCENARIOS[@]}"; do
    for mode in "${MODES[@]}"; do
        if [ "$(state_get "rc~$scen~$mode")" = SKIP ]; then
            echo "  [$scen/$mode] SKIP (unsupported on GPU)"
            continue
        fi
        state_has "prep~$scen~$mode" || { state_set "rc~$scen~$mode" 1; continue; }
        if run_mode "$scen" "$mode"; then
            state_set "rc~$scen~$mode" 0
            echo "  [$scen/$mode] completed"
        else
            state_set "rc~$scen~$mode" 1
            echo "  [$scen/$mode] FAILED (see $WORKDIR/$scen/$mode/${mode}.mpilog)" >&2
        fi
    done
done
echo

######## Diff and evaluate ###################################################

# evaluate_diff <difflog> -> echoes PASS | FAIL | ERROR based on diffDiFX.py
# output (which always exits 0 and signals problems only by what it prints).
evaluate_diff() {
    local log="$1"
    if ! grep -q "At the end" "$log"; then
        echo ERROR; return
    fi
    if grep -q "THRESHOLD EXCEEDED\|EPSILON EXCEEDED" "$log"; then
        echo FAIL; return
    fi
    local hdr
    hdr="$(grep -oE "At the end, [0-9]+ records disagreed" "$log" | grep -oE "[0-9]+" | head -1)"
    if [ -n "$hdr" ] && [ "$hdr" -gt 0 ]; then
        echo FAIL; return
    fi
    local recs
    recs="$(grep -oE "After [0-9]+ records, the mean" "$log" | grep -oE "[0-9]+" | head -1)"
    if [ -z "$recs" ] || [ "$recs" -le 0 ]; then
        echo ERROR; return
    fi
    echo PASS
}

compare_modes() {
    local scen="$1" a="$2" b="$3"
    local expname="test-$scen"
    local dira="$WORKDIR/$scen/$a" dirb="$WORKDIR/$scen/$b"
    local input="$dira/${expname}.input"
    local diffdir="$WORKDIR/$scen/diff"
    mkdir -p "$diffdir"

    local afiles=("$dira/${expname}.difx"/DIFX_*)
    if [ ! -e "${afiles[0]}" ]; then
        echo ERROR; return
    fi

    local overall=PASS f base bfile log res
    for f in "${afiles[@]}"; do
        base="$(basename "$f")"
        bfile="$dirb/${expname}.difx/$base"
        log="$diffdir/${a}-vs-${b}.${base}.log"
        if [ ! -e "$bfile" ]; then
            echo "missing $bfile" > "$log"
            overall=ERROR
            continue
        fi
        ( set +u; . "$SETUP_SCRIPT"; set -u; \
          diffDiFX.py --diagnose -i "$input" -t "$DIFF_THRESHOLD" "$f" "$bfile" ) \
          > "$log" 2>&1
        res="$(evaluate_diff "$log")"
        case "$res" in
            FAIL)  overall=FAIL ;;
            ERROR) [ "$overall" = PASS ] && overall=ERROR ;;
        esac
    done
    echo "$overall"
}

echo "== Comparing visibilities =="
for scen in "${SCENARIOS[@]}"; do
    for cmp in "${COMPARISONS[@]}"; do
        a="${cmp%%:*}"; b="${cmp##*:}"
        if [ "$(state_get "rc~$scen~$b")" = SKIP ]; then
            res=SKIP
            note=" (unsupported on GPU)"
        elif [ "$(state_get "rc~$scen~$a")" != 0 ] || [ "$(state_get "rc~$scen~$b")" != 0 ]; then
            res=ERROR
            note=" (a correlation job exited nonzero; diff not attempted)"
        else
            res="$(compare_modes "$scen" "$a" "$b")"
            note=""
        fi
        state_set "result~$scen~$cmp" "$res"
        echo "  [$scen] $cmp -> $res$note"
    done
done
echo

######## Summary table #######################################################

echo "======================= SUMMARY ======================="
printf "%-18s" "scenario"
for cmp in "${COMPARISONS[@]}"; do printf " %-16s" "$cmp"; done
printf "\n"

overall_status=0
for scen in "${SCENARIOS[@]}"; do
    printf "%-18s" "$scen"
    for cmp in "${COMPARISONS[@]}"; do
        r="$(state_get "result~$scen~$cmp")"
        [ -n "$r" ] || r=ERROR
        printf " %-16s" "$r"
        case "$r" in PASS|SKIP) ;; *) overall_status=1 ;; esac
    done
    printf "\n"
done
echo "======================================================="
echo "Detailed diff logs are under $WORKDIR/<scenario>/diff/"

exit $overall_status
