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
#   REFERENCE_SETUP_SCRIPT
#                  setup script of a second, reference DiFX build (e.g. a dev
#                  checkout).  If set, each scenario is also correlated on the
#                  CPU entirely with that build's tools (vex2difx, difxcalc,
#                  mpifxcorr) and compared with this build's CPU output
#                  (cpu:reference).  Same name as in run-slurm.sh.
#
# Scenarios listed in GPU_UNSUPPORTED below are SKIPped on the GPU legs
# (for features the GPU path refuses with NOT_SUPPORTED); remove entries as GPU
# support grows.
#
# The lower/double sideband scenarios (SIDEBAND_SCENARIOS) get one more GPU leg,
# gpucompat: DIFX_GPU_SIDEBAND_CPUCOMPAT=1 selects a GPU test path that
# reproduces the CPU's sideband handling, so cpu:gpucompat must agree to the
# usual DIFF_THRESHOLD.  For those scenarios cpu:gpucompat is the only
# CPU-vs-GPU comparison that can PASS or FAIL: the production GPU path turns
# each band into upper sideband while decoding, where the CPU reorders channels
# after the FFT, so cpu:gpu0 and cpu:gpu1 differ by more than rounding (the CPU
# leaves the DC channel of complex LSB unconjugated, and rotates about a
# different reference frequency; measured 2026-11-13: mean 0.03% DSB, 0.06% LSB,
# per record up to 0.17%).  Those two legs are still run and diffed, with
# channel 0 excluded, and are reported as INFO: they never pass or fail the run.
# Remove this exception (SIDEBAND_INFO_ONLY) once the CPU handles sidebands at
# unpack time like the GPU.
#
# The long-baseline scenarios (lb-*, data from createLongBaseline.sh, which
# needs vdifsim) record the same signal at T2 as USB and as LSB.  They add a
# sideband invariance check (sideband-invariance.py): each LSB scenario's
# output must match its USB partner's CPU output to DIFF_THRESHOLD.  That is
# the PASS/FAIL test of the production GPU sideband path (gpu0, gpu1); the
# CPU and gpucompat results are shown as INFO, since the CPU's post-FFT
# handling corrects the Doppler shift about a different reference frequency
# and does not meet it (measured 2026-11-13 on the weak-source data: 0.22%
# mean; its extra coherence loss, 3e-5, is below the noise at this signal
# level).  The lb-* scenarios run by default when their data exist.

set -u

SYNTHDIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SYNTHDIR"

: "${WORKDIR:=$SYNTHDIR/local-runs}"
: "${SETUP_SCRIPT:=$SYNTHDIR/../../setup.bash}"
: "${DIFF_THRESHOLD:=0.0005}"
: "${NTASKS:=}"
: "${REFERENCE_SETUP_SCRIPT:=}"

# Per-run state as files (mirrors run-slurm.sh; keys use '~' separators).
STATE="$(mktemp -d "${TMPDIR:-/tmp}/difx-local-test.XXXXXX")"
trap 'rm -rf "${STATE:?}"' EXIT
state_set() { printf '%s' "$2" > "$STATE/$1"; }
state_get() { cat "$STATE/$1" 2>/dev/null; }
state_has() { [ -f "$STATE/$1" ]; }

######## Scenarios and modes #################################################

ALL_SCENARIOS=(usb lsb usb-complex lsb-complex usb-dsb lsb-dsb complex-complex multi dualpol)

# Long-baseline scenarios (createLongBaseline.sh), run by default only if their
# data exist, and the sideband invariance pairs: "<lsb scenario>:<usb scenario>".
LB_SCENARIOS=(lb-usb lb-lsb lb-usb-complex lb-lsb-complex)
INVARIANCE_PAIRS=("lb-lsb:lb-usb" "lb-lsb-complex:lb-usb-complex")

# Scenarios the GPU path rejects by design (none at present: lower and double
# sideband are handled while decoding).
GPU_UNSUPPORTED=()

# Scenarios with lower- or double-sideband bands (see the gpucompat notes above).
SIDEBAND_SCENARIOS=(lsb lsb-complex usb-dsb lsb-dsb lb-lsb lb-lsb-complex)

# While the CPU handles sidebands after the FFT, cpu:gpu0 and cpu:gpu1 on the
# sideband scenarios are informational only, diffed without these channels.
# Set to 0 (and drop the exclusion) when the CPU path matches the GPU's.
SIDEBAND_INFO_ONLY=1
SIDEBAND_EXCLUDE_CHANNELS=0

if [ "$#" -gt 0 ]; then
    SCENARIOS=("$@")
else
    SCENARIOS=("${ALL_SCENARIOS[@]}")
    if [ -f "$SYNTHDIR/TESTLB1.vdif" ]; then
        SCENARIOS+=("${LB_SCENARIOS[@]}")
    else
        echo "NOTE: no long-baseline data (TESTLB1.vdif): skipping ${LB_SCENARIOS[*]};"
        echo "      run createLongBaseline.sh (needs vdifsim) to include them."
    fi
fi

# gpu0/gpu1 = --usegpu with DIFX_GPU_PIPELINE=0/1; both must PASS vs cpu
#             (INFO only on the sideband scenarios, see above).
# gpucompat = --usegpu with DIFX_GPU_SIDEBAND_CPUCOMPAT=1 (sideband scenarios only).
MODES=(cpu gpu0 gpu1 gpucompat)
COMPARISONS=("cpu:gpu0" "cpu:gpu1" "cpu:gpucompat")

# Optional reference leg: a CPU run with another DiFX build.
if [ -n "$REFERENCE_SETUP_SCRIPT" ]; then
    if [ ! -f "$REFERENCE_SETUP_SCRIPT" ]; then
        echo "ERROR: REFERENCE_SETUP_SCRIPT=$REFERENCE_SETUP_SCRIPT not found" >&2
        exit 2
    fi
    MODES+=(reference)
    COMPARISONS+=("cpu:reference")
fi

# The setup script for a mode: each mode uses only its own build's tools.
mode_setup() {
    if [ "$1" = reference ]; then echo "$REFERENCE_SETUP_SCRIPT"; else echo "$SETUP_SCRIPT"; fi
}

# The mpifxcorr a setup script puts on PATH (sourced in a subshell).
which_mpifxcorr() {
    ( set +u; . "$1" >/dev/null 2>&1; command -v mpifxcorr ) 2>/dev/null
}

gpu_unsupported() {
    local s
    for s in "${GPU_UNSUPPORTED[@]}"; do [ "$s" = "$1" ] && return 0; done
    return 1
}

is_sideband_scenario() {
    local s
    for s in "${SIDEBAND_SCENARIOS[@]}"; do [ "$s" = "$1" ] && return 0; done
    return 1
}

# skip_reason <scenario> <mode> -> prints why the mode is not run, or nothing
skip_reason() {
    if is_gpu_mode "$2" && gpu_unsupported "$1"; then
        echo "unsupported on GPU"
    elif [ "$2" = gpucompat ] && ! is_sideband_scenario "$1"; then
        echo "upper sideband only: same as gpu1"
    fi
}

# True for the modes that run on the GPU.
is_gpu_mode() {
    case "$1" in gpu*) return 0 ;; *) return 1 ;; esac
}

echo "Scenarios : ${SCENARIOS[*]}"
echo "Modes     : ${MODES[*]}"
echo "Workdir   : $WORKDIR"
echo "mpifxcorr : $(which_mpifxcorr "$SETUP_SCRIPT")"
if [ -n "$REFERENCE_SETUP_SCRIPT" ]; then
    REF_BIN="$(which_mpifxcorr "$REFERENCE_SETUP_SCRIPT")"
    echo "Reference : $REF_BIN  (from $REFERENCE_SETUP_SCRIPT)"
    if [ "$REF_BIN" = "$(which_mpifxcorr "$SETUP_SCRIPT")" ]; then
        echo "ERROR: the reference setup script selects the same mpifxcorr as SETUP_SCRIPT" >&2
        exit 2
    fi
fi
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

    rm -rf "${jobdir:?}"
    mkdir -p "$jobdir"

    cp "$SYNTHDIR/$v2d" "$jobdir/"
    local vex
    vex="$(awk -F= '/^[[:space:]]*vex[[:space:]]*=/{gsub(/[[:space:]]/,"",$2);print $2}' "$SYNTHDIR/$v2d")"
    cp "$SYNTHDIR/$vex" "$jobdir/"
    local vdif
    for vdif in $(awk -F= '/^[[:space:]]*file[[:space:]]*=/{gsub(/[[:space:]]/,"",$2);print $2}' "$SYNTHDIR/$v2d"); do
        ln -sf "$SYNTHDIR/$vdif" "$jobdir/$vdif"
    done

    local setup
    setup="$(mode_setup "$mode")"
    if ( set -e; set +u; . "$setup"; set -u; cd "$jobdir"; \
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
        reason="$(skip_reason "$scen" "$mode")"
        if [ -n "$reason" ]; then
            state_set "rc~$scen~$mode" SKIP
            state_set "skip~$scen~$mode" "$reason"
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
    local usegpu="" pipeline="" cpucompat=""

    case "$mode" in
        gpu0) usegpu="--usegpu"; pipeline=0 ;;
        gpu1) usegpu="--usegpu"; pipeline=1 ;;
        gpucompat) usegpu="--usegpu"; pipeline=1; cpucompat=1 ;;
    esac

    # 1 manager + N datastreams + 1 core, unless NTASKS overrides
    local np="$NTASKS"
    if [ -z "$np" ]; then
        np=$(awk '/^ACTIVE DATASTREAMS/{print $3+2}' "$jobdir/${expname}.input")
    fi

    local setup
    setup="$(mode_setup "$mode")"
    rm -rf "${jobdir:?}/${expname:?}.difx" "${jobdir:?}/${expname:?}.difxlog"
    # mpifxcorr sends its alerts (info, warnings, errors) over difxmessage,
    # not to stdout, so run difxlog alongside it (as startdifx does) to keep
    # them in <jobdir>/<expname>.difxlog.  Jobs run one at a time, so the
    # shared identity (the job name) does not mix logs between modes.
    ( set +u; . "$setup"; set -u; cd "$jobdir"; \
      [ -n "$pipeline" ] && export DIFX_GPU_PIPELINE=$pipeline; \
      [ -n "$cpucompat" ] && export DIFX_GPU_SIDEBAND_CPUCOMPAT=1; \
      logpid=""; \
      if command -v difxlog >/dev/null 2>&1; then \
          difxlog "$expname" "${expname}.difxlog" 4 & logpid=$!; sleep 1; \
      else \
          echo "WARNING: difxlog not found; alerts will not be saved"; \
      fi; \
      mpirun --oversubscribe --mca mpi_yield_when_idle 1 \
          -machinefile "$SYNTHDIR/machines" -np "$np" \
          mpifxcorr "${expname}.input" --nocommandthread $usegpu; \
      rc=$?; \
      if [ -n "$logpid" ]; then sleep 1; kill "$logpid" 2>/dev/null; wait "$logpid" 2>/dev/null; fi; \
      exit $rc ) \
      > "$jobdir/${mode}.mpilog" 2>&1
}

# log_summary <jobdir> <expname> -> " (difxlog: N ERROR, M SEVERE/FATAL)" or ""
log_summary() {
    local log="$1/$2.difxlog" nerr nsev
    [ -f "$log" ] || { echo " (no difxlog)"; return; }
    nerr=$(grep -c ' ERROR ' "$log")
    nsev=$(grep -cE ' (SEVERE|FATAL) ' "$log")
    [ "$nerr" -gt 0 ] || [ "$nsev" -gt 0 ] && echo " (difxlog: $nerr ERROR, $nsev SEVERE/FATAL)"
}

echo "== Running correlations (sequential) =="
for scen in "${SCENARIOS[@]}"; do
    for mode in "${MODES[@]}"; do
        if [ "$(state_get "rc~$scen~$mode")" = SKIP ]; then
            echo "  [$scen/$mode] SKIP ($(state_get "skip~$scen~$mode"))"
            continue
        fi
        state_has "prep~$scen~$mode" || { state_set "rc~$scen~$mode" 1; continue; }
        if run_mode "$scen" "$mode"; then
            state_set "rc~$scen~$mode" 0
            echo "  [$scen/$mode] completed$(log_summary "$WORKDIR/$scen/$mode" "test-$scen")"
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

# compare_modes <scenario> <a> <b> [excludechannels] -> PASS | FAIL | ERROR
compare_modes() {
    local scen="$1" a="$2" b="$3" exclude="${4:-}"
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
          diffDiFX.py --diagnose -i "$input" -t "$DIFF_THRESHOLD" \
            ${exclude:+--excludechannels "$exclude"} "$f" "$bfile" ) \
          > "$log" 2>&1
        res="$(evaluate_diff "$log")"
        case "$res" in
            FAIL)  overall=FAIL ;;
            ERROR) [ "$overall" = PASS ] && overall=ERROR ;;
        esac
    done
    echo "$overall"
}

# The mean percentage absolute difference over all files of a comparison
# (from the diffDiFX.py logs), for the INFO legs.
mean_diff() {
    local scen="$1" a="$2" b="$3"
    cat "$WORKDIR/$scen/diff/${a}-vs-${b}".*.log 2>/dev/null \
      | grep -oE "After [0-9]+ records, the mean percentage absolute difference is +[0-9.]+" \
      | awk '{n++; s+=$NF} END {if (n) printf "%.4f", s/n; else printf "?"}'
}

info_only_leg() {
    [ "$SIDEBAND_INFO_ONLY" = 1 ] && is_sideband_scenario "$1" && \
      { [ "$2" = gpu0 ] || [ "$2" = gpu1 ]; }
}

echo "== Comparing visibilities =="
for scen in "${SCENARIOS[@]}"; do
    for cmp in "${COMPARISONS[@]}"; do
        a="${cmp%%:*}"; b="${cmp##*:}"
        if [ "$(state_get "rc~$scen~$b")" = SKIP ]; then
            res=SKIP
            note=" ($(state_get "skip~$scen~$b"))"
        elif [ "$(state_get "rc~$scen~$a")" != 0 ] || [ "$(state_get "rc~$scen~$b")" != 0 ]; then
            res=ERROR
            note=" (a correlation job exited nonzero; diff not attempted)"
        elif info_only_leg "$scen" "$b"; then
            res="$(compare_modes "$scen" "$a" "$b" "$SIDEBAND_EXCLUDE_CHANNELS")"
            if [ "$res" != ERROR ]; then
                note=" (would be $res at $DIFF_THRESHOLD; mean difference $(mean_diff "$scen" "$a" "$b")%; CHANNEL(S) $SIDEBAND_EXCLUDE_CHANNELS EXCLUDED; not checked: sideband handling differs from the CPU's, see cpu:gpucompat)"
                res=INFO
            else
                note=" (diff failed)"
            fi
        else
            res="$(compare_modes "$scen" "$a" "$b")"
            note=" (threshold $DIFF_THRESHOLD)"
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
        case "$r" in PASS|SKIP|INFO) ;; *) overall_status=1 ;; esac
    done
    printf "\n"
done
echo "======================================================="
if [ "$SIDEBAND_INFO_ONLY" = 1 ]; then
    for scen in "${SCENARIOS[@]}"; do
        if is_sideband_scenario "$scen"; then
            echo "*** NOTE: INFO = NOT CHECKED.  On the lower/double sideband scenarios"
            echo "*** cpu:gpu0 and cpu:gpu1 are diffed with channel(s) $SIDEBAND_EXCLUDE_CHANNELS EXCLUDED and"
            echo "*** never PASS or FAIL: the CPU handles sidebands after the FFT.  Their"
            echo "*** CPU-vs-GPU check is cpu:gpucompat."
            break
        fi
    done
fi
echo "Detailed diff logs are under $WORKDIR/<scenario>/diff/"

######## Sideband invariance #################################################

in_scenarios() {
    local s
    for s in "${SCENARIOS[@]}"; do [ "$s" = "$1" ] && return 0; done
    return 1
}

invariance_header=""
for pair in "${INVARIANCE_PAIRS[@]}"; do
    lsbscen="${pair%%:*}"; usbscen="${pair##*:}"
    in_scenarios "$lsbscen" && in_scenarios "$usbscen" || continue
    if [ -z "$invariance_header" ]; then
        echo
        echo "== Sideband invariance (same signal recorded as LSB and as USB) =="
        invariance_header=1
    fi
    refdir="$WORKDIR/$usbscen/cpu/test-$usbscen.difx"
    for mode in gpu0 gpu1 gpucompat cpu; do
        log="$WORKDIR/$lsbscen/diff/${mode}-vs-${usbscen}-cpu.invariance.log"
        mkdir -p "$WORKDIR/$lsbscen/diff"
        if [ "$(state_get "rc~$lsbscen~$mode")" != 0 ] || [ "$(state_get "rc~$usbscen~cpu")" != 0 ]; then
            res=ERROR; note=" (a correlation job did not run or exited nonzero)"
        else
            ( set +u; . "$SETUP_SCRIPT"; set -u; \
              python3 -I "$SYNTHDIR/sideband-invariance.py" -t "$DIFF_THRESHOLD" \
                "$refdir" "$WORKDIR/$lsbscen/$mode/test-$lsbscen.difx" ) > "$log" 2>&1
            res="$(tail -1 "$log" | awk '{print $1}')"
            mean="$(grep -oE "mean percentage absolute difference [0-9.]+" "$log" | awk '{print $NF}')"
            amp="$(grep -oE "test/reference - 1 = [-+0-9.e]+" "$log" | awk '{print $NF}')"
            case "$res" in PASS|FAIL) ;; *) res=ERROR ;; esac
            note=" (mean difference ${mean:-?}%, coherent amplitude ${amp:-?} relative)"
            if [ "$mode" = cpu ] || [ "$mode" = gpucompat ]; then
                [ "$res" != ERROR ] && { note=" (would be $res at $DIFF_THRESHOLD; ${note# (}"; res=INFO; }
            fi
        fi
        echo "  $lsbscen/$mode vs $usbscen/cpu -> $res$note"
        case "$res" in PASS|INFO) ;; *) overall_status=1 ;; esac
    done
done
[ -n "$invariance_header" ] && echo "  (INFO: the CPU's post-FFT sideband handling is not expected to pass; logs in <lsb scenario>/diff/*.invariance.log)"

exit $overall_status
