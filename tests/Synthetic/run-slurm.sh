#!/bin/bash
#
# run-slurm.sh - SLURM-based CPU-vs-GPU (and optional reference) regression
# harness for the synthetic DiFX tests in this directory.
#
# Unlike run-all.sh (which runs the correlator locally via mpirun and does NOT
# check correctness), this driver correlates each synthetic scenario on a SLURM
# cluster once per mode (cpu, gpu, and optionally a reference DiFX build), then
# uses diffDiFX.py to compare the raw visibility output and reports PASS/FAIL.
# It is intended as the regression safety net for the GPU correlator work.
#
# Usage:
#   ./run-slurm.sh [scenario ...]
#
# With no arguments all scenarios are run (same set as run-all.sh).  Otherwise
# only the named scenarios run, e.g.:  ./run-slurm.sh usb lsb
#
# Configure your cluster by copying slurm.conf.example to slurm.conf and editing
# it (see that file for every setting, including the optional reference leg).

set -u

SYNTHDIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SYNTHDIR"

TMPDIR=$SYNTHDIR
# Per-run state is kept as files under a temp dir (rather than bash-4
# associative arrays) so this works on older bash too.  Keys use '~' as a
# separator, which never appears in scenario/mode names.
STATE="$(mktemp -d "${TMPDIR:-/tmp}/difx-slurm-test.XXXXXX")"
trap 'rm -rf "$STATE"' EXIT
mkdir -p "$STATE"
state_set() { printf '%s' "$2" > "$STATE/$1"; }
state_get() { cat "$STATE/$1" 2>/dev/null; }
state_has() { [ -f "$STATE/$1" ]; }

######## Load configuration ##################################################

if [ -f "$SYNTHDIR/slurm.conf" ]; then
    # shellcheck disable=SC1091
    . "$SYNTHDIR/slurm.conf"
elif [ -f "$SYNTHDIR/slurm.conf.example" ]; then
    echo "WARNING: no slurm.conf found - falling back to slurm.conf.example."
    echo "         Copy it to slurm.conf and edit it for your cluster/account."
    # shellcheck disable=SC1091
    . "$SYNTHDIR/slurm.conf.example"
else
    echo "ERROR: neither slurm.conf nor slurm.conf.example found." >&2
    exit 1
fi

# shellcheck disable=SC1091
. "$SYNTHDIR/gen-sbatch.sh"

# Required settings (no sensible default) - fail early with a clear message.
: "${WORKDIR:?WORKDIR must be set in slurm.conf}"
: "${SETUP_SCRIPT:?SETUP_SCRIPT must be set in slurm.conf}"
: "${SLURM_ACCOUNT:?SLURM_ACCOUNT must be set in slurm.conf}"

# Optional settings - default them so a minimal slurm.conf still works under set -u.
: "${SLURM_PARTITION:=}"
: "${GPU_GRES:=gpu:1}"
: "${REFERENCE_SETUP_SCRIPT:=}"
: "${DIFF_THRESHOLD:=0.0005}"
: "${CPU_NTASKS:=7}";       : "${CPU_TIME:=10:00}";       : "${CPU_MEM_PER_CPU:=3000}"
: "${GPU_NTASKS:=7}";       : "${GPU_TIME:=10:00}";       : "${GPU_MEM_PER_CPU:=4000}"
: "${REFERENCE_NTASKS:=7}"; : "${REFERENCE_TIME:=10:00}"; : "${REFERENCE_MEM_PER_CPU:=3000}"

######## Scenarios and modes #################################################

# NOTE: each scenario needs ntasks >= (its antenna count + 2); the multi
# scenario (5 stations) needs >= 7. prep_mode now enforces this per scenario
# and refuses to queue a job whose configured ntasks is too small, so a
# mis-sized slurm.conf fails immediately instead of after a queue wait.
ALL_SCENARIOS=(usb lsb usb-complex lsb-complex usb-dsb lsb-dsb complex-complex multi dualpol)

if [ "$#" -gt 0 ]; then
    SCENARIOS=("$@")
else
    SCENARIOS=("${ALL_SCENARIOS[@]}")
fi

# cpu and gpu always; reference only if a reference setup script is configured.
MODES=(cpu gpu)
COMPARISONS=("cpu:gpu")
if [ -n "${REFERENCE_SETUP_SCRIPT:-}" ]; then
    MODES+=(reference)
    COMPARISONS+=("cpu:reference" "gpu:reference")
fi

echo "Scenarios : ${SCENARIOS[*]}"
echo "Modes     : ${MODES[*]}"
echo "Workdir   : $WORKDIR"
echo

######## Ensure synthetic VDIF data exists ###################################
# Check the specific VDIF files each SELECTED scenario needs (parsed from its
# v2d), not a fixed pair - otherwise a newly-added scenario's data (e.g. the
# multi files) is silently missing and its jobs fail at correlation time.

data_missing=0
for scen in "${SCENARIOS[@]}"; do
    scen_v2d="$SYNTHDIR/test-$scen.v2d"
    [ -f "$scen_v2d" ] || continue
    for vdif in $(awk -F= '/^[[:space:]]*file[[:space:]]*=/{gsub(/[[:space:]]/,"",$2);print $2}' "$scen_v2d"); do
        if [ ! -f "$SYNTHDIR/$vdif" ]; then
            echo "Missing VDIF data for '$scen': $vdif" >&2
            data_missing=1
        fi
    done
done
if [ "$data_missing" = 1 ]; then
    echo "Synthetic VDIF data missing - generating with createData.sh ..."
    ( cd "$SYNTHDIR" && ./createData.sh )
fi

######## Per-scenario, per-mode preparation ##################################
# Build a self-contained job directory per (scenario, mode) and generate the
# .input/.calc/.im locally, using that mode's own DiFX build so the reference
# leg is produced entirely by the reference tools.

prep_mode() {
    local scen="$1" mode="$2"
    local expname="test-$scen"
    local v2d="${expname}.v2d"
    local jobdir="$WORKDIR/$scen/$mode"

    local setup="$SETUP_SCRIPT"
    [ "$mode" = "reference" ] && setup="$REFERENCE_SETUP_SCRIPT"

    if [ ! -f "$SYNTHDIR/$v2d" ]; then
        echo "  [$scen/$mode] ERROR: $v2d not found" >&2
        return 1
    fi

    rm -rf "$jobdir"
    mkdir -p "$jobdir"

    # Copy the control files and symlink only the VDIF files this scenario uses.
    cp "$SYNTHDIR/$v2d" "$jobdir/"
    local vex
    vex="$(awk -F= '/^[[:space:]]*vex[[:space:]]*=/{gsub(/[[:space:]]/,"",$2);print $2}' "$SYNTHDIR/$v2d")"
    cp "$SYNTHDIR/$vex" "$jobdir/"
    local vdif
    for vdif in $(awk -F= '/^[[:space:]]*file[[:space:]]*=/{gsub(/[[:space:]]/,"",$2);print $2}' "$SYNTHDIR/$v2d"); do
        ln -sf "$SYNTHDIR/$vdif" "$jobdir/$vdif"
    done

    # Generate .input/.calc/.im in a subshell using this mode's DiFX build.
    # DiFX setup scripts routinely reference variables they haven't set yet
    # (e.g. appending to an empty PATH-like variable), so drop nounset while
    # sourcing them; the subshell keeps set -e for the actual commands.
    if ( set -e; set +u; . "$setup"; set -u; cd "$jobdir"; \
         vex2difx "$v2d"; difxcalc "${expname}.calc" ) \
         > "$jobdir/prep.log" 2>&1; then
        # Refuse NOW (before this job is queued) if the configured ntasks is too
        # small: mpifxcorr needs 1 manager + N datastreams + 1 core =
        # (ACTIVE DATASTREAMS + 2) ranks. A job submitted with fewer only waits
        # in the SLURM queue to fail at startup - so fail fast here instead.
        local nds required modentasks confvar
        nds="$(awk '/^ACTIVE DATASTREAMS/{print $3}' "$jobdir/${expname}.input")"
        case "$mode" in
            cpu)       modentasks="$CPU_NTASKS";       confvar="CPU_NTASKS" ;;
            gpu)       modentasks="$GPU_NTASKS";       confvar="GPU_NTASKS" ;;
            reference) modentasks="$REFERENCE_NTASKS"; confvar="REFERENCE_NTASKS" ;;
        esac
        if [ -z "$nds" ]; then
            echo "  [$scen/$mode] ERROR: could not read ACTIVE DATASTREAMS from ${expname}.input" >&2
            return 1
        fi
        required=$(( nds + 2 ))
        if [ "$required" -gt "$modentasks" ]; then
            echo "  [$scen/$mode] ERROR: $confvar=$modentasks is too small - '$scen' has $nds active datastreams and needs ntasks >= $required (1 manager + $nds datastreams + 1 core). Raise $confvar in slurm.conf and re-run." >&2
            return 1
        fi
        write_sbatch "$mode" "$expname" "$jobdir" >/dev/null
        state_set "prep~$scen~$mode" 1
        echo "  [$scen/$mode] prepared (ntasks=$modentasks, needs $required)"
    else
        echo "  [$scen/$mode] ERROR: vex2difx/difxcalc failed (see $jobdir/prep.log)" >&2
    fi
}

echo "== Preparing job directories =="
for scen in "${SCENARIOS[@]}"; do
    for mode in "${MODES[@]}"; do
        prep_mode "$scen" "$mode"
    done
done
echo

######## Submit all jobs concurrently and wait ###############################
# Parallel indexed arrays (bash 3.2 compatible) map background sbatch --wait
# PIDs to their scenario/mode; exit codes are stashed in the state store.

job_pids=()
job_keys=()

echo "== Submitting SLURM jobs (sbatch --wait) =="
for scen in "${SCENARIOS[@]}"; do
    for mode in "${MODES[@]}"; do
        state_has "prep~$scen~$mode" || continue
        jobdir="$WORKDIR/$scen/$mode"
        rm -rf "$jobdir/test-$scen.difx"
        sbatch --wait "$jobdir/slurm-$mode.sh" > "$jobdir/sbatch.out" 2>&1 &
        job_pids+=($!)
        job_keys+=("$scen~$mode")
        echo "  [$scen/$mode] submitted"
    done
done

echo "Waiting for all jobs to complete ..."
i=0
while [ "$i" -lt "${#job_pids[@]}" ]; do
    wait "${job_pids[$i]}"
    rc=$?
    state_set "rc~${job_keys[$i]}" "$rc"
    scen="${job_keys[$i]%%~*}"
    mode="${job_keys[$i]##*~}"
    if [ "$rc" -eq 0 ]; then
        echo "  [$scen/$mode] job completed"
    else
        echo "  [$scen/$mode] job FAILED (sbatch --wait rc=$rc):" \
             "check $WORKDIR/$scen/$mode/sbatch.out and ${mode}.mpilog" >&2
    fi
    i=$((i + 1))
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
    # "At the end, N records disagreed on the header"
    local hdr
    hdr="$(grep -oE "At the end, [0-9]+ records disagreed" "$log" | grep -oE "[0-9]+" | head -1)"
    if [ -n "$hdr" ] && [ "$hdr" -gt 0 ]; then
        echo FAIL; return
    fi
    # "After N records, the mean percentage ..." - require at least one record
    local recs
    recs="$(grep -oE "After [0-9]+ records, the mean" "$log" | grep -oE "[0-9]+" | head -1)"
    if [ -z "$recs" ] || [ "$recs" -le 0 ]; then
        echo ERROR; return
    fi
    echo PASS
}

# compare_modes <scen> <modeA> <modeB> -> echoes overall PASS/FAIL/ERROR,
# diffing every matching DIFX_* visibility file between the two mode dirs.
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
        # If either job errored/failed to produce output, the comparison errors.
        if [ "$(state_get "rc~$scen~$a")" != 0 ] || [ "$(state_get "rc~$scen~$b")" != 0 ]; then
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
        [ "$r" = PASS ] || overall_status=1
    done
    printf "\n"
done
echo "======================================================="
echo "Detailed diff logs are under $WORKDIR/<scenario>/diff/"

exit $overall_status
