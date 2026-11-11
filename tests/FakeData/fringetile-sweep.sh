#!/usr/bin/env bash
# fringetile-sweep.sh - build and run fringetile-sweep.cu on both paths, then
# join the two tables so each shape shows untiled us, tiled us, the speedup, and
# whether the two paths produced bit-identical output.
#
# Equal hashes are the all-shapes correctness check (the two paths do the same
# arithmetic in the same order, so anything else is a mis-indexed tile); the
# times are the shape-independence check that gates landing the tiled path.
#
# Usage: ./fringetile-sweep.sh [nBufferedFFTs] [nreps]
#
# Runs on the desktop and on the cluster. Nothing here is machine-specific by
# default - the three things that differ are env-overridable:
#
#   SWEEP_SETUP    DiFX setup script to source. Default: ../../setup.bash on a
#                  machine with no SLURM, $HOME/setup_gpudifx.claude where sbatch
#                  exists (the same path the sbatch scripts here use).
#   SWEEP_NVCC     nvcc to build with. Default: whatever is on PATH after the
#                  setup, else $CUDAROOT/bin/nvcc. On OzSTAR that means the
#                  setup script (or `module load cuda`) has to put nvcc on PATH -
#                  this script will say so rather than guessing a path.
#   SWEEP_OUTDIR   where the binary, the two tables and the pcal dumps go.
#                  Default: this directory on a desktop; on the cluster a scratch
#                  dir under /fred, so a shared checkout is never written to.
#
# On OzSTAR it needs a GPU, so run it under srun:
#   srun --gres=gpu:1 --time=15 --mem=8000 --account=oz168 ./fringetile-sweep.sh 2500 30
set -u
SRCROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$SRCROOT"

# --- machine-specific bits, all overridable ---------------------------------
ON_CLUSTER=0
command -v sbatch >/dev/null 2>&1 && ON_CLUSTER=1

if [ -n "${SWEEP_SETUP:-}" ]; then
    SETUP="$SWEEP_SETUP"
elif [ "$ON_CLUSTER" = 1 ]; then
    SETUP="$HOME/setup_gpudifx.claude"
else
    SETUP="$SRCROOT/../../setup.bash"
fi
if [ -r "$SETUP" ]; then
    set +u; . "$SETUP" >/dev/null 2>&1; set -u
    echo "[fringetile-sweep] sourced $SETUP"
else
    echo "[fringetile-sweep] WARNING: no setup script at $SETUP (set SWEEP_SETUP); relying on the current environment"
fi

if [ -n "${SWEEP_OUTDIR:-}" ]; then
    OUTDIR="$SWEEP_OUTDIR"
elif [ "$ON_CLUSTER" = 1 ]; then
    OUTDIR="/fred/oz168/$USER/claude/testing/fringetile-sweep"
else
    OUTDIR="$SRCROOT"
fi
mkdir -p "$OUTDIR" || { echo "[fringetile-sweep] cannot create $OUTDIR (set SWEEP_OUTDIR)"; exit 1; }

NVCC="${SWEEP_NVCC:-$(command -v nvcc 2>/dev/null || true)}"
[ -x "${NVCC:-}" ] || NVCC="${CUDAROOT:+$CUDAROOT/bin/nvcc}"
if [ ! -x "${NVCC:-}" ]; then
    echo "[fringetile-sweep] no nvcc found. Put it on PATH (on OzSTAR: module load cuda),"
    echo "                   or set SWEEP_NVCC=/path/to/nvcc, or CUDAROOT." >&2
    exit 1
fi
# -----------------------------------------------------------------------------

SRCDIR="$SRCROOT/../../mpifxcorr/src"
BIN="$OUTDIR/fringetile-sweep"
NBUF="${1:-10}"
NREPS="${2:-20}"

# Same flags as the mpifxcorr build's .cu rule (see mpifxcorr/src/Makefile.am),
# so what is measured here is what ships.
#
# The -arch comes from the GPU this is about to run on, NOT from NVCCFLAGS: a
# cluster setup script that inherited the desktop's `-arch=sm_75` would build a
# cubin the A100 cannot launch ("no kernel image is available"), which is a
# wasted allocation and a confusing error. Order: SWEEP_ARCH, then the device's
# own compute capability, then NVCCFLAGS, then -arch=native.
INC="-I$SRCDIR${DIFXROOT:+ -I$DIFXROOT/include}"
if [ -n "${SWEEP_ARCH:-}" ]; then
    ARCH="$SWEEP_ARCH"
else
    cap=$(nvidia-smi --query-gpu=compute_cap --format=csv,noheader 2>/dev/null | head -1 | tr -d '. ')
    if [ -n "$cap" ]; then
        ARCH="-arch=sm_$cap"
        echo "[fringetile-sweep] this GPU is sm_$cap"
    else
        ARCH="${NVCCFLAGS:--arch=native}"
        echo "[fringetile-sweep] no GPU visible (login node?); building with $ARCH - run under srun on a GPU node"
    fi
fi
echo "[fringetile-sweep] building with $(basename "$NVCC") $ARCH -> $BIN"
if ! "$NVCC" $ARCH -O2 $INC -o "$BIN" "$SRCROOT/fringetile-sweep.cu"; then
    if [ "$ARCH" = "-arch=native" ]; then
        echo "[fringetile-sweep] retrying with -arch=sm_80 (nvcc may predate -arch=native)"
        "$NVCC" -arch=sm_80 -O2 $INC -o "$BIN" "$SRCROOT/fringetile-sweep.cu" || exit 1
    else
        exit 1
    fi
fi

# Results (tables, pcal dumps) belong with the run, not in the checkout.
cd "$OUTDIR"
echo "[fringetile-sweep] results in $OUTDIR"

# SWEEP_COMPLEX and SWEEP_PCAL are passed through, so the same script covers the
# complex twin and the DOPCAL path.
# Stale dumps from an earlier SWEEP_PCAL=1 run would otherwise be compared (and
# reported as a pass) by a run that never touched the pcal path.
rm -f pcal-*.f32

# Tag every output with the sampling mode, so a complex run cannot overwrite a
# real one's raw tables (the joined .log was already distinct; these were not).
TAG="${SWEEP_COMPLEX:+-complex}"
echo "[fringetile-sweep] running untiled ..."
DIFX_GPU_FRINGE_TILE=0 "$BIN" "$NBUF" "$NREPS" > "fringetile-sweep$TAG-untiled.txt" || exit 1
echo "[fringetile-sweep] running tiled ..."
DIFX_GPU_FRINGE_TILE=1 "$BIN" "$NBUF" "$NREPS" > "fringetile-sweep$TAG-tiled.txt" || exit 1

python3 - "fringetile-sweep$TAG-untiled.txt" "fringetile-sweep$TAG-tiled.txt" <<'PY' | tee "fringetile-sweep$TAG.log"
import sys
def rows(fn):
    out = []
    for line in open(fn):
        if line.startswith('#') or not line.strip():
            if line.startswith('# device'): out.append(('HDR', line.strip()))
            continue
        f = line.split()
        out.append((f[0]+'x'+f[1], float(f[2]), f[3], ' '.join(f[4:])))
    return out
u, t = rows(sys.argv[1]), rows(sys.argv[2])
for tag, line in [r for r in u if r[0]=='HDR'] + [r for r in t if r[0]=='HDR']:
    print(line)
u = [r for r in u if r[0]!='HDR']; t = [r for r in t if r[0]!='HDR']
print(f"\n{'bands x chan':>14} {'untiled us':>11} {'tiled us':>10} {'speedup':>8} {'output':>10}   why")
worst, allmatch = None, True
for a, b in zip(u, t):
    assert a[0] == b[0], (a[0], b[0])
    sp = a[1]/b[1]
    same = 'identical' if a[2] == b[2] else 'DIFFERENT'
    if same == 'DIFFERENT': allmatch = False
    if worst is None or sp < worst[1]: worst = (a[0], sp)
    print(f"{a[0]:>14} {a[1]:>11.1f} {b[1]:>10.1f} {sp:>7.2f}x {same:>10}   {a[3]}")
print(f"\nworst shape: {worst[0]} at {worst[1]:.2f}x;  output equivalence: "
      f"{'ALL SHAPES BIT-IDENTICAL' if allmatch else 'MISMATCH - see table'}")

# Phase cal, when SWEEP_PCAL=1: accumulated with atomicAdd, so the tiling changes
# the summation order and the bins are compared with a tolerance, not by hash.
# A mis-indexed pcal write would move energy between bins and blow any tolerance.
import os, struct, glob
pc = sorted(glob.glob('pcal-*-untiled-*.f32'))
if pc:
    print()
    worstrel, bad = 0.0, []
    for u in pc:
        t = u.replace('-untiled-', '-tiled-')
        if not os.path.exists(t): bad.append((u, 'no tiled counterpart')); continue
        a = struct.unpack(f'{os.path.getsize(u)//4}f', open(u,'rb').read())
        b = struct.unpack(f'{os.path.getsize(t)//4}f', open(t,'rb').read())
        if len(a) != len(b): bad.append((u, 'size mismatch')); continue
        scale = max(max(abs(x) for x in a), 1e-30)
        rel = max(abs(x-y) for x, y in zip(a, b)) / scale
        worstrel = max(worstrel, rel)
        if rel > 1e-5: bad.append((u, f'rel {rel:.2e}'))
    shapes = len(pc)
    print(f"pcal bins compared over {shapes} shapes: worst relative difference "
          f"{worstrel:.2e} " + ("(within 1e-5: PASS)" if not bad else f"FAIL {bad}"))
PY
