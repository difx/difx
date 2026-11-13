#!/bin/bash
#
# createLongBaseline.sh - simulate the data for the lb-* scenarios
# (test-lb-usb, test-lb-lsb, test-lb-usb-complex, test-lb-lsb-complex).
#
# T1 is the usual station (real USB, 200-204 MHz); T2 is about 3200 km away
# (near the MRO) and records the same band as real USB, real LSB, complex USB
# or complex LSB.  Unlike createData.sh, the data are simulated with each
# antenna's geometric delay (utilities/vdifsim), so the baseline has fringes.
#
# vdifsim applies the fine delay once per chunk of one FFT length of the job it
# reads, about the lower band edge: the same approximation the correlator
# makes.  To keep the simulation well below the correlator's error, the data
# are simulated from a job with 64 channels (vdifsim then works in chunks of
# 256 samples, 32 us; see vdifsim.log) and correlated at 4096 channels.
#
# vdifsim gives each DiFX frequency entry its own sky signal, so T2's LSB data
# cannot be simulated as a 204 MHz LSB band in the same job as T1's 200 MHz
# USB band.  Instead T2 is simulated as USB and vdif2bit_flip_odd.py negates
# every odd sample (real: (-1)^n, i.e. LSB; complex: conjugation, i.e. LSB).
# That is exactly what vdifsim does for LSB (checked bit for bit, 2026-11-13),
# so lb-lsb and lb-usb contain the same signal, and a correct correlator gives
# them the same visibilities (compare with sideband-invariance.py).
#
# Signal level: the correlation coefficient is about 0.010 (mid-band, after
# 2-bit quantisation), a weak source as in most VLBI data, so the cross-
# correlations are noise-dominated.  It is set through a vdifsim config file
# (FluxDensity and per-antenna SEFD below; calibrated by simulating and
# correlating, 2026-11-13).  With a config file, vdifsim builds the sky signal
# from the config's spectral shaping, which is zero unless FluxDensity is given,
# and FluxDensity there is not the same scale as vdifsim's built-in 100 Jy
# default, so only the measured coefficient means anything.  (Without a config
# file - vdifsim's defaults, 100 Jy against SEFD 500 - the coefficient is 0.25.)
#
# Needs a sourced DiFX setup with vdifsim installed (utilities/vdifsim is not
# built by install-difx; build it with autoreconf -fi && configure && make).
# Output: TESTLB1.vdif, TESTLB2-{usb,lsb,complex-usb,complex-lsb}.vdif here.
# Work files go under local-runs/lb-sim/.

set -eu

SYNTHDIR="$(cd "$(dirname "$0")" && pwd)"
SIMDIR="$SYNTHDIR/local-runs/lb-sim"
SEED=12345
FLUX=10      # vdifsim config FluxDensity
SEFD=1750    # both antennas; with FLUX=10 gives a correlation coefficient of 0.010

command -v vdifsim >/dev/null || { echo "vdifsim not found on PATH" >&2; exit 1; }

# simulate <kind> <scenario> <T2 file> <T2 format>
simulate() {
    local kind="$1" scen="$2" t2file="$3" t2format="$4"
    local dir="$SIMDIR/$kind" job="sim$kind"
    mkdir -p "$dir"
    cp "$SYNTHDIR/test-$scen.vex" "$dir/$job.vex"
    sed -e "s/^vex=.*/vex=$job.vex/" \
        -e 's/nChan=4096/nChan=64/' \
        -e "s|file = TESTLB1.vdif|file = TESTLB1.vdif\n format = INTERLACEDVDIF/0/8032/2|" \
        -e "s|file = $t2file|file = $t2file\n format = $t2format/0/8032/2|" \
        "$SYNTHDIR/test-$scen.v2d" > "$dir/$job.v2d"
    cp "$SYNTHDIR/threads" "$dir/" 2>/dev/null || true
    printf 'FluxDensity %s\n[T1]\nSEFD %s\n[T2]\nSEFD %s\n' "$FLUX" "$SEFD" "$SEFD" > "$dir/sim.config"
    ( cd "$dir"
      vex2difx "$job.v2d" > prep.log 2>&1
      difxcalc "$job.calc" >> prep.log 2>&1
      mpirun -np 1 vdifsim --force --seed "$SEED" --config sim.config "$job.input" > vdifsim.log 2>&1 )
    echo "simulated $kind: $dir"
}

simulate real    lb-usb         TESTLB2-usb.vdif         INTERLACEDVDIF
simulate complex lb-usb-complex TESTLB2-complex-usb.vdif INTERLACEDVDIFC

# The same seed and model give T1 identical data in both jobs.
cmp "$SIMDIR/real/TESTLB1.vdif" "$SIMDIR/complex/TESTLB1.vdif"

mv -f "$SIMDIR/real/TESTLB1.vdif" "$SIMDIR/real/TESTLB2-usb.vdif" \
      "$SIMDIR/complex/TESTLB2-complex-usb.vdif" "$SYNTHDIR/"
python3 -I "$SYNTHDIR/vdif2bit_flip_odd.py" "$SYNTHDIR/TESTLB2-usb.vdif" "$SYNTHDIR/TESTLB2-lsb.vdif" 8032
python3 -I "$SYNTHDIR/vdif2bit_flip_odd.py" "$SYNTHDIR/TESTLB2-complex-usb.vdif" "$SYNTHDIR/TESTLB2-complex-lsb.vdif" 8032
ls -l "$SYNTHDIR"/TESTLB*.vdif
