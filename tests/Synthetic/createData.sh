#!/usr/bin/sh

TSP=""
TSP_SLOTS=9
if command -v tsp > /dev/null 2>&1
then
    echo "Using TaskSpooler to parallelise"
    tsp -S $TSP_SLOTS
    TSP="tsp"
fi

export DURATION=50

export SEED1=38573
export SEED2=58573
export SEED3=48573
export SEED4=68573
export SEED5=78573

# USB Real 
$TSP generateVDIF -seed=$SEED1 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.5 -year 2020 -dayno 100 -time 07:00:00 TEST1.vdif
$TSP generateVDIF -seed=$SEED2 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 TEST2-usb.vdif

# LSB Real
$TSP generateVDIF -seed=$SEED2 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -lsb TEST2-lsb.vdif

# Complex (single side band)

$TSP generateVDIF -seed=$SEED1 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.5 -year 2020 -dayno 100 -time 07:00:00 -complex      TEST1-complex-usb.vdif
$TSP generateVDIF -seed=$SEED2 -B 32 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -hilbert      TEST2-complex-usb.vdif
$TSP generateVDIF -seed=$SEED2 -B 32 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -hilbert      TEST2-complex-usb.vdif
$TSP generateVDIF -seed=$SEED2 -B 32 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -hilbert -lsb TEST2-complex-lsb.vdif

# Complex (double side band)

$TSP generateVDIF -seed=$SEED2 -B 32 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -hilbert -doublesideband      TEST2-dsb-usb.vdif
$TSP generateVDIF -seed=$SEED2 -B 32 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -hilbert -doublesideband -lsb TEST2-dsb-lsb.vdif

# Dual polarisation (2 x 4 MHz USB at the SAME sky frequency, R and L), 2
# stations (test-dualpol scenario). The two generated channels become the two
# polarisations, so the scenario yields 4 pol products and is the only one that
# exercises the cross-polarisation autocorrelations - on either the CPU or the
# GPU path. Different seeds per station, as elsewhere.
$TSP generateVDIF -seed=$SEED1 -w 4 -b 2 -C 2  -l ${DURATION} -noise -amp2 0.05 -tone2 1.5 -year 2020 -dayno 100 -time 07:00:00 TEST1-dualpol.vdif
$TSP generateVDIF -seed=$SEED2 -w 4 -b 2 -C 2  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 TEST2-dualpol.vdif

# Multi-subband (4 x 4 MHz USB), 5 stations (test-multi scenario)

$TSP generateVDIF -seed=$SEED1 -w 4 -b 2 -C 4  -l ${DURATION} -noise -amp2 0.05 -tone2 1.5 -year 2020 -dayno 100 -time 07:00:00 TEST1-multi.vdif
$TSP generateVDIF -seed=$SEED2 -w 4 -b 2 -C 4  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 TEST2-multi.vdif
$TSP generateVDIF -seed=$SEED3 -w 4 -b 2 -C 4  -l ${DURATION} -noise -amp2 0.05 -tone2 0.8 -year 2020 -dayno 100 -time 07:00:00 TEST3-multi.vdif
$TSP generateVDIF -seed=$SEED4 -w 4 -b 2 -C 4  -l ${DURATION} -noise -amp2 0.05 -tone2 1.2 -year 2020 -dayno 100 -time 07:00:00 TEST4-multi.vdif
$TSP generateVDIF -seed=$SEED5 -w 4 -b 2 -C 4  -l ${DURATION} -noise -amp2 0.05 -tone2 2.0 -year 2020 -dayno 100 -time 07:00:00 TEST5-multi.vdif


if command -v tsp > /dev/null 2>&1
then
    echo "Waiting for TaskSpooler to finish"
    tsp -w
fi
