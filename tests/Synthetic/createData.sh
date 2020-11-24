#!/usr/bin/sh

TSP=""
TSP_SLOTS=9
if command -v tsp > /dev/null 2>&1
then
    echo "Using TaskSpooler to parallelise"
    tsp -S $TSP_SLOTS
    TSP="tsp"
fi

export DURATION=1200

export SEED1=38573
export SEED2=58573

# USB Real 
$TSP generateVDIF -seed=$SEED1 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.5 -dayno 100 -time 07:00:00 TEST1.vdif
$TSP generateVDIF -seed=$SEED2 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -dayno 100 -time 07:00:00 TEST2-usb.vdif

# LSB Real
$TSP generateVDIF -seed=$SEED2 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -dayno 100 -time 07:00:00 -lsb TEST2-lsb.vdif

# Complex (single side band)

$TSP generateVDIF -seed=$SEED1 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.5 -dayno 100 -time 07:00:00 -complex      TEST1-complex-usb.vdif
$TSP generateVDIF -seed=$SEED2 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -dayno 100 -time 07:00:00 -hilbert      TEST2-complex-usb.vdif
$TSP generateVDIF -seed=$SEED2 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -dayno 100 -time 07:00:00 -hilbert      TEST2-complex-usb.vdif
$TSP generateVDIF -seed=$SEED2 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -dayno 100 -time 07:00:00 -hilbert -lsb TEST2-complex-lsb.vdif

# Complex (double side band)

$TSP generateVDIF -seed=$SEED2 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -dayno 100 -time 07:00:00 -hilbert -doublesideband      TEST2-dsb-usb.vdif
$TSP generateVDIF -seed=$SEED2 -w 4 -b 2 -C 1  -l ${DURATION} -noise -amp2 0.05 -tone2 1.0 -dayno 100 -time 07:00:00 -hilbert -doublesideband -lsb TEST2-dsb-lsb.vdif


if command -v tsp > /dev/null 2>&1
then
    echo "Waiting for TaskSpooler to finish"
    tsp -w
fi
