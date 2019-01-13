#!/bin/bash
# FRB "Continuum" data
# Set:
#     CRAFT_FPGA_DELAY
#     data location
#     RA/Dec of beam centre (not FRB location)

export CRAFT_FPGA_DELAYS=/data/CRAFT/XXXX/XXXXXX/fpga.delay

start=`date +%s`

RA="XX:XX:XX.X"
DEC="-XX:XX:XX.X"

cards=(1 2 3 4 5 6 7)
data=/data/CRAFT/FRBXXXXXX/SBXXXX/XXXXXXXX

for c in "${cards[@]}"; do
    processTimeStep.py -t $data --ra $RA -d${DEC} -f fcm.txt -b 4 --card $c -k --ts=16
    mv data/CRAFTFR.0.bin0000.source0000.FITS data/FRBCONT_CARD${c}.FITS
done

end=`date +%s`
runtime=$((end-start))
echo "That took $runtime sec"
