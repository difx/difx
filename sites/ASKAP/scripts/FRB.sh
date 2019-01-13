#!/bin/bash
# Correlated FRB gated to arrival time 
k
# Set:
#     CRAFT_FPGA_DELAY
#     data location
#     RA/Dec
#     binconfig (and check full path for polyco
#     Output FITS file (mv /data/CRAFTFR.0....) 

export CRAFT_FPGA_DELAYS=/data/CRAFT/XXX/XXX/fpga.delay

start=`date +%s`

#cards=(1)
cards=(1 2 3 4 5 6 7)
data=/data/CRAFT/FRBXXX/XXXXX

RA="XX:XX:XX"
DEC="-XX:XX:XX"

for c in "${cards[@]}"; do
    processTimeStep.py -t $data --ra $RA  -d${DEC} -f fcm.txt -b 4 --card $c -k --ts=16 -p gate.binconfig
    mv data/CRAFTFR.0.bin0000.source0000.FITS data/FRBXXX_CARD${c}.FITS
done

end=`date +%s`
runtime=$((end-start))
echo "That took $runtime sec"




 
