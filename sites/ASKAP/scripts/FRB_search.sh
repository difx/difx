#!/bin/bash
# Binned FRB correlation searching for peak
# Set:
#     CRAFT_FPGA_DELAY
#     data location
#     RA/Dec
#     binconfig (and check full path for polyco
#     loop in "i" below matches #bins in bincondig

export CRAFT_FPGA_DELAYS=/data/CRAFT/XXXX/fpga.delay

start=`date +%s`

cards=(1)
#cards=(1 2 3 4 5 6 7)
data=/data/CRAFT/FRBXXXXXX/SBXXXX/XXXXXXXXXXXX

RA="XX:XX:XX.X"
DEC="-XX:XX:XX.X"

for c in "${cards[@]}"; do
    processTimeStep.py -t $data --ra $RA  -d${DEC} -f fcm.txt -b 4 --card $c -k --ts=16 -p craftfrb.search.binconfig -S
    pushd data
    for i in {0..29}
    do
	./rundifx2fits -B $i
	bin="$(printf "%02d" $i)"
	mv "CRAFTFR.0.bin00${bin}.source0000.FITS" FRB_B${bin}_CARD${c}.FITS
    done
    popd
done

end=`date +%s`
runtime=$((end-start))
echo "That took $runtime sec"
