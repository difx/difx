#!/bin/bash

card=c6
fpga=f0
path="../../../../ban115/SB6358/mode3"

for f in $@ ; do
    dir=`basename $f`
    [ -d $dir ] || mkdir $dir
    pushd $dir > /dev/null

    vcraft2obs.py --ra 19:39:25.0264940 --dec " -63:42:45.626590" "$path/$dir/ak*/beam00/ak*_${card}_${fpga}.vcraft" 

    antenna=`ls -m $path/$dir/ | perl -pe  's/\s//g'`
    echo "Using antenna $antenna"

    firstAnt=$(echo $antenna | cut -d, -f 1)
    framesize=$(codifsum -v ${firstAnt}.codif | sed -n 2p | cut -d/ -f 3)
    bits=$(codifsum -v ${firstAnt}.codif | sed -n 2p | cut -d/ -f 4)
    \rm fcm.txt
    cp ../../fcm.txt .
    askap2difx.py fcm.txt obs.txt chandefs.txt --ants=$antenna -b $bits -n $framesize
    ./run.sh
    \rm -rf craftfrbD2D.difx
    mergeOversampledDiFX.py craftfrb.stitchconfig craftfrb.difx
    difx2fits craftfrbD2D
    popd > /dev/null
done

