#!/bin/bash
#
# $Id: chk_ff_3372.sh 870 2013-10-07 18:21:50Z rjc $
#
# modified version of chk_ff_3372.sh which turns off some data
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`cd $srcdir/testdata; pwd`

os=`uname -s` || os=idunno
ahffile=`pwd`/cf3372ah.flag
# day 193 17:57:25 for 40s = 192*86400 + 17*3600 + 57*60 = 16739820 .. 60
# divide by 86400 subtract 1
cat > $ahffile <<EOF
* (192*86400 + 17*3600 + 57*60 + 25)/86400.0
192.74820601851851851851 FF
* (192*86400 + 17*3600 + 57*60 + 30)/86400.0
192.74826388888888888888 80
* (192*86400 + 17*3600 + 57*60 + 35)/86400.0
192.74832175925925925925 FF
* (192*86400 + 17*3600 + 57*60 + 40)/86400.0
192.74837962962962962962 80
* (192*86400 + 17*3600 + 57*60 + 45)/86400.0
192.74843750000000000000 FF
* (192*86400 + 17*3600 + 57*60 + 50)/86400.0
192.74849537037037037037 80
* (192*86400 + 17*3600 + 57*60 + 55)/86400.0
192.74855324074074074074 FF
* (192*86400 + 17*3600 + 57*60 + 60)/86400.0
192.74861111111111111111 80
* (192*86400 + 17*3600 + 57*60 + 65)/86400.0
192.74866898148148148148 FF
EOF
echo adhoc_flag_file $ahffile > ./cf3372df
grep -v $os $DATADIR/3372/cf3372 >> ./cf3372df

$verb && msg=$((2 - $testverb)) || msg=2

rm -f ff-3372df.ps
$verb && echo \
fourfit -m$msg -t -d diskfile:ff-3372df.ps -b TV:X \\ && echo \
    -c ./cf3372df \\ && echo \
    $DATADIR/3372/193-1757/0529+483.vtqbsq

fourfit -m$msg -t -d diskfile:ff-3372df.ps -b TV:X \
    -c ./cf3372df \
    $DATADIR/3372/193-1757/0529+483.vtqbsq 2>ff-3372df.out 1>&2
[ -f ./ff-3372df.ps ] || { echo ./ff-3372df.ps missing && exit 2 ; }

# pluck out line containing the snr and parse it
line=$(grep '7570 9653' ./ff-3372df.ps)

IFS='()'
read a snr b <<<"$line"

# snr bounds
low=102.3
high=102.5
aok=$(echo "$snr>$low && $snr<$high" | bc)
$verb && echo aok is $aok and "$low < $snr < $high" is expected from: $line

[ "$aok" -gt 0 ]

#
# eof
#
