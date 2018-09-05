#!/bin/bash
#
# $Id: chk_ff_dump.sh 2430 2018-07-05 13:53:31Z gbc $
#
# canonical test suite for fourfit
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`cd $srcdir/testdata; pwd`

# clean slate
rm -rf pdd2843
rm -f ff-2843pdd.ps ff-2843cpy.ps ff-2843fil.ps

# step one, process normally
$verb && echo \
fourfit -t -d diskfile:ff-2843pdd.ps -b AI:S \\ && echo \
    $DATADIR/2843/321-1701_0552+398/0552+398.oifhak \\ && echo \
    set start -3 plot_data_dir pdd2843
fourfit -t -d diskfile:ff-2843pdd.ps -b AI:S \
    $DATADIR/2843/321-1701_0552+398/0552+398.oifhak \
    set start -3 plot_data_dir pdd2843 2>/dev/null 1>&2
mv pdd2843/321-1701_0552+398-AI-S-RR.oifhak \
   pdd2843/321-1701_0552+398-AI-S-RR.oifhak.original

# step two, make a filter
$verb && echo \
HOPS_PLOT_DATA_MASK=0x01000000 HOPS_AMP_SEG_FILTER=15.0 \\ && echo \
fourfit -t -d diskfile:ff-2843cpy.ps -b AI:S \\ && echo \
    $DATADIR/2843/321-1701_0552+398/0552+398.oifhak \\ && echo \
    set start -3 plot_data_dir pdd2843
# original amp is 21.664
# discard segments with amp < 18
HOPS_PLOT_DATA_MASK=0x01000000 HOPS_AMP_SEG_FILTER=18.0 \
fourfit -t -d diskfile:ff-2843cpy.ps -b AI:S \
    $DATADIR/2843/321-1701_0552+398/0552+398.oifhak \
    set start -3 plot_data_dir pdd2843 2>/dev/null 1>&2
sed '/SEG_AMP_FILTER/,/END_AMP_FILTER/!d' \
   pdd2843/321-1701_0552+398-AI-S-RR.oifhak > \
   pdd2843/adhoc_flag_file

[ -s pdd2843/adhoc_flag_file ] || {
    echo no pdd2843/adhoc_flag_file && exit 3; }

# step three, use the filter
$verb && echo \
fourfit -t -d diskfile:ff-2843fil.ps -b AI:S \\ && echo \
    $DATADIR/2843/321-1701_0552+398/0552+398.oifhak \\ && echo \
    set start -3 adhoc_flag_file pdd2843/adhoc_flag_file
fourfit -t -d diskfile:ff-2843fil.ps -b AI:S \
    $DATADIR/2843/321-1701_0552+398/0552+398.oifhak \
    set start -3 adhoc_flag_file pdd2843/adhoc_flag_file 2>/dev/null 1>&2

[ -f ./ff-2843pdd.ps ] || { echo ./ff-2843pdd.ps missing && exit 2 ; }
[ -f ./ff-2843cpy.ps ] || { echo ./ff-2843cpy.ps missing && exit 2 ; }
[ -f ./ff-2843fil.ps ] || { echo ./ff-2843fil.ps missing && exit 2 ; }

[ -d pdd2843 ] || { echo no pdd2843 dir && exit 3; }
[ -s pdd2843/321-1701_0552+398-AI-S-RR.oifhak.original ] || {
    echo no pdd2843/outputfile.original && exit 3; }
[ -s pdd2843/321-1701_0552+398-AI-S-RR.oifhak ] || {
    echo no pdd2843/outputfile && exit 3; }

[ `diff ./ff-2843pdd.ps ./ff-2843cpy.ps | wc -l` -lt 9 ] || {
    echo these are very different: ./ff-2843pdd.ps ./ff-2843cpy.ps:
    diff ./ff-2843pdd.ps ./ff-2843cpy.ps
    exit 4;
}

# pluck out line containing the snr and parse it for the original
# original snr 48.2 amp 21.664
# revised  snr 43.6 amp 22.707
line0=$(grep '7570 9653' ./ff-2843pdd.ps)
IFS='()'
read a snr0 b <<<"$line0"
low0=48.1 high0=48.3 aok0=$(echo "$snr0>$low0 && $snr0<$high0" | bc)
$verb && echo aok0 is $aok0 and "$low0 < $snr0 < $high0" expected from: $line0
[ "$aok0" -gt 0 ]

line1=$(grep '7570 9653' ./ff-2843cpy.ps)
IFS='()'
read a snr1 b <<<"$line1"
low1=48.1 high1=48.3 aok1=$(echo "$snr1>$low1 && $snr1<$high1" | bc)
$verb && echo aok1 is $aok1 and "$low1 < $snr1 < $high1" expected from: $line1
[ "$aok1" -gt 0 ]

line2=$(grep '7570 9653' ./ff-2843fil.ps)
IFS='()'
read a snr2 b <<<"$line2"
low2=43.5 high2=43.7 aok2=$(echo "$snr2>$low2 && $snr2<$high2" | bc)
$verb && echo aok2 is $aok2 and "$low2 < $snr2 < $high2" expected from: $line2
[ "$aok2" -gt 0 ]
#
# eof
#
