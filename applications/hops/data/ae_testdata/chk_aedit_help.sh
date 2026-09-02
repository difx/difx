#!/bin/bash
#
# $Id: chk_fsumm.sh 1387 2016-08-02 21:06:10Z gbc $
#
# simple test on versions
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`

sumhelps='\
1485053011 5665 help
2041541239 769 clear
3540750592 4123 edit
2386850625 217 exit
2849026613 1251 fplot
3286695303 1549 inputs
2898987877 3739 parameter
1177976617 879 plist
4258282971 2644 plot
3511720621 1027 pwrite
1287658558 806 read
1299168689 678 run
1272602899 1096 setyear
2990362081 1616 sort
3786594108 1610 summary
2820001930 900 unflag
2168572253 163 unsort
3176920754 716 write
3716090625 651 zoom
2452557504 3127 axis
4251925990 412 grid
309005786 729 xscale
2196803992 728 yscale
2853735273 264 mode
2859580560 399 reference
3751099377 409 remote
2166827612 344 baselines
triangles
3010826899 414 experiment
1124864624 490 frequencies
902058616 497 polarizations
2566977321 1156 fraction
1136991571 325 length
2003380128 688 nfreq
434560857 743 qcodes
375624680 237 snrmax
1889219325 237 snrmin
2430772630 389 sources
4090944451 479 stations
3671938710 364 timerange
3880420156 1433 prange
861300576 318 procrange
458638435 339 type
2788746178 307 ccread
3238349212 1401 psplot
643034341 969 psfile
1497194389 2168 reproc
249463379 2171 device
1052029731 648 outversion
2630161640 290 batch
4129587049 171 nobatch
'
# these commands have no help
#0 0    bsnrmax
#0 0    bsnrmin
#0 0    twrite

rm -f aeherrs
echo "$sumhelps" |\
while read sum lines cmd
do
    echo ..$sum $lines $cmd.
    [ -z "$cmd" ] && continue
    echo  PAGER=cksum aedit -m 3 -b \"help $cmd\"
    check=$(PAGER=cksum aedit -m 3 -b "help $cmd")
    echo "#" $check $cmd
    [ "$check" = "$sum $lines" ] && touch aeherrs ||
        echo $check $cmd >> aeherrs
done

stat=`cat aeherrs | wc -l`
echo -n stat is $stat ', ' && ls -l aeherrs
exit $stat
#
# eof
#
