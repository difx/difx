#!/bin/bash
#
# More extensive testing of the passband feature
#
# The underlying dataset has 8 channels and an AB
# (Effelsburg) baseline with 4 pols that should be
# useful to establish what works correctly and what
# does not.
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`
export ALTDDIR=`cd $srcdir/testdata; pwd`
cwd=$DATADIR
$verb && echo DATADIR=$DATADIR && echo cwd=$cwd

# based on c211d-1-b1.bare.table
[ -f $ALTDDIR/3772/cf3772 ] || { echo $ALTDDIR/3772/cf3772 missing; exit 2; }
cp -p $ALTDDIR/3772/cf3772 .

gnuplot=`type -p gnuplot`
[ -x "$gnuplot" ] || { echo gnuplot not available--skipping test; exit 77; }

gpv=`$gnuplot --version | cut -d' ' -f2 | tr -d .`
[ "$gpv" -gt 42 ] || { echo gnuplot is too old ; exit 77; }

df="diskfile:ff-3772-%02d.ps"
ADD=$ALTDDIR/3772

# first one scan thorough all 4 pols using all 8 channels
# arrange for 11 lines per iteration
rm -f ff-3772.out
echo '8 chan b c d e f g h i' >> ff-3772.out
echo \
fourfit -pt -c cf3772 -b AB -d $df $ADD/No0001/1055+018.1T8DS3 \# 2\>\>ff-3772.out
fourfit -pt -c cf3772 -b AB -d $df $ADD/No0001/1055+018.1T8DS3    2>> ff-3772.out

while read line
do
    set -- $line
    [ x$1 = xx ] && continue
    echo $# chan $* >> ff-3772.out
    echo \
    fourfit -pt -c cf3772 -b AB $ADD/No0001/1055+018.1T8DS3 \
        set freqs $* \# 2\>\> ff-3772.out \# $# $*
    fourfit  -t -c cf3772 -b AB $ADD/No0001/1055+018.1T8DS3 \
        set freqs $*    2>>   ff-3772.out
done <<EOF
    b c d e f g h
x   b c d e f g   i
x   b c d e f   h i
x   b c d e   g h i
x   b c d   f g h i
x   b c   e f g h i
x   b   d e f g h i
      c d e f g h i
    b c d e f   h  
x   b c d e   g   i
x   b c d   f   h i
x   b c   e   g h i
x   b   d   f g h i
      c   e f g h i
    b   d e   g h
      c   e f   h i
    b c d e
      c d     g h
x   b c d         i
x   b c         h i
x   b         g h i
      c   e f g
            f g h i
    b   d   f   h  
x     c   e   g   i
    b   d
x     c   e
x           f   h
              g   i
    b   d   f
      c   e   g
x   b c
        d e
            f g
x               h i
    b
x     c
x       d
          e
x           f
              g
x               h
                  i
EOF

while read line
do
    set -- $line
    [ x$1 = xx ] && continue
    echo $1 passband $2 $3 >> ff-3772.out
    echo \
    fourfit -pt -c cf3772 -b AB $ADD/No0001/1055+018.1T8DS3 \
        set passband $2 $3 \# 2\>\> ff-3772.out \# $1
    fourfit  -t -c cf3772 -b AB $ADD/No0001/1055+018.1T8DS3 \
        set passband $2 $3    2>>   ff-3772.out
done <<EOF
x 86045.0 86105.0 86165.0 86227.0 86295.0 86355.0 86415.0 86465.0
    8    86045.0 86477.0
    7.5  86045.0 86471.0
    7.5  86051.0 86477.0
    7    86045.0 86427.0
    7    86105.0 86477.0
    6.5  86111.0 86477.0
    6.5  86057.0 86427.0
    6    86045.0 86367.0
    6    86105.0 86427.0
    6    86165.0 86477.0
x 86045.0 86105.0 86165.0 86227.0 86295.0 86355.0 86415.0 86465.0
    5.5  86051.0 86367.0
    5.5  86105.0 86421.0
    5.5  86165.0 86471.0
    5    86045.0 86307.0
    5    86165.0 86427.0
    4.5  86165.0 86421.0
    4.5  86227.0 86471.0
    4    86045.0 86239.0
    4    86295.0 86477.0
x 86045.0 86105.0 86165.0 86227.0 86295.0 86355.0 86415.0 86465.0
    3.5  86105.0 86301.0
    3.5  86165.0 86361.0
    3    86165.0 86307.0
    3    86045.0 86177.0
    2.5  86051.0 86177.0
    2.5  86227.0 86361.0
    2.5  86355.0 86471.0
    2    86045.0 86117.0
    2    86165.0 86239.0
    1.5  86045.0 86111.0
    1.5  86295.0 86361.0
    1.25 86298.0 86361.0
    1.25 86048.0 86111.0
x 86045.0 86105.0 86165.0 86227.0 86295.0 86355.0 86415.0 86465.0
    1    86045.0 86057.0
    1    86165.0 86177.0
    1    86355.0 86367.0
    0.75 86355.0 86364.0
    0.75 86415.0 86424.0
    0.75 86105.0 86114.0
    0.5  86051.0 86057.0
    0.5  86171.0 86177.0
    0.5  86361.0 86367.0
    0.25 86054.0 86057.0
    0.25 86174.0 86177.0
    0.25 86364.0 86367.0
    0.25 86051.0 86054.0
    0.25 86171.0 86174.0
    0.25 86361.0 86364.0
x 86045.0 86105.0 86165.0 86227.0 86295.0 86355.0 86415.0 86465.0
EOF

# build an awk script and grind
cat > ff_3772.awk <<\EOF
(NR % 41) == 1  {nc = $1; next; }
(NR % 41) == 5  {snrLL=$3; ampLL=$5; phsLL=$7; next; }
(NR % 41) == 6  {sbdLL=$3; mbdLL=$5; frrLL=$7; next; }
(NR % 41) == 7  {print "LL",nc,snrLL,ampLL,phsLL,sbdLL,mbdLL,frrLL;next}
(NR % 41) == 15 {snrRR=$3; ampRR=$5; phsRR=$7; next; }
(NR % 41) == 16 {sbdRR=$3; mbdRR=$5; frrRR=$7; next; }
(NR % 41) == 17 {print "RR",nc,snrRR,ampRR,phsRR,sbdRR,mbdRR,frrRR;next}
(NR % 41) == 25 {snrLR=$3; ampLR=$5; phsLR=$7; next; }
(NR % 41) == 26 {sbdLR=$3; mbdLR=$5; frrLR=$7; next; }
(NR % 41) == 27 {print "LR",nc,snrLR,ampLR,phsLR,sbdLR,mbdLR,frrLR;next}
(NR % 41) == 35 {snrRL=$3; ampRL=$5; phsRL=$7; next; }
(NR % 41) == 36 {sbdRL=$3; mbdRL=$5; frrRL=$7; next; }
(NR % 41) == 37 {print "RL",nc,snrRL,ampRL,phsRL,sbdRL,mbdRL,frrRL;next}
EOF
awk -f ff_3772.awk ff-3772.out > ff_3772_awked.out

rm -f ff_3772_awksep.out
echo '#P n snr    amp      phs       sbd      mbd      frr     ' > ff_3772_awksep.out
for pol in LL RR LR RL
do
    grep $pol ff_3772_awked.out >> ff_3772_awksep.out
    [ "$pol" = RL ] && break
    echo '' >> ff_3772_awksep.out
    echo '' >> ff_3772_awksep.out
done

cat > ff_3772_awksep.gnu <<\EOF
set term pdfcairo enhanced color size 18,12
set output 'ff_3772_awksep.pdf'
set key horizontal outside center top
set multiplot title '3772 survey' layout 2,3 rowsfirst upwards
set xlabel 'Eff.#chan (jittered)'
set ylabel 'SNR'
plot 'ff_3772_awksep.out' in 0 u ($2+$0*.002):3:($3/$3) w ye tit 'LL', \
                       '' in 1 u ($2+$0*.002):3:($3/$3) w ye tit 'RR', \
                       '' in 2 u ($2+$0*.002):3:($3/$3) w ye tit 'LR', \
                       '' in 3 u ($2+$0*.002):3:($3/$3) w ye tit 'RL'
set ylabel 'AMP'
plot 'ff_3772_awksep.out' in 0 u ($2+$0*.002):4:($4/$3) w ye tit 'LL', \
                       '' in 1 u ($2+$0*.002):4:($4/$3) w ye tit 'RR', \
                       '' in 2 u ($2+$0*.002):4:($4/$3) w ye tit 'LR', \
                       '' in 3 u ($2+$0*.002):4:($4/$3) w ye tit 'RL'
set ylabel 'Phase'
plot 'ff_3772_awksep.out' in 0 u ($2+$0*.002):5:($5/$3) w ye tit 'LL', \
                       '' in 1 u ($2+$0*.002):5:($5/$3) w ye tit 'RR', \
                       '' in 2 u ($2+$0*.002):5:($5/$3) w ye tit 'LR', \
                       '' in 3 u ($2+$0*.002):5:($5/$3) w ye tit 'RL'
set ylabel 'SBDelay'
plot 'ff_3772_awksep.out' in 0 u ($2+$0*.002):6:($6/$3) w ye tit 'LL', \
                       '' in 1 u ($2+$0*.002):6:($6/$3) w ye tit 'RR', \
                       '' in 2 u ($2+$0*.002):6:($6/$3) w ye tit 'LR', \
                       '' in 3 u ($2+$0*.002):6:($6/$3) w ye tit 'RL'
set ylabel 'MBDelay'
plot 'ff_3772_awksep.out' in 0 u ($2+$0*.002):7:($7/$3) w ye tit 'LL', \
                       '' in 1 u ($2+$0*.002):7:($7/$3) w ye tit 'RR', \
                       '' in 2 u ($2+$0*.002):7:($7/$3) w ye tit 'LR', \
                       '' in 3 u ($2+$0*.002):7:($7/$3) w ye tit 'RL'
set ylabel 'FringeRate'
plot 'ff_3772_awksep.out' in 0 u ($2+$0*.002):8:($8/$3) w ye tit 'LL', \
                       '' in 1 u ($2+$0*.002):8:($8/$3) w ye tit 'RR', \
                       '' in 2 u ($2+$0*.002):8:($8/$3) w ye tit 'LR', \
                       '' in 3 u ($2+$0*.002):8:($8/$3) w ye tit 'RL'
unset multiplot
set output
EOF
$gnuplot ff_3772_awksep.gnu

$verb && set -x

# LL 6.80847  RR 8.36735 LR 3.47288 RL 6.94062
LLamp=`awk '$1 ~ /LL/ {s+=$4;n++;}END{print s/n}' ff_3772_awksep.out`
RRamp=`awk '$1 ~ /RR/ {s+=$4;n++;}END{print s/n}' ff_3772_awksep.out`
LRamp=`awk '$1 ~ /LR/ {s+=$4;n++;}END{print s/n}' ff_3772_awksep.out`
RLamp=`awk '$1 ~ /RL/ {s+=$4;n++;}END{print s/n}' ff_3772_awksep.out`

# these are rather ad hoc margins
LLst=`echo "6.800 < $LLamp && $LLamp < 6.820" | bc -lq`
RRst=`echo "8.400 < $RRamp && $RRamp < 8.420" | bc -lq`
LRst=`echo "3.500 < $LRamp && $LRamp < 3.520" | bc -lq`
RLst=`echo "6.910 < $RLamp && $RLamp < 6.930" | bc -lq`

# final exit status
[ -f 'ff_3772_awksep.pdf' -a "$LLst$RRst$LRst$RLst" = '1111' ]
#
# eof
#
