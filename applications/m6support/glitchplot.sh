#!/bin/sh
#
# (c) Massachusetts Institute of Technology, 2013..2023
# (c) Geoffrey B. Crew, 2013..2023
#
# Script to plot statsplot.py output
#
# This was written to investigate changes in statistics during the
# gap between ALMA BLC subscans (about 2 seconds).  Since Cy8, the
# gap was eliminated (as the BLC requires no reprogramming) and the
# glitch naturally disappears.
#
# Ideally sort foo* > something.data
#
data=$1   # name of stats data file
tag=$2    # name for output
xoff=$3   # time offset
ps=0.5

datatitle=`echo $data | tr _ -`

[ -z "$tag" ] && tag=`basename $1`.glitch
[ -z "$xoff" ] && xoff=`head -1 $data | cut -d' ' -f1`
[ -z "$TRIO" ] && TRIO=true

[ $TRIO = true -o $TRIO = false ] ||
    { echo TRIO must be true or false ; exit 1; }

cat > $tag.gnu <<EOF
set term pdfcairo size 10, 6
set output '$tag.pdf'
set mxtics 10
set mytics 10
set grid xtics mxtics ytics mytics
set key bmargin
set ylabel 'Percentages Relative to Mean'
set xlabel 'Time (sec of day from $xoff)'
set title 'Glitch of $datatitle ($tag)'
xo=$xoff
f(x) = a
fit f(x) '$data' u 1:2 via a
EOF

$TRIO && cat >> $tag.gnu <<EOF
g(x) = b
h(x) = c
fit g(x) '$data' u 1:3 via b
fit h(x) '$data' u 1:4 via c
EOF

$TRIO && cat >> $tag.gnu <<EOF
plot $PLIMITS \
    '$data' u (\$1-xo):(\$2-a + 1) ps $ps tit 'middle + 1', \
         '' u (\$1-xo):(\$3-b - 1) ps $ps tit 'outer - 1',  \
         '' u (\$1-xo):(\$4-c) ps $ps tit '100*neg/pos'
set output
EOF
$TRIO || cat >> $tag.gnu <<EOF
plot $PLIMITS \
    '$data' u (\$1-xo):(\$2-a + 1) ps $ps tit 'middle + 1'
set output
EOF

gnuplot $tag.gnu
rm -f $tag.gnu

#
# eof
#
