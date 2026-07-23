#!/bin/bash
#
# Script to verify GSL fit example
[ -f cbsplinelsqex2p7.log ] || {
    echo No execution of cbsplinelsqex2p7 present.  Skipping out.
    exit 77
}
[ -z "$srcdir" ] && {
    echo no srcdir provided, this is a serious issue
    exit 99
}

grep -vE 'PASS|FAIL' cbsplinelsqex2p7.log |\
cmp - $srcdir/gslcheck/gslspline2p7.output &&
    echo compiled output agrees with source output ||
    exit 1

grep '^data:'   cbsplinelsqex2p7.log | cut -c 6- >  cbsdata2p7.txt
echo                                             >> cbsdata2p7.txt
echo                                             >> cbsdata2p7.txt
grep '^spline:' cbsplinelsqex2p7.log | cut -c 8- >> cbsdata2p7.txt

# now make a plot

graph=`type -p graph`
ps2pdf=`type -p ps2pdf`
havegraphpdf=false
[ -n "$graph" -a -x "$graph" -a -n "$ps2pdf" -a -x "$ps2pdf" ] && {
    havegraphpdf=true
    sed -e 's/data://' -e 's/spline://' cbsplinelsqex2p7.log |\
        grep -vE '^(.m|PASS|FAIL)'> cbsgraph2p7.txt
    $graph -T ps -X x -Y y -x 0 15 -y -1 1.3 <  cbsgraph2p7.txt \
        > cbsgraph2p7.ps
    [ -f cbsgraph2p7.ps ] && $ps2pdf cbsgraph2p7.ps && rm cbsgraph2p7.ps
    [ -s cbsgraph2p7.pdf ] || echo unable to make cbsgraph2p7.pdf
} || {
    echo do not have graph or ps2pdf, so skipping this part.
    echo "graph is '$graph' and ps2pdf is '$ps2pdf'"
}

gnuplot=`type -p gnuplot`
[ -x "$gnuplot" ] || {
    echo no gnuplot to plot the data
    exit 77
}

cat > cbsdata2p7.gnu <<EOF
    set term pdfcairo size 8,5
    set output 'cbsdata2p7.pdf'
    set xlabel 'x'
    set xrange [0:15]
    set yrange [-1.5:1.5]
    set grid
    plot 'cbsdata2p7.txt' in 0 u 1:2 with poin ps .5 pt 22 lc 22 tit 'Data', \
         'cbsdata2p7.txt' in 1 u 1:2 with line ls 7 tit 'Spline 40 breaks'
    set output
EOF
gnuplot cbsdata2p7.gnu
[ -s cbsdata2p7.pdf ]
#
# eof vim:nospell
#
