#!/bin/bash
#
# Script to verify GSL fit example
[ -f expfitexam2p1.log ] || {
    echo No execution of expfitexample present.  Skipping out.
    exit 77
}
[ -z "$srcdir" ] && {
    echo no srcdir provided, this is a serious issue
    exit 99
}

grep -vE 'data|ffun|fact|PASS|FAIL' expfitexam2p1.log |\
cmp - $srcdir/gslcheck/gslexp2p1.output &&
    echo compiled output agrees with source output ||
    exit 1

# now make a plot
gnuplot=`type -p gnuplot`
[ -x "$gnuplot" ] || {
    echo no gnuplot to plot the data
    exit 77
}

grep '^data:' expfitexam2p1.log | cut -c 6- > efedata2p1.txt
grep '^ffun:' expfitexam2p1.log | cut -c 6- > efeffun2p1.txt
grep '^fact:' expfitexam2p1.log | cut -c 6- > efefact2p1.txt
cat > efedata2p1.gnu <<EOF
    set term pdfcairo size 5,5
    set output 'efedata2p1.pdf'
    set xlabel 'time'
    set logscale xy
    set xrange [0.9:41]
    set yrange [0.9:6]
    set grid
    plot 'efedata2p1.txt' u 1:2:3 with yerr ps .5 pt 22 lc 22 tit 'Data', \
         'efeffun2p1.txt' u 1:2 with line ls 7 tit 'FIT', \
         'efefact2p1.txt' u 1:2 with line ls 2 tit 'f(t)'
    set output
EOF
gnuplot efedata2p1.gnu
[ -f efedata2p1.pdf ]
#
# eof vim:nospell
#
