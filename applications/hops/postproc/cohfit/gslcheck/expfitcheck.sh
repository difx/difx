#!/bin/bash
#
# Script to verify GSL fit example
[ -f expfitexample.log ] || {
    echo No execution of expfitexample present.  Skipping out.
    exit 77
}
[ -z "$srcdir" ] && {
    echo no srcdir provided, this is a serious issue
    exit 99
}
head -24 expfitexample.log |\
cmp - $srcdir/gslcheck/gslexpfit.output &&
    echo compiled output agrees with source output ||
    exit 1

# now make a plot
gnuplot=`type -p gnuplot`
[ -x "$gnuplot" ] || {
    echo no gnuplot to plot the data
    exit 77
}

grep '^data:' expfitexample.log | cut -c 6- > efedata.txt
grep '^ffun:' expfitexample.log | cut -c 6- > efeffun.txt
grep '^fact:' expfitexample.log | cut -c 6- > efefact.txt
cat > efedata.gnu <<EOF
    set term pdfcairo size 5,5
    set output 'efedata.pdf'
    set xlabel 'time'
    set logscale xy
    set xrange [.027:3.2]
    set yrange [.6:8]
    set grid
    plot 'efedata.txt' u 1:2:3 with yerr ps .5 pt 22 lc 22 tit 'Data', \
         'efeffun.txt' u 1:2 with line ls 7 tit 'FIT', \
         'efefact.txt' u 1:2 with line ls 2 tit 'f(t)'
    set output
EOF
gnuplot efedata.gnu
[ -f efedata.pdf ]
#
# eof vim:nospell
#
