#!/bin/bash
#
# Script to verify GSL fit example
[ -f cbsplinelsqex2p8.log ] || {
    echo No execution of cbsplinelsqex2p8 present.  Skipping out.
    exit 77
}
[ -z "$srcdir" ] && {
    echo no srcdir provided, this is a serious issue
    exit 99
}
grep breakpoints: cbsplinelsqex2p8.log |\
cmp - $srcdir/gslcheck/gslspline2p8.output &&
    echo compiled output agrees with source output ||
    exit 1

grep '^data:'   cbsplinelsqex2p8.log | cut -c 6- >  cbsdata2p8.txt
echo                                             >> cbsdata2p8.txt
echo                                             >> cbsdata2p8.txt
grep '^spline:' cbsplinelsqex2p8.log | cut -c 8- >> cbsdata2p8.txt

# now make a plot
gnuplot=`type -p gnuplot`
[ -x "$gnuplot" ] || {
    echo no gnuplot to plot the data
    exit 77
}

cat > cbsdata2p8.gnu <<EOF
    set term pdfcairo size 8,5
    set output 'cbsdata2p8.pdf'
    set xlabel 'x'
    set xrange [0:15]
    set yrange [-1.5:1.5]
    set grid
    plot 'cbsdata2p8.txt' in 0 u 1:2 with poin ps .5 pt 22 lc 22 tit 'Data', \
         'cbsdata2p8.txt' in 1 u 1:2 with line ls 7 tit 'Spline 40 breaks' , \
         'cbsdata2p8.txt' in 1 u 1:3 with line ls 2 tit 'Spline 10 breaks'
    set output
EOF
gnuplot cbsdata2p8.gnu
[ -s cbsdata2p8.pdf ]
#
# eof vim:nospell
#
