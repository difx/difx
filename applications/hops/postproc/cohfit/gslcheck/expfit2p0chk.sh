#!/bin/bash
#
# Script to verify GSL fit example
[ -f expfitexam2p0.log ] || {
    echo No execution of expfitexample present.  Skipping out.
    exit 77
}
[ -z "$srcdir" ] && {
    echo no srcdir provided, this is a serious issue
    exit 99
}

grep -vE 'data|ffun|fact|PASS|FAIL' expfitexam2p0.log |\
diff - $srcdir/gslcheck/gslexp2p0.output

# may have to lower standards
tail -4 $srcdir/gslcheck/gslexp2p0.output > efetemp2p0.txt

grep -vE 'data|ffun|fact|PASS|FAIL' expfitexam2p0.log | tail -4 |\
cmp - efetemp2p0.txt &&
    echo compiled output agrees with source output ||
    exit 1

# now make a plot
gnuplot=`type -p gnuplot`
[ -x "$gnuplot" ] || {
    echo no gnuplot to plot the data
    exit 77
}

grep '^data:' expfitexam2p0.log | cut -c 6- > efedata2p0.txt
grep '^ffun:' expfitexam2p0.log | cut -c 6- > efeffun2p0.txt
grep '^fact:' expfitexam2p0.log | cut -c 6- > efefact2p0.txt
cat > efedata2p0.gnu <<EOF
    set term pdfcairo size 5,5
    set output 'efedata2p0.pdf'
    set xlabel 'time'
    set logscale xy
    set xrange [.027:3.2]
    set yrange [.6:8]
    set grid
    plot 'efedata2p0.txt' u 1:2:3 with yerr ps .5 pt 22 lc 22 tit 'Data', \
         'efeffun2p0.txt' u 1:2 with line ls 7 tit 'FIT', \
         'efefact2p0.txt' u 1:2 with line ls 2 tit 'f(t)'
    set output
EOF
gnuplot efedata2p0.gnu
[ -f efedata2p0.pdf ]
#
# eof vim:nospell
#
