#!/bin/bash
#
# Script to reduce logged hammer.sh data and plot with gnuplot
#
USAGE='Usage: '`basename $0`' hammer-log [more logs...]

which will read the log file(s) and generate a plot of performance
for each.  You will need to have both gnuplot and bc installed.
'
[ -n "$1" -a "$1" = '--help' ] && { echo "$USAGE" ; exit 0 ; }

[ -x `type -p gnuplot` ] || { echo gnuplot not available ; exit 1; }
[ -x `type -p bc` ] || { echo bc not available ; exit 2; }

# temporary files
tag=`date +%Y%m%d`
data=rates-$tag.txt
gnu=rates-$tag.gnu
png=rates-$tag.png
rm -f $data $gnu $png

for l
do
    mods=''
    #hm=`IFS=-; set -- $l; echo $1-$2-$3`
    hm=`IFS=-; set -- $l; echo "${@: -4:1}-${@: -3:1}-${@: -2:1}"`
    hm=`expr $l : '\(.*\)-hammer-.*'`
    out=$hm-`date +%Y%m%d`.png
    echo Host-Module: $hm ' -> ' $out

    reqwrite=`(echo scale=4;echo 2 \* 8224 / 32 / 8)|bc -lq`

    grep 'write$' $l |\
    awk -v min=200 '$5<min{min=$5}{print $5}END{print "#write ",min}' >> $data
    echo '' >> $data
    echo '' >> $data
    mods="$mods $hm-write"

    grep 'read$' $l |\
    awk -v min=200 '$5<min{min=$5}{print $5}END{print "#read ",min}' >> $data
    echo '' >> $data
    echo '' >> $data
    mods="$mods $hm-read"

    grep 'write$' $l |\
    awk '$5>'"$reqwrite"'{o++;}{t++}END{print "#frac ",100.*o/t}' >> $data

    minwrite=`grep '^#write' $data | sort -n | head -1 | cut -c8-`
    minread=`grep '^#read' $data | sort -n | head -1 | cut -c7-`
    margin=`(echo scale=4;echo $minwrite \* 32 \* 8 / \(2 \* 8224 \) )|bc -lq`
    fraclow=`grep '^#frac' $data | cut -c7-`"% Ok Writes"

    sed 's/^....//' > $gnu <<....EOF
    set key below
    set xlabel 'file count'
    set ylabel 'Rate (MB/s)'
    set title "$hm RW Performance, WorstWrite/Required = $margin, $fraclow"
    set term png notransparent large size 960,640
    set output '$png'
....EOF

    n=0
    pc='plot [][50:200]'
    pd=$data
    sls='ls 1'
    als='ls 2'
    rls='ls 3'
    for m in $mods
    do
        cat >> $gnu <<........EOF
        $pc '$pd' in $n u 0:1 w p $sls notitle '$m', \\
........EOF
    n=$(($n + 1))
    pd=''
    pc=''
    tmp=$sls sls=$als als=$tmp
    done

    echo "$minwrite w l $sls title 'min write $minwrite', \\" >> $gnu
    echo "$minread w l $als title 'min read $minread', \\" >> $gnu
    echo "$reqwrite w l $rls title 'req read $reqwrite'" >> $gnu
    echo 'set output' >> $gnu

    gnuplot $gnu

    ${keep-'false'} || rm -f $data $gnu
    mv $png $out
    ls -l $out

done

#
# eof
#
