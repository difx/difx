#!/bin/bash
#
# quickie to plot rates
#
tag=`date +%Y%m%d`
data=rates-$tag.txt
gnu=rates-$tag.gnu
png=rates-$tag.png
rm -f $data

mods=''

for l
do
    hm=`IFS=-; set -- $l; echo $1-$2-$3`
    echo $hm

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
done

minwrite=`grep '^#write' $data | sort -n | head -1 | cut -c8-`
minread=`grep '^#read' $data | sort -n | head -1 | cut -c7-`
margin=`(echo scale=4 ; echo $minwrite \* 32 \* 8 / \(2 \* 8224 \) ) | bc -lq`

cat > $gnu <<EOF
set key below
set xlabel 'file count'
set ylabel 'Rate (MB/s)'
set title "Module RW Performance, WorstWrite/Required = $margin"
set term png notransparent large size 960,640
set output '$png'
EOF

n=0
pc=plot
pd=$data
sls='ls 1'
als='ls 2'
for m in $mods
do
cat >> $gnu <<EOF
$pc '$pd' in $n u 0:1 w p $sls notitle '$m', \\
EOF
n=$(($n + 1))
pd=''
pc=''
tmp=$sls sls=$als als=$tmp
done
echo "$minwrite w l $sls title 'min write $minwrite', \\" >> $gnu
echo "$minread w l $als title 'min read $minread'" >> $gnu
echo 'set output' >> $gnu

gnuplot $gnu

${keep-'false'} || rm -f $data $gnu
ls -l $png

#
# eof
#
