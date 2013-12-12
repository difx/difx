#!/bin/sh
#
# $Id: chk_baselines.sh 879 2013-11-07 13:24:24Z gbc $
#
# canonical test suite for fourfit
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`cd $srcdir/testdata; pwd`

rdir="$DATADIR/2843/321-1701_0552+398"
targ="0552+398"
time=oifhak

chmod +w $rdir

for bs in AI AT IT
do
    $verb && echo \
    fourfit -b $bs $rdir/$targ.$time

    # AIT
    fourfit -b $bs $rdir/$targ.$time
    mv $rdir/$bs.*.*.$time .
done
chgrp difx ??.*.*.$time
chmod 664 ??.*.*.$time

files=`ls ??.?.?.$time | wc -l`
$verb && echo files is $files
[ "$files" -eq 6 ] || { echo Missing baselines; exit 2 ; }

# create helper
cat > pplot_print <<-EOF
    mv \$1 temp_plot.ps
EOF
chmod +x pplot_print

rm -f ??.?.?.$time.ps
for bsx in ??.?.?.$time
do
    PATH=`pwd`:$PATH fplot -h $bsx 2>/dev/null 1>&2
    [ -f temp_plot.ps ] && mv temp_plot.ps $bsx.ps
done

# remove helper
rm -f pplot_print

for f in ??.?.?.$time.ps
do
    chgrp difx $f
    chmod 664 $f
done

files=`ls ??.?.?.$time.ps | wc -l`
bytes=`ls -s ??.?.?.$time.ps | awk '{s+=$1}END{print s}'`

$verb && echo files is $files
$verb && echo bytes is $bytes
[ "$files" -eq 6 -a "$bytes" -ge 420 ]

#
# eof
#
