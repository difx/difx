#!/bin/sh
#
# $Id: chk_baselines.sh 3326 2021-09-04 13:05:05Z gbc $
#
# test to verify local data processing
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`/DD
#export DATADIR=`cd $srcdir/testdata; pwd`
ldir=`cd $srcdir/testdata ; pwd`
rdir="$DATADIR/2843/321-1701_0552+398"
$verb && echo ldir = $ldir
$verb && echo DDIR = $rdir

# use difx if defined, otherwise first group named
for g in `groups`
do
    [ -z "$grp" ] && grp=$g
    [ "$g" = 'difx' ] && grp='difx'
done
$verb && echo using group $grp

# need to make a local copy to avoid race conditions
# it is cheaper to link rather than to copy the data
[ -d $rdir ] || mkdir -p $rdir
chgrp -R $grp $DATADIR
chmod -R 775 $DATADIR
time=oifhak
( cd $rdir && rm -f *.$time && ln -s $ldir/2843/321-1701_0552+398/* . )

targ="0552+398"
$verb && ls -l $rdir

for bs in AI AT IT
do
    $verb && echo \
    fourfit -b $bs $rdir/$targ.$time

    # AIT
    fourfit -b $bs $rdir/$targ.$time
    mv $rdir/$bs.*.*.$time .
done
chgrp $grp ??.*.*.$time
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
    chgrp $grp $f
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
