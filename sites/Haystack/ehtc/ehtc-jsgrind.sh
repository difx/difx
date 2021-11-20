#!/bin/bash
#
# Script to run the processing for one project/target group.
# See the processing template for details.
#
USAGE="Usage: \$ehtc/ehtc-jsgrind.sh true|false true|false

with these supplied in the environment:
    \$proj  the ALMA project nickname
    \$targ  the VEX target name
    \$class the eht target class (sci, cal or eht)

Both logic variables default to true and that is the normal mode of
usage from Cycle5 onwards.  Previously the first could be set to false
to pause the grind and allow development of a fourfit control file.
The second value controls whether to run polconvert or not.  Thus:

    \$ehtc/ehtc-jsgrind.sh false true
    # create fourfit control file and then continue to
    # gather products and create the release.sh script
    \$ehtc/ehtc-jsgrind.sh false false

or, if a fourfit control file is available (e.g. from prior processing
of selected scans):

    \$ehtc/ehtc-jsgrind.sh true true

(actually the first boolean is irrelevant of the 2nd is false).

In any case, a full set of environment variables as must be supplied.
"
#
# While developing fourfit control file (earlier mode of execution):
#  $ehtc/ehtc-jsgrind.sh false true
#  $ehtc/ehtc-jsgrind.sh false false
# once fourfit control file is established (this is the normal mode):
#  $ehtc/ehtc-jsgrind.sh true true
#
[ -z "$exp"   ] && { echo exp   must be defined ; exit 1 ; }
[ -z "$evs"   ] && { echo evs   must be defined ; exit 1 ; }
[ -z "$ers"   ] && { echo ers   must be defined ; exit 1 ; }
[ -z "$expn"  ] && { echo expn  must be defined ; exit 1 ; }
[ -z "$opts"  ] && { echo opts  must be defined with options for polconvert ;
    exit 1 ; }
[ -z "$pcal"  ] && { echo pcal  must be defined with name of QA2 package ;
    exit 1 ; }
[ -z "$vers"  ] && { echo vers  must be defined ; exit 1 ; }
[ -z "$relv"  ] && { echo relv  must be defined ; exit 1 ; }
[ -z "$subv"  ] && { echo subv  must be defined ; exit 1 ; }
[ -z "$iter"  ] && { echo iter  must be defined ; exit 1 ; }
[ -z "$expn"  ] && { echo expn  must be defined ; exit 1 ; }
# "$label" is ignored
[ -z "$targ"  ] && { echo targ  must be defined ; exit 1 ; }
[ -z "$dout"  ] && { echo dout  must be defined ; exit 1 ; }
[ -z "$proj"  ] && { echo proj  must be defined ; exit 1 ; }
[ -z "$release"  ] && { echo release must be defined ; exit 1 ; }

# apply joblist -u flag
[ -z "$uniq"  ] && uniq=false
[ "$uniq" = 'true' -o "$uniq" = 'false' ] || {
    echo if defined, uniq must be true or false; }
$uniq && uf=-u || uf=''

haveffconf=${1-'true'}
[ "$haveffconf" = true -o "$haveffconf" = false ] || {
    echo "the first arg must be true(have ff conf) or false(need ff conf)"
    exit 1
}
polconvert=${2-'true'}
[ "$polconvert" = true -o "$polconvert" = false ] || {
    echo "the 2nd argument must be true(run pc) or false (ff and release)"
    exit 1
}
[ $haveffconf$polconvert = truetrue   ] && {
    echo doing the full grind assuming a fourfit control file will be found ; }
[ $haveffconf$polconvert = falsetrue  ] && {
    echo doing the polconversion processing and fourfit preparations
    echo after which you will need to create or update the fourfit
    echo control file and then run this script with two false arguments; }
[ $haveffconf$polconvert = falsefalse ] && {
    echo proceeding to make the release script; }
[ $haveffconf$polconvert = truefalse  ] && {
    echo "$USAGE" ; echo poor logic choices...bailing out. ; exit 1 ; }

# check that these grouping vars are defined
[ -n "$class" ] || { echo "class='cal|sci|eht' must be defined"; exit 1; }
[ -n "$proj" ] || { echo "proj must be defined"; exit 1; }
[ -n "$targ" ] || { echo "targ must be defined"; exit 1; }

if [ -d $expn ] && [ $haveffconf$polconvert = truetrue ]; then
   echo Warning: removing existing ./$expn/ because fourfit would otherwise never run in this mode.
   rm -rf ./$expn/
fi

# export proj=yyy targ=XXX class=cal|sci
export label=$proj-$targ
export jselect="-p $proj -s $targ"
echo "preparing jselect='$jselect' to label='$label'"

$polconvert && {
    # create the list of job inputs as $jobs
    eval `$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs $jselect -J $uf`
    # display the list of selected jobs
    echo "processing these jobs:"
    echo \
    $ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs $jselect -R $uf
    $ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs $jselect -R $uf

    # review list of jobs, review $scmp, $opts
    echo \
    prepolconvert.py -v -k -s $dout $jobs
    prepolconvert.py -v -k -s $dout $jobs
    # evaluate partitioning of job list (use -v if error)
    echo \
    $ehtc/ehtc-zoomchk.py -v $jobs
    $ehtc/ehtc-zoomchk.py -v $jobs
    # subdivide $jobs as necessary, do the polconvert on each set of $jobs
    echo \
    drivepolconvert.py -v $opts -l $pcal $jobs
    drivepolconvert.py -v $opts -l $pcal $jobs
    status=$?
    # stop things dead in their tracks if we have an issue
    # in order to prevent alot of stupid tarballing activity
    [ $status -eq 0 ] && { echo drivepolconvert.py exited normally ; } ||
        { echo drivepolconvert.py exited with status $status; exit $status; }
    # evaluate results on full set of $jobs--look at ALL_IFs plots; then

    #--------------------------------------------------------------------------
    # A: until you have the fourfit control file built:
    $haveffconf || {
        echo \
        $ehtc/ehtc-postdrive.sh eval $haveffconf $jobs
        $ehtc/ehtc-postdrive.sh eval false $jobs
        # ie.: cd $expn ; $ehtc/est_manual_phases.py -c $evs.conf <root> ; cd ..
        echo now update the fourfit control file and fourfit as instructed,
        echo then re-run with
        echo $0 false false
        makerelease=false
    }
    # B: thereafter, where you just re-use it.
    $haveffconf && {
        echo \
        $ehtc/ehtc-postdrive.sh eval $jobs
        $ehtc/ehtc-postdrive.sh eval $jobs
        makerelease=true
    }
    #--------------------------------------------------------------------------
} || {
    # continue after 2nd 4fit
    makerelease=true
}

$makerelease && {
[ -d tarballs ] || { echo tarballs dir is missing; exit 2; }
[ -d logs ] || { echo logs dir is missing; exit 3; }
[ -d tbdir -a -d tbdir/logs ] || { echo tbdir or tbdir/logs missing; exit 4;}
# stage tarballs and logs in local release directory
mv tarballs tb-$label
mkdir -p tb-$label/logs/packaging
cp -a logs/$ers-*.log tb-$label/logs/packaging
cp -a tbdir/logs/* tb-$label/logs/packaging
cp -a $proj-$targ-$subv.log tb-$label/logs/packaging
rm -f  logs/$ers-*.log tbdir/logs/* $proj-$targ-$subv.log
# build the release script
sed 's/^....//' > tb-$label/release.sh <<EOF
    [ -d $release/$proj-$class ] ||
        mkdir -p $release/$proj-$class
    [ -d $release/logs ] ||
        mkdir -p $release/logs
    [ -d $release/logs/packaging ] ||
        mkdir -p $release/logs/$proj-$class-packaging
    cd `pwd`/tb-$label
    # check mirror space
    need=\`du -s . | tr -d ' \t.'\`
    have=\`df $release | tail -1 | awk '{printf("%d", int(\$4*9/10))}'\`
    echo need \$need 1k blocks, have \$have 1k blocks
    [ "\$need" -lt "\$have" ] && { echo we appear to have space; }
    [ "\$need" -lt "\$have" ] || { echo need more space; exit 1; }
    # ok, safe to do the copy
    for t in *.tar
    do
        ls -lh \$t
        mv \$t $release/$proj-$class
        ls -lh $release/$proj-$class/\$t
    done
    mv logs/packaging/* \\
        $release/logs/$proj-$class-packaging
    ( date ; sleep 2 ; chmod 644 nohup.out ; mv nohup.out \\
        $release/logs/$proj-$class-packaging/$label-$class-$subv-release.log )&
    disown
EOF
chmod +x tb-$label/release.sh
ls -lhR tb-$label/*

rm -rf tbdir/*haxp* || echo unable to remove tbdir/haxp dirs
rm tbdir/$exp.codes || echo unable to remove link tbdir/$exp.codes
rmdir tbdir/logs || echo tbdir/logs was not empty when it should be
rmdir tbdir || echo tbdir was not empty when it should be
rmdir logs || echo logs was not empty when it should be

echo ''
echo '#########'
echo '# execute'
echo '  cd tb-$label ; nohup ./release.sh &'
echo '# when you are ready to release the data'
echo '#########'
echo ''
}

#
# eof
#
