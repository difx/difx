#!/bin/bash
#
# Script to run the processing for one project/target group.
# See the processing template for details.
#
# Usage: $ehtc/ehtc-jsgrind.sh <fourfit_conf_exists:true/false> <run_polconvert:true/false>
#
# While developing fourfit control file:
#  $ehtc/ehtc-jsgrind.sh false true
#  $ehtc/ehtc-jsgrind.sh false false
# once fourfit control file is established:
#  $ehtc/ehtc-jsgrind.sh #true true
#
[ -z "$exp"   ] && { echo exp   must be defined ; exit 1 ; }
[ -z "$expn"  ] && { echo expn  must be defined ; exit 1 ; }
[ -z "$opts"  ] && { echo opts  must be defined with options for polconvert ; exit 1 ; }
[ -z "$pcal"  ] && { echo pcal  must be defined with name of QA2 package ; exit 1 ; }
[ -z "$vers"  ] && { echo vers  must be defined ; exit 1 ; }
[ -z "$relv"  ] && { echo relv  must be defined ; exit 1 ; }
[ -z "$subv"  ] && { echo subv  must be defined ; exit 1 ; }
[ -z "$iter"  ] && { echo iter  must be defined ; exit 1 ; }
[ -z "$expn"  ] && { echo expn  must be defined ; exit 1 ; }
# [ -z "$label" ] && { echo label must be defined ; exit 1 ; } ### FIXME: Readme-Cycle?.txt declares 'label', ignored, as it gets overwritten below
[ -z "$targ"  ] && { echo targ  must be defined ; exit 1 ; }
[ -z "$dout"  ] && { echo dout  must be defined ; exit 1 ; }
[ -z "$proj"  ] && { echo proj  must be defined ; exit 1 ; }

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
    echo completing the fourfit processing and making the release tarballs; }
    ### FIXME: action doesn't seem to match description of arg haveffconf, valid should be truefalse
[ $haveffconf$polconvert = truefalse  ] && {
    echo this is a nonsensical set of options...bailing out ; exit 1 ; }

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
    eval `$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs $jselect -J`
    # display the list of selected jobs
    echo "processing these jobs:"
    echo \
    $ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs $jselect -R
    $ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs $jselect -R

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
# stage tarballs and logs in local release directory
mv tarballs tb-$label
mkdir -p tb-$label/logs/packaging
mv logs/$ers-*.log tb-$label/logs/packaging
mv tbdir/logs/* tb-$label/logs/packaging
mv $proj-$targ.log tb-$label/logs/packaging
# build the release script
sed 's/^....//' > tb-$label/release.sh <<EOF
    [ -d $release/$proj-$class ] ||
        mkdir -p $release/$proj-$class
    [ -d $release/logs ] ||
        mkdir -p $release/logs
    [ -d $release/logs/packaging ] ||
        mkdir -p $release/logs/$proj-$class-packaging
    cd `pwd`/tb-$label
    for t in *.tar
    do
        mv \$t $release/$proj-$class
        ls -lh $release/$proj-$class/\$t
    done
    mv logs/packaging/* \\
        $release/logs/$proj-$class-packaging
    ( date ; sleep 2 ; chmod 644 nohup.out ; mv nohup.out \\
        $release/logs/$proj-$class-packaging/$label-$class-release.log ) &
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
echo '  cd tb-$label/logs ; echo nohup ./release.sh &'
echo '# when you are ready to release the data'
echo '#########'
echo ''
}

#
# eof
#
