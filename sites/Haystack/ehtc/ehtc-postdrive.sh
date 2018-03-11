#!/bin/bash
#
# group of commands
#
# first time around
#  $ehtc/ehtc-postdrive.sh echo false $jobs
#  $ehtc/ehtc-postdrive.sh eval false $jobs
#  and run the suggested 4fit command twice
# most of the time:
#  $ehtc/ehtc-postdrive.sh echo $jobs
#  $ehtc/ehtc-postdrive.sh eval $jobs
# and deprecated:
#  $ehtc/ehtc-postdrive.sh echo haxp $jobs
#  $ehtc/ehtc-postdrive.sh eval haxp $jobs
#
echo=${1-'help'}
ffit=true

[ $echo = 'help' ] && { echo read the script ; exit 1 ; }
[ $echo = 'echo' -o $echo = 'eval' ] || {
    echo read the script: $echo ; exit 1 ; }
shift

[ $# -ge 1 ] || { echo no jobs, no work, not quite.... ; exit 0 ; }

# only one of these, please
echo ''
[ "$1" = true  ] && ffit=true  && echo '  [WITH FOURFIT] ' && shift
[ "$1" = false ] && ffit=false && echo '  [NO FOURFIT] '   && shift
[ "$1" = haxp  ] && ffit=haxp  && echo '  [ALMA ONLY D2M]' && shift
[ $ffit = haxp ] && ffit=false && haxp=true || haxp=false

[ -z "$exp"   ] && { echo exp   must be defined ; exit 1 ; }
[ -z "$vers"  ] && { echo vers  must be defined ; exit 1 ; }
[ -z "$relv"  ] && { echo relv  must be defined ; exit 1 ; }
[ -z "$subv"  ] && { echo subv  must be defined ; exit 1 ; }
[ -z "$iter"  ] && { echo iter  must be defined ; exit 1 ; }
[ -z "$expn"  ] && { echo expn  must be defined ; exit 1 ; }
[ -z "$label" ] && { echo label must be defined ; exit 1 ; }
[ -z "$targ"  ] && { echo targ  must be defined ; exit 1 ; }
[ -z "$dout"  ] && { echo dout  must be defined ; exit 1 ; }
[ -z "$proj"  ] && { echo proj  must be defined ; exit 1 ; }
[ "$ffit" = 'true' -o "$ffit" = 'false' ] || { echo prog error ffit; exit 1; }
[ "$haxp" = 'true' -o "$haxp" = 'false' ] || { echo prog error haxp; exit 1; }

jobs="$@"

# workdir is presumably one of the polconvert postprocessing dirs
# while dout is where the correlator DiFX output sits
# tbdir is safe place for postprocessing
workdir=`pwd`
tbdir=$workdir/tbdir
[ -d $tbdir ] || mkdir $tbdir
[ -h $tbdir/$exp.codes ] ||
    ( cd $tbdir && ln -s $workdir/$exp.codes . )
[ -d $workdir/tarballs ] || mkdir $workdir/tarballs

echo ''
echo proj is $proj
echo jobs is $jobs
echo workdir $workdir
echo ''

# allows fitsname to be true or false from the environment
[ -n "$fitsname" ] && [ "$fitsname" = true -o "$fitsname" = false ] &&
    fn=fitsname=$fitsname || fn=''

# do the haxp thing which is similar to the swin case common to
# both pathways above...however we need some additional setup to
# work in the tarball directory rather than the v$vers dir
# this block of code was developmental and is deprecated
$haxp && {
    cd $tbdir
    $echo pwd
    $echo ls -l $exp.codes
    newjobs=''
    for j in $jobs ; do newjobs="$newjobs ${dout}/$j" ; done
    $echo \
    $ehtc/ehtc-tarballs.sh tar=haxp \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv \
        dest=$workdir/tarballs target=$targ jobs $newjobs
    exit 0
}

# do the project-based dance for the routine stuff
#   na:   post-corr, fits, hops, 2x 4fit
#   alma: post-alma, post-corr, 2x 4fit
[ $proj = na ] && {
    # non-ALMA case
    $echo \
    cd $tbdir
    $echo pwd
    $echo \
    $ehtc/ehtc-tarballs.sh tar=post-corr \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv \
        dest=$workdir/tarballs src=$dout jobs $jobs
    [ -d ./$exp-$vers-$subv-$proj-$targ-haxp.$expn.save ] &&
      mv ./$exp-$vers-$subv-$proj-$targ-haxp.$expn.save \
         ./$exp-$vers-$subv-$proj-$targ-haxp.$expn.prev-$$
    [ -d $dout/$exp-$vers-$subv-$proj-$targ-haxp.$expn.save ] &&
      mv $dout/$exp-$vers-$subv-$proj-$targ-haxp.$expn.save . ||
      echo unable to mv $exp-$vers-$subv-$proj-$targ-haxp.$expn.save
    $echo \
    cd $workdir
    $echo pwd
    $echo \
    $ehtc/ehtc-tarballs.sh tar=fits \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv $fn \
        dest=./tarballs target=$targ jobs $jobs
    $echo \
    $ehtc/ehtc-tarballs.sh tar=hops \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv \
        dest=./tarballs target=$targ jobs $jobs
    $ffit && $echo \
    $ehtc/ehtc-tarballs.sh tar=4fit \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv \
        dest=./tarballs target=$targ jobs $jobs
    $ffit && $echo \
    $ehtc/ehtc-tarballs.sh tar=4fit \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv \
        dest=./tarballs target=$targ jobs $jobs
    $ffit || {
        echo
        echo '============================================================'
        echo  You should execute the following command twice, and
        echo  running the est_manual_phases.py script as suggested
        echo  in between on scans to set the manual phase and delay
        echo  calibrations in the fourfit file.
        echo
        echo $ehtc/ehtc-tarballs.sh tar=4fit flab=\$flab \\ && echo \
        '    'exp=\$exp vers=\$vers subv=\$subv \\ && echo \
        '    'expn=\$expn nuke=true over=true \\ && echo \
        '    'save=true label=\$label relv=\$relv \\ && echo \
        '    'dest=./tarballs target=\$targ jobs \$jobs
        echo '============================================================'
        echo
    }
    true
} || {
    # ALMA case
    $echo \
    $ehtc/ehtc-tarballs.sh tar=post-alma \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv $fn \
        dest=./tarballs target=$targ jobs $jobs
    $echo \
    cd $tbdir
    $echo pwd
    $echo \
    $ehtc/ehtc-tarballs.sh tar=post-corr \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv \
        dest=$workdir/tarballs src=$dout jobs $jobs
    [ "$echo" = eval ] && {
        [ -d ./$exp-$vers-$subv-$proj-$targ-haxp.$expn.save ] &&
          mv ./$exp-$vers-$subv-$proj-$targ-haxp.$expn.save \
             ./$exp-$vers-$subv-$proj-$targ-haxp.$expn.prev-$$
        [ -d $dout/$exp-$vers-$subv-$proj-$targ-haxp.$expn.save ] &&
          mv $dout/$exp-$vers-$subv-$proj-$targ-haxp.$expn.save . ||
          echo unable to mv $exp-$vers-$subv-$proj-$targ-haxp.$expn.save
    } || echo mv $dout/$exp-$vers-$subv-$proj-$targ-haxp.$expn.save .
    $echo \
    cd $workdir
    $echo pwd
    $ffit && $echo \
    $ehtc/ehtc-tarballs.sh tar=4fit \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv \
        dest=./tarballs target=$targ jobs $jobs
    # no need for pc phases
    $ffit && $echo \
    $ehtc/ehtc-tarballs.sh tar=4fit \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv \
        dest=./tarballs target=$targ jobs $jobs
    $ffit || {
        echo 
        echo '============================================================'
        echo  You should execute the following command twice, and
        echo  running the est_manual_phases.py script as suggested
        echo  in between on scans to set the manual phase and delay
        echo  calibrations in the fourfit file.
        echo
        echo $ehtc/ehtc-tarballs.sh tar=4fit flab=\$flab \\ && echo \
        '    'exp=\$exp vers=\$vers subv=\$subv \\ && echo \
        '    'expn=\$expn nuke=true over=true \\ && echo \
        '    'save=true label=\$label relv=\$relv \\ && echo \
        '    'dest=./tarballs target=\$targ jobs \$jobs
        echo '============================================================'
        echo
    }
    true
}

echo
trash=`find . -name \*.prev-\*`
[ -n "$trash" ] && echo You may want to delete these: &&
    echo 'find . -name \*.prev-\*' &&
    echo "$trash" | sed 's/^/  rm -rf /' &&
    echo 'Or: find . -name \*.prev-\* -exec rm -rf {} \; -prune' &&
echo

#
# eof
#
