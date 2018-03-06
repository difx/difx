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
# and
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
[ "$1" = haxp  ] && ffit=haxp  && echo '  [NO ALMA D2M]  ' && shift
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


echo ''
echo proj is $proj
echo jobs is $jobs
echo ''

# do the haxp thing which is similar to the swin case common to
# both pathways above...however we need some additional setup to
# work in the tarball directory rather than the v$vers dir
$haxp && {
    cd ../v${vers}tb
    $echo pwd
    [ -h $exp.codes ] || [ $echo = echo ] ||
        ln -s ../v${vers}p${iter}/$exp.codes .
    $echo ls -l $exp.codes
    newjobs=''
    for j in $jobs ; do newjobs="$newjobs ../v${vers}/$j" ; done
    $echo \
    $ehtc/ehtc-tarballs.sh tar=haxp \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv \
        dest=./tarballs target=$targ jobs $newjobs
    cd ../v${vers}p${iter}
    $echo pwd
    exit 0
}

# do the project-based dance for the routine stuff
#   na:   post-corr, fits, hops, 2x 4fit
#   alma: post-alma, post-corr, 2x 4fit
[ $proj = na ] && {
    # non-ALMA case
    $echo \
    cd ../v${vers}tb
    $echo pwd
    $echo \
    $ehtc/ehtc-tarballs.sh tar=post-corr \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv \
        dest=./tarballs src=$dout jobs $jobs
    $echo \
    cd ../v${vers}p${iter}
    $echo pwd
    $echo \
    $ehtc/ehtc-tarballs.sh tar=fits \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv \
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
        save=true label=$label relv=$relv \
        dest=./tarballs target=$targ jobs $jobs
    $echo \
    cd ../v${vers}tb
    $echo pwd
    $echo \
    $ehtc/ehtc-tarballs.sh tar=post-corr \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv \
        dest=./tarballs src=$dout jobs $jobs
    $echo \
    cd ../v${vers}p${iter}
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

#
# eof
#
