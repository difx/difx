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
#
echo=${1-'help'}
ffit=true

[ $echo = 'help' ] && { echo read the script ; exit 1 ; }
[ $echo = 'echo' -o $echo = 'eval' ] || {
    echo read the script: $echo ; exit 1 ; }
shift

[ $# -ge 1 ] || { echo no jobs, no work, not quite.... ; exit 0 ; }

# only one of these, please
[ "$1" = true  ] && ffit=true && echo '  [WITH FOURFIT] ' && shift
[ "$1" = false ] && ffit=false && echo '  [NO FOURFIT] ' && shift

[ -z "$exp"   ] && { echo exp   must be defined ; exit 1 ; }
[ -z "$vers"  ] && { echo vers  must be defined ; exit 1 ; }
[ -z "$subv"  ] && { echo subv  must be defined ; exit 1 ; }
[ -z "$iter"  ] && { echo iter  must be defined ; exit 1 ; }
[ -z "$expn"  ] && { echo expn  must be defined ; exit 1 ; }
[ -z "$label" ] && { echo label must be defined ; exit 1 ; }
[ -z "$targ"  ] && { echo targ  must be defined ; exit 1 ; }
[ -z "$dout"  ] && { echo dout  must be defined ; exit 1 ; }
[ -z "$proj"  ] && { echo proj  must be defined ; exit 1 ; }
[ "$ffit" = 'true' -o "$ffit" = 'false' ] || { echo prog error ; exit 1; }

jobs="$@"


echo ''
echo proj is $proj
echo jobs is $jobs
echo ''

[ $proj = na ] && {
    # non-ALMA case
    $echo \
    cd ../v${vers}tb
    $echo pwd
    $echo \
    $ehtc/ehtc-tarballs.sh tar=post-corr \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label \
        dest=./tarballs src=$dout jobs $jobs
    $echo \
    cd ../v${vers}p${iter}
    $echo pwd
    $echo \
    $ehtc/ehtc-tarballs.sh tar=fits \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label \
        dest=./tarballs target=$targ jobs $jobs
    $echo \
    $ehtc/ehtc-tarballs.sh tar=hops \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label \
        dest=./tarballs target=$targ jobs $jobs
    $ffit && $echo \
    $ehtc/ehtc-tarballs.sh tar=4fit \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label \
        dest=./tarballs target=$targ jobs $jobs
    $ffit && $echo \
    $ehtc/ehtc-tarballs.sh tar=4fit \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label \
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
        '    'exp=\$exp vers=\$vers subv=\$subv \
        expn=\$expn nuke=true over=true \\ && echo \
        '    'save=true label=\$label dest=./tarballs target=\$targ jobs \$jobs
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
        save=true label=$label \
        dest=./tarballs target=$targ jobs $jobs
    $echo \
    cd ../v${vers}tb
    $echo pwd
    $echo \
    $ehtc/ehtc-tarballs.sh tar=post-corr \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label \
        dest=./tarballs src=$dout jobs $jobs
    $echo \
    cd ../v${vers}p${iter}
    $echo pwd
    $ffit && $echo \
    $ehtc/ehtc-tarballs.sh tar=4fit \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label \
        dest=./tarballs target=$targ jobs $jobs
    # no need for pc phases
    $ffit && $echo \
    $ehtc/ehtc-tarballs.sh tar=4fit \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label \
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
        '    'exp=\$exp vers=\$vers subv=\$subv \
        expn=\$expn nuke=true over=true \\ && echo \
        '    'save=true label=\$label dest=./tarballs target=\$targ jobs \$jobs
        echo '============================================================'
        echo
    }
    true
}

#
# eof
#
