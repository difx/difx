#!/bin/bash
#
# an alternative to ehtc-postdrive.sh that runs the
# mixed-mode packaging (pre-alma option to tarballs.sh)
# 
# Accepted usage:

#  $ehtc/ehtc-postdrive.sh help
#  $ehtc/ehtc-postdrive.sh echo $jobs
#  $ehtc/ehtc-postdrive.sh eval $jobs
#  
#
echo=${1-'help'}
#ffit=true

[ $echo = 'help' ] && { echo read the script ; exit 1 ; }
[ $echo = 'echo' -o $echo = 'eval' ] || {
    echo read the script: $echo ; exit 1 ; }
shift

[ $# -ge 1 ] || { echo no jobs, no work, not quite.... ; exit 0 ; }

# these are inputs to the original postdrive, not available here
echo ''
[ "$1" = true  ] && { echo read the script ; exit 1 ; }
[ "$1" = false ] && { echo read the script ; exit 1 ; }
[ "$1" = haxp  ] && { echo read the script ; exit 1 ; }

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
#[ "$ffit" = 'true' -o "$ffit" = 'false' ] || { echo prog error ffit; exit 1; }
#[ "$haxp" = 'true' -o "$haxp" = 'false' ] || { echo prog error haxp; exit 1; }

jobs="$@"

# workdir is presumably one of the polconvert postprocessing dirs
# while dout is where the correlator DiFX output sits
# tbdir is safe place for postprocessing
workdir=`pwd`
tbdir=$workdir/tbdir
[ -d $tbdir ] || mkdir $tbdir
[ -d $tbdir/logs ] || mkdir $tbdir/logs
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
# specifies a post-fourfit summary-plotter job to be run
#[ -n "$aeditjob" ] && ae=aeditjob=$aeditjob || ae=''

# pre-alma is dxin swin hmix fmix
# use the $dout as the source for the files to package
# tarballs are built in the $workdir/tarballs folder
true && {
    $echo \
    cd $workdir
    $echo pwd
    $echo \
    $ehtc/ehtc-tarballs.sh tar=pre-alma \
        exp=$exp vers=$vers subv=$subv \
        expn=$expn nuke=true over=true \
        save=true label=$label relv=$relv \
        dest=$workdir/tarballs src=$dout jobs $jobs

    cd $workdir
    $echo pwd
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
