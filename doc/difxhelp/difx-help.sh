#!/bin/bash
#
# Simple script to exercise every tool in $DIFXROOT/bin and try to
# extract some form of help from it.  With no arguments, the script
# will process all of $DIFXROOT/bin; otherwise it will process only
# specific files.
#
# Note that it is dangerous to execute this script for any process
# that goes and does work with no arguments.  Such programs should
# mentioned in the exceptions list.
#
# ./difx-help.sh 2>&1 | tee difx-help.chatter
#
verb=true
USAGE="[exceptions='list' $0 --help|--version [--output file] [binaries]"

# allow exceptions to be overridden
[ -z "$exceptions" ] &&
exceptions='
addMark5b.pl addVDIF.pl calcsig CalcServer checkCalcServer checkdir day2mjd
dedisperse_difx difx2profile dir2filelist.pl env_check errormon2
extractVDIFThreads filelist2human.pl fillerfrac.sh fourphase glitchplot.sh
guiServer hammer.sh hammerplot.sh libmark6sg listmedia m5fb_makeheader
m5findformats mk5display m6sg_blocknrs m6sg_test1 m6sg_test2 makefits
mk62v2d mk6summary mpifxcorr neuteredmpifxcorr padCalcScans parseDiFX.py
peekVDIF printDiFX.py probemachine restartdifx sec2time.pl startCalcServer
startcorr.pl stopmpifxcorr test5b testdifxmessagedrivestats
testdifxmessagereceivecond testdifxmessagesend testm5lock testparsedifx
teststringarray testtcal test_unpacker time2sec.pl time.pl tpcplot
updatedifxclock update_eop vex2setup.pl vhelper.sh vhelp vhelp.sh
'

[ -z "$DIFXROOT" ] && { echo You must set DIFXROOT ; exit 1 ; }
[ $# -eq 0 ] && set -- `ls -1 $DIFXROOT/bin/*`

a2ps=`type -p a2ps`
[ -x $a2ps ] || { echo the a2ps utility is required ; exit 2 ; }
psjoin=`type -p psjoin`
[ -x $psjoin ] || { echo the psjoin utility is required ; exit 2 ; }
ps2pdf=`type -p ps2pdf`
[ -x $ps2pdf ] || { echo the ps2pdf utility is required ; exit 2 ; }

[ "$1" == '--help' ] && { echo "$USAGE" ; exit 0 ; }
[ "$1" == '--version' ] && { echo "$Id$" ; exit 0 ; }

# some useful options for testing
[ "$1" == '--verb' ] && { verb=$2 ; shift 2 ; }
$verb && echo being verbose
outfile='difx-help.ps'
[ "$1" == '--output' ] && { outfile=$2 ; shift 2 ; }
$verb && echo output is to $outfile

tmpdir=${outfile/.ps/.tmp}
[ -d $tmpdir ] && { echo $tmpdir exists ; exit 3 ; }
mkdir -p $tmpdir || { echo unable to make $tmpdir ; exit 4 ; }
$verb || trap "rm -rf $tmpdir" 0 1 2 15

pslist=''
for exec
do
    b=`basename $exec`
    
    # obvious reasons to move on
    [ -x $exec ] || { echo $b not executable ; continue ; }
    pyc=`expr $b : '.*.pyc$'`
    [ "$pyc" -ge 5 ] && { echo $b is a pyc, skipping ; continue ; }

    # skip listed exceptions
    for e in $exceptions
    do
        [ $b = $e ] && {
            $verb && echo skipping $b
            exec='skip' && break
        }
    done
    [ $exec = 'skip' ] && continue

    $verb && echo testing $b
    # sleep is to prevent hangs from tools that go do something
    short=`($exec -h 2>&1 & sleep 1 ; kill $! 2>&-)`
    hlong=`($exec --help 2>&1 & sleep 1 ; kill $! 2>&-)`
    shortlen=`echo "$short" | wc -c`
    hlonglen=`echo "$hlong" | wc -c`
    $verb && echo -n ' ' $shortlen / $hlonglen

    # if they are unequal, provide both
    [ $shortlen -ne $hlonglen ] && (
        echo $b -h
        echo ''
        echo "$short"
        echo 
        echo '===================================================='
        echo 
        echo $b --help
        echo ''
        echo "$hlong"
    ) | $a2ps -1 -o $tmpdir/$b.ps -B --footer="$b" 2>&1 | grep Total |\
    sed 's/.*into the//'
    
    # if they are equal, provide hlong
    [ $shortlen -eq $hlonglen ] && (
        echo $b --help
        echo ''
        echo "$hlong"
    ) | $a2ps -1 -o $tmpdir/$b.ps -B --footer="$b" 2>&1 | grep Total |\
    sed 's/.*into the//'

    [ -f log ] && rm -f log && echo log removed

    pslist="$pslist $tmpdir/$b.ps"
done
$psjoin $pslist > $outfile
$ps2pdf $outfile
ls -l ${outfile/.ps/.pdf}

$verb && echo before running again: && echo rm -rf $tmpdir

#
# eof
#
