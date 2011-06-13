#!/bin/sh
#
# $Id: tst_fourfit.sh 320 2011-06-08 15:01:54Z gbc $
#
# Various tests of captured scans for fourfit.
#

verb=false
[ -n "$testverb" ] && verb=true
$verb && set -x

[ -z "$EXPERIMENTS" ] && EXPERIMENTS='2836 2912 2849pp 2849pk'
[ -n "$srcdir" -a -d "$srcdir" ] && cd $srcdir

[ -d testdata ] || {
    echo sorry, you are in the wrong place to run this script
    echo you need to be in the data/ff_testdata directory
    exit 1
}

[ -n "$DISPLAY" ] && export GS_DEVICE=x11 || {
    echo sorry, DISPLAY is undefined, and this is really a
    echo test of X11 display capabilities.
    exit 2
}

for t in $EXPERIMENTS
do
  echo ''
  echo experiment $t -- hit return '(in this window)' after you see the plot.
  echo ''
  case $t in
    2836)   # once upon a time this was (on different architectures):
	    # testfourfit.sh, testlinux64-a2, testlinux.dist, testsuite
	[ -d testdata/2836 ] || { echo no data for this test; continue; }
	fourfit -pt -bAE:X -c testdata/2836/cf2836 \
	    testdata/2836/scan001/2145+067.olomfh
	;;
    2912)   # once upon a time this was testsuite2
	[ -d testdata/2912 ] || { echo no data for this test; continue; }
	fourfit -pt -bAE:X -c testdata/2912/cf_2912 \
	    testdata/2912/253-1907/1128+385.pdzwql
	;;
    2849pp) # once upon a time this was testsuite3
	[ -d testdata/2849 ] || { echo no data for this test; continue; }
	fourfit -pt -m2 -b pp -c testdata/2849/cf_2849 \
	    testdata/2849/297-0311_RCAS/RCAS.oyspzt
	;;
    2849pk) # once upon a time this was testsuite3
	[ -d testdata/2849 ] || { echo no data for this test; continue; }
	fourfit -pt -m2 -b pk -c testdata/2849/cf_2849 \
	    testdata/2849/297-0311_RCAS/RCAS.oyspzt
	;;
    *)	echo no such test $test
	;;
  esac
done

exit 0

#
# eof
#
