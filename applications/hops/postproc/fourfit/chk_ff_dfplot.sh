#!/bin/bash
#
# $Id: chk_ff_dfplot.sh 4349 2025-04-21 17:49:48Z gbc $
#
# local test of display_fplot, there is additional testing
# in data/ff_testdata/testdata via chk_ff_display.sh.
#
expn=2843

verb=false
[ -n "$testverb" ] && verb=true

# fourfit has many library dependencies that need to be found.
[ -d "$abs_top_builddir" ] ||
    { echo abs_top_builddir not set; exit 1; } 
ldlibpath=`export HOPS_QUIET=x ; . ../../hops.bash ; printenv LD_LIBRARY_PATH`
[ -n "$ldlibpath" ] && export LD_LIBRARY_PATH=$ldlibpath &&
    { echo setting LD_LIBRARY_PATH for fourfit ; echo $LD_LIBRARY_PATH ; }

[ -d "$abs_top_srcdir" ] ||
    { echo abs_top_srcdir not set; exit 1; }
$verb && echo \
export DATADIR=$abs_top_srcdir/data/ff_testdata/testdata
export DATADIR=$abs_top_srcdir/data/ff_testdata/testdata

export HOPS_VEX_DIR=$abs_top_srcdir/sub/vex/text
export DEF_CONTROL=/dev/null

[ -d $DATADIR/$expn ] ||
    { echo skipping since we have no data ; exit 77; }
$verb && echo have data in $DATADIR/$expn

[ -x ./fourfit ] ||
    { echo have no executable fourfit here ; exit 1; }
$verb && echo have an executable fourfit here

$verb && echo \
./fourfit -t -b 'AI:X' $expn
./fourfit -t -b 'AI:X' $expn ||
    { echo unable to fourfit $expn ; exit 2 ; }

echo The error message:
echo "stty: 'standard input': Inappropriate ioctl for device"
echo is normal...we are brutally killing fourfit....

# PSSCREEN and XWINDOW cases require a DISPLAY these days.
[ -n "$DISPLAY" ] && {
    $verb && echo we have a DISPLAY: $DISPLAY
    $verb && echo
    echo q\| ./fourfit -pt -b 'AI:X' $expn
    echo q | ./fourfit -pt -b 'AI:X' $expn
    status=$?; [ $status -eq 0 ] ||
        { echo 'failed page via -p' status=$status ; exit 3 ; }
    echo 'passed PSSCREEN via -p' ; echo
    $verb && echo
    echo q\| ./fourfit -t -d psScreen -b 'AI:X' $expn
    echo q | ./fourfit -t -d psScreen -b 'AI:X' $expn
    status=$?; [ $status -eq 0 ] ||
        { echo 'failed page via -d psScreen' status=$status ; exit 4 ; }
    echo 'passed PSSCREEN via -d psScreen' ; echo
    $verb && echo
    echo q\| ./fourfit -t -d Xwindow -b 'AI:X' $expn
    echo q | ./fourfit -t -d Xwindow -b 'AI:X' $expn
    status=$?; [ $status -eq 0 ] ||
        { echo 'failed page via -d Xwindow' status=$status ; exit 5 ; }
    echo 'passed XWINDOW via -x' ; echo
} || {
    echo 'have no DISPLAY so skipping some tests: PSSCREEN XWINDOW'
}

# MK4_PRINTER cases: manufacture a cheap "printer"
cat > pplot_printer<<\EOF
echo Arguments: "$@"
[ -f $1 ] && ls $1 && exit 0
echo FAILED
exit 1
### commentary follows
EOF
chmod +x pplot_printer
export MK4_PRINTER=./pplot_printer
$verb && echo
echo q\| ./fourfit -t -d psHardCopy -b 'AI:X' $expn
echo q | ./fourfit -t -d psHardCopy -b 'AI:X' $expn
status=$?; [ $status -eq 0 ] ||
    { echo 'failed page via -d psHardCopy' status=$status ; exit 6 ; }
echo 'passed PSHARDCOPY via -d psHardCopy' ; echo

# fake pathway to a non-existent printer
export MK4_PRINTER='no-such-printer'
$verb && echo
echo q\| ./fourfit -t -d hARDcOPY -b 'AI:X' $expn 2\>\>pplot_printer
echo q | ./fourfit -t -d hARDcOPY -b 'AI:X' $expn 2 >> pplot_printer
status=$?; [ $status -eq 0 ] ||
    { echo 'failed page via -d hARDcOPY' status=$status ; exit 7 ; }
grep 'lpr' pplot_printer &&
    echo 'passed HARDCOPY via -d hARDcOPY' ; echo ||
    { echo 'failed a page via -d hARDcOPY with lpr' status=$status ; exit 8 ; }
echo pplot_printer commentary follows:
echo ==================================
sed 1,/commentary.follows/d < pplot_printer
echo ==================================
unset MK4_PRINTER

rm -f 'test-AI-XX-AA.ps'
$verb && echo
echo q\| ./fourfit -t -d diskfile:test-%B-%F-%P.ps -b 'AI:X' $expn
echo q | ./fourfit -t -d diskfile:test-%B-%F-%P.ps -b 'AI:X' $expn
status=$?; [ $status -eq 0 ] ||
    { echo 'failed via -d diskfile:test-%B:%F-%P.ps' status=$status ; exit 9; }
ls -l test-AI-XX-AA.ps
echo passed DISKFILE test-%B-%F-%P.ps, execution
[ -s 'test-AI-XX-AA.ps' ] &&
    echo passed DISKFILE test-%B-%F-%P.ps, file generated ||
    { echo 'failed test-AI:XX-AA.ps non-empty' ; exit 10; }

rm -f 'TEST-AI-XX-AA.p*'
$verb && echo
echo q\| ./fourfit -t -d ps2pdf:TEST-%B-%F-%P.ps -b 'AI:X' $expn
echo q | ./fourfit -t -d ps2pdf:TEST-%B-%F-%P.ps -b 'AI:X' $expn
status=$?; [ $status -eq 0 ] ||
    { echo 'failed via -d ps2pdf:TEST-%B:%F-%P.pdf' status=$status ; exit 9; }
ls -l TEST-AI-XX-AA.p*
echo passed DISKFILE TEST-%B-%F-%P.ps, execution
[ -s 'TEST-AI-XX-AA.pdf' ] &&
    echo passed DISKFILE TEST-%B-%F-%P.pdf, file generated ||
    { echo 'failed TEST-AI:XX-AA.pdf non-empty' ; exit 10; }

exit 0
#
# eof
#
