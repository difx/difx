#!/bin/bash
#
# check the ps2pdf options
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`cd $srcdir/testdata; pwd`

# HOPS3 test driver uses 77 for SKIP and 99 for ERROR
# github goes nuts with other values than 127
exsk=127 exft=127 exbg=127
[ -n "$HOPS3" ] && $HOPS3 && {
    echo \
    exsk=77 exft=99 exbg=2 PS2PDF=$PS2PDF
    exsk=77 exft=99 exbg=2
}
[ -n "$PS2PDF" ] || { echo ps2pdf not defined; exit $exft; }
[ "$PS2PDF" = '/bin/false' -o "$PS2PDF" = '/usr/bin/false' ] &&
    { echo ps2pdf not available--punt; exit $exsk;}
[ -x "$PS2PDF" ] || { echo bogus PS2PDF $PS2PDF; exit $exbg; }
[ -d 2843 ] || { echo chk_ff_2843.sh has not been run--punt; exit $exsk; }
[ -n "$PS2PDF" ] || { echo ps2pdf not defined; exit $exft; }

$verb && echo \
fplot -p fplot-2843-%02d.ps 2843/321-1701_0552+398/*X*
fplot -p fplot-2843-%02d.ps 2843/321-1701_0552+398/*X*

$verb && echo \
$fourfit -pt -d ps2pdf:ps2pdf-ff-2843-%02d.ps -b AI:S \\ && echo \
    $DATADIR/2843/321-1701_0552+398/0552+398.oifhak \\ && echo \
    set start -3
$fourfit -pt -d ps2pdf:ps2pdf-ff-2843-%02d.ps -b AI:S \
    $DATADIR/2843/321-1701_0552+398/0552+398.oifhak \
    set start -3 2>/dev/null 1>&2

ok=0
[ -f fplot-2843-00.pdf ] && ok=$(($ok + 1)) || echo fplot-2843-00.pdf missing
[ -f fplot-2843-01.pdf ] && ok=$(($ok + 1)) || echo fplot-2843-01.pdf missing
[ -f fplot-2843-02.pdf ] && ok=$(($ok + 1)) || echo fplot-2843-02.pdf missing
[ -f ps2pdf-ff-2843-00.pdf ] && ok=$(($ok + 1)) ||
    echo ps2pdf-ff-2843-00.pdf missing

[ $ok -eq 4 ]
#
# eof
#
