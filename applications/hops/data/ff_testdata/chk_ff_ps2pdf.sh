#!/bin/bash
#
# check the ps2pdf options
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`cd $srcdir/testdata; pwd`

[ -n "$PS2PDF" ] || { echo ps2pdf not defined; exit 99; }
[ "$PS2PDF" = '/bin/false' ] && { echo ps2pdf not available--punt; exit 77; }
[ -x "$PS2PDF" ] || { echo bogus PS2PDF $PS2PDF; exit 2; }

[ -d 2843 ] || { echo chk_ff_2843.sh has not been run--punt; exit 77; }

$verb && echo \
fplot -p fplot-2843-%02d.ps 2843/321-1701_0552+398/*X*
fplot -p fplot-2843-%02d.ps 2843/321-1701_0552+398/*X*

$verb && echo \
fourfit -pt -d ps2pdf:ps2pdf-ff-2843-%02d.ps -b AI:S \\ && echo \
    $DATADIR/2843/321-1701_0552+398/0552+398.oifhak \\ && echo \
    set start -3
fourfit -pt -d ps2pdf:ps2pdf-ff-2843-%02d.ps -b AI:S \
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
