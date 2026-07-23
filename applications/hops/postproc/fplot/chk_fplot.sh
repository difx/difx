#!/bin/bash
#
# $Id: chk_fplot.sh 4080 2023-10-01 15:13:21Z gbc $
#
# local test of fplot (simplified version of chk_ff_dfplot.sh)
#
expn=3562
scan='141-0002'
verb=false
[ -n "$testverb" ] && verb=true

[ -d "$abs_top_srcdir" ] ||
    { echo srcdir not set; exit 1; }
export DATADIR=$abs_top_srcdir/data/ff_testdata/testdata

[ -d $DATADIR/$expn ] ||
    { echo skipping since we have no data ; exit 77; }
$verb && echo have data in $DATADIR/$expn

[ -d $DATADIR/$expn/$scan ] ||
    { echo skipping since we have no data in scan dir ; exit 77; }
$verb && echo have data in $DATADIR/$expn/$scan

[ -x ./fplot ] ||
    { echo have no executable fplot here ; exit 1; }
$verb && echo have an executable fplot here

echo \
./fplot -d diskfile:foo-%d-%B-%P-%F.ps $DATADIR/$expn/$scan
./fplot -d diskfile:foo-%d-%B-%P-%F.ps $DATADIR/$expn/$scan ||
    { echo failed to run diskfile... ; exit 2; }
ls foo*.ps || { echo no ps files generated ; exit 3 ; }
$verb && echo passed diskfile:...

echo \
./fplot -d ps2pdf:bar-%d-%B-%P-%F.ps $DATADIR/$expn/$scan
./fplot -d ps2pdf:bar-%d-%B-%P-%F.ps $DATADIR/$expn/$scan ||
    { echo failed to run ps2pdf... ; exit 4; }
ls bar*.pdf || { echo no pdf files generated ; exit 5 ; }
$verb && echo passed ps2pdf:...

exit 0
#
# eof
#
