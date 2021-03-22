#!/bin/bash
#
# $Id$
#
# Make pdfs of things.  Useful for the future.
#
npdfs=24

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }

# sine qua non
ps2pdf=`type -p ps2pdf`
[ -x "$ps2pdf" ] || { $verb && echo no ps2pdf to execute; exit 0; }

count=0
for p in `find . -name \*.ps -print`
do
    $verb && echo \
    $ps2pdf $p

    $ps2pdf $p
    $verb && ls -l ${p/.ps/.pdf}
    count=$(($count + 1))
done

$verb && echo processed $count files...was that $npdfs\?
[ $count -eq $npdfs ]
#
# eof
#
