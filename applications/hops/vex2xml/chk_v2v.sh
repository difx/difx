#!/bin/bash
#
# $Id$
#
# Test suite for VEX2XML
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh

vexes='
a25gl.vex
b23gl.vex
bm452c.vex
c151a.vex
c151b.vex
c152a.vex
c152b.vex
c22gl.vex
d21us.vex
e16b08.vex
g26us.vex
gbc1.vex
new27.vex
'

pass=0
fail=0
for v in $vexes
do
    $verb && echo testing $v && echo ''
    $verb && echo \
    VEX2XML -in ${srcdir}/testcases/vex/$v \\ && echo \
            -out ./${v/vex/xml} \> ./${v/vex/txt}
    VEX2XML -in ${srcdir}/testcases/vex/$v \
            -out ./${v/vex/xml} > ./${v/vex/txt} 2>&1
    $verb && echo \
    cmp ${srcdir}/testcases/xml/${v/vex/xml} ./${v/vex/xml}
    cmp ${srcdir}/testcases/xml/${v/vex/xml} ./${v/vex/xml} &&
        pass=$(($pass + 1)) ||
        fail=$(($fail + 1))
done

$verb && echo $pass pass and $fail fail

[ $pass -eq 13 -a $fail -eq 0 ]

#
# eof
#
