#!/bin/bash
#
# Script to verify GSL fit example
#
[ -f 'exminimizer.log' ] || {
    echo No execution of exminimizer present.  Skipping out.
    exit 77
}
[ -z "$srcdir" ] && {
    echo no srcdir provided, this is a serious issue
    exit 99
}
grep -v PASS exminimizer.log |\
cmp - $srcdir/gslcheck/exminimizer.output  && {
    echo compiled output agrees with source output || {
        echo somethint is wrong
        exit 1
    }
}

echo good.
exit 0
#
# eof vim:nospell
#
