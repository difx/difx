#!/bin/sh
#
# When tarballs are unloaded, the files might not be in the
# internal locations specified by the difx files.
# This script does a simple substitution.
#
from=$1
into=$2

[ -n "$from" -a -n "$into" ] && echo transforming &&
    for f in *.calc *.input *joblist
    do
        mv $f $f.orig
        sed s+$from+$into+ $f.orig > $f
    done
[ -z "$from" -a -z "$into" ] && echo restoring &&
    for f in *.calc *.input *joblist
    do
        mv $f.orig $f
    done

#
# eof
#
