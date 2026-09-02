#!/bin/bash
#
# Simple script to check get_filelist().
# test_get_filelist is a thin wrapper around check_get_filelist()
# and reports some things about what it returns.
#
verb=false dv=+v
dv='+v'
[ -n "$testverb" ] && verb=true && dv=-v
errors=0

tests="#00%-1%.:Makefile%0"
datadir="$abs_top_srcdir/data/ff_testdata/testdata"

# quartets of: testnumber%typetoget%datadir:dir[:dir...]%expectederror
[ -d ${datadir}/2843 ] && tests="$tests #01%1%${datadir}:2843%3"
[ -d ${datadir}/2843 ] && tests="$tests #02%2%${datadir}:2843%0"
[ -d ${datadir}/3562 ] && tests="$tests #03%2%${datadir}:3562%3"

[ $# -gt 0 ] && tests="$@"
for tnumtipepatherre in $tests
do
    # split tnumtipepatherre on %
    tnum=`expr $tnumtipepatherre : '\([^%]*\)%[^%]*%[^%]*%[^%]*$'`
    tipe=`expr $tnumtipepatherre : '[^%]*%\([^%]*\)%[^%]*%[^%]*$'`
    path=`expr $tnumtipepatherre : '[^%]*%[^%]*%\([^%]*\)%[^%]*$'`
    erre=`expr $tnumtipepatherre : '[^%]*%[^%]*%[^%]*%\([^%]*\)$'`
    paths=`echo $path | tr ':' ' '`
    $verb && echo "test '$tnum' type '$tipe' path:" && echo "  '$path'"
    echo \
    ./test_get_filelist $dv $tipe $paths
    ./test_get_filelist $dv $tipe $paths
    errs=$?
    $verb && echo expected exit status $erre got $errs
    [ $errs -eq $erre ] || errors=$(($errors + 1))
    echo
done
$verb && echo total errors $errors
exit $errors
#
# eof
#
