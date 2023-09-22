#!/bin/bash
#
# $Id: chk_fsumm.sh 1387 2016-08-02 21:06:10Z gbc $
#
# simple test on versions
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`

samples=${srcdir}/testdata
jobset1='jobs-339-394'
jobset2='jobs-395-422'

# required collection
[ -f $samples/$jobset1-v6.fsumm ] || {
    echo data file $samples/$jobset1-v6.fsumm is missing
}
jobs="$jobset1"

# optional collection
[ -f $samples/$jobset2-v6.fsumm ] && jobs="$jobs $jobset2"

errs=0

for jj in $jobs
do
    rm -f $jj-v5.fsumm $jj-v6.fsumm $jj-v5.tsumm $jj-v6.tsumm
    # generate some fringe files
    aedit -f $samples/$jj-v6.fsumm <<....EOF
    batch
    close
    outversion 5
    write $jj-v5.fsumm
    twrite $jj-v5.tsumm
    outversion 6
    write $jj-v6.fsumm
    twrite $jj-v6.tsumm
    quit
....EOF

    # compare against captive fringe copies
    grep -v ' file processed by ' $jj-v5.fsumm > $jj-v5-x.fsumm
    grep -v ' file processed by ' $samples/$jj-v5.fsumm > $jj-v5-y.fsumm
    $verb && echo comparing $jj-v5-x.fsumm $jj-v5-y.fsumm
    cmp $jj-v5-x.fsumm $jj-v5-y.fsumm || {
        echo $jj-v5-x.fsumm $jj-v5-y.fsumm differ
        errs=$(($errs + 1))
    }
    grep -v ' file processed by ' $jj-v6.fsumm > $jj-v6-x.fsumm
    grep -v ' file processed by ' $samples/$jj-v6.fsumm > $jj-v6-y.fsumm
    $verb && echo comparing $jj-v6-x.fsumm $jj-v6-y.fsumm
    cmp $jj-v6-x.fsumm $jj-v6-y.fsumm || {
        echo $jj-v6-x.fsumm $jj-v6-y.fsumm differ
        errs=$(($errs + 1))
    }

    # compare against captive triangle copies
    grep -v ' file processed by ' $jj-v5.tsumm > $jj-v5-x.tsumm
    grep -v ' file processed by ' $samples/$jj-v5.tsumm > $jj-v5-y.tsumm
    $verb && echo comparing $jj-v5-x.tsumm $jj-v5-y.tsumm
    cmp $jj-v5-x.tsumm $jj-v5-y.tsumm || {
        echo $jj-v5-x.tsumm $jj-v5-y.tsumm differ
        errs=$(($errs + 1))
    }
    grep -v ' file processed by ' $jj-v6.tsumm > $jj-v6-x.tsumm
    grep -v ' file processed by ' $samples/$jj-v6.tsumm > $jj-v6-y.tsumm
    $verb && echo comparing $jj-v6-x.tsumm $jj-v6-y.tsumm
    cmp $jj-v6-x.tsumm $jj-v6-y.tsumm || {
        echo $jj-v6-x.tsumm $jj-v6-y.tsumm differ
        errs=$(($errs + 1))
    }

done

exit $errs

#
# eof
#
