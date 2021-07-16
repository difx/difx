#!/bin/bash
#
# A simple script that makes some summary plots from an alist.
# Likely this could be made much smarter.
#
export DATADIR=`pwd`
export DISPLAY=''
alist=${1-'no-such-alist'}
lab=${2-''}
# this invocation (as in the Readme) makes one alist of all
[ "$alist" = 'all' -a -n "$ers" -a -n "$expn" ] && {
    # provide a header
    first=`ls -1 */*.alist | head -1`
    head -4 $first > $ers-$expn.alist
    # and all the data, DOY-HHMMSS sorted, and then by BL and pol
    cat */*.alist | grep -v '^*' | sort -k12 -k15 -k18 >> $ers-$expn.alist
    exec $0 $ers-$expn.alist $ers-$expn- > $ers-$expn.errors 2>&1
}
[ -f "$alist" ] || { echo no such alist $alist; exit 1; }
# generate list of useful baselines
awks='NR>4{a=substr($15,1,1);b=substr($15,2,1)}a<b{print a b}b<a{print b a}'
bl=`awk "$awks" $alist | sort | uniq | tr \\\\012 ' '`
# build a set of aedit commands
cat > aeditjob.aedit <<EOF
grid 2 10
batch
base $bl
ed in
sum 2
device amp-time.ps/VCPS
plot amp time
device sbd-time.ps/VCPS
plot sbd time
device drate-time.ps/VCPS
plot drate time
device snr-time.ps/VCPS
plot snr time
exit
EOF
# run aedit
aedit -f $alist < aeditjob.aedit
# make pdfs
pp=`type -p ps2pdf`
[ -x "$pp" ] && for p in amp sbd drate snr ; do
[ -s $p-time.ps ] && $pp $p-time.ps $lab$p-time.pdf ||
    echo $p-time.ps is missing
rm -f $p-time.ps
done
rm -f aeditjob.aedit
#
# eof
#
