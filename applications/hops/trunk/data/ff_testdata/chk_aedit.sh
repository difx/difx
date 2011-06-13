#!/bin/sh
#
# $Id: chk_aedit.sh 330 2011-06-10 13:32:10Z gbc $
#
# canonical test suite for aedit
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`

#rdir="$DATADIR/2843/321-1701_0552+398"
#targ="0552+398"
time=oifhak

rm -f alist.out
$verb && echo \
alist '??.?.?.'$time && ls -1 ??.?.?.$time
alist ??.?.?.$time 2>/dev/null 1>&2 || {
    echo run chk_baselines.sh first
    exit 1
}

cat > "aedit.in" <<-EOF
	sum 2
	axis snr
	grid 2 3
	dev aedit.ps/vcps
	ed dup snr
	plot
	write alist-aedit.out
	exit
	y
EOF

# aedit insists on having editted something.
cat alist.out alist.out > alist2.out
rm -f alist-aedit.out aedit.ps

$verb && echo \
aedit -f alist2.out \< "aedit.in" \> "aedit.out"
aedit -f alist2.out  < "aedit.in"  > "aedit.out" 2>&1

lines=`cat aedit.out | wc -l` || lines=0
$verb && echo lines is $lines
set -- `ls -s aedit.ps` 0
$verb && echo $@

[ "$lines" -ge 38 -a "$1" -ge 76 ]

#
# eof
#
