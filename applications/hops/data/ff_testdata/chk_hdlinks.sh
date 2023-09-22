#!/bin/sh
#
# $Id: chk_hdlinks.sh 325 2011-06-09 18:56:07Z gbc $
#
# Script to check hops_data_links.pl
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`

[ -f alist.out ] || { echo run chk_alist.sh first; exit 2; }

#rdir="$DATADIR/2843/321-1701_0552+398"

rm -rf alist-hdl.out datadir
$verb && echo \
hops_data_links.pl -a alist.out -s . -d datadir \> hdl.out 2\>\&1
hops_data_links.pl -a alist.out -s . -d datadir  > hdl.out  2>&1

[ -d datadir ] || { echo no datadir was made; exit 3; }
cd datadir
$verb && echo \
alist -o ../alist-hdl.out 2843 \> hdl-alist.out 2\>\&1
alist -o ../alist-hdl.out 2843 >  hdl-alist.out  2>&1
cd ..

set -- `wc -l alist.out alist-hdl.out`
$verb && echo word counts "$@"

[ "$1" -eq "$3" ]

#
# eof
#
