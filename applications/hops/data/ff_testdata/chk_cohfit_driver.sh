#!/bin/bash
#
# exercises alist/fringex/average/cohfit using cohfit_driver.sh
#
verb=false
[ -n "$testverb" ] && verb=true
[ -d "$abs_srcdir" ] || { echo abs_srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || {
    echo sourcing chk_env.sh && . $srcdir/chk_env.sh ; }
echo PATH is $PATH
# export DATADIR=`pwd`

rdir="3769"
[ -d $abs_srcdir/testdata/$rdir ] || {
    echo Missing $rdir data; exit 2;
}
td=$abs_srcdir/testdata/$rdir
$verb && echo Using data in $td

che=`type -p cohfit`
[ -x "$che" ] || { echo No cohfit executable ; exit 77; }
echo cohfit is $che
viable=`cohfit < /dev/null 2>&1 | grep Wrote`
[ "$viable" = "cohfit: Wrote 0 coherence-analyzed output records" ] || {
    echo This is a you-lose cohfit, so we punt ; exit 77; }

driver=`type -p cohfit-driver.sh`
[ -n "$driver" -a -x "$driver" ] || {
    echo executable cohfit-driver.sh not found ; exit 3;
}

expectprods=46
montage=`type -p montage`
[ -n "$montage" -a -x "$montage" ] && havemonty=true || {
    echo have no montage program, will have two fewer products
    expectprods=44
    havemonty=false
}
gnuplot=`type -p gnuplot`
[ -n "$gnuplot" -a -x "$gnuplot" ] && havegplot=true || {
    echo have no gnuplot program, will have many fewer products
    expectprods=32
    havegplot=false
}

case "$testverb" in
0)  msglv="" ;;
1)  msglv="msglev=1"  ;;
2)  msglv="msglev=0"  ;;
3)  msglv="msglev=-1" ;;
esac

# do the work in a sub-directory
rm -rf cohdrv
mkdir cohdrv
$verb && echo \
$driver expn=3769 cdir=$td verb=$verb $msglv \\ && echo
    tag=w iarg=2:50:2 wdir=cohdrv exam=dets-%d.data
$driver expn=3769 cdir=$td verb=$verb $msglv \
    tag=w iarg=2:50:2 wdir=cohdrv exam=dets-%d.data
status=$?

products=`ls -1 cohdrv 2>&- | wc -l`
[ "$products" -eq $expectprods ] || {
    echo missing products, have $products, want $expectprods ; exit 5;
}

$verb && echo output products directory cohdrv: && ls -l cohdrv && echo

data="cohdrv/w.alist cohdrv/w.fringex cohdrv/w.coavg cohdrv/w.cohfit"
lines=`wc -l $data | grep total | tr -s ' ' | cut -f2 -d' '`
[ "$lines" -eq 6061 ] || {
    echo missing alines, have $lines, want 6061 ; exit 6;
}

# no pdfs if cannot run gnuplot and fewer without montage
pdfcnt=`ls cohdrv/*pdf | wc -l`
pdfexp=1        # the PGPLOT result
$havegplot && {
    $havemonty && pdfexp=15 || pdfexp=13
}
echo have $pdfcnt PDF files, expected $pdfexp

# only if have montage
$havemonty && {
    [ -f cohdrv/w-dets-4.montage.pdf -a -f cohdrv/w-dets-12.montage.pdf ] || {
        echo missing montage PDF files; exit 7;
    }
}

[ -f cohdrv/w-dets-4.summary.txt -a -f cohdrv/w-dets-12.summary.txt ] || {
    echo missing summary text files; exit 8;
}
cat cohdrv/w-dets-4.summary.txt cohdrv/w-dets-12.summary.txt |\
grep -v 'workdir:' > cohdrv/summary-check.txt
cks=`cksum cohdrv/summary-check.txt`
echo cksum of cohdrv/summary-check.txt is $cks
echo cksum of cohdrv/summary-check.txt is 1286553992 6343 is expected

[ "$cks" = '1286553992 6343 cohdrv/summary-check.txt' ] || {
    echo WARNING: numerical stability issues in summary results
}
set -- `wc -l cohdrv/summary-check.txt`
echo expect at least 200 lines
[ "$1" -ge 200 ] || { echo only have $1 lines ; exit 9 ; }

echo "status 0 from the driver is $status"
exit $?
#
# eof
#
