#!/bin/bash
# usage ./postproc.sh datasim 7002 M87
[ $# -eq 3 ] || { echo "Usage: ./postproc.sh expr expNum source"
echo "(e.g. ./postproc.sh datasim 7002 M87)" ; exit 1 ; }

# remove directory .difx and expNum if exist
[ -d $1_$2.difx ] && { rm -rf $1_$2.difx; echo delete $1_$2.difx directory ...; }
[ -d $2 ] && { rm -rf $2; echo delete directory $2; }

cp $HOME/data/rundifx/default.machines $1_$2.machines
cp $HOME/data/rundifx/default.threads $1_$2.threads
mpirun --machinefile $1_$2.machines -np 5 mpifxcorr $1_$2.input
#startdifx -n -f $1_$2.input
[ -d $1_$2.difx ] || { echo No .difx dir; exit 1; }
ff=`type -p fourfit`
[ -z "$ff" ] && {
    echo no fourfit to run
    exit 1
} || {
    echo Postprocessing to $2/075-0558 with
    echo $ff
    difx2mark4 -v -e $2 $1_$2
    root=`echo $2/075-0558/$3.* | sed "s/.*$3.//"`
    [ -s "$2/075-0558/$3.$root" ] || { echo No Root; exit 1; }
    echo '' ; echo Root is $root ; echo ''
    [ -z "x-m2 -c" ] || {
    # beg hacking
    ff_opts_x="-m2 -c cf-$2"

    cat > cf-$2 <<..EOF
    optimize_closure true
    * other things...
..EOF
    echo \
    $ff $ff_opts_x $2/075-0558/$3.$root 
    $ff $ff_opts_x $2/075-0558/$3.$root
    # end hacking
  } 
#[ -z "`type psmerge `" -o -z "`type ps2pdf`" ] && {
#  echo no psmerge or ps2pdf
#  exit 2
#    } || {
#  echo Making a summary pdf file
  pushd $2/075-0558
  for d in ??.?.*.$root
        do fplot -d $d.ps $d ; ps2pdf $d.ps; ls -l $d.pdf ; done
#  psmerge -o$3.ps *.$root.ps
#  ps2pdf $3.ps
  pdftk *.pdf cat output $2.pdf
  popd
#  ls -l $2/075-0558/$3.pdf | cut -c24-
#    }
}
exit 0
# eof

