#!/bin/bash
#
# script to assist reconciliation with a difx trunk
#
[ -z "$bzr" ] && bzr=/home/gbc/PolConvert/trunk
[ -z "$dfx" ] && dfx=/swc/difx/difx-svn/applications/polconvert/trunk/src
[ -n "$bzr" -a -d "$bzr" ] || echo define bzr to source
[ -n "$dfx" -a -d "$dfx" ] || echo define dfx to destination

action=${1-'help'} ; shift

skip='build mytasks.py polconvert_cli.py polconvert.py'
skip="$skip build/temp.linux-x86_64-2.7"
skip="$skip QA2/scriptForCalibrationAPP_C4.py"
skip="$skip result_ALL.txt"

[ $# -eq 0 ] && set -- 'no-such-file'

for f
do
  # ignore directories
  [ -d $f ] && continue
  # ignore things that don't go to DiFX but are in bzr
  punt=false
  for s in $skip
  do [ "$f" = "$s" ] && punt=true ; done
  $punt && echo skipping $f && continue
  F=$f
  [ `expr $F : 'TOP.*'` -ge 3 ] && F=../`basename $f`
  # decide what to do
  case $action in
  dir)
    ls -ld $bzr
    ls -ld $dfx
    ;;
  ls)
    ls -l $bzr/$f $dfx/$F 2>&- | sed -e "s+$bzr+\$bzr+" -e "s+$dfx+\$dfx+"
    ;;
  diff)
    diff $bzr/$f $dfx/$F || echo diff $bzr/$f $dfx/$F
    ;;
  sdiff)
    echo sdiff -lw164 $bzr/$f $dfx/$F
    sdiff -lw164 $bzr/$f $dfx/$F
    ;;
  cmp)
    cmp $bzr/$f $dfx/$F || echo cmp $bzr/$f $dfx/$F
    ;;
  dcp)
    [ -f $dfx/$F ] || { echo \#\#\# skipping $f ; continue ; }
    cmp $bzr/$f $dfx/$F 2>&- 1>&- || cp -p $bzr/$f $dfx/$F
    ;;
  dget)
    [ -f $bzr/$f ] || { echo \#\#\# skipping $f ; continue ; }
    cmp $dfx/$F $bzr/$f 2>&- 1>&- || cp -p $dfx/$F $bzr/$f
    ;;
  cp)
    cp -p $bzr/$f $dfx/$F
    ;;
  get)
    cp -p $dfx/$F $bzr/$f
    ;;
  *)
    [ "$action" = 'help' ] || action $action is not supported
    cat <<....EOF
    Usage: $0 action file ...

    Legal actions are
      dir   -- list dirs
      ls    -- list the files
      cmp   -- run cmp
      diff  -- run diff
      sdiff -- run sdiff (need width 164)
      cp    -- bzr to difx
      get   -- difx to bzr
      dcp   -- diff and cp files different
      dget  -- diff and cp files different

    The wildcards *  PP/* TOP/* (or combinations) are useful 
....EOF
    exit 1
    ;;
  esac

done

#
# eof
#
