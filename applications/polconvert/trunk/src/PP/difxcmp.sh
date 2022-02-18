#!/bin/bash
#
# script to assist reconciliation with a difx trunk
# Ivan started with git, then went to brz (py3), then finally git.
# The defaults for the paths are consistent with GBC supports at Haystack,
# but you can put git, DFX or dfx into the environment to do this elsewhere.
#
[ -z "$git" ] && git=/home/gbc/PolConvert/PolConvert
[ -z "$tag" ] && tag=''
[ -z "$dxb" ] && {
    [ -z "$tag" ] && dxb=trunk  # branches/py3temp
    [ -n "$tag" ] && dxb=''     # trunk missing on masters
}
[ -z "$DFX" ] && DFX=/swc/difx/difx-svn
[ -z "$dfx" ] && dfx=$DFX/$tag/applications/polconvert/$dxb/src
[ -n "$git" -a -d "$git" ] || echo define git source trunk with \$git=...
[ -n "$DFX" -a -d "$DFX" ] || echo define DiFX source trunk with \$DFX=...
[ -n "$dfx" -a -d "$dfx" ] || echo define full path to DiFX pc src \$dfx=...

action=${1-'help'} ; shift

[ -d .git ] || {
    echo
    echo '' This script is intended to be run from a clone
    echo '' of the PolConvert git repository
    echo '' ' 'https://github.com/marti-vidal-i/PolConvert.git
    echo
    echo '' If you want to do something else, you are on your own...
    echo
    exit 1
}

# things that do not end up in DiFX vendor branch
skip="INSTALL build QA2 TODO.txt setup.py TOP/contents"
skip="$skip __init__.py polconvert_CASA.py polconvert_standalone.py"

[ $# -eq 0 ] && set -- 'no-such-file'

for f
do
  # ignore directories
  [ -d $f ] && continue
  # ignore things that don't go to DiFX but are in git
  punt=false
  for s in $skip
  do [ "$f" = "$s" ] && punt=true ; done
  $punt && echo skipping $f && continue
  F=$f
  [ `expr $F : 'TOP.*'` -ge 3 ] && F=../`basename $f`
  # pretty dodgy way to put TOP things into src
  [ "$F" = "../setup.py" ] && F=setup.py
  [ "$F" = "../polconvert.xml" ] && F=polconvert.xml
  [ "$F" = "../task_polconvert.py" ] && F=task_polconvert.py
  # decide what to do
  case $action in
  dir)
    ls -ld $git
    ls -ld $dfx
    ;;
  cmpls)
    cmp $git/$f $dfx/$F 2>&- ||
    ls -l $git/$f $dfx/$F 2>&- | sed -e "s+$git+\$git+" -e "s+$dfx+\$dfx+"
    ;;
  ls)
    ls -l $git/$f $dfx/$F 2>&- | sed -e "s+$git+\$git+" -e "s+$dfx+\$dfx+"
    ;;
  diff)
    diff $git/$f $dfx/$F || { echo diff $git/$f $dfx/$F ; echo ; }
    ;;
  sdif)
    echo sdiff -lw164 $git/$f $dfx/$F
    sdiff -lw164 $git/$f $dfx/$F
    ;;
  vdif)
    echo vimdiff $git/$f $dfx/$F
    vimdiff $git/$f $dfx/$F
    ;;
  cmp)
    cmp $git/$f $dfx/$F || { echo cmp $git/$f $dfx/$F ; echo ; }
    ;;
  dcp)
    [ -f $dfx/$F ] || { echo \#\#\# skipping $f ; continue ; }
    cmp $git/$f $dfx/$F 2>&- 1>&- || cp -p $git/$f $dfx/$F
    ;;
  dget)
    [ -f $git/$f ] || { echo \#\#\# skipping $f ; continue ; }
    cmp $dfx/$F $git/$f 2>&- 1>&- || cp -p $dfx/$F $git/$f
    ;;
  cp|push)
    cp -p $git/$f $dfx/$F
    ;;
  get|pull)
    cp -p $dfx/$F $git/$f
    ;;
  *)
    [ "$action" = 'help' ] || action $action is not supported
    cat <<....EOF
    Usage: PP/difxcmp.sh action file ...

    Legal actions are
      dir   -- list dirs
      ls    -- list the files
      cmpls -- list the files with cmp issues
      cmp   -- run cmp (and echo cmp cmd if it fails)
      diff  -- run diff ( and echo diff cmd if it fails)
      sdif  -- run sdiff (you will need window width 164)
      vdif  -- run vimdiff (you will need window width 164)
      cp    -- git to difx (i.e. push from git to DiFX)
      push  -- git to difx (i.e. push from git to DiFX)
      get   -- difx to git (i.e. pull from DiFX to git)
      pull  -- difx to git (i.e. pull from DiFX to git)
      dcp   -- push if diff and cp files different
      dget  -- pull if diff and cp files different

    After pull or push actions, you will need to review and commit.

    The wildcards * PP/* TOP/* (or combinations) are useful 
    to consider collections of files.  The hierarchy locations may be
    adjusted with environment variables that default to these:
      (git repo dir) git=$git
      (DiFX svn dir) DFX=$DFX
      (master tag)   tag=$tag
      (DiFX branch)  dxb=$dxb
      dfx=\$DFX/\$tag/applications/polconvert/\$dxb/src

    The EU-VGOS is currently not imported to DiFX.
    The build directory contains specific linux builds.

    The previous bzr, brz repositories are considered obsolete;
      $ git remote -v
      origin  https://github.com/marti-vidal-i/PolConvert.git (fetch)
      origin  https://github.com/marti-vidal-i/PolConvert.git (push)
    is now the master.

....EOF
    exit 1
    ;;
  esac

done

#
# eof
#
