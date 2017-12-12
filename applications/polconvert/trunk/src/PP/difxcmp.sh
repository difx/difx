#!/bin/sh
#
# script to assist reconciliation with a difx trunk
#
[ -z "$bzr" ] && bzr=/home/gbc/PolConvert/trunk
[ -z "$dfx" ] && dfx=/swc/difx/difx-svn/applications/polconvert/trunk/src
[ -n "$bzr" -a -d "$bzr" ] || echo define bzr to source
[ -n "$dfx" -a -d "$dfx" ] || echo define dfx to destination

action=$1 ; shift

for f
do case $action in
dir)
    ls -ld $bzr
    ls -ld $dfx
    ;;
ls)
    ls -l $bzr/$f $dfx/$f 2>&- | sed -e "s+$bzr+\$bzr+" -e "s+$dfx+\$dfx+"
    ;;
diff)
    diff $bzr/$f $dfx/$f
    ;;
cmp)
    cmp $bzr/$f $dfx/$f
    ;;
dcp)
    [ -f $dfx/$f ] || { echo \#\#\# skipping $f ; continue ; }
    cmp $bzr/$f $dfx/$f 2>&- 1>&- || cp -p $bzr/$f $dfx/$f
    ;;
dget)
    [ -f $bzr/$f ] || { echo \#\#\# skipping $f ; continue ; }
    cmp $dfx/$f $bzr/$f 2>&- 1>&- || cp -p $dfx/$f $bzr/$f
    ;;
cp)
    cp -p $bzr/$f $dfx/$f
    ;;
get)
    cp -p $dfx/$f $bzr/$f
    ;;
*)  cat <<....EOF
    action $action is not supported
    legal actions are
    dir  -- list dirs
    ls   -- list the files
    cmp  -- run cmp
    diff -- run diff
    cp   -- bzr to difx
    get  -- difx to bzr
    dcp  -- diff and cp files different
    dget -- diff and cp files different
....EOF
    exit 1
    ;;
esac ; done

#
# eof
#
