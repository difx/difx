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
cp)
    cp -p $bzr/$f $dfx/$f
    ;;
*)  echo action $action is not supported
    echo legal actions are cmp diff cp dcp ls
    exit 1
    ;;
esac ; done

#
# eof
#
