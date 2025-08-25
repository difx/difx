#!/bin/bash
#
# Incantation to list all the source tree files git is ignoring
#
USAGE="Usage: $0 [--help|nuke|list] ...

Without an argument, the current repo is searched for files that
git is currently ignoring and a short report is prepared.  Otherwise

    list    will print the full list
    nuke    will (silently) delete them

additional arguments are passed to git ls-files ... ; e.g. paths
"
# parse args
nuke=false list=false
[ $# -eq 0 ] && set -- ok
case $1 in
ok)                          ;;
nuke)   nuke=true            ;;
list)   list=true            ;;
*)      echo "$USAGE"; exit 2;;
esac
[ $# -eq 1 ] && shift

# do the basic work
zombies=`git ls-files --others --ignored --exclude-standard $@`
[ -z "$zombies" ] && echo No ignored files were found && exit 0
$nuke && rm $zombies && echo The ignored files were removed && exit 0
$list && echo $zombies | tr ' ' \\012 && exit 0

# provide the no-argument report
crowd=`echo "$zombies" | wc -l`
echo There are $crowd files in this tree that git is ignoring.
names=`for zzz in $zombies ; do basename $zzz; done | sort | uniq`
echo
echo They have names such as these:
echo $names | fold -sw 60 | sed 's/^/    /'
echo
echo "If you want to see them removed, re-run with 'nuke' as an argument."
echo 
echo "If you want to do other things, this is the relevant git command:"
echo
echo "  git ls-files --others --ignored --exclude-standard"
echo
echo to start you on your way....
echo
#
# eof
#
