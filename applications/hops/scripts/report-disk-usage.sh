#!/bin/bash
#
# $Id: report-disk-usage.sh 4072 2023-09-30 20:46:22Z gbc $
#
# widget for reviewing disk usage
#
USAGE="Usage: $0 module [auto=true|false]

provides a list of total size, size, filename for the named
module (use . for all files), and exclude=re-expression may
be put in the environment to exclude things, e.g.

exclude='swc/|tar.gz' scripts/report-disk-usage.sh | head

if auto is in the environment and true, then arguments are ignored
and are taken from MODULE, abs_top_srcdir and exclude (as is done
in the check test.)
"
[ $# -eq 1 -a ${1-'not'} = '--help' ] && { echo "$USAGE" ; exit 0; }

[ -n "$auto" ] && {
    module=${MODULE-'.'}
    auto=$auto
    dir=${abs_top_srcdir='.'}/
    lines=10
} || {
    module=${1-'.'}
    auto=${2-'false'}
    dir='./'
    lines=100000
}
exclude=${exclude-'aslfjkajfdsadajkf'}

find $dir -name .svn -prune -o -type f -ls |\
grep -vE "$exclude" |\
awk '{printf "%8d %s",$7,$11;print ""}' |\
sed "s,$dir,," |\
sort -nr |\
grep $module |\
awk "NR < $lines"'{t+=$1;printf "%9d %8d %-50.50s...",t,$1,$2;print ""}'
echo

# never fail
exit 0
#
# eof
#
