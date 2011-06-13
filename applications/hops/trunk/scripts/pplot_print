#!/bin/sh
#########################################################################
[ "$MK4_PRINTER" = "@PRINTER@" -a -n "$PRINTER" ] && MK4_PRINTER=$PRINTER
[ -z "$MK4_PRINTER" -a -n "$PRINTER" ] && MK4_PRINTER=$PRINTER
[ -z "$MK4_PRINTER" -o "$MK4_PRINTER" = "@PRINTER@" ] && {
    ME=`basename $0`
    echo "${ME}: Define MK4_PRINTER (or PRINTER) as a valid printer" >&2
    echo "${ME}: in your environment if you wish to print the plot." >&2
    exit 1
}
#
lpr -P$MK4_PRINTER $1
# eof
