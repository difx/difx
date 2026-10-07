#!/bin/sh
# Find which wrapper entries in arch_wrappers.inc do not compile for an
# implementation, and write them as ARCH_SKIP_<name> defines.
#
#   find_skips.sh <wrapper.cpp> <skips.h> <architecture header> <compiler and flags...>
#
# Compiles <wrapper.cpp> with an empty skip list, maps each error back to
# its entry in arch_wrappers.inc, skips those entries and repeats until it
# compiles.  Each skip is defined as 2 if the entry's vector* macro is not
# defined at all for that implementation (not provided), or 1 if it is
# defined but does not compile (broken).  <architecture header> is kept as
# an argument for reference; the check uses the preprocessed source.

src=$1
skips=$2
header=$3
shift 3

# gcc reports an error inside a macro at the macro's definition (in the
# header); turning off macro expansion tracking makes it report the line in
# arch_wrappers.inc where the macro is used, as clang does
if [ -z "$("$@" -ftrack-macro-expansion=0 -fsyntax-only -x c++ /dev/null 2>&1)" ]
then
	set -- "$@" -ftrack-macro-expansion=0
fi

: > "$skips"
for pass in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
do
	errs=$("$@" -fsyntax-only "$src" 2>&1 | grep -E "arch_wrappers.inc:[0-9]+:[0-9]+: error" | sed -E 's/.*arch_wrappers.inc:([0-9]+):.*/\1/' | sort -un)
	if [ -z "$errs" ]
	then
		if "$@" -fsyntax-only "$src" > /dev/null 2>&1
		then
			# classify: is the entry's first vector* macro defined at all once
			# the header is preprocessed (only the active architecture counts)?
			"$@" -E -dM "$src" > "$skips.macros" 2>/dev/null
			for name in $(sed -n 's/^#define ARCH_SKIP_\([A-Za-z0-9_]*\).*/\1/p' "$skips")
			do
				mac=$(awk -v N="ARCH_SKIP_$name" '$1 == "#ifndef" && $2 == N { getline; print; exit }' arch_wrappers.inc | grep -oE "vector[A-Za-z0-9_]+" | head -1)
				if grep -qE "^#define[[:space:]]+$mac[(]" "$skips.macros"
				then
					echo "#define ARCH_SKIP_$name 1"
				else
					echo "#define ARCH_SKIP_$name 2"
				fi
			done > "$skips.tmp"
			mv "$skips.tmp" "$skips"
			rm -f "$skips.macros"
			exit 0
		fi
		echo "find_skips.sh: $src has errors outside arch_wrappers.inc:" >&2
		"$@" -fsyntax-only "$src" 2>&1 | grep error | head -5 >&2
		exit 1
	fi
	for line in $errs
	do
		awk -v L="$line" 'NR <= L && /^#ifndef ARCH_SKIP_/ { n = $2 } END { print "#define " n " 1" }' arch_wrappers.inc >> "$skips"
	done
	sort -u "$skips" -o "$skips"
done
echo "find_skips.sh: still failing after 20 passes" >&2
exit 1
