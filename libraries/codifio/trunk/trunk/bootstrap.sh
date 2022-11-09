#!/bin/sh

echo "Running the GNU autotools"


if [ -z $DIFXROOT ]; then
    echo "\$DIFXROOT not set. Please first source setup file"
    exit 1
fi

aclocal
libtoolize --copy --force
autoconf
autoheader
automake -a -c

./configure --prefix=$DIFXROOT

echo
echo "Bootstrap should be complete. If no errors, now run make"
echo

