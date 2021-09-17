#!/bin/bash

echo "Running the GNU autotools"


if [ -z $DIFXROOT ]; then
    echo "\$DIFXROOT not set. Please first source setup file"
    exit 1
fi

aclocal -I m4
if [[ "$OSTYPE" == "darwin"* ]]; then
    glibtoolize --copy --force
else
    libtoolize --copy --force
fi
autoconf
autoheader
automake -acf

./configure --prefix=$DIFXROOT

echo
echo "Bootstrap should be complete. If no errors, now run make"
echo

