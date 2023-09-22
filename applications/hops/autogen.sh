#!/bin/bash
libtoolize --force
aclocal -I m4
autoconf
autoheader
automake -a --add-missing
