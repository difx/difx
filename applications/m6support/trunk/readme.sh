#!/bin/bash
#
# This tarball contains a few utililities for testing Mark6 recorders.
# For the convenience of the true slacker, you can just execute this file.
#
# $Id: readme.sh.in 2045 2014-05-24 16:18:09Z gbc $
# -----------------------------------------------------------------------------
# (c) Massachusetts Institute of Technology, 2013,2014
#
# Everything in this package is free sofware which you may
# use and modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3.0 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
# -----------------------------------------------------------------------------
# This license is found in the tarball in lgpl-3.0.txt.
#
# The tarball may be untarred anywhere; then you can build and
# installed it in $HOME with the following commands.  Since you
# are probably lazy and too busy to read anything, just execute
# this file and you may do just fine.

# You will need to have installed fuse, fuse-libs and fuse-devel
# (the libraries to link to, the fuse-devel to compile).

# untar:
# tar zxf m6support-0.14.tar.gz
# cd m6support-0.14

[ $# -eq 0 ] && {

# configure:
aclocal
autoconf
autoheader
automake -a
./configure --prefix=$HOME
#/configure --prefix=/usr/local --enable-fuse  

# build, check install and test
make all
#make check
make install
# make test

}

#
# Basic help:
#
cat <<-EOF

This package provides supporting utilities and testing scripts which
are not part of the core Mark6 product.  After installation, you should
have available

    vdifuse         a fuse application providing a filesystem which
                    hides the underlying details of the scatter-gather
                    filesystem

    scan_check      similar to cplane intrinsic scan check, but with
                    more detailed output available

    vdif_time       a simple time conversion utility

and several test scripts:

    hammer.sh       a script to condition a module (verify r/w performance)
    hammerplot.sh   a script to process output from hammer.sh (only if
                    you have gnuplot installed, otherwise roll your own)

Each of these scripts/programs supports --help for more detailed usage.

configure --help=short provides details on some configuration options.

EOF

# dualtest.sh

exit 0

#
# eof
#
