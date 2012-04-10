How to install HOPS from the DiFX tree.
---------------------------------------

Until HOPS is fully integrated with DiFX, the following should work:

Go to the svn source directory:

    cd ...path.../applications/hops/trunk
    svn info | grep URL
      URL: https://svn.atnf.csiro.au/difx/applications/hops/trunk

Perform these autoconfiguration steps in this source directory:

    aclocal -I m4
    autoconf
    autoheader
    automake -acf

There should be no messages about missing files and directories.
It is not necessary to define HOPS_ROOT although you may.

You can build the tools anywhere (and delete the build directory
afterwards if you like), e.g.

    mkdir ../build
    cd ../build
    ../trunk/configure

See the README.txt for some help with the various options to configure
if it doesn't immediately work.  Now you can build it:

    make
    make check
    make install

make install should have placed hops.bash in ~/bin if you have one;
thereafter you can source it (from there or wherever else you've put it):

    . ~/bin/hops.bash
    Setup HOPS vX.X with HOPS_ROOT=... for arch-X.X

This will define HOPS_ROOT and set up a working environment (path
adjustments and some environment variables).

