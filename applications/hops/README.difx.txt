How to install HOPS from the DiFX tree.
---------------------------------------

Use --withhops on installdifx

You will need to configure your environment with

    . <path-to-HOPS_ROOT>/bin/hops.bash
    Setup HOPS v<version> with HOPS_ROOT=<root> for <architecture>

or alternatively add the following to the difx setup file:

    # HOPS environment variables
    export GS_DEVICE=x11
    export PGPLOT_TYPE=/xw
    export DEF_CONTROL=/dev/null
    export DATADIR=/tmp
    export HOPS_DOCS=${DIFXROOT}/share/hops
    export PROGDOC=${HOPS_DOCS}/vhelp
    export AHELP=${HOPS_DOCS}/vhelp/aedit
    export TEXT=${HOPS_DOCS}/text

How to install HOPS from the DiFX tree otherwise:
-------------------------------------------------

Until HOPS is fully integrated with DiFX, the following should work:

Go to the svn source directory:

    cd ...path.../applications/hops/trunk
    svn info | grep URL
      URL: https://svn.atnf.csiro.au/difx/applications/hops/trunk

Perform these autoconfiguration steps in this source directory:

    libtoolize --force
    aclocal -I m4
    autoconf
    autoheader
    automake -acf
    # autoreconf as necessary

Ignore the messages about missing files and directories.

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

    . <path-to-HOPS_ROOT>/bin/hops.bash
    Setup HOPS v<version> with HOPS_ROOT=<root> for <architecture>

This will define HOPS_ROOT and set up a working environment (path
adjustments and some environment variables).

