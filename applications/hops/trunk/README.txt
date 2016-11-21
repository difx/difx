This is a distribution of HOPS (version 3.13), the
Haystack Observatory Post-processing System software.


0. Quick Install Instructions
-----------------------------

If you haven't already done so, unpack the tarball in a convenient place:

    cd /Somewhere
    tar zxf hops-3.13.tar.gz

and proceed with the standard build process, e.g.:

    mkdir bld-3.13
    cd bld-3.13
    ../hops-3.13/configure
    make install

which installs the tools into a subdirectory of /Somewhere.   You should
use the --prefix=... option of configure if you want to install HOPS in
some other directory (e.g. /usr/local).  If the configure or install fails,
do not despair:  read on below in the next section of these instructions.

A (ba)sh setup script, hops.bash is created in the build directory
(and copied to $HOME/bin if $HOME/bin exists).  You can make an alias:

    alias hops='source $HOME/bin/hops.bash'

and source it to set up your shell environment for HOPS work:

    hops

This script provides some detail on the environment modifications

    hops.bash --help

Some help with the HOPS tools is available with

    vhelp

but this help tool is not fully up-to-date in this release.


1. Additional Install instructions
----------------------------------

The software is installed relative to a directory $HOPS_ROOT,
which is synonymous with the install directory specified by the
--prefix option of configure.  (It defaults to /Somewhere.)
You can define HOPS_ROOT in your environment or pass the assignment
as an argument to configure if you wish to install the HOPS tools
somewhere completely different.  If you do this, then the
/Somewhere directory (i.e. where you unpacked the tarball and
built the tools) can be entirely deleted after `make install`.

You may configure the software to build in the source directory
(i.e. into the directory you untarred the software), but this is
not recommended or required.

The complete software package requires X11, PGPLOT, (possibly) version
1.2 of the PNG library (depending on how PGPLOT was compiled), ghostscript
(usuall compiled with X11 support) and FFTW3 (for FFTs).  Unfortunately
these are often not to be found in the usual places, so configure may need
some help to get it right.

    ../hops-3.13/configure --help=short

provides the (short) help on configure.  So if the default X11 search
didn't work, one of these might find X11 on your system:

    ../hops-3.13/configure --enable-xtrap
    ../hops-3.13/configure --enable-pathx

Likewise, PGPLOT is normally installed in /usr/local/pgplot, but that
may not be what you want:

    ../hops-3.13/configure PGPLOT_DIR=/usr/local/pgplot64

and if those fail, you can try setting more variables, e.g. the
following worked once upon a time  on MacOS with fink support for
all the usual linux tools:

    ../hops-3.13/configure \
	LDFLAGS=-L/usr/X11/lib \
	CC=gcc-4 F77=gfortran \
	X_INSANE='-lobjc -framework Foundation -L/sw/lib -lpng -laquaterm'

In particular the X_INSANE variable is included last in the link step,
(in fact, this gibberish works with the Fink MacOS pgplot package) so
you can put all sorts of things here.  The underlying issue is that
there are too many ways that pgplot might have been compiled.  Lately,
we've had better luck with MacPorts.

Note that if you set PGPLOT_DIR as an environment variable, this directory
should contain (at least) the three files:

    libpgplot.a
    libcpgplot.a
    grfont.dat

and possibly pgxwin_server if you're using X11 (which you almost certainly
are unless you've got your own hacked-up version of PGPLOT).

If you're trying to install this under MacOS, it may compile, but still
suffer from some unidentified issues, so beware.  See the two files

    MacPorts.txt
    Fink.txt

for some help on installing HOPS using those MacOS package systems.

Another general issue is that the automated machinery is not always able
to identify all the flag/linkage information needed by all the fortran
possibilites.  (Fortran is mostly used to compile pgplot, see above.)
E.g.

    ../hops-3.13/configure FLIBS='-lf2c -lgcc

might be needed if f77 is really a wrapper around f2c.

If nothing works, contact gbc@haystack.mit.edu, who will be interested
in your problems.  The file config.summary generated in the top of the
build directory will be useful to pass along.

Once you've successfully configured the package, `make install` will
build and install everything.  If you want to break this into the two
logical steps

    make all
    make install

that's ok.


2. Checking the Installation
----------------------------

There is a set of (non-exhaustive) checks that the tools have installed
correctly.  You can run these with

    make check

at the top of the build directory.  They should be relatively quick
and painless.  The x11 plotting is disabled for these tests, however
you can see some plots quickly flash by with:

    pushd $HOPS_ROOT/bld-3.13/data/ff_testdata
    make check test_gs_device=x11

Additionally, from the same directory

    make check TESTS=tst_fourfit.sh

to verify X11 display capabilities.  Or

    pushd $HOPS_ROOT/hops-3.13/data/ff_testdata
    ./tst_fourfit.sh

or just read the script and type the commands....

3. Other Shells
---------------

If your unix shell isn't bash or sh, you can't source hops.bash directly.
However, you can invoke bash as a sub-shell, and then your normal shell
as a sub-shell of bash.  For example:

    tcsh> bash
    bash$ source ~/bin/hops.bash
    Setup HOPS v3.13 with HOPS_ROOT=/home/gbc/HOPS for i686-3.13
    bash$ tcsh

and if all the shell nesting bothers you

    tcsh> xterm &

will get you a new window with your normal shell and the proper environment.


4. Finally
----------

Copyright (c) 1992-2015 by Haystack Observatory, MIT.
See the included file "Copyright" for the full notice.

Authors: Colin Lonsdale, Roger Cappallo, and many others.

If you experience problems with this release, contact Geoff Crew
(gbc@haystack.mit.edu) who is to blame for this packaging.

