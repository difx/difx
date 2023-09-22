PGPLOT is not public-domain software.  However, it is freely available
for non-commercial use. The source code and documentation are copyrighted
by California Institute of Technology, and may not be redistributed or
placed on public Web servers without permission. The software is provided
``as is'' with no warranty. 

BUILD-IT-YOURSELF:

It is available from

    ftp://ftp.astro.caltech.edu/pub/pgplot/pgplot5.2.tar.gz

For installation in, e.g. from /usr/local/src/pgplot into
/usr/local/pgplot, some verison of the  following may work
on a linux machine using gfortran.  There are full instructions
within the tarball for other hosts and configurations.

PNG support is frozen at v1.2 and there have been significant
changes to PNG since then, so you probably will not be able
to make PNGs directly with PGPLOT without some effort.

    cd /usr/local/src/pgplot
    cp -p <path-to-the-tarball>/pgplot5.2.tar.gz .
    tar xf pgplot5.2.tar.gz
    cd /usr/local/pgplot
    pgplot=/usr/local/src/pgplot/pgplot
    # cp $pgplot/drivers.list .
    # vi drivers.list
    cp -p <path-to-HOPS-distro>/pgplot-drivers.list .
    grep -v ^\! drivers.list 

    # should have:
    NUDRIV 0 /NULL      Null device (no output)                       Std F77
    PSDRIV 1 /PS        PostScript printers, monochrome, landscape    Std F77
    PSDRIV 2 /VPS       Postscript printers, monochrome, portrait     Std F77
    PSDRIV 3 /CPS       PostScript printers, color, landscape         Std F77
    PSDRIV 4 /VCPS      PostScript printers, color, portrait          Std F77
    TTDRIV 5 /XTERM     XTERM Tektronix terminal emulator             Std F77
    XWDRIV 1 /XWINDOW   Workstations running X Window System          C
    XWDRIV 2 /XSERVE    Persistent window on X Window System          C

    # /usr/local/src/pgplot/makemake /usr/local/src/pgplot linux g77_gcc
    sed s/g77/gfortran/ $pgplot/sys_linux/g77_gcc.conf >\
                        $pgplot/sys_linux/gfortran_gcc.conf
    $pgplot/makemake $pgplot linux gfortran_gcc
    make && make cpg

    # may need to logout to get DISPLAY defined.
    # test:
    cd /usr/local/pgplot
    PATH=`pwd` LD_LIBRARY_PATH=`pwd` ./pgdemo1
    # /xw - hit return in the main window a dozen times
    make clean

INSTALL-A-PACKAGE:

    rpmfusion.org offers pgplot packages; these are not currently supported.
    It includes a tcl-pgplot Tcl/Tk driver that is also not supported (yet).

PGPLOT Environment variables:

    The full list (at least found with 'grep GRGENV' in the *.f sources) is:

    PGPLOT_TYPE             -- get default type
    PGPLOT_PS_VERBOSE_TEXT  -- draws text as vectors
    PGPLOT_ENVOPT           -- set some options:
        P                       draw Projecting tick marks
        I                       Invert the tick marks
        IV                      Invert tick marks and label y Vertically
    PGPLOT_DEV              -- get default device/type
    PGPLOT_BUFFER           -- start buffering output (whatever that means)
    PGPLOT_BACKGROUND       -- select background color
    PGPLOT_FOREGROUND       -- select foreground color
    PGPLOT_DIR              -- the directory with everything
    PGPLOT_DEBUG            -- controls debug output regarding:
    PGPLOT_FONT             -- full path to $PGPLOT_DIR/grfont.dat
    PGPLOT_RGB              -- full path to $PGPLOT_DIR/rgb.txt

    The search for its files is found in grgfil.f which first
    consider PGPLOT_FONT and PGPLOT_RGB prior to PGPLOT_DIR/whatever

eof

