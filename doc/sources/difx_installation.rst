.. _sec:install:

Installation and upgrade guide
==============================

There are at least three methods of installing DiFX employed by its
various users. Ironically, these do not include some of the more common
installation mechanisms such as those offered natively by various Linux
distributions (e.g., ``.rpm`` or ``.deb`` files). DiFX has many modules,
some of which have dependencies that are not easily met or that are not
needed. Some modules have optional dependencies. Finally, many folks may
wish to have several versions installed at one time. These
considerations and the effort involved in overcoming some of them have
led to the current situation where package-by-package compilation is the
standard mechanism for installation.

The sections that follow document two of these methods. First an install
method using the ``difxbuild`` script is described. Then a more manual
method is described.

.. _sec:installdifxbuild:

Installation with ``difxbuild``
-------------------------------

Introduction
~~~~~~~~~~~~

This installation guide is based on the python program ``difxbuild``.
This program allows installation and management of multiple versions,
each on multiple platforms, and associate setup scripts. First some
terminology: A *version* is a official numbered release of DiFX or an
unofficial intermediate version drawn from the subversion repository.
Examples are the recent stable 2.1 release and the head of the
development, called trunk. Each *version* described here has a name. The
name for the two versions mentioned here are ``DIFX-2.1`` and
``DIFX-DEVEL`` respectively. An *architecture* (or *arch* for short) is
a computer type, usually defined by the instruction set of the CPU.
Currently ``difxbuild`` explicitly knows about two architectures:
``x86_64`` for 64-bit Intel CPUs and ``i386`` for 32-bit Intel CPUs. The
architecture of your machine can be determined on the command line with
``uname -p``. Finally a *platform* is a particular configuration defined
both by the *architecture* and the set of dependent software installed
on it. For example, if multiple Mark5 units with different software
development kit (SDK) versions are being used, each would be a different
*platform*. For each DiFX installation there is a primary, or default,
*platform* simply identified by the name of the *version*. Each
additional *platform* is assigned an additional name (which could be
``SDK8`` and ``SDK9`` for the Mark5 situation) and is identified by
concatenating the version name, a hyphen, and the additional name (e.g.,
``DIFX-2.1-SDK8``).

The installation process has a number of steps, including bootstrapping,
source acquisition and autotooling, building (separately on each
*platform*). Any of these steps can be repeated if needed, however, in
many cases it does not make sense to repeat a single step out of order,
so following steps may need to be performed to be meaningful.

In the description below, each place where a command is to be typed into
the computer, it is displayed after a :math:`\longrightarrow` .

Assumptions
~~~~~~~~~~~

To simplify the installation and eventual execution of DiFX, some
assumptions are explicitly made:

#. It is assumed that before the DiFX installation is started that a
   fully usable Linux operating system is already running and a few bits
   of software are installed. This specifically includes the Intel
   Integrated Performance Primitives (IPP) which must be directly
   acquired through Intel’s web site.

#. It is assumed that all machines running DiFX cross mount the same
   filesystem, that the local name of the cross-mounted filesystem is
   the same on each machine, and that all parts of the DiFX installation
   will reside on such a partition.

#. It is assumed that during source acquisition steps the machine on
   which <code>difxbuild</code> is being invoked has access to the
   internet (specifically http and svn).

#. It is assumed here that ``bash`` (or a compatible equivalent) is the
   shell used both by root and the user. If this is not met, it is up to
   the user to make any needed procedural changes.

Installation Part 1
~~~~~~~~~~~~~~~~~~~

Part 1 of installation deals with aspects of the installation that are
specific to one *version* but all *architectures* and *platforms*. For
each new *version* of DiFX, these steps will be repeated.

**Bootstrapping:**

The bootstrapping step can start from a pristine computer (as long as
the above assumptions are met) and will generate a skeleton DiFX
environment from a rather simple input file. This bootstrap input file
consists of a few lines of ASCII text that describe in a minimal manner
the intended parameters of the DiFX installation. A complete description
of such a file can be found in
§\ `[sec:bootstrapfile] <#sec:bootstrapfile>`__.

Below is an example ``.bootstrap`` file:

::

   #-------------------------------------
   # here are version-specific parameters
   #-------------------------------------

   # version of DiFX installed by this file
   version = DIFX-DEVEL

   #--------------------------------------------------------
   # below here, all installed versions should look the same
   #--------------------------------------------------------

   # identify which node should run the core process
   headnode = node-1

   # top level directory for all DiFX software
   difxbase = /home/usno/difx

   # location of installed Intel Integrated Performance Primitives
   ipproot = /home/usno/intel

   # define mark5 alternate architecture
   altplatform1     = SDK9
   altplatform1arch = i686
   altplatform1test = [[ x`pkg-config --modversion streamstor` &gt; "x9.0" ]]
   altplatform1host = mark5fx-usno-1

   # MPI network selection: restrict which network devices are used
   mca = btl_tcp_if_include=p2p1

This file could logically be called ``trunk.bootstrap`` as is assumed
here. Note a similar file for DiFX version 2.1 could be made by simply
changing *version*. Bootstrapping is executed as follows (with optional
``-v`` option for increased verbosity):

:math:`\longrightarrow` ``difxbuild -v bootstrap trunk.bootstrap``

If this completes successfully, you will be told to source the new setup
file:

:math:`\longrightarrow` ``. /home/usno/difx/DIFX_DEVEL/setup_difx``

This command must be issued before installation can continue. It should
be reissued after each new shell is started, and must be reissued if
changes are made to the ``.bootstrap`` file and bootstrapping is redone.

**Check out source code from subversion:**

A single command will cause the built-in selection of components to be
downloaded from the ATNF subversion repository:

:math:`\longrightarrow` ``difxbuild -v svn all``

The ``all`` parameter here, and in later commands, refers to all
components (modules) supported by ``difxbuild`` for the version of DiFX
being installed. To see which components this would apply to:

:math:`\longrightarrow` ``difxbuild list``

If the ``all`` is excluded, the component corresponding to your current
working directory (which would be none at this point) would be selected.
Alternately, a list of components can be selected. Each component’s
source will be put in a separate subdirectory of ``$DIFX_SRC``.

**Configure the source trees for out-of-tree building:**

This step runs the “autotools” on the selected components. To achieve
the purpose of supporting multiple *platforms*, all building is
performed out of the source directories, so this step stops short of
running ``configure`` itself.

:math:`\longrightarrow` ``difxbuild -v autotool all``

Set this version of DiFX as the default version
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you want this version of DiFX to be the default:

:math:`\longrightarrow` ``difxbuild -v default``

This step simply makes a symlink to the newly created ``setup_difx``
script. Note that this step can be performed at any time.Changing to a
different default version is done by sourcing the ``setup_difx`` script
for that version and running this command.

Installation Part 2
~~~~~~~~~~~~~~~~~~~

Part 2 of the installation deals with installations of *architecture*
dependent code that can work across different *versions* (and
*platforms* as long as the they are of the same *architecture*). Sharing
these bits of code across different *versions* requires that the base
directory, as specified in the bootstrapping stage, are the same for
each *version*. Repeat these steps for each *architecture* by logging
into a representative machine of each *architecture*, sourcing the
appropriate ``setup_difx`` file, and continuing… Note that several extra
libraries such as ``PGPLOT`` are almost certainly not needed for any of
the alternate platforms.

**Installing OpenMPI:**

Most Linux operating systems come with some version of OpenMPI these
days, but most won’t work for DiFX installations with multiple
*architectures* as a particular configure-time parameter (
``–enable-heterogenerous``) is usually not set. To download and install
the latest stable version of OpenMPI:

:math:`\longrightarrow` ``difxbuild -v openmpi``

**Installing Caltech’s PGPLOT library (optional):**

If you want to build the "sniffer" plotting tools or hops, you need to
install the pgplot plotting library:

:math:`\longrightarrow` ``difxbuild -v pgplot``

Installation Part 3
~~~~~~~~~~~~~~~~~~~

The 3rd part of installation must be done once for each *platform* (and
always separately for each *version*, there are no shortcuts here!) This
is the actual source code building step. For the non-primary
*platforms*, simply log onto one of the machines representing that
*platform* and be sure to source the appropriate ``setup_difx`` file and
then proceed.

**Build DiFX:**

This part is simple, but may take a few minutes:

:math:`\longrightarrow` ``difxbuild -v build all``

Installation Part 4
~~~~~~~~~~~~~~~~~~~

The final step of installation is configuring the account of the user
that will run DiFX. It is assumed here that this account is called
``oper`` and the account used for installation was ``difxmgr`` (but
these are for example only; any usernames can be used). The only
remaining steps are to ensure the environment is correctly configured by
copying some files from the ``difxmgr`` account.

:math:`\longrightarrow`
``echo ". /home/usno/difx/bin/setup_difx"``\ :math:`>>`\  /.profile

:math:`\longrightarrow` ``ln -s  /.profile  /.bashrc``

At this point once ``oper`` logs back in DiFX should be ready to run.

Upgrading the installation
~~~~~~~~~~~~~~~~~~~~~~~~~~

Manual installation
-------------------

This section describes module-by-module installation. This install
method is not recommended in general and documentation for this may be
out of date or eventually removed from this document. This method does
give a deeper understanding of what actually happens behind the scenes
when using the other methods and so may be useful to read through in any
case.

The sections below should be followed more or less in order. Before you
begin installing code, you should take a few moments to prepare your
environment. First choose a top level source directory, here called
*sourcedir*. Also choose an installation top level directory, called
*prefixdir*, which should be visible to all the nodes in the cluster.
Into this directory, subdirectories such as ``bin``, ``lib``,
``include`` will be created containing the installed code from the many
packages you will need. At this time four environment variables need
creation or expansion:

#. ``IPPROOT``: §\ `1.2.4 <#sec:ipp>`__. Set this to something trivial
   (such as ``.``) until IPP has been installed, then remember to change
   it as appropriate.

#. ``LD_LIBRARY_PATH``, a standard environment variable containing a
   dynamic library search path. Add *prefixdir*\ ``/lib`` and
   ``$IPPROOT/sharedlib`` to this path.

#. ``PATH``, a standard environment variable containing the execution
   path. Add *prefixdir*\ ``/bin`` to this path.

#. ``PKG_CONFIG_PATH``, a search path for package installation
   information. Add *prefixdir*\ ``/lib/pkgconfig`` to this path.

Note that all of these environment variables (in addition to those
described in §\ `[sec:env] <#sec:env>`__) are required at run-time as
well as compile-time, so it is advisable to put these path commands into
your shell initialization file and start a new shell at this point. Note
that these variables will be needed not only in interactive shells, but
also non-interactive ones, so be sure that these are set no matter how
the shell is invoked.

To download, compile and install the software, you will need the
standard gnu tools (gcc, libtool, autoconf, automake, make, ...),
python, subversion, and of course ssh. Be aware that many distributions
don’t install by default all of these needed tools (xubuntu for example
installs very few development tools by default. Relatively few external
libraries are used. It also assumes you have an account that allows
access to the subversion repository at https://svn.atnf.csiro.au.

The ``make install`` steps may require root permission, depending on the
*prefixdir* you have chosen. If so, become root before each
``make install``. It is advisable not to compile code as root. Be wary
of errors along the way; occasional warnings may be issued, but if the
building proceeds, things are probably okay. Please report any build
issues to ``wbrisken@nrao.edu``. Be warned that these instructions may
change.

All of the subversion repositories below point to a ``difx-1.5`` tagged
release of the repository. This is in order to provide a relatively
stable source tree that allows development to continue on the main
development branch (called ``trunk``). In order to check out code that
is on this development branch, simply replace ``tags/difx-1.5`` with
``trunk`` in all of the ``svn`` commands below. *Caveat emptor:* the
``trunk`` branch code may at any time refuse to compile, be unstable,
lack documentation, or produce incorrect results. Don’t let this stop
you if you are an intrepid developer or want to see ongoing development
in progress!

.. _sec:expat:

expat
~~~~~

Expat is a standard library for simple XML parsing. By default it is
installed on almost all Linux distributions. If it is not, it can be
downloaded and installed based on instructions that can be found at its
web page: http://expat.sourceforge.net/ .

.. _sec:cxOracle:

cx_Oracle
~~~~~~~~~

In the implementation of the VLBA-DiFX operations plans, with the
exception of the ``DOI``, all of the access to VLBA database is done
using python programs that employ the cx_Oracle library. This library
directly talks to Oracle databases; its use in Python makes for nearly
effortless database interfacing. To install:

#. Download the latest source distribution from
   http://cx-oracle.sourceforge.net/ (ver. 5.1.2 as of this writing)

#. Decompress the contents into perhaps *sourcedir*; enter the newly
   created directory

#. Run ``python setup.py build``

#. Make sure install directory exists:
   ``mkdir -p $DIFXROOT/lib/python2.4/site-packages``

#. Run ``python setup.py install –prefix=$DIFXROOT``

Notes:

#. You should substitute ``python2.4`` with the string appropriate for
   your python version.

#. ``lib`` may need to be replaced with ``lib64`` in the path above.

#. Proper installation can be tested by running ``python`` and typing
   ``import cx_Oracle`` at the ``>>>`` prompt. If another prompt is
   given without any “ImportError” message, then it should be installed
   properly.

#. The ``setup.py`` file from version 5.1.2 seems to have an
   incompatibility with RedHat Enterprise Linux 6 (and there may be
   other varients of this incompatibility). Inserting
   ``extraCompileArgs.append("-D__USE_XOPEN2K8")`` at a logical
   outer-level location around line 200 seems to fix this problem.

.. _sec:mpi:

OpenMPI
~~~~~~~

The core of DiFX uses Message Passing Interface (MPI) for inter-node
communication. Many MPI libraries exist; we choose to use OpenMPI as it
is simple to install, runs well, and appears to have good community
support.

#. Download the latest source distribution from http://www.open-mpi.org/
   (ver. 1.4.2 as of this writing)

#. Decompress the contents into perhaps *sourcedir*; enter the newly
   created directory

#. ``./configure –prefix=``\ *openmpiprefix* where *openmpiprefix* could
   be the same as *prefixdir*, but does not have to be.

#. Run ``make`` and finally ``make install`` to put the parts where they
   belong.

.. _sec:ipp:

Intel Performance Primitives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Intel CPUs support an increasing variety of vector math instructions.
The Intel Performance Primitives (IPP) makes exploiting these
capabilities on any recent generation CPU simple. An inexpensive license
must be purchased to make use of these. More information can be found on
http://www.intel.com.

Once installed, set environment variable ``IPPROOT`` to point to its
install prefix, which will look something like:
``/home/swc/difx/intel/ipp/6.0.2.076/ia32``; you want to choose the
directory containing ``lib``, ``include``, etc. Remember to change this
in your shell initialization file as well. This install directory should
be visible to all nodes in the cluster.

.. _sec:fftw:

FFTW
~~~~

The FFTs performed by ``mpifxcorr`` are done using the Intel Performance
Primitives library, but FFTs done in an optional piece of\ ``difx2fits``
and the utility ``m5spec`` that comes with mark5access use FFTW, a
standard, fast, freely available FFT library. This library is probably
installed for you with any modern Linux distribution, but you should
check to make sure it is recent enough; version 3.0 and up are
supported, but version 3.1.2 or newer is recommended. If this library is
not installed and the extra functionality that requires FFTW is not
installed, follow the instructions below:

#. Download the latest source distribution from http://www.fftw.org
   (ver. 3.1.2 as of this writing)

#. Decompress the contents into perhaps *sourcedir*; enter the newly
   created directory

#. ``./configure –prefix=``\ *prefixdir*

#. Run ``make`` and finally ``make install`` to put the parts where they
   belong.

.. _sec:difxio:

difxio
~~~~~~

Parsing of text files can be tedious. The library difxio makes parsing
difx-style files simple. It also contains functionality to completely
represent the configuration of a DiFX correlation, simplifying format
conversions. To install:

#. ``cd`` *sourcedir*

#. | Check out the subversion repository:
   | ``svn co``\ https://svn.atnf.csiro.au/difx/libraries/difxio/branches/difx-1.5\ `` difxio``
   | *Note: don’t forget the ``difxio`` at the end of the line!*

#. Enter the new directory ``cd difxio``

#. View the ``README`` file. Note the next 5 instructions only need to
   be done once in this directory, even after updating the repository.
   You can ``man`` the commands if you want to know what they do.

#. ``aclocal``

#. ``libtoolize –copy –force``

#. ``autoconf``

#. ``autoheader``

#. ``automake -a``

#. Generate the Makefile: ``./configure –prefix=``\ *prefixdir*

#. Build it: ``make``

#. Install it: ``make install``

You can test for successful installation by running
``pkg-config –cflags difxio``. If you get a sensible answer, things are
probably good. If you wish to upgrade the installation:

#. ``cd`` *sourcedir*\ ``/difxio``

#. Get updates from the repository: ``svn update``

#. Build it: ``make``

#. Install it: ``make install``

Note that doing this upgrade may break other packages that depend on it,
such as ``difx2fits`` and ``calcif``, forcing a recompile of these
programs.

.. _sec:difxmessage:

difxmessage :math:`\mathrm{(optional)}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The library difxmessage implements in the C language XML generation and
parsing and multicast sending and receiving functionality that is used
for communication between various parts of the DiFX system. See
§\ `[sec:xml] <#sec:xml>`__ for details on the XML documents supported.
The communication model is based on that of the EVLA. This package is
optional; if not built, you will not be able to use ``mk5daemon`` or any
program packaged with it, or ``genmachines``. To install:

#. ``cd`` *sourcedir*

#. | Check out the subversion repository:
   | ``svn co``\ https://svn.atnf.csiro.au/difx/libraries/difxmessage/branches/difx-1.5\ `` difxmessage``

#. Enter the new directory ``cd difxmessage``

#. View the ``README`` file. Note the next 5 instructions only need to
   be done once in this directory, even after updating the repository.
   You can ``man`` the commands if you want to know what they do.

#. ``aclocal``

#. ``libtoolize –copy –force``

#. ``autoconf``

#. ``autoheader``

#. ``automake -a``

#. Generate the Makefile: ``./configure –prefix=``\ *prefixdir*

#. Build it: ``make``

#. Install it: ``make install``

You can test for successful installation by running
``pkg-config –cflags difxmessage``. If you get a sensible answer, things
are probably good. If you wish to upgrade the installation:

#. ``cd`` *sourcedir*\ ``/difxio``

#. Get updates from the repository: ``svn update``

#. Build it: ``make``

#. Install it: ``make install``

Note that doing this upgrade may break other packages that depend on it,
such as ``mpifxcorr`` and ``mk5daemon``, forcing a recompile of these
programs.

.. _sec:m5a:

mark5access
~~~~~~~~~~~

mark5access is a library to parse various VLBI baseband data formats,
including Mark4, VLBA, and Mark5B, with other formats to be added. This
is needed to decode these various formats from within mpifxcorr. To
install:

#. ``cd`` *sourcedir*

#. | Check out the subversion repository:
   | ``svn co``\ https://svn.atnf.csiro.au/difx/libraries/mark5access/branches/difx-1.5\ `` mark5access``

#. Enter the new directory ``cd mark5access``

#. View the ``README`` file. Note the next 5 instructions only need to
   be done once in this directory, even after updating the repository.

#. ``aclocal``

#. ``libtoolize –copy –force``

#. ``autoconf``

#. ``autoheader``

#. ``automake -a``

#. Generate the Makefile: ``./configure –prefix=``\ *prefixdir*

#. Build it: ``make``

#. Install it: ``make install``

You can test for successful installation by running
``pkg-config –cflags mark5access``. If you get a sensible answer, things
are probably good. If you wish to upgrade the installation:

#. ``cd`` *sourcedir*\ ``/mark5access``

#. Get updates from the repository: ``svn update``

#. Build it: ``make``

#. Install it: ``make install``

Note that doing this upgrade may break other packages that depend on it,
such as ``mpifxcorr``, forcing a recompile of these programs.

mpifxcorr
~~~~~~~~~

The core of the DiFX software correlator is ``mpifxcorr``. Installation
and running this program requires that MPI (§\ `1.2.3 <#sec:mpi>`__),
IPP (§\ `1.2.4 <#sec:ipp>`__), difxio and mark5access all be installed.
To install:

#. ``cd`` *sourcedir*

#. | Check out the subversion repository:
   | ``svn co``\ https://svn.atnf.csiro.au/difx/mpifxcorr/branches/difx-1.5\ `` mpifxcorr``

#. Enter the new directory ``cd mpifxcorr``

#. View the ``README`` file. Note the next 4 instructions only need to
   be done once in this directory, even after updating the repository.

#. ``aclocal``

#. ``autoconf``

#. ``autoheader``

#. ``automake -a``

#. Generate the Makefile: ``./configure –prefix=``\ *prefixdir*
   ``CXX=``\ *openmpiprefix*\ ``/bin/mpicxx``

#. Build it: ``make``

#. Install it: ``make install``

If successfully installed, the command ``which mpifxcorr`` should return
*prefixdir*\ ``/bin/mpifxcorr``. If you wish to upgrade the
installation:

#. ``cd`` *sourcedir*\ ``/mpifxcorr``

#. Get updates from the repository: ``svn update``

#. Build it: ``make``

#. Install it: ``make install``

.. _sec:calcserver:

calcserver
~~~~~~~~~~

The Goddard Space Flight Center CALC package version 9.1 is used to
calculate the delay models needed for time-alignment of the raw data.
This software is wrapped in a program that exposes the capabilities of
CALC via a Remote Procedure Call (RPC) and this program runs as a
server. An environment variable ``CALC_SERVER`` should be set that
contains the name of the computer running ``calcserver``. Within DiFX,
the only program that makes use of this server is ``calcif2``
(§\ `[sec:calcif2] <#sec:calcif2>`__). To install:

#. ``cd`` *sourcedir*

#. | Check out the subversion repository:
   | ``svn co``\ https://svn.atnf.csiro.au/difx/applications/calcserver/branches/difx-1.5\ `` calcserver``

#. Enter the new directory ``cd calcserver``

#. View the ``README`` file.

#. ``aclocal``

#. ``libtoolize –copy –force``

#. ``autoconf``

#. ``automake -a``

#. Generate the Makefile: ``./configure –prefix=``\ *prefixdir*

#. Build it: ``make``

#. Install it: ``make install``

Since ``calcserver`` is a single-instance program that is always running
as a service, it is usually convenient to have this program start upon
boot of the calcserver host. The calcserver distribution produces a file
called *srcDir*\ *calcserver/init.d/calcserver* that can be copied (as
root) to the system ``/etc/init.d`` directory. After doing so, the
``/etc/rc.d`` directories may need to be updated to run this script at
the right time. On RedHat systems, this is done with:

``/sbin/chkconfig –add calcserver``

.. _package:calcif2:

calcif2
~~~~~~~

The ``calcif2`` package contains several programs that are useful for
DiFX input file creation and managing correlation, most notably
``calcif2`` . Note that this package used to be called *job2difx*. To
install:

#. ``cd`` *sourcedir*

#. | Check out the subversion repository:
   | ``svn co``\ https://svn.atnf.csiro.au/difx/utilities/branches/difx-1.5/calcif2\ `` calcif2``

#. Enter the new directory ``cd calcif2``

#. View the ``README`` file. Note the next 4 instructions only need to
   be done once in this directory, even after updating the repository.

#. ``aclocal``

#. ``autoconf``

#. ``autoheader``

#. ``automake -a``

#. Generate the Makefile: ``./configure –prefix=``\ *prefixdir*

#. Build it: ``make``

#. Install it: ``make install``

If successfully installed, the command ``which calcif2`` should return
*prefixdir*\ ``/bin/calcif2``. Several other programs should also be
installed, including: ``genmachines``, ``getjobs``, ``jobdisks``,
``joblist``, ``jobstatus``, ``difxsniff``, ``mk5take``, ``mk5return``,
and ``vlog``. If you wish to upgrade the installation:

#. ``cd`` *sourcedir*\ ``/calcif2``

#. Get updates from the repository: ``svn update``

#. Build it: ``make``

#. Install it: ``make install``

difx2fits
~~~~~~~~~

The initial NRAO adaptation of DiFX is designed to interface as
seamlessly as possible into our existing infrastructure and habits. This
means generation of FITS-IDI output files for compliance with AIPS. The
program ``difx2fits`` takes many input files (see
Fig. `[fig:block] <#fig:block>`__) and produces a FITS file for every
DiFX input file. To install:

#. ``cd`` *sourcedir*

#. | Check out the subversion repository:
   | ``svn co``\ https://svn.atnf.csiro.au/difx/applications/difx2fits/branches/difx-1.5\ `` difx2fits``

#. Enter the new directory ``cd difx2fits``

#. View the ``README`` file. Note the next 4 instructions only need to
   be done once in this directory, even after updating the repository.

#. ``aclocal``

#. ``autoconf``

#. ``autoheader``

#. ``automake -a``

#. Generate the Makefile: ``./configure –prefix=``\ *prefixdir*

#. Build it: ``make``

#. Install it: ``make install``

If successfully installed, the command ``which difx2fits`` should return
*prefixdir*\ ``/bin/difx2fits``. If you wish to upgrade the
installation:

#. ``cd`` *sourcedir*\ ``/difx2fits``

#. Get updates from the repository: ``svn update``

#. Build it: ``make``

#. Install it: ``make install``

mk5daemon :math:`\mathrm{(optional)}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The optional package ``mk5daemon`` relies on package ``difxmessage`` and
is only really needed for installations requiring playback off Mark5
modules. Root permission is required for proper installation and running
of this program. See §\ `[sec:mk5daemon] <#sec:mk5daemon>`__ for a
description of the main program, ``mk5daemon``, that comes with this
package. Other useful scripts are included here. To install:

#. ``cd`` *sourcedir*

#. | Check out the subversion repository:
   | ``svn co``\ https://svn.atnf.csiro.au/difx/applications/mk5daemon/branches/difx-1.5\ `` mk5daemon``

#. Enter the new directory ``cd mk5daemon``

#. View the ``README`` file. Note the next 4 instructions only need to
   be done once in this directory, even after updating the repository.

#. ``aclocal``

#. ``autoconf``

#. ``autoheader``

#. ``automake -a``

#. Generate the Makefile: ``./configure –prefix=``\ *prefixdir*

#. Build it: ``make``

#. Install it: ``make install``

#. Ensure that this program starts at boot. This requires the following
   to occur on each computer in the cluster. Note that the instructions
   may vary depending on your operating system. The program is likely to
   get started before NSF is started, so ``mk5daemon`` should be
   installed locally on each compute in the cluster. On each machine,
   run as root: ``cp``\ *prefixdir*\ ``/bin/mk5daemon`` *localdir*
   ``; echo``\ *localdir*\ ``/mk5daemon >> /etc/rc.local`` . Here
   *localdir* is a directory on the particular machine, such as
   ``/usr/bin`` This only needs to be run

If successfully installed, the command ``which mk5daemon`` should return
*prefixdir*\ ``/bin/mk5daemon``. If you wish to upgrade the
installation:

#. ``cd`` *sourcedir*\ ``/mk5daemon``

#. Get updates from the repository: ``svn update``

#. Build it: ``make``

#. Install it: ``make install``

#. Copy it to the local disk (as root):
   ``cp -f``\ *prefixdir*\ ``/bin/mk5daemon`` *localdir*

vex2difx
~~~~~~~~

The ``vex2difx`` program aims to convert any legal, complete vex format
experiment description file into ``.input`` and ``.calc`` files for use
with DiFX. To install:

#. ``cd`` *sourcedir*

#. | Check out the subversion repository:
   | ``svn co``\ https://svn.atnf.csiro.au/difx/applications/vex2difx/branches/difx-1.5\ `` vex2difx``

#. Enter the new directory ``cd vex2difx``

#. View the ``README`` file. Note the next 4 instructions only need to
   be done once in this directory, even after updating the repository.

#. ``aclocal``

#. ``autoconf``

#. ``autoheader``

#. ``automake -a``

#. Generate the Makefile: ``./configure –prefix=``\ *prefixdir*

#. Build it: ``make``

#. Install it: ``make install``

If successfully installed, the command ``which vex2difx`` should return
*prefixdir*\ ``/bin/vex2difx``. If you wish to upgrade the installation:

#. ``cd`` *sourcedir*\ ``/vex2difx``

#. Get updates from the repository: ``svn update``

#. Build it: ``make``

#. Install it: ``make install``

difx_db :math:`\mathrm{(NRAO\ only)}`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Package ``difx_db`` contains several scripts that either make direct
connection to the VLBA database or require site-specific access. Thus,
this package is not available from the standard DiFX repositories. If
any programs in this package seem especially appropriate for use at a
correlator other than the VLBA DiFX correlator, please let me know and
I’ll see what I can do to make it more generally useful. To install:

#. ``cd`` *sourcedir*

#. Check out the subversion repository

#. Enter the new directory ``cd difx_db``

#. View the ``README`` file. Note the next 4 instructions only need to
   be done once in this directory, even after updating the repository.

#. ``aclocal``

#. ``autoconf``

#. ``automake -a``

#. Generate the Makefile: ``./configure –prefix=``\ *prefixdir*

#. Build it: ``make``

#. Install it: ``make install``

#. (*as root*) Change file ownership: ``chown root``
   *prefixdir*\ ``/e2ecopy``

#. (*as root*) Set UID: ``chmod +s`` *prefixdir*\ ``/e2ecopy``

If successfully installed, the command ``which difxqueue`` should return
*prefixdir*\ ``/bin/difxqueue``. If you wish to upgrade the
installation:

#. ``cd`` *sourcedir*\ ``/vex2difx``

#. Get updates from the repository: ``svn update``

#. Build it: ``make``

#. Install it: ``make install``

#. (*as root*) Change file ownership: ``chown root``
   *prefixdir*\ ``/e2ecopy``

#. (*as root*) Set UID: ``chmod +s`` *prefixdir*\ ``/e2ecopy``
