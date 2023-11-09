Hardware and operating system configuration
===========================================

The sections below may be useful in setting up your cluster and
operating system (including the user account that will be running DiFX).

.. _sec:cluster:

Cluster configuration
---------------------

For production correlation, it is suggested that a dedicated user
account be created; for the rest of this document it will be assumed to
be “difx”. This account should have few or no other uses in order to
ensure that the environment is not disturbed. The user account must
exist on all nodes in the cluster and ssh should be configured so that
no password is required when logging into one node on the cluster from
another. This user account must also exist on the Mark5 units that are
used for playback. It is recommended that all computers in the cluster,
including the Mark5s, run the same version of Linux to avoid library
compatibility issues.

All of the nodes in the cluster, including the Mark5s, should be
interconnected by a fast network and have NFS access to the directories
from which correlation is to proceed. Complicated network topologies,
such as having more than one cluster node attached to more than one
network, can lead to unpredictable results as OpenMPI (the suggested MPI
library to use with ``mpifxcorr``) is network aggressive and will use
any means possible to enhance performance, even if such antics are
counterproductive. If your network topology is not simple be aware of
any network-related issues and keep in mind that you might need to
explicitly specify which network interfaces to use.

One node should be deemed the “head node”. In general this node should
have lots of hard disk space which is cross mounted to all the others
and could serve as the network gateway to the remainder of the cluster.
It is convenient, but not necessary, to locate all of the software and
correlation directories physically on this node to improve the
interchangeability of the other nodes. This node can participate in the
actual correlation, either as the manager node, a processing node or
both. By default, the head node will always be the manager node.

.. _sec:env:

Environment variables
---------------------

In addition to environment variables needed at build-time
(§\ `[sec:install] <#sec:install>`__), some others are needed at run
time. These are:

#. ``CALC_SERVER`` contains the name of the computer running calcServer
   (§\ `[sec:calcserver] <#sec:calcserver>`__). This is accessed only by
   program ``calcif``.

#. ``DIFX_ARCHIVE_ROOT`` points to the base directory of the archive
   staging area.

#. ``DIFX_ARCHIVE_USERNAME`` specifies the username of the archiving
   process (NRAO use only).

#. ``DIFX_CALC_PROGRAM`` specifies the command used to evaluate the
   delay model. By default this is currently ``calcif2``, however it is
   expected this will change to ``difxcalc`` in the DiFX 2.6 release
   series.

#. ``DIFX_GROUP_ID`` the Unix group to use. If set, umask is changed to
   002 and all new files/directories become group writable.

#. ``DIFX_HEAD_NODE`` contains the name of the cluster head node.

#. ``DIFX_MACHINES`` points to a file containing a list of cluster
   members and their capabilities. See
   §\ `[sec:difxmachines] <#sec:difxmachines>`__

#. ``DIFX_MESSAGE_GROUP`` (optional) specifies, with
   ``DIFX_MESSAGE_PORT``, the multicast group and port to be used for
   GUI and monitoring.

#. ``DIFX_MESSAGE_PORT`` (optional, *see above*)

#. ``DIFX_QUEUE_BASE`` points to the base of the correlator staging area
   for jobs to be run.

#. ``DIFX_VERSION`` (optional, but recommended) the version of difx
   being used, e.g., ``DIFX-2.2`` .

#. ``EVLAMPTS_DB`` contains the connection information for the Postgress
   E VLA monitor point database (NRAO only, needed only if VLA is
   included in array).

#. ``GAIN_CURVE_PATH`` (optional) points to a directory that contains
   keyin format files containing gain curves. This is used only by
   ``difx2fits``. If not set, ``difx2fits`` will not create gain curve
   tables. This directory must be readable by the difx user. Every file
   in this directory will be read, assuming it is a keyin format gain
   curve, so nothing else should be stored here. This directory needs to
   be created by hand if it does not exist.

#. ``JOB_ROOT`` points to the base directory that is to contain copies
   of job scripts of projects to correlate. This directory must be
   visible by all nodes on the cluster.

#. ``DIFX_LOG_DIR`` points to the directory where logs shall be written.

#. ``MARK5_DIR_PATH`` points to a directory that is used to cache the
   contents of Mark5 modules. This directory must be readable and
   writable by the user running mpifxcorr. This directory needs to be
   created by hand if it does not exist. It will get populated
   automatically. If there are problems with playback of a module, the
   files in this directory can sometimes be useful.

#. ``MARK6_ROOT`` points to the mountpoints for Mark6 files. The default
   is ``/mnt/disks/*/*/data``.

#. ``PGPLOT_FONT`` points to a font file for PGPLOT. The font file is
   usually called ``grfont.dat`` .

#. ``TCAL_PATH`` points to a directory containing :math:`T_{\rm cal}`
   values for receivers.

#. ``TESTS`` points to a path containing test data projects.

#. ``VLBA_DB`` contains the connection information for the Oracle legacy
   VLBA database (NRAO only).

#. ``VLBAMPTS_DB`` contains the connection information for the Postgress
   EVLA-style VLBA monitor point database (NRAO only).

Like the environment variables described in
§\ `[sec:install] <#sec:install>`__, these should all be set in shell
initialization files and should be set whether the shell is used
interactively or not. For the ``difx`` user account at NRAO, these are
set in a file called ``setup_difx`` which is run upon login (see
§\ `1.3 <#sec:versions>`__). Note that this file needs to be run whether
the login is interactive or not; please consult the documentation for
your shell if you have problems. To test if this file is being run in
non-interactive sessions, try the following: ``ssh`` *computername*
``env | grep DIFX`` and make sure you see the environment variables you
expect to.

.. _sec:versions:

Directory structure and versioning
----------------------------------

The directory structure of the NRAO deployment of DiFX is outlined in
Fig. `[fig:dirtree] <#fig:dirtree>`__. The aim is to cleanly programs,
libraries, and other version-specific files from data in a way that
switching from one version (e.g., 1.5) to another (e.g., the development
version) is simple, accountable, and complete, in order to assure that a
self-consistent set of software is used for an entire project. Each DiFX
version has its own root directory, such as ``/home/swc/DiFX-1.5`` . All
files associated with this version are under this directory. No data or
files associated with any other DiFX version shall be placed within.

Setting up a particular version is quite simple. Assuming the ``bash``
shell:

``. /home/swc/DiFX-1.5/setup_difx``

This script contains, among other lines, the following:

::

   export DIFX_PREFIX=/home/swc/NRAO-DiFX-trunk
   export DIFX_BASE=/home/swc/difx
   export DIFX_ARCHIVE_ROOT=/home/ngas_staging/difx
   export JOB_ROOT=${DIFX_BASE}/projects
   export TESTS=${DIFX_BASE}/tests
   export MARK5_DIR_PATH=${DIFX_BASE}/directories
   export CALC_SERVER=swc000
   export GAIN_CURVE_PATH=${DIFX_BASE}/gaincurves
   export DIFX_MACHINES=${DIFX_BASE}/machines.difx
   export DIFX_QUEUE_BASE=${DIFX_BASE}/queue
   export DIFX_HEAD_NODE=swc000
   export DIFX_VERSION=DIFX-1.5
   export DIFX_GROUP_ID=vlba_difx
   export DIFX_MESSAGE_GROUP=224.2.2.1
   export DIFX_MESSAGE_PORT=50200
   export IPPROOT=/home/swc/difx/intel/ipp/6.0.2.076/ia32
   export PATH=${DIFX_PREFIX}/bin:${ORACLE_HOME}/bin:/users/difx/bin:/bin:/usr/bin
   export LD_LIBRARY_PATH=${DIFX_PREFIX}/lib:${IPPROOT}/sharedlib:${ORACLE_HOME}/lib
   echo "DIFX version 1.5 is selected"

The “difx” account is set up to execute this script upon login. Note
that the settings here are useful for both compilation of the various
DiFX components as well as using them. Each installed version of DiFX
will have its own setup file like this. Selecting which version is to be
used is a simple as running the correct setup file. To change to the
development version:

``. /home/swc/DiFX-trunk/setup_difx``

It is highly recommended that one set the ``DIFX_VERSION`` environment
variable and make sure that for each installed version of DiFX this is
set differently. It may also be desirable to customize this for your
correlator. For example, one may set it to ``USNO-DIFX-1.5`` . This
string will be stored in intermediate files and the output FITS files
and will be able to identify more exactly where the data were
correlated.
