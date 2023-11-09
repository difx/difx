.. role:: raw-latex(raw)
   :format: latex
..

.. _sec:files:

Description of various files
============================

In the descriptions that follow, the locations of some files is given as
``/home/vlbiobs``, meaning the directory
``/home/vlbiobs/astronomy/``\ *mmmyy*\ ``/``\ *project* or one of its
subdirectories (this is VLBA-centric). Here *mmmyy* is the month and
year of the project’s observation (i.e., ``jan08``) and project is the
full project name, with segment, in lower case, such as ``bw088n``. In
what follows, the “software correlator project directory” (sometimes
“project directory”) refers to the directory from which software
correlation is to proceed. File names beginning with a period (e.g.,
``.acb``) represent file name extensions, typically (but not always) to
job file bases, such as ``job121.000`` .

.. _sec:aapd:

.aapd
-----

The program ``apd2antenna`` (see
Sec. `[sec:apd2antenna] <#sec:apd2antenna>`__) takes the ``.apd`` file
(Sec. `1.5 <#sec:apd>`__ created by ``difx2fits``
(Sec. `[sec:difx2fits] <#sec:difx2fits>`__) and performs least-squares
fits to reference the phase, delay, and rate measurements to a specified
reference antenna.

The first line in the file is ``obscode:`` followed by the observation
code, e.g., ``MT831`` .

Each subsequent line has the same format with the following fields:

+----------------------+----------------------+----------------------+
| Key                  | Units/allowed values | Comments             |
+======================+======================+======================+
| *MJD*                | integer              | MJD day number       |
|                      | :math:`\ge 1`        | corresponding to     |
|                      |                      | line                 |
+----------------------+----------------------+----------------------+
| *hour*               | :math:`\ge 0.0`,     | hour within day      |
|                      | :math:`< 24.0`       |                      |
+----------------------+----------------------+----------------------+
| *source name*        | string               | name of source; no   |
|                      |                      | spaces allowed       |
+----------------------+----------------------+----------------------+
| *antenna number*     | integer              | antenna table index  |
|                      | :math:`\ge 1`        | for first antenna    |
+----------------------+----------------------+----------------------+
| *antenna name*       | string               | name of antenna 1;   |
|                      |                      | no spaces allowed    |
+----------------------+----------------------+----------------------+
| *n*\ :ma             | integer              | number of baseband   |
| th:`_{\mathrm{BBC}}` | :math:`\ge 1`        | channels,            |
|                      |                      | :mat                 |
|                      |                      | h:`n_{\mathrm{BBC}}` |
+----------------------+----------------------+----------------------+
|                      |                      | The next four        |
|                      |                      | columns are repeated |
|                      |                      | :mat                 |
|                      |                      | h:`n_{\mathrm{BBC}}` |
|                      |                      | times                |
+----------------------+----------------------+----------------------+
| *delay*              | ns                   | the fringe fit delay |
+----------------------+----------------------+----------------------+
| *phase*              | degrees              | phase of fringe fit  |
|                      |                      | peak                 |
+----------------------+----------------------+----------------------+
| *rate*               | Hz                   | the fringe fit rate  |
+----------------------+----------------------+----------------------+

.. _sec:abp:

.abp
----

When run with the ``–bandpass`` option, ``difx2fits`` will create a
``.bandpass`` file. This file can be run through ``bp2antenna``
(Sec. `[sec:bp2antenna] <#sec:bp2antenna>`__ to convert the
baseline-based bandpasses to antenna-based bandpasses.

It is possible that line comments, starting with #, are present in the
file.

The first line of the file is ``obscode:`` followed by the project code.

A header line starting with ``Bandpass`` will signify the start of the
data for one antenna for one baseband channel. Such a line has exactly
eight space-separated fields:

#. The keyword ``Bandpass``

#. Zero-based antenna number for the antenna

#. Uppercase antenna code for the antenna

#. Baseband channel number (zero-based)

#. Number of points in the bandpass, :math:`N_{\rm point}`

#. Signed Sum of Local Oscillator (SSLO) for the baseband channel
   frequency (MHz)

#. Baseband channel bandwidth (MHz)

#. Polarization (typically ``R``, ``L``, ``H``, or ``V``)

Following a header line should be the specified number,
:math:`N_{\rm point}`, of data lines, each with three space-separated
values:

#. Sky frequency (MHz)

#. Real part of bandpass at this frequency

#. Imaginary part of bandpass at this frequency

Note that the bandpass is represented as the measurements. The
correction factor to flatten the bandpass would be the complex-valued
reciprocal of these measurements.

.. _sec:acb:

.acb
----

When generation of sniffer output files is not disabled, each ``.FITS``
file written by ``difx2fits`` will be accompanied by a corresponding
``.acb`` file. This file contains auto-correlation spectra for each
antenna for each source. In order to minimize the output data size,
spectra for the same source will only be repeated once per 15 minutes.
The file contains many concatenated records. Each record has the spectra
for all baseband channels for a particular antenna and has the following
format. Note that no spaces are allowed within any field. Values in
``typewriter`` font without comments are explicit strings that are
required.

+----------------+----------------+----------------+----------------+
| Line(s)        | Value          | Units          | Comments       |
+================+================+================+================+
| 1              | ``timerange:`` |                |                |
+----------------+----------------+----------------+----------------+
|                | *MJD*          | integer        | MJD day number |
|                |                | :math:`\ge 1`  | corresponding  |
|                |                |                | to line        |
+----------------+----------------+----------------+----------------+
|                | *start time*   | string         | e.g.,          |
|                |                |                | `              |
|                |                |                | `13h34m22.6s`` |
+----------------+----------------+----------------+----------------+
|                | *stop time*    | string         | e.g.,          |
|                |                |                | `              |
|                |                |                | `13h34m52.0s`` |
+----------------+----------------+----------------+----------------+
|                | ``obscode:``   |                |                |
+----------------+----------------+----------------+----------------+
|                | *observe code* | string         | e.g., MT831    |
+----------------+----------------+----------------+----------------+
|                | ``chans:``     |                |                |
+----------------+----------------+----------------+----------------+
|                | *              | :math:`\ge 1`  | number of      |
|                | n*\ :math:`_{\ |                | channels per   |
|                | mathrm{chan}}` |                | baseband       |
|                |                |                | channel        |
+----------------+----------------+----------------+----------------+
|                | ``x``          |                |                |
+----------------+----------------+----------------+----------------+
|                | *n*\ :math:`_{ | :math:`\ge 1`  | number of      |
|                | \mathrm{BBC}}` |                | baseband       |
|                |                |                | channels       |
+----------------+----------------+----------------+----------------+
| 2              | ``source:``    |                |                |
+----------------+----------------+----------------+----------------+
|                | *source name*  | string         | e.g.,          |
|                |                |                | ``0316+413``   |
+----------------+----------------+----------------+----------------+
|                | ``bandw:``     |                |                |
+----------------+----------------+----------------+----------------+
|                | *bandwidth*    | MHz            | baseband       |
|                |                |                | channel        |
|                |                |                | bandwidth      |
+----------------+----------------+----------------+----------------+
|                | ``MHz``        |                |                |
+----------------+----------------+----------------+----------------+
| 3 to           | ``bandfreq:``  |                |                |
| 2+\ :math:`n_{ |                |                |                |
| \mathrm{BBC}}` |                |                |                |
+----------------+----------------+----------------+----------------+
|                | *frequency*    | GHz            | band edge      |
|                |                |                | (SSLO)         |
|                |                |                | frequency of   |
|                |                |                | baseband       |
|                |                |                | channel        |
+----------------+----------------+----------------+----------------+
|                | ``GHz polar:`` |                |                |
+----------------+----------------+----------------+----------------+
|                | *polarization* | 2 chars        | e.g. ``RR`` or |
|                |                |                | ``LL``         |
+----------------+----------------+----------------+----------------+
|                | ``side:``      |                |                |
+----------------+----------------+----------------+----------------+
|                | *sideband*     | ``U`` or ``L`` | for upper or   |
|                |                |                | lower sideband |
+----------------+----------------+----------------+----------------+
|                | ``bbchan:``    |                |                |
+----------------+----------------+----------------+----------------+
|                | *bbc*          | ``0``          | Currently not  |
|                |                |                | used but       |
|                |                |                | needed for     |
|                |                |                | conformity     |
+----------------+----------------+----------------+----------------+
| 3+\ :math:`n_{ | *antenna       | :math:`\ge 1`  | antenna table  |
| \mathrm{BBC}}` | number*        |                | index          |
| to             |                |                |                |
+----------------+----------------+----------------+----------------+
| 2+\ :ma        | *antenna name* | string         |                |
| th:`n_{\mathrm |                |                |                |
| {BBC}}(n_{\mat |                |                |                |
| hrm{chan}}+1)` |                |                |                |
+----------------+----------------+----------------+----------------+
|                | *channel       | :math:`\ge 1`  | :math:`= \     |
|                | number*        |                | mathrm{chan} + |
|                |                |                |  (\mathrm{bbc} |
|                |                |                | -1) \cdot n_{\ |
|                |                |                | mathrm{chan}}` |
|                |                |                | for chan, bbc  |
|                |                |                | :math:`\ge 1`  |
+----------------+----------------+----------------+----------------+
|                | *amplitude*    | :              |                |
|                |                | math:`\ge 0.0` |                |
+----------------+----------------+----------------+----------------+

The above are repeated for each auto-correlation spectrum record. This
file can be plotted directly with ``plotbp`` or handled more
automatically with ``difxsniff``.

.. _sec:apc:

.apc
----

This file type is nearly identical to the better known ``.apd`` file;
the name acronym refers to Amplitude Phase Channel. The amplitude,
phase, and rate for the brightest channel is determined for each IF for
each solution interval. When generation of sniffer output files is not
disabled, each ``.FITS`` file written by ``difx2fits`` will be
accompanied by a corresponding ``.apc`` file. This file contains
*channel-based* fringe fit solutions typically every 30 seconds for the
entire experiment. These solutions are not of calibration quality but
are sufficient for use in evaluating the data quality.

The first line in the file is the observation code, e.g., ``MT831`` .

Each subsequent line has the same format with the following fields:

+----------------------+----------------------+----------------------+
| Key                  | Units/allowed values | Comments             |
+======================+======================+======================+
| *MJD*                | integer              | MJD day number       |
|                      | :math:`\ge 1`        | corresponding to     |
|                      |                      | line                 |
+----------------------+----------------------+----------------------+
| *hour*               | :math:`\ge 0.0`,     | hour within day      |
|                      | :math:`< 24.0`       |                      |
+----------------------+----------------------+----------------------+
| *source number*      | integer              | source table index   |
|                      | :math:`\ge 1`        |                      |
+----------------------+----------------------+----------------------+
| *source name*        | string               | name of source; no   |
|                      |                      | spaces allowed       |
+----------------------+----------------------+----------------------+
| *ant1 number*        | integer              | antenna table index  |
|                      | :math:`\ge 1`        | for first antenna    |
+----------------------+----------------------+----------------------+
| *ant2 number*        | integer              | antenna table index  |
|                      | :math:`\ge 1`        | for second antenna   |
+----------------------+----------------------+----------------------+
| *ant1 name*          | string               | name of antenna 1;   |
|                      |                      | no spaces allowed    |
+----------------------+----------------------+----------------------+
| *ant2 name*          | string               | name of antenna 2;   |
|                      |                      | no spaces allowed    |
+----------------------+----------------------+----------------------+
| *n*\ :ma             | integer              | number of baseband   |
| th:`_{\mathrm{BBC}}` | :math:`\ge 1`        | channels,            |
|                      |                      | :mat                 |
|                      |                      | h:`n_{\mathrm{BBC}}` |
+----------------------+----------------------+----------------------+
|                      |                      | The next four        |
|                      |                      | columns are repeated |
|                      |                      | :mat                 |
|                      |                      | h:`n_{\mathrm{BBC}}` |
|                      |                      | times                |
+----------------------+----------------------+----------------------+
| *channel*            | :math:`\ge 1`,       | the strongest        |
|                      | :math:`              | channel              |
|                      | \le n_\mathrm{chan}` |                      |
+----------------------+----------------------+----------------------+
| *amplitude*          | :math:`\ge 0.0`      | the amplitude of the |
|                      |                      | peak channel         |
+----------------------+----------------------+----------------------+
| *phase*              | degrees              | phase of the peak    |
|                      |                      | channel              |
+----------------------+----------------------+----------------------+
| *rate*               | Hz                   | the channel phase    |
|                      |                      | rate                 |
+----------------------+----------------------+----------------------+

.. _sec:apd:

.apd
----

When generation of sniffer output files is not disabled, each ``.FITS``
file written by ``difx2fits`` will be accompanied by a corresponding
``.apd`` file. This file contains Amplitude, Phase, Delay (hence the
name) and rate results from fringe fit solutions typically every 30
seconds for the entire experiment. These solutions are not of
calibration quality but are sufficient for use in evaluating the data
quality.

The first line in the file is the observation code, e.g., ``MT831`` .

Each subsequent line has the same format with the following fields:

+----------------------+----------------------+----------------------+
| Key                  | Units/allowed values | Comments             |
+======================+======================+======================+
| *MJD*                | integer              | MJD day number       |
|                      | :math:`\ge 1`        | corresponding to     |
|                      |                      | line                 |
+----------------------+----------------------+----------------------+
| *hour*               | :math:`\ge 0.0`,     | hour within day      |
|                      | :math:`< 24.0`       |                      |
+----------------------+----------------------+----------------------+
| *source number*      | integer              | source table index   |
|                      | :math:`\ge 1`        |                      |
+----------------------+----------------------+----------------------+
| *source name*        | string               | name of source; no   |
|                      |                      | spaces allowed       |
+----------------------+----------------------+----------------------+
| *ant1 number*        | integer              | antenna table index  |
|                      | :math:`\ge 1`        | for first antenna    |
+----------------------+----------------------+----------------------+
| *ant2 number*        | integer              | antenna table index  |
|                      | :math:`\ge 1`        | for second antenna   |
+----------------------+----------------------+----------------------+
| *ant1 name*          | string               | name of antenna 1;   |
|                      |                      | no spaces allowed    |
+----------------------+----------------------+----------------------+
| *ant2 name*          | string               | name of antenna 2;   |
|                      |                      | no spaces allowed    |
+----------------------+----------------------+----------------------+
| *n*\ :ma             | integer              | number of baseband   |
| th:`_{\mathrm{BBC}}` | :math:`\ge 1`        | channels,            |
|                      |                      | :mat                 |
|                      |                      | h:`n_{\mathrm{BBC}}` |
+----------------------+----------------------+----------------------+
|                      |                      | The next four        |
|                      |                      | columns are repeated |
|                      |                      | :mat                 |
|                      |                      | h:`n_{\mathrm{BBC}}` |
|                      |                      | times                |
+----------------------+----------------------+----------------------+
| *delay*              | ns                   | the fringe fit delay |
+----------------------+----------------------+----------------------+
| *amplitude*          | :math:`\ge 0.0`      | the amplitude of     |
|                      |                      | fringe fit peak      |
+----------------------+----------------------+----------------------+
| *phase*              | degrees              | phase of fringe fit  |
|                      |                      | peak                 |
+----------------------+----------------------+----------------------+
| *rate*               | Hz                   | the fringe fit rate  |
+----------------------+----------------------+----------------------+

.. _sec:filebandpass:

.bandpass
---------

When run with the ``–bandpass`` option, ``difx2fits`` will create a
``.bandpass`` file. The data in this file is created after applying the
results of the fringe fit process and then averaging over all data. This
option should only be used when it is expected that all of the data
being processed is on a source strong enough for valid fringe fit
solutions. The contents of the file complex-valued bandpasses determined
on each of the baselines. In its current form (2023/04/24) only
cross-correlations are considered, but the option remains open to write
real-valued autocorrelations as well.

It is possible that line comments, starting with #, are present in the
file.

The first line of the file is ``obscode:`` followed by the project code.

A header line starting with ``Bandpass`` will signify the start of the
data for one baseline for one baseband channel. Such a line has exactly
ten space-separated fields:

#. The keyword ``Bandpass``

#. Zero-based antenna number for first antenna

#. Zero-based antenna number for second antenna

#. Uppercase antenna code for first antenna

#. Uppercase antenna code for second antenna

#. Baseband channel number (zero-based)

#. Number of points in the bandpass, :math:`N_{\rm point}`

#. Signed Sum of Local Oscillator (SSLO) for the baseband channel
   frequency (MHz)

#. Baseband channel bandwidth (MHz)

#. Polarization (typically ``R``, ``L``, ``H``, or ``V``)

Following a header line should be the specified number,
:math:`N_{\rm point}`, of data lines, each with three space-separated
values:

#. Sky frequency (MHz)

#. Real part of bandpass at this frequency

#. Imaginary part of bandpass at this frequency

Note that the bandpass is represented as the measurements. The
correction factor to flatten the bandpass would be the complex-valued
reciprocal of these measurements.

.. _sec:binconfig:

.binconfig
----------

The ``.binconfig`` file is a file created by the user of DiFX and
referenced by the ``.input`` file to specify pulsar options. The file
uses the standard DiFX input file format and has the following
parameters:

+-------------------+-----------------------+-----------------------+
| Key               | Units/allowed values  | Comments              |
+===================+=======================+=======================+
| NUM POLYCO FILES  | integer :math:`\ge 1` | Number of polyco      |
|                   |                       | files to read         |
|                   |                       | (*nPoly*)             |
+-------------------+-----------------------+-----------------------+
|                   |                       | The next row is       |
|                   |                       | duplicated *nPoly*    |
|                   |                       | times                 |
+-------------------+-----------------------+-----------------------+
| POLYCO FILE *p*   | string                | Name of               |
|                   |                       | *p*\ :math:`^{th}`    |
|                   |                       | polynomial file       |
+-------------------+-----------------------+-----------------------+
| NUM PULSAR BINS   | integer :math:`\ge 1` | Number of pulse bins  |
|                   |                       | to create (*nBin*)    |
+-------------------+-----------------------+-----------------------+
| SCRUNCH OUTPUT    | boolean               | Sum weighted bins? If |
|                   |                       | not, write all bins   |
+-------------------+-----------------------+-----------------------+
|                   |                       | The next rows are     |
|                   |                       | duplicated *nBin*     |
|                   |                       | times                 |
+-------------------+-----------------------+-----------------------+
| BIN PHASE END *b* | float 0.0-1.0         | Pulsar phase where    |
|                   |                       | bin ends              |
+-------------------+-----------------------+-----------------------+
| BIN WEIGHT *b*    | float :math:`\ge 0.0` | Weight to use when    |
|                   |                       | scrunching            |
+-------------------+-----------------------+-----------------------+

The start of one bin is equal to the end of the previous bin; bins wrap
around phase 1.0. The BIN PHASE END parameters must be listed in
ascending phase order. See Sec. `[sec:pulsars] <#sec:pulsars>`__ for
example usage of ``.binconfig`` files.

.. _sec:bootstrapfile:

.bootstrap
----------

The ``difxbuild`` installer program begins its process by building an
environment based on the contents of a ``.bootstrap`` file. In the
simplest case only three parameters are required (``version``,
``headnode``, and ``difxbase``), however installations can be customized
through the use of several other parameters. The ``.bootstrap`` file is
a text file containing zero or one *key = value* statements on each
line. Comments begin with a #.

The parameters specified can include:

-  ``version``: which version of difx to install. Currently supported
   values are DIFX-DEVEL, DIFX-2.1, and DIFX-2.2. The ``DIFX_VERSION``
   environment varialbe will reflect this value. This parameter is
   required.

-  ``headnode``: the computer that will be singled out as the head node.
   The ``DIFX_HEAD_NODE`` environment variable will reflect this value.
   This parameter is required.

-  ``difxbase``: the top level directory for DiFX software.
   DiFX-version-independent files will be placed directly beneath this
   directory. By default DiFX version specific files will be installed
   in a subdirectory of this (see information about the root parameter
   below). It is okay (and encouraged) to use the same difxbase for all
   installed versions as this allows common third-party software to be
   used. This parameter is required.

-  ``root``: the base directory for DiFX version/label specific files
   for the primary platform. Secondary platforms will use the same but
   with a provided extension (see altplatformX below). If not provided,
   this parameter will default to *difxbase* (or *label* if specified).

-  ``ipproot``: path to the base of the Intel Performance Primitives
   library. This is IPP version dependent and may require a bit of trial
   and error to get right. If this is set to none then an IPP-free DiFX
   will be installed. This requires FFTW to be installed. Each
   architecture can have its own ipproot value. ``ipproot`` specifies
   the the default; architecture-dependent overrides are specified with
   a parameter such as ``ipprooti686`` or ``ipprootx86_64``.

-  ``label``: a label used to identify an installation of DiFX. By
   default it is set equal to the specified version. Setting it to an
   alternate value allows multiple installations of the same DiFX
   version to be later identifiable. The ``DIFX_LABEL`` environment
   variable will reflect this value.

-  ``calcserver``: the computer to send RPC model requests to. If not
   specified, this will default to the value of the headnode parameter.
   The ``CALC_SERVER`` environment variable will reflect this value.

-  ``cflags``: default c and c++ compiler flags to use. If not
   specified, the default of ``-O2 -Wall -march=core2`` will be used.

-  ``pathextra``: extra binary search paths to add the the ``PATH``
   environment variable that is set in the ``setup_difx`` script.

-  ``ldextra``: extra paths to be added to the ``LD_LIBRARY_PATH``
   environment variable that is set in the ``setup_difx`` script.

-  ``wrapper``: an optional wrapper program that can be used to spawn
   ``mpifxcorr``. This value gets coded into the
   ``runmpifxcorr.``\ *label* launcher script. For example, ``valgrind``
   could be used as the wrapper program if memory leek checking is
   desired. Use this parameter with caution.

-  ``mca``: parameters to add to the ``/etc/openmpi-mca-params.conf``
   file. If not provided, no such file will be created. This can be
   useful to include or exclude certain network interfaces. You can set
   this on a per platform basis. To do this, for example, set
   ``mcai686`` and ``mcax86_64`` separately.

-  ``primaryarch``: Normally bootstrapping needs to be done on a machine
   running on the primary architecture. If ``primaryarch`` is set, the
   bootstrapping step can be run on any machine. This should be set to
   ``i686``, ``x86_64``, or whatever ``"uname -m"`` returns on the
   primary architecture.

-  ``altplatform``\ *X*: Here *X* is a number from 1 to 9. This
   parameter gives a sub-label to each non-primary platform. Examples
   might be ``SDK8`` and ``SDK9`` for Mark5 units using two different
   Conduant library versions. For each specified alternate platform the
   following three additional parameters are needed …

   -  ``altplatform``\ *X*\ ``arch``: The CPU architecture, as
      determined by ``"uname -m"``, that this platform is based upon.

   -  ``altplatform``\ *X*\ ``host``: A representative computer making
      use of this platform. This is used when spawning a parallel build
      process.

   -  ``altplatform``\ *X*\ ``test``: A bash conditional expression used
      to determine if the computer running the ``setup_bash`` script
      belongs to this platform. An example is:
      ``x‘pkg-config –modversion streamstor‘ < "x9.0"``

.. _sec:cablecal:

.cablecal
---------

Cable calibration is used to measure the electrical pathlength of the
oscillator signals being sent from the control building to the antenna
vertex room. The VLBA was designed to minimize pathlength variations
over time, but inevitably some temperature change and stretching due to
antenna motion is to be expected. At the VLBA, the program ``db2cc`` is
used to pull cable cal from the VLBA monitor database and format it in a
manner that can be read by ``difx2fits``. There will be one file per
antenna which is given filename of *exp*.\ *stn*.cablecal, where *exp*
is the experiment code and *stn* is the antenna station code.

The ``.cablecal`` files are text format files. Comments within the file
start with #. Valid data lines have four space-separated columns of
data:

-  station code (in capital letters, usually two characters long)

-  MJD timestamp

-  integration period for the measurement (measured in seconds, or zero
   if not specified)

-  the cable cal round-trip pathlenght (measured in picoseconds)

.. _sec:cal:

cal.vlba
--------

Monitor data that gets attached to FITS files is extracted by ``tsm``
into a file called *project*\ ``cal.vlba`` where *project* is the name
of the project, i.e., ``bg167`` or ``bc120a``. A single file contains
the monitor data for all VLBA antennas, maybe also including GBT,
Effelsberg and Arecibo, for the duration of the project. The file is
left in ``/home/vlbiobs`` and is compressed with ``gzip`` after some
time to save disk space, resulting in additional file extension ``.gz``.
A program called ``vlog`` (sec §\ `[sec:vlog] <#sec:vlog>`__) reads this
file and produces files called ``flag``, ``pcal``, ``tsys``, and
``weather`` in the software correlator project directory. This file type
can be read by AIPS task ANTAB.

.. _sec:calc:

.calc
-----

The main use of the ``.calc`` file is to drive the geometric model
calculations but this file also serves as a convenient place to store
information that is contained in the ``.fx`` file but not in the
``.input file`` and is needed for ``.FITS`` file creation. In the DiFX
system, one ``.calc`` file is created by ``vex2difx``
(§\ `[sec:vex2difx] <#sec:vex2difx>`__) for each ``.input`` file. This
file is read by ``calcif2``) (§\ `[sec:calcif2] <#sec:calcif2>`__) to
produce a tabulated delay model, :math:`u, v, w` values, and estimates
of atmospheric delay contributions.

In brief, the parameters in this file that are relevant for correlation
include time, locations and geometries of antennas, pointing of antennas
(and hence delay centers) as a function of time and the Earth
orientation parameters relevant for the correlator job in question.
Additional parameters that are stuffed into this file include spectral
averaging, project name, and information about sources such as
calibration code and qualifiers. In the NRAO application of DiFX, source
names are faked in the actual ``.input`` file in order to allow multiple
different configurations for the same source. A parameter called
*realname* accompanies each source name in the ``.calc`` file to
correctly populate the source file in ``.FITS`` file creation.

The syntax of this file is similar to that of the ``.input`` file. The
file consists entirely of key-value pairs separated by a colon. The
value column is not constrained to start in column 21 as it is for the
files used by ``mpifxcorr``. There are five sections in the ``.calc``
file; these sections are not separated by any explicit mark in the file.

The first section contains values that are fixed for the entire
experiment and at all antennas; all data in this section is scalar. In
the following table, all numbers are assumed to be floating point unless
further restricted. The keys and allowed values in this section are
summarized below. Optional keys are identified with a :math:`\star`.
Deprecated keys that will likely be removed in an upcoming version are
identified with an :math:`\times`.

+--------------+-----------------------+--------------------------+
| Key          | Units/allowed values  | Comments                 |
+==============+=======================+==========================+
| JOB ID       | integer :math:`\ge 1` | taken from ``.fx`` file  |
+--------------+-----------------------+--------------------------+
|              | MJD + fraction        | start time of original   |
|              |                       | ``.fx`` file             |
+--------------+-----------------------+--------------------------+
|              | MJD + fraction        | end time of original     |
|              |                       | ``.fx`` file             |
+--------------+-----------------------+--------------------------+
|              | float :math:`\le 1`   | fraction of the job      |
|              |                       | contained within scans   |
+--------------+-----------------------+--------------------------+
| OBSCODE      | string                | observation code         |
|              |                       | assigned to project      |
+--------------+-----------------------+--------------------------+
|              | short string          | session suffix to        |
|              |                       | OBSCODE, e.g., ``A`` or  |
|              |                       | ``BE``                   |
+--------------+-----------------------+--------------------------+
|              | string                | version of correlator,   |
|              |                       | e.g. ``DIFX-1.5``        |
+--------------+-----------------------+--------------------------+
|              | string                | name of correlator       |
|              |                       | install, e.g.            |
|              |                       | ``DIFX-WALTER``          |
+--------------+-----------------------+--------------------------+
| VEX FILE     | string                | dir/filename of vex file |
|              |                       | used to create the job   |
+--------------+-----------------------+--------------------------+
| START MJD    | MJD + fraction        | start time of this       |
|              |                       | subjob                   |
+--------------+-----------------------+--------------------------+
| START YEAR   | integer               | calendar year of START   |
|              |                       | MJD                      |
+--------------+-----------------------+--------------------------+
| START MONTH  | integer               | calendar month of START  |
|              |                       | MJD                      |
+--------------+-----------------------+--------------------------+
| START DAY    | integer               | day of calendar month of |
|              |                       | START MJD                |
+--------------+-----------------------+--------------------------+
| START HOUR   | integer               | hour of START MJD        |
+--------------+-----------------------+--------------------------+
| START MINUTE | integer               | minute of START MJD      |
+--------------+-----------------------+--------------------------+
| START SECOND | integer               | second of START MJD      |
+--------------+-----------------------+--------------------------+
|              | integer :math:`\ge 1` | number of channels to    |
|              |                       | average in FITS creation |
+--------------+-----------------------+--------------------------+
|              | integer :math:`\ge 0` | start channel number     |
|              |                       | (before averaging)       |
+--------------+-----------------------+--------------------------+
|              | integer :math:`\ge 1` | total number of channels |
|              |                       | to write to FITS         |
+--------------+-----------------------+--------------------------+
|              | :math:`> 0.0 , < 1.0` | fraction of total        |
|              |                       | channels to write to     |
|              |                       | FITS                     |
+--------------+-----------------------+--------------------------+
|              | string                | currently only           |
|              |                       | ``UNIFORM`` is supported |
+--------------+-----------------------+--------------------------+

The second section contains antenna(telescope) specific information.
After an initial parameter defining the number of telescopes, there are
*nTelescope* sections (one for each antenna), each with the following
six parameters. Lowercase *t* in the table below is used to indicate the
telescope index, an integer ranging from 0 to *nTelescope* - 1. Note
that in cases where units are provided under the Key column, these units
are actually part of the key.

+----------------------+----------------------+----------------------+
| Key                  | Units/allowed values | Comments             |
+======================+======================+======================+
| NUM TELESCOPES       | integer              | number of telescopes |
|                      | :math:`\ge 1`        | (*nTelescope*).      |
+----------------------+----------------------+----------------------+
|                      |                      | The rows below are   |
|                      |                      | duplicated           |
|                      |                      | *nTelescope* times.  |
+----------------------+----------------------+----------------------+
| TELESCOPE *t* NAME   | string               | upper case antenna   |
|                      |                      | name abbreviation    |
+----------------------+----------------------+----------------------+
| TELESCOPE *t* MOUNT  | string               | the mount type:      |
|                      |                      | altz, equa, xyew, or |
|                      |                      | xyns                 |
+----------------------+----------------------+----------------------+
| TELESCOPE *t* OFFSET | meters               | axis offset in       |
| (m)                  |                      | meters               |
+----------------------+----------------------+----------------------+
| TELESCOPE *t* X (m)  | meters               | X geocentric         |
|                      |                      | coordinate of        |
|                      |                      | antenna at date      |
+----------------------+----------------------+----------------------+
| TELESCOPE *t* Y (m)  | meters               | Y geocentric         |
|                      |                      | coordinate of        |
|                      |                      | antenna at date      |
+----------------------+----------------------+----------------------+
| TELESCOPE *t* Z (m)  | meters               | Z geocentric         |
|                      |                      | coordinate of        |
|                      |                      | antenna at date      |
+----------------------+----------------------+----------------------+
| *t* SHELF            | string               | shelf location of    |
|                      |                      | module to correlate  |
+----------------------+----------------------+----------------------+

The third section contains a table of sources. Sources are indexed from
the following section describing the scans.

+--------------------+-----------------------+-----------------------+
| Key                | Units/allowed values  | Comments              |
+====================+=======================+=======================+
| NUM SOURCES        | integer :math:`\ge 1` | number of sources     |
|                    |                       | (*nSource*)           |
+--------------------+-----------------------+-----------------------+
|                    |                       | The rows below are    |
|                    |                       | duplicated *nSource*  |
|                    |                       | times.                |
+--------------------+-----------------------+-----------------------+
| SOURCE *s* NAME    | string                | name of source        |
|                    |                       | (possibly renamed     |
|                    |                       | from ``.vex``         |
+--------------------+-----------------------+-----------------------+
| SOURCE *s* RA      | radians               | J2000 right ascension |
+--------------------+-----------------------+-----------------------+
| SOURCE *s* DEC     | radians               | J2000 declination     |
+--------------------+-----------------------+-----------------------+
| SOURCE *s* CALCODE | string                | usually upper case    |
|                    |                       | letters or blank      |
+--------------------+-----------------------+-----------------------+
| SOURCE *s* QUAL    | integet :math:`\ge 0` | source qualifier      |
+--------------------+-----------------------+-----------------------+

The fourth section contains scan specific information. Except for one
initial line specifying the number of scans, *nScan*, this section is
composed of nine parameters per scan. Each parameter is indexed by *s*
which ranges from 0 to *nScan* - 1.

+----------------------+----------------------+----------------------+
| Key                  | Units/allowed values | Comments             |
+======================+======================+======================+
| NUM SCANS            | integer              | number of scans      |
|                      | :math:`\ge 1`        | (*nScan*)            |
+----------------------+----------------------+----------------------+
|                      |                      | The rows below are   |
|                      |                      | duplicated *nScan*   |
|                      |                      | times.               |
+----------------------+----------------------+----------------------+
| SCAN *s* IDENTIFIER  | string               | name of the scan     |
|                      |                      | (not of the source)  |
+----------------------+----------------------+----------------------+
| SCAN *s* START (S)   | seconds              | start time of scan,  |
|                      |                      | relative to job      |
|                      |                      | start time           |
+----------------------+----------------------+----------------------+
| SCAN *s* DUR (S)     | seconds              | duration of scan     |
+----------------------+----------------------+----------------------+
| SCAN *s* OBS MODE    | string               | reference to         |
| NAME                 |                      | ``.input`` file      |
|                      |                      | configuration        |
+----------------------+----------------------+----------------------+
| SCAN *s* UVSHIFT     | time to integrate    |                      |
| INTERVAL (NS)        | before doing uv      |                      |
|                      | shifts (used mainly  |                      |
|                      | for                  |                      |
|                      | multi-phase-center   |                      |
|                      | observing)           |                      |
+----------------------+----------------------+----------------------+
| SCAN *s* AC AVG      | averaging interval   |                      |
| INTERVAL (NS)        | for export of        |                      |
|                      | fast-dump spectra    |                      |
|                      | (used for VFASTR)    |                      |
+----------------------+----------------------+----------------------+
| SCAN *s* POINTING    | integer              | source table index   |
| SRC                  | :math:`\ge 1`        | identifying pointing |
|                      |                      | center of scan       |
+----------------------+----------------------+----------------------+
| SCAN *s* NUM PHS     | integer              | number of phase      |
| CTRS                 | :math:`\ge 1`        | centers (*nPC*)      |
+----------------------+----------------------+----------------------+
|                      |                      | The rows below are   |
|                      |                      | duplicated *nPC*     |
|                      |                      | times.               |
+----------------------+----------------------+----------------------+
| SCAN *s* PHS CTR *p* | integer              | index to the source  |
|                      | :math:`\ge 1`        | table                |
+----------------------+----------------------+----------------------+

The fifth section contains Earth orientation parameters (EOP). Except
for one initial line specifying the number of days of EOPs, *nEOP*, this
section is composed of five parameters per day of sampled EOP values.
Each parameter is indexed by *e* which ranges from 0 to *nEOP* - 1.

+----------------------+----------------------+----------------------+
| Key                  | Units/allowed values | Comments             |
+======================+======================+======================+
| NUM EOP              | integer              | number of tabulated  |
|                      | :math:`\ge 1`        | EOP values (*nEOP*)  |
+----------------------+----------------------+----------------------+
|                      |                      | The rows below are   |
|                      |                      | duplicated *nEOP*    |
|                      |                      | times.               |
+----------------------+----------------------+----------------------+
| EOP *e* TIME (MJD)   | MJD + fraction       | time of sample;      |
|                      |                      | fraction almost      |
|                      |                      | always zero          |
+----------------------+----------------------+----------------------+
| EOP *e* TAI_UTC      | integer seconds      | leap seconds accrued |
| (sec)                |                      | at time of job start |
+----------------------+----------------------+----------------------+
| EOP *e* UT1_UTC      | seconds              | UT1 - UTC            |
| (sec)                |                      |                      |
+----------------------+----------------------+----------------------+
| EOP *e* XPOLE        | arc seconds          | X coordinate of      |
| (arcsec)             |                      | polar offset         |
+----------------------+----------------------+----------------------+
| EOP *e* YPOLE        | arc seconds          | Y coordinate of      |
| (arcsec)             |                      | polar offset         |
+----------------------+----------------------+----------------------+

The next (completely optional) section has a table for positions and
velocites of spacecraft. Each spacecraft is indexed by *s* and each row
thereof by *r*.

+----------------------+----------------------+----------------------+
| Key                  | Units/allowed values | Comments             |
+======================+======================+======================+
|                      | integer              | number of spacecraft |
|                      | :math:`\ge 0`        | (*nSpacecraft*)      |
+----------------------+----------------------+----------------------+
|                      |                      | Everything below is  |
|                      |                      | duplicated           |
|                      |                      | *nSpacecraft* times. |
+----------------------+----------------------+----------------------+
| SPACECRAFT *s* NAME  | string               | name of spacecraft   |
+----------------------+----------------------+----------------------+
| SPACECRAFT *s* ROWS  | integer              | number of data rows, |
|                      | :math:`\ge 1`        | *nRow*\ :math:`_s`   |
|                      |                      | for spacecraft *s*   |
+----------------------+----------------------+----------------------+
|                      |                      | The row below is     |
|                      |                      | repeated             |
|                      |                      | *nRow*\ :math:`_s`   |
|                      |                      | times.               |
+----------------------+----------------------+----------------------+
| SPACECRAFT *s* ROW   | 7 numbers            | tabulated data; see  |
| *r*                  |                      | below                |
+----------------------+----------------------+----------------------+

Each data vector of data consists of seven double precision values: time
(mjd), :math:`x`, :math:`y`, and :math:`z` (meters), and
:math:`\dot{x}`, :math:`\dot{y}`, and :math:`\dot{z}` (meters per
second). These values should be separated by spaces.

The final section identifies the files to be produced.

+---------------+----------------------+-------------------------+
| Key           | Units/allowed values | Comments                |
+===============+======================+=========================+
| IM FILENAME   | string               | dir/filename of ``.im`` |
|               |                      | file to create          |
+---------------+----------------------+-------------------------+
| FLAG FILENAME | string               | dir/filename of         |
|               |                      | ``.flag`` file to       |
|               |                      | create                  |
+---------------+----------------------+-------------------------+

.. _sec:difx:

.difx/
------

The SWIN format visibilities produced by ``mpifxcorr`` are written to a
directory with extension ``.difx``. Three kinds of files can be placed
in this directory as described below.

Note that the formats and naming conventions of these files is not
guaranteed to stay unchanged from version to version of DiFX, and hence
it is not recommended to rely on these files for archival purposes.

.. _sec:difxvisibilities:

Visibility files
~~~~~~~~~~~~~~~~

The bulk of the output from ``mpifxcorr`` is usually in the form of a
binary visibility file. Usually there will be a single visibility file
in this directory, but there are three ways in which multiple files may
be produced: 1. a restart of the correlation, 2. if there are multiple
phase centers, and 3. if there are multiple pulsar bins.

The visibility files are systematically named in the form:
``DIFX_``\ *day*\ ``_``\ *sec*\ ``.s``\ *src*\ ``.b``\ *bin*, where
*day* is the 5 digit integer MJD of the start of visibilities, *sec* is
a zero-padded 6 digit number of seconds since the MJD midnight, *src* is
a 4 digit zero-padded integer specifying the phase center number
(starting at 0), and *bin* is a 4 digit zero-padded integer specifying
the pulsar bin number (starting at 0).

These files contain visibility data records. Each record contains the
visibility spectrum for one polarization of one baseband channel of one
baseline for one integration time. Each starts with a binary header and
is followed by binary data.

The format of the header is shown in the table below.

+-----------------+-----------+-----------------+-----------------+
| Key             | data type | units           | comments        |
+=================+===========+=================+=================+
| baseline number | int       |                 | :               |
|                 |           |                 | math:`= (a_1+1) |
|                 |           |                 | *256 + (a_2+1)` |
|                 |           |                 | for             |
|                 |           |                 | :math:`         |
|                 |           |                 | a_1, a_2 \ge 1` |
+-----------------+-----------+-----------------+-----------------+
| day number      | int       | MJD             | date of         |
|                 |           |                 | visibility      |
|                 |           |                 | centroid        |
+-----------------+-----------+-----------------+-----------------+
| seconds         | double    | sec             | vis. centroid   |
|                 |           |                 | seconds since   |
|                 |           |                 | beginning of    |
|                 |           |                 | MJD             |
+-----------------+-----------+-----------------+-----------------+
| config index    | int       | :math:`\ge 0`   | index to        |
|                 |           |                 | ``.input`` file |
|                 |           |                 | configuration   |
|                 |           |                 | table           |
+-----------------+-----------+-----------------+-----------------+
| source index    | int       | :math:`\ge 0`   | index to        |
|                 |           |                 | ``.calc`` file  |
|                 |           |                 | scan number     |
+-----------------+-----------+-----------------+-----------------+
| freq index      | int       | :math:`\ge 0`   | index to        |
|                 |           |                 | ``.input``      |
|                 |           |                 | frequency table |
+-----------------+-----------+-----------------+-----------------+
| antenna 1       | char      | ``R``, ``L``,   |                 |
| polarization    |           | ``X``, ``Y``    |                 |
+-----------------+-----------+-----------------+-----------------+
| antenna 2       | char      | ``R``, ``L``,   |                 |
| polarization    |           | ``X``, ``Y``    |                 |
+-----------------+-----------+-----------------+-----------------+
| pulsar bin      | int       | :math:`\ge 0`   |                 |
| number          |           |                 |                 |
+-----------------+-----------+-----------------+-----------------+
| visibility      | double    | :math:`\ge 0.0` | data weight for |
| weight          |           |                 | spectrum;       |
|                 |           |                 | typically       |
|                 |           |                 | :math:`\sim 1`  |
+-----------------+-----------+-----------------+-----------------+
| :math:`u`       | double    | meter           | :math:`u`       |
|                 |           |                 | component of    |
|                 |           |                 | baseline vector |
+-----------------+-----------+-----------------+-----------------+
| :math:`v`       | double    | meter           | :math:`v`       |
|                 |           |                 | component of    |
|                 |           |                 | baseline vector |
+-----------------+-----------+-----------------+-----------------+
| :math:`w`       | double    | meter           | :math:`w`       |
|                 |           |                 | component of    |
|                 |           |                 | baseline vector |
+-----------------+-----------+-----------------+-----------------+

Note that for both the header and the data, the endianness is native to
the machine running ``mpifxcorr``, and there are currently no provisions
for processing such files on a machine of different endianness.

Following the end-of-line mark for the last header row begins binary
data in the form of (real, imaginary) pairs of 32-bit floating point
numbers. The ``.input`` file parameter ``NUM CHANNELS`` indicates the
number of complex values to expect. In the case of upper sideband data,
the first reported channel is the “zero frequency” channel, that is its
sky frequency is equal to the value in the frequency table for this
spectrum. The Nyquist channel is not retained. For lower sideband data,
the last channel is the “zero frequency” channel. That is, in all cases,
the spectrum is in order of increasing frequency and the Nyquist channel
is excised.

.. _sec:difxpulsecal:

Pulse cal data files
~~~~~~~~~~~~~~~~~~~~

Pulse calibration data can be extracted by ``mpifxcorr``. Extraction is
configured on a per-antenna basis. Data for each antenna is written to a
separate file; if correlation is restarted, an additional pulse cal data
file will be written.

The pulse cal data files are systematically named in the form:
``PCAL_``\ *day*\ ``_``\ *sec*\ ``_``\ *ant*, where *day* is the 5 digit
integer MJD of the start of visibilities, *sec* is a zero-padded 6 digit
number of seconds since the MJD midnight, and *ant* is the 1 or 2 letter
antenna name in capital letters. There is potential for these text files
to have very long lines (more than 10,000 bytes) when many pulse cal
tones are extracted.

For DiFX versions 2.3 and earlier the data format was exactly the same
as documented in §\ `1.35 <#sec:pcal>`__. This old version will be
considered “version 0”.

The data format being used now is similar in spirit but more convenient
for ``mpfixcorr`` to produce and for ``difx2fits`` and ``difx2mark4`` to
digest leading to broader support (in theory complete) of the various
polarization, frequency, and sideband combinations allowed by DiFX. The
data format is as follows:

Comment lines begin with an octothorpe (#). The first few lines of
comments may contain machine-readable information in the following
format:

::

   # DiFX-derived pulse cal data
   # File version = 1
   # Start MJD = 
   # Start seconds = 
   # Telescope name = 

Data lines always contain 6 fixed-size fields:

#. *antId* : Station name abbreviation, e.g., ``LA``

#. *day* : Time centroid of measurement (MJD, including fractional
   portion)

#. *dur* : Duration of measurement (days)

#. *datastreamId* : The datastream index of for this data.

#. *nRecBand* : Number of recorded baseband channels

#. *nTone* : (Maximum) number of pulse cal tones detected per band per
   polarization

Following these fields is a variable-length arrays of numbers. This
array contains the pulse cal data and consists of *nRecBand*nTone*
groups of four numbers. The groups are arranged in ascending record band
index (slow index) and ascending tone number (fast index) where the tone
number increases away from the reference frequency; not sure what
happens with dual-sideband complex! The first member of this group is
the tone frequency (MHz), or -1 to indicate there was not a measurment.
The second member of this group is the polarization, one of ``R``,
``L``, ``X`` or ``Y``. The third and fourth are respectively the real
and imaginary parts of the tone measured at the given sky frequency.

.. _sec:difxswitchedpower:

Switched power files
~~~~~~~~~~~~~~~~~~~~

``mpifxcorr`` can be used to extract switched power from individual
antennas. Extraction is configured on a per-datastream (usually the same
as per-antenna) basis. Data for each data stream is written to a
separate file; if correlation is restarted, an additional set of
switched power files will be started.

The switched power files are systematically named in the form:
``SWITCHEDPOWER_``\ *day*\ ``_``\ *sec*\ ``_``\ *ds*, where *day* is the
5 digit integer MJD of the start of visibilities, *sec* is a zero-padded
6 digit number of seconds since the MJD midnight, and *ds* is the
datastream index as set in the ``.input`` file. The test lines in these
files can be long (more than 1000 bytes).

The format of these files is as follows. Each line of the file
represents all measurements made on one datastream at over one
integration period. The lines contain the following columns:

#. *mjdstart* : The start of the integration period in mjd+fraction

#. *mjdstop* : The end of the integration period in mjd+fraction

These are then followed by 4 numbers for each recorded channel:

#. :math:`P_\mathrm{on}` : power in the “on” state

#. :math:`\sigma_{P_\mathrm{on}}` : uncertainty of the power in the “on”
   state

#. :math:`P_\mathrm{off}` : power in the “off” state

#. :math:`\sigma_{P_\mathrm{off}}` : uncertainty of the power in the
   “off” state

The magnitudes of the numbers are meaningless but their ratios have
meaning. Text after a comment character (#) are ignored.

.. _sec:dotdifxlog:

.difxlog
--------

The ``difxlog`` program
(§\ `[sec:difxlogprogram] <#sec:difxlogprogram>`__) captures
``DifxAlertMessage`` and ``DifxStatusMessage`` message types that are
sent from an ongoing software correlation process and writes the
information contained within to a human readable text file. One line of
text is produced for each received message. The first five columns
contain the date and time in *ddd MMM dd hh:mm:ss yyyy* format (e.g.,
``Wed Apr 22 12:48:41 2009``). The sixth column contains a word
describing the contents of the remainder of the line: Options are:

-  ``STATUS`` : The status of the process is described

-  ``WEIGHTS`` : The playback weights for each antenna are listed

-  *other* : This word represents an alert severity level (one of
   ``FATAL``, ``SEVERE``, ``ERROR``, ``WARNING``, ``INFO``, ``VERBOSE``
   and ``DEBUG``) and is followed by the alert message itself.

.. _sec:speed:

.speed
------

The program ``difxspeed`` (§\ `[sec:difxspeed] <#sec:difxspeed>`__) runs
a set of performance benchmarks described by a ``.speed`` file as
documented here. This file is a text file containing various parameters.
There are 5 required parameters:

+-----------------+---------------------------------------------------+
| Parameter       | Comments                                          |
+=================+===================================================+
| ``datastreams`` | comma separated list of nodes on which to run     |
|                 | datastream processes                              |
+-----------------+---------------------------------------------------+
| ``cores``       | comma separated list of nodes on which to run     |
|                 | core processes                                    |
+-----------------+---------------------------------------------------+
| ``antennas``    | list of 1 or 2 letter antenna names to process    |
+-----------------+---------------------------------------------------+
| ``nThread``     | one or more values listing number of threads to   |
|                 | use (see below)                                   |
+-----------------+---------------------------------------------------+
| ``vex``         | vex file to use as descriptor of observation      |
+-----------------+---------------------------------------------------+

The value of ``nThread`` applies to all core processes; if multiple
comma-separated values are specified, these will result in additional
runs of benchmarking.

Other parameters that can be specified, either as single values or as
arrays to be used in full combination with all other value arrays,
include:

+---------------------+-----------------------------------------------+
| Parameter           | Comments                                      |
+=====================+===============================================+
| ``nAnt``            | number of antennas (in order of listed        |
|                     | antennas, starting with first listed)         |
+---------------------+-----------------------------------------------+
| ``nCore``           | number of core processes to start, using in   |
|                     | order ``cores``                               |
+---------------------+-----------------------------------------------+
| ``tInt``            | integration time (seconds)                    |
+---------------------+-----------------------------------------------+
| ``specRes``         | spectral resolution (MHz)                     |
+---------------------+-----------------------------------------------+
| ``fftSpecRes``      | resolution of transform                       |
+---------------------+-----------------------------------------------+
| ``xmacLength``      | cross-multiply stride size                    |
+---------------------+-----------------------------------------------+
| ``strideLength``    | fringe rotation stride size                   |
+---------------------+-----------------------------------------------+
| ``numBufferedFFTs`` | number of FFTs to process in one go           |
+---------------------+-----------------------------------------------+
| ``toneSelection``   | pulse cal tone selection                      |
+---------------------+-----------------------------------------------+

Notes:

#. ``datastreams`` and ``cores`` lists can repeat hostnames.

#. Some combinations of parameters is illegal; at the moment it is up to
   the user to ensure all combinations of values are allowed.

#. Additional parameters can be easily added to the program on demand.

.. _sec:speed.out:

.speed.out
----------

The output of ``difxspeed`` (§\ `[sec:difxspeed] <#sec:difxspeed>`__) is
a file usually ending in ``.speed.out``. The first many lines are
comments describing to a human reader the fixed parameters of the
benchmark trials and a table describing the meanings of the columns of
the uncommented data lines that follow. In summary, the columns in the
lines that follow are:

+------------------------------+--------------------------------------+
| Column(s)                    | value(s)                             |
+==============================+======================================+
| 1 to :math:`N`               | values of variable parameters as     |
|                              | described by comments above          |
+------------------------------+--------------------------------------+
| :math:`N+1`                  | The average execution time of all    |
|                              | trials run with the combination of   |
|                              | parameters                           |
+------------------------------+--------------------------------------+
| :math:`N+2`                  | The RMS scatter in execution time    |
+------------------------------+--------------------------------------+
| :math:`N+3` to :math:`N+2+R` | List of execution times from all     |
|                              | trials                               |
+------------------------------+--------------------------------------+

In the above table, :math:`N` is the number of parameters taking on
multiple values and :math:`R` is the number of times ``difxspeed`` was
run.

.. _sec:difxmachines:

$DIFX_MACHINES
--------------

This section describes the format of a file used through DiFX-2.2. For
more recent versions please see documentation on the DiFX wiki
http://www.atnf.csiro.au/vlbi/dokuwiki/doku.php/difx/start/ .

Environment variable ``DIFX_MACHINES`` should point to a file containing
a list of machines that are to be considered elements of the software
correlator. Program ``genmachines``
(§\ `[sec:genmachines] <#sec:genmachines>`__) uses this file and
information within a ``.input`` file to populate the ``.machines`` file
needed by ``mpifxcorr``. Because usually only one node in a cluster has
direct access to a particular Mark5 module (or data from that module),
the ordering of computer names in the ``.machines`` file is important.
Rows in the ``$DIFX_MACHINES`` file contain up to three items, the last
one being optional. The first column is the name of the machine. The
second column is the number of processes to schedule on that machine
(typically the number of CPU cores). The third column is a 1 if the
machine is a Mark5 unit and 0 otherwise. If this column is omitted, the
machine will be assumed to be a Mark5 unit if the first 5 characters of
the computer name are ``mark5``, and will be assumed not to be
otherwise. Comments in this file begin with an octothorpe (#). Lines
with fewer than two columns (after excision of comments) are ignored.

.. _sec:dir:

.dir
----

Reading directory information off Mark5 modules can take a bit of time
(measured in minutes usually). Since the same modules are often accessed
multiple times, the directories are cached in ``$MARK5_DIR_PATH/`` . In
this directory, there will be one file per module that has been used,
named *VSN*\ ``.dir``, where *VSN* is the volume serial number of the
module, e.g., NRAO\ :math:`-`\ 023. The format of these files is as
follows: The first line contains three fields: *VSN*, the number of
scans on the module, *nScan*, and either ``A`` or ``B`` indicating the
last bank the module was installed in. At the end of this line the
characters *RT* can be added (by hand) which will cause the modules to
be accessed using *Real-Time* mode which is tolerant of missing or bad
disks within the module. Then there are *nScan* rows containing
information about each scan, each with 11 columns. Values are floating
point unless otherwise noted.

+----------------+----------------------+-------------------------+
| Key            | Units/allowed values | Comments                |
+================+======================+=========================+
| Start byte     | 64-bit integer bytes | offset of the scan on   |
|                |                      | the Mark5 module        |
+----------------+----------------------+-------------------------+
| Length         | 64-bit integer bytes | length of the scan      |
+----------------+----------------------+-------------------------+
| Start day      | integer MJD          | the modified Julian day |
|                |                      | of the scan start       |
+----------------+----------------------+-------------------------+
| Start time     | integer seconds      | the scan start time     |
+----------------+----------------------+-------------------------+
| Frame num      | integer              | frame number since last |
|                |                      | second tick             |
+----------------+----------------------+-------------------------+
| Frames per sec | integer              | number of frames per    |
|                |                      | second                  |
+----------------+----------------------+-------------------------+
| Scan Duration  | seconds              | the duration of the     |
|                |                      | scan                    |
+----------------+----------------------+-------------------------+
| Frame size     | integer bytes        | the length of one data  |
|                |                      | frame, including        |
|                |                      | headers                 |
+----------------+----------------------+-------------------------+
| Frame offset   | integer bytes        | the offset to the start |
|                |                      | of the first entire     |
|                |                      | frame                   |
+----------------+----------------------+-------------------------+
| Tracks         | integer              | the number of data      |
|                |                      | tracks                  |
+----------------+----------------------+-------------------------+
| Format         | integer              | 0 for VLBA format, 1    |
|                |                      | for Mark4 format, 2 for |
|                |                      | Mark5B                  |
+----------------+----------------------+-------------------------+
| Name           | string               | scan name, usually      |
|                |                      | including the project   |
|                |                      | code and station        |
+----------------+----------------------+-------------------------+
| Extra info     | string(s)            | see below               |
+----------------+----------------------+-------------------------+

After the name of the scan additional free-form text can appear. These
extra parameters can be machine parsable. The only use of this as of
this writing is to indicate the thread ids for VDIF data. This will
always be formatted as ``Threads=`` followed by a comma separated list
of thread ids without any spaces. For example: ``Threads=0,640,256,896``
.

.. _sec:filelist:

.filelist
---------

When using the ``filelist`` parameter in an ``ANTENNA`` section of a
``.v2d`` (§\ `1.42 <#sec:v2d>`__), the list of data files to correlate
are stored in a text file. This is a text file containing data lines and
optionally comments. Any text after the comment character (#) is
ignored. A data line consists of a filename (must have complete path as
can be used to find the file on the datastream node for this antenna)
and optionally a start time and stop time. Start and stop times can be
expressed in any of the formats supported by ``vex2difx``.

.. _sec:FITS:

.FITS
-----

The ``.FITS`` files discussed here are produced by ``difx2fits``. They
aim to conform to the same table structures as the FITS-IDI files
produced by the VLBA correlator. The format is described in AIPS Memo
102, “The FITS Interferometry Data Interchange Format”, however, this
memo is a bit out of date and the data structures described are not in
exact agreement with those made by the VLBA correlator; in all cases the
format of data produced by the VLBA hardware correlator is favored where
the two disagree. The tables in these FITS files have a nearly 1 to 1
relationship with the tables that are seen within AIPS, though their two
letter abbreviations differ. The following tables are produced by
``difx2fits``:

===== =========================================================
Table Description
===== =========================================================
AG    The array geometry table
SU    The source table
AN    The antenna table
FR    The frequency table
ML    The model table
CT    The correlator (eop) table
MC    The model components table
SO    The spacecraft orbit table
UV    The visibility data table
FG    The flag table
TS    The system temperature table
PH    The phase calibration table (pulse cals and state counts)
WR    The weather table
GN    The gain curve table
GM    The pulsar gate model table
===== =========================================================

Not all of these tables will always be written.

.. _sec:fitslist:

.fitslist
---------

A ``.fitslist`` file is written by ``makefits`` and contains the entire
list of ``.FITS`` files for the correlator pass. Due to the different
constraints of the correlation process and the FITS-IDI format, the
number of resultant FITS files may be greater or less than the number of
jobs. This file type is used by ``difxarch`` to ensure that all of the
correlated output ends up in the archive. The file is composed of two
parts: a header line and one line for each ``.FITS`` file. The header
line consists of a series of *key=value* pairs. Each *key* and *value*
must have no whitespace and no whitespace should separate these words
from their connecting ``=`` sign. While any number of key-value pairs
may be specified, the following ones (which are case sensitive) are
expected to be present:

#. ``exper`` : the name of the experiment, including the segment code

#. ``pass`` : the name of the correlator pass

#. ``jobs`` : the name of the ``.joblist`` file used by ``makefits``

#. ``mjd`` : the modified Julian day when ``makefits`` created this file

#. ``DiFX`` : the version name for the DiFX deployment (the value of
   ``$DIFX_VERSION`` when ``vex2difx`` was run)

#. ``difx2fits`` : the version of ``difx2fits`` that was run

Each additional line contains information for one ``.FITS`` file of the
correlation pass. These lines contain three fields:

#. *archiveName* : the name of the file that will get injected into the
   archive (see §\ `[sec:archive] <#sec:archive>`__)

#. *fileSize* : the size of the file in MB

#. *origName* : the name of the file as produced by ``difx2fits`` (via
   ``makefits``)

.. _sec:dotflag:

.flag
-----

The program ``vex2difx`` may write a ``.flag`` file for each ``.input``
file it creates. This file is referenced from the ``.calc`` file. This
flag file is used by ``difx2fits`` to exclude nonsense baselines that
might have been correlated. Data from nonsense baselines can occur in
DiFX output when multiple subarrays are coming and going. The flag file
instructs ``difx2fits`` to drop these data during conversion to
FITS-IDI. The format of this text file is as follows. The first line
contains an integer, :math:`n`, which is the number of flag lines that
follow. The next :math:`n` lines each have three numbers: :math:`MJD_1`,
:math:`MJD_2` and :math:`ant`. The first two floating point numbers
determine the time range of the flag in Modified Julian Days. The last
integer number is the antenna number to flag, a zero-based index
corresponding to the ``TELESCOPE`` table of the corresponding ``.input``
file.

.. _sec:dotantflag:

.<antId>.flag
-------------

A series of files called ``.<antId>.flag`` are created when program
``vlog`` operates on the ``cal.vlba`` file. These files contain lists of
antenna-based flags generated by the on-line system that should be
propagated into the FITS FL table. These flag files contains two kinds
of lines. Comment lines begin with an octothorpe (#) and contain no
vital information. Flag lines always consist of exactly 5 fields:

#. *antId* : Station name abbreviation, e.g., ``LA``; also part of the
   file name.

#. *start* : Beginning of flagged period (day of year, including
   fractional portion; or Modified Julian Days)

#. *end* : End of flagged period (day of year, including fractional
   portion; or Modified Julian Days)

#. *recChan* : Record channel affected; -1 for all record channels,
   otherwise a zero-based index

#. *reason* : Reason for flag, enclosed in single quotes, truncated to
   24 characters

The flag rows are sorted by start time. The flags are propagated into
the FL table. Visibility data are not altered. Special VLBA reason codes
recognized by ``difx2fits`` are as follows: :math:`'recorder'`, the flag
entry will be ignored and is not propagated into FITS, and
:math:`'observing system idle'`, the value of :math:`MJD_2` is ignored
and replaced by the ending MJD of the observation.

.. _sec:dotchannelflags:

.channelflags
-------------

User scripts or the program ``vex2difx`` may write a ``.channelflags``
file for each ``.input`` file. ``Difx2fits`` will propagate the user
flags of bad spectral channels into the FITS flag table. This file
contains two kinds of lines. Comment lines begin with an octothorpe (#)
and contain no vital information. Flag lines always consist of 7 fields:

#. *antId* : Station name abbreviation, e.g., ``LA``

#. *start* : Beginning of flagged period (Day of Year or Modified Julian
   Day; including fractional portion)

#. *end* : End of flagged period (Day of Year or Modified Julian Day;
   including fractional portion)

#. *freqIndex* : The DiFX frequency to flag, a zero-based index
   corresponding to a frequency in the ``FREQ TABLE`` of the
   corresponding ``.input`` file.

#. *startCh* : The first spectral channel to flag, a zero-based index.

#. *endCh* : The last spectral channel to flag, a zero-based index.

#. *reason* : Reason for flag, enclosed in single quotes, truncated to
   24 characters.

``Difx2fits`` translates the DiFX frequency :math:`freqIndex` to the
corresponding FITS IF, and flags the specified channels in *all
polarizations* of that IF. These flags are propagated into the FL table.
Visibility data are not altered.

.. _sec:flag:

flag
----

A file called ``flag`` is created when program ``vlog`` operates on the
``cal.vlba`` file. The file contains a list of antenna-based flags
generated by the on-line system that should be propagated into the FITS
FL table. The file is an experiment-wide flag file and effectively a
concatenation of ``.<antId>.flag`` files. This file contains two kinds
of lines. Comment lines begin with an octothorpe (#) and contain no
vital information. Flag lines always consist of exactly 5 fields:

#. *antId* : Station name abbreviation, e.g., ``LA``

#. *start* : Beginning of flagged period (Day of Year or Modified Julian
   Day; including fractional portion)

#. *end* : End of flagged period (Day of Year or Modified Julian Day;
   including fractional portion)

#. *recChan* : Record channel affected; -1 for all record channels,
   otherwise a zero-based index

#. *reason* : Reason for flag, enclosed in single quotes, truncated to
   24 characters

The flag rows are sorted first by antenna, and then start time. Special
VLBA reason codes recognized by ``difx2fits`` are as follows:
*’recorder’*, the flag entry will be ignored and is not propagated into
FITS, and *’observing system idle’*, the value of :math:`MJD_2` is
ignored and replaced by the ending MJD of the observation.

.. _sec:im:

.im
---

The ``.im`` file contains polynomial models used by ``difx2fits`` in the
creation of ``FITS`` files. After a header that is similar to that of a
``.rate`` file, the contents are organized hierarchically with scan
number, sub-scan interval, and antenna number being successively
faster-incrementing values. The keys and allowed values in this section
are summarized below: Note that the values of the delay polynomials in
this file have the opposite sign as compared to those generated by CALC
and those stored in ``.FITS`` files. Keys preceded by :math:`\star` are
optional. Note that all polynomials are expanded about their
``MJD, SEC`` start time and use seconds as the unit of time.

| l l l Key & Units/allowed values & Comments
| & string & name of the calc server computer used
| & integer & RPC program ID of the calc server used
| & integer & RPC version ID of the calc server used
| START YEAR & integer & calendar year of START MJD
| START MONTH & integer & calendar month of START MJD
| START DAY & integer & day of calendar month of START MJD
| START HOUR & integer & hour of START MJD
| START MINUTE & integer & minute of START MJD
| START SECOND & integer & second of START MJD
| POLYNOMIAL ORDER & 2, 3, 4 or 5 & polynomial order of interferometer
  model *order*
| INTERVAL (SECS) & integer & interval between new polynomial models
| ABERRATION CORR &
  :math:`\left\{\begin{array}{l}\mbox{\tt UNCORRECTED}\\\mbox{\tt APPROXIMATE}\\\mbox{\tt EXACT}\end{array}\right.`
  & level of :math:`u, v, w` aberration correction
| NUM TELESCOPES & integer :math:`\ge 1`\ & number of telescopes
  (*nTelescope*)
| && The row below is duplicated *nTelescope* times.
| TELESCOPE *t* NAME & string & upper case antenna name abbreviation
| NUM SCANS & integer :math:`\ge 1` & number of scans (*nScan*).
| && Everything below is duplicated *nScan* times.
| SCAN *s* POINTING SRC & string & name of source used as pointing
  center
| SCAN *s* NUM PHS CTRS :math:`\ge 1` & number of phase centers this
  scan (*nPC*\ :math:`_\mathit{s}`)
| && Everything below is duplicated *nPC*\ :math:`_\mathit{s}` times.
| SCAN *s* PHS CTR *p* SRC & string & name of source defining this phase
  center
| SCAN *s* NUM POLY & :math:`\ge 1` & number of polynomials covering
  scan (*nPoly*\ :math:`_\mathit{s,p}`)
| && Everything below is duplicated *nPoly*\ :math:`_\mathit{s,p}`
  times.
| SCAN *s* POLY *p* MJD & integer :math:`\ge 0` & the start MJD of this
  polynomial
| SCAN *s* POLY *p* SEC & integer :math:`\ge 0` & the start sec of this
  polynomial
| && Everything below is duplicated *nTelescope* times.
| ANT *a* DELAY (us) & *order*\ +1 numbers & terms of delay polynomial
| ANT *a* DRY (us) & *order*\ +1 numbers & terms of dry atmosphere
| ANT *a* WET (us) & *order*\ +1 numbers & terms of wet atmosphere
| *a* AZ & *order*\ +1 numbers & azimuth polynomial (deg)
| *a* EL GEOM & *order*\ +1 numbers & geometric (encoder) elevation
  (deg)
| *a* EL CORR & *order*\ +1 numbers & refraction corrected elevation
  (deg)
| *a* PAR ANGLE & *order*\ +1 numbers & parallactic angle (deg)
| ANT *a* U (m) & *order*\ +1 numbers & terms of baseline :math:`u`
| ANT *a* V (m) & *order*\ +1 numbers & terms of baseline :math:`v`
| ANT *a* W (m) & *order*\ +1 numbers & terms of baseline :math:`w`

.. _sec:input:

.input
------

This section describes the ``.input`` file format used by ``mpifxcorr``
to drive correlation. Because NRAO-DiFX 1.0 uses a non-standard branch
of ``mpifxcorr`` some of the data fields will differ from those used in
the official version, either in parameter name or in the available range
of values. Currently the parameters must be in the order listed here. To
get the most out of this section it is advisable to look at an actual
file while reading. An example file is stashed at
http://www.aoc.nrao.edu/~wbrisken/NRAO-DiFX-1.1/ . In the tables below,
numbers are assumed to floating point unless otherwise stated.

Note that the input file format has undergone a few minor changes since
NRAO-DiFX version 1.0.

Common settings table
~~~~~~~~~~~~~~~~~~~~~

Below are the keywords and allowed values for entries in the common
settings table. This table begins with header

-  ``# COMMON SETTINGS ##!``

This is always the first table in a ``.input`` file.

+--------------------+-----------------------+-----------------------+
| Key                | Units/allowed values  | Comments              |
+====================+=======================+=======================+
| CALC FILENAME      | string                | name and full path to |
|                    |                       | ``.calc`` file        |
+--------------------+-----------------------+-----------------------+
| CORE CONF FILENAME | string                | name and full path to |
|                    |                       | ``.threads`` file     |
+--------------------+-----------------------+-----------------------+
| EXECUTE TIME (SEC) | integer seconds       | observe time covered  |
|                    |                       | by this ``.input``    |
|                    |                       | file                  |
+--------------------+-----------------------+-----------------------+
| START MJD          | integer MJD           | start date            |
+--------------------+-----------------------+-----------------------+
| START SECONDS      | integer seconds       | start time            |
+--------------------+-----------------------+-----------------------+
| ACTIVE DATASTREAMS | integer :math:`\ge 2` | number of antennas    |
|                    |                       | (*nAntenna*)          |
+--------------------+-----------------------+-----------------------+
| ACTIVE BASELINES   | integer :math:`\ge 1` | number of baselines   |
|                    |                       | to correlate          |
|                    |                       | (*nBaseline*)         |
+--------------------+-----------------------+-----------------------+
| VIS BUFFER LENGTH  | integer :math:`\ge 1` | the number of         |
|                    |                       | concurrent            |
|                    |                       | integrations to allow |
+--------------------+-----------------------+-----------------------+
| OUTPUT FORMAT      | boolean               | always ``SWIN`` here  |
+--------------------+-----------------------+-----------------------+
| OUTPUT FILENAME    | string                | name of output        |
|                    |                       | ``.difx`` directory   |
+--------------------+-----------------------+-----------------------+

Typically,
:math:`\mathit{nBaseline} = \mathit{nAntenna} \cdot (\mathit{nAntenna}-1)/2`.
Autocorrelations are not included in this count.

Configurations table
~~~~~~~~~~~~~~~~~~~~

Below are the keywords and allowed values for entries in the
configurations table. This table begins with header

-  ``# CONFIGURATIONS ###!``

Two indexes are used for repeated keys. The index over datastream
(antenna) is *d*, running from 0 to *nAntenna* - 1 and the index over
baseline is *b*, running from 0 to *nBaseline* - 1.

+----------------------+----------------------+----------------------+
| Key                  | Units/allowed values | Comments             |
+======================+======================+======================+
| NUM CONFIGURATIONS   | integer              | number of modes in   |
|                      | :math:`\ge 1`        | file (*nConfig*)     |
+----------------------+----------------------+----------------------+
| CONFIG NAME          | string               | name of              |
|                      |                      | configuration        |
+----------------------+----------------------+----------------------+
| INT TIME (SEC)       | seconds              | integration time     |
+----------------------+----------------------+----------------------+
| SUBINT NANOSECONDS   | nanosec              | amount of time to    |
|                      |                      | process as one       |
|                      |                      | subintegration       |
+----------------------+----------------------+----------------------+
| GUARD NANOSECONDS    | nanosec              | amount of extra data |
|                      | :math:`\ge 0`        | to send for overlap  |
+----------------------+----------------------+----------------------+
| FRINGE ROTN ORDER    | int                  | 0 is post-FFT, 1 is  |
|                      |                      | delay/rate, …        |
+----------------------+----------------------+----------------------+
| ARRAY STRIDE LENGTH  | int                  | used for optimized   |
|                      |                      | fringe rotation      |
|                      |                      | calculations         |
+----------------------+----------------------+----------------------+
| XMAC STRIDE LENGTH   | int                  | number of channels   |
|                      |                      | to cross multiply in |
|                      |                      | one batch (must      |
|                      |                      | evenly divide into   |
|                      |                      | number of channels)  |
+----------------------+----------------------+----------------------+
| NUM BUFFERED FFTS    | int                  | number of FFTs to    |
|                      |                      | cross-multiply in    |
|                      |                      | one batch            |
+----------------------+----------------------+----------------------+
| WRITE AUTOCORRS      | boolean              | enable               |
|                      |                      | auto-correlations;   |
|                      |                      | *TRUE* here          |
+----------------------+----------------------+----------------------+
| PULSAR BINNING       | boolean              | enable pulsar mode   |
+----------------------+----------------------+----------------------+
| PULSAR CONFIG FILE   | string               | (*only if BINNING is |
|                      |                      | True*) see           |
|                      |                      | § `1.7               |
|                      |                      |  <#sec:binconfig>`__ |
+----------------------+----------------------+----------------------+
| PHASED ARRAY         | boolean              | set to FALSE         |
|                      |                      | (placeholder for     |
|                      |                      | now)                 |
+----------------------+----------------------+----------------------+
| DATASTREAM *d* INDEX | integer              | DATASTREAM table     |
|                      | :math:`\ge 0`        | index, starting at 0 |
+----------------------+----------------------+----------------------+
| BASELINE *b* INDEX   | integer              | BASELINE table       |
|                      | :math:`\ge 0`        | index, starting at 0 |
+----------------------+----------------------+----------------------+

.. _table:rule:

Rule table
~~~~~~~~~~

The rule tables describes which configuration will be applied at any
given time. Usually this filters on scan attributes such as source, but
can also be done in a time-based manner (start and stop times). An time
for which no configuration matches will not be correlated. If more than
one rule matches a given time, they must all refer to the same
configuration.

This table begins with header

-  ``# RULES ############!``

The table below uses *r* to represent the rule index, which ranges from
0 to *nRule* - 1.

+----------------------+----------------------+----------------------------------+
| Key                  | Units/allowed values | Comments                         |
+======================+======================+==================================+
| RULE *r* CONFIG NAME | string               | name to associate with this rule |
+----------------------+----------------------+----------------------------------+
|                      | string               | source to match                  |
+----------------------+----------------------+----------------------------------+
|                      | string               | scan name to match               |
+----------------------+----------------------+----------------------------------+
|                      | string               | cal code to match                |
+----------------------+----------------------+----------------------------------+
|                      | string               | source qualifier to match        |
+----------------------+----------------------+----------------------------------+
|                      | string               | earliest time to match           |
+----------------------+----------------------+----------------------------------+
|                      | string               | latest time to match             |
+----------------------+----------------------+----------------------------------+

.. _table:freq:

Frequency table
~~~~~~~~~~~~~~~

Below are the keywords and allowed values for entries in the frequency
table which defines all possible sub-bands used by the configurations in
this file. Each sub-band of each configuration is mapped to one of these
through a value in the datastream table
(§\ `1.26.6 <#table:datastream>`__). Each entry in this table has three
parameters which are replicated for each frequency table entry. This
table begins with header

-  ``# FREQ TABLE #######!``

The table below uses *f* to represent the frequency index, which ranges
from 0 to *nFreq* - 1 and *t* to represent pulse cal tone index, which
ranges from 0 to *nTone*\ :math:`_f`.

+----------------------+----------------------+----------------------+
| Key                  | Units/allowed values | Comments             |
+======================+======================+======================+
| FREQ ENTRIES         | integer              | number of frequency  |
|                      | :math:`\ge 1`        | setups (*nFreq*)     |
+----------------------+----------------------+----------------------+
| FREQ (MHZ) *f*       | MHz                  | sky frequency at     |
|                      |                      | band edge            |
+----------------------+----------------------+----------------------+
| BW (MHZ) *f*         | MHz                  | bandwidth of         |
|                      |                      | sub-band             |
+----------------------+----------------------+----------------------+
| SIDEBAND *f*         | ``U`` or ``L``       | net sideband of      |
|                      |                      | sub-band             |
+----------------------+----------------------+----------------------+
| NUM CHANNELS *f*     | integer              | initial number of    |
|                      | :math:`\ge 1`        | channels (FFT size,  |
|                      |                      | *nFFT*, is twice     |
|                      |                      | this)                |
+----------------------+----------------------+----------------------+
| CHANS TO AVG *f*     | integer              | average this many    |
|                      | :math:`\ge 1`        | channels before      |
|                      |                      | generating output    |
|                      |                      | spectra)             |
+----------------------+----------------------+----------------------+
| OVERSAMPLE FAC. *f*  | integer              | total oversampling   |
|                      | :math:`\ge 1`        | factor of baseband   |
|                      |                      | data                 |
+----------------------+----------------------+----------------------+
| DECIMATION FAC. *f*  | integer              | portion of           |
|                      | :math:`\ge 1`        | oversampling to      |
|                      |                      | handle by decimation |
+----------------------+----------------------+----------------------+
| PHASE CALS *f* OUT   | integer              | number of phase cals |
|                      | :math:`\ge 0`        | to produce           |
|                      |                      | (                    |
|                      |                      | *nTone*\ :math:`_f`) |
+----------------------+----------------------+----------------------+
|                      |                      | The row below is     |
|                      |                      | duplicated           |
|                      |                      | *nTone*\ :math:`_f`  |
|                      |                      | times.               |
+----------------------+----------------------+----------------------+
| PHASE CAL *f*/*t*    | integer              | tone number of band  |
| INDEX                |                      |                      |
+----------------------+----------------------+----------------------+

Telescope table
~~~~~~~~~~~~~~~

Below are the keywords and allowed values for entries in the telescope
table which tabulates antenna names and their associated peculiar clock
offsets, and the time derivatives of these offsets. Much of the other
antenna-specific information is stored in the datastream table
(§\ `1.26.6 <#table:datastream>`__). Each datastream of each
configuration is mapped to one of these through a value in the
datastream table. Each entry in this table has three parameters which
are replicated for each telescope table entry. This table begins with
header

-  ``# TELESCOPE TABLE ##!``

The table below uses *a* to represent the antenna index, which ranges
from 0 to *nAntenna* - 1 and *c* to represent clock coefficient, ranging
from 0 to *nCoeff*\ :math:`_a`.

+----------------------+----------------------+----------------------+
| Key                  | Units/allowed values | Comments             |
+======================+======================+======================+
| TELESCOPE ENTRIES    | integer              | number of antennas   |
|                      | :math:`\ge 1`        | (*nAntenna*)         |
+----------------------+----------------------+----------------------+
| TELESCOPE NAME *a*   | string               | abbreviation of      |
|                      |                      | antenna name         |
+----------------------+----------------------+----------------------+
| CLOCK REF MJD *a*    | double               | date around which    |
|                      |                      | the following        |
|                      |                      | polynomial is        |
|                      |                      | expanded             |
+----------------------+----------------------+----------------------+
| CLOCK POLY ORDER *a* | int :math:`\ge 0`    | polynomial order of  |
|                      |                      | telescope clock      |
|                      |                      | model                |
|                      |                      | (                    |
|                      |                      | *nCoeff*\ :math:`_a` |
+----------------------+----------------------+----------------------+
| CLOCK COEFF *a*/*c*  | :math:`\mu`\         | clock model          |
|                      |  sec/sec\ :math:`^c` | polynomial           |
|                      |                      | coefficient          |
+----------------------+----------------------+----------------------+

.. _table:datastream:

Datastream table
~~~~~~~~~~~~~~~~

The datastream table begins with header

-  ``# DATASTREAM TABLE #!``

The table below uses *f* to represent recorded frequencies, which ranges
from 0 to *nFreq* - 1. A second index, *z*, is used to iterate over zoom
bands, ranging from 0 to *nFreq* - 1. A third index, *i*, is used to
cover the range 0 to :math:`\mathit{nBB}` - 1, where the total number of
basebands is given by
:math:`\mathit{nBB} \equiv \sum_f \mathit{nPol}_f`. In the DiFX system,
all sub-bands must have the same polarization structure, so
:math:`\mathit{nBB} = \mathit{nFreq} \cdot \mathit{nPol}`. This index is
reused for the zoom bands in an analogous manner.

+----------------------+----------------------+----------------------+
| Key                  | Units/allowed values | Comments             |
+======================+======================+======================+
| DATASTREAM ENTRIES   | integer              | number of antennas   |
|                      | :math:`\ge 1`        | (*nDatastream*)      |
+----------------------+----------------------+----------------------+
| DATA BUFFER FACTOR   | integer              |                      |
|                      | :math:`\ge 1`        |                      |
+----------------------+----------------------+----------------------+
| NUM DATA SEGMENTS    | integer              |                      |
|                      | :math:`\ge 1`        |                      |
+----------------------+----------------------+----------------------+
| TELESCOPE INDEX      | integer              | telescope table      |
|                      | :math:`\ge 0`        | index of datastream  |
+----------------------+----------------------+----------------------+
| TSYS                 | Kelvin               | if zero (normal in   |
|                      |                      | NRAO usage), don’t   |
|                      |                      | scale data by *tsys* |
+----------------------+----------------------+----------------------+
| DATA FORMAT          | string               | data format          |
+----------------------+----------------------+----------------------+
| QUANTISATION BITS    | integer              | bits per sample      |
|                      | :math:`\ge 1`        |                      |
+----------------------+----------------------+----------------------+
| DATA FRAME SIZE      | integer              | bytes in one         |
|                      | :math:`\ge 1`        | frame(or file) of    |
|                      |                      | data                 |
+----------------------+----------------------+----------------------+
| DATA SAMPLING        | string               | ``REAL`` or          |
|                      |                      | ``COMPLEX``          |
+----------------------+----------------------+----------------------+
| DATA SOURCE          | string               | ``FILE`` (see        |
|                      |                      | §                    |
|                      |                      | \ `[sec:datafiles] < |
|                      |                      | #sec:datafiles>`__), |
|                      |                      | ``MODULE`` for Mark5 |
|                      |                      | playback, or         |
|                      |                      | ``FAKE`` for         |
|                      |                      | benchmarking mode    |
+----------------------+----------------------+----------------------+
| FILTERBANK USED      | boolean              | currently only       |
|                      |                      | ``FALSE``            |
+----------------------+----------------------+----------------------+
| PHASE CAL INT (MHZ)  | int                  | pulse cal comb       |
|                      |                      | frequency spacing,   |
|                      |                      | or 0 if no pulse cal |
|                      |                      | tones                |
+----------------------+----------------------+----------------------+
| NUM RECORDED FREQS   | integer              | number of different  |
|                      | :math:`\ge 0`        | frequencies recorded |
|                      |                      | for this datastream  |
+----------------------+----------------------+----------------------+
| REC FREQ INDEX *f*   | integer              | index to frequency   |
|                      | :math:`\ge 0`        | table                |
+----------------------+----------------------+----------------------+
| CLK OFFSET *f* (us)  | :math:`\mu`\ sec     | frequency-dependent  |
|                      |                      | clock offset         |
+----------------------+----------------------+----------------------+
| FREQ OFFSET *f* (us) | :math:`\mu`\ sec     | frequency-dependent  |
|                      |                      | LO offset            |
+----------------------+----------------------+----------------------+
| NUM REC POLS *f*     | 1 or 2               | for this recorded    |
|                      |                      | frequency, the       |
|                      |                      | number of            |
|                      |                      | polarizations        |
+----------------------+----------------------+----------------------+
| REC BAND *i* POL     | *R* or *L*           | polarization         |
|                      |                      | identity             |
+----------------------+----------------------+----------------------+
| REC BAND *i* INDEX   | integer              | index to frequency   |
|                      | :math:`\ge 1`        | setting array above; |
|                      |                      | *nBB* per entry      |
+----------------------+----------------------+----------------------+
| NUM ZOOM FREQS       | integer              | number of different  |
|                      | :math:`\ge 0`        | zoom bands set for   |
|                      |                      | this datastream      |
+----------------------+----------------------+----------------------+
| ZOOM FREQ INDEX *z*  | integer              | index to frequency   |
|                      | :math:`\ge 0`        | table                |
+----------------------+----------------------+----------------------+
| NUM ZOOM POLS *z*    | 1 or 2               | for this recorded    |
|                      |                      | frequency, the       |
|                      |                      | number of            |
|                      |                      | polarizations        |
+----------------------+----------------------+----------------------+
| ZOOM BAND *i* POL    | *R* or *L*           | polarization         |
|                      |                      | identity             |
+----------------------+----------------------+----------------------+
| ZOOM BAND *i* INDEX  | integer              | index to frequency   |
|                      | :math:`\ge 1`        | setting array above; |
|                      |                      | *nBB* per entry      |
+----------------------+----------------------+----------------------+

Baseline table
~~~~~~~~~~~~~~

In order to retain the highest level of configurability, each baseline
can be independently configured at some level. This datastream table
begins with header

-  ``# BASELINE TABLE ###!``

The baseline table has multiple entries, each one corresponding to a
pair of antennas, labeled ``A`` and ``B`` in the table. For each of
*nBaseline* baseline entries, *nFreq* sub-bands are processed, and for
each a total of *nProd* polarization products are formed. Indexes for
each of these dimensions are *b*, *f* and *p* respectively, each
starting count at 0. Within the DiFX context, all baselines must have
the same *nFreq* and *nProd*, though this is not a requirement of
``mpifxcorr`` in general. Each of the *nFreq* sub-bands specifies a pair
of (possibly identical) datastream table bands to correlate. The global
frequency table index of that product is by default identical to some
index that one band out of the datastream table band pair ultimately
refers to. Under DiFX 2.7 that target frequency can be specified
explicitly and is interpreted as a spectral placement directive – which
target frequency the sub-band shall contribute data to. One or more
sub-bands can refer to the same target frequency index.

+----------------------+----------------------+----------------------+
| Key                  | Units/allowed values | Comments             |
+======================+======================+======================+
| BASELINE ENTRIES     | integer              | number of entries in |
|                      | :math:`\ge 1`        | table, *nBaseline*   |
+----------------------+----------------------+----------------------+
| D/STREAM A INDEX *b* | integer              | datastream table     |
|                      | :math:`\ge 0`        | index of first       |
|                      |                      | antenna              |
+----------------------+----------------------+----------------------+
| D/STREAM B INDEX *b* | integer              | datastream table     |
|                      | :math:`\ge 0`        | index of second      |
|                      |                      | antenna              |
+----------------------+----------------------+----------------------+
| NUM FREQS *b*        | integer              | number of            |
|                      | :math:`\ge 1`        | frequencies on this  |
|                      |                      | baseline,            |
|                      |                      | *nFreq\              |
|                      |                      | :math:`_\mathit{b}`* |
+----------------------+----------------------+----------------------+
| (TARGET FREQ *b*     | integer              | index to frequency   |
|                      | :math:`\ge 0`        | table)               |
+----------------------+----------------------+----------------------+
| POL PRODUCTS *b*/*f* | integer              | number of            |
|                      | :math:`\ge 1`        | polarization         |
|                      |                      | products,            |
|                      |                      | *nProd\              |
|                      |                      | :math:`_\mathit{b}`* |
+----------------------+----------------------+----------------------+
| D/STREAM A BAND *p*  | integer              | index to frequency   |
|                      | :math:`\ge 0`        | array in datastream  |
|                      |                      | table                |
+----------------------+----------------------+----------------------+
| D/STREAM B BAND *p*  | integer              | same as abovem, but  |
|                      | :math:`\ge 0`        | for antenna ``B``,   |
|                      |                      | not ``A``            |
+----------------------+----------------------+----------------------+

Data Table
~~~~~~~~~~

In the following table, *d* is the datastream index, ranging from 0 to
*nDatastream* - 1 and *f* is the file index ranging from 0 to
*nFile\ :math:`_\mathit{d}`*.

+--------------------+-----------------------+-----------------------+
| Key                | Units/allowed values  | Comments              |
+====================+=======================+=======================+
| D/STREAM *d* FILES | integer :math:`\ge 1` | number of files       |
|                    |                       | *nFile\               |
|                    |                       |  :math:`_\mathit{d}`* |
|                    |                       | associated with       |
|                    |                       | datastream *d*        |
+--------------------+-----------------------+-----------------------+
| FILE *d*/*f*       | string                | name of file or       |
|                    |                       | module associated     |
|                    |                       | with datastream *d*   |
+--------------------+-----------------------+-----------------------+

For datastreams reading off Mark5 modules, *nFile* will always be 1 and
the filename is the *VSN* of the module being read.

.. _sec:joblistfile:

.joblist
--------

A single ``.joblist`` file is written by ``vex2difx``
(§\ `[sec:vex2difx] <#sec:vex2difx>`__) as it produces the DiFX
``.input`` (and other) files for a given correlator pass. This file
contains the list of jobs to run and some versioning information that
allows improved accountability of the software versions being used. This
file us used by ``difxqueue`` and ``makefits`` to ensure that a complete
set of jobs is accounted for. The file is composed of two parts: a
header line and one line for each job. The header line consists of a
series of *key=value* pairs. Each *key* and *value* must have no
whitespace and no whitespace should separate these words from their
connecting ``=`` sign. While any number of key-value pairs may be
specified, the following ones (which are case sensitive) are expected to
be present:

#. ``exper`` : the name of the experiment, including the segment code

#. ``v2d`` : the ``vex2difx`` input file used to produce the jobs of
   this pass

#. ``pass`` : the name of the correlator pass

#. ``mjd`` : the modified Julian day when ``vex2difx`` created this file

#. ``DiFX`` : the version name for the DiFX deployment (the value of
   ``$DIFX_VERSION`` when ``vex2difx`` was run)

#. ``vex2difx`` : the version of ``vex2difx`` that was run

Each additional line contains information for one job in the pass. The
columns are:

#. *jobName* : the name/prefix of the job

#. *mjdStart* : the observe start time of the job

#. *mjdStop* : the observe stop time of the job

#. *nAnt* : the number of antennas in the job

#. *maxPulsarBin* : the maximum number of pulsar bins to come from any
   scan in this job (usually zero)

#. *nPhaseCenters* : the maximum number of phase centers to come from
   any scan in this job (usually one)

#. *tOps* : estimated number of trillion floating point operations
   required to run the job

#. *outSize* : estimated FITS file output size (MB)

Usually the comment character ``#`` followed by a list of station codes
is appended to the end of each line.

.. _sec:jobmatrixfile:

.jobmatrix
----------

As of version 2.6 of ``difx2fits`` a file with extension ``.jobmatrix``
will be written for each ``.FITS`` file created. This file is meant as a
summary for human use and as such does not have a format that should be
considered fixed. The file contains a 2-dimensional map (antenna number
vs. time) of which jobs contributed to the ``.FITS`` file.

.. _sec:lag:

.lag
----

Program ``zerocorr`` (§\ `[sec:zerocorr] <#sec:zerocorr>`__ produces lag
output in a format documented here. There is one line of output per lag.
Each line has 7 columns as per the following table:

==== ==================================================
Line Contents
==== ==================================================
1    Channel (spectral point) number (counts from zero)
2    Time lag (sec)
3    Real value of the lag function
4    Imaginary value of the lag function
5    Amplitude
6    Phase
7    Window function (weight at this lag)
==== ==================================================

.. _sec:log:

.log
----

When generation of sniffer output files is not disabled, each ``.FITS``
file written by ``difx2fits`` will be accompanied by a corresponding
``.log`` file. This file contains a summary of the contents of that
``.FITS`` file. It is analogous to the ``logfile.lis`` file produced by
the old ``FITSsniffer`` program. This file is free-form ASCII that is
intended for viewing by human eyes, and is should not be used as input
to any software as the format is not guaranteed to remain constant.

.. _sec:machines:

.machines
---------

The ``.machines`` file is used by ``mpirun`` to determine which machines
will run ``mpifxcorr``. This is a text file containing a list of
computers, one to a line possibly with additional options listed, on
which to spawn the software correlator process. As a general rule the
MPI rank, a unique number for each process that starts at 0, are
allocated in the order that the computer names are listed. This general
rule can break down in cases where the same computer name is listed more
than once; the behavior in this case depends on the MPI implementation
being used. MPI rank 0 will always be the manager process. Ranks 1
through *nDatastream* will each be a datastream process. Additional
processes will be computing (core) processes. If more processes are
specified for ``mpirun`` with the ``-np`` option than there are lines in
this file, the file will be read again from the top, so the processes
will be assigned in a cyclic fashion (again, this depends somewhat on
the MPI implementation and the other parameters passed to ``mpirun``;
for DiFX with OpenMPI, this assumes ``–bynode`` is used). If the program
``startdifx`` is used to start the correlation process, the number of
processes to start is determined by the number of lines in this file. If
wrapping to the top of this file is desired, dummy comment lines
(beginning with #) can be put at the end of the ``.machines`` file to
artificially raise the number of processes to spawn. Within DiFX, this
file is typically produced by ``genmachines``. Keep in mind that this
file is directly read by the MPI execution program ``mpirun`` and the
format of the file may differ depending on the MPI implementation that
you are using. With OpenMPI appending ``slots=1 max-slots=1`` to the end
of each line ensures that a single instance of ``mpifxcorr`` is run on
that machine. If both a datastream process and a core process are to be
run on the same computer, then using options ``slots=1 max-slots=2``
might be appropriate.

.. _sec:mark4list:

.mark4list
----------

A ``.mark4list`` file is written by ``makemark4`` and contains the
entire list of ``gzip`` compressed file sets (ending in
``.mark4.tar.gz``) for the correlator pass. Due to the different
constraints of the correlation process and the Mark4 format, the number
of resultant compressed file sets may be greater or less than the number
of jobs. This file type is used by ``difxarch`` to ensure that all of
the correlated output ends up in the archive. The file is composed of
two parts: a header line and one line for each compressed file set. The
header line consists of a series of *key=value* pairs. Each *key* and
*value* must have no whitespace and no whitespace should separate these
words from their connecting ``=`` sign. While any number of key-value
pairs may be specified, the following ones (which are case sensitive)
are expected to be present:

#. ``exper`` : the name of the experiment, including the segment code

#. ``pass`` : the name of the correlator pass

#. ``jobs`` : the name of the ``.joblist`` file used by ``makemark4``

#. ``mjd`` : the modified Julian day when ``makemark4`` created this
   file

#. ``DiFX`` : the version name for the DiFX deployment (the value of
   ``$DIFX_VERSION`` when ``vex2difx`` was run)

#. ``difx2mark4`` : the version of ``difx2mark4`` that was run

Each additional line contains information for one compressed file set of
the correlation pass. These lines contain three fields:

#. *archiveName* : the name of the file that will get injected into the
   archive (see §\ `[sec:archive] <#sec:archive>`__)

#. *fileSize* : the size of the file in MB

#. *origName* : the name of the file as produced by ``difx2mark4`` (via
   ``makemark4``)

.. _sec:oms:

.oms
----

A ``.oms`` file is written by ``sched`` and contains machine (and human)
readable information that is useful in setting up correlator jobs. In
the case of the VLBA DiFX correlator, program ``oms2v2d``
(§\ `[sec:oms2v2d] <#sec:oms2v2d>`__) uses this file to prepare a
template ``.v2d`` file (§\ `1.42 <#sec:v2d>`__) that contains some
information not available in the vex file, such as intended integration
time and number of channels.

.. _sec:params:

.params
-------

A file with extension ``.params`` is written by ``vex2difx``
(§\ `[sec:vex2difx] <#sec:vex2difx>`__) when it is provided with the
``-o`` option. This file is a duplicate of the ``.v2d`` file that was
supplied but with all unspecified parameters listed with the defaults
that they assumed. The format is exactly the same as the ``.v2d`` files;
see §\ `1.42 <#sec:v2d>`__ for documentation of the format. The
``.params`` file can be used as a legal ``.v2d`` file if necessary.

.. _sec:pcal:

pcal
----

A file called ``pcal`` is created when program ``vlog`` operates on the
``cal.vlba`` file. This file contains three measurements: the cable
length calibration, pulse calibration, and state counts. This file
contains two kinds of lines. Comment lines begin with an octothorpe (#)
and contain no vital information. Data lines always begin with 9 fields
describing the content of that data line:

#. *antId* : Station name abbreviation, e.g., ``LA``

#. *day* : Time centroid of measurement (MJD or day of year, including
   fractional portion)

#. *dur* : Duration of measurement (days)

#. *cableCal* : Cable calibration measurement (picoseconds)

#. *nPol* : Number of polarizations with measurements

#. *nBand* : Number of sub-bands with measurements

#. *nTone* : Number of pulse cal tones detected per band per
   polarization, possibly zero

#. *nState* : Number of state count states measured per band per
   polarization, possibly zero

#. *nRecChan* : Number of record channels at time of measurement
   (:math:`\le` *nPol \* nBand)*

Following these nine fields are two variable-length arrays of numbers.
The first variable-length field is the pulse cal data field consisting
of *nPol*nBand*nTone* groups of four numbers. The first member of this
group is the recorder channel number (zero-based) corresponding to the
measurement. The second member of this group is the tone sky frequency
(MHz). The third and fourth are respectively the real and imaginary
parts of the tone measured at the given sky frequency. The order in
which the groups are presented (in ‘C’ language array syntax, as used
throughout this document) is
:math:`[`\ *nPol*\ :math:`][`\ *nBand*\ :math:`][`\ *nTone*\ :math:`]`.
Note that if there are fewer than *nPol*nBand* record channels, the
record channel will be :math:`-1` for some groups. The second
variable-length field is the state count data. For each band of each
polarization, *nState* + 1 values are listed. The first number is the
record channel number or -1 if that polarization/band combination was
not observed or monitored. The remainder contain state counts. *nState*
can be either 0 or :math:`2^{\mathit nBit}`, where *nBit* is the number
of quantization bits. The order in which these groups are listed is
:math:`[`\ *nPol*\ :math:`][`\ *nBand*\ :math:`]`.

.. _sec:polycofile:

.polyco
-------

A polyco file contains a single polynomial for pulse phase that is valid
for a fraction (up to 100%) of a job file. An additional numeric suffix
is appended to the filename specifying the polynomial index for a
particular ``.pulsar`` file that shares the same base name. The format
of the file is the same as a Tempo pulsar file
:raw-latex:`\cite{tempo}`.

.. _sec:shelf:

.shelf
------

The vex file format (see §\ `1.44 <#sec:dotvex>`__ and references
within) does not have a formal slot to record the shelf location of
media, so ``db2vex`` stashes the shelf location in a separate file. This
information is critical for the correlator operators to know where to
find modules for a project and for analysts preparing correlator jobs to
know if media have arrived. The ``.shelf`` file is used by ``vex2difx``
when writing ``.calc files``. It can also be used as input to
``getshelf``. The file format is very simple. One row is used for each
module that was used in the observation. Typically rows are sorted in
the same order as antennas in the ``.input`` file. The comment character
is ``#`` – any text following this character on a line is ignored. Each
line contains 3 white-space separated columns:

#. *antId* : The typically 2 letter station abbreviation

#. *vsn* : The volume serial number of the media (e.g., ``NRAO-123``)

#. *shelf* : The shelf location, which can be any string without
   whitespace (e.g., ``BD89``), or ``none`` if the media is not at the
   correlator

.. _sec:threads:

.threads
--------

The ``.threads`` file tells ``mpifxcorr`` how many threads to start on
each processing node. Within DiFX, this file is typically produced by
``genmachines``. The ``.threads`` file has a very simple format. The
first line starts with ``NUMBER OF CORES:``. Starting at column 21 is an
integer that should be equal to the number of processing nodes (*nCore*)
specified in the corresponding ``.machines`` file. Each line thereafter
should contain a single integer starting at column 1. There should be
*nCore* such lines.

.. _sec:tsys:

tsys
----

A file called ``tsys`` is created when program ``vlog`` operates on the
``cal.vlba`` file. This text file contains measurements of the system
temperature and name of receiver for each baseband channel. This file
contains two kinds of lines. Comment lines begin with an octothorpe (#)
and contain no vital information. Data lines always contain 4 fixed-size
fields:

#. *antId* : Station name abbreviation, e.g., ``LA``

#. *day* : Time centroid of measurement (day of year or mjd, including
   fractional portion)

#. *dur* : Duration of measurement (days) or zero if not known

#. *nRecChan* : Number of baseband channels recorded

Following these 4 fields are *nRecChan* pairs of values, one for each
baseband channel. The first element of each pair is the system
temperature (in K) and the second is the receiver name (e.g., ``4cm``,
or ``7mm``).

This format should not be confused with switched power files produced by
``mpifxcorr`` (see §\ `1.12.3 <#sec:difxswitchedpower>`__).

.. _sec:weather:

weather
-------

A file called ``weather`` is created when program ``vlog`` operates on
the ``cal.vlba`` file. This file contains tabulated values of various
meteorological measurements. This file contains two finds of lines.
Comment lines begin with an octothorpe (#) and contain no vital
information. Data lines always contain 9 fixed-size fields:

#. *antId* : Station name abbreviation, e.g., ``LA``

#. *day* : Time of measurement (MJD or day of year, including fractional
   portion)

#. *T* : Ambient temperature (Centigrade)

#. *P* : Pressure (mbar)

#. *dewPoint* : Dew point (Centigrade)

#. *windSpeed* : Wind speed (m/s)

#. *windDir* : Wind direction (degrees E of N)

#. *precip* : Accumulated rain since UT midnight (cm)

#. *windGust* : Maximum wind gust over collection period (m/s)

.. _sec:wts:

.wts
----

When generation of sniffer output files is not disabled, each ``.FITS``
file written by ``difx2fits`` will be accompanied by a corresponding
``.wts`` file. This file contains statistics of the data weights,
typically dominated by the completeness of records as determined by the
data transport system, over a typically 30 second long period.

The first line is simply the observe code, e.g., ``MT831`` .

Each additional line in the file is a complete record for a given
antenna for a given interval, containing information for each baseband
channel separately. The format of these lines is as follows:

+----------------------+----------------------+----------------------+
| Key                  | Units/allowed values | Comments             |
+======================+======================+======================+
| *MJD*                | integer              | MJD day number       |
|                      | :math:`\ge 1`        | corresponding to     |
|                      |                      | line                 |
+----------------------+----------------------+----------------------+
| *hour*               | :math:`\ge 0.0`,     | hour within day      |
|                      | :math:`< 24.0`       |                      |
+----------------------+----------------------+----------------------+
| *antenna number*     | :math:`\ge 1`        | antenna table index  |
+----------------------+----------------------+----------------------+
| *antenna name*       | string               |                      |
+----------------------+----------------------+----------------------+
| *n*\ :ma             | :math:`\ge 1`        | Number of baseband   |
| th:`_{\mathrm{BBC}}` |                      | channels             |
+----------------------+----------------------+----------------------+
| *mean weight*        | :math:`\ge 0.0`      | This column repeated |
|                      |                      | :mat                 |
|                      |                      | h:`n_{\mathrm{BBC}}` |
|                      |                      | times                |
+----------------------+----------------------+----------------------+
| *min weight*         | :math:`\ge 0.0`      | This column repeated |
|                      |                      | :mat                 |
|                      |                      | h:`n_{\mathrm{BBC}}` |
|                      |                      | times                |
+----------------------+----------------------+----------------------+
| *max weight*         | :math:`\ge 0.0`      | This column repeated |
|                      |                      | :mat                 |
|                      |                      | h:`n_{\mathrm{BBC}}` |
|                      |                      | times                |
+----------------------+----------------------+----------------------+

This file can be used directly with plotting program ``plotwt`` or used
more automatically with ``difxsniff``.

.. _sec:v2d:

.v2d
----

The ``.v2d`` file is used to specify correlation options to ``vex2difx``
and adjust the way in which it forms DiFX input files based on the
``.vex`` file. The ``.v2d`` file consists of a number of global
parameters that affect the way that jobs are created and several
sections that can customize correlation on a per-source, per mode, or
per scan basis. All parameters (those that are global and those that
reside inside sections) are specified by a parameter name, the equal
sign, and one value, or a comma-separated list of values, that cannot
contain whitespace. Whitespace is not required except to keep parameter
names, values, and section names separate. All parameter names and
values are case sensitive except for source names and antenna names. The
# is a comment character; any text after this on a line is ignored.

Most parameters are one of the following types:

-  **bool** : A boolean value that can be True or False. Any value
   starting with ``0``, ``f``, ``F``, or ``-`` will be considered False
   and otherwise True.

-  **float** : A floating point number. Can be of the forms: ``1.23``,
   ``1.2e-4``, ``-12.6``, ``4``

-  **int** : An integer.

-  **string** : Any sequence of printable(non-whitespace) characters.
   Certain fields require strings of a maximum length or certain form.

-  **date** : A date field; see below.

-  **array** : Array can be of any of the four above types and are
   indicated by enclosing brackets, e.g., :math:`[`\ int\ :math:`]`. The
   empty list is indicated with :math:`[ ]` which is usually implied to
   be all-inclusive.

All times used in ``vex2difx`` are in Universal Time and are internally
represented as a double precision value. The integer part of this value
is the date corresponding to 0\ :math:`^h` UT. The fractional part, when
multiplied by 86400, gives the number of seconds since 0\ :math:`^h` UT.
Note that this format does not allow one to specify the actual leap
second if one occurs on that day. Several date formats are supported:

-  **Modified Julian Day** : A decimal MJD possibly including fractional
   day. E.g.: ``54345.341944``

-  **Vex time format** : A string of the form: ``2009y245d08h12m24s``

-  **VLBA-like format** : A string of the form: ``2009SEP02-08:12:24``

-  **ISO 8601 format** : A string of the form: ``2009-09-02T08:12:24``

Global parameters can be specified one or many per line such as:

``maxGap = 2000 # seconds``

or

``mjdStart = 52342.522 mjdStop=52342.532``

The following parameter names are recognized:

+--------------+--------------+-------+--------------+--------------+
| Name         | Type         | Units | Defaults     | Comments     |
+==============+==============+=======+==============+==============+
| vex          | string       |       |              | filename of  |
|              |              |       |              | the vex file |
|              |              |       |              | to process;  |
|              |              |       |              | **this is    |
|              |              |       |              | required**   |
+--------------+--------------+-------+--------------+--------------+
| mjdStart     | date         |       | obs. start   | discard any  |
|              |              |       |              | scans or     |
|              |              |       |              | partial      |
|              |              |       |              | scans before |
|              |              |       |              | this time    |
+--------------+--------------+-------+--------------+--------------+
| mjdStop      | date         |       | obs. stop    | discard any  |
|              |              |       |              | scans or     |
|              |              |       |              | partial      |
|              |              |       |              | scans after  |
|              |              |       |              | this time    |
+--------------+--------------+-------+--------------+--------------+
| break        | date         |       |              | list of MJD  |
|              |              |       |              | date/times   |
|              |              |       |              | where jobs   |
|              |              |       |              | are forced   |
|              |              |       |              | to be broken |
+--------------+--------------+-------+--------------+--------------+
| minSubarray  | int          |       | 2            | don’t make   |
|              |              |       |              | jobs for     |
|              |              |       |              | subarrays    |
|              |              |       |              | with fewer   |
|              |              |       |              | antennas     |
|              |              |       |              | than this    |
+--------------+--------------+-------+--------------+--------------+
| maxGap       | float        | sec   | 180          | split an     |
|              |              |       |              | observation  |
|              |              |       |              | into         |
|              |              |       |              | multiple     |
|              |              |       |              | jobs if      |
|              |              |       |              | there are    |
+--------------+--------------+-------+--------------+--------------+
|              |              |       |              | correlation  |
|              |              |       |              | gaps longer  |
|              |              |       |              | than this    |
|              |              |       |              | number       |
+--------------+--------------+-------+--------------+--------------+
| tweakIntTime | bool         |       | False        | adjust (up   |
|              |              |       |              | to 40%)      |
|              |              |       |              | int. time to |
|              |              |       |              | ensure       |
|              |              |       |              | int. blocks  |
|              |              |       |              | per send     |
+--------------+--------------+-------+--------------+--------------+
| singleScan   | bool         |       | False        | if True,     |
|              |              |       |              | split each   |
|              |              |       |              | scan into    |
|              |              |       |              | its own job  |
+--------------+--------------+-------+--------------+--------------+
| singleSetup  | bool         |       | True         | if True,     |
|              |              |       |              | allow only   |
|              |              |       |              | one setup    |
|              |              |       |              | per job;     |
|              |              |       |              | True is      |
|              |              |       |              | required     |
+--------------+--------------+-------+--------------+--------------+
|              |              |       |              | for FITS-IDI |
|              |              |       |              | conversion   |
+--------------+--------------+-------+--------------+--------------+
| maxLength    | float        | sec   | 7200         | don’t allow  |
|              |              |       |              | individual   |
|              |              |       |              | jobs longer  |
|              |              |       |              | than this    |
|              |              |       |              | amount of    |
|              |              |       |              | time         |
+--------------+--------------+-------+--------------+--------------+
| minLength    | float        | sec   | 2            | don’t allow  |
|              |              |       |              | individual   |
|              |              |       |              | jobs shorter |
|              |              |       |              | than this    |
|              |              |       |              | amount of    |
|              |              |       |              | time         |
+--------------+--------------+-------+--------------+--------------+
| maxSize      | float        | MB    | 2000         | max FITS-IDI |
|              |              |       |              | file size to |
|              |              |       |              | allow        |
+--------------+--------------+-------+--------------+--------------+
| data         | int          |       | 32           | the          |
| BufferFactor |              |       |              | `            |
|              |              |       |              | `mpifxcorr`` |
|              |              |       |              | DATA         |
|              |              |       |              | BUFFERFACTOR |
|              |              |       |              | parameter    |
+--------------+--------------+-------+--------------+--------------+
| n            | int          |       | 8            | the          |
| DataSegments |              |       |              | `            |
|              |              |       |              | `mpifxcorr`` |
|              |              |       |              | NUM          |
|              |              |       |              | DATASEGMENTS |
|              |              |       |              | parameter    |
+--------------+--------------+-------+--------------+--------------+
| jobSeries    | string       |       |              | the base     |
|              |              |       |              | filename of  |
|              |              |       |              | ``.input``   |
|              |              |       |              | and          |
|              |              |       |              | ``.calc``    |
|              |              |       |              | files to be  |
+--------------+--------------+-------+--------------+--------------+
|              |              |       |              | created;     |
|              |              |       |              | defaults to  |
|              |              |       |              | the base     |
|              |              |       |              | name of the  |
|              |              |       |              | ``.v2d``     |
|              |              |       |              | file         |
+--------------+--------------+-------+--------------+--------------+
| startSeries  | int          |       | 20           | the default  |
|              |              |       |              | starting     |
|              |              |       |              | number for   |
|              |              |       |              | jobs created |
+--------------+--------------+-------+--------------+--------------+
| sendLength   | float        | sec   | 0.262144     | roughly the  |
|              |              |       |              | amount of    |
|              |              |       |              | data to send |
|              |              |       |              | at a time    |
|              |              |       |              | from         |
+--------------+--------------+-------+--------------+--------------+
|              |              |       |              | datastream   |
|              |              |       |              | processes to |
|              |              |       |              | core         |
|              |              |       |              | processes    |
+--------------+--------------+-------+--------------+--------------+
| antennas     | :mat         |       | :math:`[ ]`  | a comma      |
|              | h:`[`\ strin |       | = all        | separated    |
|              | g\ :math:`]` |       |              | list of      |
|              |              |       |              | antennas to  |
|              |              |       |              | include in   |
|              |              |       |              | correlation  |
+--------------+--------------+-------+--------------+--------------+
| baselines    | :mat         |       | :math:`[ ]`  | a comma      |
|              | h:`[`\ strin |       | = all        | separated    |
|              | g\ :math:`]` |       |              | list of      |
|              |              |       |              | baselines to |
|              |              |       |              | correlate;   |
|              |              |       |              | see below    |
+--------------+--------------+-------+--------------+--------------+
| padScans     | bool         |       | True         | insert       |
|              |              |       |              | non          |
|              |              |       |              | -correlation |
|              |              |       |              | scans in     |
|              |              |       |              | recording    |
|              |              |       |              | gaps to      |
|              |              |       |              | prevent      |
+--------------+--------------+-------+--------------+--------------+
|              |              |       |              | `            |
|              |              |       |              | `mpifxcorr`` |
|              |              |       |              | from         |
|              |              |       |              | complaining  |
+--------------+--------------+-------+--------------+--------------+
| invalidMask  | int          |       | 0xFFFF       | this         |
|              |              |       |              | bit-field    |
|              |              |       |              | selects      |
|              |              |       |              | which flag   |
|              |              |       |              | conditions   |
|              |              |       |              | are          |
|              |              |       |              | considered   |
+--------------+--------------+-------+--------------+--------------+
|              |              |       |              | when writing |
|              |              |       |              | flag file:   |
|              |              |       |              | 1=Recording, |
|              |              |       |              | 2=On source, |
|              |              |       |              | 4=Job        |
+--------------+--------------+-------+--------------+--------------+
|              |              |       |              | time range,  |
|              |              |       |              | 8=Antenna in |
|              |              |       |              | job          |
+--------------+--------------+-------+--------------+--------------+
| vis          | int          |       | 32           | number of    |
| BufferLength |              |       |              | visibility   |
|              |              |       |              | buffers to   |
|              |              |       |              | allocate in  |
|              |              |       |              | mpifxcorr    |
+--------------+--------------+-------+--------------+--------------+
| overSamp     | int          |       |              | force all    |
|              |              |       |              | basebands to |
|              |              |       |              | use the      |
|              |              |       |              | given        |
|              |              |       |              | oversample   |
|              |              |       |              | factor       |
+--------------+--------------+-------+--------------+--------------+
| mode         | string       |       | normal       | mode of      |
|              |              |       |              | operation;   |
|              |              |       |              | see below    |
+--------------+--------------+-------+--------------+--------------+
| threadsFile  | string       |       |              | overrides    |
|              |              |       |              | the name of  |
|              |              |       |              | the threads  |
|              |              |       |              | file to use  |
+--------------+--------------+-------+--------------+--------------+
| nCore        | int          |       |              | with nThread |
|              |              |       |              | and          |
|              |              |       |              | machines,    |
|              |              |       |              | cause a      |
|              |              |       |              | ``.threads`` |
|              |              |       |              | file to be   |
|              |              |       |              | made         |
+--------------+--------------+-------+--------------+--------------+
| nThreads     | int          |       |              | number of    |
|              |              |       |              | threads per  |
|              |              |       |              | core in      |
|              |              |       |              | ``.th        |
|              |              |       |              | reads file`` |
+--------------+--------------+-------+--------------+--------------+
| machines     | :mat         |       |              | comma        |
|              | h:`[`\ strin |       |              | separated    |
|              | g\ :math:`]` |       |              | list of      |
|              |              |       |              | machines to  |
|              |              |       |              | use as       |
|              |              |       |              | processors   |
+--------------+--------------+-------+--------------+--------------+
|              |              |       |              | first is     |
|              |              |       |              | head node,   |
|              |              |       |              | then         |
|              |              |       |              | datastreams, |
|              |              |       |              | then cores   |
+--------------+--------------+-------+--------------+--------------+
| maxReadSize  | int          | bytes | 25000000     | maximum      |
|              |              |       |              | number of    |
|              |              |       |              | bytes to     |
|              |              |       |              | read at a    |
|              |              |       |              | time         |
+--------------+--------------+-------+--------------+--------------+
| minReadSize  | int          | bytes | 10000000     | minimum      |
|              |              |       |              | number of    |
|              |              |       |              | bytes to     |
|              |              |       |              | read at a    |
|              |              |       |              | time         |
+--------------+--------------+-------+--------------+--------------+

The *baselines* parameter supports the wildcard character ```` an
individual antenna name, or lists of antenna names separated by ``+`` on
each side of a hyphen (``-``). Multiple baseline designators can be
listed. Examples:

-  ``A1-A2`` : Only correlate one baseline

-  ``A1-A2, A3-A4`` : Correlate 2 baselines

-  ``-*`` : Correlate all baselines

-  ``A1-*`` *or* ``-A1`` : Correlate all baselines to antenna A1

-  ``A1+A2-*`` : Correlate all baselines to antenna A1 or A2

-  ``A1+A2-A3+A4+A5`` : Correlate 6 baselines

A source section can be used to change the properties of an individual
source, such as its position or name. In the future this is where
multiple correlation centers for a given source will be specified. A
source section is enclosed in a pair of curly braces after the keyword
SOURCE followed by the name of a source, for example

::

   SOURCE 3C273
     {
       |bfit[source parameters go here]
     }

or equivalently

::

   SOURCE 3c273 { |bfit[source parameters go here] }

+------------------+--------+-------+----------+--------------------+
| Name             | Type   | Units | Defaults | Comments           |
+==================+========+=======+==========+====================+
| ra               |        | J2000 |          | right ascension,   |
|                  |        |       |          | e.g.,              |
|                  |        |       |          | ``12h34m12.6s`` or |
|                  |        |       |          | ``12:34:12.6``     |
+------------------+--------+-------+----------+--------------------+
| dec              |        | J2000 |          | declination, e.g., |
|                  |        |       |          | ``34d12’23.1``" or |
|                  |        |       |          | ``34:12:23.1``     |
+------------------+--------+-------+----------+--------------------+
| name             | string |       |          | new name for       |
|                  |        |       |          | source             |
+------------------+--------+-------+----------+--------------------+
| calCode          | char   |       | ’ ’      | calibration code,  |
|                  |        |       |          | typically ``A``,   |
|                  |        |       |          | ``B``, ``C`` for   |
|                  |        |       |          | calibrators,       |
+------------------+--------+-------+----------+--------------------+
|                  |        |       |          | ``G`` for a gated  |
|                  |        |       |          | pulsar, or blank   |
|                  |        |       |          | for normal target  |
+------------------+--------+-------+----------+--------------------+
| naifFile         | string |       |          | name of leap       |
|                  |        |       |          | seconds file       |
|                  |        |       |          | (e.g.,             |
|                  |        |       |          | ``naif0010.tls``   |
|                  |        |       |          | for ephemeris      |
+------------------+--------+-------+----------+--------------------+
| ephemObject      | string |       |          | name or number of  |
|                  |        |       |          | object in          |
|                  |        |       |          | ephemeris file     |
+------------------+--------+-------+----------+--------------------+
| ephemFile        | string |       |          | path of ephemeris  |
|                  |        |       |          | file (either       |
|                  |        |       |          | ``.bsp`` or        |
|                  |        |       |          | ``.tle`` format    |
+------------------+--------+-------+----------+--------------------+
| doPointingCentre | bool   |       | true     | Whether the        |
|                  |        |       |          | pointing centre    |
|                  |        |       |          | should be          |
|                  |        |       |          | correlated         |
+------------------+--------+-------+----------+--------------------+
|                  |        |       |          | (only ever turned  |
|                  |        |       |          | off for            |
|                  |        |       |          | multi-phase        |
|                  |        |       |          | center)            |
+------------------+--------+-------+----------+--------------------+
| addPhaseCentre   | string |       |          | contains info on a |
|                  |        |       |          | source to add; see |
|                  |        |       |          | below              |
+------------------+--------+-------+----------+--------------------+

To add additional phase centers, add one or more “addPhaseCentre”
parameters to the source setup. In the parameter, the RA and dec must be
provided. A name and/or calibrator code can be added as well. For
example:
``addPhaseCentre=name@1010-1212/RA@10:10:21.1/Dec@-12:12:00.34`` .

An antenna section allows properties of an individual antenna, such as
position, name, or clock/LO offsets, to be adjusted. Note that the
“late” convention is used in *clockOffset* and *clockRate*, unlike the
“early” convention used in the ``.vex`` file itself (see
§\ `[sec:clockconventions] <#sec:clockconventions>`__).

+-------------+-------------+----------+-------------+-------------+
| Name        | Type        | Units    | Defaults    | Comments    |
+=============+=============+==========+=============+=============+
| name        | string      |          |             | new name to |
|             |             |          |             | assign to   |
|             |             |          |             | this        |
|             |             |          |             | antenna     |
+-------------+-------------+----------+-------------+-------------+
| polSwap     | bool        |          | False       | swap the    |
|             |             |          |             | po          |
|             |             |          |             | larizations |
|             |             |          |             | (i.e.,      |
|             |             |          |             | ``L``       |
|             |             |          |             | :           |
|             |             |          |             | math:`\Left |
|             |             |          |             | rightarrow` |
|             |             |          |             | ``R``) for  |
|             |             |          |             | this        |
|             |             |          |             | antenna     |
+-------------+-------------+----------+-------------+-------------+
| clockOffset | float       | us       | vex value   | overrides   |
|             |             |          |             | the clock   |
|             |             |          |             | offset      |
|             |             |          |             | value from  |
|             |             |          |             | the vex     |
|             |             |          |             | file        |
+-------------+-------------+----------+-------------+-------------+
| clockRate   | float       | us/s     | vex value   | overrides   |
|             |             |          |             | the clock   |
|             |             |          |             | offset rate |
|             |             |          |             | value from  |
|             |             |          |             | the vex     |
|             |             |          |             | file        |
+-------------+-------------+----------+-------------+-------------+
| clockEpoch  | date        |          | vex value   | overrides   |
|             |             |          |             | the epoch   |
|             |             |          |             | of the      |
|             |             |          |             | clock rate  |
|             |             |          |             | value; must |
|             |             |          |             | be present  |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | present if  |
|             |             |          |             | clockRate   |
|             |             |          |             | or          |
|             |             |          |             | clockOffset |
|             |             |          |             | parameter   |
|             |             |          |             | is set      |
+-------------+-------------+----------+-------------+-------------+
| deltaClock  | float       | us       | 0.0         | adds to the |
|             |             |          |             | clock       |
|             |             |          |             | offset      |
|             |             |          |             | (either the |
|             |             |          |             | vex value   |
|             |             |          |             | or the      |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | clockOffset |
|             |             |          |             | above       |
+-------------+-------------+----------+-------------+-------------+
| del         | float       | us/s     | 0.0         | adds to the |
| taClockRate |             |          |             | clock rate  |
|             |             |          |             | (either the |
|             |             |          |             | vex value   |
|             |             |          |             | or the      |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | clockRate   |
|             |             |          |             | above       |
+-------------+-------------+----------+-------------+-------------+
| X           | float       | m        | vex value   | change the  |
|             |             |          |             | X           |
|             |             |          |             | coordinate  |
|             |             |          |             | of the      |
|             |             |          |             | antenna     |
|             |             |          |             | location    |
+-------------+-------------+----------+-------------+-------------+
| Y           | float       | m        | vex value   | change the  |
|             |             |          |             | Y           |
|             |             |          |             | coordinate  |
|             |             |          |             | of the      |
|             |             |          |             | antenna     |
|             |             |          |             | location    |
+-------------+-------------+----------+-------------+-------------+
| Z           | float       | m        | vex value   | change the  |
|             |             |          |             | Z           |
|             |             |          |             | coordinate  |
|             |             |          |             | of the      |
|             |             |          |             | antenna     |
|             |             |          |             | location    |
+-------------+-------------+----------+-------------+-------------+
| format      | string      |          |             | force       |
|             |             |          |             | format to   |
|             |             |          |             | be one of   |
|             |             |          |             | VLBA, MKIV, |
|             |             |          |             | Mark5B, S2, |
|             |             |          |             | or          |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | one of the  |
|             |             |          |             | VDIF types  |
+-------------+-------------+----------+-------------+-------------+
| file        | :math:      |          | (none)      | a comma     |
|             | `[`\ string |          |             | separated   |
|             | \ :math:`]` |          |             | list of     |
|             |             |          |             | data files  |
|             |             |          |             | to          |
|             |             |          |             | correlate   |
+-------------+-------------+----------+-------------+-------------+
| filelist    | string      |          |             | a filename  |
|             |             |          |             | listing     |
|             |             |          |             | files for   |
|             |             |          |             | the DATA    |
|             |             |          |             | TABLE (see  |
|             |             |          |             | §\ `1.      |
|             |             |          |             | 18 <#sec:fi |
|             |             |          |             | lelist>`__) |
+-------------+-------------+----------+-------------+-------------+
| networkPort | int         |          |             | The eVLBI   |
|             |             |          |             | network     |
|             |             |          |             | port to use |
|             |             |          |             | for         |
|             |             |          |             | TCP/UDP. A  |
|             |             |          |             | non-number  |
|             |             |          |             | indicates   |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | raw mode    |
|             |             |          |             | attached to |
|             |             |          |             | an ethernet |
|             |             |          |             | interface.  |
|             |             |          |             | Both force  |
|             |             |          |             | NETWORK     |
|             |             |          |             | media       |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | type in     |
|             |             |          |             | ``.input``  |
|             |             |          |             | file.       |
+-------------+-------------+----------+-------------+-------------+
| windowSize  | int         |          |             | TCP window  |
|             |             |          |             | size in     |
|             |             |          |             | kilobytes   |
|             |             |          |             | for eVLBI.  |
|             |             |          |             | Set to      |
|             |             |          |             | :math:`<0`  |
|             |             |          |             | in bytes    |
|             |             |          |             | for UDP     |
+-------------+-------------+----------+-------------+-------------+
| UDP_MTU     | int         |          |             | Same as     |
|             |             |          |             | setting     |
|             |             |          |             | windowSize  |
|             |             |          |             | to negative |
|             |             |          |             | of value.   |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | For raw     |
|             |             |          |             | mode, the   |
|             |             |          |             | number of   |
|             |             |          |             | bytes to    |
|             |             |          |             | strip from  |
|             |             |          |             | ethernet    |
|             |             |          |             | frame.      |
+-------------+-------------+----------+-------------+-------------+
| vsn         | string      |          |             | override    |
|             |             |          |             | the Mark5   |
|             |             |          |             | Module to   |
|             |             |          |             | be used     |
+-------------+-------------+----------+-------------+-------------+
| zoom        | string      |          |             | uses the    |
|             |             |          |             | global zoom |
|             |             |          |             | co          |
|             |             |          |             | nfiguration |
|             |             |          |             | with        |
|             |             |          |             | matching    |
|             |             |          |             | name for    |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | this        |
|             |             |          |             | antenna;    |
|             |             |          |             | ``z         |
|             |             |          |             | oom=Zoom1`` |
|             |             |          |             | will match  |
|             |             |          |             | ZOOM block  |
|             |             |          |             | called      |
|             |             |          |             | ``Zoom1``   |
+-------------+-------------+----------+-------------+-------------+
| addZoomFreq | string      |          |             | adds a zoom |
|             |             |          |             | band with   |
|             |             |          |             | specified   |
|             |             |          |             | freq/bw as  |
|             |             |          |             | shown:      |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | freq@1810.0 |
|             |             |          |             | /bw@4.0\ :m |
|             |             |          |             | ath:`[`/spe |
|             |             |          |             | cAvg@4\ :ma |
|             |             |          |             | th:`][`/nop |
|             |             |          |             | arent@false |
|             |             |          |             | \ :math:`]` |
+-------------+-------------+----------+-------------+-------------+
| fr          | :math       | microsec |             | adds clock  |
| eqClockOffs | :`[`\ float |          |             | offsets to  |
|             | \ :math:`]` |          |             | each        |
|             |             |          |             | recorded    |
|             |             |          |             | frequency   |
|             |             |          |             | using the   |
|             |             |          |             | format:     |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | `           |
|             |             |          |             | `freqClockO |
|             |             |          |             | ffs=``\ *f1 |
|             |             |          |             | ,f2,f3,f4*; |
|             |             |          |             | must be     |
|             |             |          |             | same length |
|             |             |          |             | as          |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | number of   |
|             |             |          |             | recorded    |
|             |             |          |             | freqs,      |
|             |             |          |             | first value |
|             |             |          |             | must be     |
|             |             |          |             | zero        |
+-------------+-------------+----------+-------------+-------------+
| loOffsets   | :math       | Hz       |             | adds LO     |
|             | :`[`\ float |          |             | offsets to  |
|             | \ :math:`]` |          |             | each        |
|             |             |          |             | recorded    |
|             |             |          |             | frequency   |
|             |             |          |             | using the   |
|             |             |          |             | format:     |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | ``loOffs    |
|             |             |          |             | ets=``\ *f1 |
|             |             |          |             | ,f2,f3,f4*; |
|             |             |          |             | must be     |
|             |             |          |             | same length |
|             |             |          |             | as          |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | number of   |
|             |             |          |             | recorded    |
|             |             |          |             | freqs.      |
+-------------+-------------+----------+-------------+-------------+
| tcalFreq    | int         | Hz       | 0           | enables     |
|             |             |          |             | switched    |
|             |             |          |             | power       |
|             |             |          |             | detection   |
|             |             |          |             | at          |
|             |             |          |             | specified   |
|             |             |          |             | frequency   |
+-------------+-------------+----------+-------------+-------------+
| phaseCalInt | int         | MHz      | 1           | zero turns  |
|             |             |          |             | off phase   |
|             |             |          |             | cal         |
|             |             |          |             | extraction, |
|             |             |          |             | positive    |
|             |             |          |             | value is    |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | the         |
|             |             |          |             | interval    |
|             |             |          |             | between     |
|             |             |          |             | tones to be |
|             |             |          |             | extracted   |
+-------------+-------------+----------+-------------+-------------+
| toneGuard   | float       | MHz      | 0.125 of bw | when using  |
|             |             |          |             | to          |
|             |             |          |             | neSelection |
|             |             |          |             | *smart* or  |
|             |             |          |             | *most*      |
|             |             |          |             | don’t       |
|             |             |          |             | select      |
|             |             |          |             | tones       |
+-------------+-------------+----------+-------------+-------------+
|             |             |          |             | within this |
|             |             |          |             | range of    |
|             |             |          |             | band edge,  |
|             |             |          |             | if possible |
+-------------+-------------+----------+-------------+-------------+
| to          | string      |          | ``smart``   | tone        |
| neSelection |             |          |             | selection   |
|             |             |          |             | algorithm;  |
|             |             |          |             | see below   |
+-------------+-------------+----------+-------------+-------------+
| sampling    | string      |          | ``REAL``    | set to      |
|             |             |          |             | ``COMPLEX`` |
|             |             |          |             | for complex |
|             |             |          |             | sampled     |
|             |             |          |             | data        |
+-------------+-------------+----------+-------------+-------------+
| fake        | bool        |          | False       | enable a    |
|             |             |          |             | fake data   |
|             |             |          |             | source      |
+-------------+-------------+----------+-------------+-------------+

Possible values of “tone Selection” are:

+-------+-------------------------------------------------------------+
| smart | write the 2 most extreme tones at least toneGuard from band |
|       | edge (default)                                              |
+-------+-------------------------------------------------------------+
| vex   | write the tones listed in the vex file to FITS              |
+-------+-------------------------------------------------------------+
| none  | don’t write any tones to FITS                               |
+-------+-------------------------------------------------------------+
| all   | write all extracted tones to FITS                           |
+-------+-------------------------------------------------------------+
| ends  | write the 2 most extreme tones to FITS                      |
+-------+-------------------------------------------------------------+
| most  | write all tones not closer than toneGuard to band edge      |
+-------+-------------------------------------------------------------+

Setup sections are enclosed in braces after the word SETUP and a name
given to this setup section. The setup name is referenced by a RULE
section (see below). A setup with the special name ``default`` will be
applied to any scans not otherwise assigned to setups by rule sections.
If no setup sections are defined, a setup called ``default``, with all
default parameters, will be implicitly created and applied to all scans.
The order of setup sections is immaterial.

+--------------+--------------+-------+--------------+--------------+
| Name         | Type         | Units | Defaults     | Comments     |
+==============+==============+=======+==============+==============+
| tInt         | float        | sec   | 2            | integration  |
|              |              |       |              | time         |
+--------------+--------------+-------+--------------+--------------+
| FFTSpecRes   | float        | MHz   | 0.125        | frequency    |
|              |              |       |              | resolution   |
|              |              |       |              | of FFT       |
+--------------+--------------+-------+--------------+--------------+
| specRes      | float        | MHz   | 0.5          | output freq  |
|              |              |       |              | res (must be |
|              |              |       |              | mult. of     |
|              |              |       |              | FFTSpecRes   |
+--------------+--------------+-------+--------------+--------------+
| nChan        | int          |       | 16           | number of    |
|              |              |       |              | channels per |
|              |              |       |              | spectral     |
|              |              |       |              | window; must |
|              |              |       |              | be           |
|              |              |       |              | :math:`5^    |
|              |              |       |              | m \cdot 2^n` |
+--------------+--------------+-------+--------------+--------------+
| specAvg      | int          |       | 1            | how many     |
|              |              |       |              | channels to  |
|              |              |       |              | average      |
|              |              |       |              | together     |
|              |              |       |              | after        |
|              |              |       |              | correlation  |
+--------------+--------------+-------+--------------+--------------+
| fr           | int          |       | 1            | fhe fringe   |
| ingeRotOrder |              |       |              | rotation     |
|              |              |       |              | order:       |
|              |              |       |              | 0=post-F,    |
|              |              |       |              | 1=linear,    |
|              |              |       |              | 2=quadratic  |
+--------------+--------------+-------+--------------+--------------+
| strideLength | int          |       | 16           | number of    |
|              |              |       |              | channels to  |
|              |              |       |              | “stride” for |
|              |              |       |              | fringe       |
|              |              |       |              | rotation,    |
|              |              |       |              | etc.         |
+--------------+--------------+-------+--------------+--------------+
| xmacLength   | int          |       | 128          | number of    |
|              |              |       |              | channels to  |
|              |              |       |              | “stride” for |
|              |              |       |              | cr           |
|              |              |       |              | oss-multiply |
|              |              |       |              | a            |
|              |              |       |              | ccumulations |
+--------------+--------------+-------+--------------+--------------+
| num          | int          |       | 1            | number of    |
| BufferedFFTs |              |       |              | FFTs to do   |
|              |              |       |              | in a row for |
|              |              |       |              | each         |
|              |              |       |              | datastream,  |
|              |              |       |              | before XMAC  |
+--------------+--------------+-------+--------------+--------------+
| doPolar      | bool         |       | True         | correlate    |
|              |              |       |              | cross hands  |
|              |              |       |              | when         |
|              |              |       |              | possible     |
+--------------+--------------+-------+--------------+--------------+
| postFFringe  | bool         |       | False        | do fringe    |
|              |              |       |              | rotation     |
|              |              |       |              | after FFT?   |
+--------------+--------------+-------+--------------+--------------+
| binConfig    | string       |       | none         | if           |
|              |              |       |              | specified,   |
|              |              |       |              | apply this   |
|              |              |       |              | pulsar bin   |
|              |              |       |              | config file  |
|              |              |       |              | to this      |
|              |              |       |              | setup        |
+--------------+--------------+-------+--------------+--------------+
| freqId       | :            |       | :math:`[ ]`  | frequency    |
|              | math:`[`\ in |       | = all        | bands to     |
|              | t\ :math:`]` |       |              | correlate    |
+--------------+--------------+-------+--------------+--------------+
| out          | float’auto’  | MHz   | n/a          | Target       |
| putBandwidth |              |       |              | bandwidth    |
|              |              |       |              | when auto    |
|              |              |       |              | determining  |
|              |              |       |              | outputbands, |
|              |              |       |              | or,          |
+--------------+--------------+-------+--------------+--------------+
| a            | :mat         |       | n/a          | Add an       |
| ddOutputBand | h:`[`\ strin |       |              | outputband   |
|              | g\ :math:`]` |       |              | with         |
|              |              |       |              | explicit     |
|              |              |       |              | placement    |
|              |              |       |              | and          |
|              |              |       |              | bandwidth    |
+--------------+--------------+-------+--------------+--------------+
|              |              |       |              | freq@1       |
|              |              |       |              | 810.0/bw@4.0 |
+--------------+--------------+-------+--------------+--------------+

Note that either “FFTSpecRes” and “specRes” can be used, or “nChan” and
“specAvg” can be used, but the two sets cannot be mixed.

Zoom channels can be configured in a special section and referenced from
ANTENNA sections to minimize complexity of the ``.v2d`` file. Each ZOOM
section has a name and one or more “addZoomFreq” parameters, with the
same format as they would have in the ANTENNA block.

Output bands can be defined in the SETUP block in DiFX 2.7 (OUTPUTBAND
block in DiFX 2.8) via “outputBandwidth” or alternatively
“addOutputBand”. Permitted “outputBandwidth” values are ’auto’ to
auto-determine the bandwidth, or, an explicit bandwidth as a decimal
value in MHz. Band placement is automatic. For explicit placement and
bandwith instead use “addOutputBand”.

Earth Orientation Parameter (EOP) data can be provided via one or more
EOP sections. EOP data should be provided either in the ``.v2d`` file or
in the vex file, but not both. Normally the vex file would be used to
set EOP values, but there may be cases (eVLBI?) that want to use the vex
file from ``sched`` without any modification. Like ANTENNA and SOURCE
sections, each EOP section has a name. The name must be in a form that
can be converted directly to a date (see above for legal date formats).
Conventional use suggests that these dates should correspond to 0 hours
UT; deviation from this practice is at the users risk. There are four
parameters that should all be set within an EOP section:

======= ===== ====== ======== ====================================
Name    Type  Units  Defaults Comments
======= ===== ====== ======== ====================================
tai_utc float sec             TAI minus UTC; the leap-second count
ut1_utc float sec             UT1 minus UTC; Earth rotation phase
xPole   float arcsec          X component of spin axis offset
yPole   float arcsec          Y component of spin axis offset
======= ===== ====== ======== ====================================

A rule section is used to assign a setup to a particular source name,
calibration code (currently not supported), scan name, or vex mode. The
order of rule sections *does* matter as the order determines the
priority of the rules. The first rule that matches a scan is applied to
that scan. The correlator setup used for scans that match a rule is
determined by the parameter called “setup”. A special setup name
``SKIP`` causes matching scans not to be correlated. Any parameters not
specified are interpreted as fully inclusive. Note that multiple rule
sections can reference the same setup section. Multiple values may be
applied to any of the parameters except for “setup”. This is
accomplished by comma separation of the values in a single assignment or
with repeated assignments. Thus

::

     RULE rule1
     {
       source = 3C84,3C273
       setup = BrightSourceSetup
     }

is equivalent to

::

     RULE rule2
     {
       source = 3C84 3C273
       setup = BrightSourceSetup
     }

is equivalent to

::

     RULE rule3
     {
       source = 3C84
       source = 3C273
       setup = BrightSourceSetup
     }

The names given to rules (e.g., rule1, rule2 and rule3 above) are not
used anywhere (yet) but are required to be unique.

+---------+-----------------------+-------+-----------------------+---+
| Name    | Type                  | Units | Comments              |   |
+=========+=======================+=======+=======================+===+
| scan    | :math:`               |       | one or more scan      |   |
|         | [`\ string\ :math:`]` |       | name, as specified in |   |
|         |                       |       | the vex file, to      |   |
|         |                       |       | select with this rule |   |
+---------+-----------------------+-------+-----------------------+---+
| source  | :math:`               |       | one or more source    |   |
|         | [`\ string\ :math:`]` |       | name, as specified in |   |
|         |                       |       | the vex file, to      |   |
|         |                       |       | select with this rule |   |
+---------+-----------------------+-------+-----------------------+---+
| calCode | :math                 |       | one or more           |   |
|         | :`[`\ char\ :math:`]` |       | calibration code to   |   |
|         |                       |       | select with this rule |   |
+---------+-----------------------+-------+-----------------------+---+
| mode    | :math:`               |       | one or more modes as  |   |
|         | [`\ string\ :math:`]` |       | defined in the vex    |   |
|         |                       |       | file to select with   |   |
|         |                       |       | this rule             |   |
+---------+-----------------------+-------+-----------------------+---+
| setup   | string                |       | The name of the SETUP |   |
|         |                       |       | section to use, or    |   |
|         |                       |       | SKIP if this rule     |   |
|         |                       |       | describes scans       |   |
+---------+-----------------------+-------+-----------------------+---+
|         |                       |       | not to correlate      |   |
+---------+-----------------------+-------+-----------------------+---+

Note that source names and calibration codes reassigned by source
sections are not used. Only the names and calibration codes in the vex
file are compared.

There are currently two modes of operation supported by ``vex2difx``.
The mode used in the vast majority of situations is called ``normal``
and is the default if none is specified. Currently one alternative mode,
``profile``, is supported. This mode is useful for generating pulse
profiles that would be useful for pulsar gating, scrunching, and
binning. The difference compared to normal mode is that the standard
autocorrelations are turned off and instead are computed as if they are
cross correlations. This allows multiple pulsar bins to be stored. No
formal cross correlations are performed. To be useful, one must create
and specify a ``.binconfig`` file and select only the pulsar(s) from the
experiment.

See http://www.atnf.csiro.au/vlbi/dokuwiki/doku.php/difx/vex2difx for
more complete information and examples.

.. _sec:xcb:

.xcb
----

When generation of sniffer output files is not disabled, each ``.FITS``
file written by ``difx2fits`` will be accompanied by a corresponding
``.xcb`` file. This file contains cross-correlation spectra for each
antenna for each baseline. In order to minimize the output data size,
spectra for the same source will only be repeated once per 15 minutes.
The file contains many concatenated records. Each record has the spectra
for all baseband channels for a particular baseline and has the
following format which is very similar to that of the ``.acb`` files.
Note that no spaces are allowed within any field. Values in
``typewriter`` font without comments are explicit strings that are
required.

+----------------+----------------+----------------+----------------+
| Line(s)        | Value          | Units          | Comments       |
+================+================+================+================+
| 1              | ``timerange:`` |                |                |
+----------------+----------------+----------------+----------------+
|                | *MJD*          | integer        | MJD day number |
|                |                | :math:`\ge 1`  | corresponding  |
|                |                |                | to line        |
+----------------+----------------+----------------+----------------+
|                | *start time*   | string         | e.g.,          |
|                |                |                | `              |
|                |                |                | `13h34m22.6s`` |
+----------------+----------------+----------------+----------------+
|                | *stop time*    | string         | e.g.,          |
|                |                |                | `              |
|                |                |                | `13h34m52.0s`` |
+----------------+----------------+----------------+----------------+
|                | ``obscode:``   |                |                |
+----------------+----------------+----------------+----------------+
|                | *observe code* | string         | e.g.,          |
|                |                |                | ``MT831``      |
+----------------+----------------+----------------+----------------+
|                | ``chans:``     |                |                |
+----------------+----------------+----------------+----------------+
|                | *              | :math:`\ge 1`  | number of      |
|                | n*\ :math:`_{\ |                | channels per   |
|                | mathrm{chan}}` |                | baseband       |
|                |                |                | channel        |
+----------------+----------------+----------------+----------------+
|                | ``x``          |                |                |
+----------------+----------------+----------------+----------------+
|                | *n*\ :math:`_{ | :math:`\ge 1`  | number of      |
|                | \mathrm{BBC}}` |                | baseband       |
|                |                |                | channels       |
+----------------+----------------+----------------+----------------+
| 2              | ``source:``    |                |                |
+----------------+----------------+----------------+----------------+
|                | *source name*  | string         | e.g.,          |
|                |                |                | ``0316+413``   |
+----------------+----------------+----------------+----------------+
|                | ``bandw:``     |                |                |
+----------------+----------------+----------------+----------------+
|                | *bandwidth*    | MHz            | baseband       |
|                |                |                | channel        |
|                |                |                | bandwidth      |
+----------------+----------------+----------------+----------------+
|                | ``MHz``        |                |                |
+----------------+----------------+----------------+----------------+
| 3 to           | ``bandfreq:``  |                |                |
| 2+\ :math:`n_{ |                |                |                |
| \mathrm{BBC}}` |                |                |                |
+----------------+----------------+----------------+----------------+
|                | *frequency*    | GHz            | band edge      |
|                |                |                | (SSLO)         |
|                |                |                | frequency of   |
|                |                |                | baseband       |
|                |                |                | channel        |
+----------------+----------------+----------------+----------------+
|                | ``GHz polar:`` |                |                |
+----------------+----------------+----------------+----------------+
|                | *polarization* | 2 chars        | e.g., ``RR``   |
|                |                |                | or ``LL``      |
+----------------+----------------+----------------+----------------+
|                | ``side:``      |                |                |
+----------------+----------------+----------------+----------------+
|                | *sideband*     | ``U`` or ``L`` | for upper or   |
|                |                |                | lower sideband |
+----------------+----------------+----------------+----------------+
|                | ``bbchan:``    |                |                |
+----------------+----------------+----------------+----------------+
|                | *bbc*          | ``0``          | Currently not  |
|                |                |                | used but       |
|                |                |                | needed for     |
|                |                |                | conformity     |
+----------------+----------------+----------------+----------------+
| 3+\ :math:`n_{ | *ant1 number*  | :math:`\ge 1`  | number of      |
| \mathrm{BBC}}` |                |                | first antenna  |
| to             |                |                |                |
+----------------+----------------+----------------+----------------+
| 2+\ :ma        | *ant2 number*  | :math:`\ge 1`  |                |
| th:`n_{\mathrm |                |                |                |
| {BBC}}(n_{\mat |                |                |                |
| hrm{chan}}+1)` |                |                |                |
+----------------+----------------+----------------+----------------+
|                | *ant1 name*    | string         |                |
+----------------+----------------+----------------+----------------+
|                | *ant2 name*    | string         |                |
+----------------+----------------+----------------+----------------+
|                | *channel       | :math:`\ge 1`  | :math:`= \     |
|                | number*        |                | mathrm{chan} + |
|                |                |                |  (\mathrm{bbc} |
|                |                |                | -1) \cdot n_{\ |
|                |                |                | mathrm{chan}}` |
|                |                |                | for chan, bbc  |
|                |                |                | :math:`\ge 1`  |
+----------------+----------------+----------------+----------------+
|                | *amplitude*    | :              |                |
|                |                | math:`\ge 0.0` |                |
+----------------+----------------+----------------+----------------+
|                | *phase*        | degrees        |                |
+----------------+----------------+----------------+----------------+

The above are repeated for each cross correlation spectrum record. This
file can be plotted directly with ``plotbp`` or handled more
automatically with ``difxsniff``.

.. _sec:dotvex:

.vex, .skd, .vex.obs, & .skd.obs
--------------------------------

The vex (Vlbi EXperiment) file :raw-latex:`\cite{vex}` format is a
standard observation description format used globally for scheduling
observations and for driving the correlation thereof. The original vex
file for an experiment is typically created by ``sched`` or ``sked``. In
the former case (the case used by most astronomical VLBI), the vex file
has the unfortunate file extension ``.skd``; in the later, the file
extension is usually the less confusing ``.vex`` . These two vex
formatted files contain only observation-scheduling based information. A
small amount of information based on the actualities of the observation
are added by ``db2vex``, producing a new vex file with an additional
file extension ``.obs`` . Please see vex documentation external to this
manual for more information.

.. _sec:vis:

.vis
----

Program ``zerocorr`` (§\ `[sec:zerocorr] <#sec:zerocorr>`__ produces
visibility output in a format documented here. There is one line of
output per generated spectral point. Each line has 8 columns as per the
following table:

==== ====================================================
Line Contents
==== ====================================================
1    Channel (spectral point) number (counts from zero)
2    Frequency relative to first spectral channel (Hz)
3    Real value of the visibility
4    Imaginary value of the visibility
5    Amplitude
6    Phase
7    Autocorrelation of the first datastream (real only)
8    Autocorrelation of the second datastream (real only)
==== ====================================================

.. _sec:zc:

.zerocorr
---------

Program ``zerocorr`` (§\ `[sec:zerocorr] <#sec:zerocorr>`__ is a simple
cross correlator. It is limited to correlating one visibility spectrum
from one baseband channel and can only make use of a constant offset
delay model. The section documents the file that drives this program.
This file consists of 17 lines of text. The first 7 lines describe
properties of the data from the first antenna and the following (7)
lines describe properties of the second antenna as follows:

+--------+------------------------------------------------------------+
| Line   | Contents                                                   |
+========+============================================================+
| 1 (8)  | Input baseband data file name                              |
+--------+------------------------------------------------------------+
| 2 (9)  | Data format (e.g., Mark5B-2048-16-2)                       |
+--------+------------------------------------------------------------+
| 3 (10) | Input sub-band to process (0-based index)                  |
+--------+------------------------------------------------------------+
| 4 (11) | Offset into the file (bytes)                               |
+--------+------------------------------------------------------------+
| 5 (12) | Size of FFT to perform over input bandwidth                |
|        | (:math:`2\times n_{\rm chan}`)                             |
+--------+------------------------------------------------------------+
| 6 (13) | First channel (spectral point) to correlate                |
+--------+------------------------------------------------------------+
| 7 (14) | Number of channels to correlate (negative indicates LSB)   |
+--------+------------------------------------------------------------+

The last three lines dictate the output data files and the number of
FFTs to process:

==== =================================================================
Line Contents
==== =================================================================
15   Name of output visibility (``.vis``; §\ `1.45 <#sec:vis>`__) file
16   Name of output ``.lag`` (§\ `1.29 <#sec:lag>`__) file
17   Number of FFTs to process (if -1, run on entire input files)
==== =================================================================
