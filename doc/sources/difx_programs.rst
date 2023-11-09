.. role:: raw-latex(raw)
   :format: latex
..

.. _sec:programs:

Reference guide to programs and utilities
=========================================

This section has usage information for the numerous programs and scripts
used in the DiFX system. Basic help information for most or all of these
programs can be gotten by typing the program name with either no command
line arguments or with a ``-h`` option, depending on the program. In the
usage descriptions below, arguments in square brackets
:math:`[` :math:`]` are optional and can often include multiple
different parameters. Cases where 1 or more arguments of a certain type
(such as files) can be passed to the program, the usage instructions
will look like *arg*\ 1 :math:`[\ \cdots` *arg*\ N\ :math:`]`, with the
implication that N arguments of this type were passed. In cases where 0
arguments of that type is also allowed, that first argument will also be
in square brackets. If it is not obvious from the program name, the
software package containing the program follows the section header. The
package that includes each program is included in its section heading.

Note that several VLBA specific programs are discussed in this manual
that are not documented here, such as ``tsm``. These are preexisting
programs that may be documented elsewhere and are less likely to be
useful outside VLBA operations. Also note that programs from the
``nrao_difx_db`` package are internal to NRAO and in general are not
applicable outside VLBA operations. The code for these programs can be
made available upon request.

.. _sec:apd2antenna:

apd2antenna :math:`\mathrm{(package: difx2fits)}`
-------------------------------------------------

Python program ``apd2antenna`` will read a ``.apd file`` (see
Sec. `[sec:apd] <#sec:apd>`__), which stores baseline-based fringe-fit
solutions, and writes to ``stdout`` an antenna-based set of fringe-fit
values. These are determined through a least-squares fit. A reference
antenna must be specfied.

-  Usage: ``apd2antenna`` *apdFile refAnt*

-  *apdFile* is a ``.apd`` file (§\ `[sec:apd] <#sec:apd>`__), created
   by ``difx2fits``

-  *refAnt* is a 1-based number or text string indicating the reference
   antenna to be used

-  Example: ``apd2antenna DQ1206.apd 2``

A reference antenna is required as the least-squares solution is
ill-determined otherwise. The reference antenna has its delay, rate, and
phase set to zero in the process.

The output of this program, which is sent to ``stdout`` is documented in
Sec. `[sec:aapd] <#sec:aapd>`__. It is conventional to redirect the
output of this program to a file ending in ``.aapd`` .

.. _sec:avgDiFX:

avgDiFX :math:`\mathrm{(package: difxio)}`
------------------------------------------

``avgDiFX`` is a utility packaged with difxio. It reads two complete
DiFX filesets that need to cover the same timerange and have essentially
the same array structure and produces a new complete fileset. The
``.input``, ``.calc``, and ``.im`` files, along with any extracted pulse
cal data, are copied from the first of the two datasets. The visibility
data from the two file sets is averaged. Certain pulsar processing may
benefit from this capability.

-  Usage: ``avgDiFX`` *configFile1 configFile2 outputConfigFile*

-  Example: ``avgDiFX pass1_01.input pass2_01.input output_01.input``

.. _sec:bp2antenna:

bp2antenna :math:`\mathrm{(package: difx2fits)}`
------------------------------------------------

Python program ``bp2antenna`` will read a ``.bandpass file`` (see
Sec. `[sec:filebandpass] <#sec:filebandpass>`__), which stores
baseline-based bandpass solutions determined by ``difx2fits``, and
writes to ``stdout`` an antenna-based set of bandpasses. These are
determined through a least-squares fit separately to phase and
amplitude. This program requests a reference antenna be provided and a
minimum of three antennas are required. This antenna’s phases are fixed
to be zero and all other antennas’ phases are determined relative to
that antenna. If a negative number is provided for the reference
antenna, the antenna phases will be adjusted after antenna-based
solutions are found such that the average bandpass phase at each
frequency is zero. The bandpass can be smoothed using the low-pass
filter ``LPF`` option. The value to be provided is measured in MHz;
bandpass features smaller than this are smoothed out. Optionally a pulse
cal data file (determined by non-DiFX program pcalanal) can be applied
to create “absolute phase” antenna-based bandpasses. This feature is
experimental. If autocorrelation bandpasses are included in the
``.bandpass`` file, they will be ignored.

-  Usage: ``bp2antenna`` *bandpassFile refAnt* :math:`[` *LPF* :math:`[`
   *pcalFile* :math:`]` :math:`]`

-  *bandpassFile* is a ``.bandpass`` file
   (§\ `[sec:filebandpass] <#sec:filebandpass>`__), created by
   ``difx2fits``

-  *refAnt* is a 1-based number or text string indicating the reference
   antenna to be used

-  *LPF* is the low pass filter parameter, measured in MHz

-  *pcalFile* contains pulse cal phases after removing dominant delays
   (FIXME: to be documented...)

-  Example: ``bp2antenna DQ1206.bandpass 2``

The output of this program, which is sent to ``stdout`` is documented in
Sec. `[sec:abp] <#sec:abp>`__. It is conventional to redirect the output
of this program to a file ending in ``.abp`` .

.. _sec:calcif2:

calcif2
-------

Program ``calcif2`` evaluates the delay model, producing a delay model
file (ending with ``.im``) from a file containing the source, antenna
and scan timing information (ending with ``.calc``). The detailed
calculations are performed by the Goddard CALC program. Prior to this
writing (May 4, 2013), the only option for the calculation back-end was
CALC version 9 with NRAO additions which add ocean loading and
near-field corrections (accurate as close as a few
:math:`\times 10^5` km). Now new options are being introduced, including
CALC 9 with the Sekido-Fukushima near-field model (using the ``–sekido``
option). Note that this option requires an installation of a special
version of CALC that is not covered in this document.

Instead of calling CALC for every tabulated model row, ``calcif2``
computes a 5th degree polynomial every 120 seconds (typically), very
closely resembling the delay model generation used at the VLBA hardware
correlator. These polynomials are then evaluated at each model point.
This results in a tremendous speedup at negligible loss of accuracy. By
default ``calcif2`` will call CALC three times for each model point and
calculates more accurate :math:`u, v, w` coordinates from delay
measurements made over a small patch of the sky:

.. math:: (u, v, w) = \left(-c \frac{d \tau}{d l}, c \frac{d \tau}{d m}, c \tau \right)

where :math:`l, m` are angular coordinates (in radians) relative to the
delay center on the sky, :math:`\tau` is the delay at the delay center
and :math:`c` is the speed of light.

Normally ``calcif2`` will be called by ``difxqueue``, ``startdifx``, or
another higher-level program if needed.

``calcif2`` connects via Remote Procedure Call (RPC) to an instance of
``CalcServer`` which must be running on a computer identified by
environment variable ``$CALC_SERVER``, or by the specified computer if
the ``-s`` option is used. If the output files (specified in the
``.calc`` file) exist and are current (have newer modification times
than the ``.calc`` file, then the files will not be recreated unless the
force option is used.

In addition to calculating the delay model, this program computes the
baseline vectors, :math:`u,v,w` (relative to Earth center on a
per-antenna basis) and source elevation vs. time.

-  Usage: ``calcif2`` :math:`[` *options* :math:`]` :math:`\{` ``-a``
   :math:`\mid` *calcFile1* :math:`[` *calcFile2* :math:`[\cdots]`
   :math:`]` :math:`\}`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-a`` or ``–all`` : run on all ``.calc`` files found in the
      current directory

   -  ``-v`` or ``–verbose`` : print more verbose logging/debug info

   -  ``-q`` or ``–quiet`` : print less verbose logging/debug info

   -  ``-f`` or ``–force`` : rerun even if output files exist and are
      current

   -  ``-n`` or ``–noaber`` : don’t perform aberration :math:`u,v,w`
      corrections

   -  ``-F`` or ``–fit`` : Instead of producing an :math:`n` term
      polynomial from :math:`n` samples, calculate more samples and
      perform a fit. This is not of general use as tests have shown that
      the improvement is negligible.

   -  ``-z`` or ``–allow-neg-delay`` : don’t zero delays that are
      negative (i.e., shadowed by Earth)

   -  ``-A`` or ``–noatmos`` : don’t include atmosphere in calculation
      of :math:`u,v,w`

   -  ``-s`` *server* or ``–server`` *server* : connect to *server*, not
      ``$CALC_SERVER``

   -  ``-o`` *order* or ``–order`` *order* : make polynomials with
      *order*\ +1 terms (default 5)

   -  ``-i`` *int* or ``–interval`` *int* : make a polynomial every
      *int* seconds (default 120)

   -  ``–override-version`` ignore difx version clashes

-  *calcFile* is a ``.calc`` file (§\ `[sec:input] <#sec:input>`__),
   such as one generated by ``vex2difx`` (§\ `1.102 <#sec:vex2difx>`__)

-  Example 1: ``calcif2 job1420.000.calc job1421.000.calc``

-  Example 2: ``calcif2 -s kepler job1420.000.calc``

-  Example 3: ``calcif2 -a -i 60``

.. _sec:CalcServer:

CalcServer
----------

Program ``CalcServer`` contains the Goddard Space Flight Center CALC
package version 9.1, used to compute geometric delay models for VLBI
applications. It is a repackaged version of the same source code that is
used to compute models on the VLBA correlator. It is configured to run
as a server. All of its interactions are via RPC calls from other
programs, such as ``calcif2``, which could be running on the same or
different computer. This program only needs to be started once on a
given machine using the ``startCalcServer`` script. It should probably
be set to start automatically upon boot of the machine on which
``CalcServer`` runs. Environment variable ``$CALC_SERVER`` should be set
to the name of the computer on which ``CalcServer`` is running.

-  Start: ``startCalcServer``

-  Test: ``checkCalcServer $CALC_SERVER``

-  Stop: ``killall CalcServer``

Note that ``CalcServer`` must be installed (with ``make install``) to be
usable as the paths for various files are permanently set in the
executables at compile time. At this time it seems ``CalcServer`` cannot
be compiled for 64-bit machines.

checkdir :math:`\mathrm{(package: mk5daemon)}`
----------------------------------------------

Program ``checkdir`` can be used to check the integrity of one or more
``.dir`` files that are stored at a location pointed by environment
variable ``MARK5_DIR_PATH`` . Even after many years of use, the Mark5
units tend to be a weak point in the reliability of correlation. Since
reading the module directory and examining a bit of data from each scan
are the first actions done to a module, many of the possible problems
show up at this time. This utility looks for a number of possible
problems, including scans that could not be decoded, overlapping or
out-of-order scans, scans with illegal format parameters and others.
This program makes no attempt to fix problems. It is up to the operator
to determine if a problem is real or not and if further action should be
taken. In cases where many scans are not properly decoded it is
worthwhile to rename (or remove) the ``.dir`` file in question and
regenerate the directory. A second directory read often succeeds when a
first one does not.

-  Usage: ``checkdir`` :math:`[` *options* :math:`]` :math:`[` *module
   list* :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : be more verbose in execution (``-v -v``
      for more)

   -  ``-q`` or ``–quite`` : be less verbose in execution

   -  ``-a`` or ``–all`` : run on all files in $\ ``MARK5_DIR_PATH``

   -  ``-s`` or ``–show`` : print the entire directory file to screen

   -  ``-H`` or ``–histogram`` : print a histogram of record rates

-  Example 1: ``checkdir -a``

-  Example 2: ``checkdir NRAO-123 NRAO+266``

-  Example 3: ``checkdir -s NRAO+233``

Either ``-a`` or a list of module names can be provided (but not both
simultaneously). If the former, a less verbose output will be generated
by default. Except in the lowest verbosity mode (the default for
``-a``), module directories without any detected problems will show a
one line summary consisting of the number of scans and the time range of
the module.

checkmpifxcorr :math:`\mathrm{(package: mpifxcorr)}`
----------------------------------------------------

Program ``checkmpifxcorr`` reads the ``.input`` and other associated
files for a DiFX job and parses them with the same logic used by
``mpifxcorr`` in order to determine their validity.

-  Usage: ``checkmpifxcorr`` :math:`[` *options* :math:`]` *configFile*
   …

-  *options* can be:

   -  ``-h`` : print usage information and exit

   -  ``-f -s -e -w -i -v -d`` : select the verbosity level of output
      (options refer to “fatal”, “severe”, “error”, “warning”, “info”,
      “verbose”, and “debug” levels).

All of the files referenced from the provided configuration (``.input``)
files are read as well (excepting any baseband files or the ``.vex``
file). This check has proven especially useful for pulsar processing.
The default verbosity level will lead to printing of any problems at the
“warning” level or worse. See
Sec. `[sec:difxalertmessage] <#sec:difxalertmessage>`__ for details on
the severity levels.

cleanVDIF :math:`\mathrm{(package: vdifio)}` [sec:printVDIF]
------------------------------------------------------------

Program ``cleanVDIF`` loops through a VDIF file writing valid content to
a new output file.

-  Usage: ``cleanVDIF`` *inputvdiffile outputvdiffile Mbps* :math:`[`
   *options* :math:`]`

-  *inputvdiffile* is the recorded VDIF file to clean

-  *outputvdiffile* is the corrected VDIF file to write

-  *Mbps* is the data rate in megabits/second

-  *options* can be:

   -  ``-v`` or ``–verbose`` : be verbose in execution

-  Example: ``cleanVDIF bad.vdif good.vdif 256``

condition :math:`\mathrm{(package: nrao\_difx\_db)}`
----------------------------------------------------

This is an NRAO-only program owing to its ties to the VLBA database.

Program ``condition`` is mainly used to extract Mark5 module
conditioning reports from the database but also has the means to
manually import data into the database. When querying (with the ``find``
action), one or more “identifiers” can be supplied which can be either
the names of the Mark5 modules or serial number of individual disks (or
a mix of the two!). Environment variable ``VLBA_DB`` must be set to
point to the correct database.

-  Usage: ``condition`` :math:`[` *options* :math:`]` *action + args*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : be verbose in execution

-  *action* can be one of:

   -  add *report1* :math:`[`\ *report2* :math:`\cdots` :math:`]`

   -  find *identifier1* :math:`[`\ *identifier2* :math:`\cdots`
      :math:`]`

-  *report* is the name of a file containing one or more condition
   reports from ``SSErase``

-  *identifier* is either a Mark5 module VSN or a hard disk serial
   number

-  Example 1: ``condition add NRAO-040``

-  Example 2: ``condition find NRAO-042``

-  Example 3: ``condition find NRAO+342 NRAO+270``

-  Example 4: ``condition find Y66M3BQE``

condition_watch :math:`\mathrm{(package: nrao\_difx\_db)}`
----------------------------------------------------------

This is an NRAO-only program owing to its ties to the VLBA database.

Program ``condition_watch`` is meant to run as a background process on
the correlator head node. Its function is to receive
``Mark5ConditionMessage``\ s emitted by a special version of ``SSErase``
(the module conditioning program) and stuff this data into the database.
This program is automatically started by ``mk5daemon`` when it is
supplied with the ``-w`` or ``–condition-watch`` arguments. When
restarting ``mk5daemon`` by hand, make sure that a duplicate copy of
``condition_watch`` is not left running. Environment variable
``VLBA_DB`` must be set to point to the correct database.

-  Usage: ``condition_watch`` :math:`[` *options* :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

-  Example: ``condition_watch``

countVDIFpackets :math:`\mathrm{(package: vdifio)}` [sec:countVDIFpackets]
--------------------------------------------------------------------------

Program ``countVDIFpacket`` loops through a VDIF file and counts number
of valid and skipped frames. Packet counts are performed only on the
thread ID requested.

-  Usage: ``countVDIFpackets`` *vdiffile Mbps threadId*

-  *vdiffile* is the recorded VDIF file

-  *Mbps* is the data rate in megabits/second

-  *threadId* is the threadId to report on

-  Example: ``countVDIFpackets example.vdif 256 3``

.. _sec:cpumon:

cpumon :math:`\mathrm{(package: difxmessage)}`
----------------------------------------------

Program ``cpumon`` is a program that listens for ``difxLoad`` messages
multicast from the Mark5 units and displays the information; updating
the display as new messages are received.

-  Usage: ``cpumon``

Make sure the terminal is at least 60 characters wide and is at least as
tall as there are computers that may transmit information. To quit, use
ctrl-C. The columns displayed are:

#. Computer name

#. CPU load averaged over 10 seconds

#. Memory usage / Total memory

#. Network receive rate (Mbps)

#. Network transmit rate (Mbps)

#. Number of CPU cores

.. _sec:diffDiFX:

diffDiFX.py :math:`\mathrm{(package: vis2screen)}`
--------------------------------------------------

Program ``diffDiFX.py`` generates a context-sensitive difference of two
DiFX output files for detailed version testing. Corresponding visibility
records are differenced and statistics on the differences are
accumulated and printed at the end of the processing.

-  Usage: ``diffDiFX.py`` :math:`[` *options* :math:`]` :math:`\{`
   *difxfile1* *difxfile2*\ :math:`]` :math:`\}`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-f FREQ`` or ``–freq=FREQ`` : Only look at visibilities from
      this FREQ index

   -  ``-b BASELINE`` or ``–baseline=BASELINE`` : Only look at
      visibilities from this BASELINE num

   -  ``-t THRESHOLD`` or ``–threshold=THRESHOLD`` : Display any
      difference that exceeds THRESHOLD

   -  ``-e EPSILON`` or ``–epsilon=EPSILON`` : Display any differences
      that exceeds allowed numerical error EPSILON

   -  ``-s SKIPRECORDS`` or ``–skiprecords=SKIPRECORDS`` : Skip
      SKIPRECORDS records before starting comparison

   -  ``-m MAXRECORDS`` or ``–maxrecords=MAXRECORDS`` : Stop after
      comparing MAXRECORDS (if >0) records

   -  ``-p PRINTINTERVAL`` or ``–printinterval=PRINTINTERVAL`` : Print a
      summary every PRINTINTERVAL records

   -  ``-c MAXCHANNELS`` or ``–maxchannels=MAXCHANNELS`` : The length of
      the array that will be allocated to hold vis results

   -  ``-v`` or ``–verbose`` : Turn verbose printing on

   -  ``-i INPUTFILE`` or ``–inputfile=INPUTFILE`` : Parse INPUTFILE for
      the correlation setup

   -  ``–matchheaders`` : On seeing a header mismatch, skip through file
      2 looking for next match

-  *difxfile1* is the first difx file to compare

-  *difxfile2* is the second difx file to compare

-  | Example:
     ``difxDiFX.py -i example_1.input example_1.difx/DIFX_55523_025239.s0000.b0000``
   | ``comparison_1.difx/DIFX_55523_025239.s0000.b0000``

If the error for any record exceeds the specified threshold a verbose
error message is printed. Summary statistics are printed at the end of
the file. Warnings are printed if the headers do not match between the
two files.

.. _sec:difx2fits:

difx2fits
---------

Program ``difx2fits`` creates a FITS output file from the native output
format created by ``mpifxcorr`` and several other files carrying
information about the observation. Multiple input file sets can be
specified. A separate output FITS file is created for each unique
frequency setup encountered. When run, ``difx2fits`` requires the
following files to be present for each DiFX file set being converted:

#. *baseFilename*\ ``.difx/``

#. *baseFilename*\ ``.input``

#. *baseFilename*\ ``.calc``

#. *baseFilename*\ ``.im``

Several other files are optional and are typically used to populate
calibration and ancillary tables:

#. *baseFilename*\ ``.flag``

#. ``flags``

#. ``pcal``

#. ``tsys``

#. ``weather``

#. ``$GAIN_CURVE_PATH/``

#. ``.difx/*.history``

With the exception of the gain curve files, all the input files to
``difx2fits`` are expected to be in the current working directory or in
the place indicated by the ``.input`` file. As the visibility file
(``.difx``) is read, any records that are all zero are omitted.

-  Usage: ``difx2fits`` :math:`[` *options* :math:`]` :math:`\{` ``-d``
   :math:`\mid` *baseFilename1*
   :math:`[\cdots`\ *baseFilenameN*\ :math:`]`
   :math:`[`\ *outFile*\ :math:`]` :math:`\}`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-n`` or ``–no-model`` : don’t write model (ML) table

   -  ``-s`` *scale* or ``–scale`` *scale* : scale visibility data by
      *scale*

   -  ``-t`` *interval* or ``–deltat`` *interval* : generate
      ``.jobmatrix`` file with time intervals of length *interval*
      seconds

   -  ``–difx-tsys-interval`` *interval* : the Difx-derived tsys
      interval (sec) (default 30.0s averaging)

   -  ``–difx-pcal-interval`` *interval* : the Difx-derived pcal
      interval (sec) (default 30.0s averaging)

   -  ``-S`` or ``–sniff-all`` : sniff all bins and phase centers, not
      just the first

   -  ``-T`` *interval* or ``–sniff-time`` *interval* : use *interval*
      as the sniffer time resolution

   -  ``-v`` or ``–verbose`` : increase verbosity of output; use twice
      or thrice to get even more

   -  ``-d`` or ``–difx`` : run on all ``.difx`` files found in the
      directory

   -  ``-k`` or ``–keep-order`` : don’t sort the antennas by name

   -  ``-1`` or ``–dont-combine`` : make a separate FITS file for each
      input job

   -  ``-x`` or ``–dont-sniff`` : don’t generate sniffer output files

   -  ``-0`` or ``–zero`` : don’t put visibility data in FITS file

   -  ``–bin`` *b* : Select on this pulsar bin number

   -  ``–phasecentre`` *p* : (U.S. spelling okay too) Create a FITS file
      for all the *p*\ :math:`^{\rm th}` phase centers (default 0)

   -  ``–override-version`` : ignore difx version clashes

   -  ``–bandpass`` : write the ``.bandpass`` file (see
      Sec. `[sec:filebandpass] <#sec:filebandpass>`__)

   -  ``-m`` *nJob* or ``–max-jobs`` *nJob* : split into more FITS files
      after reaching *nJob* input files.

   -  ``–eop-merge-mode`` *mode* : sets conditions for allowing jobs
      with different EOPs to be merged or not; options are ``strict``
      (default), ``drop``, ``relaxed``

   -  ``–clock-merge-mode`` *mode* : sets conditions for allowing jobs
      with different clock models to be merged or not; options are
      ``strict`` (default) or ``drop``

   -  ``–antpol`` : use antenna-based polarization labels as in VEX.
      Note: fits-idi file will violate original specifications and abide
      extended specifications.

   -  ``–polxy2hv`` : re-labels all polarizations XY to HV. Requires
      –antpol option.

-  *baseFilename* is the prefix of the jobfile to convert; it is okay to
   use the ``.difx`` filename instead

-  *outFile* is the name of the ``FITS`` file to produce; if not
   provided one will be made based on the project code

-  Example 1: ``difx2fits dq109_1 DQ109.FITS``

-  Example 2: ``difx2fits -v -v -d``

Environment variables respected:

-  ``DIFX_GROUP_ID`` : if set, run difx2fits with ``umask(2)``.

-  ``DIFX_LABEL`` : the local name of the difx install. Used to verify
   matching versions and put inside FITS file; if not set,
   ``DIFX_VERSION`` will be used instead.

-  ``DIFX_MAX_SNIFFER_MEMORY`` : maximum amount mf memory (bytes) to
   allow sniffer to use.

-  ``DIFX_VERSION`` : the difxbuild version name.

-  ``GAIN_CURVE_PATH`` : directory containing gain files.

-  ``TCAL_PATH`` : a directory containing Tcal value files.

-  ``TCAL_FILE`` : if ``TCAL_PATH`` is not set, use the file pointed to
   by this env. var.

Unless adjusted with the ``–difx-pcal-interval`` and
``–difx-tsys-interval`` parameters, the respective PC and TY data will
be time averaged to the default of 30 seconds. For geodetic VLBI or
other observations with very short scans you may want to shorten the
averaging time.

Unless disabled with the ``–dont-sniff`` or ``-x`` flag, four “sniffer”
output files (``.acb``, ``.apd``, ``.wts`` and ``.xcb``) will be written
for each ``.FITS`` file produced. These files are used by ``difxsniff``
and its associated programs to produce data plots that are used to
assess data quality.

Unless disabled by setting *interval* to a non-positive number with the
``-t`` or ``–deltat`` option, an output file with suffix ``.jobmatrix``
will be produced. This file contains an ASCII art diagram of which jobs
contributed to each ``.FITS`` file produced as a function of both time
and antenna.

Unless augmented with option ``–antpol`` the produced FITS files are
fully compliant with the original FITS-IDI specifications. In case of
mixed mode polarization (XY against RL) or certain feed types (HV in
particular) the option ``–antpol`` allows to force output of
noncompliant but polarization correct FITS files; FITS-IDI numerical
parameter STK_1 is set to the new value of -9, indicating to
post-processing software that it should refer to the existing FITS-IDI
character parameters Antenna1Feed1 and Antenna2Feed1 (populated from
VEX) for the polarization details.

If submitting a bug report for ``difx2fits``, please include in it the
full output of ``difx2fits -v -v`` and the ``.input`` and ``.calc``
files.

``difx2fits`` displays several diagnostics during the conversion
process, separately for each output FITS file. The size of each FITS
table is printed; a zero size indicates that table is not produced. For
the visibility table, input files contributing to the output are
printed. Scan information is printed at increased verbosity levels. Not
all DiFX output visibilities are written to the FITS file. Accounting of
the disposition of the visibilities is provided. Invalid records are
those containing infinite or NaN values and indicate a likely bug in the
software. Flagged records are those identified by vex2script (or other
DiFX file set generation programs) in the ``.flag`` file as being
illogical, such as cases where a particular baseline during a job
belongs to a different subarray. When using integration times longer
than 1 second it is possible for one visibility to span two scans. Such
records are dropped. Finally any visibilities, produced outside a normal
scan start/stop time are dropped; this should not occur unless the
``.calc`` file is modified between correlation and FITS creation.

If there are any files matching ``.difx/*.history`` for ``.difx/``
output being converted to ``.FITS``, the contents of these files will be
inserted into the FITS HISTORY table.

.. _sec:difx2mark4:

difx2mark4
----------

Program ``difx2mark4`` creates a Mark4 output file set from
``mpifxcorr`` input and output files. When run, ``difx2mark4`` requires
the following files to be present for each file set being converted:

#. *baseFilename*\ ``.difx/``

#. *baseFilename*\ ``.input``

#. *baseFilename*\ ``.im``

as well as the ``.vex`` file referenced in the ``.input`` file, which
may be common to many DiFX file sets.

-  Usage: ``difx2mark4`` :math:`[` *options* :math:`]` *baseFilename1*
   :math:`[\cdots`\ *baseFilenameN*\ :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : increase verbosity of output; use twice
      or thrice to get even more

   -  ``-d`` or ``–difx`` : run on all ``.difx`` files found in the
      directory

   -  ``-k`` or ``–keep-order`` : don’t sort the antennas by name

   -  ``–override-version`` : ignore difx version clashes

   -  ``-r`` or ``–raw`` : suppresses normalization of amplitudes

   -  ``-p`` or ``–pretend`` : do a dry run

   -  ``-e`` or ``–experiment-number`` *n* : set the experiment number
      to *n* which must be a 4 digit number (default is 1234)

   -  ``-b`` *code* *flo* *fhi* : Override freq band codes. Frequencies
      are in MHz. Multiple parameters of this kind can be specified.

-  *baseFilename* is the prefix of the jobfile to convert *without* the
   underscore and job number

-  Example ``difx2mark4 dq109``

.. _sec:difxarch:

difxarch :math:`\mathrm{(package: nrao\_difx\_db)}`
---------------------------------------------------

Program ``difxarch`` is a simple script that moves ``FITS`` files
produced by ``makefits`` from the correlation queue staging area
(defined by the ``DIFX_QUEUE_BASE`` environment variable) to the archive
staging area (defined by environment variable ``DIFX_ARCHIVE_ROOT``). A
process running on the archive computer will periodically monitor new
files in this staging area and will then copy them to the actual
archive. In order to prevent premature pick-up of these files, they are
first moved into a directory with a name beginning with a period
(``.``). This directory is renamed without the period once all files to
be archived are copied.

-  Usage: ``difxarch`` :math:`[` *options* :math:`]` *passName1*
   :math:`[\cdots`\ *passNameN*\ :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : increase verbosity of output

   -  ``-p`` or ``–pretend`` : generate SQL and bash commands but don’t
      execute them

   -  ``–override-version`` ignore DiFX version clashes

-  *passName* is the name of a correlator pass; a file called
   *passName*\ ``.fitslist`` is expected to be present

-  Example: ``difxarch -v clock``

.. _sec:difxbuild:

difxbuild
---------

Program ``difxbuild`` aids in the installation of DiFX onto a cluster.
Full documentation on the install process can be found in
§\ `[sec:installdifxbuild] <#sec:installdifxbuild>`__, so details will
not be shown here. Command syntax is as follows:

-  Usage: ``difxbuild`` :math:`[` *options* :math:`]` *command*
   :math:`[` *command arguments* :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-d`` or ``–documentation`` : print full in-line documentation to
      screen

   -  ``-t`` ot ``–todo`` : print developer’s to-do list

   -  ``-v`` or ``–verbose`` : increase verbosity of output

   -  ``-q`` or ``–quiet`` : decrease output verbosity

   -  ``-p`` or ``–pretend`` : generate SQL and bash commands but don’t
      execute them

   -  ``-V`` or ``–version`` : print version and quit

-  *command* is one of the ``difxbuild`` commands, such as ``build`` or
   ``svn``; the program help information will list all options

-  *command arguments* are options for some commands

difxcalc11
----------

.. _sec:difxcalculator:

difxcalculator :math:`\mathrm{(package: difxio)}`
-------------------------------------------------

Program difxcalculator looks at a set of DiFX input files (``.input``,
``.calc``, etc.) and reports/calculates key operating parameters. This
program is inspired by the ``difx_calculator.xls`` spread sheet
available at
http://www.atnf.csiro.au/vlbi/dokuwiki/doku.php/difx/calculator.

-  Usage: ``difxcalculator`` :math:`[` *options* :math:`]` *baseName*
   :math:`[`\ *speedUp*\ :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

-  *baseName* is the base name of a correlator job

-  *speedUp* is the expected processing rate relative to real-time

-  Example: ``difxcalculator mt933_01``

Known bugs:

#. Does not take into consideration multiple phase centers or zoom
   bands.

.. _sec:difxclean:

difxclean :math:`\mathrm{(package: nrao\_difx\_db)}`
----------------------------------------------------

Program ``difxclean`` simply deletes all data from ``$DIFX_QUEUE/`` for
a particular project. It also removes all jobs with status not equal to
``COMPLETE`` for this project from the ``DIFXQUEUE`` table of the
database. It is intended that is be run at the same time the project is
released, meaning data has been correlated and archived. The user does
not have to be in any particular directory when running this program.

-  Usage: ``difxclean`` :math:`[` *options* :math:`]` *project*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-p`` or ``–pretend`` : don’t actually do the erasure

-  *project* is the name of the project to be “cleaned” out

-  Example: ``difxclean mt917``

.. _sec:difxcopy:

difxcopy :math:`\mathrm{(package: misc\_utils)}`
------------------------------------------------

Python program ``difxcopy`` is used to copy DiFX input (and other) files
to a different directory. In the process, explicit references to other
files that are being copied are changed to reflect their new file system
path. For a given file prefix, *prefix*, the following files are copied
if they exist: *prefix*\ ``.input``, *prefix*\ ``.calc``,
*prefix*\ ``.flag``, *prefix*\ ``.delay``, *prefix*\ ``.uvw``,
*prefix*\ ``.rate``, and *prefix*\ ``.im``. The ``.vex`` file referenced
within the ``.calc`` file is also copied.

-  Usage: ``difxcopy`` :math:`[` *options* :math:`]` *jobPrefix1*
   :math:`[\cdots`\ *jobPrefixN*\ :math:`]` *destDir*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : possibly increase verbosity of output

-  *jobPrefixN* is the file prefix of a job, e.g., ``mt911_04`` would be
   the prefix corresponding to input file ``mt911_04.input``

-  *destDir* is the directory in which the copied and modified files
   will be placed

-  Example:
   ``difxcopy mt911_02 mt911_03 mt911_04 /home/difx/queue/MT911``

.. _sec:difxdiagnosticmon:

difxdiagnosticmon :math:`\mathrm{(package: difxmessage)}`
---------------------------------------------------------

Program ``difxdiagnosticmon`` listens for multicast messages of the
``difxStatus`` variety and simply prints their contents to the terminal.
This is mainly useful for debugging ``mpifxcorr``. Diagnostic
information that is produced includes status of internal buffers, memory
usage, execution time, data throughput and lost subintegrations.

-  Usage: ``difxdiagnosticmon`` :math:`[` *options* :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

.. _sec:difxlogprogram:

difxlog :math:`\mathrm{(package: difxmessage)}`
-----------------------------------------------

Program ``difxlog`` can be used to collect DiFX multicast messages for a
particular correlator job and write them to a file. A new instance of
``difxlog`` must be started for each job being run. Normally this will
be done automatically if implemented in the particular deployment of
DiFX. Both ``startdifx`` and ``mk5daemon`` can start DiFX and will
instantiate a ``difxlog`` process as needed. Only messages of type
``DifxAlertMessage`` and ``DifxStatusMessage`` are collected and written
to the output file.

-  Usage: ``difxlog`` *jobIdentity* *outFile* :math:`[` *logLevel*
   *pidWatch* :math:`]`

-  *jobIdentity* : the name of the job being run (specifically, it
   should match the ``identifier`` field in the DifxMessage being sent).

-  *outFile* : the name of the output file containing log information.

-  *logLevel* : the minimim message severity to retain (see
   §\ `[sec:difxalertmessage] <#sec:difxalertmessage>`__).

-  *pidWatch* : the program id (in the Unix sense) of the mpifxcorr
   process running.

-  Example: ``difxlog mt911_04 mt911_04.difxlog 4 1243``

Unless a *pidWatch* value is specified, ``difxlog`` will run until
killed. If a *pidWatch* value is provided, ``difxlog`` will quit as soon
as that process stops running. The *loglevel* parameter can be used to
select the maximum severity level to write to the log. The possible
values and their meanings are:

+---+-------------+--------------------------------------------------+
| 0 | Fatal       | ``mpifxcorr`` cannot continue because of the     |
|   |             | noted problem                                    |
+---+-------------+--------------------------------------------------+
| 1 | Severe      | an internal error that should never happen       |
|   |             | happened (likely bug)                            |
+---+-------------+--------------------------------------------------+
| 2 | Error       | a problem was encountered in the data processing |
+---+-------------+--------------------------------------------------+
| 3 | Warning     | something suboptimal was noted                   |
+---+-------------+--------------------------------------------------+
| 4 | Informative | a note containing progress information           |
+---+-------------+--------------------------------------------------+
| 5 | Verbose     | more detailed progress information               |
+---+-------------+--------------------------------------------------+
| 6 | Debug       | values probably of use only to software          |
|   |             | developers                                       |
+---+-------------+--------------------------------------------------+

.. _sec:difxqueue:

difxqueue :math:`\mathrm{(package: nrao\_difx\_db)}`
----------------------------------------------------

This is an NRAO-only program owing to its ties to the VLBA database.

Python program ``difxqueue`` is a program used to maintain the DiFX
correlator queue. There are two main responsibilities in doing so:
copying or deleting files in the correlator queue directory (which is
project specific: ``$DIFX_QUEUE_BASE/``\ *projectName*) and maintaining
the database entries for each queued job. In the VLBA context, this
program is the main interface between the analysts and the correlator
operators. This program is mainly intended to work on one *job pass* at
a time rather than single jobs or whole projects. In some cases one job
pass could be one job, or it could be a whole project (or both), but in
many cases there will be multiple passes per project with possibly
multiple jobs per pass. It is possible for ``difxqueue`` to operate on
individual jobs when a list of job numbers is provided. The first
command line argument describes the action to perform. Each subsequent
argument is then context dependent; see the examples or run with the
``-h`` command line parameter to get a feel for the variety of options
allowed. Once a job has been correlated successfully, its status will be
COMPLETE. There is no need to ``del``\ ete a job from the queue once it
is complete. Doing so will require recorrelation if the results of that
job are still needed. Each job in the queue has a priority. The smaller
the priority, the lower the number. By default a queued job will have
priority 2.

-  Usage: ``difxqueue`` :math:`[` *options* :math:`]` *action* :math:`[`
   *args* :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-p`` *priority* or ``–priority`` *priority* : set the priority
      of jobs to *priority*

   -  ``-q`` *queuedir* or ``–queuedir`` *queuedir* : manually set the
      staging directory

   -  ``-v`` or ``–verbose`` : increase verbosity of output

   -  ``-d`` or ``–db-only`` : do not copy/delete/move files; operate
      only on database

   -  ``–override-version`` : ignore DiFX version clashes

-  *action* : the action to perform

   -  ``add`` : add job(s) to the queue, usually a whole pass at a time.
      Default priority is 2; use the ``-p`` option to set the priority
      if a different priority is required.

   -  Example 1: ``difxqueue add clock``

   -  Example 2: ``difxqueue add mt911 1 2 3 4``

   -  Example 3: ``difxqueue -p 3 add geodesy``

   -  ``bump`` : increase the priority of queued job(s)

   -  Example: ``difxqueue bump clock``

   -  ``del`` : remove job(s) from the queue

   -  Example 1: ``difxqueue del clock``

   -  Example 2: ``difxqueue del mt911 2 3``

   -  ``list`` : list all jobs within a pass

   -  Example: ``difxqueue list mt911``

   -  ``listall`` : list all incomplete jobs in the queue; note that
      this is not restricted even to any particular project. If one or
      more projects is specified, all jobs, complete or not, for those
      projects will be listed. If no segment code is appended to a
      project name, then all matching proposal codes will be listed.

   -  Example 1: ``difxqueue listall``

   -  Example 2: ``difxqueue listall BX123 BY321``

   -  Example 3: ``difxqueue listall BR138A``

   -  ``log`` : list all correlations that have happened for a given
      project. This simply searches the DIFXLOG database table and dumps
      it to the screen in a readable fashion.

   -  Example 1: ``difxqueue log BX123``

   -  ``prod`` : print production queue list, possibly sending to a file

   -  Example 1: ``difxqueue prod``

   -  Example 2: ``difxqueue prod queue.txt``

   -  ``set`` : set the status of queued job(s)

   -  Example 1: ``difxqueue set tc015d COMPLETE``

   -  Example 2: ``difxqueue set tc015d QUEUED 3 4``

   -  ``slide`` : decrease the priority of queued job(s)

   -  Example: ``difxqueue slide mt911 6``

-  *args* : *action* dependent arguments, usually a pass name and
   possible list of job numbers

Note that exept for the ``listall`` and ``prod`` actions, the current
working directory must contain the ``.joblist`` file for a project.

.. _sec:difxsniff:

difxsniff :math:`\mathrm{(package: SniffPlots)}`
------------------------------------------------

Program ``difxsniff`` is a reimplementation of the VLBA analysts’
program ``sniff.pd`` to be more appropriate for software correlation
where the sniffer data is generated at the same time as the FITS files.
It uses the same underlying set of plotting programs (``plotwt``,
``plotbp``, and ``plotapd``) as ``sniff.pd`` did. It should be run in a
project directory as it will create a subdirectory (if not existing
already) which by default is called ``sniffer/``\ *refant* within the
current directory. All files created by ``difxsniff`` will be placed in
this directory, overwriting existing files with the same filenames.
Unlike ``sniff.pd``, ``difxsniff`` is a purely non-interactive command
line program. Note that although ``.FITS`` files are provided to
``difxsniff``, it is the associated files ending in ``.apd``, ``.wts``,
``.acb`` and ``.xcb`` that are actually read.

-  Usage: ``difxsniff`` :math:`[` *options* :math:`]` *refants*
   *FITS*\ 1 :math:`[\ \cdots` *FITS*\ N :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

-  *refants* is a list reference antennas, separated by spaces

-  *FITS* is a FITS file created by ``difx2fits``; multiple FITS files
   can be specified together

-  Example 1: ``difxsniff LA *.FITS``

-  Example 2: ``difxsniff NL FD *.FITS``

.. _sec:difxspeed:

difxspeed :math:`\mathrm{(package: vex2difx)}`
----------------------------------------------

Program ``difxspeed`` does processing benchmarking, possibly over a
range of parameters, of DiFX. To ensure that data playback (reading from
files, Mark5 modules or network) are not limiting performance, the FAKE
mode of DiFX (see §\ `[sec:fake] <#sec:fake>`__) is used; thus the
output data are meaningless. ``difxspeed`` takes a ``.speed``
(§\ `[sec:speed] <#sec:speed>`__) file as input. This file contains
various parameters, many of which are identical to those in the ``.v2d``
(§\ `[sec:v2d] <#sec:v2d>`__) files. An important difference with the
parameters specified in ``.speed`` files is that multiple values can be
provided for many of the parameters. In the benchmarking process, a
separate run of DiFX for each combination of the supplied parameters is
performed. The first combination is run twice, with the first being
labeled a *dummy* run. This is because the timing of the first execution
can vary depending on recent usage of the correlator.

-  Usage: ``difxspeed`` :math:`[` *options* :math:`]` *inputFile*
   :math:`[` *numIterations* :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

-  *inputFile* is the ``.speed`` file describing the series of
   benchmarks to run

-  *numIterations* is the number of times to execute all test
   combinations

Each run of ``difxspeed`` will append a new column of data to a file
called *inputFile*\ ``.out`` ; if the file does not exist, a new file
will be created. Documentation of this output file format can be found
in §\ `[sec:speed.out] <#sec:speed.out>`__.

.. _sec:difxusage:

difxusage :math:`\mathrm{(package: nrao\_difx\_db)}`
----------------------------------------------------

This is an NRAO-only program owing to its ties to the VLBA database.

Program ``difxusage`` mines the VLBA database for correlator usage
statistics. Usage is as follows:

-  Usage: ``difxusage`` :math:`[` *options* :math:`]` *mjdStart*
   *mjdStop*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : be more verbose in execution

   -  ``-l`` or ``–list`` : print all matching jobs

   -  ``-a`` or ``–all`` : select jobs in all states

   -  ``-c`` or ``–complete`` : select only complete jobs (default)

   -  ``-k`` or ``–killed`` : select only killed jobs

   -  ``-u`` or ``–unknown`` : select only unknown jobs

-  *mjdStart* is the start time (in Modified Julian Days) to look for
   jobs

-  *mjdStop* is the stop time (in Modified Julian Days) to look for jobs

Note that environment variable ``VLBA_DB`` must be set to point to the
postgres database in question.

.. _sec:difxvmf:

difxvmf :math:`\mathrm{(package: calcif2)}`
-------------------------------------------

*Note: this is coming in DiFX 2.7 series…*

``difxvmf`` takes a DiFX fileset (including the ``.im`` file) and
modifies the wet and dry troposphere values based on the Vienna Mapping
Functions. This program retrieves the needed external data from
http://vmf.geo.tuwien.ac.at. The ``.im`` file will be replaced with an
updated version.

-  Usage: ``difxvmf`` :math:`[` *options* :math:`]` *filebase1*
   :math:`[` *filebase2* …\ :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : be more verbose in execution

   -  ``-w`` or ``–usewx`` : use metrology data from each site rather
      than defaults

-  *filebase*\ n is the ``.input`` file or prefix to be processed;
   multiple can be provided.

If ``–usewx`` is specified, files of the form
*project*\ ``.``\ *station*\ ``.weather`` will be looked for in the
local directory and used to supply metrology data, overriding defaults.

Environment variables:

-  ``DIFX_VMF_DATA`` : a writable directory for caching downloaded VMF
   data

-  ``DIFX_VERSION`` : to enforce DiFX version compatibility

.. _sec:difxwatch:

difxwatch :math:`\mathrm{(package: difxmessage)}`
-------------------------------------------------

Program ``difxwatch`` can be used to monitor progress of ongoing DiFX
jobs and kill jobs that appear to be hung.

-  Usage: ``difxwatch`` :math:`[` *options* :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``–version`` : show program’s version number and exit

   -  ``-i`` *idletime* or ``–idle-time`` *idletime* : maximum number of
      seconds a job is allowed to be idle before it is killed.

.. _sec:doi:

DiFX Operator Interface
-----------------------

The DiFX Operator Interface (DOI) is a java-based application to monitor
and control the correlation of DiFX jobs. Jobs to be correlated can be
selected with a file browser or retrieved with a database request. A
separate manual :raw-latex:`\cite{doi}` will be made available with
instructions for its use. The only specific detail that will be
mentioned here is the contorted path the starting of a job takes:

#. The job to run is selected.

#. The DOI determines which resources (Mark5 units and processor nodes)
   are required.

#. If the intended output file already exists, a dialog will ask the
   operator whether to overwrite this file or not.

#. The DOI allocates resources.

#. The DOI assembles a ``DifxStartMessage`` XML document and multicasts
   it with the correlator head node as the recipient.

#. The ``mk5daemon`` process running on the head node captures this
   message.

#. ``mk5daemon`` fork()s; the child process changes its userId to
   ``difx`` and spawns an ``mpirun`` process via ``ssh`` to ensure the
   proper environment variables are set.

#. The ``mpirun`` process starts a copy of ``mpifxcorr`` on each of the
   Mark5 units and processing nodes that is requested.

#. ``mk5daemon`` fork()s again; the child process changes its userId to
   ``difx`` and spawns a ``difxlog`` process via ``ssh`` to ensure the
   proper environment variables are set.

#. All processes continue until job end is reached or the job is killed.

#. When the first fork()ed ``mk5daemon`` process ends, the ``difxlog``
   process stops automatically, causing the second fork()ed process also
   to stop.

#. The DOI receives messages suggesting the job has ended and frees the
   allocated resources.

.. _sec:e2ecopy:

e2ecopy :math:`\mathrm{(package: nrao\_difx\_db)}`
--------------------------------------------------

Program ``e2ecopy`` copies files one directory to another, changing the
ownership to ``$DIFX_ARCH_USERNAME`` in the process. This program must
be setuid root; the person installing the program must run
``chmod +s e2ecopy`` after installation if ``make install`` is not run
by root. Normally this program is run by ``difxarch`` (see
§\ `1.16 <#sec:difxarch>`__). This is a VLBA-centric program, but could
be used by others.

-  Usage: ``e2ecopy`` :math:`[` *options* :math:`]` *fromDir* *toDir*
   *file1* :math:`[\ \cdots` *file*\ N :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : be more verbose

-  *fromDir* : the source directory of the file(s) to copy

-  *toDir* : the destination directory

-  *file* : a file to copy (multiple files may be provided)

Note: “e2e” is NRAO terminology for “End to End”, a philosophy of
providing user software covering the full project lifecycle from
proposal handling to archive access. In this particular case the name
arose due to the location of the archive staging area at NRAO.

.. _sec:errormon:

errormon :math:`\mathrm{(package: difxmessage)}`
------------------------------------------------

Program ``errormon`` listens for multicast messages of the ``difxError``
variety and simply prints their contents to the terminal. It is
effectively the same as ``difxlog`` except that log data is sent to
*stdout* rather than a systematically named file.

-  Usage: ``errormon`` :math:`[` options :math:`]`
   :math:`[`\ *maxSeverity*\ :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

-  *maxSeverity* : maximum severity level to display (default = 8)

See § `[sec:difxalertmessage] <#sec:difxalertmessage>`__ for a list of
severity codes. If no *maxSeverity* is provided, the default level of 8
will cause no selection to occur; all messages will be printed.

A similar program, ``errormon2``, does nearly the same thing, but
defaults to a less verbose output, and sends output to *stderr* rather
than *stdout* (so use with ``grep`` or other \*nix tools is more
cumbersome. It also writes its output to a log file.

See documentation for ``difxlog`` to see the list of alert levels.

extractSingleVDIFThread :math:`\mathrm{(package: vdifio)}`
----------------------------------------------------------

This program has been superceded by ``vmux`` (see
Sec. `1.105 <#sec:vmux>`__).

extractVDIFThreads :math:`\mathrm{(package: vdifio)}`
-----------------------------------------------------

This program has been superceded by ``vmux`` (see
Sec. `1.105 <#sec:vmux>`__).

.. _sec:fakemultivdif:

fakemultiVDIF :math:`\mathrm{(package: vdifio)}`
------------------------------------------------

Note: the input file for ``fakemultiVDIF`` must have a single thread or
unpredictable results will occur.

.. _sec:fileto5c:

fileto5c :math:`\mathrm{(package: mark5daemon)}`
------------------------------------------------

.. _sec:filtervdif:

filterVDIF :math:`\mathrm{(package: vdifio)}`
---------------------------------------------

.. _sec:generatevdif:

generateVDIF :math:`\mathrm{(package: vdifio)}`
-----------------------------------------------

.. _sec:genmachines:

genmachines :math:`\mathrm{(package: mpifxcorr)}`
-------------------------------------------------

Program ``genmachines`` uses the information in a ``.input`` file and a
file containing information about the members of the compute cluster
(such as the file pointed to by ``$DIFX_MACHINES``) to produce a
``.machines`` file (§\ `[sec:machines] <#sec:machines>`__) needed by
``mpifxcorr``. Note that ``genmachines`` is not intended to be run by
hand anymore as ``startdifx`` does this, if necessary. If playback
directly off Mark5 units is to be done, ``genmachines`` will send a
multicast request to all Mark5 units on the correlator requesting an
inventory of loaded Mark5 modules. The ``mk5daemon`` process on each
unit will respond with another multicast message containing the loaded
modules and the status of the unit, i.e., whether busy or available to
be used. This information is collected by ``genmachines`` which will
look for availability of all the modules and detect conflicts (i.e., two
needed modules loaded in the same unit). If all needed modules are found
and enough resources remain for the computations, a ``.machines`` file
and a ``.threads`` file are written. Note that the ``.machines`` file
contains a certain number of comment lines so that the use of Unix
command ``wc -l`` can be used to determine exactly how many processes
will be started. It is suggested to run this program immediately before
starting the software correlator to minimize the chance that the Mark5
units change their status or that information about the modules
whereabouts becomes stale; it is thus discouraged to run with
``.input``.

-  Usage: ``genmachines`` :math:`[` *options* :math:`]` *input*\ 1
   :math:`[\ \cdots` *input*\ N :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : be more verbose

   -  ``-o`` or ``–overheadcores`` *ohc* : leave at least *ohc* on each
      compute node unscheduled

   -  ``-m`` *file* or ``–machinesfile`` *file* : use *file* instead of
      ``$DIFX_MACHINES``

   -  ``-n`` or ``–no-threads`` : don’t write a ``.threads`` file.

   -  ``-d`` or ``–difxdb`` : lookup module locations in a database.

-  *input* is a ``.input`` file; multiple files can be specified, each
   producing its own ``.machinesfile``

.. _sec:getshelf:

getshelf :math:`\mathrm{(package: nrao\_difx\_db)}`
---------------------------------------------------

This is an NRAO-only program owing to its ties to the VLBA database.

Program ``getshelf`` retrieves the shelf location of specified modules
from the legacy VLBA database and prints them to the screen. While
possibly useful, this program is not required for the software
correlation process.

-  Usage 1: ``getshelf`` :math:`[` *options* :math:`]` *module1*
   :math:`[` *module2* :math:`[ \cdots ] ]`

-  Usage 2: ``getshelf`` :math:`[`\ *options*\ :math:`]` *shelfFile*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : print the database query string as well

-  *moduleN* is the volume serial number (VSN) of a module; multiple
   modules can be specified

-  *shelfFile* is a ``.shelf`` file (§\ `[sec:shelf] <#sec:shelf>`__),
   as may be written by ``db2vex`` Default is the current working
   directory if none is provided.

-  Example 1: ``getshelf NRAO+267``

-  Example 2: ``getshelf bx123a.skd.shelf``

.. _sec:jobdisks:

jobdisks :math:`\mathrm{(package: mpifxcorr)}`
----------------------------------------------

Program ``jobdisks`` looks through job files to see which modules
(disks) are needed for correlation. It reads through ``.input`` files,
as used by ``mpifxcorr``, to get the needed information. There are two
modes of operation. By default, a matrix of all modules for all stations
is displayed, with a ``–`` symbol indicating that a particular station
is not used in a particular job. An asterisk (````) indicates a module
change. The second mode, instigated with command line argument ``-c``,
summarizes only module changes. Running without any arguments will cause
``jobdisks`` to look at job files within the current directory,
prioritizing on ``.input`` files if any exist and falling back on
``.fx`` files otherwise. Listings for a subset of jobs can be made by
specifying particular files.

-  Usage: ``jobdisks`` :math:`[` *options* :math:`]`
   :math:`[`\ *file*\ 1\ :math:`] \cdots [` *file*\ N :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-c`` or ``–changes`` : print module changes only

-  *file* is a ``.fx`` or ``.input`` file; mixed types are not
   supported. Multiple input files may be supplied.

-  Example 1: ``jobdisks``

-  Example 2: ``jobdisks job1420*.input``

-  Example 3: ``jobdisks *.fx``

-  Example 4: ``jobdisks -c``

Known bugs:

#. Program should make sure datastream type is MODULE.

.. _sec:joblist:

joblist :math:`\mathrm{(package: mpifxcorr)}`
---------------------------------------------

Program ``joblist`` prints useful information about DiFX correlator jobs
to *stdout*. Six columns of output are produced:

#. Job file base filename

#. File indicator, showing a particular character for each one of the
   files associated with that job that is found within a pair of square
   brackets, :math:`[\ ]`:

   -  ``.calc`` file (§\ `[sec:calc] <#sec:calc>`__)

   -  ``.machines`` file (§\ `[sec:machines] <#sec:machines>`__)

   -  ``.threads`` file (§\ `[sec:threads] <#sec:threads>`__)

   -  ``.im`` file (§\ `[sec:im] <#sec:im>`__)

   -  ``.difx`` file (§\ `[sec:difx] <#sec:difx>`__)

#. Band code of first scan in file

#. Observation duration of correlation (in minutes)

#. Recording mode triplet; three integers(data rate(Mbps), number of
   baseband channels & quantization bits) separated by dashes

#. Comma separated list of antennas

One line is printed for each ``.input`` file found in the list of
directories provided (or current directory if not listed).

-  Usage: ``joblist`` :math:`[` *options* :math:`]` :math:`[` *dir*\ 1
   :math:`] \cdots [` *dir*\ N :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

-  *dir* is a directory for which to print job information (default is
   current shell directory). Multiple directories can be specified.

-  Example 1: ``joblist``

-  Example 2: ``joblist $JOB_ROOT/*``

.. _sec:jobstatus:

jobstatus :math:`\mathrm{(package: mpifxcorr)}`
-----------------------------------------------

**Warning: As of DiFX 2.0.1, this utility has not yet been updated to
work with DiFX 2 output format.**

Program jobstatus lists the current correlation progress for each DiFX
job in one or more directories. This program is normally run without any
command line arguments from within the project directory. For each job,
the base filename is listed with 5 or 6 additional columns of data.
These columns are

#. Observation duration (minutes)

#. Record mode triplet (*Mpbs-nChan-nBit*)

#. Number of stations in job

#. Speed up factor (ratio of correlation time to observe time), or zero
   if correlation has not yet begun.

#. Percentage complete

#. Number of minutes remaining (only if Percentage complete isn’t 0% or
   100%)

Below these lines, five more lines containing information about the
group of jobs as a whole is are presented. The contents of these lines
are:

#. Total job time : Minutes of observe time in listed jobs

#. Fraction complete : Percentage in time through the entire project

#. Job time remaining : Minutes of observation left to be correlated

#. Wall time remaining : Minutes of real time needed to complete jobs

#. Average speedup : Ratio of total correlation time to run time, up to
   current point

Note that the speedup and time remaining values are estimates and don’t
include model calculation, conversion to FITS, and job startup time.

-  Usage: ``jobstatus`` :math:`[` *options* :math:`]` :math:`[` *dir*\ 1
   :math:`] \cdots [` *dir*\ N :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

-  *dir* is a directory for which to print job information (default is
   current shell directory). Multiple directories can be specified.

-  Example 1: ``jobstatus``

-  Example 2: ``jobstatus $JOB_ROOT/*``

Known bugs:

#. This program has not been updated to work with DiFX 2.0 output

.. _sec:listcpus:

listcpus :math:`\mathrm{(package: mk5daemon)}`
----------------------------------------------

Python program ``listcpus`` uses ssh to connect to each machine listed
in a file (usually ``$DIFX_MACHINES``) and peaks at the list of CPUs on
that machine and prints to stdout. Only the first column of this file is
used and any content after a ``#`` is ignored. For each CPU on the
machine, the model name, which usually also contains the CPU speed, is
listed. For multi-core CPUs, each core will appear as its own CPU.

-  Usage: ``listcpus`` :math:`[` *options* :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : increase output verbosity

   -  ``-m`` *file* or ``–machines`` *file* : use *file* instead of
      ``$DIFX_MACHINES`` for list of machines to probe

   Multiple directories can be specified.

-  Example 1: ``listcpus``

-  Example 2: ``listcpus -m myCPUs.list``

.. _sec:makefits:

makefits :math:`\mathrm{(package: difx2fits)}`
----------------------------------------------

This program is tuned for NRAO use; some modifications may be required
for use at other sites. In particular, this program requires that jobs
were queued to be run in ``$DIFX_QUEUE_BASE/``/*experiment*\ ``/``.

Program ``makefits`` is basically a wrapper for ``difx2fits``
(§\ `1.14 <#sec:difx2fits>`__) that does some sanity checking and
ensures that files end up in the proper places with the proper names.
This program is intrinsically *pass-based* and it bases its
functionality on the ``.joblist``
(§\ `[sec:joblistfile] <#sec:joblistfile>`__) file that is written by
``vex2difx`` (§\ `1.102 <#sec:vex2difx>`__). One must run this program
on the software correlator head node (``swc000`` in the current VLBA
DiFX implementation). Upon successful completion, FITS-IDI files are
created in the same directory in the correlator job staging area
(``$DIFX_QUEUE_BASE/``\ *projectName*) and the sniffer output files are
left in a subdirectory of the current working directory. An additional
output file is left in the current working directory called
*passName*\ ``.fitslist`` . This file has a list of the FITS files that
are to be archived once the data for this pass are deemed valid.

The checks that ``makefits`` performs will by default not allow an
incomplete set of FITS files to be produced. This can be overridden with
a special command line argument (below). This is part of an
accountability chain that aims to ensure that nothing gets omitted.

-  Usage: ``makefits`` :math:`[` *options* :math:`]` *passName*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : increase output verbosity

   -  ``–override-version`` : ignore potential difx version conflicts

   -  ``–allow-partial`` : bypass check for complete set of correlated
      output and proceed

   Multiple directories can be specified.

-  Example: ``makefits clock``

.. _sec:makemark4:

makemark4 :math:`\mathrm{(package: difxdb)}`
--------------------------------------------

This program is tuned for NRAO use; some modifications may be required
for use at other sites.

Program ``makemark4`` is essentially a wrapper for ``difx2mark4``
(§\ `1.15 <#sec:difx2mark4>`__) that does some sanity checking and
ensures that files end up in the proper places with the proper names.
This program is intrinsically *pass-based* and it bases its
functionality on the ``.joblist``
(§\ `[sec:joblistfile] <#sec:joblistfile>`__) file that is written by
``vex2difx`` (§\ `1.102 <#sec:vex2difx>`__). One must run this program
on the software correlator head node (``swc000`` in the current VLBA
DiFX implementation). Upon successful completion, Mark4 file sets are
created in the same directory in the correlator job staging area
(``$DIFX_QUEUE_BASE/``\ *projectName*). An additional output file is
left in the current working directory called *passName*\ ``.mark4list``
. This file has a list of the Mark4 file sets that are to be archived
once the data for this pass are deemed valid.

-  Usage: ``makemark4`` :math:`[` *options* :math:`]` *passName*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : increase output verbosity

-  Example: ``makemark4 rdv95``

.. _sec:m5bstate:

m5bstate :math:`\mathrm{(package: mark5access)}`
------------------------------------------------

Program ``m5bstate`` will perform state counts on a baseband data file.

-  Usage: ``m5bstate`` *file* *format* *nFrames* :math:`[` *offset*
   :math:`]`

-  *file* is the file to decode

-  *format* is the format of the data

-  *nFrames* is the number of data frames (typically a few kB in size)
   to decode

-  *offset* is the number of bytes into the file to start decoding

-  Example: ``m5bstate sample.vlba VLBA1_2-256-8-2 100``

Notes:

#. See documentation for ``m5b`` for details on specifying the data
   format.

#. Only real-sampled data with 1 or 2 bits per sample is supported at
   this time.

#. In the case of VDIF data, only single thread data with :math:`2^n`
   channels is supported. For equivalent functionality in the
   multi-thread VDIF case see ``vdifd``.

.. _sec:m5d:

m5d :math:`\mathrm{(package: mark5access)}`
-------------------------------------------

Program ``m5d`` is a very simple example program using the mark5access
decoding library. It turns out to be useful enough as a stand-alone
program to be separately documented. This program takes as command line
input the name of a file containing (or thought to be containing) VLBI
baseband data, the expected format of the data, and the number of
samples per baseband to decode. Optionally a starting file offset can be
supplied. If the data can be decoded correctly, information about the
data will be printed to the screen along with a table of decoded data.
The output values, -3, -1, 1, or 3 for valid data, are printed in
*nchan* columns. Data that cannot be decoded (either due to data
replacement headers, data fill pattern replacing the actual data after
unloading from a Mark5 module, or identified via the VDIF invalid bit)
will show as 0. It should be invoked with the following parameters:

-  Usage: ``m5d`` *file* *format* *n* :math:`[` *offset* :math:`]`

-  *file* is the file to decode

-  *format* is the format of the data

-  *n* is the number of samples to decode

-  *offset* is the number of bytes into the file to start decoding

-  Example 1: ``m5d sample.vlba VLBA1_2-256-8-2 24``

-  Example 2: ``m5d sample.mk4 MKIV1_4-128-2-1 600 200``

-  Example 3: ``m5d sample.5b Mark5B-512-16-2 1200``

The format parameter is constructed from four parts as
*type*-*rate*-*nchan*-*nbit* where:

-  *type* is the type of format and should be one of ``VLBA1_1``,
   ``VLBA1_2``, ``VLBA1_4``, ``MKIV1_1``, ``MKIV1_2``, ``MKIV1_4``,
   ``Mark5B``, or ``VDIF``

-  *rate* is the data rate in Mbps

-  *nchan* is the number of baseband channels

-  *nbit* is the number of bits per recorded sample

See the usage examples above for some explicit values. Note for the
``VLBA`` and ``MKIV`` format types the fanout is appended as this
affects the decodability of the files.

Notes:

#. In the case of VDIF data, only single thread data with :math:`2^n`
   channels is supported. For equivalent functionality in the
   multi-thread VDIF case see ``vdifd``.

.. _sec:m5findformat:

m5findformats :math:`\mathrm{(package: mark5access)}`
-----------------------------------------------------

Program ``m5findformats`` attempts to determine which format a baseband
data file may be. Currrently it searches over 16 to 2048 Mbps data rates
in factors of 2 and checks only for MKIV, VLBA and Mark5B types.

-  Usage: ``m5findformats`` *filename*

-  *filename* is the name of the baseband data file.

Run with no command line arguments to get help information.

.. _sec:m5fold:

m5fold :math:`\mathrm{(package: mark5access)}`
----------------------------------------------

Program ``m5fold`` takes a baseband data stream and integrates the power
(formed by squaring the voltage) in a number of time bins that equally
divide a given period. This is a simplifed version of “folding” such as
is used in pulsar processing. A typical use of such functionality would
be to investigate the waveform of the switched power injected into the
receiver for calibration. This program has found considerable utility in
determining time offsets between the sample clock and formatter time
(modulo the period of the calibration cycle). In the case of 2-bit
sampling a non-linear correction is applied before results are written
to a file. This correction takes the form

.. math:: P = \frac{1}{\left({\rm erf}^{-1}\left(\frac{\hat{P} - v_{\rm high}^2}{1 - v_{\rm high}^2} \right)\right)^2},

where :math:`P` is a value proportional to true power and
:math:`\hat{P}` is the value obtained by calculating
:math:`\left<\hat{v}^2\right>` when the bitstream is reproduced with
values :math:`\hat{v} \in ( -v_{\rm high}, -1, 1, v_{\rm high} )`. This
non-linear correction can be turned off by setting *nbin* to a negative
value. Note that this program is not useful for 1-bit quantized data.
The program should be used as follows:

-  Usage: ``m5fold`` *infile* *format* *nbin* *nchunk* *freq* *outfile*
   :math:`[` *offset* :math:`]`

-  *infile* is the file to decode

-  *format* is the format of the data‘

-  *nbin* is the number of bins to calculate per period; if negative,
   power correction is not performed and the absolute value of *nbin* is
   used

-  *nchunk* is the number of 10000 sample chunks to operate on

-  *freq* is the reciprocal of the period to be observed (Hz)

-  *outfile* is the name of the output file

-  *offset* (optional) is the number of bytes into the file to start
   decoding

-  Example:
   ``m5fold sample.vlba VLBA1_2-256-8-2 128 10000 80 sample.fold``

See the documentation for ``m5d`` for information on specifying the data
format.

The output file will contain *nchan*\ +1 columns where *nchan* is the
number of baseband channels in the data stream. The first column
contains the time (seconds) within the period. Each remaining column is
folded power for one baseband channel. If *nbin* is positive and the
data is 2-bit quantized, the scaling is such that
:math:`\left<v^2\right> = \sigma^2` yields a power reading of 1.0, for
sampler threshold :math:`\sigma`. Optimal signal to noise ratio occurs
for a value of about 1.03. For non 2-bit quantization, the power will be
in units of reconstituted :math:`{\rm counts}^2`.

In the case of VDIF data, only single thread data with :math:`2^n`
channels is supported. For equivalent functionality in the multi-thread
VDIF case see ``vdiffold``.

.. _sec:m5pcal:

m5pcal :math:`\mathrm{(package: mark5access)}`
----------------------------------------------

Program ``m5pcal`` can be used to extract pulse cal tones from baseband
data in Mark4, VLBA, Mark5B and single-thread VDIF formats.

-  Usage: ``m5pcal`` :math:`[` *options* :math:`]` *infile* *format*
   *freq1* :math:`[` *freq2* :math:`[ \ldots ] ]` *outfile*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-c`` *n* or ``–chunksize`` *n* : use a fixed rather than
      automatic chunk size

   -  ``-v`` or ``–verbose`` : increase output verbosity

   -  ``-q`` or ``–quiet`` : decrease output verbosity

   -  ``-n`` *n* : loop over *n* chunks of data (default is 1000)

   -  ``-N`` *N* : perform *N* outer loops, each yielding a result set

   -  ``-o`` *o* or ``–offset`` *o* : jump *o* bytes into file

   -  ``-i`` *i* or ``–interval`` *i* : use pulse cal comb interval of
      *i* MHz (default is 1)

   -  ``-e`` *e* or ``–edge`` *e* : don’t use channels closer than *e*
      MHz from band edges when computing delay (default is 1/8 of
      bandwidth)

-  *infile* is the file to decode

-  *format* is the format of the data‘

-  *freq1* …is/are the frequencies (MHz) relative to baseband of the
   first tone to detect; there should be one *freq* specified per
   baseband channel

-  *outfile* is the name of the output file

Note: The position of the first tone in a baseband channel (*freq1* for
baseband 1, and so on) must not be larger than the tone interval (set
with ``-i`` *i*). All tones are extracted from each baseband channel.
The tone interval is allowed to exceed the bandwidth of a baseband
channel in which case *freqN* will effectively select just a single tone
from the baseband.

See the documentation for ``m5d`` for information on specifying the data
format.

.. _sec:m5slice:

m5slice :math:`\mathrm{(package: mark5access)}`
-----------------------------------------------

.. _sec:m5spec:

m5spec :math:`\mathrm{(package: mark5access)}`
----------------------------------------------

Program ``m5spec`` is an example program using the mark5access decoding
library that is a bit more advanced than the ``m5d`` program is. It
forms total power spectra for each baseband channel in the data,
including cross spectra for polarization pairs, assuming data is in
alternating polarization pairs (if not, the cross spectra should make no
sense, but they are formed anyway). The results are written to a text
file with the following columns: Column 1 is the frequency offset from
baseband for each channel; Columns 2 to *nchan*\ +1 are the total power
spectra for each baseband channel; Columns *nchan*\ +2 to
:math:`4 \times`\ *nchan*\ +1 contain, in pairs, the amplitude and phase
of the cross spectra for each pair of channels. It should be invoked
with the following parameters:

-  Usage: ``m5spec`` *infile* *format* *npoint* *n* *outfile* :math:`[`
   *offset* :math:`]`

-  *infile* is the file to decode

-  *format* is the format of the data

-  *npoint* is the number of points to calculate for each spectrum

-  *n* is the number of FFT frames to include in the calculation

-  *outfile* is the name of the output file

-  *offset* (optional) is the number of bytes into the file to start
   decoding

-  Example: ``m5spec sample.vlba VLBA1_2-256-8-2 256 1000 vlba.spec``

See the documentation for ``m5d`` for information on specifying the data
format.

In the case of VDIF data, only single thread data with :math:`2^n`
channels is supported. For equivalent functionality in the multi-thread
VDIF case see ``vdifspec``.

.. _sec:m5test:

m5test :math:`\mathrm{(package: mark5access)}`
----------------------------------------------

Program ``m5test`` is an example program using the mark5access decoding
library that works its way through a VLBI baseband data stream
attempting to decode data and header information to look for problems.
Every million samples (per baseband channel) a summary line containing
frame number, decoded date and time, and counts of valid and invalid
frames are shown. After 20 invalid frames are encountered the program
will stop. Otherwise the program will run until end of file or until
interrupted by the user. Usage is as follows:

-  Usage: ``m5test`` *infile* *format* :math:`[` *offset* :math:`]`

-  *infile* is the file to decode

-  *format* is the format of the data

-  *offset* (optional) is the number of bytes into the file to start
   decoding

-  Example: ``m5test sample.vlba VLBA1_2-256-8-2``

See the documentation for ``m5d`` for information on specifying the data
format.

.. _sec:m5time:

m5time\ :math:`\mathrm{(package: mark5access)}`
-----------------------------------------------

Program ``m5time`` decodes the time of the beginning of a Mark4, VLBA,
or Mark5B datastream and prints the result in integer MJD and UT hours,
minutes, seconds to the screen.

-  Usage: ``m5time`` *infile* *format*

-  *infile* is the file to decode

-  *format* is the format of the data‘

.. _sec:m5timeseries:

m5timeseries :math:`\mathrm{(package: mark5access)}`
----------------------------------------------------

Program ``m5timeseries`` produces a power measurments for each of
channel of a baseband data file, averaging over a specified time
interval.

-  Usage: ``m5timeseries`` *infile* *format* *tint* *ntime* *outfile*
   :math:`[` *offset* :math:`]`

-  *infile* is the file to decode

-  *format* is the format of the data‘

-  *tint* is the integration time per sample in milliseconds

-  *ntime* is the number of samples to generate

-  *outfile* is the name of the output file

-  *offset* (optional) is the number of bytes into the file to start
   decoding

-  Example:
   ``m5timeseries sample.vlba VLBA1_2-256-8-2 6.25 8000 sample.series``

The output file contains *nchan*\ +2 columns of data where *nchan* is
the number of channels in the data file. The first column is sample
number. The second column is time since beginning of series, in seconds.
The remaining columns are power measurements for the channels.

.. _sec:tsysal:

m5tsys :math:`\mathrm{(package: mark5access)}`
----------------------------------------------

.. _sec:mk5cat:

mk5cat :math:`\mathrm{(package: mk5daemon)}`
--------------------------------------------

This program sends data on a module to standard out. See additional
documentation under ``mk5cp`` which operates on similar principles
(mk5cat is mk5cp writing to *stdout* Note that the executable for mk5cat
is identical to that for mk5cp and only the name of the program actually
differs.

-  Usage: ``mk5cat`` :math:`[` *options* :math:`]` { *bank* :math:`\mid`
   *VSN* } *scans*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : increase verbosity, e.g., print directory
      to screen

-  *bank* is either ``A`` or ``B``

-  *VSN* is a valid 8-character VSN of a loaded module

-  *scans* is one or more scan numbers (starting at 1) with scan numbers
   separated by commas.

Many of the other baseband data utilities documented here such as
``m5d``, ``m5spec`` and ``vmux`` can take input from *stdin* and thus
can be mated with ``mk5cat``. Usually a single hyphen (``-``) as the
name of the input file indicates this to these programs.

-  Example:
   ``mk5cat B PT_BB241_No0111 | m5spec - Mark5B-2048-16-2 128 10000 methanol.spec``

.. _sec:mk5control:

mk5control :math:`\mathrm{(package: mk5daemon)}`
------------------------------------------------

``mk5control`` is a program that sends XML messages of type
``DifxCommand`` to the ``mk5daemon`` programs that run on the software
correlator cluster members. This program is a superset of ``mk5take``
and ``mk5return``, allowing any allowed command to be sent.

-  Usage: ``mk5control`` :math:`[` *options* :math:`]` *command*
   *unit*\ 1 :math:`\cdots` *unit*\ N

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit.

-  *command* is the (non-case-sensitive) command to be executed; see
   list below.

-  *unit* is the number of a correlator Mark5 unit, a range, ``all`` for
   all software correlator cluster members, ``mark5`` for all Mark5
   units, or ``swc`` for all software correlator compute nodes.

-  Example 1: ``mk5control stopmark5a 07 08 09 11 14``

-  Example 2: ``mk5control resetmark5 14-24``

-  Example 3: ``mk5control startmark5a mark5``

The list of supported *command* types is below. All commands are not
case sensitive.

-  ``GetVSN`` Request a ``Mark5Status`` XML document to be multicast
   from the *unit*

-  ``ResetMark5`` Execute ``SSReset`` and ``ssopen``; this cures
   many/most mark5 hangs

-  ``Clear`` Clear the stat of the Mark5 unit and get the VSNs, can be
   dangerous if other programs are currently using the Streamstor card

-  ``Reboot`` Reboot the machine

-  ``Poweroff`` Shut down the machine

-  ``StopMk5Daemon`` Stop the ``mk5daemon`` program; you probably never
   need to do this

-  ``GetDir`` Extract the directory from the modules in both banks and
   save to files in $MARK5_DIR_PATH

-  ``GetDirA`` Same as above, but look only at bank A

-  ``GetDirB`` Same as above, but look only at bank B

-  ``StopDir`` Stop a directory read that is in progress

-  ``KillMpifxcorr`` Send sigkill (like ``kill -9``) to all processes on
   machine called mpifxcorr

-  ``Copy`` Copy data from a module to files in a provided directory. At
   least three parameters must be provided that match the parameters of
   ``mk5cp``. Because of the way mk5control parses the command line, the
   word ``copy`` and the parameters must all be enclosed in quotes.

-  ``StopCopy`` Stop a data copy process

-  ``GetVer`` Request send of a ``DifxMessageMk5Version`` XML message

-  ``mount``\ *XX* Cause Linux device ``/dev/sd``\ *XX* to be mounted on
   ``/mnt/usb``

-  ``umount`` Cause ``/mnt/usb to be unmounted``

-  ``Test`` Used in debugging — for developers only

.. _sec:mk5cp:

mk5cp :math:`\mathrm{(package: mk5daemon)}`
-------------------------------------------

Program ``mk5cp`` copies baseband VLBI data from a module to a file
somewhere on the operating system filesystem, perhaps an external USB
disk. This program is often started using ``mk5control`` to tell the
instance of ``mk5daemon`` running on the desired Mark5 unit to start the
copy. Status information is multicast via a ``Mark5StatusMessage``
document.

-  Usage: ``mk5cp`` :math:`[` *options* :math:`]` { *bank* :math:`\mid`
   *VSN* } *scans* *outputDirectory*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : increase verbosity: print directory to
      screen

-  *bank* is either ``A`` or ``B``

-  *VSN* is a valid 8-character VSN of a loaded module

-  *scans* is one or more scan numbers (starting at 1) with scan numbers
   separated by commas. No spaces are allowed in the list. A range can
   be specified with a hyphen (see examples). Alternatively, a scan
   name, or portion thereof, can be specified. When a partial scan is
   provided, any scan name matching that partial scan name will be
   copied. The scans parameter can also take either a time range (two
   floating point modified Julian Days connected with an underscore, or
   a byte range (must be a multiple of 4) via two integers separated by
   an underscore, or a byte start and a length can be specified with two
   integers separated by a plus sign.

-  *outputDirectory* is the directory to which files will be copied.
   Make sure the destination directory exists before running this
   program and make sure sufficient free space remains on that
   filesystem. If the *outputDirectory* parameter is set to ``-`` (the
   hyphen), data will go to stdout. Another utility called ``mk5cat`` is
   derived from this behavior.

-  Example 1: ``mk5cp NRAO-123 4 /mnt/usb/WaltersProject``

-  Example 2: ``mk5cp A 1,2,3 /tmp``

-  Example 3: ``mk5cp B BC120A /mnt/usb/BC120A/PT``

-  Example 4: ``mk5cp NRAO+255 6-12 /tmp``

-  Example 5: ``mk5cp NRAO+255 123123100_124123100 /tmp``

-  Example 6: ``mk5cp NRAO+255 54327.13124_54327.15124 /tmp``

-  Example 7: ``mk5cp NRAO+322 123123100+1000000 /tmp``

.. _sec:mk5daemon:

mk5daemon :math:`\mathrm{(package: mk5daemon)}`
-----------------------------------------------

``mk5daemon`` is a program that started automatically at boot time on
all of the software correlator cluster nodes (not only the Mark5 units!)
that performs a number of operations in support of the software
correlator.

The functions that mk5daemon performs are:

-  **Logging**

   All received multicast messages, significant internal functions, and
   interactions of the ``Mark5A`` program are logged to human readable
   log files. These log files are restarted at the beginning of each
   day. By default these log files are saved in ``/tmp``.

-  **High level control of Mark5 units**

   The ``Mark5A`` program (written by Haystack) is the principle program
   used to access the Mark5 systems at the VLBA stations and the
   hardware correlator. DiFX directly accesses the Streamstor card via a
   library level programming interface. Since only one program is
   allowed to do this (or face a crash of varying degree of
   seriousness), access to the Streamstor card must be carefully
   managed. One function of ``mk5daemon`` is to maintain knowledge of
   who “owns” the Streamstor card at a given time to prevent conflicts.
   The starting and stopping of the ``Mark5A`` program can be requested
   by two messages of type ``DifxCommand`` : ``startmark5a`` and
   ``stopmark5a``. When these commands are received by ``mk5daemon``,
   the requested action is taken unless Streamstor conflict is likely.
   This type of command and others can be sent to ``mk5daemon`` with the
   ``mk5control`` program (§\ `1.59 <#sec:mk5control>`__).

-  **Low level control of Mark5C units**

   This program exposes a VLBI Standard Interface (VSI) over TCP port
   2620 that very closely emulates equivalent functionality of the
   Mark5C Data Recording System (DRS) program provided by Haystack
   observatory. The implementation of the DRS command set is not
   complete but is sufficient for monitor and control at record time. At
   the time of writing this program is used in lieu of DRS at the two
   Mark5C units provided by USNO.

-  **CPU, memory, and network monitoring**

   Every 10 seconds, ``mk5daemon`` extracts data from the ``/proc``
   directory to get information about the CPU load, memory usage, and
   network traffic. These numbers are multicast in a ``DifxLoad``
   message and logged.

-  **Module VSN and state determination**

   Receipt of a multicast ``getvsn`` command will result in
   ``mk5daemon`` multicasting out a ``Mark5Status`` message containing
   information on the VSNs of the inserted modules as well as the state
   of the Mark5 unit. When ``Mark5A`` is running, a socket is opened to
   this program and the ``bank_set?`` query is issued, which returns the
   VSNs, regardless of the activity. When ``Mark5A`` is not running,
   ``mk5daemon`` either directly determines the VSNs through a
   Streamstor API library call if the Mark5 unit is idle, or doesn’t
   respond if the Mark5 unit is busy. With each ``Mark5Status`` message
   that is multicast from ``mk5daemon`` the state of the Mark5 unit is
   included. See §\ `[sec:xml] <#sec:xml>`__ for details on these XML
   messages.

-  **Starting of mpifxcorr**

   If ``mk5daemon`` is started with the ``-H`` or ``–head-node`` option,
   it will be allowed to start new correlations. A correlation will be
   started when a ``difxStart`` message is received if it passes some
   minor sanity checks. Since ``mk5daemon`` runs as root, it has the
   capability of changing file ownerships. By default, the output files
   from ``mpifxcorr`` and ``difxlog`` will have their ownership and
   permissions changed to match those of the ``.input`` file.

Normally ``mk5daemon`` is started automatically, either by
``/etc/rc.local`` or by a script in ``/etc/init.d`` . The command line
options supported are:

-  Usage: ``mk5daemon`` :math:`[` *options* :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-q`` or ``–quiet`` : be less verbose and don’t mulitcast state

   -  ``-H`` or ``–head-node`` : give head-node permissions

   -  ``-m`` or ``–isMk5`` : force mk5daemon on this host to act as
      Mark5 regardless of hostname (default is mark5fx??)

   -  ``-u`` *userID* or ``–user`` *userID* : use *userID* when
      executing remote commands (default is ’difx’)

   -  ``-l`` *logPath* or ``–log-path`` *logPath* : put logs in
      directory *logPath*, not ``/tmp``

Please be sure not to have multiple instances of ``mk5daemon`` running
at any one time on any individual Mark5 or correlator unit!

.. _sec:mk5dir:

mk5dir :math:`\mathrm{(package: mark5daemon)}`
----------------------------------------------

Program ``mk5dir`` extracts the directory from a module. Normally one
would not call this program directly but would use the ``getdir`` option
of ``mk5control``. By default this program will change the disk module
state to ``played``. There is a danger that if this is done with an
SDK 9 unit and the disk is later needed in an SDK 8 unit that it will no
longer be readable in the later without a full reset of its VSN. As of
April 2014 this program supports decoding of all directory types
described by Mark5 Memos 81
(http://www.haystack.mit.edu/tech/vlbi/mark5/mark5_memos/081.pdf) and
100 (http://www.haystack.mit.edu/tech/vlbi/mark5/mark5_memos/100.pdf).

-  Usage: ``mk5dir`` :math:`[` *options* :math:`]` { *bank* :math:`\mid`
   *VSN* }

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : increase verbosity: print directory to
      screen

   -  ``-f`` or ``–force`` : force directory read even if current

   -  ``-F`` or ``–fast`` : get format details from new-style directory
      (Mark5B format only)

   -  ``-n`` or ``–nodms`` : get directory but don’t change disk module
      state

   -  ``-s`` or ``–safedms`` : only change disk module state if SDK 8
      unit or new dir type

   -  ``-d`` or ``–dmsforce`` : proceed with change of module state even
      if this makes module unreadable in SDK 8 units

   -  ``-b`` *b* or ``–begin`` *b* : begin with scan number *b*

   -  ``-e`` *e* or ``–end`` *e* : end with scan number *b*

   -  ``-w`` *file* or ``–write`` *file* : write directory file *file*
      to the module

-  *bank* is one of ``A``, ``B`` or ``AB``

-  *VSN* is a valid 8-character VSN of a loaded module

The resultant directory file will be saved in a file called
*VSN*\ ``.dir`` in the directory pointed to by environment variable
``MARK5_DIR_PATH`` .

This program responds to the value of environment variable
``DEFAULT_DMS_MASK``. This variable should be an integer representing
the state of three bits. ``mk5dir`` only responds to the setting of bit
1 (value 2); if this bit is set, the disk module state will not be
updated on directory reading. It is recommended to set this environment
variable at recording stations so auto-erasure of modules does not
occur.

In the mode where a specified ``.dir`` file is to be written to a Mark5
module directory the VSN must be provided explicitly (i.e., selecting by
bank is not allowed)

.. _sec:mk5erase:

mk5erase :math:`\mathrm{(package: mark5daemon)}`
------------------------------------------------

Program ``mk5erase`` replaces the functionality of ``SSErase``. It is
used to either erase or condition a Mark5 module. It supports SDK9 and
earlier revisions of the Conduant API and either legacy or new (see
Mark5 memop 81) module directories. Conditioning results are multicast
upon conclusion of conditioning, to be received bt ``condition_watch``.
Conditioning (which is started with the ``-c`` option) causes an entire
read/write cycle of the entire module to be performed. This can require
a good fraction of 24 hours to complete. Status updates and progress are
sent every 10 seconds as well. By default the original directory version
will be restored on the module, with zero scans. The version of
directory to use can be forced with either the ``-l`` or ``-n`` options.

-  Usage: ``mk5erase`` :math:`[` *options* :math:`]` *VSN*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : increase verbosity: print directory to
      screen

   -  ``-f`` or ``–force`` : force directory read even if current

   -  ``-c`` or ``–condition`` : Do full conditioning, not just erasing

   -  ``-r`` or ``–readonly`` : Perform read-only conditioning

   -  ``-w`` or ``–writeonly`` : Perform write-only conditioning

   -  ``-d`` or ``–getdata`` : Save the performance data to a file
      called *VSN*\ ``.timedata``

   -  ``-l`` or ``–legacydir`` : Put an empty legacy directory on the
      module when complete

   -  ``-n`` or ``–newdir`` : Put an empty new style directory on the
      module when complete

   -  ``-0`` or ``–nodir`` : Put a zero-size directory on the module
      when complete

-  *VSN* is a valid 8-character VSN of a loaded module

Note that this program will not run without specifying a legal mounted
module VSN. If you wish to erase a module that has no VSN set, use the
``vsn`` program first.

Control-C can be used to safely abort conditioning early. The directory
will be left in an indeterminate state, so use caution when doing this;
if conditioning is stopped before completion use the ``vsn`` program to
assess and possibly modify the current module state.

.. _sec:mk5mon:

mk5mon :math:`\mathrm{(package: difxmessage)}`
----------------------------------------------

Program ``mk5mon`` is a program that listens for ``mark5Status``
messages multicast from the Mark5 units and displays the information;
updating the display as new messages are received.

-  Usage: ``mk5mon``

Make sure the terminal is at least 110 characters wide and is at least
as tall in characters as there are Mark5 units that may transmit
information. To quit, use ctrl-C. The columns being displayed are:

#. Mark5 unit name

#. VSN of module in Bank A

#. VSN of module in Bank B

#. State of the Mark5 unit

#. Playback rate, if playing, in Mbps

#. Playback position, in bytes from beginning of module

#. Scan number of data being played, if playing

#. Scan name of data being played, if playing

mk6cp :math:`\mathrm{(package: mark6sg)}` [sec:mk6cp]
-----------------------------------------------------

``mk6cp`` is a wrapper around ``mk6gather`` which makes the operation of
copying multiple files from a Mark6 module easier.

-  Usage: ``mk6cp`` :math:`[` *options* :math:`]` *filematch1* :math:`[`
   *filematch2* …\ :math:`]` *destination*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : increase output verbosity

   -  ``-r`` or ``–resume`` : don’t copy files that exist in the
      destination path

-  *filematch1* is a shell-style pattern for matching scan names

-  *destination* is the output path to place files; needs to start with
   ``.`` or ``/`` or end with ``/``

mk6gather :math:`\mathrm{(package: mark6sg)}` [sec:mk6gather]
-------------------------------------------------------------

``mk6gather`` extracts data from a Mark6 module, assembling as necessary
the data scattered across the disks in the module.

-  Usage: ``mk6gather`` :math:`[` *options* :math:`]` *template*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : increase output verbosity

   -  ``-a`` or ``–append`` : append to existing file; this continues a
      previous gather

   -  ``-o`` *outfile* or ``–output`` *outfile* : send output to
      *outfile* (default is ``gather.out``)

   -  ``-b`` *bytes* or ``–bytes`` *bytes* : stop writing after *bytes*
      are written

   -  ``-s`` *bytes* or ``–skip`` *bytes* : skip first *bytes* of file

-  *template* specifies a file list to match, in a shell-style wildcard
   pattern, such as the name of a scan (e.g., ``BB407A_LA_No0001``)

If more than one file is to be copied, it is best to use the ``mk6cp``
(`[sec:mk6cp] <#sec:mk6cp>`__) program instead.

mk6ls :math:`\mathrm{(package: vdifio)}` [sec:mk6ls]
----------------------------------------------------

``mk6ls`` looks in the standard Mark6 mountpoint locations (either in
``/mnt/disks/*/*/data`` or in the path pattern set by environment
variable ``$MARK6_ROOT`` for VDIF formatted files. Found files will be
probed and summarized.

Three printing levels are supported:

-  *short* : Simply prints the file names. The portion of the filename
   corresponding to the mountpoint location is excised.

-  *long* : Prints the file names (same as for *short*), the number of
   actual files making up the virtual Mark6 file (e.g., scattered across
   multiple mountpoints), the full size of the virtual file, and an
   indication of the completeness of the virtual file set.

-  *full* : Prints the same information as for *long* followed by
   details of the files, such as details of the Mark6 file version,
   block sizes, packet numbers. This mode is mainly useful for
   developers with access to the vdifio source code.

-  Usage: ``mk6ls`` :math:`[` *options* :math:`]`

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-s`` or ``–short`` : print long form output (default)

   -  ``-l`` or ``–long`` : print long form output

   -  ``-f`` or ``–full`` : print full information for each file

.. _sec:mk6mon:

mk6mon :math:`\mathrm{(package: difxmessage)}`
----------------------------------------------

.. _sec:mk6summary:

mk6summary :math:`\mathrm{(package: mark6sg)}`
----------------------------------------------

.. _sec:mk6vmux:

mk6vmux :math:`\mathrm{(package: vdifio)}`
------------------------------------------

.. _sec:mpifxcorr:

mpifxcorr
---------

The core of the DiFX software correlator is the program called
``mpifxcorr``. This program uses Message Passing Interface (MPI) to
exploit parallel computing to make correlation practical on a cluster of
ordinary computers. This program runs on all the machines listed in the
``.machines`` file that is passed to ``mpirun``, the program that starts
``mpifxcorr``. It should be initiated from the cluster head node from
within the project directory. The usage line below is appropriate for
use with OpenMPI (§\ `[sec:mpi] <#sec:mpi>`__) and within the DiFX
context; other incantations may provide better results depending on the
setup. See the OpenMPI documentation for more details.

-  Usage: ``mpirun -np`` *nProcess* ``–bynode –hostfile`` :math:`[`
   *otherOptions* :math:`]` *machinesFile* ``mpfixcorr`` *inputFile*
   :math:`[` *options* :math:`]`

-  *nProcess* is the number of processes to start; found with ``wc -l``
   *machineFile*

-  *machinesFile* the ``.machineFile``

-  *inputFile* the ``.input`` to run; the full path to this file needs
   to be given, so prepending the file with ``‘pwd‘/`` is typical

-  *otherOptions* can be any additional option to ``mpirun``;
   ``startdifx`` uses the
   ``–mca btl \hat{\ }udapl,openib –mca mpi_yield_when_idle 1`` to
   suppress some warning messages and be less aggressive on networking

-  *options* are additional options that ``mpifxcorr`` can take which
   include:

   -  ``-M``\ *monHostname*\ ``:``\ :math:`[`\ *monSkip*\ :math:`]` :
      hostname of a machine serving as a monitor data server, with
      optional value indicating how many records to skip between sends.

   -  ``-r``\ *startSec* : start *startSec* seconds into the job,
      writing a new set of files into the visibility directory
      (``.difx/``)

   -  ``–vgoscomplex`` : flips sideband of all Complex VDIF stations by
      complex conjugating the unpacked Complex VDIF data

Within the DiFX framework, the user should never have to directly start
``mpifxcorr`` as this is done more simply with ``startdifx`` or via the
DiFX Operator Interface in conjunction with ``mk5daemon``.

oms2v2d :math:`\mathrm{(package: vex2difx)}` [sec:oms2v2d]
----------------------------------------------------------

The VLBI scheduling program ``sched`` generates a file with extension
``.oms`` which is used to populate some fields in the VLBA database.
These fields are usually used to feed the dynamic scheduler but can also
be used to reduce the tedium of transferring information from the
``sched`` input file (``.key``) to the ``vex2difx`` input file ``.v2d``.
For simple experiments this resulting ``.v2d`` file can be used
unedited, but for more typical experiments editing will be required. The
resulting file will have the same file prefix as the input file and will
end with ``.v2d``.

-  Usage: ``oms2v2d`` :math:`[` *options* :math:`]` *file*\ ``.oms``

-  *file*\ ``.oms`` is an ``.oms`` file written by ``sched``

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-f`` or ``–force`` : allow overwrite of output file

-  Example: ``oms2v2d bx123.oms``

Note: ``sched`` now produces a ``.tv2d`` file (template vex2difx) that
can contain useful information for some projects (especially
multi-phase-center projects), however, this file is not tied to any
particular version of DiFX (or more importantly, version of
``vex2difx``) and thus cannot be guaranteed to be legal. It is thus
suggested to use ``oms2v2d`` and transfer over needed information by
hand after the fact.

padVDIF :math:`\mathrm{(package: vdifio)}` [sec:padVDIF]
--------------------------------------------------------

Program ``padVDIF`` takes an input VDIF file and inserts additional
packets as needed to create a contiguous without gaps. Newly inserted
frames will have the invalid bit set.

-  Usage: ``padVDIF`` *infile* *outfile* *Mbps* :math:`[` *newStartMJD*
   :math:`]`

-  *infile* is the input VDIF file

-  *outfile* is the output VDIF file

-  *Mbps* is the data rate in megabits/second

-  *newStartMJD* is the MJD (with fractional component) to overwrite the
   times with

-  Example: ``padVDIF raw.vdif smooth.vdif 2048``

plotapd :math:`\mathrm{(package: SniffPlots)}` [sec:plotapd]
------------------------------------------------------------

Program ``plotapd`` takes a text file containing *sniffer* fringe fit
results and makes plot files. Separate plots for Amplitude, Phase and
Delay (hence the name suffix “apd”are made for each baseline in the
resultant file. A plot of delay rate is also produced.

This is an interactive command line program; running ``plotapd`` will
prompt the user for inputs. The program ``difxsniff`` will run
``plotapd`` and its sister programs automatically, so usually it won’t
be necessary to run by hand.

The ``PGPLOT_FONT`` environment variable must be set, otherwise all plot
text will be missing.

plotbp :math:`\mathrm{(package: SniffPlots)}` [sec:plotbp]
----------------------------------------------------------

Program ``plotbp`` takes a text file containing *sniffer* bandpass
output files and creates plots. This program can produce both auto- and
cross-correlation data plots.

This is an interactive command line program; running ``plotbp`` will
prompt the user for inputs. The program ``difxsniff`` will run
``plotbp`` and its sister programs automatically, so usually it won’t be
necessary to run by hand.

The ``PGPLOT_FONT`` environment variable must be set, otherwise all plot
text will be missing.

plotwt :math:`\mathrm{(package: SniffPlots)}` [sec:plotwt]
----------------------------------------------------------

Program ``plotwt`` takes a text file containing *sniffer* data weights.
In this context, a data weight of 0 indicates complete loss of data and
a weight of 1 indicates complete data. Each plotted data point will
usually span many correlator integration periods. A solid dot will be
plotted for the mean of these points, and “error bars” will indicate the
range of weights over the averaging period. Note that in some cases
where awkward integration times are used it may be possible for the
weight to occasionally exceed 1, as long as the long running average
never does.

This is an interactive command line program; running ``plotwt`` will
prompt the user for inputs. The program ``difxsniff`` will run
``plotwt`` and its sister programs automatically, so usually it won’t be
necessary to run by hand.

The ``PGPLOT_FONT`` environment variable must be set, otherwise all plot
text will be missing.

printDiFX.py :math:`\mathrm{(package: vis2screen)}` [sec:printDiFX]
-------------------------------------------------------------------

Program ``printDiFX`` prints a summary of the visibility information in
a DiFX output file. It loops through all the records printing some basic
info about frequencies, baselines, polarizations, times etc., plus a
couple of selected visibility values from the start and middle of the
record, to the screen.

-  Usage: ``printDiFX`` *difxfile* *inputfile*

-  *difxfile* is the full name of the visibility file in the ``.difx``
   directory

-  *inputfile* is the path to the input file used to generate this difx
   output

-  Example:
   ``printDiFX example_1.difx/DIFX_55523_025239.s0000.b0000 example_1.input``

printVDIF :math:`\mathrm{(package: vdifio)}` [sec:printVDIF]
------------------------------------------------------------

Program ``printVDIF`` loops through a VDIF file inspecting each packet
header and printing some basic summary info (time etc.) to the screen.

-  Usage: ``printVDIF`` *vdiffile* *Mbps*

-  *vdiffile* is the recorded VDIF file to inspect

-  *Mbps* is the data rate in megabits/second

-  Example: ``printVDIF example.vdif 256``

.. _sec:printVDIFgaps:

printVDIFgaps :math:`\mathrm{(package: vdifio)}`
------------------------------------------------

.. _sec:printVDIFheader:

printVDIFheader :math:`\mathrm{(package: vdifio)}`
--------------------------------------------------

Program ``printVDIFheader`` is a powerful diagnostic tool that prints
details of each VDIF header found in a VDIF file. All fields of each
header, including information in known Extended VDIF Headers (see
Sec. `[sec:vdifedv] <#sec:vdifedv>`__) are printed. Three different
print formats are allowed: compact ``short`` version, detailed ``long``,
and hexidecimal ``hex``.

-  Usage: ``printVDIFheader`` *inputFile* :math:`[` *inputFrameSize*
   :math:`[` *printLevel* :math:`] ]`

-  *inputFile* is the input multi-thread VDIF file, or ``-`` for *stdin*

-  *inputFrameSize* is the size of one thread’s data frame, including
   header (for RDBE VDIF data this is 5032)

-  *printLevel* describes what is to be printed; one of ``short``,
   ``long``, or ``hex``

Run this program with no arguments to get some additiona explanation.

.. _sec:psrflag:

psrflag :math:`\mathrm{(package: difxio)}`
------------------------------------------

``psrflag`` is a program that reads one or more DiFX filesets and
produces a flag table readable by AIPS UVFLG that contains flags for
times when the fringe rate, on a per-baseline basis, resonates with the
pulse period, allowing DC bias to correlate. The pulsar ``.binconfig``
file is used to determine the harmomic content of the pulsar profile.

-  Usage: ``psrflag`` :math:`[` *options* :math:`]` *inputFile* …

-  *inputFile* is the input multi-thread VDIF file; multiple may be
   provided

-  *options* can be:

   -  ``-h`` or ``–help`` : print help information and quit

   -  ``-v`` or ``–verbose`` : be more verbose in execution

.. _sec:record5c:

record5c :math:`\mathrm{(package: mark5daemon)}`
------------------------------------------------

.. _sec:recover:

recover :math:`\mathrm{(package: mk5daemon)}`
---------------------------------------------

``recover`` is a program that wraps the XLRRecover call for convenient
use. This replaces the functionality of the ``recover=`` command of the
``Mark5A`` program.

-  Usage: ``recover`` :math:`[` *options* :math:`]` *type* *bank*

-  *type* is the type of recovery to attempt. See below.

-  *bank* should be either ``A`` or ``B`` and is the bank containing the
   module to address.

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-f`` or ``–force`` : allow overwrite of output file

   -  ``-v`` or ``–verbose`` : be more verbose in execution

-  Example: ``recover -v 2 A``

There are three possible modes of operation that are selected with the
*type* argument:

-  Fix directory if power failed during recording

-  Allow access to data that might have been overwritten

-  Unerase the module

These recovery attempts will not always be successful.

.. _sec:reducepoly:

reducepoly :math:`\mathrm{(package: difxio)}`
---------------------------------------------

Program ``reducepoly`` takes one or more DiFX file sets as input and
will modify each fileset’s delay model (``.im`` file) to have polynomial
representations with fewer terms. All polynomials in the ``,im`` file
will be reduced, including the baseline vectors, atmospheric components,
azimuth, and elevation. The original ``.im`` files will be overwritten.
The main purpose of this program is to evaluate the impact of using
different polynomial orders,

-  Usage: ``reducepoly`` :math:`[` *options* :math:`]` *inputFile* …

-  *inputFile* is the input multi-thread VDIF file, or ``-`` for
   *stdin*; multiple may be provided

-  *options* can be:

   -  ``-h`` or ``–help`` : print help information and quit

   -  ``-2`` : reduce polynomials to two terms

   -  ``-3`` : reduce polynomials to three terms

   -  ``-4`` : reduce polynomials to four terms

   -  ``-5`` : reduce polynomials to five terms

.. _sec:searchVDIF:

searchVDIF :math:`\mathrm{(package: vdifio)}`
---------------------------------------------

.. _sec:splitVDIFbygap:

splitVDIFbygap :math:`\mathrm{(package: vdifio)}`
-------------------------------------------------

.. _sec:startdifx:

startdifx :math:`\mathrm{(package: mpifxcorr)}`
-----------------------------------------------

Starting ``mpifxcorr`` generally requires a lengthy command. This
inspired the creation of ``startdifx`` which vastly simplifies use of
the DiFX correlator. In addition to spawning the ``mpifxcorr``
processes, ``startdifx`` can orchestrate some of the preparatory work
(for example running ``calcif2`` and ``genmachines``) and optionally run
``difx2fits`` to create a ``.FITS`` file for each job. This program is
meant to work within the DiFX environment and would probably require
modification to be useful in other situations.

-  Usage: ``startdifx`` :math:`[` *options* :math:`] [` *startDelay*
   :math:`]` *input1* :math:`[` *input2* :math:`\cdots` :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : be more verbose

   -  ``-q`` or ``–quiet`` : be quieter

   -  ``-f`` or ``–force`` : proceed on files even if correlator output
      already exists and is up to date

   -  ``-a`` or ``–automachines`` : run ``genmachines`` only if no
      ``.machines`` file exits

   -  ``-g`` or ``–genmachines`` : run ``genmachines`` unconditionally
      (default)

   -  ``-n`` or ``–nomachines`` : don’t run ``genmachines``

   -  ``-d`` or ``–dont-calc`` : don’t run ``calcif`` even if needed –
      will skip file

   -  ``-m`` or ``–message`` : start ``mpifxcorr`` by sending difxStart
      message to ``mk5daemon``

   -  ``-F`` or ``–fits`` : run ``difx2fits`` on output of each job
      separately

   -  ``-l`` or ``–localhead`` : use the current host as the headnode,
      not ``$DIFX_JEAD_NODE``

   -  ``–override-version`` : ignore potential difx version conflicts

   -  ``-A`` *agent* or ``–agent=``\ *agent* : call mpirun through
      *agent* with filebase as only argument

   -  ``-M`` *machinesFile* or ``–machines-file=``\ *machinesFile* : use
      *machinesFile* instead of the one expected based on the job name

-  *startDelay* is an optional number of seconds to jump into the job
   upon start

-  *input*\ N is a ``.input`` file, or its prefix

-  Example 1: ``startdifx job1420.000.input``

-  Example 2: ``startdifx -f -n job1420.000 job1421.000``

-  Example 3: ``startdifx -F *.input``

Environment variables respected:

-  ``DIFX_MESSAGE_GROUP`` : multicast group to use (when using -m
   option), overriding default 224.2.2.1

-  ``DIFX_MESSAGE_PORT`` : multicast port to use (when using -m option),
   overriding default 50200

-  ``DIFX_HEAD_NODE`` : when using -m option, this must be set, which
   specifies which machine will serve as the head node

-  ``DIFX_MPIRUNOPTIONS`` : used to pass options to the mpirun command

-  ``DIFX_CALC_PROGRAM`` : can be used to change the delay model program
   (default is ``calcif2``); only needed if model calculations have not
   been done

-  ``DIFX_CALC_OPTIONS`` : used to override options to the delay model
   program

.. _sec:statemon:

statemon :math:`\mathrm{(package: difxmessage)}`
------------------------------------------------

Program ``statemon`` listens for multicast messages of the
``difxStatus`` variety and simply prints their contents to the terminal.
This is mainly useful for debugging ``mpifxcorr`` and any programs
responsible for launching it.

-  Usage: ``statemon`` :math:`[` *options* :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

Environment variables respected:

-  ``DIFX_MESSAGE_GROUP`` : multicast group to use, overriding default
   224.2.2.1

-  ``DIFX_MESSAGE_PORT`` : multicast port to use, overriding default
   50200

.. _sec:stopmpifxcorr:

stopmpifxcorr :math:`\mathrm{(package: mpifxcorr)}`
---------------------------------------------------

If software correlation is in progress and it is desired to stop it, it
is best to gently stop it rather than killing it abruptly. In most
circumstances this can be accomplished with ``stopmpifxcorr``. This
program must be run on the machine running the *manager* process of the
software correlator (usually this is ``swc000`` for the VLBA). If
multiple ``mpifxcorr`` processes are found running on a machine,
``stopmpifxcorr`` will not proceed unless the ``-f`` option is used.

-  Usage: ``stopmpifxcorr`` :math:`[` *options* :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-f`` or ``–first-pid`` : send stop message to the numerically
      first process ID found

   -  ``-q`` or ``–quiet`` : don’t produce much output

stripVDIF :math:`\mathrm{(package: vdifio)}` [sec:stripVDIF]
------------------------------------------------------------

Program ``stripVDIF`` strips network headers from a VDIF format basebad
data file (e.g., captured from wireshark) and dumps a pure VDIF stream.

-  Usage: ``stripVDIF`` *infile* *outfile* :math:`[` *skipbytesfront*
   :math:`[` *skipbytesback* :math:`[` *skipbytesinitial* :math:`] ] ]`

-  *infile* is the input VDIF file

-  *outfile* is the output VDIF file

-  *skipbytesfront* is the number of bytes to skip over before each
   frame (default is 54)

-  *skipbytesback* is the number of bytes to skip over after each frame
   (default is 4)

-  *skipbytesinitial* is the number of bytes to skip over only once
   after opening the file (default is 28)

-  Example: ``stripVDIF vdif.wireshark vdif.pure 54 4 28``

.. _sec:tabulatedelays:

tabulatedelays :math:`\mathrm{(package: difxio)}`
-------------------------------------------------

Program ``tabulatedelays`` takes one or more DiFX filesets and produces
a text file containing, for each scan, a table of interferometer delays
(:math:`\mu`\ s) and rate (:math:`\mu`\ s/s) based on the values in the
``.im`` file at a cadence of one entry every 8 seconds. Based on command
line options values other than the delay can be extracted and tabulated.
The details of the output files are documented in comments at the top of
the output file.

-  Usage: ``tabulatedelays`` :math:`[` *options* :math:`]` *inputFile* …

-  *inputFile* is the input multi-thread VDIF file, or ``-`` for
   *stdin*; multiple may be provided

-  *options* can be:

   -  ``-h`` or ``–help`` : print help information and quit

   -  ``–az`` : print azimuth (deg) and azimuth rate (deg/s) instead of
      delay, rate

   -  ``–el`` : print elevation (deg) and azimuth rate (deg/s) instead
      of delay, rate

   -  ``–dry`` : print the dry component of atmospheric delay
      (:math:`\mu`\ s) instead of delay, rate

   -  ``–wet`` : print the wet component of atmospheric delay
      (:math:`\mu`\ s) instead of delay, rate

   -  ``–uvw`` : print the antenna-based baseline coordinates
      :math:`(x, y, z)` (meters) instead of delay, rate

   -  ``–clock`` : print the clock offset (:math:`\mu`\ s) and rate
      (:math:`\mu`\ s/s) instead of delay, rate

   -  ``–perint`` : print values at the center of every integration
      rather than every 8s

   -  ``–addclock`` : include clock offset/rate in printed delay/rate
      values

This program reads through one or more difx datasets and evaluates delay
polynomials in the ``.im`` files on a regular time grid (every 8
seconds). Delays and rates are both calculated. Output should be self
explanatory. Plotting utilities such as gnuplot can be used directly on
the output.

When operating without ``–perint``, the entirety of the delay
polynomials are plotted, even exceeding the time range of the scans to
which they belong. Comments in the output separate scans cleanly. When
``–perint`` is used, only the time covered by the scans is output.

Sign conventions:

-  Delay: a positive delay indicates wavefront arrival at the station
   before wavefront arrival at earth center. The delay includes
   contribution from wet and dry atmosphere components.

-  Rate: simply the time derivative of Delay.

-  Clock Offset: sign convention is opposite that of
   ``.vex "clock_early"`` parameter; a positive clock offset indicates
   slow station clock. The sum of Clock Offset and Delay is the total
   correlator delay.

-  Clock Rate: simply the time derivative of Clock Offset.

.. _sec:testdifxinput:

testdifxinput :math:`\mathrm{(package: difxio)}`
------------------------------------------------

This program was intended mainly for helping debug parsing of ``.input``
files and associated other files. It turned out to be useful as a
general tool to investigate the contents of such files. When multiple
input files are provided on the command line merging of the resultant
data structures is attempted. Two output files are created when run:
``input.out`` and ``calc.out``. These files should closely resemble the
input files if the parsing was done properly.

-  Usage: ``testdifxinput`` :math:`[` *options* :math:`]`
   *inputFilePrefix1* :math:`[` *inputFilePrefix2* :math:`\cdots ]`

-  *options* can be:

   -  ``-v`` or ``–verbose`` : be a bit more verbose

   -  ``-h`` or ``–help`` : print help information and quit

-  *inputFilePrefix*\ :math:`n` is the base name of an input file

.. _sec:testdifxmessagereceive:

testdifxmessagereceive\ :math:`\mathrm{(package: difxmessage)}`
---------------------------------------------------------------

Test program ``testdifxmessagereceive`` captures multicast DiFX messages
and prints them to the screen. Both the raw XML is shown and the decoded
values. It is mostly useful as a tool for examining the correctness of
the multicast messages that are broadcast and is not intended to be part
of an operational system. There is a special binary mode which instead
listens for the multicast high time resolution autocorrelations. In this
mode, only a terse summary of what is received is printed (see the
source code for more information).

-  Usage: ``testdifxmessagereceive`` :math:`[` *options* :math:`]`
   :math:`[` *type* :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print help information

   -  ``-b`` or ``–binary`` : generate output based on binary records

-  *type* is the kind of message to capture (not for use with binary
   records):

   #. DifxLoadMessage

   #. DifxAlertMessage

   #. Mark5StatusMessage

   #. DifxStatusMessage

   #. DifxInfoMessage

   #. DifxDatastreamMessage

   #. DifxCommand

   #. DifxParameter

   #. DifxStart

   #. DifxStop

   #. Mark5VersionMessage

   #. Mark5ConditionMessage

   #. DifxTransientMessage

If *type* is not provided, all message types will be captured.

.. _sec:testmod:

testmod :math:`\mathrm{(package: mk5daemon)}`
---------------------------------------------

``testmod`` is a program that is used to perform read and write tests on
Mark5 modules. It is meant as a replacement of the ``ResetModule``
program that relies on the ``Mark5A`` program which is being phased out
of VLBA operations. Read-only tests can be performed without risk of
erasing astronomical data recorded on the disks. The more invasive
write-read tests (which are the default) will erase all preexisting
data! A matrix of numbers similar to what is produced by ``ResetModule``
or ``SSerase`` in condition mode, but with statistics from a much
smaller volume of reading/writing is produced. Usually badly performing
disks will occur in pairs with both bad disks belonging to the same bus
(e.g., disks 0 and 1, 2 and 3, 4 and 5, or 6 and 7). Badly performing
drives should have their directory files ``.dir`` (see
§ `[sec:dir] <#sec:dir>`__) updated by hand to include ``RT`` at the end
of the top line.

-  Usage: ``testmod`` :math:`[` *options* :math:`]` *bank*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : produce more informative/diagnostic
      output; ``-v -v`` for even more

   -  ``-f`` or ``–force`` : continue to produce files despite warnings

   -  ``-r`` or ``–readonly`` : Perform read-only test

   -  ``-R`` or ``–realtime`` : Switches to real-time mode (see below)

   -  ``-d`` or ``–skipdircheck`` : Disable directory checking (see
      below)

   -  ``-S`` or ``–speed`` : Disable correctness testing to better test
      throughput

   -  ``-n`` *n* or ``–nrep`` *n* : Repeat the test *n* times (default
      is 2)

   -  ``-s`` *s* or ``–blocksize`` *s* : Read and write *bytes* at a
      time (default is 10 MB)

   -  ``-b`` *b* or ``–nblock`` *b* : Perform *b* reads per test
      (default is 50)

   -  ``-p`` *p* or ``–pointer`` *p* : Start read-only tests at byte
      position ``p``

   -  ``-o`` *file* or ``–dirfile`` *file* : Write the module directory
      to file *file*

-  *bank* is the Mark5 bank containing the disk to be studied (``A`` or
   ``B``)

Many modules being tested are perhaps damaged in some way and may
require the ``-R`` and/or ``-d`` options above. Is is generally safe to
use these options, but the diagnostic power of this program may be
reduced in cases where some drives are intrinsically slow, but still
produce valid data.

.. _sec:testsequnumbers:

testseqnumbers :math:`\mathrm{(package: difxmessage)}`
------------------------------------------------------

Program ``testseqnumbers`` is a utility to listen for DiFX multicast
messages and identify any that come with a sequence number that is not
sequential. This is a good way to identify possible packet loss or
duplication on a DiFX cluster network.

-  Usage: ``testseqnumbers`` :math:`[` *options* :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : produce more output; ``-v -v`` for even
      more

If run without the ``-v`` option, only unexpected packets will be noted.
If run with one ``-v`` flag, each received packet will be identified
with a period being written to the screen. If run with 2 ``-v`` flags,
each packet received will have its source and sequence number printed.

.. _sec:vdif2to8:

vdif2to8 :math:`\mathrm{(package: vdifio)}`
-------------------------------------------

Program ``vdifb2to8`` takes a 2-but sampled VDIF stream and reencodes it
as 8-bit samples. It should work on any form of VDIF with 2 bits per
sample (2+2 bits complex). It is anticipated that any 8-bit decoder will
be performed with linear level spacings, unlike the case for 2-bit
samples which use a high to low ratio of 3.3359 to minimized
quantization noise. Given this anticipation the levels chosen in the
output 8-bit stream correspond to levels of 35.5 and 118.5 counts for
the low and high states respectively. These levels lead to a ratio as
close to 3.3359 as possible. The 0.5 offset comes about from assuming
that the 256 output states are centered on zero and thus range from
-127.5 …-0.5, 0.5 …127.5. Extra bytes in the input stream (not
corresponding to valid VDIF frames) will be excised but invalid frames
(as marked with the invalid bit) will be retained and encoded to 8 bits.

-  Usage: ``vdif2to8`` *inputFile* *frameSize* *outputFile*

-  *inputFile* is the file to read (2 bits per sample)

-  *frameSize* is the size of the VDIF frames including frame headers
   (5032 for VLBA or VLA VDIF data)

-  *outputFile* is the output file (8 bits sper sample)

-  Example: ``vdif2to8 input.vdif 5032 output.vdif``

.. _sec:vbstate:

vdifbstate :math:`\mathrm{(package: vdifio)}`
---------------------------------------------

Program ``vdifbstate`` will perform state counts on a multi-thread VDIF
baseband data file.

-  Usage: ``vdifbstate`` *file* *frameSize* *dataRate* *threadlist*
   *nFrames* :math:`[` *offset* :math:`]`

-  *file* is the file to decode

-  *frameSize* is the size of the VDIF frames including frame headers
   (5032 for VLBA or VLA VDIF data)

-  *dataRate* is the file data rate, measured in Mbps, not including
   frame headers

-  *threadList* is a comma-separated list of thread ids to decode

-  *nFrames* is the number of data frames (typically a few kB in size)
   to decode

-  *offset* is the number of bytes into the file to start decoding

-  Example: ``vdifbstate sample.vdif 5032 1024 1,2,3,4 100``

Notes:

#. See documentation for ``m5b`` for details on specifying the data
   format.

#. Only real-sampled data with 1 or 2 bits per sample is supported at
   this time.

#. If a non-power-of-two number of threads is requested, extra channels
   will be invented to pad out the next power of two. Data in these
   extra channels has undefined qualities.

.. _sec:vdifChanSelect:

vdifChanSelect :math:`\mathrm{(package: vdifio)}`
-------------------------------------------------

.. _sec:vdifd:

vdifd :math:`\mathrm{(package: vdifio)}`
----------------------------------------

Program ``vdifd`` takes a multi-thread VDIF file and decodes some
samples. It is implemented as a python script that makes use of ``vmux``
and ``m5d`` to do most of the work. It is thus a good program to study
to understand how vmux can be used. This program takes as command line
input the name of a file containing (or thought to be containing)
multi-thread VDIF baseband data, information about the VDIF stream, and
the number of samples per baseband to decode. Optionally a starting file
offset can be supplied. If the data can be decoded correctly,
information about the data will be printed to the screen along with a
table of decoded data. The output values, -3, -1, 1, or 3 for valid
data, are printed in *nchan* columns. Data that cannot be decoded
(either due to data replacement headers, data fill pattern replacing the
actual data after unloading from a Mark5 module, or identified via the
VDIF invalid bit) will show as 0. It should be invoked with the
following parameters:

-  Usage: ``vdifd`` *file* *frameSize* *dataRate* *threadlist* *n*
   :math:`[` *offset* :math:`]`

-  *file* is the file to decode

-  *frameSize* is the size of the VDIF frames including frame headers
   (5032 for VLBA or VLA VDIF data)

-  *dataRate* is the file data rate, measured in Mbps, not including
   frame headers

-  *threadList* is a comma-separated list of thread ids to decode

-  *n* is the number of samples to decode

-  *offset* is the number of bytes into the file to start decoding
   (default is 0)

-  Example: ``vdifd sample.vdif 5032 1024 1,2,3,4 24``

In the above example, a stream consisting of 4 channels, each with
64 MHz bandwidth and 2 bits per sample and with thread ids 1, 2, 3 and 4
are to be decoded. If the thread ids were to be permuted, the decoded
output data would be permuted in the same manner.

Notes:

#. At this time only 2-bit real-valued data can be properly decoded.

#. If a non-power-of-two number of threads is requested, extra channels
   will be invented to pad out the next power of two. Data in these
   extra channels has undefined qualities.

.. _sec:vdiffold:

vdiffold :math:`\mathrm{(package: vdifio)}`
-------------------------------------------

Program ``vdiffold`` takes a baseband data stream and integrates the
power (formed by squaring the voltage) in a number of time bins that
equally divide a given period. This is a simplifed version of “folding”
such as is used in pulsar processing. A typical use of such
functionality would be to investigate the waveform of the switched power
injected into the receiver for calibration. This program has found
considerable utility in determining time offsets between the sample
clock and formatter time (modulo the period of the calibration cycle).

In the case of 2-bit sampling a non-linear correction is applied before
results are written to a file. This correction takes the form

.. math:: P = \frac{1}{\left({\rm erf}^{-1}\left(\frac{\hat{P} - v_{\rm high}^2}{1 - v_{\rm high}^2} \right)\right)^2},

where :math:`P` is a value proportional to true power and
:math:`\hat{P}` is the value obtained by calculating
:math:`\left<\hat{v}^2\right>` when the bitstream is reproduced with
values :math:`\hat{v} \in ( -v_{\rm high}, -1, 1, v_{\rm high} )`. This
non-linear correction can be turned off by setting *nbin* to a negative
value. Note that this program is not useful for 1-bit quantized data.

The program should be used as follows:

-  Usage: ``vdiffold`` *infile* *frameSize* *dataRate* *threadlist*
   *nbin* *nchunk* *freq* *outfile* :math:`[` *offset* :math:`]`

-  *infile* is the file to decode

-  *frameSize* is the size of the VDIF frames including frame headers
   (5032 for VLBA or VLA VDIF data)

-  *dataRate* is the file data rate, measured in Mbps, not including
   frame headers

-  *threadList* is a comma-separated list of thread ids to decode

-  *nbin* is the number of bins to calculate per period; if negative,
   power correction is not performed and the absolute value of *nbin* is
   used

-  *nchunk* is the number of 10000 sample chunks to operate on

-  *freq* is the reciprocal of the period to be observed (Hz)

-  *outfile* is the name of the output file

-  *offset* (optional) is the number of bytes into the file to start
   decoding

-  Example:
   ``vdiffold sample.vdif 5032 1024 1,2,3,4 128 10000 80 sample.fold``

The output file will contain *nchan*\ +1 columns where *nchan* is the
number of baseband channels in the data stream. The first column
contains the time (seconds) within the period. Each remaining column is
folded power for one baseband channel. If *nbin* is positive and the
data is 2-bit quantized, the scaling is such that
:math:`\left<v^2\right> = \sigma^2` yields a power reading of 1.0, for
sampler threshold :math:`\sigma`. Optimal signal to noise ratio occurs
for a value of about 1.03. For non 2-bit quantization, the power will be
in units of reconstituted :math:`{\rm counts}^2`.

Notes:

#. At this time only 2-bit real-valued data can be properly decoded.

#. If a non-power-of-two number of threads is requested, extra channels
   will be invented to pad out the next power of two. Data in these
   extra channels has undefined qualities.

#. The output columns are in the same order as the thread id list. Thus,
   you can rearrange the output order by changing the order of the
   thread list. This enables reordering of data so that polarization
   pairs occur consecutively, allowing more sensible cross-correlation
   columns.

.. _sec:vdifspec:

vdifspec :math:`\mathrm{(package: vdifio)}`
-------------------------------------------

Program ``vdifspec`` forms total power spectra for each baseband channel
in the data, including cross spectra for polarization pairs, assuming
data is in alternating polarization pairs (if not, the cross spectra
should make no sense, but they are formed anyway). The results are
written to a text file with the following columns: Column 1 is the
frequency offset from baseband for each channel; Columns 2 to
*nchan*\ +1 are the total power spectra for each baseband channel;
Columns *nchan*\ +2 to :math:`4 \times`\ *nchan*\ +1 contain, in pairs,
the amplitude and phase of the cross spectra for each pair of channels.
It should be invoked with the following parameters:

-  Usage: ``vdifspec`` *infile* *frameSize* *dataRate* *threadlist*
   *npoint* *n* *outfile* :math:`[` *offset* :math:`]`

-  *infile* is the file to decode

-  *frameSize* is the size of the VDIF frames including frame headers
   (5032 for VLBA or VLA VDIF data)

-  *dataRate* is the file data rate, measured in Mbps, not including
   frame headers

-  *threadList* is a comma-separated list of thread ids to decode

-  *npoint* is the number of points to calculate for each spectrum

-  *n* is the number of FFT frames to include in the calculation

-  *outfile* is the name of the output file

-  *offset* (optional) is the number of bytes into the file to start
   decoding

-  Example:
   ``vdifspec sample.vdif 5032 1024 1,2,3,4 256 1000 vlba.spec``

Notes:

#. At this time only 2-bit real-valued data can be properly decoded.

#. If a non-power-of-two number of threads is requested, extra channels
   will be invented to pad out the next power of two. Data in these
   extra channels has undefined qualities.

#. The output columns are in the same order as the thread id list. Thus,
   you can rearrange the output order by changing the order of the
   thread list. This enables reordering of data so that polarization
   pairs occur consecutively, allowing more sensible cross-correlation
   columns.

.. _sec:vex2difx:

vex2difx
--------

``vex2difx`` is a program that takes a ``.vex`` files (such as one
produced by ``sched`` with various tables based on observe-time data
appended, probably by ``db2vex`` in the case of VLBA operations) and a
``.v2d`` configuration file (see §\ `[sec:v2d] <#sec:v2d>`__) and
generates one or more ``.input`` and ``.calc`` file pairs for use with
the DiFX correlator. Note specifically that ``.ovex`` files, as used at
many/most Mark4 correlators, are not supported. ``vex2difx``, along with
``calcif2``, supercedes the functionality of ``vex2config`` and
``vex2model``, two programs that were widely used but never fully
integrated into the VLBA’s software chain. Don’t forget that ``oms2v2d``
can be used to create a valid baseline ``.v2d`` file from the ``.oms``
file made by ``sched``, perhaps saving some time.

The following guiding principles drove the design of ``vex2difx``:

#. The output files should never need to be hand edited

#. Simple experiments should not require complicated configuration

#. All features implemented by mpifxcorr should be accessible

#. All experiments expressible by vex should be supported

#. The configuration file should be human and machine friendly

#. Command line arguments should not influence the processing of the vex
   file

Note that not all of these ideals have been completely reached as of
now. It is not the intention of the developer to guess all possible
future needs of this program. Most new features will be easy to
implement so send a message to the difx-users mailing list or file a
JIRA :raw-latex:`\cite{jira}` bug tracking ticket for requests.

-  Usage: ``vex2difx`` :math:`[` *options* :math:`]` *v2dFile*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-o`` or ``–output`` : write a configuration file called
      *v2dFile*\ ``.params`` (see §\ `[sec:params] <#sec:params>`__) as
      output

   -  ``-v`` or ``–verbose`` : produce more informative/diagnostic
      output; ``-v -v`` for even more

   -  ``-d`` or ``–delete-old`` : delete old output from same .v2d file

   -  ``-f`` or ``–force`` : continue to produce files despite warnings

   -  ``-s`` or ``–strict`` : treat some warnings as errors and quit
      (default)

-  *v2dFile* is a ``.v2d`` file (see §\ `[sec:v2d] <#sec:v2d>`__) that
   controls the operation of this program; the filename cannot contain
   underscore characters

-  Example: ``vex2difx bx123.v2d``

VDIF issues
~~~~~~~~~~~

Unlike for the other formats, VDIF does not make use of the ``$TRACKS``
section for numeric assignment of channel. Instead the channels as
listed in the ``$FREQ`` section are sorted alphabetically by their link
name (usually something like ``Ch01``. The alphabetical list is matched
against the thread-channel order where the threads are listed in numeric
order; the “thread index” takes precedence over the “channel index”. The
``track_frame_format`` parameter of the ``$TRACKS`` section is still
required.

Mark5B issues
~~~~~~~~~~~~~

The Mark5B format, including its 2048 Mbps extension, is now supported
by ``vex2difx``. The ``.vex`` file track assignments for Mark5B format
has never been formally documented. ``vex2difx`` has adopted the track
assignment convention used by Haystack. Formally speaking, Mark5B has no
tracks. Instead it stores up to 32 bitstreams in 32 bit words. The
concept of fanout is no longer used with Mark5B. Instead, the equivalent
operation of spreading one bitstream among 1 or more bits in each 32 bit
word is performed automatically. Thus to specify a Mark5B mode, only
three numbers are needed: Total data bit rate (excluding frame headers),
number of channels, and number of bits per sample (1 or 2). The number
of bitstreams is the product of channels and bits.

The ``$TRACKS`` section of the vex file is used to convey the bitstream
assignments. Individually, the sign and magnitude bits for each channel
are specified with ``fanout_def`` statements. In unfortunate
correspondence with existing practice, 2 is the first numbered bitstream
and 33 is the highest. In 2-bit mode, all sign bits must be assigned to
even numbered bitstreams and the corresponding magnitude bit must be
assigned to the next highest bitstream. To indicate that the data is in
Mark5B format, one must either ensure that a statement of the form

``track_frame_format = MARK5B;``

must be present in the appropriate ``$TRACKS`` section or

``format = MARK5B``

must be present in each appropriate ``ANTENNA`` section of the ``.v2d``
file. As a concrete example, a ``$TRACKS`` section may resemble:

::

   $TRACKS;
   def Mk34112-XX01_full;
     fanout_def = A : &Ch01 : sign : 1 : 02;
     fanout_def = A : &Ch01 : mag  : 1 : 03;
     fanout_def = A : &Ch02 : sign : 1 : 04;
     fanout_def = A : &Ch02 : mag  : 1 : 05;
     fanout_def = A : &Ch03 : sign : 1 : 06;
     .
     .
     .
     fanout_def = A : &Ch15 : mag  : 1 : 31;
     fanout_def = A : &Ch16 : sign : 1 : 32;
     fanout_def = A : &Ch16 : mag  : 1 : 33;
     track_frame_format = MARK5B;
   enddef;

Media specification
~~~~~~~~~~~~~~~~~~~

``vex2difx`` allows ``.input`` file generation for two types of media. A
single ``.input`` file can have different media types for different
stations. Ensuring that media has been specified is important as
antennas with no media will be dropped from correlation. The default
media choice is Mark5 modules. The ``TAPELOG_OBS`` table in the input
vex file should list the time ranges valid for each module. Jobs will be
split at Mark5 module boundaries; that is, a single job can only support
a single Mark5 unit per station. All stations using Mark5 modules will
have ``DATA SOURCE`` set to ``MODULE`` in ``.input`` files. If
file-based correlation is to be performed, the ``TAPELOG_OBS`` table is
not needed and the burden of specifying media is moved to the ``.v2d``
file. The files to correlate are specified separately for each antenna
in an ``ANTENNA`` block. Note when specifying filenames, it is up to the
user to ensure that full and proper paths to each file are provided and
that the computer running the datastream for each antenna can see that
file. Two keywords are used to specify data files. They are not mutually
exclusive but it is not recommended to use both for the same antenna.
The first is ``file``. The value assigned to ``file`` is one or more
(comma separated) file names. It is okay to have multiple file keywords
per antenna; all files supplied will be stored in the same order
internally. The second keyword is ``filelist`` which takes a single
argument, which is a file containing the list of files to read. The file
pointed to by ``filelist`` only needs to be visible to ``vex2difx``, not
the software correlator nodes. This file contains a list of file names
and optionally start and stop MJD times. Comments can be started with a
# and are ended by the end-of-line character. Like for the file keyword,
the file names listed must be in time order, even if start and stop MJD
values are supplied. An example file as supplied to ``filelist`` is
below:

::

   # This is a comment.  File list for MK for project BX123
   /data/mk/bx123.001.m5a  54322.452112 54322.511304
   /data/mk/bx123.002.m5a  54322.512012 54322.514121 # a short scan
   /data/mk/bx123.003.m5a  54322.766323 54322.812311 

If times for a file are supplied, the file will be included in the
``.input`` file DATA TABLE only if the file time range overlaps with the
``.input`` file time range. If not supplied, the file will be included
regardless of the ``.input`` file time range, which could incur a large
performance problem.

A few sample ANTENNA blocks are shown below:

::

   ANTENNA MK 
   {
     filelist=bx123.filelist.mk
   }

::

   ANTENNA OV { file=/data/ov/bx123.001.m5a, 
                     /data/ov/bx123.002.m5a,
                     /data/ov/bx123.003.m5a }

::

   ANTENNA PT { file=/data/pt/bx123.003.m5a } # recording started late here

Pulsars
~~~~~~~

Some information, including example ``.v2d`` sections, on setting up
pulsar correlation can be found in §\ `[sec:pulsars] <#sec:pulsars>`__.

You may find additional information at
http://www.atnf.csiro.au/vlbi/dokuwiki/doku.php/difx/vex2difx .

.. _sec:vexpeek:

vexpeek :math:`\mathrm{(package: vex2difx)}`
--------------------------------------------

Program ``vexpeek`` takes a vex file as input and sends to *stdout* the
experiment name, segment, and a list of antennas and the MJD times that
they were included in the observation. This program is mainly intended
to be called from python program ``db2vex`` which needs to know a little
about the file before appending the CLOCK and TAPELOG_OBS tables. The
VLBA operations system relies on such functionality but there is no
reason other operations couldn’t use this. This program uses the same
parsing infrastructure as ``vex2difx`` so the warnings that may be
produced and sent to ``stderr`` in running ``vex2difx`` will also do so
with ``vexpeek``. Thus, when ``db2vex`` is run some of these error
messages may be seen.

-  Usage: ``vexpeek`` :math:`[`\ *options*\ :math:`]` *vexFile*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : print decoded version of *vexFile* to
      screen

   -  ``-f`` or ``–format`` : add per-antenna format description to
      output

   -  ``-b`` or ``–bands`` : print list of bands used by *vexFile*

   -  ``-u`` or ``–diskusage`` : add per-antenna disk usage to output

   -  ``-s`` or ``–scans`` : print list of scans and antennas used by
      each

-  *vexFile* is the vex format file to be inspected.

.. _sec:vlog:

vlog :math:`\mathrm{(package: vex2difx)}`
-----------------------------------------

Program ``vlog`` takes as input a calibration file (``cal.vlba``,
§\ `[sec:cal] <#sec:cal>`__). This file is parsed to produce four files
containing formatted arrays that are convenient for use in the
construction of FITS tables: ``flag``, ``pcal``, ``tsys``, and
``weather`` (§\ `[sec:flag] <#sec:flag>`__,
§\ `[sec:pcal] <#sec:pcal>`__, §\ `[sec:tsys] <#sec:tsys>`__ and
§\ `[sec:weather] <#sec:weather>`__). This program is named after AIPS
task ``vlog`` that does nearly the same thing.

-  Usage: ``vlog`` *calFile* :math:`[`\ *antennaList*\ :math:`]`

-  *calFile* is the ``cal.vlba`` file produced by ``tsm`` to be
   processed.

-  *antennaList* is an optional comma-separated list of antennas to
   process. If omitted, all antennas with calibration data will be
   processed.

Running with no command line arguments will print usage information to
the terminal and exit.

.. _sec:vmux:

vmux :math:`\mathrm{(package: vdifio)}`
---------------------------------------

Program ``vmux`` takes a VDIF file with multiple threads of one channel
each and multiplexes the data into a new VDIF file consisting of a
single thread containing all the channels. In the case that a
non-power-of-two number of channels are contained in the input file (or
equivalently, a non-power-of-two number of threads are specified on the
command line), the next power of two will be selected as the number of
channels in the output thread and any unused channel slots will contain
random data.

-  Usage: ``vmux`` :math:`[`\ *options*\ :math:`]` *inputFile*
   *inputFrameSize* *framesPerSecond* *threadList* *outputFile*
   :math:`[`\ *offset* :math:`[`\ *chunkSize*\ :math:`] ]`

-  *inputFile* is the input multi-thread VDIF file, or ``-`` for *stdin*

-  *inputFrameSize* is the size of one thread’s data frame, including
   header (for RDBE VDIF data this is 5032)

-  *framesPerSecond* is the number of frames per second in the input
   file for each thread (and is thus the number of output frames per
   second as well)

-  *threadList* is a comma-separated list of integers in range 0 to
   1023; the order of the numbers is significant and dictates the order
   of channels in the output data

-  *outputFile* is the name of the output, single-thread VDIF file, or
   ``-`` for *stdout*

-  *offset* is an optional offset into the input file (in bytes)

-  *chunkSize* is (roughly) how many bytes to operate on at a time
   (default is 2000000)

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : be more verbose in execution

   -  ``-q`` or ``–quiet`` : be less verbose in execution

   -  ``-f`` *f* or ``–fanout`` *f* : set fanout factor to *f*

   -  ``-e`` or ``–EDV4`` : convert VDIF extended data version to EDV4
      (default)

   -  ``-n`` or ``–noEDV4`` : don’t convert VDIF extended data version
      to EDV4

The concept of fanout applies to certain variants of VDIF data where one
logical sampled channel is interleaved multiple threads. Certain modes
of the DBBC3 makes use of this mode. See
Sec. `[sec:vdiffanout] <#sec:vdiffanout>`__ for details.

VDIF Extended Data Version (EDV) 4 is used to contain per-channel
validity flags within a multi-channel VDIF file. See
Sec. `[sec:vdifedv4] <#sec:vdifedv4>`__ for details.

.. _sec:vsntool:

vsn :math:`\mathrm{(package: mk5daemon)}`
-----------------------------------------

Program ``vsn`` is used to check or set the Volume Serial Number (VSN),
the write protect state, and the Disk Module State (DMS) of a module.
This program can not be used when the Mark5 unit is being used for
something else. You must be logged into the Mark5 unit that contains the
module to inspect/change. In addition to displaying the VSN of the
module, this utility will list information about each disk in the
module. The columns displayed are:

#. *Disk number:* in the range 0 to 7.

#. *Drive model:* the model number of the disk.

#. *Serial number:* the serial number of the disk (in parentheses).

#. *Drive model revision number:* addition model information.

#. *SMART capable:* 1 indicates SMART information is available; 0
   otherwise.

#. *SMART state:* (only valid if SMART capable) 1 indicates good health.

Note that the drive model, serial number and revision number can have
spaces making it hard to tell when one field start and the next begins.
Thus the serial number is contained within parentheses to make this
clear.

-  Usage: ``vsn`` :math:`[` *options* :math:`]` *bank* :math:`[`
   *newVSN* :math:`]`

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-f`` or ``–force`` : proceed without asking

   -  ``-v`` or ``–verbose`` : be more verbose in operation

   -  ``-p`` or ``–played`` : set DMS to played

   -  ``-r`` or ``–recorded`` : set DMS to recorded

   -  ``-e`` or ``–erased`` : set DMS to erased

   -  ``-w`` or ``–writeprotect`` : set write protection

   -  ``-u`` or ``–unwriteprotect`` : clear write protection

-  ``-s`` or ``–smart`` : Get S.M.A.R.T. data from disks and write to
   file *VSN*\ ``.smart``)

-  *bank* is the Mark5 unit bank to look at (must be ``A`` or ``B``)

-  *newVSN* is the new name to assign to the module and must be a legal
   VSN

-  Example 1: show VSN: ``vsn A``

-  Example 2: set VSN: ``vsn A NRAO+456``

If you get a message such as “Watchdog caught a hang-up executing
…” that means access to the module failed. This could indicate a bad
module. The module should be reinserted (perhaps in a different bank or
unit) and the unit rebooted before coming to a firm conclusion.

.. _sec:vsum:

vsum :math:`\mathrm{(package: vdifio)}`
---------------------------------------

Program ``vsum`` prints a summary of one or more VDIF files, printing
such information as list of threads found, collectively, in the first
and last few MB of the file, the VDIF epoch and other time information,
and packet size. If the data is not recognized as VDIF, an error code
will be printed; see below for the list of codes and their meanings.
Legacy format VDIF data is not supported.

-  Usage: ``vsum`` :math:`[` *options* :math:`]` *file1* :math:`[`
   *file2* :math:`[ \ldots ] ]`

-  *file1* …is/are the VDIF file(s) to summarize

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-s`` or ``–shortsum`` : print one-line summary per file

   -  ``-6`` or ``–mark6`` : interpret provided file names as Mark6
      scans

   -  ``–allmark6`` : summarize all files found on mounted Mark6 modules

If the ``-6`` or ``–mark6`` or ``–allmark6`` option is used, it is
assumed that the files are to be found in their expected location, which
could be altered by an environment variable. See
sec `[sec:mark6path] <#sec:mark6path>`__ for more details.

If the summary operation failed for a file, one of the following error
codes will be returned:

== =======================================================
-1 The file size could not be determined
-2 The file could not be opened
-3 Memory allocation failure
-4 File read failed
-5 Frame size could not be determined
-6 First frame could not be found
-7 Seek to near-end-of-file failed
-8 Read at end of file failed
-9 A valid frame at the end of the file could not be found
== =======================================================

The ``-s`` or ``–shortsum`` option produces output that can be used
directly by ``vex2difx`` as a file list.

.. _sec:zerocorr:

zerocorr :math:`\mathrm{(package: mark5access)}`
------------------------------------------------

Program ``zerocorr`` is intended to cross correlate data with zero time
delay (and a window determined by the spectral resolution) between two
recordings made at the same station. It is possible to correlate data
observed in different formats and even mis-matched bandpasses, through
the construction of a file describing the details of the single sub-band
that is to be correlated.

-  Usage: ``zerocorr`` :math:`[` *options* :math:`]` *confFile*

-  *options* can be:

   -  ``-h`` or ``–help`` : print usage information and exit

   -  ``-v`` or ``–verbose`` : be more verbose in operation

-  *confFile* is a file describing the correlation parameters

-  Example: ``zeroconf td006.zc``

See documentation on *confFile* (or ``.zc`` file) in section
`[sec:zc] <#sec:zc>`__. Output data documentation can be found for
``.vis`` files in section `[sec:vis] <#sec:vis>`__ and for ``.lag``
files in section `[sec:lag] <#sec:lag>`__.
