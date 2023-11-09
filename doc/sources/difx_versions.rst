.. role:: raw-latex(raw)
   :format: latex
..

The DiFX correlator
===================

This document is centered around the NRAO installation of the DiFX
:raw-latex:`\cite{difx}` software correlator and its supporting
software. Much of the contents here applies to other installations of
DiFX as well, but keep in mind that not a lot of effort is made to
generalize these instructions. Fig. `[fig:block] <#fig:block>`__ shows
the general data flow-path within the DiFX software correlator system.

Past, present, and future versions of DiFX as packaged and used by VLBA
operations are described in the following subsections.

NRAO-DiFX 1.0
-------------

Versions 1.0 and 1.1 based correlation on the VLBA hardware correlator
job scripts – the ``.fx`` files. This ensures a compatibility period
during which both correlators can produce visibilities with expectations
of functionally identical results, a feature critical for validation.
This strategy also minimizes the required software effort at its
earliest phases. Version 1.0 came with the following features:

#. A complete path from ``.fx`` job scripts to ``.FITS`` files

#. A command-line only interface

#. Documentation (you are reading it now)

#. Support for VLBA and Mark IV formats

#. Correlation directly off Mark5 modules

#. Support for all projects types except those using special modes, such
   as pulsars, space VLBI, and near field objects

#. Spectral and time resolution bounded only by practicality

While this version should handle most observations, fast frequency
switching and geodesy experiments will produce a large number of output
FITS files which may be annoying to observers and the archive. Version
1.0 was available on February 6, 2008.

NRAO-DiFX 1.1
-------------

Version 1.1 builds on version 1.0 and adds the following features:

#. Used version of ``mpifxcorr`` that has gone through code merge with
   the official version

#. Blanking of data replaced by headers (Mark4 format only)

#. Proper data weights

#. Initial Mark5B support

#. Support for oversampled data through decimation

#. Multicast status information for GUI interface

#. Correlation of moving and near field objects

#. Concatenation of multiple output files into a single or multiple
   FITS-IDI file(s)

#. Better support for jobs with multiple configuration tables

#. Playback off Mark5 modules with missing disks

#. Support for Amazon based Mark5 units

#. Completely replaced the “Makefile” system with better integrated
   alternative

#. Generation of delay model polynomials rather than tables, more like
   VLBA HW correlator

#. :math:`u, v, w` values are derived from the delay model (and hence
   include corrections for aberration, near field observations, and
   other subtle effects) and are evaluated when writing the FITS file

#. DiFX version accountability

#. Validation of data frames prior to decoding

#. Data evaluation (“sniffing”) built into FITS converter

This version was released on September 3, 2008.

Bugs fixed
~~~~~~~~~~

Here are listed some of the more important bug fixes:

#. The clock offset was used with the wrong sign in the IM table.

#. Printed precision of some important numbers (RA and Dec) was
   increased.

#. Autocorrelations were ordered incorrectly for observations with a
   single polarization.

#. The Mark4 format decoder had a 1 day off bug.

#. The Mark4 format decoder had a 64\ :math:`\times`\ *fanout* sample
   timing offset.

#. Several causes of crashes were fixed; no known crashes remain.

#. Missing VLBA monitor data was handled badly.

#. Due to OpenMPI peculiarity, some processing nodes would get most or
   all of the work in some cases, which cause the work being done on
   other nodes to be ignored. This was fixed by looking for results in a
   round-robin manner.

#. Integrations that contain data from two adjacent scans are stripped
   when writing FITS files.

#. Allow FITS files larger than 2GiB in size.

Known problems
~~~~~~~~~~~~~~

Known bugs as of the NRAO-DiFX 1.1 release:

#. The last couple (typically 2) integrations of a job (not a scan) tend
   to have low weight due to a premature termination of data processing.

DiFX 1.5.0
----------

With DiFX 1.5.0 comes a name change. Past releases of this series have
been known as “NRAO-DiFX”. The DiFX community has been largely receptive
to the NRAO additions in support of ``mpifxcorr`` and it was decided
that dropping the “NRAO” was appropriate. In some cases the term “VLBA
DiFX” or “VLBA DiFX 1.5” may be mentioned. These are simply the
deployment of DiFX 1.5.0 for the VLBA correlator with some VLBA specific
features. Note that the name given to the VLBA deployment of DiFX is
formally “VLBA DiFX”.

Version 1.5.0 will start allowing correlation of experiments that cannot
be represented by ``.fx`` files and will be based on vex files. Version
1.5.0 builds on version 1.1 and adds the following features:

#. Support for using a wide variety of vex files as the basis for
   correlation.

#. Native ephemeris-based object trajectories are supported.

#. Pulsar gating is supported.

#. Pulsar binning is supported, but not cleanly yet.

#. A graphical user interface is available for correlator operators.

#. The multicast system is fully implemented and is used monitor and
   control correlation and other operations.

#. Mark5B formatted data, including its 2048 Mbps extension, is
   supported.

#. The VLBA DiFX Operations Plan :raw-latex:`\cite{opsplan}` is
   implemented, including interface to the VLBA archive.

Non-NRAO users of DiFX 1.5.0 will still be able to use the tools
provided but may not be able to take full advantage of the database
back-end without some customization; it is the aim of this document to
point out cases where the database is required. Many of the programs
described in previous versions of this document will be upgraded or
overtaken by more capable replacements.

Release of DiFX 1.5.0 was announced on June 25, 2009.

.. _bugs-fixed-1:

Bugs fixed
~~~~~~~~~~

Here are listed some of the more important bug fixes:

#. A rounding issue in ``mpifxcorr`` occasionally caused the wrong
   source’s UVWs to be assigned.

#. Lower side band data would come out of the sniffer portion of
   ``difx2fits`` with the wrong sign for phase, rate, and delay.

#. Different rounding was used to generate start times for ``.input``
   and ``.calc`` files. There are no severe consequences of this issue.

#. Scaling in pulsar gating has been made more sane.

DiFX 1.5.1
----------

DiFX 1.5.1 is mostly a bug fix update to version 1.5.0, but with a few
new features. The new features include:

#. Option to force job breaks (with the break parameter) has been added
   to ``vex2difx``

#. Time/date formats other than decimal MJD are now accepted by
   ``vex2difx``

#. Specification of data files to correlate (rather than Mark5 units) is
   supported in ``vex2difx``

#. Specification of network parameters in ``vex2difx`` to allow
   correlation of eVLBI projects

#. ``difx2fits`` produces a new output file with suffix ``.jobmatrix``
   provides the user with a better idea of the mapping of jobs into
   ``.FITS`` files

#. A ``vex2difx`` mode for generating DiFX files useful for determining
   pulsar phase has been added

#. EOP values can now be provided within the ``.v2d`` file

#. Upcoming FITS-IDI keyword WEIGHTYP populated

#. Zero-weight data is not written from ``mpifxcorr``

#. New utility ``checkdir`` to look for oddities in Mark5 module
   directory files

.. _bugs-fixed-2:

Bugs fixed
~~~~~~~~~~

Here are listed some of the more important bug fixes:

#. Concatenation of jobs in the creation of ``.FITS`` files does the
   right thing for cases where the antenna subsets change and where
   antenna reordering is done.

#. The Pulsar Gate Model (GM) ``.FITS`` file table is now correctly
   populated for pulsar observations.

#. Autocorrelations are written for each pulsar bin

#. The FXCORR simulator mode of ``vex2difx`` now selects the correct
   reference time for antenna clock offsets.

#. A work-around for a Streamstor problem has been added that should
   improve reliability in Mark5 module correlation when a change in bank
   is needed.

#. The sign of clock offsets in vex files has been reversed to follow
   the vex standard

#. Jobs are split at leap seconds

#. LBA data formats are handled more correctly in ``vex2difx``

#. The model generator (``calcif2``) now respects polynomial parameters
   interval and order given on the command line.

DiFX 1.5.1 was made available via subversion on Sep 8, 2009.

DiFX 1.5.2
----------

DiFX 1.5.2 is mostly a bug fix update to version 1.5.1, but with a few
new features. This version of DiFX comes with the following components
(and versions): calcif2 (1.1), calcserver (1.2), difx_db (1.12;
NRAO-only), difx2fits (2.6.1), difx2profile (0.1), difxio (2.12.1),
difxmessage (0.7), mark5access (1.3.3), mk5daemon (1.2), mpifxcorr
(1.5.2), vex2difx (1.0.2), and vis2screen (0.1). The new features
include:

#. Support unmodulated VLBA format data with new pseudo-format “VLBN”

#. ``mpifxcorr`` now warns when difxmessage is in use so the user knows
   why no messages appear on the screen

#. New utility ``difxcalculator`` in the ``difxio`` package

#. eVLBI support within ``vex2difx``

#. Vastly improved real-time correlation monitoring

#. New utility ``diffDiFX.py`` to compare two DiFX output files

#. Improved and more consistent error messages (and some of them are now
   documented!)

#. ``vex2difx`` now operates in strict parsing mode by default

#. Additional user feedback to indicate suspicious or bad ``.polyco``
   and ``.v2d`` files

#. ``calcif2`` warns if any NaNs or Infs are produced

#. Clock adjustments are easier now with *deltaClock* and
   *deltaClockRate* parameters in the ``vex2difx`` antenna settings

.. _bugs-fixed-3:

Bugs fixed
~~~~~~~~~~

#. Improve timestamp precision (thanks to John Morgan)

#. The ``vlog`` program (used at NRAO only, I think) misparsed the pulse
   cal information in some cases

#. Fixed memory leak in ``difx2fits`` when combining a large number of
   jobs

#. Improved FXCORR simulation mode in ``vex2difx``

#. Mark5 directory reading systematically generates unique names for all
   scans even when two scans have the same name

#. Improve reporting of Mark5 errors during playback and change alert
   severity to be more appropriate

#. Don’t overblank certain Mark4 modes (thanks to Sergei Pogrebenko for
   the bug report)

#. Vex ‘data valid’ period now properly respected

#. Vex clock table tolerance issue corrected

#. Changes in Mark5 mode should be safer (note that currently
   ``vex2difx`` never exercises multiple modes in a single job)

#. When making the cross spectrum sniffer plots, respect the reference
   antenna

#. Improved pulsar polynomial file error checking is performed

#. Amplitude-phase-delay (APD) sniffer plots always have refant first
   when multiple refants are supplied

#. Project name should now appear on sniffer APD plots

#. Mark5 units now send status information even when no playback is
   occuring (eliminating the incorrect ``LOST`` state issue as displayed
   in the DOI)

.. _known-problems-1:

Known problems
~~~~~~~~~~~~~~

#. Extensive use in VLBA operations has shown that occasional data
   dropouts of one or more antenna, sometimes in a quasi-repeatable
   manner, affect completeness of some jobs. It is not clear exactly
   what the cause is at this point, however its cure is a high priority.

#. Loss of a few FFTs of data will occur in rare circumstances.

#. Clock accountability is poor when jobs containing multiple clock
   models for antennas are combined.

DiFX 1.5.2 was made available via subversion on Jan 20, 2010.

DiFX 1.5.3
----------

DiFX 1.5.3 is mainly intended as a bug fix update to version 1.5.2,
though some new features have made their way into the codebase. This
version of DiFX comes with the following components (and versions):
calcif2 (1.3), calcserver (1.3), difx_db (1.13; NRAO-only), difx2fits
(2.6.2), difx2profile (0.2), difxio (2.12.2), difxmessage (7.2),
mark5access (1.3.4), mk5daemon (1.3), mpifxcorr (1.5.3), vex2difx
(1.0.3), and vis2screen (0.2). Many changes are motivated by issues
found running DiFX full time in Socorro.

The new features include:

#. Mark5 directory (``.dir``) files can contain ``RT`` on the top line
   to indicate the need to play back using *Real-Time* mode.

#. ``difxqueue`` (NRAO only) now takes an optional parameter specifying
   the staging area to use.

#. New Mark5 diagnostic programs (``vsn`` and ``testmod``) introduced to
   wean off the use of the ``Mark5A`` program.

#. ``mk5daemon`` can now mount and dismount USB and eSATA disks through
   mk5commands.

#. ``mk5cp`` now makes the destination directory if it doesn’t exist.

#. ``mk5daemon`` will now warn if free disk space is getting low.

#. ``db2vex`` (NRAO only) now allows field station logs to be provided.
   As of now, only media VSNs are extracted.

#. Playback off Mark5 units has been made more robust with better error
   reporting.

#. New utility ``m5fold`` that can be used to look at repeating signals
   in baseband data total power (e.g., switched power)

#. ``vex2difx`` now supports job generation in cases where upper side
   band was observed at one antenna and lower sideband at another.

.. _bugs-fixed-4:

Bugs fixed
~~~~~~~~~~

#. Don’t unnecessarily drop any FFTs of data.

#. Sub-integrations longer than one second could cause integer
   overflows.

#. Fix bug in ``vex2difx`` where jobs were not split at clock breaks.

#. ``difx2fits`` was guilty of incorrect clock accoutability after a
   clock change at a station when merging multiple jobs. Worked around
   by not allowing such jobs to merge.

#. ``db2vex`` (NRAO only) warns when more than one clock value is found
   for an antenna.

#. Mark5 unit bank switches now routinely call ``XLRGetDirectory()`` to
   work around a newly discovered bug in the StreamStor software.

#. A couple possible memory leaks in the mark5access library were fixed
   (thanks Alexander Neidhardt and Martin Ettl).

#. Lots of compiler warnings quashed (mostly of the “unused return
   value” kind).

#. Olaf Wuchnitz found two FITS file writing problems in ``difx2fits``
   dating back to code inherited from FXCORR!

#. Two more digits are retained for the time and one more digit is
   retained for amplitude information in the ``.apd`` and ``.apc``
   sniffer files.

#. Some bugs related to replacement of special characters by “entities”
   in XML messages are fixed.

#. New traps are in place in many places to catch string overruns.

#. Fix for writing ``.calc`` files with more than one ephemeris driven
   object.

#. ``vex2difx`` would get *very* slow due to constantly sorting a list
   of events. Now this list is only sorted when necessary, drastically
   speeding it up.

#. The ``RCfreqId`` parameter in the difxdatastream structure (in
   difxio) was used with two different meanings that are normally the
   same. Cases where they differred caused exceptions. Fixed in difxio
   and difx2fits. (Thanks to Randall Wayth for leading to the discovery)

#. ``difx2fits`` would assign a bogus ``.jobmatrix`` filename when not
   running the sniffer.

#. ``vex2difx`` could get caught in an infinite loop when making jobs
   where two disk modules had zero time gap.

#. ``difx2fits`` used a bad config index when making the puslar GM table
   when multiple configs were present.

#. Within ``mpifxcorr`` an extra second was added to the validity period
   for polycos to ensure no gap in coverage.

#. ``mk5dir`` would add correct the date improperly for Mark4 formats
   after beginning of 2010.

#. Lots of fixes for building FITS files out of a subset of baseband
   recorded channels.

#. FITS files now support antennas with differing numbers of
   quantization bits.

#. Lots of Mac OS/X build issues fixed.

DiFX 1.5.3 was released on April 16, 2010.

DiFX 1.5.4
----------

DiFX 1.5.4 is likely the last 1.5 series formal release of DiFX, though
an additional release could be made if demand is there.

The new features include:

#. ``difx2fits`` can now produce FITS files with only a subset of the
   correlated sub-bands.

#. ``difx2fits`` can be instructed to sniff on an arbitrary timescale.

#. The ``makefits`` wrapper for ``difx2fits`` now respects a -B option
   for phase bin selection.

#. ``difxio`` has improved checking that prevents merging of jobs with
   incompatible clocks.

#. ``difxio`` now maintains a separate clockEpoch parameter for each
   antenna.

#. ``difxStartMessage`` now contains DiFX version to run, allowing
   queued jobs to be run under different DiFX versions.

#. The curses utilities ``mk5mon`` and ``cpumon`` now catch exceptions
   and can be resized without infecting the terminals they are run in.

#. New sub-library called mark5ipc added that provides a semaphore lock
   for Mark5 units.

#. The ``testdifxmessagereceive`` utility can now filter on message
   types.

#. Support for SDK9 throughout (e.g., in tt mpifxcorr, ``mk5daemon``,
   and other utilites).

#. Support for new Mark5 module directory formats (Haystack Mark5 memo
   81).

#. Several new Mark5 utilities to make up for Mark5A functionality that
   will not longer be available: ``vsn``, ``testmod``, ``recover``,
   ``m5erase``.

#. ``mk5cp`` can now copy data based on byte range.

#. Many programs directly talking to the StreamStor card of Mark5 units
   use WATCHDOG macros for improved diagnostics when problems occur.

#. More protection against incomplete polyco files added to
   ``mpifxcorr`` (Note: should add this to ``vex2difx`` as well).

#. The GUI can now spawn different DiFX versions at will through the use
   of difxVersion parameter in the DifxStartMessage and wrapper scripts.

#. ``difx2fits`` can now convert LSB to USB for matching purposed. When
   used, all LSB sub-bands must have corresponding USB sub-bands on one
   or more other antenna.

#. ``mark5access``-based utilities (e.g., ``mp5spec``) can now read from
   stdin.

#. New utility ``mk5cat`` can send data on a Mark5 module to stdout.

.. _bugs-fixed-5:

Bugs fixed
~~~~~~~~~~

#. Only alt-az telescopes received the correct model. Fixed. Note that
   CALC and FITS-IDI don’t have a good match between their sets of
   allowed mount types.

#. ``difx2fits`` now properly propagates quantization bits on a per
   antenna basis.

#. Logic errors in ``difxio`` would confuse ``difx2fits`` in cases where
   different antennas use different frequency setups. Fixed.

#. Weights are blanked in ``difx2fits`` prior to populating each record,
   preventing screwy weights for unused sub-bands.

#. ``vex2difx`` would sometimes hang or not converge on job generation.
   Fixed.

#. ``vex2difx`` now doesn’t assume source name is same as vex source def
   identifier.

#. ``mpifxcorr`` generated corrupted weights and amplitudes when
   post-FFT fringe rotation was done. Fixed.

Known bugs
----------

#. Tweak Integration Time feature of ``vex2difx`` often does the wrong
   thing.

DiFX 1.5.4 was released on October 12, 2010.

DiFX 2.0.0
----------

DiFX 2.0.0 is based on an upgraded ``mpifxcorr`` that breaks ``.input``
file compatibility with the 1.0 series. This new version will allow more
flexible correlation of mis-matched bands and correlation at multiple
phase centers along with general performance improvements. Development
of the 2.0 capabilities will occur in parallel with the 1.0 series
features.

New features
~~~~~~~~~~~~

#. Pulse cal extraction in ``mpifxcorr``.

#. Massive multi-phase center capabilitiy.

#. New utitility ``zerocorr`` added.

#. External pulse cal extraction utility ``m5pcal`` added.

#. DiFX output format is all-binary, meaning speed and disk savings

.. _known-bugs-1:

Known bugs
----------

#. Zoom band support has multiple problems.

DiFX 2.0.0 was released on October 12, 2010.

DiFX 2.0.1
----------

DiFX 2.0.1 is a bug fix and clean-up version in response to numerous
improvements to DiFX 2.0.0. There are a number of new features as well.

.. _new-features-1:

New features
~~~~~~~~~~~~

#. New utility ``checkmpifxcorr`` to validate DiFX input files

#. Switched power detection in ``mpifxcorr``

#. Early multi-thread VDIF format support

#. RedHat RPM file generation for some packages (can extend to others on
   request)

#. Improvements to method of selecting which pulse cal tones get
   propagated to FITS

#. Initial complex sampling support

#. Improved locking mechanism for direct mark5 access (using IPC
   semaphores; difxmessage)

Bug fixes
~~~~~~~~~

#. Fix model accountability bug in difx2fits when combining jobs

#. Numerous fixes for zoom bands (in ``mpifxcorr``, ``vex2difx`` &
   ``difx2fits``)

#. Native Mark5 has improved stability for cranky modules

#. Numerous fixes for DiFX-based phase cal extraction (mostly in
   ``difx2fits``, mostly for multi-job)

#. Fractional bit correction for a portion of lower sideband data got
   broken in difx 2.0.0. Fixed.

#. Migrate ``difxcalculator`` to DiFX 2; was not complete for DiFX 2.0.0

DiFX 2.0.1 was released on June 24, 2011.

DiFX 2.1
--------

.. _new-features-2:

New features
~~~~~~~~~~~~

#. Mark5-based correlation: easy access to S.M.A.R.T. data (can be
   viewed with getsmart)

#. Mark5-based correlation: emit multicast message containing drive
   statistics after each scan

#. VSIS interface added to mk5daemon

#. Support for non power-of-2 FFT lengths

#. New utilities: ``mk5map`` (limited functionality), ``fileto5c``,
   ``record5c``

#. Remote running of ``vex2difx`` from ``mk5daemon``

#. Multithread VDIF support enabled for the data sources FILE and
   MODULE, including stripping of non-VDIF packets

#. New features added to existing utilities:

   -  ``mk5cp``: copy without reference to a module directory

   -  ``mk5cp``: ability to send data over ssh connection

   -  vsn: get SMART data from disk drives

#. e-Control source code analysis (Martin Ettl, Wettzell)

#. Restart of correlation is now possible

#. ``difx2fits``: -0 option to write minimal number of visibilities to
   FITS

#. ``difx2fits``: write new RAOBS, DECOBS columns in source table

#. tweakIntTime option to ``vex2difx`` has been re-enabled

#. ``diffDiFX.py`` can now cope with two files that don’t have exactly
   the same visibilities (i.e., some visibilities are missing from one
   file)

#. ``plotDiFX.py`` and ``plotDynamicSpectrum.py`` now have better
   plotting and more options

#. New FAKE datastream type for performance testing

#. Espresso, a lightweight system for managing disk-based correlation,
   has been added to the DiFX repository.

#. Option to correlate only one polarization has been added.

#. ``mk5dir`` can now produce ``.dir`` file information for VDIF
   formatted data.

#. Add NRAO’s sniffer plotters to the repository.

.. _bug-fixes-1:

Bug fixes
~~~~~~~~~

#. LBA format data now scaled roughly correctly (removing the need for
   large ACCOR corrections).

#. There was a bug when xmaclength was :math:`>` nfftchan for pulsar
   processing. This has been corrected.

#. guardns was incorrectly (overzealously) calculated in mpifxcorr.

#. ``Mk5DataStream::calculateControlParams: bufferindex>=bufferbytes``
   bug fixed.

#. Low weight reads could result in uninitialized memory; fixed.

#. Streamstor ``XLRRead()`` bug work-around installed several places
   (read at position 0 before reading at position :math:`> 0`). This is
   thought not to be needed with Conduant SDK 9.2 but the work-around
   has no performance impact.

#. Fix to pulse calibration data ordering for LSB or reordered channels.

#. Pulse cal amplitude now divided by pulse cal averaging time in
   seconds.

#. Pulse cal system would cause crash if no tones in narrow channel.
   Fixed.

#. Zoom band support across mixed bandwidths (see caveat below).

#. Fix for spurious weights at end of jobs (untested…)

#. Mixed 1 and 2 bit data are handled more cleanly

#. mpifxcorr terminates correctly for all short jobs. Previously it hung
   for jobs with a number of subints between nCores and :math:`4 \times`
   nCores

#. Correctly scale cross-correlation amplitudes for pulsar binning when
   using ``TSYS`` :math:`>0` (accounts for varying number of samples per
   bin c.f. nominal)

#. Lower side-band pulse cal tones had sign error. Fixed.

DiFX 2.1 was released on May 25, 2012.

DiFX 2.1.1
----------

DiFX 2.1.1 was a minor patch release to fix a scaling issue with
autocorrelations of LBA-format data in mpifxcorr.

DiFX 2.1.1 was committed as a patch to DiFX 2.1 on June 7, 2012.

DiFX 2.2
--------

.. _new-features-3:

New features
~~~~~~~~~~~~

#. ``calcif2``: ability to estimate delay polynomial interpolarion
   errors

#. Support for a “label” identifier for a local version of DiFX that
   will help discriminate exact version used.

#. Faster Mark5 directory reading

#. Faster VDIF corner turning through customized bit shifting functions

#. ``mpifxcorr`` can now be built without Intel Integrated Performance
   Primitives, though resulting in a slower correlator.

#. ``vdifio``: several new VDIF manipulation and processing utilities
   added: ``vmux``, ``vsum``, ``vdifd``, ``vdifspec``, ``vdiffold``,
   ``vdifbstate``

#. ``difxbuild``: a new installation program

#. ``difxspeed``: a program to benchmark and help optimize DiFX

.. _bug-fixes-2:

Bug fixes
~~~~~~~~~

#. Mutex locking bugfix for very short jobs

#. Prevent MODE errors when a datastream runs out of data well before
   the end of a job

#. ``calcif2``: fix azimuth polynomial generation in case of wrap

#. Fix for FITS file generation for mixed sideband correlation

#. ``difx2fits`` now uses appropriate gain tables for S and X band in
   S/X experiments (Thanks to James Miller-Jones for reporting)

#. ``difx2fits``: correct pcal, weather, tsys and flag data for
   observations crossing new year

#. Fixed scaling of autocorrelations for LBA format data

#. 0.5 ns wobble in delays for 2 Gbps Mark5B data fixed

#. Fix bug preventing subintegrations longer than 1 second. Now 2
   seconds is allowed (this limit comes from signed integer number of
   nanoseconds).

#. Weights corrected in cases where two setups differening only by pcal
   setup were correlated against each other

#. Quashed data and weight echos that would occur for about 1
   integration at the beginning of each scan for datstreams that ran out
   of data before end of job.

#. The multicast (diagnostic) weights were low or zero in case of
   frequency selection (zoom band or freqId selection). Fixed.

#. ``mark5access``: fix (non)blocking issue when receiving data from
   *stdin*

DiFX 2.2 was released on June 12, 2013.

DiFX 2.3
--------

.. _new-features-4:

New features
~~~~~~~~~~~~

#. mpifxcorr: LO offsets are now corrected in the time domain when
   fringe rotation is also done in the time domain (the usual mode),
   allowing considerably larger LO offsets without decorrelation

#. mpifxcorr: Working polarization dependent delay and phase offsets

#. mpifxcorr: Experimental linear2circular conversion

#. mpifxcorr: Complex Double sideband (RDBE/Xcube) sampling support
   (Note: things are not perfect here; wait for 2.4 for real use)

#. mpifxcorr: new file/Mark5 based VDIF/Mark5b datastream (faster and
   more robust)

#. mpifxcorr: implement work-around for buggy kernel-driver
   combinations; Mark5 read sizes >20 MB now allowed

#. utilities: some new command line tools for Mark5B and VDIF files
   (vsum, mk5bsum, vmux, mk5bfix)

#. new options for passing calibration (Walter B: memo forthcoming)

#. Hops updated to version 3.9

.. _bug-fixes-3:

Bug fixes
~~~~~~~~~

#. mpifxcorr: Datasteam buffer send size now calculated correctly for
   complex sampled data

#. mpifxcorr: Avoid very rare bug where combination of geometric delay
   and data commencing mid-subint meant one invalid FFT might be
   computed

#. mpifxcorr: multicast weights are now computed correctly for
   mixed-sideband correlation

#. mpifxcorr: fixed bug where some autocorrelations were not saved in a
   mixed-sideband correlation

#. mpifxcorr: fixed bug where send size could be computed incorrectly by
   1-2 bytes for

#. Mark4/VLBA/Mark5B/VDIF formats, potentially resulting in very small
   amounts of data loss

DiFX 2.3 was released on January 18, 2014.

DiFX 2.4
--------

.. _new-features-5:

New Features
~~~~~~~~~~~~

#. mpifxcorr

   -  Support a FAKE correlation mode for multi-threaded VDIF.

   -  The mpifxcorr produced PCAL files have had a format change that
      allows unambiguous interpretation across all use cases.

   -  Add network support (TCP, UDP and Raw Ethernet) for multi-threaded
      VDIF: 1. TCP and UDP variants not tested yet; 2. raw Ethernet
      variant is used for the VLITE project.

   -  Support updated Mark5 module directories.

   -  Better checking that Mark5 data being processed matches what is
      expected.

   -  Improved Mark5B decoding: 1. Mark5B data streams are now filtered
      for extra or missing data; 2. packets with invalid bit (actually
      the TVG bit) set replace missing data; 3. this means any valid
      Mark5B data with the TVG bit set will not correlate.

   -  Information about each Mark5 unit used in “native mode” is emitted
      at start of jobs so it can be logged.

   -  Ultra-low frame rate VDIF data was affected by allowing a long
      “sort window” in the VDIF multiplexer. This has been reduced to 32
      frames and seems to work fine for all bandwidths now.

#. difx2fits

   -  Slightly improved compliance with the FITS-IDI convention:
      1. invalid Tsys values become NaN, not 999; 2. populate ``DELTAT``
      keyword in ModelComps table.

#. difxio

   -  Support for X/Y polarization correlation. Many fundamental issues
      with linear polarization remain though: 1. this does not support
      in a meaningful way Linear*Circular correlations; 2. there is a
      terminology gap in many bits of software and file formats that
      confuses X/Y with H/V polarization bases; 3. the intent of this
      support is for short baselines (VLITE).

#. mark5access

   -  Support for “d2k” mode in Mark5B format (swapped sign and mag
      bits).

   -  fixmark5b() function fixed for case that fill pattern is seen at
      the 1 second transition.

   -  Make use of the TVG bit as an “invalid frame” indicator for Mark5B
      data.

   -  m5bstate: support complex sampled data

#. mk5daemon

   -  New utility mk5putdir: reads a binary file and replaces a Mark5
      directory with it.

   -  mk5dir: when reading the directories, saves a copy of the binary
      representation in case it is needed later (perhaps via mk5putdir)

   -  Reworked mark5 module directory support, including support for
      many new variants of the directory format.

   -  mk5erase will save a “conditioning report” to
      ``$MARK5_CONDITION_PATH`` if that environment variable is set.

#. vdifio

   -  Fairly large change to the API. Please read the ChangeLog for
      details.

#. vex2difx

   -  Respect the record enable bit in the SCHED block. If that value is
      0 no correlation will be attempted for that antenna.

   -  Bug fixes preventing some LSB/zoom bands from being correlated.

   -  Complex data type and number of bits are now read from vex file.

.. _bug-fixes-4:

Bug fixes
~~~~~~~~~

#. A “jitter” of 0.5 ns when using 2Gbps Mark5B format was fixed. A fix
   was back-ported to DiFX 2.3.

#. A similar jitter was corrected for high frame rate VDIF (problem
   identified by the VLITE project)

#. Fix case of intermittant fringes that was due to incorrect assumption
   about the sizeof(unsigned long): 32 bits on a 32-bit system vs. 64
   bits on a 64-bit system. Some other variable types were changed for
   long term type safety

#. Fix off-by-one in correlation using LSB and zoom bands together.

DiFX 2.5.1
----------

.. _new-features-6:

New features
~~~~~~~~~~~~

#. Innitial support for correlating Mark6. This is still much a work in
   progress.

#. Multiple datastreams per antenna supported via ``vex2difx``

#. New delay model program: difxcalc11.? No longer requires calcserver.

#. Support for more than 6 days of EOP values.

#. “Union mode” in difx2fits allows merging of correlation output that
   uses different setups. Some restrictions apply. Designed for GMVA and
   RadioAstron use.

#. Improved VDIF support: wider range of bits/threads, support for
   multi-channel, multi-thread VDIF, support for complex multi-thread
   VDIF

#. Support for new VDIF Extended Data Version 4 which is useful for
   multiplexed VDIF data. See:
   http://vlbi.org/vdif/docs/edv4description.pdf

#. Python bindings for vdifio and mark5access

#. mpifxcorr: per-thread weights implemented

#. Automatic selection of arraystride by mpifxcorr if set to zero; this
   is done per-datastream.? Very useful for correlation of ALMA data or
   others with non-standard sample rates.

#. Automatic selection of xmacstride by mpifxcorr if set to zero

#. Automatic selection of guardns by mpifxcorr if set to zero

#. mpifxcorr can now operate with unicast messages instead of multicast.
   Useful in some situations where multicast is not supported.

#. New “dirlist” module/file directory listing format.

#. ``mk5cp`` append mode to resume interrupted copy

#. ALMA support in HOPS: non-power-of-two FFTs, up to 64 freq. channels,
   full linear/circular/mixed polarization support

#. HOPS improvemetns for VGOS through improved manual phase cal support

#. New package: polconvert. Used to post-correlation convert from linear
   to circular polariations

#. New package: autozoom. Helps a user develop ``.v2d`` file content
   when setting up complicated zoom band configurations.

#. New package: datasim: generate baseband data suitable for simulated
   correlation

#. Improved error reporting in many places

.. _bug-fixes-5:

Bug fixes
~~~~~~~~~

#. fix for incorrect reporting of memory use by mpifxcorr (needed longer
   int sizes)

#. dataweights would sometimes be incorrect after abrupt ending of data
   from a datastream.

#. FITS-IDI files produced by difx2fits more standards compliant; fix
   problem that caused AIPS task VBGLU to fail.

#. Several segfaults across a number of programs/utils now are caught
   and provide useful feedback.

Caveats
~~~~~~~

#. Various changes made between DiFX 2.4 and 2.5 are not API-compatible.
   Please don’t mix packages from these two releases. If you have
   non-DiFX software that links against the DiFX libraries, be sure to
   recompile them. A small number of changes may result in need to
   restructure such code.

#. Unlike previous DiFX releases, each tagged version will be its own
   SVN copy. If the number of minor releases within the 2.5 series gets
   large, some (reversible) pruning of the SVN repository may occur.
   There has been some debate about the best tagging strategy: bring any
   strong opinions to the Bologna meeting, where further changes to
   release and tagging policies can be discussed if needed.

DiFX 2.5.2
----------

.. _bug-fixes-6:

Bug fixes
~~~~~~~~~

#. Fixes for the HOPS ’rootid’ rollover. The new rootcode is a
   conventional base-36 timestamp in seconds from the start of the new
   epoch (zzzzzz in the old epoch). This will last until 2087.

#. Major fixes/improvements to PolConvert for use with ALMA by the EHTC
   and GMVA.

DiFX 2.5.3
----------

Updates
~~~~~~~

#. genmachines

   -  r8264 genmachines and mark6 datastream updates

   -  r8357 add mark6 activity message to mark6 datastream

   -  r8409 allow multiple nodes to serve as datastream nodes for FILE
      based-data in the same location

#. hops 3.19 new features

   -  increased the number of allowed frequency “notches” to ridiculous
      levels

   -  an “ad hoc” data flagging capability to allow improved time /
      channel data selection for fringing

   -  a capability to dump all the information on the fringe plots into
      ascii files for “roll your own” plotting

   -  removed obsolete ``max_parity``

   -  introduced ``min_weight`` (to discard APs with very little
      correlated data in support)

   -  vex2xml, a program that converts VEX (v1.5) into XML to allow easy
      parsing via standard XML parsers.

   -  added ``type_222`` to save control file contents, enabled by
      keyword ``gen_cf_record``.

   polconvert to v1.7.5 (mostly minor bug fixes and robustifications)

DiFX 2.5.3 was released on March 28, 2019.

DiFX 2.5.4
----------

.. _new-features-7:

New features
~~~~~~~~~~~~

#. HOPS updated to 3.22

#. Vex2difx and difxio

   -  permit H and V polarization labels (but need trunk difx2fits if
      these are to be propagated into “enhanced” FITS-IDI)

   -  support the v2d parameter ’exhaustiveAutocorrs’

   -  support Mark6 MSNs i.e. MSNs that contain ’%’

#. Difx2mark4

   -  support PCal data from multi-datastream correlation

   -  option ’-e expt_nr’ additionally propagates the experiment number
      into the generated root files

   -  backported the options -w for mixed-bandwidth data, and -g for
      filtering freq groups

#. Genmachines updated to support Mark6 host auto-detection

#. Includes copies of more recent utilities: packHops.py,
   distFourfit.py, fplot2pdf, plotResiduals.py

.. _bug-fixes-7:

Bug fixes
~~~~~~~~~

#. Mark6 native playback fixed to support other than VLBA VDIF recording

#. Vdifio

   -  fix excessively long station name printout in printVDIFheader

   -  fix to mk6gather to not crop a scan

   -  resync code extended with further VDIF frame sizes that are common
      in VGOS

#. Mpifxcorr fix for complex data (VDIFC) PCal extraction for bands with
   other than 16 tones

#. Mpifxcorr fix for IVS-related 5/10MHz pcal issue specific to those
   channels where first pcal is at baseband 2.01 MHz

#. Difx2mark4

   -  fix count of total bands and polarizations for multi datastream
      datasets, solving an issue in HOPS post-processing

   -  fix parallactic angle calculation error in first t303 record

   -  fix to permit Tab characters in VEX

#. Mpifxcorr no longer segfaults after “exiting gracefully” message upon
   missing data files

#. Tkinter add-on package renamed from tkinter to kinter_difx to avoid
   name collision

#. Install-difx now works under Python 3 and supports the option
   –withmark6meta

#. Startdifx no longer piles up difxlog processes

.. _caveats-1:

Caveats
~~~~~~~

Support for Complex VDIF is incomplete in 2.5.3 and 2.5.4, both treat
Complex VDIF LSB as USB data. This handling was retained in 2.5.4 for
VGOS compatibility reasons in order to have all-LSB(-mislabeled) data
products, and avoid postprocessing issues with mixed USB and LSB data
products. One should not correlate any actual Complex VDIF LSB
recordings - this produces no fringes. The issue affects only VDIFC
(complex VDIF), not VDIF nor other formats.

DiFX 2.5.4 was released on August 27, 2021.

DiFX 2.5.5
----------

.. _new-features-8:

New features
~~~~~~~~~~~~

#. genmachines extended to support Mark6 with multiple expansion chassis

#. update polconvert scripts

.. _bug-fixes-8:

Bug fixes
~~~~~~~~~

#. genmachines fix to support December i.e. doy 335 and later

#. vex2difx

   -  fix freqClockOffs and loOffsets parameters to not expect more
      values than recorded frequencies

   -  removed obsolete warning about 10 MHz PCal not being supported

DiFX 2.5.5 was released on October 24, 2022.

DiFX 2.6.1
----------

.. _new-features-9:

New features
~~~~~~~~~~~~

#. Improved VDIF support

   -  Increased robustness in processing VDIF data with many gaps

   -  Improvements in processing VDIF with frame sizes very different
      from 5000 bytes

   -  New in-line reordering functionality via vdifreader…() functions;
      allows operation on more highly skewed VDIF files

#. mpifxcorr ``.input``, ``.calc``, ``.threads``, and pulsar files are
   now only read by the head node

#. mpifxcorr can be provided a new stop time via a DifxParameter
   message; results in clean shutdown at that time.

#. mpifxcorr can extract pulse cals with tone spacing smaller than 1 MHz

#. Support for Intel Performance Primitives version > 9 (specfically IPP
   2018 and 2019)

   -  These newer IPP versions are more readily available than earlier
      versions

#. Improved support for Mark6 playback

   -  Mark6 activity messages in difxmessage

   -  Support in genmachines with updated mk5daemon

   -  Support playback of Mark5B data on Mark6

   -  New and improved mark6 utilities

#. difx2fits: populate antenna diameters and mount types for antennas
   known to the difxio antenna database

#. difx2fits: in verbose mode, explain why files are being split

#. difx2fits: new options for merging correlator jobs run with different
   clock models

#. vex2difx: new parameter ``exhaustiveAutocorrs`` can be used to
   generate cross-hand autocorrelations even when the two polarizations
   for an antenna come from different datastreams

#. difx2mark4: support multiple bandwidths in one pass

#. hops: to rev 3.19 (see notes on 2.5.3 above for details on several
   new and useful features)

#. polconvert: to rev 1.7.5 (see notes on 2.5.3 above for details)

.. _bug-fixes-9:

Bug fixes
~~~~~~~~~

#. mpifxcorr: Retry on NFS open errors of kind: “EAGAIN Resource
   temporarily unavailable”

#. mpifxcorr: Fix weight issue when the parameter ``nBufferedFFTs``
   :math:`> 1`

#. startdifx/genmachines: Fixes for cases when multiple input files are
   provided

#. Python 2 scripts now explicitly call python2

#. vex2difx: allow up to 32 IFs (was 4) and warn when this is exceeded

#. vex2difx: support units in the clock rate (e.g., usec/sec); in
   general support time in the numerators.

#. Sun RPC is on its way out; support for “tirpc” added to calcif2 and
   calcserver

.. _caveats-2:

Caveats
~~~~~~~

#. Moved “mark6gather” functions from vdifio to mark6sg; this changes
   the order of dependencies!

#. Various changes made between DiFX 2.5 and 2.6 are not API-compatible.
   Please don’t mix packages from these two releases. If you have
   non-DiFX software that links against the DiFX libraries, be sure to
   recompile them. A small number of changes may result in need to
   restructure such code.

#. There is some suspicion that correlation of very narrow bandwidth
   VDIF modes on Mark6 media can result in premature termination of
   datastreams.

#. The ``.threads`` file must now exist; previously (before the change
   to only have manager read these files), a missing ``.threads`` file
   would cause each core process instance to have a single thread.

#. difx_monitor won’t compile with IPP :math:`\ge 9`

DiFX 2.6.1 was released on August 28, 2019.

DiFX 2.6.2
----------

.. _new-features-10:

New features
~~~~~~~~~~~~

#. Python parseDiFX package added

.. _updates-1:

Updates
~~~~~~~

#. HOPS updated to version 3.21

#. PolConvert updated to version 1.7.8

#. Former FTP access to CDDIS servers changed to FTP-SSL (geteop.pl)

.. _bug-fixes-10:

Bug fixes
~~~~~~~~~

#. mpifxcorr: Fix correlation of Complex LSB data, restore fringes.
   Note: DiFX 2.5.x and 2.6.1 treated Complex LSB as if Complex USB,
   while Trunk prior to r9647 05aug2020 treated LSB nearly correctly
   except for a off-by-one channel bug

#. difx2mark4: Fix seg-fault in createType3s.c when a station has only a
   single entry in the PCAL file

#. difx2mark4: Remove unneeded debugging statement (calling
   d2m4_pcal_dump_record())

#. difx2mark4: Update createType3s.c to add support for DiFX PCAL files
   generated from station data where each data-stream thread resides in
   a separate file (multi-datastream support). This separates the code
   reading the PCAL files from the code filling the type-3 records so
   that tone records from multiple data streams can be merged before
   populating the type-309s

#. difx2mark4: Update createType3s.c to remove support for DiFX
   version-0 PCAL files

#. difx2mark4: Add support for 10 MHz p-cal tone spacing (needed by VGOS
   at Yebes)

#. difx2mark4: Significantly increase hardcoded array sizes
   (difx2mark4.h: NVRMAX 8M, MAX_FPPAIRS 10k, MAX_DFRQ 800) as required
   for EHT2018

#. difx2mark4: Fix a bounds check, permit tabs in VEX file

#. difx2fits: Fix FITS PH table having missing or superfluous pcal
   records when one correlates multi-datastream antennas, or not all
   recorded frequencies, or multiple zooms per recorded frequency

#. mark6gather: Fix poor weights in native Mark6 correlation for VDIF
   frame sizes not equal to 5032 bytes

#. difxio: Fix PCal tone frequency rounding bug on some platforms

#. difxio: Cope with recorded bands that lack PCal tones, e.g., 200 MHz
   PCal spacing of KVN with say 32 MHz recorded bands

#. calc11: Dave Gordon provided ocean loading params at EHT stations

#. calc11: Increased the number of field rows supported in .calc files

#. vex2difx: Fix internal merge of SamplingType (real, complex) when
   info found in VEX and/or v2d file

#. Minor changes to oms2v2d and vexpeek

#. More IPP versions supported

#. Minor issues with vis2screen fixed

#. Fixed build failure with gcc defaults

#. Python3 support in many/most places

.. _caveats-3:

Caveats
~~~~~~~

#. difx2mark4: Some LSB-LSB baselines do not get converted in
   mixed-sideband correlation setups (DiFX 2.6.1, 2.6.2); if affected,
   use difx2mark4 2.5.3 with –override-version. A bugfix is pending for
   DiFX 2.6.3 later this year.

#. difx2mark4: Performance regression with p-cal files, conversion of
   p-cal data may take noticeably longer than before

#. calcserver and difxcalc11: With the latest versions of gfortran (10.1
   or newer) you will need to uncomment the line with
   -fallow-argument-mismatch line in the environment setup in order to
   compile. Users who do this should be alert to possible issues.

DiFX 2.6.2 was released on September 11, 2020.

DiFX 2.6.3
----------

This version has never been officially released.

DiFX 2.7.1
----------

This version has never been officially released. The 2.7 series was used
by the Event Horizon Telescope. The 2.7 series is a feature branch
derived from DiFX 2.6.2 with support added for outputbands.

DiFX 2.8.1
----------

The DiFX 2.8 series collected updates and fixes from many DiFX users and
is the first version universally usable on VGOS, EHT, and “traditional”
VLBI data. Because the 2.7 series was never formally released, the notes
below include all changes since the 2.6 series.

.. _new-features-11:

New features
~~~~~~~~~~~~

#. Support for appending contiguous subbands together to create
   outputbands

#. Initial support for vex2

#. Complete conversion to Python 3

#. Support for CODIF format

#. mark5access programs: error output to stderr to allow piping

#. Support for IPP 2019 series

#. Experimental support for Vienna Mapping Functions in calcif2

#. Some features (within mpifxcorr and vex2difx) that provide additional
   options for real-time correlation

#. Espresso modification to work with singularity (or docker) image

.. _updates-2:

Updates
~~~~~~~

#. HOPS updated to version 3.24

#. PolConvert updated to version 2.0.3

#. Several new VDIF decoders and corner turners introduced to widen
   range of support

#. Improved support for more than 2 bits per sample (see
   https://library.nrao.edu/public/memos/vlba/up/VLBASU_52.pdf)

#. Mark6: support for larger number of expansion units

#. Add a few more stations to ocean loading tables within difxcalc

#. Several new options in the tabulatedelays program

#. difxio programs (e.g., vex2difx and difx2fits) can support project
   codes up to 24 characters long (was 7)

.. _bug-fixes-11:

Bug fixes
~~~~~~~~~

#. difx2mark4: fix a parallactic angle calculation bug

#. vdifmux() function had some logic errors causing bad performance in
   gappy data; fixed.

#. In subarray cases difx2fits could provide incorrect pulse cal values;
   fixed.

#. mark5access bug fix to prevent crash

#. mpifxcorr: fix bug affecting data weights when nBufferedFFTs
   :math:`> 1` and datastreams weight :math:`< 1`

#. mpifxcorr could end early due to bug in receiving a DifxParameter
   message; fixed.

#. some of the vdif python utilities (e.g., vdifd) had errors in parsing
   the command line: nbit and offset were swapped

.. _caveats-4:

Caveats
~~~~~~~

#. Outputbands support can only work on one frequency setup at a time.

#. The DiFX-2.8 series may be the last to support Mark5 recordings.

Many additional non-user-visible improvements were made to the code as
well as many small user-visible improvements that do not warrant
specific mention. The ``Changelog`` files that are packaged with most of
the DiFX software modules contain more detailed lists of code changes.
DiFX 2.8.1 was released on XXX 2023. A wide array of testing has been
done and this version is considered ready to be adopted by all DiFX
users.

Features left to implement
==========================

Here is a list of other features to add to DiFX that are not directly
tied to any particular version:

#. Support for K5 format

#. Pulsar bins with proper output format

#. Space VLBI support

DiFX and AIPS
-------------

Only one task in AIPS, ``FITLD``, has to deal with the
telescope/correlator specific aspect of the FITS-IDI files that the VLBA
correlator and DiFX generate. The FITS-IDI variant of FITS was first
documented in AIPS Memo 102 :raw-latex:`\cite{aips102}`, and more
recently in AIPS Memo 114 :raw-latex:`\cite{aips114}`, which will be
generally available shortly. It has been modified for better support
support of DiFX FITS output. In general, these changes make ``FITLD``
less telescope specific so the resulting FITS-IDI files from any DiFX
installation should be highly compatible with AIPS. Several changes have
been made to the 31DEC08 AIPS as a result of DiFX testing:

#. Correction for digital *saturation* in auto-correlations is disabled
   for DiFX FITS files. See :raw-latex:`\cite{sci12}` for some details
   on this correction which is not needed for DiFX data.

#. Support for FITS-IDI files greater than 2 GiB in size.

#. Weather table was not populated properly.

#. FITS files with multiple UV tables would generate incomplete GEODELAY
   columns in CL tables (not relevant to DiFX).

It is recommended that your AIPS installation be kept up to date.

With the following exceptions, data reduction of DiFX correlated data
should be identical to that of VLBA hardware correlator data. This
includes the continued use of ``DIGICOR=1`` in ``FITLD`` and the use of
``ACCOR`` as you would have for the hardware correlator. The exceptions
are:

#. Use of ``FXPOL`` to correct data ordering in the case of *half* polar
   (e.g., ``RR`` and ``LL`` products) is no longer needed.

#. Use of ``VBGLU`` to concatenate data sets in the case of 512 Mbps
   observations is no longer needed.

#. Data is usually combined into a single FITS-IDI file with proper
   calibration data attached, usually implying that ``TBMRG`` is not
   needed to properly concattenate calibration data. This makes DiFX
   FITS-IDI data similar to the *pipeline-processed* VLBA data that was
   made available to users of the hardware correlator with the
   difference being that the original FITS-IDI format is retained,
   keeping file sizes typically 25% smaller.

These changes should make data processing easier in almost all
circumstances.
