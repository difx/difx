.. _sec:messages:

DiFX alert messages
===================

This section attempts to list all of the messages you might see coming
from the software correlator with some explanation about their meaning.
For some messages, certain actions to be taken are suggested. Each
subsection below contains descriptions of the messages for a particular
severity level, ordered from most severe to least severe. There are
almost 500 distinct messages that could be produced by ``mpifxcorr`` as
of DiFX version 1.5.2, thus no effort has been made to be compete in the
descriptions here. Effort has been made to document the most important
ones in detail. Messages are sorted first by their severity level and
are then roughly alphabetical within each subsection.

Fatal
-----

All fatal errors will cause immediate termination of the correlation
project. Most such errors would occur at the very start of a job as the
input files are being processed.

-  **Bin phase breakpoints are not in linear ascending order!!!**: The
   pulsar bins are not listed in phase order in the ``.binConfig`` file.
   Each ``BIN PHASE END`` entry must be greater than the previous and
   they must all be in the range 0 to 1.

-  **Cannot create output directory *outDir*: *flag* - aborting!!!**:
   The output directory could not be created. The most common cause is
   an existing output file from a previous attempt to correlate the job
   in question. Other possible causes include: permission issues,
   inaccessibility of output directory to the DiFX head node, and output
   filesystem being full.

-  **Cannot locate *stationName* in delay file *delayFile* -
   aborting!!!**: The specified delay file does not contain needed
   information for a station called *stationName*. This should never
   happen for data going through the correlator in a standard way.

-  **Cannot open Streamstor device. Either this Mark5 unit has crashed,
   you do not have read/write permission to /dev/windrvr6, or some other
   process has full control of the Streamstor device.**: This message
   will only come from a Mark5 unit that is requested to play back data.
   The most likely cause of such a problem is the Streamstor card
   getting stuck in a compromised state, although fresh correlator
   installations that may not have left ``/dev/windrvr6`` with global
   read/write permission is a second likely cause of this problem. The
   fix likely requires a reboot of the Mark5 unit. On occasion a full
   power cycle of the Mark5 unit (not just a soft reboot) is required.

-  **Cannot open file *inputFile* - aborting!!!**: The ``.input`` file
   named *inputFile* is not readable by the software correlator. This
   could be due to one of a number of issues, including: read permission
   problems, the request file not existing, or the file existing but not
   visible from one or more of the software correlator nodes.

-  **Cannot open delay file *delayFile* - aborting!!!**: The ``.delay``
   file named *delayFile* is not readable by the software correlator.
   This could be due to one of a number of issues, including: read
   permission problems, the request file not existing, or the file
   existing but not visible from one or more of the software correlator
   nodes.

-  **Cannot open output file *outputFile* - aborting!!!**: The output
   file could not be created. The most common cause is an existing
   output file from a previous attempt to correlate the job in question.
   Other possible causes include: permission issues, inaccessibility of
   output directory to the DiFX head node, and output filesystem being
   full.

-  **Cannot open pulsar config file *binConfigFile* - aborting!!!**: The
   specified pulsar bin file, *binConfigFile* cannot be opened. This
   could be due to one of a number of issues, including: read permission
   problems, the request file not existing, or the file existing but not
   visible from one or more of the software correlator nodes.

-  **Cannot open uvw file *uvwFile* - aborting!!!**: The specified
   ``.uvw`` file, *uvwFile* cannot be opened. This could be due to one
   of a number of issues, including: read permission problems, the
   request file not existing, or the file existing but not visible from
   one or more of the software correlator nodes.

-  **Cannot quad interpolate delays with post-f fringe rotation -
   aborting!!!**: Two mutually exclusive options (``QUAD DELAY INTERP``
   and ``POST-F FRINGE ROT``) were both enabled in the ``.input`` file.

-  **Caught an MPI exception!!! *errorString***: A process communication
   error has occured causing the correlator to terminate. The outcome of
   such an event cannot be good; contact a DiFX developer.

-  **Config encountered inconsistent setup in config file -
   aborting!!!**: One or more of the configurations (group of settings
   defined in the ``.input`` file) is either illegal or incompletely
   defined. Usually this message will come with another more detailed
   error message. In any case, either there is a correlator version
   mismatch or there is something wrong with the ``.input`` file.

-  **Core received a request to process data from time *time* which does
   not have a config - aborting!!!**: This is likely due to failed
   consistency check in the software correlator that resulted from a
   logic error in the code. This should be reported to a DiFX developer.

-  **Could not find station *stationName* in the uvw file when making
   rpfits header!!! This station is used in the correlation so I will
   abort!!!**: This error only occurs with RPFITS output format which is
   not supported by this documentation – please seek other sources of
   assistance if needed.

-  **Could not locate any of the specified sources in the specified time
   range - aborting!!!**: The correlator gave up since none of the
   sources to be correlated appeared in the ``.uvw`` file. This should
   never happen for data going through the correlator in a standard way.

-  **Could not locate a polyco to cover the timerange ...**: A pulsar
   polynomial for a certain time period could not be found. The person
   supplying the puslar polynomial should be contacted.

-  **DataStream assumes long long is 8 bytes, it is ``x`` bytes -
   aborting!!!**: This should not occur on any modern operating system.
   If this message is seen, please contact a DiFX developer and be sure
   to indicate exactly which operating system and computer type you are
   using.

-  **DataStream assumes int is 4 bytes, it is *x* bytes - aborting!!!**:
   This should not occur on any modern operating system. If this message
   is seen, please contact a DiFX developer and be sure to indicate
   exactly which operating system and computer type you are using.

-  **DataStream *mpiid*: expected *x* bytes, got *y* bytes -
   aborting!!!**: In an eVLBI read, the wrong number of bytes was
   received, indicating a mismatch in send/receive setups. Check the
   setup at both ends and try again.

-  **Datastream *mpiid*: implied UDP packet size is negative -
   aborting!!!**: When considering the size of a UDP header, an
   unphysical packet size results. This should only occur when
   attempting UDP based eVLBI.

-  **Datastream *mpiid*: read too few UDP packets: bytestoread=\ *x*
   udp_offset=\ *y* bytes=\ *z* - aborting!!!**: Fewer than expected UDP
   packets were received in an eVLBI transfer (FIXME – more details
   please!)

-  **Datastream *mpiid*: read too many UDP packets: bytestoread=\ *x*
   udp_offset=\ *y* bytes=\ *z* - aborting!!!**: More than expected UDP
   packets were received in an eVLBI transfer (FIXME – more details
   please!)

-  **Datastream *mpiid*: could not allocate databuffer (length *size*) -
   aborting!!!**: A memory allocation failed. This would probably be due
   to either a developer error or attempt to run a job on an
   underpowered computer. In any case, the developer should be
   contacted.

-  **Developer error: Cannot handle delays more negative than
   *maxDelay*. Need to unimplement the datastream check for negative
   delays to indicate bad data - aborting!!!**: The maximum negative
   delay (which is quite large) has been exceeded. This is almost
   certainly a logic error in the software and should be reported to a
   DiFX developer.

-  **Developer error: in Mk5Mode::Mk5Mode, mark5stream is null**: An
   internal error related to initializing a Mark5 decoder has occurred.
   Contact a DiFX developer.

-  **Developer error: in Mk5Mode::Mk5Mode, framesamples is inconsistent
   (*x*/*y*)**: An internal inconsistency has been found in the Mark5
   data frame size that would lead to downstream errors. This could only
   be caused by a software logic error. Contact a DiFX developer.

-  **Developer error: UVW has not been created!!!**: This message would
   come from an internal consistency check that failed. If this occurs,
   the DiFX developers should be notified as this indicates a logic
   error inside the software correlator.

-  **genMk5FormatName : *formatType* format : framebytes = *frameBytes*
   is not allowed**: An illegal frame size was specified in the
   ``.input`` file. This should never happen for data going through the
   correlator in a standard way. If you see this message it is probably
   due to a programming error and the DiFX developers should be
   notified.

-  **genMk5FormatName : unsupported format encountered**: A data format
   not handled by the software correlator has been requested in the
   ``.input`` file.

-  **Input file out of order!**: There is an error in the input file.
   This might be caused by using an input file formatted for one version
   of ``mpifxcorr`` on a different version or by an incorrectly written
   file.

-  **Manager aborting correlation!**: The configuration of a visibility
   buffer was not OK. There are many possible causes for this, but is
   most likely due to a logic error in the software. Contact a DiFX
   developer if this is encountered.

-  **Mk5DataStream::readnetwork bytestoread too large (*x*/*y*) -
   aborting!!!**: An eVLBI read size is too large. (FIXME – more details
   here please!)

-  **mpifxcorr must be invoked with at least *x* processors (was invoked
   with *y* processors) - aborting!!!**: For a correlation of :math:`N`
   datastreams (usually equal to the number of antennas), at least
   :math:`N+2`, but preferably even more, processes must be started.

-  **NativeMk5DataStream::NativeMk5DataStream stub called, meaning
   mpifxcorr was not compiled for nativemk5 support, but it was
   requested (with MODULE in .input file) - aborting!!!**: Correlation
   directly off a Mark5 module requires compiling against the Streamstor
   libraries which was apparently not done but requested by the
   ``.input`` file. Contact the person responsible for your correlator
   setup and ask them to properly link the Streamstor libraries to the
   ``mpifxcorr`` executable.

-  **No config section in input file**: The input file is missing its
   config section and hence correlation cannot proceed. This should
   never happen for data going through the correlator in a standard way.

-  **Not enough baselines are supplied in the baseline table (*x*)
   compared to the number of baselines (*y*)!!!**: The ``.input`` file
   requests more baselines in the common table than there are enumerated
   in the baseline table. This should never happen for data going
   through the correlator in a standard way.

-  **Not enough datastreams are supplied in the datastream table (``x``)
   compared to the number of datastreams (``y``)!!!** The ``.input``
   file requests more datastreams (nominally equal to the number of
   antennas) in the common table than there are enumerated in the
   baseline table. This should never happen for data going through the
   correlator in a standard way.

-  **Please invoke with mpifxcorr ...**: The correlator was not started
   according to is usage. See §\ `[sec:mpifxcorr] <#sec:mpifxcorr>`__
   for more details.

-  **Polyco *polycoId* / *subcount* is malformed** The pulsar polynomial
   file is not compliant (possibly due to manual editing).

-  **RPFITS not compiled in - aborting**: RPFITS output format is
   requested, but support for RPFITS is not compiled into ``mpifxcorr``.
   Either requested output format, or recompile ``mpifxcorr`` with
   RPFITS. Note: This document does not contain instructions for RPFITS
   installations – you are on your own!

-  **Samplesperblock is less than 1, current implementation cannot
   handle this situation - aborting!!!**: An illegal data sub-mode has
   been requested. Contact a developer.

-  **Unknown data format *formatName***: A data format (e.g., VLBA,
   Mark4, LBA) has been requested that cannot be processed.

-  **Unknown data source *sourceName***: A data source (e.g., FILE,
   MODULE, EVLBI) has been requested that cannot be used.

Severe
------

Severe errors typically reflect an unexpected software error. All severe
errors are related to a process control (threading) failure or a failure
in a numerical routine. Errors of the severe type are likely to cause
widespread erroneous results or complete failure of correlation. Except
during periods of software development errors of these two types are
unexpected. If encountered, they should be reported to a DiFX developer
and the correlation should be reattempted once. Since there are many
errors in the “severe” class, all unlikely and with the same procedure
for working around, individual errors of this type are not listed below.

Error
-----

Messages in the “Error” class are typically fairly significant, often
cascading to fatal messages and termination of the correlation. Many
errors of the Mark5 variety result data loss ranging from fractions of a
second to the full job in length. For errors of this type, operator
judgement is needed: whether to restart correlation or keep going will
depend on many circumstances. Note that many of the eVLBI and real-time
monitor errors are not documented here yet.

-  **All data from this module was discarded: ...**: Due either to
   malformed data or a directory file with incorrect values, no data was
   deemed suitable for correlation for a particular Mark5 module. It is
   probably worth trying to extract the module directory structure again
   and trying the job again and/or moving the module to a different unit
   for playback. It is likely that other jobs using this module will
   face a similar problem.

-  ***n* consecutive sync errors. Something is probably wrong!**: A
   large number of sync errors were seen. This probably means that the
   Mark5 data being read is somehow corrupted.

-  **All bandwidths for a given datastream must be equal**: Currently,
   ``mpifxcorr`` does not allow different bandwidths on different
   frequency bands. This will lead to temination of the correlator. The
   creator of the ``.input`` file should be contacted.

-  **All configs must have the same telescopes! Config *m* datastream
   *n* refers to different telescopes**: The same set of telescopes
   (antennas) must be used throughout a job. Two configurations have
   been found that use different antenna subsets. This will lead to
   temination of the correlator. The creator of the ``.input`` file
   should be contacted.

-  **All LBASTD Modes must have 2 bit sampling - overriding input
   specification!!!**: The Australian LBASTD data format modes can only
   handle 2-bit (4 level) quantization at the moment. The ``.input``
   file value for ``QUANTISATION BITS`` is being ignored here. Probably
   the maker of the ``.input`` file made a mistake.

-  **Attempting to get a delay from offset time *time*, will take
   ``first``\ \|\ ``last`` source**: Correlation is requested for a time
   either before or after the list of scans. Data affected by this is
   likely to have the wrong delay applied and is unlikely to be useful.
   The creator of the input files should be contacted.

-  **Attempting to refer to freq outside local table!!!**: The frequency
   index of a datastream table exceeds the length of the frequency table
   in the ``.input`` file. This will most certainly cause this frequency
   band to be incorrectly correlated.

-  **Baseline table entry *m*, frequency *n*, polarisation product *p*
   for datastream *q* refers to a band outside datastream *q*\ ’s range
   (*r*)**: The band index for a baseline table exceeds its legal range.
   This will lead to temination of the correlator. The creator of the
   ``.input`` file should be contacted.

-  **Baseline table entry *m* has a datastream index outside the
   datastream table range! Its two indices are *n*, *p***: The baseline
   table has a datastream index that exceeds the number of datastreams.
   This will lead to temination of the correlator. The creator of the
   ``.input`` file should be contacted.

-  **Cannot clear Mark5 write protect**: The software correlator
   attempts to set the Disk Module State to ``PLAYED``. This requires
   temporarily disabling write protection. If this fails, then either
   the Mark5 unit is in a bad state or the module has a problem.

-  **Cannot open data file *dataFile***: The datastream process cannot
   open the specified file containing baseband data to be correlated.
   Correlation will proceed, but no data for an antenna for the duration
   of this file will be correlated. Usually this error should be a cause
   for concern.

-  **Cannot open polyco file *polycoFile***: Either file permissions,
   file location, or non-existence is preventing the file called
   *polycoFile* from being opened. This will likely end badly.

-  **Cannot put Mark5 unit in bank mode**: The command to put the Mark5
   unit in single bank mode failed and further commands to the Mark5
   unit will probably fail as well. Usually this only happens if the
   Mark5 unit is in a bad state. The Mark5 unit probably needs a reboot.

-  **Cannot read data from Mark5 module...**: Read from a Mark5 module
   failed. No further attempt to read data from the module will be made.
   It is strongly recommended that the Mark5 unit be rebooted and the
   correlation be reattempted. If the error is reproducible, the
   additional information contained at the end of this error message may
   help diagnose the problem.

-  **Cannot read the Mark5 module label**: The command to retrieve the
   module label, which includes the volume serial number (VSN) and the
   previous state, has failed. This implies trouble with the Mark5 unit
   or module. First the module should be moved to a different Mark5 unit
   and the correlation reattempted. Upon further failure, the module
   should be checked for problems

-  **Cannot set Mark5 data replacement mode / fill pattern**: Either the
   command to tell the Mark5 unit to enter real-time playback mode or
   the command to set the fill pattern have failed; expect more problems
   with this Mark5 unit. The Mark5 unit probably needs a reboot.

-  **Cannot set the Mark5 module state**: The software correlator failed
   to set the Disk Module State to ``PLAYED``. If this fails, then
   either the Mark5 unit is in a bad state or the module has a problem.

-  **Cannot set Mark5 write protect**: The software correlator attempts
   to set the Disk Module State to ``PLAYED``. This requires temporarily
   disabling write protection. If this fails, then either the Mark5 unit
   is in a bad state or the module has a problem.

-  **Cannot unpack Mark5 format data at sampleoffset *n* from buffer
   *time***: An error of this kind likely represents a logic error in
   the software correlator and should be reported to a DiFX developer.

-  **Config *m* baseline index *n* refers to baseline *p* which is
   outside the range of the baseline table**: The baseline index of a
   config table exceeds the number of baselines in the baseline table.
   This will lead to temination of the correlator. The creator of the
   ``.input`` file should be contacted.

-  **Config *m* baseline index *n* refers to baseline *p* which is out
   of order with the previous baseline ...**: Entries in the datastream
   table are not in the expected order. This will lead to temination of
   the correlator. The creator of the ``.input`` file should be
   contacted.

-  **Could not find a polarisation pair, will be put in position
   *x* !!!"**: This is due either to an uncaught inconsistency in the
   ``.input`` file or a logic error in the software correlator. Contact
   a DiFX developer.

-  **Could not find any bands for frequency *m* of datastream *n***:
   This is due either to an uncaught inconsistency in the ``.input``
   file or a logic error in the software correlator. Contact a DiFX
   developer.

-  **Could not open *threadFile* - will set all numthreads to 1!!!**:
   The requested ``.threads`` file could not be opened. Possible causes
   include: permission issues, inaccessibility of directory to the DiFX
   head node, and the file simply not existing. The impact of this is
   that correlation will proceed at a potentially much reduced speed
   since each CPU will use only a single processing thread. Accuracy of
   the results will not be affected.

-  **Could not parse LBA file header**: An LBA format file appears
   corrupt. Data for one antenna for the duration of this file will not
   be correlated.

-  **Datastream table entry *m* has a frequency index (freq *n*) that
   refers outside the frequency table range (*p*)**: The ``.input`` file
   has a frequency index error. This will lead to temination of the
   correlator. The creator of the ``.input`` file should be contacted.

-  **Datastream table entry *m* has an input band local frequency index
   (band *n*) that refers outside the local frequency table range
   (*p*)**: The ``.input`` file has a frequency index error. This will
   lead to temination of the correlator. The creator of the ``.input``
   file should be contacted.

-  **Datastream table entry *m* has a telescope index that refers
   outside the telescope table range (*n*)**: The ``.input`` file has a
   telescope index error. This will lead to temination of the
   correlator. The creator of the ``.input`` file should be contacted.

-  **FFT chunk time for config *m*, datastream *n* is not a whole number
   of nanoseconds (*p*)**: In order to keep track of time properly, each
   FFT must start on an integer nanosecond boundary. There are currently
   no modes supported where this should be a problem, so if you see this
   message, there is a more serious problem. Contact a DiFX developer!

-  **First datastream for baseline *n* has a higher number than second
   datastream - reversing!!!**: The entries of the baseline table
   (indicating which antenna pairs to correlate) should always have the
   datastream indices in ascending order. If you see this warning, the
   correlator is correcting your baseline ordering, but be aware that
   this might be hinting that something else might be awry with the
   ``.input`` file. The creator of the ``.input`` file should be
   contacted.

-  **Hit the end of the file! Setting the numthread for Core *n* to 1**:
   The ``.threads`` file ended unexpectedly early and one or more core
   processes will be forced to use a single processing thread, with
   potentially crippling performance penalty. Accuracy of the results
   will not be affected.

-  **Increment per read in nanoseconds is *x* - too large to fit in an
   int**: The way timekeeping works in DiFX, data chunks larger than
   :math:`2^{31}-1` nanoseconds in length are not possible at the
   moment. The maker of the ``.input`` file needs to reduce the read
   size by changing some of the parameters (such as ``NUM CHANNELS``,
   ``BLOCKS PER SEND``, or possibly others).

-  **lastoffsetns less than 0 still! = *x***: This message probably
   indicates a logic error in the correlator program so should be
   reported to a DiFX developer.

-  **Mk5DataStream::calculateControlParams : vlbaoffset=\ *x*
   bufferindex=\ *y* atsegment=\ *z***: A Mark5 data frame was found
   unaligned. A corresponding subintegration of data will be
   invalidated. A large number of messages of this type probably
   indicates corrupted data.

-  **Module *VSN* contains undecoded scans!**: The module directory for
   module *VSN* has problems. Please correct the problem with the
   directory (as stored in ``$MARK5_DIR_PATH``) and try again.

-  **Module *VSN* not found in unit!**: The ``.machines`` file suggested
   that a specified Mark5 module would be found this this Mark5 unit but
   it was not. Check to make sure the module is in the unit.

-  **Most of the data from this module was discarded: ...**: Due either
   to malformed data or a directory file with incorrect values, a large
   fraction of the data for this job from a particular module was not
   decodable. It is probably worth trying to extract the module
   directory structure again and trying the job again and/or moving the
   module to a different unit for playback. It is likely that other jobs
   using this module will face a similar problem.

-  **No valid data found. Stopping playback!**: According to the
   directory for the module in question there is no valid data available
   to correlate. It is possible that a directory reconstruction will
   allow some or all of the data on the module to be retrieved.

-  **Not all datastreams accounted for in the datastream table for
   config *m***: All datastreams (roughly equivalent to antennas) must
   be represented in each configuration within the ``.input`` file. This
   will lead to temination of the correlator. The creator of the
   ``.input`` file should be contacted.

-  **Number of input bands for datastream *m* (*n*) does not match with
   Mark5 file *fileName* (*p*), will be ignored!!!**: The description of
   Mark5 baseband data (VLBA or Mark4 format) in the ``.input`` file is
   inconsistent with the actual content. The creator of the ``.input``
   file should verify that the format is correctly specified. All data
   related to this condition will be left uncorrelated.

-  **Oversamplefactor (*m*) is less than decimation factor (*n*)**: The
   oversample factor must be an integer :math:`r` times the decimation
   factor. Essentially oversampling is handled by two mechanisms:
   decimation of the input data stream in the data unpacker and through
   spectral selection at the time of FITS file creation. The
   oversampling factor of these two approaches must multiply to be the
   total *Oversamplefactor*. This will lead to temination of the
   correlator. The creator of the ``.input`` file should be contacted.

-  **Pulsar phase as calculated fromt the polyco will not be accurate
   over entire range of *time* as the frequency is changing too rapidly.
   The maximum safe calc length would be *x* - try reducing blocks per
   send or numchannels ...**: A detail of the way the pulsar polynomial
   is used is likely to cause imperfect binning. Please consult with the
   PI and the creator of the ``.input`` file.

-  **Stale data was received from core *n* regarding time *time* seconds
   - it will be ignored!!!**: One subintegration of data is being
   discarded as it did not arrive in time for the data to be written to
   disk. If this is a chronic problem, increasing the value of
   ``VIS BUFFER LENGTH`` in the ``.input`` might help. A small number of
   errors of this type is not a problem.

-  **Telescope *antName* could not be found in the uvw file!!!**: Data
   for this antenna will not have useful UVW values in its output file.
   Contact the creator of the ``.input`` file.

-  **There must be an integer number of sends per datasegment. Presently
   databufferfactor is *m*, and numdatasegments is ``n``**: The
   ``.input`` file contains an inconsistency in its parameterization of
   the various buffer sizes. This will lead to temination of the
   correlator. The creator of the ``.input`` file should be contacted.

-  **Trying to read past the end of file!!!**: One of the files read by
   ``mpifxforr`` ended prematurely. The maker of the ``.input`` file
   should be contacted.

-  **Unknown output format *outFormat* (case sensitive choices are
   RPFITS, SWIN and ASCII)**: ``mpifxcorr`` currently supports 3 output
   formats (ASCII, DIFX and RPFITS) and the requested one does not match
   one of these. This will lead to temination of the correlator. The
   creator of the ``.input`` file should be contacted.

-  **Unsupported format or mode requested!!!**: A data format has been
   requested that is recongnized but not supported by the correlator.
   Scans using this format (probably all scans for the antenna in
   question) will produce no data. The maker of the ``.input`` file
   should be contacted.

-  **Waited 6 seconds for a Mark5 read and gave up.**: A Mark5 read
   timed out. A small number of such errors can be tolerated, especially
   for a module that is known to be bad, but many such messages should
   prompt a second correlation attempt after module move / unit reboot.

-  **We thought we were reading something starting with
   ’\ *something*\ ’, when we actually got ’\ *somethingElse’***: The
   ordering or content of one of the files being read by ``mpifxcorr``
   does not match expectations and is thus non-conformant. The maker of
   the file should be contacted. Note that this could be caused by a
   version mismatch between in the input files and the correlator.

-  **XLRCardReset() failed. Remainder of data from this antenna will not
   be correlated and a reboot of this Mark5 unit is probably needed.**:
   It is probably worth reattempting correlation after Mark5 reboot and
   possible module move.

-  **XLROpen() failed. Remainder of data from this antenna will not be
   correlated and a reboot of this Mark5 unit is probably needed.**:
   After a successful Streamstor card reset, the card was not able to be
   accessed. It is probably worth reattempting correlation after Mark5
   reboot and possible module move.

Warning
-------

Warning level messages typically relate to a situation that may result
in the loss of a very small amount of data or indicate some other
irregularity to the operator. A single warning should not be of concern,
but a large number of warnings should be noted.

-  ***n* consecutive fill patterns at time *time***: Instead of reading
   data from the module, the Streamstor card returned fill pattern for
   this many consecutive data frames. If the module is flakey, it may be
   possible to recover more data after moving the module to a different
   Mark5 unit, but usually warnings of this type indicate inevitable
   data loss due to fill pattern replacement.

-  ***n* consecutive sync errors starting at readpos *p* ...**: Data was
   read off the module, but the sync word was not found. This either
   indicates attempted playback of the wrong data format, completely
   corrupted data, or valid data that has slipped samples and is thus
   unfortunately not processable by the software correlator. It might be
   worth a recorrelation attempt after moving the module, but not
   likely.

-  **Cannot find a valid configindex to set Mk5-related info. Flagging
   this subint**: Application of the delay model caused a small amount
   of data to cross a scan boundary in a manner that required the
   flagging of an entire subintegration. Such an event should be very
   rare.

-  **Connection to monitor socket still pending**: When real-time
   monitoring of the output visibilities is requested but no connection
   to a monitor program has been establish this warning may appear. A
   warning of this type is not associated with any data loss.

-  **Copying a polyco with no frequency information!**: A pulsar
   polynomial without frequency information has been found. This could
   present problems in setting the gate properly across frequency
   channels, but could be intentional.

-  **Could not find station *stationName* in the uvw file when making
   rpfits header!!! This station is not used in this correlation so its
   parameters will be initialised to 0!!!**: The ``.input`` file
   included a station not in the ``.uvw`` file, but this station is not
   used so the results won’t be affected. This warning will only be
   issued for data being written in RPFITS format. The resultant RPFITS
   file will still list the missing antenna, but it’s information will
   be bogus. This could confuse downstream software.

-  **Could not open command monitoring socket! Aborting message receive
   thread.**: Real-time monitoring was wanted, but a problem at the
   operating system level (perhaps permissions?) prevented the needed
   socket from being created. This has no bearing on the quality of the
   output data.

-  **Data was received which is too recent (*x* sec + *y* ns)! ...**: A
   portion of one visibility record will have incomplete weight due to
   forced flushing of the long term accumulator buffer for that
   visibility record. A single warning of this type on source change is
   nothing to worry about, but constant warnings of this should be
   reported as this may indicate a different problem.

-  **DataStream *mpiid*: could not identify Mark5 segment time
   (*formatName*)**: An eVLBI data packet could not be decoded and hence
   its time cannot be determined. This is usually not a problem since
   time can be dead-reckoned from previous data and the corrupt packet
   won’t be used anyway.

-  **Filterbank not yet supported!!!**: The filterbank mode, enabled
   with the ``FILTERBANK USED`` option in the ``.input`` file, was
   requested, but is currently not supported. When supported, the
   filterbank mode will offer options for *crisper* spectral channels.

-  **Fractional start time of *x* seconds plus *y* ns was specified, but
   the start time corresponded to a configuration not specified in the
   input file and hence we are skipping *z* seconds ahead! The ns offset
   will be set to 0!!!**: This warning indicates that the start time of
   the job is not contained within a scan to be correlated and the
   subsequent start time will be rounded to the next integer second.
   Except in cases where exact timestamp matching is needed this is not
   a problem. The warning is issued as it is not normal for the start
   time of a job to be outside a scan. If you see this a lot, contact
   the person generating your jobs and let them know.

-  **FXMANAGER caught a signal and is going to shut down the
   correlator**: A terminate signal was sent to the manager process
   indicating that the correlator should be stopped immediately. The
   resultant output file will be incomplete, but the early stop was
   probably on purpose so that should be expected.

-  **Hit end of first line prematurely - check your polyco conforms to
   standard! Some values may not have been set properly, but likely
   everything is ok**: The pulsar polynomial file had an unexpectedly
   short first line. Some parameters may not have been properly set so
   the pulsar gating may not work. Retrying correlation will not help!
   If you are concerned, contact the producer of the polynomial.

-  **Hit end of second line prematurely - check your polyco conforms to
   standard! Some values may not have been set properly. This often
   happens for non-binary pulsars. Likely everything is ok.** It is
   possible that the pulsar polynomial file is incomplete, but more
   likely nothing is wrong.

-  **Incorrectly set config index at scan boundary! Flagging this
   subint**: A subintegration that crosses the end of a scan got flagged
   as a result of a potential format change at this time. This should
   happen no more than once per scan and affects only a small fraction
   of one integration period in most cases.

-  **Internal Error, trying to copy pass buffer size**: This is an eVLBI
   related error. (FIXME – please add more description here)

-  **Mk5DataStream::calculateControlParams : bufferindex=\ *x*
   :math:`>=` bufferbytes=\ *y***: A fraction of a dataframe is being
   discarded due to a frame misalignment. This affects a very small
   amount of data and should be very rare.

-  **Module label is not terminated!**: A Mark5 module has an oversized
   extended serial number label. This in itself is not a problem but may
   inidicate either corruption on the module or poor seating of either
   the Mark5 module or one of the cables/cards inside the Mark5 unit.

-  **Module label record separator not found!**: No “Disk Module State”
   was stored on this module. This is probably due to recording on a
   very old version of Mark5A or is possibly the result of some other
   incompatibility.

-  **MPI Id *mpiid*: warning - received a parameter instruction
   regarding *paramName* which cannot be honored and will be ignored!**
   The software correlator can receive a number of different commands
   that can affect its dumping of its long-term accumulator to an
   external piggy-back processor. If an unrecognized parameter is
   received a warning of this type will be issued. This is completely
   unrelated to correlator data quality.

-  **No more data for project on module *mpiid***: Although data on the
   module extends past the end of the current job, none of the remaining
   data is relevant for this job so playback is stopping early. This is
   only a problem if it is due to an incorrect transcription of the
   module directory. It might be prudent to look at the observe log to
   see if data is expected.

-  **No more data on module *mpiid***: The Mark5 unit sending this
   message is stopping its reading before the end of the job since no
   more data exists. This is only a problem if it is due to an incorrect
   transcription of the module directory. It might be prudent to look at
   the observe log to see if data is expected.

-  **Post-f fringe rotation not yet tested - use at your own risk!!!**:
   Post FFT fringe rotation (enabled with the ``POST-F FRINGE ROT``
   ``.input`` file option) has not been extensively tested. Results may
   be OK, but you are on your own!

-  **Trying to read *s* seconds past the end of the UVW array!**: The
   ``.uvw`` file does not cover an entire scan. If more than a couple
   such warnings are seen, the creator of the ``.input`` and ``.calc``
   file should be notified.

-  **Waited *s* sec state=\ *state***: A Mark5 module is taking longer
   than expected to respond. Additional messages will follow if this
   situation is serious.

-  **XLRCardReset() being called!**: If a read timeout occurs, the
   Streamstor card in the Mark5 unit will be reset. This message
   indicates that this non-standard procedure is occuring. The result of
   this reset will usually either be success (in which case a message
   will indicate such), failure (in which case an error message will
   state that no more data will be read from the module), or a Mark5
   unit hang, and hence the correlation will stop. If this is not
   successful, it is recommended that the correlation be retried after
   rebooting the affected Mark5 unit and possibly moving the module to a
   different Mark5 unit.

Info
----

Info messages convey normal messages to the operator.

Verbose
-------

Verbose messages convey normal messages to the operator that are either
not very important or come in vast quantities; they are typically
filtered out of data logging to prevent unnecessary bloat.

Debug
-----

Debug messages are useful only to developers and don’t usually indicate
error conditions. In some cases they might be useful in diagnosing a
problem. None of the debug messages are explicitly documented here
because they will be very version dependent and in general provide
little value to the operator.
