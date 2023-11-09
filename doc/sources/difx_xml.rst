.. role:: raw-latex(raw)
   :format: latex
..

.. _sec:xml:

XML message types
=================

The ``difxmessage`` library
(§\ `[sec:difxmessage] <#sec:difxmessage>`__) implements a system for
sending and receiving messages using XML format. This section documents
the “difxMessage” XML document type that is used for interprocess
communication during correlation within DiFX. These messages are sent
via UDP multicast and are thus restricted to fit within one
standard-sized Ethernet packet (:math:`\sim`\ 1500 bytes). Various
logging and monitoring programs (``mk5mon``, ``cpumon``, and
``errormon``, all eventually to be replaced by a single interactive
operator interface) can accept these messages and perform actions based
on their content. Several different message types are derived from the
following XML base type:

   ::

      <?xml version="1.0" encoding="UTF-8"?>
      <difxMessage>
        <header>
          <from>|bfit[from]</from>
          <to>|bfit[to]</to>
          <mpiProcessId>|bfit[mpiId]</mpiProcessId>
          <identifier>|bfit[idString]</identifier>
          <type>|bfit[messageType]</type>
        </header>
        <body>
          <seqNumber>|bfit[seqNum]</seqNumber>
          |bfit[body]
        </body>
      </difxMessage>

The italicized fields are as follows:

   the hostname of the sender.

   the intended recipient of the XML document. Note that this field is
   typically not included for report-only messages as it’s intended
   purpose is for directing commands to particular recipients. Also note
   that multiple fields can be present in a message. Three “shortcuts”
   are currently allowed: ``all`` causes all receiving programs (such as
   ``mk5daemon``) on all software correlator cluster members to respond;
   ``mark5`` causes all Mark5 units to respond; and ``swc`` causes all
   non-Mark5 units to respond.

   the MPI process id of the sender. If there are :math:`D` (typically
   10) datastream processes (i.e., Mark5 units playing back), then takes
   on the following numbers:

   =============== ===========================================
   value           ``mpifxcorr`` process type
   =============== ===========================================
   :math:`< 0`     a process not associated with ``mpifxcorr``
   :math:`0`       the manager process of ``mpifxcorr``
   1 to :math:`D`  one of the datastream processes
   :math:`\ge D+1` one of the core (computing) processes
   =============== ===========================================

   an additional string identifying the source of the message. For
   messages sent from ``mpifxcorr``, this will be the job id, for
   example ``job3322.000``. Other programs will typically set this field
   to the name of the program sending the message.

   the type of message being sent:

   +----------------------------+----------------------------------------+
   | value                      | description of message type            |
   +============================+========================================+
   | ``DifxAlertMessage``       | an error message.                      |
   +----------------------------+----------------------------------------+
   | ``DifxCommand``            | a command message.                     |
   +----------------------------+----------------------------------------+
   | ``DifxDatastreamMessage``  | *Not yet implemented*                  |
   +----------------------------+----------------------------------------+
   | ``DifxDiagnosticMessage``  | used by ``mpifxcorr`` to pass out      |
   |                            | diagnostic-type info such as buffer    |
   |                            | states                                 |
   +----------------------------+----------------------------------------+
   | ``DifxFileTransfer``       | used by the new (USNO) GUI to cause a  |
   |                            | file to be sent to or from a specified |
   |                            | host                                   |
   +----------------------------+----------------------------------------+
   | ``DifxFileOperation``      | used by the new (USNO) GUI to cause    |
   |                            | some operation to a file (such as      |
   |                            | ``mkdir``, ``mv``, ``rm`` …) to be     |
   |                            | performed                              |
   +----------------------------+----------------------------------------+
   | ``DifxGetDirectory``       | used by the new (USNO) GUI to request  |
   |                            | a Mark5 ``.dir`` file                  |
   +----------------------------+----------------------------------------+
   | ``DifxInfoMessage``        | *not used?*                            |
   +----------------------------+----------------------------------------+
   | ``DifxLoadMessage``        | CPU and memory usage (usually sent by  |
   |                            | ``mk5daemon``).                        |
   +----------------------------+----------------------------------------+
   | ``DifxMachinesDefinition`` | used by the new (USNO) GUI to remotely |
   |                            | write a ``.machines`` file             |
   +----------------------------+----------------------------------------+
   | ``DifxParameter``          | specify new parameter value to an      |
   |                            | ``mpifxcorr`` process.                 |
   +----------------------------+----------------------------------------+
   | ``DifxSmartMessage``       | contains smart data for one drive of a |
   |                            | module; usually sent by ``mk5daemon``  |
   +----------------------------+----------------------------------------+
   | ``DifxStart``              | tell head node to start a difx job.    |
   +----------------------------+----------------------------------------+
   | ``DifxStop``               | tell ``mk5daemon`` to stop a           |
   |                            | particular instance of ``mpifxcorr``.  |
   +----------------------------+----------------------------------------+
   | ``DifxStatusMessage``      | status of the ``mpifxcorr`` program.   |
   +----------------------------+----------------------------------------+
   | ``DifxTransientMessage``   | used by the VFASTR project to indicate |
   |                            | an event of interest                   |
   +----------------------------+----------------------------------------+
   | ``DifxVex2DifxRun``        | used by the new (USNO) GUI to remotely |
   |                            | run ``vex2difx``                       |
   +----------------------------+----------------------------------------+
   | ``Mark5DriveStatsMessage`` | Mark5 module conditioning statistics   |
   |                            | for one disc.                          |
   +----------------------------+----------------------------------------+
   | ``Mark5StatusMessage``     | status of the mark5 unit and modules.  |
   +----------------------------+----------------------------------------+
   | ``Mark5VersionMessage``    | versions, board types and serial       |
   |                            | numbers of a Streamstor card.          |
   +----------------------------+----------------------------------------+

   Many of these message types are described in sections that follow.
   For those without documentation, the source file ``difxmessage.h`` in
   the difxmessage package contains the full set of parameters.

   the sequence number (starting at 0) of messages coming from the
   particular program running on the particular host. The number
   advances by 1 for each sent message and can be used to detect lost
   packets.

   message contents that are specific to the particular . See sections
   that follow.

A “C” language library for generating, multicasting, receiving, and
parsing XML documents of this type is used within some of the programs,
including ``mpifxcorr`` (the core of the DiFX :raw-latex:`\cite{difx}`
software correlator) and ``mk5daemon`` (a program that runs on each
Mark5 unit that is responsible for multicast communication when
``mpifxcorr`` is not running), that transact these XML documents. The
default multicast group to be used is 224.2.2.1 and the default port is
50200, though these can be overridden with environment variables
``DIFX_MESSAGE_GROUP`` and ``DIFX_MESSAGE_PORT`` respectively.

.. _sec:difxalertmessage:

DifxAlertMessage
----------------

This section describes messages with = ``DifxAlertMessage``. These
messages come from mpifxcorr or the head node agent and contain an error
message string and severity code that should be displayed to the
operator and logged.

The of the message contains:

   ::

      <difxAlert>
            <alertMessage>|bfit[message]</alertMessage>
            <severity>|bfit[severity]</severity>
          </difxAlert>

The italicized fields are as follows:

a string containing the error message.

an integer indicating the severity. The severity scale is based on that
from the EVLA and has values with the following meanings:

+-------+---------+--------------------------------------------------+
| value | name    | meaning                                          |
+=======+=========+==================================================+
| 0     | FATAL   | processing has failed; a restart is needed       |
+-------+---------+--------------------------------------------------+
| 1     | SEVERE  | data from one or more station is affected badly  |
+-------+---------+--------------------------------------------------+
| 2     | ERROR   | data from one or more station may be affected;   |
|       |         | e.g., low weights                                |
+-------+---------+--------------------------------------------------+
| 3     | WARNING | minor error of no consequence to ongoing         |
|       |         | processing                                       |
+-------+---------+--------------------------------------------------+
| 4     | INFO    | informational only                               |
+-------+---------+--------------------------------------------------+
| 5     | VERBOSE | overly verbose infomation                        |
+-------+---------+--------------------------------------------------+
| 6     | DEBUG   | debugging information                            |
+-------+---------+--------------------------------------------------+

DifxCommand
-----------

This section describes messages with = ``DifxCommand``. These messages
require the field to be set and cause the intended recipient to take an
action.

The of the message contains:

   ::

      <difxCommand>
            <command>|bfit[command]</command>
          </difxCommand>

The italicized field is as follows:

the command to execute. Commands are not case sensitive and should be
among the following:

+-------------------------------+-------------------------------------+
| command                       | action                              |
+===============================+=====================================+
| ``GetVSN``                    | cause the mark5 unit to multicast   |
|                               | loaded VSNs if possible.            |
+-------------------------------+-------------------------------------+
| ``GetLoad``                   | request CPU and memory usage to be  |
|                               | reported.                           |
+-------------------------------+-------------------------------------+
| ``ResetMark5``                | cause ``SSReset`` and ``ssopen`` to |
|                               | be run to reset Streamstor.         |
+-------------------------------+-------------------------------------+
| ``StartMark5A``               | start the Mark5A program.           |
+-------------------------------+-------------------------------------+
| ``StopMark5A``                | stop the Mark5A program.            |
+-------------------------------+-------------------------------------+
| ``KillMpifxcorr``             | kill with signal 9 (sigkill) any    |
|                               | process with name ``mpifxcorr``.    |
+-------------------------------+-------------------------------------+
| ``Clear``                     | reset the mk5daemon; useful         |
|                               | sometimes if ``mpifxcorr`` crashes. |
+-------------------------------+-------------------------------------+
| ``Reboot``                    | causes machine to reboot.           |
+-------------------------------+-------------------------------------+
| ``Poweroff``                  | causes machine to power down.       |
+-------------------------------+-------------------------------------+
| ``Copy <bank> <vsn> <scans>`` | causes scans to be copied to local  |
|                               | disk.                               |
+-------------------------------+-------------------------------------+

DifxLoadMessage
---------------

This section describes messages with = ``DifxLoadMessage``. These
messages contain CPU and memory utilization information and are
voluntarily sent by various nodes of the cluster, to be received by the
operator interface.

The of this message type contains:

   ::

      <difxLoad>
            <cpuLoad>|bfit[cpuLoad]</cpuLoad>
            <totalMemory>|bfit[totalMemory]</totalMemory>
            <usedMemory>|bfit[usedMemory]</usedMemory>
          </difxLoad>

The italicized fields are as follows:

CPU utilization on the cluster node. It is a floating point value
containing the average number of processes scheduled at one time.

total memory on node, in kiB.

used memory on node, in kiB.

DifxParameter
-------------

The structure of a DifxParameter message is as follows:

   ::

      <difxParameter>
            <targetMipId>|bfit[id]</targetMpiId>
            <name>|bfit[name]</name>
            <index1>|bfit[index1]</index1>
            .
            .
            .
            <indexN>|bfit[indexN]</indexN>
            <value>|bfit[value]</value>
          </difxParameter>

Such a message is intended to allow a parameter, possibly qualified with
array indices, to be set to a particular value. A possible use, for
example, is to change a station clock value within a running DiFX
instantiation or to enable generation of fast dump spectra for transient
searching.

The italicized fields are as follows:

the MPI process Id (rank) to target with this message. Values zero and
greater target specific MPI processes, with zero always being the
manager process. Other special values include:

===== ===============================
value meaning
===== ===============================
-1    all MPI processes
-2    all core (processing) processes
-3    all datastream processes
===== ===============================

the name of the parameter to set. For array types, the following values
specify the element to set.

an integer specifying the index of the particular array axis.

a string containing the value.

DifxSmartMessage
----------------

The Self-Monitoring, Analysis and Reporting Technology (SMART) protocol
is used by hard drives for health monitoring. The Mark5 units support
this and make the SMART values available via the Streamstor API.
``mk5daemon`` accesses this information periodically and on module
insertion so that operators can be made aware of obvious and potential
module problems as early as possible. The DifxSmartMessage multicast
message is used to convey such SMART information for individual drives
in a module. It should be expected that when such messages are sent a
separate message will come for each drive (typically 8) of a module.

The of this message type contains:

   ::

      <difxSmart>
            <mjd>|bfit[mjd]</mjd>
            <vsn>|bfit[vsn]</vsn>
            <slot>|bfit[slot]</slot>
            <smart id=|bfit[smartId] value=|bfit[value] />
            .
            .
            .
          </difxSmart>

Multple ``smart`` tags (8 to 16 are usual) will usually be present in
each smart message.

The italicized fields are as follows:

The time at which the SMART value was extracted.

The module number.

The slot of the hard drive in the module (0 to 7).

The identifier for the value being represented. This is usually a small
(< 300) positive integer. More information can be found at
http://en.wikipedia.org/wiki/S.M.A.R.T.

The value of the monitor point corresponding to the .

DifxTransientMessage
--------------------

This section describes messages with = ``DifxTransientMessage``. This
message is related to a commensal transient search project. A message of
this type should be sent as soon as possible after detection; it is
likely that no provisions will be made for data copying that does not
start before resources assigned to the job are released. When a possible
transient is identfied by a detecting program which looks at
autocorrelations it sends this message to all Mark5 units. Once
correlation is complete, the ``mk5daemon`` program on appropriate Mark5
units will take a few seconds to copy data from the time range(s) of
interest.

The of this message type contains:

   ::

      <difxTransient>
            <jobId>|bfit[jobId]</jobId>
            <startMJD>|bfit[startMJD]</startMJD>
            <stopMJD>|bfit[stopMJD]</stopMJD>
            <priority>|bfit[priority]</priority>
            <destDir>|bfit[destDir]</destDir>
            <comment>|bfit[comment]</comment>
          </difxTransient>

The italicized fields are as follows:

The job indentification string. This is required so that only relevant
Mark5 units take action on the received message.

A string containing the name of the event as declared by the transient
detector.

The start time (MJD) of the segment of data to copy.

The stop time (MJD) of the segment of data to copy. Note that the total
amount of data to be copied should be low enough to have an
inconsequential impact on correlation throughput.

A floating point value indicating the relative importance of capturing
this event. In jobs where many triggers occur this field will be used to
select the most important ones to save to disk. Higher numbers indicate
higher priority.

(optional) A final directory to store the baseband data. If not
provided, a default will be assumed. Note that behavior is undefined if
different destination directories are provided within a single job.

(optional) A string provided by the transient detector containing a
tentative classification.

(optional) A comment that could be appended to a log file.

All data and a log file will be stored in a subdirectory of the data
staging area named . The log file, at a minimum, will contain the list
of events sent by the transient detection program and a log of the
copying process, indicating any errors that may have occured. The
subdirectory will have a temporary name starting with a period (``.``)
until all data copy for the job in question is complete. The use of this
message type is demonstrated in
Fig. `[fig:transient] <#fig:transient>`__.

DifxStart
---------

This document type causes the head node to spawn a correlator job. The
doument contents describe which resources to use and which ``.input``
file to use.

The of the message contains:

   ::

      <difxStart>
            <input>|bfit[input file]</input>
            <force>|bfit[forceOverwrite]</force>
            <manager node="|bfit[node]" />
            <datastream nodes="|bfit[nodes]" />
            <process nodes="|bfit[nodes]" threads="|bfit[count]" />
            <env>|bfit[envvar]=|bfit[value]</env>
            <difxProgram>|bfit[program]</difxProgram>
            <difxVersion>|bfit[version]</difxVersion>
            <mpiWrapper>|bfit[mpiWrapper]</mpiWrapper>
            <mpiOptions>|bfit[options]</mpiOptions>
          </difxStart>

In the above XML file, exactly one manager node must be supplied. There
must be at least one datastream node (one per antenna being correlated).
There must be at least one process node. Zero or more (up to a maximum
of 8) environment variables may be set. The italicized fields are as
follows:

complete path to the ``.input`` file for this correlation.

cause any previous correlator output of this job to be deleted before
starting the correlation. A value of ``1`` or ``True`` will enable
overwrite.

the value of the environment variable.

the name of the node being assigned, e.g. ``mark5fx02`` or ``swc000``.

a list of node names. The list members should be space or comma
separated.

the maximum number of threads to schedule. If not specified, 1 will be
assumed. This applies only to process nodes.

an environment variable to set before running mpifxcorr.

the name of the software correlator executable. This is optional and
defaults to ``mpifxcorr`` if not set.

the version (e.g., *DIFX-1.5.4*) of DiFX to run.

the name of the program used to start the MPI processes. This field is
optional and defaults to ``mpirun`` if none is provided.

extra options to pass to ``mpirun``. This is optional; sensible defaults
are assumed if not explicitly set.

Note that multiple ``<process />`` tags can be specified, each with its
own thread count. Each tag’s thread count only affects those nodes
specified in that tag. If is provided, then a wrapper script called
``runmpifxcorr.``\ *version* is expected to be in the default path which
sets the environment for the version of DiF to actuallyt run.

DifxStatusMessage
-----------------

This section describes messages with = ``DifxStatusMessage``. This
message type is only produced by ``mpifxcorr`` or the programs
immediately responsible for starting and stopping it.

The of the message contains:

   ::

      <difxStatus>
            <state>|bfit[state]</state>
            <message>|bfit[message]</message>
            <visibilityMJD>|bfit[visibilityMJD]</visibilityMJD>
            <weight ant=|bfit[antId] wt=|bfit[weight]>
          </difxStatus>

The italicized fields are as follows:

the state of ``mpifxcorr``, which must be one of the following:

+-----------------+---------------------------------------------------+
| state           | meaning                                           |
+=================+===================================================+
| ``Spawning``    | the ``mpifxcorr`` processes are being started     |
|                 | (not sent by ``mpifxcorr``).                      |
+-----------------+---------------------------------------------------+
| ``Starting``    | all the processes are ready to begin.             |
+-----------------+---------------------------------------------------+
| ``Running``     | the correlator is running.                        |
+-----------------+---------------------------------------------------+
| ``Ending``      | the correlator has reached the end of the job.    |
+-----------------+---------------------------------------------------+
| ``Done``        | the correlation has completed.                    |
+-----------------+---------------------------------------------------+
| ``Aborting``    | correlation is stopping early due to an error.    |
+-----------------+---------------------------------------------------+
| ``Terminating`` | correlation is stopping early due to signal.      |
+-----------------+---------------------------------------------------+
| ``Terminated``  | correlation has stopped early.                    |
+-----------------+---------------------------------------------------+
| ``MpiDone``     | all of the MPI processes have ended (not sent by  |
|                 | ``mpifxcorr``).                                   |
+-----------------+---------------------------------------------------+
| ``Crashed``     | ``mpifxcorr`` crashed; usually sent by            |
|                 | ``mk5daemon``.                                    |
+-----------------+---------------------------------------------------+

a string containing information for the operator.

the time-stamp (MJD + fraction) of last completed visibility record.

the antenna id for the associated weight, ranging from 0 to
:math:`N_{\mathrm{ant}}-1`.

the data weight for the associated antenna, ranging from 0 to 1. Note
that in each XML document of this type there will in general be one
value for each antenna being correlated.

DifxStop
--------

Messages with = ``DifxStop`` are typically sent by the DiFX Operator
Interface to the ``mk5daemon`` running on the correlator head node to
cause a particular instance of DiFX to be killed.

The of the message contains:

   ::

      <difxStop>
          </difxStop>

Mark5DriveStatsMessage
----------------------

Mark5 module conditioning is done periodically to ensure top performance
of Mark5 modules. Each disk in the module gets written across its whole
surface to identify bad areas and to *calibrate* the electronics. One
message applies to one disk of the module

The of the message contains:

   ::

      <difxDriveStats>
            <serialNumber>|bfit[serial]</serialNumber>
            <modelNumber>|bfit[model]</modelNumber>
            <size>|bfit[size]</size>
            <moduleVSN>|bfit[vsn]</moduleVSN>
            <moduleSlot>|bfit[slot]</moduleSlot>
            <startMJD>|bfit[startMJD]</startMJD>
            <stopMJD>|bfit[stopMJD]</stopMJD>
            <bin|bfit[N]>|bfit[statsN]</bin|bfit[N]>
            <type>|bfit[statsType]</type>
            <startByte>|bfit[startByte]</startByte>
          </difxDriveStats>

The italicized fields are as follows:

the serial number of the disk.

the model number of the disk.

the size of the disk, in GB.

the module Volume Serial Number (VSN).

the location of the disk within the module, from 0 to 7.

the time when conditioning began.

the time when condtioning ended.

the histogram count for bin for in the range 0 to 7.

type of operation which is one of ``condition``, ``condition_read``,
``condition_write``, ``read``, ``write``, ``unknown``, ``test``.

if not present, assumed to be zero; only relevant for some types of
operations.

Mark5StatusMessage
------------------

This section describes messages with = ``Mark5StatusMessage``. This
message type cones from either ``mpifxcorr`` or ``mk5daemon`` (or
perhaps another program that makes heavy use of Mark5 units and wishes
to volunteer status information).

The of the message contains:

   ::

      <mark5Status>
            <bankAVSN>|bfit[vsnA]</bankAVSN>
            <bankBVSN>|bfit[vsnB]</bankBVSN>
            <statusWord>|bfit[statusWord]</statusWord>
            <activeBank>|bfit[activeBank]</activeBank>
            <state>|bfit[state]</state>
            <scanNumber>|bfit[scanNumber]</scanNumber>
            <scanName>|bfit[scanName]</scanName>
            <position>|bfit[position]</position>
            <playRate>|bfit[playRate]</playRate>
            <dataMJD>|bfit[dataMJD]</dataMJD>
          </mark5Status>

The italicized fields are as follows:

the VSN of the module in bank A.

the VSN of the module in bank B.

a hexadecimal number with the following bits: *TBD*

the active bank, either ``A`` or ``B`` for banks A and B respectively,
``N`` if the unit is in non-bank mode, or blank if no modules are
active.

the state of the Mark5 unit:

+------------------+--------------------------------------------------+
| state            | meaning                                          |
+==================+==================================================+
| ``Opening``      | the Streamstor card is being opened.             |
+------------------+--------------------------------------------------+
| ``Open``         | the Streamstor was successfully opened and is    |
|                  | ready for use.                                   |
+------------------+--------------------------------------------------+
| ``Close``        | the Streamstor has been closed.                  |
+------------------+--------------------------------------------------+
| ``GetDirectory`` | the unit is recovering the directory or finding  |
|                  | data.                                            |
+------------------+--------------------------------------------------+
| ``GotDirectory`` | the unit successfully found needed data on the   |
|                  | module.                                          |
+------------------+--------------------------------------------------+
| ``Play``         | the unit is playing back data.                   |
+------------------+--------------------------------------------------+
| ``PlayStart``    | the unit is about to start playback.             |
+------------------+--------------------------------------------------+
| ``PlayInvalid``  | the unit is playing data, but the data is        |
|                  | invalid.                                         |
+------------------+--------------------------------------------------+
| ``Idle``         | the unit is not doing anything; no process has   |
|                  | control of it.                                   |
+------------------+--------------------------------------------------+
| ``Error``        | the unit is unusable due to an error.            |
+------------------+--------------------------------------------------+
| ``Busy``         | the unit is busy and cannot respect commands.    |
+------------------+--------------------------------------------------+
| ``Initializing`` | the Streamstor card is initializing.             |
+------------------+--------------------------------------------------+
| ``Resetting``    | the unit is resetting the Streamstor card.       |
+------------------+--------------------------------------------------+
| ``Rebooting``    | the unit is about to reboot.                     |
+------------------+--------------------------------------------------+
| ``Poweroff``     | the unit is about to turn off.                   |
+------------------+--------------------------------------------------+
| ``NoData``       | the unit is not playing data since there is none |
|                  | that is appropriate.                             |
+------------------+--------------------------------------------------+
| ``NoMoreData``   | the unit has played all the data for the job and |
|                  | is stopped.                                      |
+------------------+--------------------------------------------------+
| ``Copy``         | data is being copied off the module to a local   |
|                  | disk.                                            |
+------------------+--------------------------------------------------+

the directory index number for the current scan. This number starts at
1.

the name associated with the current scan.

the byte position being accessed. Note that this number can be very
large (:math:`> 2^{46}`).

the time-averaged playback rate in Mbps.

the date stamp (MJD + fraction) of the most recently read data.

Mark5VersionMessage
-------------------

This section describes messages with = ``Mark5VersionMessage``. This
message comes from ``mk5daemon``. It is typically broadcast once upon
the start of ``mk5daemon`` and when requested.

The of the message contains:

   ::

      <mark5Version>
            <ApiVer>|bfit[ApiVer]</ApiVer>
            <ApiDate>|bfit[ApiDate]</ApiDate>
            <FirmVer>|bfit[FirmVer]</FirmVer>
            <FirmDate>|bfit[FirmDate]</FirmDate>
            <MonVer>|bfit[MonVer]</MonVer>
            <XbarVer>|bfit[XbarVer]</XbarVer>
            <AtaVer>|bfit[AtaVer]</AtaVer>
            <UAtaVer>|bfit[UAtaVer]</UAtaVer>
            <DriverVer>|bfit[DriverVer]</DriverVer>
            <BoardType>|bfit[BoardType]</BoardType>
            <SerialNum>|bfit[SerialNum]</SerialNum>
            <DaughterBoard>
              <PCBType>|bfit[PCBType]</PCBType>
              <PCBSubType>|bfit[PCBSubType]</PCBSubType>
              <PCBVer>|bfit[PCBVersion]</PCBVer>
              <FPGAConfig>|bfit[FPGAConfig]</FPGAConfig>
              <FPGAConfigVer>|bfit[FPGAConfigVersion]</FPGAConfigVer>
            </DaughterBoard>
          </mark5Version>

Note that the ``DaughterBoard`` tag and its subtags are optional and are
not broadcast if a daughter board is not detected on the Mark5C unit.
The italicized fields are as follows:

The software API version of the Streamstor API.

Date associated with the above.

The version of the firmware that is loaded.

Date associated with the above.

The version of the Monitor FPGA code.

The version of the cross bar FPGA code

The version of the ATA disk controller FPGA code.

The version of the UATA disk controller FPGA code.

The version number of the driver code.

The type of Streamstor board.

The serial number of the Streamstor board.

The type of Streamstor daughter board.

Subtype of the above, if any.

The version of the daughter board.

Name of the FPGA configuration.

Version number of FPGA configuration.
