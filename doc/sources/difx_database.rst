.. _sec:database:

Database tables
===============

A database backend (currently Oracle) is used to store certain bits of
information that are important for the operation of the VLBA-DiFX
correlator. The same physical database is used to store monitor data
from VLBA observations and log data from foreign stations that are
processed through ``fs2db``. This version of this document describes
only new features, including database tables not used in the hardware
correlator era and the software that populates and uses this
information. The new software in question accesses the database through
one of two mechanisms. Python programs use the ``cx_Oracle`` library and
Java programs (e.g., the DiFX Operator Interface) use ``JAXB``.

The DIFXQUEUE table
-------------------

The DIFXQUEUE table is used to specify the state of the correlator
queue. Each job can have a unique entry in this table. The structure of
this table is based on the FXQUEUE table used by ``OMS``, but this table
is incompatible with ``OMS`` and should be treated as a completely
parallel development. The program ``difxqueue`` will populate this table
for each job being staged for correlation. Initially the STATUS field
will be “QUEUED”, but will change to one of the other options in the
course of correlation. The DiFX Operator Interface (DOI) uses this
database table directly.

+-----------------+---------------+----------------------------------+
| Column          |               |                                  |
+=================+===============+==================================+
| PROPOSAL        | VARCHAR2(10)  | The proposal code                |
+-----------------+---------------+----------------------------------+
| SEGMENT         | VARCHAR2(2)   | Segment (epoch) of proposal, or  |
|                 |               | blank                            |
+-----------------+---------------+----------------------------------+
| JOB_PASS        | VARCHAR2(32)  | Name of correlator pass (e.g.    |
|                 |               | “geodesy”)                       |
+-----------------+---------------+----------------------------------+
| JOB_NUMBER      | INTEGER       | Number of job in the pass        |
+-----------------+---------------+----------------------------------+
| PRIORITY        | INTEGER       | Number indicating the priority   |
|                 |               | of the job in the queue;         |
+-----------------+---------------+----------------------------------+
|                 |               | 1 is high, 2 is default, and 3   |
|                 |               | is low                           |
+-----------------+---------------+----------------------------------+
| JOB_START       | DATE          | Observe time of job start        |
+-----------------+---------------+----------------------------------+
| JOB_STOP        | DATE          | Observe time of job stop         |
+-----------------+---------------+----------------------------------+
| SPEEDUP         | FLOAT         | Estimated speed-up factor for    |
|                 |               | job                              |
+-----------------+---------------+----------------------------------+
| INPUT_FILE      | VARCHAR2(512) | Full path of the VLBA-DiFX input |
|                 |               | file  [3]_                       |
+-----------------+---------------+----------------------------------+
| STATUS          | VARCHAR2(32)  | Status of the job, perhaps       |
|                 |               | “QUEUED”, “KILLED”,              |
+-----------------+---------------+----------------------------------+
|                 |               | “RUNNING”, “FAILED”, “UNKNOWN”   |
|                 |               | or “COMPLETE”                    |
+-----------------+---------------+----------------------------------+
| NUM_ANT         | INTEGER       | Number of antennas in the job    |
+-----------------+---------------+----------------------------------+
| CORR_TYPE       | VARCHAR2(32)  | Type of correlation (e.g.,       |
|                 |               | “PRODUCTION” or “CLOCK”)         |
+-----------------+---------------+----------------------------------+
| CORR_VERSION    | VARCHAR2(32)  | The DiFX version string          |
+-----------------+---------------+----------------------------------+
| NUM_FOREIGN     | INTEGER       | Number of non-VLBA antennas in   |
|                 |               | job                              |
+-----------------+---------------+----------------------------------+
| OUTPUT_SIZE_EST | INTEGER       | Estimated correlator output size |
|                 |               | (MB)                             |
+-----------------+---------------+----------------------------------+

The DIFXLOG table
-----------------

The DIFXLOG table contains a list of all correlation attempts and is
basesd on the FXLOG table used by the hardware correlator and ``OMS``.
In the case of successful correlation, the CORR_STATUS field will be set
to “COMPLETE” and the SPEEDUP and OUTPUT_SIZE fields will be set.

+--------------+---------------+-------------------------------------+
| Column       |               |                                     |
+==============+===============+=====================================+
| PROPOSAL     | VARCHAR2(10)  | The proposal code                   |
+--------------+---------------+-------------------------------------+
| SEGMENT      | VARCHAR2(2)   | Segment (epoch) of proposal, or     |
|              |               | blank                               |
+--------------+---------------+-------------------------------------+
| JOB_PASS     | VARCHAR2(32)  | Name of correlator pass (e.g.       |
|              |               | “geodesy”)                          |
+--------------+---------------+-------------------------------------+
| JOB_NUMBER   | INTEGER       | Number of job in the pass           |
+--------------+---------------+-------------------------------------+
| CORR_START   | DATE          | Start time/date of correlation      |
+--------------+---------------+-------------------------------------+
| CORR_STOP    | DATE          | Stop time/date of correlation       |
+--------------+---------------+-------------------------------------+
| SPEEDUP      | FLOAT         | Measured speed-up factor            |
+--------------+---------------+-------------------------------------+
| INPUT_FILE   | VARCHAR2(512) | File name of .input file            |
+--------------+---------------+-------------------------------------+
| OUTPUT_FILE  | VARCHAR2(512) | File name of correlator output      |
+--------------+---------------+-------------------------------------+
| OUTPUT_SIZE  | INTEGER       | Size (in :math:`10^6` bytes) of     |
|              |               | correlator output                   |
+--------------+---------------+-------------------------------------+
| CORR_STATUS  | VARCHAR2(32)  | Status of correlation, typically    |
|              |               | “COMPLETED”                         |
+--------------+---------------+-------------------------------------+
| CORR_TYPE    | VARCHAR2(32)  | Type of correlation (e.g.,          |
|              |               | “PRODUCTION” or “CLOCK”)            |
+--------------+---------------+-------------------------------------+
| CORR_VERSION | VARCHAR2(32)  | The DiFX version string             |
+--------------+---------------+-------------------------------------+

The CONDITION table
-------------------

The CONDITION table contains performance information for the hard disks
comprising Mark5 modules. A separate table entry is made for each disk
in a module so typically there will be 8 entries generated for each
module conditioned. There are two paths to get data into this table. The
``condition`` program can be used to manually load condition reports
from the SSErase program. Secondly, the ``condition_watch`` program
automatically populates the database immediately after module
conditioning upon receipt of a ``Mark5ConditionMessage`` that is now
generated by a specially modified version of Haystack Observatory’s
``SSErase`` program.

+-----------+--------------+-----------------------------------------------------+
| Column    |              |                                                     |
+===========+==============+=====================================================+
| SERIALNUM | VARCHAR2(32) | The hard disk serial number                         |
+-----------+--------------+-----------------------------------------------------+
| MODEL     | VARCHAR2(32) | Model number of hard disk                           |
+-----------+--------------+-----------------------------------------------------+
| CAPACITY  | INTEGER      | Size of disk in :math:`10^9` bytes                  |
+-----------+--------------+-----------------------------------------------------+
| MODULEVSN | VARCHAR2(10) | The name of the module containing the disk          |
+-----------+--------------+-----------------------------------------------------+
| SLOT      | INTEGER      | The slot number within the module (0 to 7)          |
+-----------+--------------+-----------------------------------------------------+
| STARTTIME | DATE         | Date/time of conditioning start                     |
+-----------+--------------+-----------------------------------------------------+
| STOPTIME  | DATE         | Date/time of conditioning completion                |
+-----------+--------------+-----------------------------------------------------+
| BIN0      | INTEGER      | Bin 0 of performance histogram (:math:`<` 1.125 ms) |
+-----------+--------------+-----------------------------------------------------+
| BIN1      | INTEGER      | Bin 1 of performance histogram (:math:`<` 2.25 ms)  |
+-----------+--------------+-----------------------------------------------------+
| BIN2      | INTEGER      | Bin 2 of performance histogram (:math:`<` 4.5 ms)   |
+-----------+--------------+-----------------------------------------------------+
| BIN3      | INTEGER      | Bin 3 of performance histogram (:math:`<` 9 ms)     |
+-----------+--------------+-----------------------------------------------------+
| BIN4      | INTEGER      | Bin 4 of performance histogram (:math:`<` 18 ms)    |
+-----------+--------------+-----------------------------------------------------+
| BIN5      | INTEGER      | Bin 5 of performance histogram (:math:`<` 36 ms)    |
+-----------+--------------+-----------------------------------------------------+
| BIN6      | INTEGER      | Bin 6 of performance histogram (:math:`<` 72 ms)    |
+-----------+--------------+-----------------------------------------------------+
| BIN7      | INTEGER      | Bin 7 of performance histogram (:math:`\ge` 72 ms)  |
+-----------+--------------+-----------------------------------------------------+

.. [1]
   INPUT_FILE is the primary key for this table.

.. [2]
   INPUT_FILE is the primary key for this table.

.. [3]
   INPUT_FILE is the primary key for this table.
