Baseband Data Formats
=====================

.. _sec:lbaformat:

LBA
---

.. _sec:vlbaformat:

VLBA
----

.. _sec:markivformat:

Mark IV
-------

.. _sec:mark5bformat:

Mark5B
------

.. _sec:vdifformat:

VDIF
----

The VLBI Data Interchange Format (FIXME: reference to
`vlbi.org <vlbi.org>`__) is a very flexible baseband data format being
used in almost all new VLBI backends (in 2015). The variety of modes
supported is very large. As of DiFX version 2.5 just about all possible
modes are supported in one way or another.

Some special VDIF concepts are described below.

.. _sec:vdiffanout:

Fanout modes
~~~~~~~~~~~~

The concept of fanout applies to certain variants of VDIF data where one
logical sampled channel is interleaved multiple threads. Certain modes
of the DBBC3 makes use of this mode. Reassembly of sampled data streams
is possible using exactly the same mechanism as multiplexing multiple
threads into a multi-channel, single-thread file. Certain restrictions
and usage rules apply:

#. Total number of threads is a multiple of the fanout factor.

#. The first *f* threads listed belong to the first channel to be
   reconstructed; these will become “channel 0” of the output
   single-thread VDIF file.

#. It is not possible to make use of fanout mode on VDIF data that
   contains multiple channels within each thread.

.. _sec:vdifedv:

Extended Data Versions
~~~~~~~~~~~~~~~~~~~~~~

.. _sec:vdifedv4:

Extended Data Version 4
~~~~~~~~~~~~~~~~~~~~~~~
