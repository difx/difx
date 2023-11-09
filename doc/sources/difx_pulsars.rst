.. role:: raw-latex(raw)
   :format: latex
..

.. _sec:pulsars:

DiFX and pulsars
================

DiFX supports four pulsar processing modes:

-  [psrmode:binarygate] **Binary Gating** A simple on-off pulse
   accumulation window can be specified with an “on” phase and an “off”
   phase. This can be used to boost the signal to noise ratio of pulsar
   observations by a factor of typically 3 to 6 and can also be used to
   search for off-pulse emission.

-  [psrmode:matchedgate] **Matched-filter Gating** If the pulse profile
   at the observation frequency is well understood and the pulse phase
   is very well predicted by the provided pulse ephemeris, additional
   signal to noise over binary gating can be attained by appropriately
   scaling correlation coefficients as a function of pulse phase.
   Depending on the pulse shape, addition gains by a factor of up to 1.5
   in sensitivity over binary gating are realizable.

-  [psrmode:bin] **Pulsar Binning** Pulsar *binning* is supported within
   DiFX. This entails generating a separate visibility spectrum for each
   requested range of pulse phase. There are no explicit limits to the
   number of pulse phase bins that are supported, however, data rates
   can become increasingly large. Currently AIPS does not support
   databases with multiple phase bins. Until there is proper
   post-processing support for pulsar binning, a separate FITS file will
   be produced for each pulsar phase bin.

-  [psrmode:profile] **Profile Mode** Profile mode is very different
   than the other three as it generates autocorrelations only that are
   used to determine the pulse shape and phase rather than generating
   cross correlations. This mode is enabled by placing ``mode=profile``
   in the global scope of the ``.v2d`` file (conventionally near the
   top). The ``.v2d`` file can enable as many antennas as desired (they
   will be averaged, so if you have a single large antenna it is
   probably best to include only that one), but can only operate on one
   source at a time. The output of ``mpifxcorr`` can be turned into an
   ASCII profile with ``difx2profile``. This profile can then be given
   to ``profile2binconfig.py`` to generate the ``.binconfig`` file that
   is used by the other three pulsar modes. There is some evidence that
   after about 10 minutes of integration the signal to noise ratio of
   the resultant profile stops growing. This remains to be fully
   understood. It could be that increasing the integration time helps;
   there is no reason not to use quite large integration times in this
   mode.

In all cases the observer will be responsible for providing a pulsar
spin ephemeris, and in all cases this ephemeris must provide an accurate
description of the pulsar’s rotation over the observation duration (the
pulsar phase must ont drift substantially with time). If gating is to be
applied then the ephemeris must be additionally be capable of pedicting
the absolute rotation phase of the pulsar. Enabling pulsar modes incurs
a minimum correlation-time penalty of about 50%. High output data rates
(computed from time resolution, number of spectral channels, and number
of pulsar bins) may require greater correlator resourse allocations. The
details of pulsar observing, including practical details of using the
pulsar modes and limitations imposed by operations, are documented at
http://library.nrao.edu/public/memos/vlba/up/VLBASU_32v2.pdf.

Pulse ephemeris
---------------

The use of any pulsar mode requires a pulse ephemeris to be provided by
the astronomer. This is a table of one or more polynomial entries, each
of which evaluates the pulsar’s rotation phase over an interval of
typically a few hours. The classic pulsar program ``Tempo`` can be used
to produce the polynomials required :raw-latex:`\cite{tempo}`. The pulse
phase must be evaluated at the Earth center which is usually specified
in ``tempo`` by station code 0 (zero). Many pulsars exhibit a great
degree of timing noise and hence the prediction of absolute pulse phase
may require updated timing observations. When submitting the polynomial
for use at the VLBA correlator, please adhere to the following naming
convention: *experiment*\ ``-``\ *pulsar*\ ``.polyco`` , e.g.,
``BB118A-B0950+08.polyco`` . Instructions for generating the polynomial
file are beyond the scope of this document.

Each ``.polyco`` contains one or more polynomials along with metadata;
an example ``.polyco`` file that is known to work with DiFX is shown
immediately below:

::

   1913+16     6-MAY-15   90748.00   57148.38041666690           168.742789 -0.722 -6.720
      6095250832.610975   16.940537786201    0   30   15  1408.000 0.7785   3.0960
     0.18914380470191894D-06  0.26835472311898462D+00 -0.10670985785738883D-02
    -0.85567503020416261D-05 -0.55633960226391698D-07 -0.37190642692987219D-09
    -0.58920583351397697D-12 -0.27311855964499407D-12 -0.21723579215912174D-13
     0.11968684344685061D-14  0.92517174535020731D-16 -0.28179552068141251D-17
    -0.18403230317431974D-18  0.25241984130137833D-20  0.13743173681516959D-21

A description of the file format is available at
http://tempo.sourceforge.net/ref_man_sections/tz-polyco.txt. Currently
``tempo`` (version 1) is well supported and ``tempo2`` is only supported
in ``tempo1`` compatibility mode. Eventual support for the ``tempo2``
*predictors* will be added. All ephemerides must be made for the virtual
Earth Center observatory (i.e., XYZ coordinates 0,0,0, usually
observatory code 0; DiFX versions prior to 2.5 would not accept any
non-numeric code even though they are legal). Any reference frequency
can be specified as the correlator takes dispersion into consideration.

Note that although ``tempo`` version 2 can produce usable ``.polyco``
files experience has shown that version 1 has fewer failure modes.

Bin configuration file
----------------------

All three pulsar modes also require the preparation of a ``.binconfig``
file by the astronomer. The contents of this file determine which of the
three pulsar modes is being used. Three pieces of information are
contained within this file: the pulsar ephemeris (polyco) files to
apply, definitions of the pulsar bins, and a boolean flag that
determines whether the bins are weighted and added within the
correlator. The file consists of a set of keywords (including a colon at
the end) that must be space padded to fill the first 20 columns of the
file and the values to assign to these keywords that start at column 21.
The file is case sensitive. The pulsar bins each consist of a ending
phase and a weight; each bin is implicitly assumed to start when the
previous ends and the first bin starts at the end phase of the last. The
phases are represented by a value between 0 and 1 and each successive
bin must have a larger ending phase than the previous. Examples for each
of the three pulsar modes are shown below:

Binary gating
~~~~~~~~~~~~~

::

   NUM POLYCO FILES:   1
   POLYCO FILE 0:      BB118A-B0950+08.polyco
   NUM PULSAR BINS:    2
   SCRUNCH OUTPUT:     TRUE
   BIN PHASE END 0:    0.030000
   BIN WEIGHT 0:       1.0
   BIN PHASE END 1:    0.990000
   BIN WEIGHT 1:       0.0

Matched-filter gating
~~~~~~~~~~~~~~~~~~~~~

::

   NUM POLYCO FILES:   1
   POLYCO FILE 0:      BB118A-B0950+08.polyco
   NUM PULSAR BINS:    6
   SCRUNCH OUTPUT:     TRUE
   BIN PHASE END 0:    0.010000
   BIN WEIGHT 0:       1.0
   BIN PHASE END 1:    0.030000
   BIN WEIGHT 1:       0.62
   BIN PHASE END 2:    0.050000
   BIN WEIGHT 2:       0.21
   BIN PHASE END 3:    0.950000
   BIN WEIGHT 3:       0.0
   BIN PHASE END 4:    0.970000
   BIN WEIGHT 4:       0.12
   BIN PHASE END 5:    0.990000
   BIN WEIGHT 5:       0.34

Note here that there is zero weight given to pulse phases ranging
between 0.05 and 0.95.

Pulsar binning
~~~~~~~~~~~~~~

::

   NUM POLYCO FILES:   1
   POLYCO FILE 0:      BB118A-B0950+08.polyco
   NUM PULSAR BINS:    20
   SCRUNCH OUTPUT:     FALSE
   BIN PHASE END 0:    0.025000
   BIN WEIGHT 0:       1.0
   BIN PHASE END 1:    0.075000
   BIN WEIGHT 1:       1.0
   BIN PHASE END 2:    0.125000
   BIN WEIGHT 2:       1.0
   BIN PHASE END 3:    0.175000
   BIN WEIGHT 3:       1.0
   .
   .
   .
   BIN PHASE END 18:   0.925000
   BIN WEIGHT 18:      1.0
   BIN PHASE END 19:   0.975000
   BIN WEIGHT 19:      1.0

The primary difference is ``SCRUNCH OUTPUT: FALSE`` which causes each
pulsar bin to be written to disk.

Preparing correlator jobs
-------------------------

When using ``vex2difx`` to prepare correlator jobs, one must associate
the pulsar with a setup of its own that includes reference to the
``.binconfig`` file. An excerpt from a ``.v2d`` file is below:

::

   SETUP gateB0950+08
   {
           tInt = 2.000
           nChan = 32
           doPolar = True
           binConfig = BB118A-B0950+08.binconfig
   }

   RULE B0950+08
   {
           source = B0950+08
           setup = gateB0950+08
   }

The ``.binconfig`` file should be in the same path as the ``.v2d`` file
when running ``vex2difx``.

Making FITS files
-----------------

For the two gating modes, preparing FITS files with ``difx2fits`` is no
different than for any other DiFX output. FITS-IDI does not support
multiple phase bins so the pulsar binning case is different and the
situation is non-optimal. Each pulsar bin must be made into its own
``FITS`` file with a separate execution of ``difx2fits``. The ``-B`` (or
``–bin``) command line option takes the bin number (starting at zero as
above) and writes a FITS file containing data only associated with that
bin number. Be sure to systematically name output files such that the
bin number is understood.
