Internals for Developers
========================

This appendix is not intended as a DiFX Developer Guide. Rather, this
appendix documents a few complex internal details of DiFX source code in
spots where the source code lacks Doxygen and other self-documentation
and is not documented elsewhere either. Currently this appendix
describes the data layout of a few key data areas in DiFX that are
highly multidimensional.

Data Layout in Mpifxcorr class Core
-----------------------------------

The most complex “flat" arrays in class Core are
*threadscratchspace::threadcrosscorrs[]* and *processslot::results[]*.
Although these are flat 1-D arrays of complex32 float data in memory,
they contain concatenated data elements of non-constant size that
internally have a high level of logical nesting (6-D).

Core threadcrosscorrs[]
~~~~~~~~~~~~~~~~~~~~~~~

The data array *threadcrosscorrs[]* does not contain contiguous spectra,
but rather, stores scattered spectral slices of length
xmacLength :math:`\ge` 1. A full cross spectrum at one DiFX freqency is
reconstrictible by striding the array by NumBaselines \* NumPulsarBins
\* Sum(BaselinePolpairs) \* xmacLength.

.. container::
   :name: tab:A.threadcrosscorrs

   .. table:: Memory layout of *threadcrosscorrs[]*
   data[tab:A.threadcrosscorrs]

      +-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
      | Freq  |       |       |       |       |       |       |       |       |       |       |       |       |       |       |       |
      | 0     |       |       |       |       |       |       |       |       |       |       |       |       |       |       |       |
      +=======+=======+=======+=======+=======+=======+=======+=======+=======+=======+=======+=======+=======+=======+=======+=======+
      | Xmac  |       |       |       |       |       |       |       |       |       |       |       |       |       |       |       |
      | 0     |       |       |       |       |       |       |       |       |       |       |       |       |       |       |       |
      +-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
      | Bas   |       |       |       |       |       |       |       |       |       |       |       |       |       |       |       |
      | eline |       |       |       |       |       |       |       |       |       |       |       |       |       |       |       |
      | 0     |       |       |       |       |       |       |       |       |       |       |       |       |       |       |       |
      +-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
      | P     |       |       |       |       |       |       |       |       |       |       |       |       |       |       |       |
      | ulsar |       |       |       |       |       |       |       |       |       |       |       |       |       |       |       |
      | Bin 0 |       |       |       |       |       |       |       |       |       |       |       |       |       |       |       |
      +-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
      | Po    | Po    | Po    | Po    | Po    | Po    | Po    | Po    | Po    | Po    | Po    | Po    | Po    | Po    | Po    | Po    |
      | lpair | lpair | lpair | lpair | lpair | lpair | lpair | lpair | lpair | lpair | lpair | lpair | lpair | lpair | lpair | lpair |
      | 0     | 1     | 0     | 1     | 0     | 1     | 0     | 1     | 0     | 1     | 0     | 1     | 0     | 1     | 0     | 1     |
      +-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
      | Ch [0 | [0 1  | [0 1  | [0 1  | [0 1  | [0 1  | [0 1  | [0 1  | [4 5  | [4 5  | [4 5  | [4 5  | [4 5  | [4 5  | [4 5  | [4 5  |
      | 1 2   | 2 3]  | 2 3]  | 2 3]  | 2 3]  | 2 3]  | 2 3]  | 2 3]  | 6 7]  | 6 7]  | 6 7]  | 6 7]  | 6 7]  | 6 7]  | 6 7]  | 6 7]  |
      | 3]    |       |       |       |       |       |       |       |       |       |       |       |       |       |       |       |
      +-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+
      | 4 x   | 4 x   | 4 x   | 4 x   | 4 x   | 4 x   | 4 x   | 4 x   | 4 x   | 4 x   | 4 x   | 4 x   | 4 x   | 4 x   | 4 x   | 4 x   |
      | <     | <     | <     | <     | <     | <     | <     | <     | <     | <     | <     | <     | <     | <     | <     | <     |
      | cf32> | cf32> | cf32> | cf32> | cf32> | cf32> | cf32> | cf32> | cf32> | cf32> | cf32> | cf32> | cf32> | cf32> | cf32> | cf32> |
      +-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+-------+

Channel numbers in the Table `1 <#tab:A.threadcrosscorrs>`__ are an
example and assume an xmacLength of 4 channels, and a length of raw
un-averaged cross spectra of 8 channels.

Core results[]
~~~~~~~~~~~~~~

The results array contains up to six data areas
(Table `2 <#tab:A.coreresults>`__). Not all of them are always present.

.. container::
   :name: tab:A.coreresults

   .. table:: Data areas in Core *results[]* [tab:A.coreresults]

      +----------+----------+----------+----------+----------+----------+
      | Cross    | Baseline | Decor    | A        | Autocorr | Phase    |
      | Corrs    | Weights  | relation | utocorrs | Weights  | Cal Data |
      |          |          | Factors  |          |          |          |
      +==========+==========+==========+==========+==========+==========+
      | <K x     | <L x     | <M x     | <N x     | <N x     | <P x     |
      | cf32>    | f32>     | cf32>    | cf32>    | f32>     | cf32>    |
      +----------+----------+----------+----------+----------+----------+

| The layout of most of the data areas in Core *results[]* is easily
  reverse engineered from the source code. The area that stores
  cross-correlation data (Cross Corrs) is the most complex of these
  areas. The top level structure of that area is illustrated in
  Table `3 <#tab:A.coreresults.crosscorrs>`__. The sub-elements (Cross
  Corr Elements) of that area are described in
  Table `4 <#tab:A.coreresults.crosscorrselement>`__.
| Helper indices into some areas of Core *results[]* are pre-calculated
  in class Configuration. Such helper indices are utilized by classes
  Core and Visibility to locate shared data at a coarse level. For
  example, *coreresultbaselineoffset[freq][baseline]* points to the
  starting indices of whole Cross Corr Elements but not deeper.

After a coarse index lookup, manual iteration over the varying-length
higher data dimensions is needed to pin down the final absolute index of
the desired data of, say, one spectral channel.

.. container::
   :name: tab:A.coreresults.crosscorrs

   .. table:: Memory layout of Cross Corrs area within *results[]* data
   [tab:A.coreresults.crosscorrs]

      +----------------+----------------+----------------+----------------+
      | Freq 0         |                |                |                |
      +================+================+================+================+
      | Baseline 0     | Baseline 1     | Baseline 0     | Baseline 1     |
      +----------------+----------------+----------------+----------------+
      | <Cross Corr    | <Cross Corr    | <Cross Corr    | <Cross Corr    |
      | Element>       | Element>       | Element>       | Element>       |
      +----------------+----------------+----------------+----------------+
      | at coreindex=0 | at             | at             | at             |
      |                | corein         | coreinde       | coreinde       |
      |                | dex=crElemSize | x=2.crElemSize | x=3.crElemSize |
      +----------------+----------------+----------------+----------------+

The size of one Core Result Element in complex32 float elements is the
product of MaxConfigPhaseCenters \* BinLoops \* PolProducts \* FreqChans
/ ChansToAverage. This size is not constant - some baselines may have
fewer polarization products than other baselines, or the number of
channels may differ between DiFX frequencies.

.. container::
   :name: tab:A.coreresults.crosscorrselement

   .. table:: Memory layout of Cross Corr Elements in the Cross Corr
   area of within *results[]* data [tab:A.coreresults.crosscorrselement]

      +-------+-------+-------+-------+-------+-------+-------+-------+
      | Phase |       |       |       |       |       |       |       |
      | c     |       |       |       |       |       |       |       |
      | enter |       |       |       |       |       |       |       |
      | 0     |       |       |       |       |       |       |       |
      +=======+=======+=======+=======+=======+=======+=======+=======+
      | P     |       |       |       |       |       |       |       |
      | ulsar |       |       |       |       |       |       |       |
      | bin 0 |       |       |       |       |       |       |       |
      +-------+-------+-------+-------+-------+-------+-------+-------+
      | Po    | Po    | Po    | Po    | Po    | Po    | Po    | Po    |
      | lpair | lpair | lpair | lpair | lpair | lpair | lpair | lpair |
      | 0     | 1     | 0     | 1     | 0     | 1     | 0     | 1     |
      +-------+-------+-------+-------+-------+-------+-------+-------+
      | <spec | <spec | <spec | <spec | <spec | <spec | <spec | <spec |
      | trum> | trum> | trum> | trum> | trum> | trum> | trum> | trum> |
      +-------+-------+-------+-------+-------+-------+-------+-------+
      | nchan | nchan | nchan | nchan | nchan | nchan | nchan | nchan |
      | x     | x     | x     | x     | x     | x     | x     | x     |
      | <     | <     | <     | <     | <     | <     | <     | <     |
      | cf32> | cf32> | cf32> | cf32> | cf32> | cf32> | cf32> | cf32> |
      +-------+-------+-------+-------+-------+-------+-------+-------+
