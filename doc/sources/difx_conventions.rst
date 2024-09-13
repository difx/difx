.. _sec:conventions:

Conventions
===========

.. _sec:clockconventions:

Clock offsets and rates
-----------------------

The clock offset (and its first derivative with respect to time, the
clock rate) are stored in a number of places through the correlator
toolchain. The convention used by vex is that the clock offset is
positive if the Data Acquisition System (DAS) time tick is early (i.e.,
the station clock is running fast) and accordingly the full name in the
vex file is *clock_early*. In all other parts of the system the opposite
sign convention is used, that is the clock offset is how late (slow) the
DAS clock is. In particular, all VLBA correlator job files, the VLBA
database, the DiFX ``.input`` file and FITS formatted output files all
use the “late” clock convention. Note in particular that the
*clockOffset* and *clockRate* parameters of the ANTENNA sections in the
``.v2d`` files use the “late” convention, not vex’s “early” convention.

.. _delayconventions:

Geometric delays and rates
--------------------------

The delays used to align datastreams before correlation are nominally
produced by the Goddard CALC package. The calculations are done on an
antenna basis using the Earth center for antenna A and the requested
station for antenna B and thus results in a negative delay for sources
above the horizon. The core of DiFX uses the opposite convention and
thus the delays and their time derivatives (rates) as stored in the
``.delay``, ``.rate`` and ``.im`` files use the “positive delay above
horizon” convention. The FITS-IDI files (as produced by ``difx2fits``
and the VLBA hardware correlator) use the same convention as CALC, that
is “negative delay above horizon”.

.. _sec:antennacoordconventions:

Antenna coordinates
-------------------

Geocentric :math:`(X,Y,Z)` coordinates are used universally within the
software correlator and its associated files. This usage pattern extends
to cover ``sched``, CALC, the VLBA database and the existing hardware
correlator as well. The values are everywhere reported in meters. The
:math:`Z`-axis points from the Earth center to the geographic North
pole. The :math:`X`-axis points from the Earth center to the
intersection of the Greenwich meridian and the equator (geographic
longitude :math:`0^{\circ}`, latitude :math:`0^{\circ}`). The
:math:`Y`-axis is orthogonal to both, forming a right handed coordinate
system; The :math:`Y`-axis thus points from the Earth center to
geographic longitude :math:`90^{\circ}`\ E, latitude :math:`0^{\circ}`.
The unit-length basis vectors for this coordinate system are called
:math:`\hat{x}`, :math:`\hat{y}`, and :math:`\hat{z}`. The only
exception to the above stated rules is within the AIPS software package.
Task FITLD flips the sign of the :math:`Y` coordinate, apparently to
maintain consistency with software behavior established in the early
days of VLBI.

.. _sec:uvwvconventions:

Baseline coordinates
--------------------

The baseline vectors are traditionally put in a coordinate system that
is fixed to the celestial sphere rather than the Earth; see the
discussion in §\ `[sec:calcif2] <#sec:calcif2>`__ for a discussion of
coordinates that is mathematically more precise. The unit-length basis
vectors for this coordinate system are called :math:`\hat{u}`,
:math:`\hat{v}`, and :math:`\hat{w}`. The axes are defined so that
:math:`\hat{w}` points in the direction of the observed source tangent
point, the :math:`\hat{u}` is orthogonal to both the vector pointing to
the celestial north pole :math:`\hat{N}`
(:math:`\delta_{\rm J2000} = +90^{\circ}`) and :math:`\hat{w}`, and
:math:`\hat{v}` orthogonal to :math:`\hat{u}` and :math:`\hat{w}`. Note
that the sign of the :math:`\hat{v}` is such that
:math:`\hat{v} \cdot \hat{N} > 0` and
:math:`\{\hat{u}, \hat{v}, \hat{w}\}` form a right-handed coordinate
system. A baseline vector :math:`\vec{B}_{ij}` is defined for an ordered
pair of antennas, indexed by :math:`i` and :math:`j` at geocentric
coordinates :math:`\vec{x}_i` and :math:`\vec{x}_j`. For convenience,
the Earth center will be denoted by :math:`\vec{x}_0` which has
coordinate value :math:`\vec{0}`. The :math:`(u, v, w)` coordinates for
baseline vector :math:`\vec{B}_{ij}` is given
by\ :math:`(\vec{B}_{ij} \cdot \hat{u}, \vec{B}_{ij} \cdot \hat{v}, \vec{B}_{ij} \cdot \hat{w})`.
There are two natural conventions for defining baseline vectors:
:math:`\vec{B}_{ij} = \vec{x}_i - \vec{x}_j` and
:math:`\vec{B}_{ij}  = -\vec{x}_i + \vec{x}_j`, which will be refered to
as first-plus and second-plus respectively; both conventions are used
within the correlator system. Antenna-based baseline vectors are stored
in polynomial form in the ``.im`` file and tabulated in the ``.uvw``
file. In all cases antenna-based baseline vectors are baseline vectors
defined as
:math:`\vec{B}_i \equiv \vec{B}_{0i} = \pm \vec{x}_0 \mp \vec{x}_i`. The
values stored in the ``.im`` and ``.uvw`` files adopt the first-plus
convention. For antennas on the Earth surface this implies that the
:math:`w` baseline component is always negative for antennas that see
the target source above the horizon. The ``difx`` format output from
``mpifxcorr`` contains a baseline vector for each visibility spectrum
computed from the locations of the antenna pair using the first-plus
convention. Note that in this file output, the reported baseline number
is :math:`256*i + j` and :math:`i` and :math:`j` are antenna indices
starting at 1. The FITS-IDI format written by ``difx2fits`` adheres to
the second-plus convention.

.. _phaseconventions:

Visibility phase
----------------
