      DOUBLE PRECISION FUNCTION sla_GMST (UT1)
*+
*     - - - - - 
*      G M S T
*     - - - - -
*
*  Conversion from universal time to sidereal time (double precision)
*
*  Given:
*    UT1    dp     universal time (strictly UT1) expressed as
*                  modified Julian Date (JD-2400000.5)
*
*  The result is the Greenwich mean sidereal time (double
*  precision, radians).
*
*  The IAU 1982 expression (see page S15 of 1984 Astronomical
*  Almanac) is used, but rearranged to reduce rounding errors.
*  This expression is always described as giving the GMST at
*  0 hours UT.  In fact, it gives the difference between the
*  GMST and the UT, which happens to equal the GMST (modulo
*  24 hours) at 0 hours UT each day.  In this routine, the
*  fractional UT is used directly as the argument for the
*  standard formula, and the fractional part of the UT is
*  added separately;  note that the factor 1.0027379... does
*  not appear.
*
*  Called:  sla_DRANRM
*
*  P.T.Wallace   Starlink   November 1988
*-

      IMPLICIT NONE

      DOUBLE PRECISION UT1

      DOUBLE PRECISION sla_DRANRM

      DOUBLE PRECISION D2PI,S2R
      PARAMETER (D2PI=6.283185307179586476925287D0,
     :           S2R=0.7272205216643039849D-4)

      DOUBLE PRECISION TU



*  Julian centuries from fundamental epoch J2000 to this UT
      TU=(UT1-51544.5D0)/36525D0

*  GMST at this UT
      sla_GMST=sla_DRANRM(MOD(UT1,1D0)*D2PI+
     :                    (24110.54841D0+
     :                    (8640184.812866D0+
     :                    (0.093104D0-6.2D-6*TU)*TU)*TU)*S2R)

      END
