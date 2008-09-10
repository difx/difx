      DOUBLE PRECISION FUNCTION sla_DTT (UTC)
*+
*     - - - -
*      D T T
*     - - - -
*
*  Increment to be applied to Coordinated Universal Time UTC to give
*  Terrestrial Time TT (formerly Ephemeris Time ET)
*
*  (double precision)
*
*  Given:
*     UTC      d      UTC date as a modified JD (JD-2400000.5)
*
*  Result:  TT-UTC in seconds
*
*  Pre 1972 January 1 a fixed value of 10 + ET-TAI is returned.
*
*  Called:  sla_DAT
*
*  P.T.Wallace   Starlink   2 November 1993
*-

      IMPLICIT NONE

      DOUBLE PRECISION UTC

      DOUBLE PRECISION sla_DAT


      sla_DTT=32.184D0+sla_DAT(UTC)

      END
