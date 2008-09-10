      DOUBLE PRECISION FUNCTION sla_EQEQX (DATE)
*+
*     - - - - - -
*      E Q E Q X
*     - - - - - -
*
*  Equation of the equinoxes  (double precision)
*
*  Given:
*     DATE    dp      TDB (loosely ET) as Modified Julian Date
*                                          (JD-2400000.5)
*
*  The result is the equation of the equinoxes (double precision)
*  in radians:
*
*     Greenwich apparent ST = GMST + sla_EQEQX
*
*  Called:  sla_NUTC
*
*  Patrick Wallace   Starlink   February 1984
*- 

      IMPLICIT NONE

      DOUBLE PRECISION DATE

      DOUBLE PRECISION DPSI,DEPS,EPS0


*  Nutation
      CALL sla_NUTC(DATE,DPSI,DEPS,EPS0)

*  Equation of the equinoxes
      sla_EQEQX=DPSI*COS(EPS0+DEPS)

      END
