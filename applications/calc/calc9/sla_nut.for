      SUBROUTINE sla_NUT (DATE, RMATN)
*+
*     - - - -
*      N U T
*     - - - -
*
*  Form the matrix of nutation for a given date - IAU 1980 theory
*  (double precision)
*
*  References:
*     Final report of the IAU Working Group on Nutation,
*      chairman P.K.Seidelmann, 1980.
*     Kaplan,G.H., 1981, USNO circular no. 163, pA3-6.
*
*  Given:
*     DATE   dp         TDB (loosely ET) as Modified Julian Date
*                                           (=JD-2400000.5)
*  Returned:
*     RMATN  dp(3,3)    nutation matrix
*
*  The matrix is in the sense   V(true)  =  RMATN * V(mean) .
*
*  Called:   sla_NUTC, sla_DEULER
*
*  P.T.Wallace   Starlink   10 May 1990
*-

      IMPLICIT NONE

      DOUBLE PRECISION DATE,RMATN(3,3)

      DOUBLE PRECISION DPSI,DEPS,EPS0



*  Nutation components and mean obliquity
      CALL sla_NUTC(DATE,DPSI,DEPS,EPS0)

*  Rotation matrix
      CALL sla_DEULER('XZX',EPS0,-DPSI,-(EPS0+DEPS),RMATN)

      END
