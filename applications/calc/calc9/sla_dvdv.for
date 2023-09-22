      DOUBLE PRECISION FUNCTION sla_DVDV (VA, VB)
*+
*     - - - - -
*      D V D V
*     - - - - -
* 
*  Scalar product of two 3-vectors  (double precision)
*
*  Given:
*      VA      dp(3)     first vector
*      VB      dp(3)     second vector
*
*  The result is the scalar product VA.VB (double precision)
*
*  P.T.Wallace   Starlink   November 1984
*-

      IMPLICIT NONE

      DOUBLE PRECISION VA(3),VB(3)


      sla_DVDV=VA(1)*VB(1)+VA(2)*VB(2)+VA(3)*VB(3)

      END
