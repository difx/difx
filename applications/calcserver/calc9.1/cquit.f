      SUBROUTINE QUIT_CALC(IPAR)
      IMPLICIT NONE
C
C     QUIT_CALC send a message to the scheduling program and quits.
C
C     Calling sequence -
C
C        Input variables:
C
C        1.  IPAR(5)   -  The array to be sent to the scheduler.
C
      INTEGER*2 IPAR(5)
C
C     CALL PRTN(IPAR)
      STOP
      END
