      PROGRAM TEST
C
C     Compiler problems?
C
      INTEGER I, J
C -------------------------
      J = 5
      I = 3
      IF( I .EQ. J ) THEN
         I = J + 1
      END IF
      STOP
      END
