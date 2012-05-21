      INTEGER FUNCTION WHICH( NAME, ARRAY, N )
C
C     A function that returns the element of ARRAY(N) that matches NAME.
C     It returns 0 if there is no match.
C
      INTEGER N, I
      CHARACTER  NAME*(*), ARRAY(N)*(*)
C
C ----------------------------------------------------------------------
C
      WHICH = 0
      DO I = 1, N
         IF ( NAME .EQ. ARRAY(I) ) THEN
            WHICH = I
            GO TO 999
         END IF
      END DO
  999 CONTINUE
      RETURN
      END
