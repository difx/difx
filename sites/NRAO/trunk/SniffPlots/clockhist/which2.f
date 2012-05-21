      INTEGER FUNCTION WHICH2( I1, I2, A1, A2, N )
C
C     A function that returns the index I at which NAME1=ARRAY1(I)
C     and NAME2=ARRAY2(I).  It is for identifying baselines.
C     It returns 0 if there is no match.
C
      INTEGER N, I
      INTEGER  I1, A1(N)
      INTEGER  I2, A2(N)
C
C ----------------------------------------------------------------------
C
      WHICH2 = 0
      DO I = 1, N
         IF ( I1 .EQ. A1(I) .AND. I2 .EQ. A2(I) ) THEN
            WHICH2 = I
            GO TO 999
         END IF
      END DO
  999 CONTINUE
      RETURN
      END
