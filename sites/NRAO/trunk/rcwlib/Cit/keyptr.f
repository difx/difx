      INTEGER FUNCTION KEYPTR( NAME, KC, KI )
C
C     Function originally for SCHED to find the location in the
C     KEYIN arrays of the data for input parameter NAME.
C
      CHARACTER    NAME*(*), KC(*)*(*), ONAME*8
      INTEGER      KI(*), NKEYS, I
C ------------------------------------------------------------------
      NKEYS = KI(3)
C
      IF( NKEYS .LE. 3 ) CALL ERROR( 'KEYPTR: No keys. ' )
C
C     Get the key.
C
      KEYPTR = 0
      DO I = 4, NKEYS
         IF( NAME .EQ. KC(I) ) THEN
            KEYPTR = KI(I)
            GO TO 100
         END IF
      END DO
C
      ONAME = NAME
      CALL ERROR( 'KEYPTR: Input '//ONAME//' not found.' )
C
  100 CONTINUE
C
      RETURN
      END
