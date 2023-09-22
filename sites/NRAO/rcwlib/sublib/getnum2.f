      DOUBLE PRECISION FUNCTION GETNUM2( STRING, CHAR1, CHARN )
C
C     Alternate routine to GETNUM that uses FORTRAN internal reads.
C
C     R. C. Walker 5jan98.
C     It seems from a test program that this routine is actually
C     a bit slower than the much more involved GETNUM.
C
      CHARACTER    STRING*(*)
      INTEGER      CHAR1, CHARN, IVAR
      DOUBLE PRECISION  VAR
C ----------------------------------------------------------------------
C     Treat integers and reals differently
C
      IF( INDEX( STRING(CHAR1:CHARN), '.' ) .EQ. 0 ) THEN
         READ( STRING(CHAR1:CHARN), *, ERR = 100 ) IVAR
         VAR = IVAR
      ELSE
         READ( STRING(CHAR1:CHARN), *, ERR = 100 ) VAR
      END IF
C
      GETNUM2 = VAR
      RETURN
C
  100 CONTINUE
      WRITE( *, '(A,A)' ) ' GETNUM: Bad number: ', STRING(CHAR1:CHARN)
      GETNUM2 = 0.D0
C
      RETURN
      END
