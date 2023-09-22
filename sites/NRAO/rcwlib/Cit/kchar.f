      CHARACTER*(*) FUNCTION KCHAR( NAME, NCH, UP, VALUE, KC, KI )
C
C     Function for SCHED to recover a character string from the KEYIN
C     values array.  
C       NAME  is the input variable name.
C       NCH   is the length of the desired output.
C       UP    is a logical that tells whether to UPCASE the result.
C       VALUE is the KEYIN values array.
C       KC and KI are the arrays needed by KEYPTR to find where in
C             VALUES array to find the start of NAME
C
C     Note:  Do not put this function within write statements.  At
C     least on a SUN, you cannot have a write (the internal below)
C     within a write.
C
      INTEGER       MAXCHR, KCLEN
      PARAMETER     (MAXCHR=256)
      CHARACTER     NAME*(*), TEMP*(MAXCHR), KC(*)*(*)
      INTEGER       NCH, KI(*), I, I1, I2, KEYPTR
      DOUBLE PRECISION  VALUE(*)
      LOGICAL       UP
C -----------------------------------------------------------------
C     Protect against too long strings.
C      
      KCLEN = LEN(KCHAR)
      IF( NCH .GT. KCLEN ) THEN
         TEMP = 'KCHAR: Too many characters requested. '// NAME
         CALL ERROR( TEMP )
      END IF
C
C     First find where in VALUE the string should be.
C 
      I1 = KEYPTR( NAME, KC, KI )
C
C     Find the last element of value that is used.
C
      I2 = I1 + ( NCH - 1 ) / 8 
C
C     Extract the string.
C
      WRITE( TEMP, '( 32A8 )' ) (VALUE(I),I=I1,I2)
C
C     Upcase, if requested.
C
      IF( UP ) CALL UPCASE( TEMP )
C
C     Move to output.
C
      KCHAR = ' '
      KCHAR(1:NCH) = TEMP
C
      RETURN
      END
