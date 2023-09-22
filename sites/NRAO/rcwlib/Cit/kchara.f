      CHARACTER*(*) FUNCTION KCHARA( NAME, NCH, NVCH, IS, UP, 
     1                               VALUE, KC, KI )
C
C     Function for SCHED to recover a character string from the KEYIN
C     values array.   This is for the case of recovering an element
C     from an array of character strings. 
C       NAME  is the input variable name.
C       NCH   is the length of the desired output.
C       NVCH  is the length of space requested when array set up.
C             Same as NVCH in the call to KEYCHRA.  Determines the
C             number of elements in VALUES used for each value.
C       IS    is the element of the strings array.
C       UP    is a logical that tells whether to UPCASE the result.
C       VALUE is the KEYIN values array.
C       KC and KI are the arrays needed by KEYPTR to find where in
C             VALUES array to find the start of NAME
C
C     Note:  Do not put this function within write statements.  At
C     least on a SUN, you cannot have a write (the internal below)
C     within a write.
C
C     Adapted from very similar routine KCHARA on May 21, 2010.
C
      INTEGER       MAXCHR, KCLEN, NVCH, IS
      PARAMETER     (MAXCHR=256)
      CHARACTER     NAME*(*), TEMP*(MAXCHR), KC(*)*(*)
      INTEGER       NCH, KI(*), I, I1, I2, KEYPTR, N8
      DOUBLE PRECISION  VALUE(*)
      LOGICAL       UP
C -----------------------------------------------------------------
C     Protect against too long strings.
C      
      KCLEN = LEN(KCHARA)
      IF( NCH .GT. KCLEN ) THEN
         TEMP = 'KCHARA: Too many characters requested. '// NAME
         CALL ERROR( TEMP )
      END IF
C
C     How many elements of VALUES per array element.
C
      N8 = 1 + ( NVCH - 1 ) / 8
C
C     First find where in VALUE the string should be.
C 
      I1 = KEYPTR( NAME, KC, KI ) + ( IS - 1 ) * N8
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
      KCHARA = ' '
      KCHARA(1:NCH) = TEMP
C
      RETURN
      END
