C*KEYCHK -- used by KEYIN
C+
      LOGICAL FUNCTION KEYCHK(RWORD,NC)
      DOUBLE PRECISION RWORD
      INTEGER NC
C 
C Test whether value could be a character string
C - It is considered to be character rather than
C numeric unless one or more bytes are ASCII control
C characters (0-31 decimal). Of course this doesn't
C always work as any character string is also a
C valid number.
C
C Other invalid codes: 127 (DEL)
C Character not allowed in 1st position: @
C-----------------------------------------------------------------------
      CHARACTER*8 WORD
      INTEGER I
C
      WRITE (WORD, '(A8)') RWORD
      KEYCHK=.FALSE.
      IF (WORD(1:1).EQ.'@') RETURN
      DO 10 I=1,NC
          IF(ICHAR(WORD(I:I)).LT.32 .OR. 
     1       ICHAR(WORD(I:I)).GE.127) RETURN
   10 CONTINUE
      KEYCHK=.TRUE.
      RETURN
      END
