      SUBROUTINE NXTWRD( STRING, START, CHAR1, CHARN )
C
C     Routine to deliver the range of characters of the next word in
C     a string.
C
      CHARACTER   STRING*(*), TAB*1
      INTEGER     START, CHAR1, CHARN
      INTEGER     ICHAR, LSTRING
C
      TAB = CHAR(9)
      LSTRING = LEN( STRING )
      ICHAR = START - 1
C
   10 CONTINUE
         ICHAR = ICHAR + 1
         IF( ICHAR .GT. LSTRING ) GO TO 980
         IF( STRING(ICHAR:ICHAR) .EQ. ' ' .OR.
     1       STRING(ICHAR:ICHAR) .EQ. TAB ) GO TO 10
      CHAR1 = ICHAR
C
   20 CONTINUE
         ICHAR = ICHAR + 1
         IF( STRING(ICHAR:ICHAR) .NE. ' ' .AND.
     1       STRING(ICHAR:ICHAR) .NE. TAB .AND.
     2       ICHAR .LT. LSTRING ) GO TO 20
      CHARN = ICHAR - 1
      IF( ICHAR .EQ. LSTRING ) CHARN = LSTRING
      GO TO 990
C
  980 CONTINUE
      CHAR1 = 0
C
  990 CONTINUE
      RETURN
      END
