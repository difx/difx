C*KEYOUT -- SAVE/SHOW routine for KEYIN
C+
      SUBROUTINE KEYOUT(KEYS,VALUES,N,P)
      INTEGER N, P
      DOUBLE PRECISION KEYS(N), VALUES(N)
C
C SAVE/SHOW routine for KEYIN
C - list parameters and values to unit P
C Comments added RCW Jan. 19, 2010
C Input lines can be up to 255 characters. Keep the ~80 character
C standard for arrays, but allow longer character strings.
C-----------------------------------------------------------------------
      DOUBLE PRECISION BLANK8
      LOGICAL KEYCHK
      INTEGER I, J, K, LEN1, IVAL, LB
      CHARACTER*1 CONTIN
      CHARACTER*10 WORD
      CHARACTER*255 TEXT, DUMMYC
      DATA LB/0/
C
      CALL KPACK( '        ', BLANK8 )
C
C     Initialize variable counter and indicator of array vs long text.
C
      I = 1
      CONTIN = ' '
C
C     Begin loop through variables.  Return if done.
C
  100 IF (I.GT.N) RETURN
C
C     Write the variable name and "=" to the start of the output
C     line.  Write blank if this will be a continuation for an array.
C
      WRITE (WORD,'(A8)') KEYS(I)
      IF (CONTIN.EQ.',') WORD = ' '
      J = 1
C
C     Determine how many elements there are in a character string
C     (has blanks for the name beyond the first).
C
  110 IF (I+J.LE.N .AND. KEYS(I+J).EQ.BLANK8) THEN
          J = J+1
          GOTO 110
      END IF
C
C     Write the character string up to the maximum line length.
C     This was adjusted for longer lines.
C
      WRITE (TEXT,'(31A8)') (VALUES(K),K=I,I+MIN(J-1,30))
C
C     Reset array indicator to indicate whether the next variable
C     will be part of this array (KEYS equal).  If so, prepare to
C     write a comma after the value.  Write the keyword if this
C     is not an array or is the first element.
C
      CONTIN = ' '
      IF (I.LT.N .AND. KEYS(I).EQ.KEYS(I+J)) CONTIN = ','
      IF (WORD.NE.' ') CALL KEYPUT(WORD//' =',P,LB)
C
C     Deal with character strings including putting quotes around
C     them.  DUMMYC allows the right shift needed.
C
      IF (J.GT.1 .OR. KEYCHK(VALUES(I),8)) THEN
          IF (TEXT.NE.' ') THEN
              DUMMYC = TEXT
              TEXT = '"'//DUMMYC(1:LEN1(DUMMYC))//'"'
          ELSE
              TEXT = '""'
          END IF
C
C     Deal with numbers.  Just writes one of the numbers out.
C
      ELSE IF (DABS(VALUES(I)).LT.1D7) THEN
          IF (DMOD(VALUES(I),1D0).EQ.0D0) THEN
              IVAL = IDINT(VALUES(I))
              WRITE (TEXT,620) IVAL
          ELSE
              WRITE(TEXT,630) VALUES(I)
          END IF
      ELSE
          WRITE (TEXT,630) VALUES(I)
      END IF
C
C     Flush leading blanks from the output TEXT
C
  615 IF (TEXT(1:1).EQ.' ') THEN
          TEXT = TEXT(2:)
          GOTO 615
      ENDIF
      IF (TEXT.NE.' ') THEN
          DUMMYC = TEXT
          IF (CONTIN.EQ.',') TEXT = DUMMYC(1:LEN1(DUMMYC))//','
      ELSE
          IF (CONTIN.EQ.',') TEXT = ','
      ENDIF
C
C     Write the variable value.  Note KEYPUT only actually
C     writes when the line fills or a ***END is issued.
C
      CALL KEYPUT(' '//TEXT(1:LEN1(TEXT)),P,LB)
C
C     Cause the line to be written out.
C 
      IF (CONTIN.NE.',') CALL KEYPUT('***END',P,LB)
C
C     Get index for next variable and close the loop.
C
      I = I+J
      GOTO 100
C
  620 FORMAT(I9)
  630 FORMAT(1PG25.16)
C
      END
