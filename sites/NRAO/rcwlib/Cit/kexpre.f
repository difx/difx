C*KEXPRE -- evaluate arithmetic expression
C+
      DOUBLE PRECISION FUNCTION KEXPRE (S,I,IER)
      CHARACTER*(*) S
      INTEGER I, IER
C
C Evaluate an arithmetic expression in string S(I:).
C The value of I returned points to the
C first character which cannot be part of the expression, 
C or is IMAX+1. This is a pseudo-
C recursive procedure: the "stack level" is LEVEL.
C Dimensions are sufficient for 9 nested parentheses.
C
C History:
C  1985 Oct 8  - TJP.
C  1991 May 17 - TJP.
C-----------------------------------------------------------------------
      CHARACTER*1 PLUS,MINUS,STAR,SLASH,BRA,KET
      PARAMETER (PLUS='+', MINUS='-', STAR='*', SLASH='/')
      PARAMETER (BRA='(', KET=')')
      INTEGER SIGN(10), LEVEL, IP
      CHARACTER*1 OP(10)
      DOUBLE PRECISION RESULT(10),TERM(10),FACTOR(10),KCTOR
C
      IER = 0
      LEVEL = 1
C
C Evaluate 'expression'.
C
  100 RESULT(LEVEL) = 0.0
  110 SIGN(LEVEL) = +1
      IF (S(I:I).EQ.PLUS) THEN
          I = I+1
      ELSE IF (S(I:I).EQ.MINUS) THEN
          I = I+1
          SIGN(LEVEL)=-1
      END IF
C
C Evaluate 'term'.
C
      TERM(LEVEL) = 1.0
      OP(LEVEL) = STAR
  200 CONTINUE
C
C Evaluate 'factor': parenthesized expression = 'expression' at next
C level.
C
      IF (S(I:I).NE.BRA) GOTO 320
          I = I+1
          LEVEL = LEVEL+1
          IF (LEVEL.LE.10) GOTO 100
              IER = 11
              RETURN
  310     FACTOR(LEVEL-1) = RESULT(LEVEL)
          LEVEL = LEVEL-1
          IF (S(I:I).NE.KET) THEN
              IER = 12
              RETURN
          END IF
          I = I+1
          GOTO 340
  320 CONTINUE
      IP = I
      FACTOR(LEVEL) = KCTOR(S,I)
      IF (I.EQ.IP) THEN
          IER = 13
          RETURN
      END IF
  340 CONTINUE
C
C End 'factor'.
C
      IF (OP(LEVEL).EQ.STAR) THEN
          TERM(LEVEL) = TERM(LEVEL)*FACTOR(LEVEL)
      ELSE IF (OP(LEVEL).EQ.SLASH) THEN
          IF (FACTOR(LEVEL).EQ.0.0) THEN
              IER = 14
              RETURN
          ELSE
              TERM(LEVEL) = TERM(LEVEL)/FACTOR(LEVEL)
          END IF
      END IF
      IF (S(I:I).EQ.SLASH.OR.S(I:I).EQ.STAR) THEN
          OP(LEVEL) = S(I:I)
          I = I+1
          GOTO 200
      END IF
C
C End 'term'.
C
      RESULT(LEVEL) = RESULT(LEVEL) + SIGN(LEVEL)*TERM(LEVEL)
      IF (S(I:I).EQ.PLUS.OR.S(I:I).EQ.MINUS) GOTO 110
C
C End 'expression'.
C
      IF (LEVEL.GT.1) GOTO 310
      KEXPRE = RESULT(LEVEL)
      RETURN
      END
