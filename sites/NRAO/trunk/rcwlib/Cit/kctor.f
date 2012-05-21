C*KCTOR -- convert character string to floating-point number
C+
      DOUBLE PRECISION FUNCTION KCTOR (S,I)
      CHARACTER*(*) S
      INTEGER I
C
C KCTOR: attempt to read a real number from a character string, and
C return the result as a double-precision quantity. No attempt is made
C to avoid floating-point overflow or underflow. 
C
C A "number" consists of the following parts, any of which
C may be omitted: SIGN, INTEGER-PART, FRACTIONAL-PART, SCALE-FACTOR.
C Blanks are not allowed anywhere within the number.
C SIGN is + or -. INTEGER-PART is 0 or more decimal digits. FRACTIONAL-
C PART is a decimal-point (.) followed by 0 or more decimal digits.
C SCALE-FACTOR is the letter E or e followed by an optional + or - sign
C and one or more digits, representing a power of 10.  A value may not
C consist only of an scale-factor. One or more colons (:) are allowed 
C in the integer-part. The effect of a colon is to multiply the 
C preceding part by 60 and add it to the following part.  This allows 
C the notation hh:mm:ss or ddd:mm:ss. 
C
C Examples of acceptable values:
C  0  1  -13  13.35  0.1335E+2  1.3351e1  04:13  12:00:05  -14:57.63
C
C Returns:
C  KCTOR           : the value of the number; if a valid number is
C                    not found starting at S(I:I), the value returned
C                    is zero.
C Arguments:
C  S      (input)  : character string to be parsed.
C  I      (in/out) : on input, I is the index of the first character
C                    in S to be examined; on output, either it points
C                    to the next character after a valid number, or
C                    it is equal to LEN(S)+1.
C History:
C  1985 Oct  8 - New routine, based on CTOR (T. J. Pearson).
C-----------------------------------------------------------------------
      CHARACTER*1 COLON, PLUS, MINUS, POINT, E, ELC
      PARAMETER (COLON=':', PLUS='+', MINUS='-', POINT='.')
      PARAMETER (E='E', ELC = 'e')
      DOUBLE PRECISION KCTOD, FRACTN
      INTEGER   KCTOI, SIGN, ISTART, N, J
C
      N = LEN(S)
      KCTOR = 0D0
      IF (I.GT.N) RETURN
C
C Read the SIGN (default plus).
C
      SIGN = +1
      IF (S(I:I).EQ.MINUS) THEN
          I = I+1
          SIGN = -1
      ELSE IF (S(I:I).EQ.PLUS) THEN
          I = I+1
      END IF
      ISTART = I
C
C Read the INTEGER-PART, which may contain colons for hexadecimal
C notation.
C
   10 KCTOR = KCTOD(S,I)*SIGN + KCTOR*60D0
      IF (I.GT.N) RETURN
      IF (S(I:I).EQ.COLON) THEN
          I = I+1
          GOTO 10
      END IF
C
C Read the FRACTIONAL-PART, introduced by a decimal point.
C
      IF (S(I:I).EQ.POINT) THEN
          J = I+1
          FRACTN = KCTOD(S,J)*SIGN
          KCTOR = KCTOR + FRACTN*10D0**(I-J+1)
          I = J
          IF (I.GT.N) RETURN
      END IF
C
C Read the SCALE-FACTOR, introduced by E or e.
C
      IF( (S(I:I).EQ.E .OR. S(I:I).EQ.ELC) .AND.
     1    (I.NE.ISTART)) THEN
          I = I+1
          IF (I.GT.N) RETURN
          SIGN = +1
          IF (S(I:I).EQ.MINUS) THEN
              I = I+1
              SIGN = -1
          ELSE IF (S(I:I).EQ.PLUS) THEN
              I = I+1 
          END IF
          KCTOR = KCTOR * 10D0**(SIGN*KCTOI(S,I))
      END IF
      END
