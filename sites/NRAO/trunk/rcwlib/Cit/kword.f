C*KWORD  -- get next word from character string
C+
      SUBROUTINE KWORD (S, I, WORD, L)
      CHARACTER*(*) S, WORD
      INTEGER I, L
C
C KWORD: attempt to read an keyword from a character string, and return
C the result. A keyword is any sequence of letters or digits, starting
C with a letter. As a special case, "/" is a word.
C
C Arguments:
C  S      (input)  : character string to be parsed.
C  I      (in/out) : on input, I is the index of the first character
C                    in S to be examined; on output, either it points
C                    to the next character after a valid keyword, or
C                    it is equal to LEN(S)+1.
C  WORD   (output) : the keyword found.
C  L      (output) : the number of characters in the keyword; zero
C                    if no keyword was found.
C
C History:
C  1985 Oct 13 - New routine (T. J. Pearson).
C-----------------------------------------------------------------------
      CHARACTER ALPHAS*52, DIGITS*13
      PARAMETER (ALPHAS='ABCDEFGHIJKLMNOPQRSTUVWXYZ'//
     1                  'abcdefghijklmnopqrstuvwxyz')
      PARAMETER (DIGITS='0123456789-_?')
      INTEGER K
C
      WORD = ' '
      L = 0
      IF (I.GT.LEN(S)) RETURN
      IF (INDEX(ALPHAS,S(I:I)).EQ.0) THEN
          IF (S(I:I).EQ.'/') THEN
              I = I+1
              WORD = '/'
              L = 1
          END IF
          RETURN
      END IF
      DO 10 K=I+1,LEN(S)
          IF (INDEX(ALPHAS//DIGITS,S(K:K)).EQ.0) THEN
              WORD = S(I:K-1)
              L = K-I
              I = K
              RETURN
          END IF
   10 CONTINUE
      WORD = S(I:LEN(S))
      L = LEN(S)-I+1
      I = LEN(S)+1
C
      END
