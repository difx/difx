C*DWCASE -- convert character string to lower case
C+
      SUBROUTINE DWCASE (SRC)
      CHARACTER*(*) SRC
C
C Convert character string to lower case; all upper case letters
C (A-Z) are converted to upper case equivalents (a-z); other characters
C are unchanged. This version assumes the ASCII character set.
C
C Arguments:
C  SRC    (in/out) : input string to be converted.
C
C History:
C  1987 nov 12 - TJP.  UPCASE
C  1991 apr 1  - RCW.  Adapted for lower case.
C-----------------------------------------------------------------------
      INTEGER I, K
C
      DO 10 I=1,LEN(SRC)
          K = ICHAR(SRC(I:I))
          IF (K.GE.ICHAR('A') .AND. K.LE.ICHAR('Z'))
     1         SRC(I:I) = CHAR(K+32)
   10 CONTINUE
C
      END

