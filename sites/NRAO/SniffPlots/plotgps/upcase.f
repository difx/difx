C*UPCASE -- convert character string to upper case
C+
      SUBROUTINE UPCASE (SRC)
      CHARACTER*(*) SRC
C
C Convert character string to upper case; all lower case letters
C (a-z) are converted to upper case equivalents (A-Z); other characters
C are unchanged. This version assumes the ASCII character set.
C
C Arguments:
C  SRC    (in/out) : input string to be converted.
C
C History:
C  1987 nov 12 - TJP.
C-----------------------------------------------------------------------
      INTEGER I, K
C
      DO 10 I=1,LEN(SRC)
          K = ICHAR(SRC(I:I))
          IF (K.GE.ICHAR('a') .AND. K.LE.ICHAR('z'))
     1         SRC(I:I) = CHAR(K-32)
   10 CONTINUE
C
      END
