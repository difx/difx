      INTEGER FUNCTION NCHAR (TEXT)
C-----------------------------------------------------------------------
C     A.J. Hunt  -  August, 1981.
C
C  CHARACTER*n TEXT
C   .
C   . .
C   .
C  NCH = NCHAR (TEXT)
C
C     NCHAR is an integer function subroutine which returns the number of
C     significant characters in a character string.  Trailing blanks are
C     regarded as insignificant.  This differs from the VAX/FORTRAN77
C     intrinsic function LEN which returns the defined or passed length
C     of the character string including trailing blanks.
C-----------------------------------------------------------------------
      CHARACTER*(*) TEXT
      INTEGER NN
C-----------------------------------------------------------------------
      NN = LEN (TEXT)

      DO WHILE (NN .GT. 0  .AND.  TEXT(NN:NN) .EQ. ' ')
         NN = NN - 1
      END DO

      NCHAR = NN
      RETURN
      END
