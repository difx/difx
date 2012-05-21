C*KPACK  -- write characters into numeric array (KEYIN)
C+
      SUBROUTINE KPACK(S, A)
      CHARACTER*(*) S
      BYTE A(*)
C
C KEYIN: write data from character string into array
C-----------------------------------------------------------------------
      INTEGER I
      DO 10 I=1,LEN(S)
          A(I) = ICHAR(S(I:I))
   10 CONTINUE
      END
