C*SYMSUB -- DCL symbol substitution [UNIX]
C+
	SUBROUTINE SYMSUB (A,B,L,N)
	CHARACTER*(*) A,B
        INTEGER L,N
C
C SYMSUB: perform DCL symbol substitution in a string. For UNIX, this
C routine just copies input to output, without making any changes.
C
C Arguments:
C  Input:  CHARACTER*(*) A	Input string.
C  Output: CHARACTER*(*) B	Output string; may be same as A.
C  Output: INTEGER L		The number of characters in B.
C  Output: INTEGER N		The number of substitutions performed.
C
C History:
C  Version 1.0:  1983 Dec 5	T.J. Pearson
C  Version 1.1:  1984 Jan 2	TJP; change from &XX to {XX}
C  Version 1.2:  1991 Feb 14    TJP: Unix version.
C-----------------------------------------------------------------------
      INTEGER I
C
      L = MIN(LEN(A),LEN(B))
      DO 10 I=1,L
          B(I:I) = A(I:I)
   10 CONTINUE
      N = 0
      END
