C*KEYCMP -- compare two strings (KEYIN)
C+
      INTEGER FUNCTION KEYCMP (S1, S2)
      CHARACTER*(*) S1, S2
C
C KEYCMP determines whether the test string, S1, matches the
C comparison string, S2, according to the following rules.
C (1) If S1 is longer than S2, the additional characters in S1 are
C ignored; if S2 is longer than S1, the additional characters in S2
C are ignored.
C (2) A lower-case letter matches the corresponding upper or lower
C case letter. 
C (3) A question mark ('?') matches any character.
C
C Returns:
C  KEYCMP          : 2 for an exact match
C                    1 for an abbreviated match
C                    0 for no match
C
C Arguments:
C  S1     (input)  : test string.
C  S2     (input)  : comparison string.
C
C History:
C   8-Oct-1985  New routine (T. J. Pearson).
C-----------------------------------------------------------------------
      INTEGER I, L1, L2
      CHARACTER*1 T1, T2
      INTEGER LEN1
C
      L1 = LEN1(S1)
      L2 = LEN1(S2)
      IF (L1.LE.L2) THEN
        DO 10 I=1,L1
          T1 = S1(I:I)
          T2 = S2(I:I)
          IF (('a').LE.T1 .AND. T1.LE.('z')) T1 = CHAR(ICHAR(T1)-32)
          IF (('a').LE.T2 .AND. T2.LE.('z')) T2 = CHAR(ICHAR(T2)-32)
          IF ((T1.EQ.T2) .OR. (T1.EQ.'?') .OR. (T2.EQ.'?')) GOTO 10
          KEYCMP = 0
          RETURN
   10   CONTINUE
        KEYCMP = 1
        IF (L1.EQ.L2) KEYCMP = 2
      ELSE
        KEYCMP = 0
      END IF
      END
