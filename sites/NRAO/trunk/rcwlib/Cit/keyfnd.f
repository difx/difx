C*KEYFND -- minimum match keyword search (KEYIN)
C+
      SUBROUTINE KEYFND (KEYWRD, NAMES, NNAMES, IPOS, IER)
      INTEGER NNAMES, IPOS, IER
      CHARACTER*(*) KEYWRD, NAMES(*)
C
C KEYFND: this routine returns the sequence number of the supplied
C keyword in the list of allowed keywords. It is case-insensitive,
C and allows use of '?' as a "wild character" in the keyword and list.
C It attempts a minimum match search, and returns an error if the
C tested keyword is ambiguous.
C
C Arguments:
C  KEYWRD (input)  : the name of the keyword to be found.
C  IPOS   (output) : index position of the keyword in the list, or 0
C                    if the keyword was not found. If the keyword is
C                    ambiguous, IPOS is the first element that matches.
C  IER    (output) : status code:
C                    0 = found
C                    21= ambiguous
C                    4 = not found
C
C History:
C   8-Oct-1985  New routine (T. J. Pearson).
C-----------------------------------------------------------------------
      INTEGER I, J, TEST
      INTEGER KEYCMP
C
      DO 20 I=1,NNAMES
          TEST = KEYCMP(KEYWRD,NAMES(I))
          IF (TEST.EQ.2) THEN
              IPOS = I
              IER = 0
              RETURN
          END IF
   20 CONTINUE
      DO 30 I=1,NNAMES
          TEST = KEYCMP(KEYWRD,NAMES(I))
          IF (TEST.NE.0) THEN
              IPOS = I
              DO 10 J=I+1,NNAMES
                  IF (KEYCMP(KEYWRD,NAMES(J)).NE.0) THEN
                      IER = 21
                      RETURN
                  END IF
   10         CONTINUE
              IER = 0
              RETURN
          END IF
   30 CONTINUE
      IPOS = 0
      IER = 4
      END
