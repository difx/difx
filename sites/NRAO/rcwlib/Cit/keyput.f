C*KEYPUT -- output routine for KEYIN
C+
      SUBROUTINE KEYPUT(TEXT,P,LB)
      CHARACTER*(*) TEXT
      INTEGER       P, LB
C
C Write TEXT to unit P, filling lines if possible.
C Jan. 19, 2010 RCW.  Making longer outputs possible, but folding
C arrays before the buffer is full.
C-----------------------------------------------------------------------
C      CHARACTER*78  BUFFER
      CHARACTER*255  BUFFER
      SAVE BUFFER
C
      IF (TEXT.EQ.'***END') THEN
            IF (LB.GT.0) WRITE (P,'(2X,A)') BUFFER(1:LB)
            LB = 0
            RETURN
      END IF
C      IF (LB+LEN(TEXT).GT.LEN(BUFFER)) THEN
      IF (LB+LEN(TEXT).GT.78) THEN
            WRITE (P,'(2X,A)') BUFFER(1:LB)
            LB = 0
      END IF
      BUFFER(LB+1:) = TEXT
      LB = LB+LEN(TEXT)
      RETURN
      END
