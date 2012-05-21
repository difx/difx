      INTEGER FUNCTION BASNUM( ANTA, ANTB, BSTA, NBAS, MTBAS )
C
C     Find the baseline number of this data point.
C     BSTA is in the call arguments just so that it can use an
C     adjustable array size.
C
      INTEGER   NBAS, MTBAS
      CHARACTER ANTA*8, ANTB*8, BSTA(2,MTBAS)*8
      INTEGER   IB
C --------------------------------------------------------------------
C     Set first baseline if necessary.
C
      IF( NBAS .EQ. 0 ) THEN
         NBAS   = 1
         BASNUM = 1
         BSTA(1,1) = ANTA
         BSTA(2,1) = ANTB
C
C     Look for this baseline among the ones read before.
C
      ELSE
         DO IB = 1, NBAS
            IF( ANTA .EQ. BSTA(1,IB) .AND. 
     1          ANTB .EQ. BSTA(2,IB) ) THEN
C
C              If found, set baseline number and skip out.
               BASNUM = IB
               GO TO 100
            END IF
         END DO
C
C        Only get here for new baseline.
C
         NBAS = NBAS + 1
         IF( NBAS .GT. MTBAS ) CALL ERROR(' Too many baselines. ')
         BASNUM = NBAS
         BSTA(1,NBAS) = ANTA
         BSTA(2,NBAS) = ANTB
      END IF
C
 100  CONTINUE
      RETURN
      END
