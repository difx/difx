      SUBROUTINE REFSET
C
C     Routine to adjust to a common reference antenna.
C
      INCLUDE 'clock.inc'
C
      INTEGER  IEXP, JB, IRB
      REAL     RDELAY
C ----------------------------------------------------------------
C
C     Loop over experiments.  Only do anything if the reference station
C     has to change.
C
      DO IEXP = 1, NEXP
         NREF(IEXP) = NBAS(IEXP)
         IF( NBAS(IEXP) .GE. 1 .AND. ISTA1(1,IEXP) .NE. IREF ) THEN
C
C           Find reference baseline.
C
            IRB = 0
            DO JB = 1, NBAS(IEXP)
               IF( IREF .EQ. ISTA2(JB,IEXP) ) THEN
                  IRB = JB
                  GO TO 100
               END IF
            END DO
  100       CONTINUE
C
C           Reference (or skip if ref station not there)
C
            IF( IRB .EQ. 0 ) THEN
               NREF(IEXP) = 0
               WRITE(*,*) ' Reference station not present for ',
     1            DATAFILE(IEXP)
            ELSE
C
C              Actually adjust the stored data.  I'm not
C              totally sure I like doing this.
C
               RDELAY = DELAY(IRB,IEXP)
               DO JB = 1, NREF(IEXP)
                  IF( JB .EQ. IRB ) THEN
                     DELAY(JB,IEXP) = -1. * RDELAY
                     ISTA2(JB,IEXP) = ISTA1(JB,IEXP)
                  ELSE
                     DELAY(JB,IEXP) = DELAY(JB,IEXP) - RDELAY
                  END IF
                  ISTA1(JB,IEXP) = IREF
               END DO
C
            END IF
         END IF
      END DO
C
      RETURN
      END
