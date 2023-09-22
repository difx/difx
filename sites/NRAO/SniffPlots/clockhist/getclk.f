      SUBROUTINE GETCLK
C
C     Routine to read John Benson's output amplitude, phase,
C     and delay file and extract the delay for each baseline
C     corresponding to the maximum amplitude.
C
      INCLUDE    'clock.inc'
C
C     Input data
C
      REAL       AMP
      INTEGER    IAMP0, INCR, IAMP, IDEL, NSKIP
      CHARACTER  INST1*2, INST2*2
      INTEGER    NCHAN, I, ST1, ST2, JB
      LOGICAL    FIRST
      CHARACTER  FORMAT*3, SRCNAM*12, INLINE*512
C
C     Functions
C
      INTEGER    WHICH, WHICH2, LEN1
      DOUBLE PRECISION  GETNUM
C -----------------------------------------------------------------
C     Initialize various things.  
C
      DO JB = 1, MAXBAS
         MAXAMP(JB,NEXP) = -1.E4
      END DO
      NBAS(NEXP) = 0.0
      FIRST = .TRUE.
      NSKIP = 0
      IAMP0 = 7
      INCR  = 4
C
C     Space past first line.
C
      READ( 9, '(A)' ) WORD(1)    
C
C     Now read the data in a loop.  Only read new format.
C
400   CONTINUE
C      
C        Read the next line of data.
C
         NWORDS = MWORDS
         CALL GETLINE( WORD, WLEN, NWORDS, INLINE, 9, 6 )
         IF( NWORDS .EQ. -1 ) GO TO 900
C
C        Assume the new format.  Just complain if the format is wrong.
C
         IF( FIRST ) THEN
            IF( WHICH( WORD(5)(1:WLEN(5)), NAMES, MAXBAS ) .NE. 0 ) THEN
               FORMAT = 'OLD'        
               WRITE(*,*) ' Old datafile.lis format in file: ', 
     1              DATAFILE(NEXP)
               GO TO 900
            END IF
            FIRST = .FALSE.
         END IF
C
C        Now get data for this line.  First get format dependent
C        stuff.  Ignore lines with unrecognized stations.
C
         ST1 = WHICH( WORD(7), NAMES, MAXSTA )
         ST2 = WHICH( WORD(8), NAMES, MAXSTA )
         INST1 = WORD(7)
         INST2 = WORD(8)
         IF( ST1 .GT. 0 .AND. ST2 .GT. 0 ) THEN
            NCHAN = GETNUM( WORD(9), 1, WLEN(9) )
            SRCNAM = WORD(4)
C
C           Get which baseline this is.
C
            JB = WHICH2( ST1, ST2, ISTA1(1,NEXP), ISTA2(1,NEXP), 
     1             NBAS(NEXP) )
            IF( JB .EQ. 0 .AND. NBAS(NEXP) .LT. MAXBAS ) THEN
               JB = NBAS(NEXP) + 1
               NBAS(NEXP) = JB
               ISTA1(JB,NEXP) = ST1
               ISTA2(JB,NEXP) = ST2
            ELSE IF( JB .EQ. 0 .AND. NBAS(NEXP) .EQ. MAXBAS ) THEN
               WRITE(*,*) ' Too many baselines in ', DATAFILE(NEXP)
               GO TO 400
            END IF
         ELSE
            IF( NSKIP .LE. 10 ) THEN
               WRITE(*,*) ' Skipped ', INST1, ' ', INST2, 
     1            DATAFILE(NEXP)(1:LEN1(DATAFILE(NEXP)))
               NSKIP = NSKIP + 1
            END IF
            GO TO 400
         END IF
C
C        Now get the data
C
         DO I = 1, NCHAN  
            IAMP = IAMP0 + I * INCR
            AMP = GETNUM( WORD(IAMP), 1, WLEN(IAMP) )
            IF( AMP .GT. MAXAMP(JB,NEXP) ) THEN
               MAXAMP(JB,NEXP) = AMP
               SOURCE(JB,NEXP) = SRCNAM
               IDEL = IAMP - 1
               DELAY(JB,NEXP) = GETNUM( WORD(IAMP-1), 1, WLEN(IAMP-1) )
               RATE(JB,NEXP) = GETNUM(WORD(IAMP+2),1,WLEN(IAMP+2)) *
     1                 1000.0                                  
C
C              Get time of this data point in JD-50000
C
               TIME(JB,NEXP) = GETNUM( WORD(1), 1, WLEN(1) ) - 50000 +
     1              GETNUM( WORD(2), 1, WLEN(2) ) / 24.000               
            END IF        
         END DO
C
C        Get next data point.
C
         GO TO 400
C
C     Done.
C
  900 CONTINUE 
      RETURN
      END

