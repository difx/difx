      SUBROUTINE HIWT( IB )
C
C     Routine for PLOTAPD that attempts to get a good delay offset
C     for each baseline.
C     It calculates a weight for each point based on adding a weight
C     for every other point based on a gaussian of width DELSIG (ns)
C     for the delay and RATSIG (mHz) for the rate.
C     The idea is to find the point with the most near neighbors in
C     both delay and rate.
C     The width should be near the noise.  The idea is to not penalize
C     points that are within the noise of each other.
C
      INCLUDE 'plotapd.inc'
C
      INTEGER   IB, JB, IT, JT, ICH, IDC, ND, LEN1
      INTEGER   ITMAX, NMAX, NACC, HR, HRF, MIN, SEC
      CHARACTER INAVG*1
      REAL      ACCWT, MAXWT, DIFFD, DIFFR
      REAL      DELSIG, DELSIG2, RATSIG, RATSIG2
      REAL      ACCDEL, ACCRAT, WD, WR
      PARAMETER  (DELSIG=5.0)
      PARAMETER  (RATSIG=2.0)
      PARAMETER  (DELSIG2=DELSIG**2)
      PARAMETER  (RATSIG2=RATSIG**2)
      INTEGER   IOUT
      PARAMETER  (IOUT=10)
      LOGICAL   OPENED
      DATA  OPENED  / .FALSE. /
      SAVE      OPENED
C ----------------------------------------------------------------------
C     Open the output text file
C
      IF( .NOT. OPENED ) THEN
         OPEN( UNIT=IOUT, FILE='apd.delays' )
         OPENED = .TRUE.
      END IF
C
C     Get baseline within arrays.  This routine is called inside
C     a loop of IB = IP1, IP2.
C
      JB = IB - IP1 + 1

      IF( NTIME(JB) .GT. 0 ) THEN
C
C        Write some information and a table header.
C
         WRITE(IOUT,'( 1X, /, 1X )')
         WRITE(IOUT,*) 'Delay and Rate results for the point '//
     1       'with the most close neighbors in value.'
         WRITE(IOUT,*) 'The closer the Quality is to the Npts, '//
     1       'the better the data set.'
         WRITE(IOUT,'( 1X )')
         WRITE(IOUT,*) 'Stations Chan  Npts     Quality '//
     1       '    Source          Time     Delay    Rate  In'
         WRITE(IOUT,*) '                                '//
     1       '                             (ns)    (mHz) Avg'
C
C        Loop through the channels, getting a delay and rate
C        for each channel.
C
         WD = 0.0
         WR = 0.0
         ND = 0
         ACCDEL = 0.0
         ACCRAT = 0.0
C
         DO ICH = 1, NCHAN
C
C           Loop through data points.
C
            MAXWT = 0.0
            NMAX = 0
            ITMAX = 0
            DO IT = 1, NTIME(JB)
C
C              For each point, loop through all the others 
C              accumulating the weights.  
C
               ACCWT = 0.0
               NACC = 0
               DO JT = 1, NTIME(JB)
                  IF( AMP(ICH,IT,JB) .GT. 0.0 .AND. 
     1                AMP(ICH,JT,JB) .GT. 0.0 .AND.
     2                IT .NE. JT ) THEN
                     DIFFD = DELAY(ICH,IT,JB) - DELAY(ICH,JT,JB)
                     DIFFR = RATE(ICH,IT,JB) - RATE(ICH,JT,JB)
                     ACCWT = ACCWT + 1.0 / (
     1                   EXP( DIFFD**2 / ( 2.0 * DELSIG2 ) ) +
     2                   EXP( DIFFR**2 / ( 2.0 * RATSIG2 ) ) )
                     NACC = NACC + 1
                  END IF
               END DO
C
C              Keep a record of the highest weight point.
C
               IF( ACCWT .GT. MAXWT ) THEN
                  MAXWT = ACCWT
                  NMAX = NACC
                  ITMAX = IT
               END IF
C
C              Debug stuff, commented out.
C               IF( jb .eq. 1 .and. ich .eq. 1 ) then
C                  write(*,*) ich,it,jb,delay(ich,it,jb),
C     1                      rate(ich,it,jb),
C     2                      accwt,maxwt,nmax,itmax
C              end if
C
            END DO
            IF( ITMAX .GT. 0 ) THEN
               HRF = TIME(ITMAX,JB) / 3600.0
               HR  = MOD( HRF, 24 )
               MIN = ( TIME(ITMAX,JB) - HRF * 3600.0 ) / 60.0
               SEC = TIME(ITMAX,JB) - HRF *3600.0 - MIN * 60.0 
C
C              Detect channel selected for plot.
C
               INAVG = ' ' 
               DO IDC = 1, NDOCH
                  IF( DOCH(IDC) .EQ. ICH ) INAVG = '*'
               END DO
C
C              Write channel info.
C
               WRITE(IOUT,'( 1X, A4, 1X, A4, I3, I6, F12.3,'//
     1            '4X, A12, I5, I3, I3, F9.1, F8.2, 2X, A1 )' ) 
     2            NAMEA(IB), NAMEB(IB), ICH, NMAX, MAXWT,
     3            SOURCE(ITMAX,JB), HR, MIN, SEC, 
     4            DELAY(ICH,ITMAX,JB), RATE(ICH,ITMAX,JB), INAVG
C
C              Accumulate for the averages.  Only accumulate
C              channels that were selected for plotting.  For
C              example, this allows selection of X band in SX
C              observations where the S band channels will have
C              high ionospheric delays.
C
               IF( INAVG .EQ. '*' ) THEN
                  ND = ND + 1
                  WD = WD + MAXWT
                  ACCDEL = ACCDEL + DELAY(ICH,ITMAX,JB) * MAXWT
                  WR = WR + MAXWT
                  ACCRAT = ACCRAT + RATE(ICH,ITMAX,JB) * MAXWT
               END IF
            ELSE
               WRITE(IOUT,'( 1X, A4, 1X, A4, I3, 4X, A )' )
     2            NAMEA(IB), NAMEB(IB), ICH, 'No result'
            END IF
         END DO
C
C        Write the averages.
C
         IF( WD .GT. 0.0 .AND. WR .GT. 0.0 ) THEN
            WRITE(IOUT,'( A, I2, 4A, T59, F9.1, F8.2 )' ) 
     1       'Weighted Averages of ', ND, ' channels for ',
     2       NAMEA(IB)(1:LEN1(NAMEA(IB))), ' - ', 
     3       NAMEB(IB)(1:LEN1(NAMEB(IB))),
     4       ACCDEL / WD, ACCRAT / WR
         END IF
      END IF
C
      RETURN
      END




