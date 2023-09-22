      SUBROUTINE GETAPD
C
C     Routine to read John Benson's output amplitude, phase,
C     and delay file and fill the arrays needed by PLTAPD
C     It will read up to MBAS baselines each pass - using the
C     baselines between IP1 and IP2.  On the first pass, it
C     determines some information about all baselines.
C
C     Data will be read into the data arrays in the channel slot
C     matching the channel number in the data set.
C
      INCLUDE    'plotapd.inc'
C
C     Input data
C
      INTEGER    JDAY
      REAL       RTIME
C
C     Program data.
C
      INTEGER    I, J, IT, IB, JB, NB, ICH, ISTA1, ISTA2, BASNUM
      INTEGER    NLINES, NWCHAN, NDCHAN, ND
      REAL       DAVG, DMINR, RAVG, RMINR
      LOGICAL    KEEP, FULL(MBAS)
      CHARACTER  ANTA*8, ANTB*8, BSTA(2,MTBAS)*8
      PARAMETER  (DMINR=50.0)    ! Minimum delay range.
      PARAMETER  (RMINR=15.0)    ! Minimum rate range.
      DOUBLE PRECISION  GETNUM
      SAVE BSTA
C -----------------------------------------------------------------
C     Initialize various things.  
C
      IF( NBAS .EQ. 0 ) THEN
         NCHAN = 0
         JDAY1 = 0
      END IF
      NLINES = 0
      DO JB = 1, MBAS
         NTIME(JB) = 0
         FULL(JB) = .FALSE.
      END DO
C
C     Now read the data in a loop
C     Assume that the first record is at the earliest time.
C     If not, I may need to make two passes through the data.
C     Allow for concatenated data sets by ignoring extra headers,
C     which have 2 words on the line.
C
400   CONTINUE
C      
C        Read the next line of data.
C
         NWORDS = MWORDS
         CALL RDLINE( WORD, WLEN, NWORDS, INUNIT, 6 )
         IF( NWORDS .EQ. -1 ) GO TO 900
         NLINES = NLINES + 1
         IF( NWORDS .EQ. 2 ) GO TO 400
C
C        Ignore lines from non-requested baselines.
C
         ANTA = WORD(7)
         ANTB = WORD(8)
         KEEP = .FALSE.
         DO I = 1, NANT
            IF( DOANT(I) .EQ. 'ALL' .OR. DOANT(I) .EQ. ANTA .OR.
     1          DOANT(I) .EQ. ANTB ) KEEP = .TRUE.
         END DO
         IF( .NOT. KEEP ) GO TO 400
C
         NWCHAN = ( NWORDS - 6 ) / 4
C
C        If NDOCH is set to the maximum possible (usually with a
C        request to do all channels), then set it to the number
C        of channels in the data.  Allow for changes in that number
C        during the observation.
C
         IF( NDOCH .EQ. MCHAN .OR. NWCHAN .GT. NDOCH ) THEN
            NDOCH = NWCHAN
         END IF
C
C        Check for a bad data line.
C
         IF( NWCHAN .LE. 0 .OR. NWCHAN .GT. MCHAN ) THEN
            WRITE(*,*) ' Bad number of channels (', NWCHAN, 
     1      ' on line ', NLINES
            GO TO 400
         END IF
C
C        Decode the data.  Time in seconds.
C
         JDAY   = GETNUM( WORD(1), 1, WLEN(1) )
         RTIME  = GETNUM( WORD(2), 1, WLEN(2) ) * 3600.0
         ISTA1  = GETNUM( WORD(5), 1, WLEN(5) )
         ISTA2  = GETNUM( WORD(6), 1, WLEN(6) )
         NDCHAN = GETNUM( WORD(9), 1, WLEN(9) )
C
C        Get baseline number.  Note that NBAS is set in BASNUM.
C
         IB = BASNUM( ANTA, ANTB, BSTA, NBAS, MTBAS )
C
C        Get first Julian day.  Also initialize TMIN and TMAX based 
C        on RTIME.  This is done before JDAY1 is subtracted, but 
C        that is ok because JDAY - JDAY1 will be zero on the first 
C        data point.
C
         IF( JDAY1 .EQ. 0 ) THEN
            JDAY1 = JDAY
            TMIN = RTIME
            TMAX = RTIME
         END IF
C
C        Get the time and the limits for the time axis of the plots.
C        Note that we are still looking at all baselines at this point.
C
         RTIME = (JDAY - JDAY1) * 24.0 * 3600.0 + RTIME
         TMAX = MAX( TMAX, RTIME )
         TMIN = MIN( TMIN, RTIME )
C
C        Get station names.
C
         NAMEA(IB) = ANTA
         NAMEB(IB) = ANTB
C
C        In first pass, increase IP2 as new baselines are read.
C
         IF( IP1 .EQ. 1 ) IP2 = MIN( MBAS, NBAS )
C
C        Check number of channels and get the maximum number.
C
         IF( NWCHAN .LT. 1 .OR. NWCHAN .GT. MCHAN ) THEN
            WRITE(*,*)  ' Bad number of channels (', NWCHAN, 
     1           ') in data point at ', JDAY, RTIME
            IF( NWCHAN .GT. MCHAN ) CALL ERROR()
         ELSE IF( NWCHAN .NE. NDCHAN ) THEN
            WRITE(*,*)  ' Number of data points does not match'//
     1          ' claimed number of channels (Day, sec, ndat, nch) ',
     2            JDAY, RTIME, NWCHAN, NDCHAN
         END IF
         NCHAN = MAX( NCHAN, NWCHAN )
C
C        Get the actual data if the baseline and time are within the 
C        desired range.
C        Get amps in "correlation units" of 10**5 times corr. coeff. 
C        Get rates in mHz.
C
         IF( IB .GE. IP1 .AND. IB .LE. IP2 .AND.
     1       RTIME .GE. TMINA .AND. RTIME .LT. TMAXA ) THEN
            JB = IB - IP1 + 1
            IF( JB .LT. 1 .OR. JB .GT. MBAS ) THEN
                WRITE(*,*) 'PLOTAPD - GETAPD: Programming problem JB',
     1                ' - tell Craig.', JB, MBAS
                STOP
            END IF
            IF( .NOT. FULL(JB) ) THEN
               NTIME(JB) = NTIME(JB) + 1
               IF( NTIME(JB) .GT. MTIME ) THEN
                  WRITE(*,*) ' Too many data points - will not use all'
                  WRITE(*,*) '   Baseline ', NAMEA(IB), ' ', NAMEB(IB)
                  NTIME(JB) = NTIME(JB) - 1
                  FULL(JB) = .TRUE.
               ELSE
                  IT = NTIME(JB)
                  TIME(IT,JB) = RTIME
                  DO I = 1, NWCHAN
                     ND = 4 * I + 6
                     DELAY(I,IT,JB) = GETNUM(WORD(ND),1,WLEN(ND))
                     AMP(I,IT,JB)   = GETNUM(WORD(ND+1),1,WLEN(ND+1)) *
     1                                1.0E5
                     PHASE(I,IT,JB) = GETNUM(WORD(ND+2),1,WLEN(ND+2))
                     RATE(I,IT,JB)  = GETNUM(WORD(ND+3),1,WLEN(ND+3)) *
     1                                1.0E3                      
                     SOURCE(IT,JB) = WORD(4)(1:WLEN(4))
                  END DO
               END IF
            END IF
         END IF
C
C        Return for next record.
C
         GO TO 400
C
 900  CONTINUE
C
C     Get plot scale limits.
C
      IF( TMINA .NE. 0.0 ) TMIN = TMINA
      IF( TMAXA .NE. 1.E10 ) TMAX = TMAXA
      NB = IP2 - IP1 + 1
      IF( NB .LT. 1 .OR. NB .GT. MBAS ) THEN
          WRITE(*,*) 'PLOTAPD - GETAPD: Programming problem NB',
     1                ' - tell Craig.', NB, MBAS
          STOP
      END IF
C
C     Loop over baselines.
C
      DO JB = 1, NB
C
C        Initialize the baseline limits.
C
         AMIN(JB) = 99.E9
         AMAX(JB) = -99.E9
         PMIN(JB) = 99.E9
         PMAX(JB) = -99.E9
         DMIN(JB) = 99.E9
         DMAX(JB) = -99.E9
         RMIN(JB) = 99.E9
         RMAX(JB) = -99.E9
C
C        Get plot limits, searching through time and all 
C        requested channels
C
         DO J = 1, NDOCH
            ICH = DOCH(J)
            DO IT = 1, NTIME(JB)
               IF( AMP(ICH,IT,JB) .GT. 0.0 ) THEN
                  AMIN(JB) = MIN( AMIN(JB), AMP(ICH,IT,JB) )
                  AMAX(JB) = MAX( AMAX(JB), AMP(ICH,IT,JB) )
                  PMIN(JB) = MIN( PMIN(JB), PHASE(ICH,IT,JB) )
                  PMAX(JB) = MAX( PMAX(JB), PHASE(ICH,IT,JB) )
                  DMIN(JB) = MIN( DMIN(JB), DELAY(ICH,IT,JB) )
                  DMAX(JB) = MAX( DMAX(JB), DELAY(ICH,IT,JB) )
                  RMIN(JB) = MIN( RMIN(JB), RATE(ICH,IT,JB) )
                  RMAX(JB) = MAX( RMAX(JB), RATE(ICH,IT,JB) )
               END IF
            END DO
         END DO
      END DO
C
C     Expand the limits to give some space at the edges of the plots.
C     Set up for amplitude plots that go from zero to max.
C     Plot phase on fixed scale in any case where there isn't data out
C     of range.
C
      CALL FIXLIM( TMIN, TMAX, 0.04, 0.04 )
      DO JB = 1, NB
         AMIN(JB) = 0.0
         CALL FIXLIM( AMIN(JB), AMAX(JB), 0.0, 0.1 )
C         CALL FIXLIM( PMIN(JB), PMAX(JB), 0.1, 0.1 )
         PMIN(JB) = MIN( PMIN(JB), -185.0 )
         PMAX(JB) = MAX( PMAX(JB),  185.0 )
         CALL FIXLIM( DMIN(JB), DMAX(JB), 0.1, 0.1 )
         CALL FIXLIM( RMIN(JB), RMAX(JB), 0.1, 0.1 )
C
C        Force a moderate delay and rate range.
C
         IF( DMAX(JB) - DMIN(JB) .LT. DMINR ) THEN
            DAVG = ( DMAX(JB) + DMIN(JB) ) / 2.0
            DMIN(JB) = DAVG - DMINR / 2.0
            DMAX(JB) = DAVG + DMINR / 2.0
         END IF
         IF( RMAX(JB) - RMIN(JB) .LT. RMINR ) THEN
            RAVG = ( RMAX(JB) + RMIN(JB) ) / 2.0
            RMIN(JB) = RAVG - RMINR / 2.0
            RMAX(JB) = RAVG + RMINR / 2.0
         END IF
      END DO
C
C     Get some other numbers.
C
      RETURN
      END

