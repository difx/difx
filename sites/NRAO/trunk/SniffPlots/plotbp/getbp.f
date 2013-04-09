      SUBROUTINE GETBP
C
C     Routine to read and plot the data from John Benson's bandpass files.
C
C     Change NCHAN to be for the current spectrum rather than using the 
C     biggest spectrum seen so far as it was up to March 26, 2013.  The
C     previous style resulted in some spectra all compressed against the
C     side of the plot.
C
      INCLUDE 'plotbp.inc'
C
      INTEGER    ICH, IER, INCHAN, LINCHAN, NAVG
      REAL       ALIM(MCHAN)
      DOUBLE PRECISION  GETNUM
      LOGICAL    GOTDAT, FIRST, WARNLONG
C -------------------------------------------------------------------
      GOTDAT = .FALSE.
      FIRST  = .TRUE.
      WARNLONG = .TRUE.
C
C     Read the data.  Jump back here after reading a header
C
 100  CONTINUE
         NWORDS = MWORDS
         CALL RDLINE( WORD, WLEN, NWORDS, INUNIT, 6 )
C
C        If the spectrum is finished, plot it before starting
C        to parse the next one.  Detect a finished spectrum
C        by detecting that the channel number decreased or the
C        last record was not a data record (will give INCHAN=0).
C
         INCHAN = 0
         IF( WORD(1) .EQ. 'timerange:' .OR.
     1       WORD(1) .EQ. 'source:' .OR.
     2       WORD(1) .EQ. 'bandfreq:' ) THEN
C
C           Header record.  Deal with later after plotting.
C
         ELSE IF( NWORDS .EQ. 4 ) THEN
            INCHAN = GETNUM(WORD(3), 1, WLEN(3) )
         ELSE IF( NWORDS .GE. 7 ) THEN
            INCHAN = GETNUM(WORD(5), 1, WLEN(5) )
         END IF
C
C        Protect against spectra of excessive length.
C        Dump any extra points into the last point and warn the
C        first time.
C
         IF( INCHAN .GT. MCHAN ) THEN
            INCHAN = MCHAN
            IF( WARNLONG ) THEN
               WRITE(*,*) ' **** WARNING ***** ',
     1           '  Number of IFs times number of channels too large.'
               WRITE(*,*) '                    ',
     1           '  Not all channels will be plotted'
               WARNLONG = .FALSE.
            END IF
         END IF
C
C        Detect that the previous spectrum is finished and needs
C        to be plotted.  INCHAN is likely to be 0 if hit a header.
C
         IF( GOTDAT .AND. INCHAN .LT. LINCHAN ) THEN
C ------------------------------------------------------------------
C           Special addition for pcal tests.
C
C            IF( EXPNAM(1:5) .EQ. 'BB014' ) CALL PCXFIX
C ------------------------------------------------------------------
C
C           Plot the spectrum if this is a desired station.
C
            IF( DOSTA .EQ. ' ' .OR. ( DOSTA .EQ. NAME1 .OR.
     1          DOSTA .EQ. NAME2 ))  THEN
C
C              Get the average and the plot limit for a possible 
C              expanded plot.
C    
               AMPAVG = AMPAVG / NAVG
               ALMAX = MIN( AMAX, AMPAVG * 3.0 )
               DO ICH = 1, NCHAN
                  ALIM(ICH) = MIN( AMP(ICH), ALMAX )
               END DO
               CALL PLTBP( ALIM )
            END IF
C
C           Accumulate the summary information.
C
            CALL ACCSUM
C
            GOTDAT = .FALSE.
         END IF
C
C        Quit if out of data.
C
         IF( NWORDS .EQ. -1 ) GO TO 200
C
C        Extract header information if it is found.  The call to 
C        GETHDR is here so the plots from the previous scan
C        will not have the wrong header info.  Ie, this should come
C        after the call to PLTBP.
C
         IF( WORD(1) .EQ. 'timerange:' ) THEN 
            CALL GETHDR( IER )
            IF( IER .NE. 0 ) GO TO 200
            GO TO 100
         END IF
C
C        Initialize the arrays.
C
         IF( FIRST .OR. INCHAN .LT. LINCHAN ) THEN
            DO ICH = 1, MCHAN
               AMP(ICH) = 0.0
               ALIM(ICH) = 0.0
               PHASE(ICH) = 0.0
            END DO
            AMPAVG = 0.0
            NAVG = 0
            NCHAN = 0
C
C           Initialize plot limits
C
            AMAX = -1.E9
            PMIN = 200.
            PMAX = -200.
         END IF
         LINCHAN = INCHAN
C
C        Detect whether AC or XC.  If number of words wrong, 
C        skip record.
C
         IF( NWORDS .EQ. 4 ) THEN
            TYPE = 'AC'
            NAME1 = WORD(2)
            AMP(INCHAN)  = GETNUM(WORD(4), 1, WLEN(4) )
            AMPAVG = AMPAVG + AMP(INCHAN) 
            NAVG = NAVG + 1
         ELSE IF( NWORDS .EQ. 8 .OR. NWORDS .EQ. 7 ) THEN
            TYPE = 'XC'
            NAME1 = WORD(3)
            NAME2 = WORD(4)
            AMP(INCHAN)  = GETNUM(WORD(6), 1, WLEN(6) ) * 1000.0
            PHASE(INCHAN) = GETNUM(WORD(7), 1, WLEN(7) )
         ELSE
            WRITE(*,*)  ' Skipping unrecognized record. ', NWORDS
            GO TO 100
         END IF
C
         GOTDAT = .TRUE.
         FIRST = .FALSE.
C
C        Get plot limits.
C
         NCHAN = MAX( NCHAN, INCHAN )
         AMAX = MAX( AMAX, AMP(INCHAN) )
         PMIN = MIN( PMIN, PHASE(INCHAN) )
         PMAX = MAX( PMAX, PHASE(INCHAN) )
C
C        Return for next point.
C
         GO TO 100
C
 200  CONTINUE
C
      RETURN
      END
