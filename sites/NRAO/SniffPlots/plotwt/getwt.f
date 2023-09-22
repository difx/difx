      SUBROUTINE GETWT
C
C     Routine to read John Benson's output weight file and
C     fill the arrays needed by PLOTWT.
C     This will also read PLOTWT's output summary file.  It
C     will do so in a manner very similar to reading a sniffer
C     file.  This saves duplicated code and allows integration
C     time changes.
C
      INCLUDE    'plotwt.inc'
C
C     Input data
C
      INTEGER    JDAY, ISTA
      REAL       SEC, WT(MCHAN), LOWWT(MCHAN), HIGHWT(MCHAN)
      CHARACTER  CSTA*8
C
C     Program data.
C
      INTEGER    INAVG(MTIME,MSTA), INCAVG(MCHAN,MTIME,MSTA)
      INTEGER    IT, IS, IL, IC, I, J, K, NCHAN, NSAV
      LOGICAL    FIRST, KEEP1
      REAL       TPAD, HR
      DOUBLE PRECISION  GETNUM
C -----------------------------------------------------------------
C     Open the file.
C
C
C     Initialize various things.  The min will be used
C     to detect if there was any data in an interval.
C
      FIRST = .TRUE.
      KEEP1 = .TRUE.
      NSTA = 0
      NTIME = 0
      NCHMAX = 0
      NSAV = 0
      DO IS = 1, MSTA
         DO IT = 1, MTIME
            INAVG(IT,IS) = 0
            TIME(IT,IS) = 0.0
            AVGWT(IT,IS) = 0.0
            MINWT(IT,IS) = 99.0
            MAXWT(IT,IS) = -1.0
            DO IC = 1, MCHAN
               INCAVG(IC,IT,IS) = 0
               TIMEC(IC,IT,IS) = 0.0
               AVGCWT(IC,IT,IS) = 0.0
               MINCWT(IC,IT,IS) = 99.0
               MAXCWT(IC,IT,IS) = -1.0
            END DO
         END DO
      END DO
C
C     Now read the data in a loop
C     Assume that the first record is at the earliest time.
C     If not, I may need to make two passes through the data.
C     Allow for concatenated files - don't trip over extra headers.
C
400   CONTINUE
C      
         NWORDS = MWORDS
         CALL RDLINE( WORD, WLEN, NWORDS, INUNIT, 6 )
         IF( NWORDS .EQ. -1 ) GO TO 900
         IF( NWORDS .LE. 3 ) THEN
            GO TO 400
         END IF
         JDAY = GETNUM( WORD(1), 1, WLEN(1) )
         SEC  = GETNUM( WORD(2), 1, WLEN(2) ) * 3600.D0
         ISTA = GETNUM( WORD(3), 1, WLEN(3) )
         CSTA = WORD(4)
C
C        Save the Julian Day of the first day of the observation.
C
         IF( FIRST ) THEN
            FIRST = .FALSE.
            JDAY1 = JDAY
         END IF
C
C        Add the day of experiment to the time.
C
         SEC = (JDAY - JDAY1) * 24.D0 * 3600.D0 + SEC
C
C        Select on input time range.
C
         IF( SEC .LT. TMINA ) GO TO 400
         IF( SEC .GT. TMAXA ) GO TO 900
C
C        Don't keep the record if the number of channels it 
C        claims to have doesn't match the number of works.
C
         NCHAN = GETNUM( WORD(5), 1, WLEN(5) )
         IF( ( GOTSUM .AND. NWORDS .NE. 3 * NCHAN + 5 ) .OR.
     1       ( .NOT. GOTSUM .AND. NWORDS .NE. NCHAN + 5 ) ) THEN
            WRITE(*,*) ' Number of channels and number of values '
     1        //'don''t match for: '
            WRITE(*,*) ' Day, time: ', JDAY, SEC/3600., '  at ', CSTA
            GO TO 400
         END IF
C
C        Save the first time actually kept.  Adjust the TMIN, in 
C        the default case, back by half an integration.  This helps
C        deal with cases where there are two integration times.  It
C        also avoids some averaging that can happen otherwise when
C        reading the summary file.
C
         IF( KEEP1 ) THEN
            KEEP1 = .FALSE.
            IF( TMINA .EQ. 0.0 ) THEN
               TMIN = SEC - TAVG / 2.0
            ELSE
               TMIN = TMINA
            END IF
         END IF
C
C        Pick up the station name and number to use from here.  Do not
C        use the number in the file in case concatenated files were
C        used and the numbering changed.  This station number is the
C        cell to use in the accumulation arrays.  The number in a 
C        summary file will always be zero.
C
         IF( NSTA .EQ. 0 ) THEN
            NSTA = 1
            IS = 1
            STNAME(NSTA) = CSTA
         ELSE
            DO IL = 1, NSTA
               IF( CSTA .EQ. STNAME(IL) ) THEN
                  IS = IL
                  GO TO 600
               END IF
            END DO
            NSTA = NSTA + 1
            IS = NSTA
            STNAME(NSTA) = CSTA
         END IF
  600    CONTINUE
         IF( IS .GT. MSTA ) THEN
            WRITE(*,*) ' Too many stations ', IS, ' max is ', MSTA
            GO TO 900
         END IF
C
C        Get the time cell.
C
         IT = ( SEC - TMIN ) / TAVG + 1
         IF( IT .GT. MTIME) THEN
            WRITE(*,*) ' Maximum of ', MTIME, 
     1        ' output data points per antenna exceeded.'
            WRITE(*,*) ' Not all data plotted -',
     1        ' use timerange to get rest. '
            GO TO 900
         END IF
         IF( IS .LE. 0 ) THEN
            WRITE(*,*) ' Bad station number:', IS
            GO TO 900
         END IF
C
C        Test for bad time.  With two integration times in use,
C        it is possible to have points out of order, which can
C        cause problems at the start.  For times early by less than
C        30 seconds, simply skip the points.  Otherwise warn the 
C        user about bad times.
C
         IF( IT .LE. 0 ) THEN
            IF( SEC - TMIN .GT. -30.0 ) THEN
               GO TO 400
            ELSE
               WRITE(*,*) ' Bad time (out of order?):', IT
               WRITE(*,*) ' If data file is not in time order, '//
     1            'restart and give start (non-zero) and stop times.'
               GO TO 900
            END IF
         END IF
         IF( IT .GT. NTIME ) NTIME = IT
C
C        Read the data.
C
         NCHMAX = MAX( NCHAN, NCHMAX )
         DO I = 1, NCHAN
            WT(I) = GETNUM( WORD(I+5), 1, WLEN(I+5) )
            IF( GOTSUM ) THEN
               J = I + NCHAN + 5
               K = I + 2 * NCHAN + 5
               LOWWT(I) = GETNUM( WORD(J), 1, WLEN(J) )
               HIGHWT(I) = GETNUM( WORD(K), 1, WLEN(K) )
            END IF
         END DO
C
C        Accumulate the data.
C
         DO IC = 1, NCHAN
            INAVG(IT,IS) = INAVG(IT,IS) + 1
            TIME(IT,IS) = TIME(IT,IS) + SEC
            AVGWT(IT,IS) = AVGWT(IT,IS) + WT(IC)
            IF( GOTSUM ) THEN
               MINWT(IT,IS) = MIN( MINWT(IT,IS), LOWWT(IC) )
               MAXWT(IT,IS) = MAX( MAXWT(IT,IS), HIGHWT(IC) )
            ELSE
               MINWT(IT,IS) = MIN( MINWT(IT,IS), WT(IC) )
               MAXWT(IT,IS) = MAX( MAXWT(IT,IS), WT(IC) )
            END IF
C
C           Channel data.  Only for output summary file which is
C           only written when input is not a summary.
C
            IF( .NOT. GOTSUM ) THEN
               INCAVG(IC,IT,IS) = INCAVG(IC,IT,IS) + 1
               TIMEC(IC,IT,IS) = TIMEC(IC,IT,IS) + SEC
               AVGCWT(IC,IT,IS) = AVGCWT(IC,IT,IS) + WT(IC)
               MINCWT(IC,IT,IS) = MIN( MINCWT(IC,IT,IS), WT(IC) )
               MAXCWT(IC,IT,IS) = MAX( MAXCWT(IC,IT,IS), WT(IC) )
            END IF
         END DO
C
C        Return for next record.
C
         GO TO 400
C
 900  CONTINUE
C
C     Normalize etc.
C
      TMIN =  99.E9
      TMAX = -99.E9
      DO IT = 1, NTIME
         DO IS = 1, NSTA
            IF( INAVG(IT,IS) .NE. 0 ) THEN
               IF( INAVG(IT,IS) .NE. NCHAN .AND. GOTSUM ) THEN
                  NSAV = NSAV + 1
               END IF
               TIME(IT,IS) = TIME(IT,IS) / INAVG(IT,IS)
               AVGWT(IT,IS) = AVGWT(IT,IS) / INAVG(IT,IS)
               TMIN = MIN( TMIN, TIME(IT,IS) )
               TMAX = MAX( TMAX, TIME(IT,IS) )
               IF( WRTSUM ) THEN
                  DO IC = 1, NCHMAX
                     IF( INCAVG(IC,IT,IS) .GT. 0 ) THEN
                        TIMEC(IC,IT,IS) = TIMEC(IC,IT,IS) / 
     1                                    INCAVG(IC,IT,IS)
                        AVGCWT(IC,IT,IS) = AVGCWT(IC,IT,IS) / 
     1                                     INCAVG(IC,IT,IS)
                     ELSE
                        TIMEC(IC,IT,IS) = 0.0
                        AVGCWT(IC,IT,IS) = 0.0
                     END IF
                  END DO
               END IF
C
C              Write the summary file data.
C
               IF( WRTSUM ) THEN
                  JDAY = JDAY1 + INT( TIME(IT,IS) / 86400.0 )
                  HR = MOD( TIME(IT,IS), 86400.0 ) / 3600.0
                  WRITE( SUNIT, '( I5, F9.5, I3, 1X, A8, I3, 24F7.3 )' )
     1              JDAY, HR, IS, STNAME(IS), NCHAN, 
     2              (AVGCWT(IC,IT,IS), IC=1,NCHAN),
     3              (MINCWT(IC,IT,IS), IC=1,NCHAN),
     4              (MAXCWT(IC,IT,IS), IC=1,NCHAN)
               END IF               
            END IF
         END DO
      END DO
C
C     Warn if some sum file points were averaged.
C
      IF( NSAV .GT. 0 .AND. GOTSUM ) THEN
         WRITE(*,*) NSAV, ' output points represent an average ',
     1       'of more than one summary input point'
      END IF
C
C     Pad out the time range a bit before plotting.
C
      IF( TMIN .EQ. TMAX ) THEN
         TMIN = TMIN - 1800.
         TMAX = TMAX + 1800.
      ELSE
         TPAD = ( TMAX - TMIN ) * 0.02
         TMIN = TMIN - TPAD
         TMAX = TMAX + TPAD
      END IF
C
C     Get some other numbers.
C
      RETURN
      END
