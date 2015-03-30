      SUBROUTINE GETPGAIN( IER, FREQ, YR1, YR2, TIMERANGE, 
     1                     TCALR, TCALL, FTR, FTL, 
     2                     TSYSR, TSYSL, TRECR, TRECL, 
     3                     DPFUR, DPFUL, POLY, 
     4                     STATION, BANDNAME, COMMENT )
C
C     Subroutine for PLOTSUM others that reads a record from the
C     the gain records file (eg vlba_gains.key).
C     Modified from GETNGAIN for new format.
C     It is adapted from GETGAIN for PTANAL and is now called by GETGAIN.
C
C     Sept. 17, 2014.  Convert YR1 and YR2 to double precision.
C
C     Call output parameters:
C
      INTEGER   IER          ! 0-ok, 1-eof.
      REAL      FREQ         ! Observing frequency (MHz).
      DOUBLE PRECISION   YR1, YR2     ! Time range for data fractional years.
      INTEGER   TIMERANGE(*) ! Time range (y,m,d,h,y,m,d,h).
      REAL      TCALR, TCALL ! Cal temperature (R and L).
      REAL      FTR, FTL     ! Scale factors for cal and Tsys.
      REAL      TSYSR, TSYSL ! Expected zenith Tsys (R and L).
      REAL      TRECR, TRECL ! Expected receiver temp (R and L).
      REAL      DPFUR, DPFUL ! Expected antenna gain.
      REAL      POLY(7)      ! Polynomial coeffs for gain curve.
      CHARACTER COMMENT*(*)  ! Comment with data.  (64)
      CHARACTER STATION*(*)  ! Station code. (2)
      CHARACTER BANDNAME*(*) ! Band name. (6)
C
C     Program variables
C
      INTEGER           JYR1, JMONTH1, JDAY1, JHR1
      INTEGER           JYR2, JMONTH2, JDAY2, JHR2
      DOUBLE PRECISION  GETYR
C
C     Data input via KEYIN
C
      INTEGER           NPAR, I, MODE, IS
      PARAMETER         (NPAR=48)
      DOUBLE PRECISION  NAME(NPAR), VALUE(NPAR), ENDMARK, BLANK
      CHARACTER         NAMEC(NPAR)*8
      LOGICAL           FIRST
      DATA (NAMEC(I),I=1,11)  / 8*'TIMERANG', 'FREQ', 2*'TCAL' /
      DATA (NAMEC(I),I=12,19) / 2*'FT', 2*'TS', 2*'TR', 'GAIN', 'Dummy'/
      DATA (NAMEC(I),I=20,30) / 'ALTAZ', 2*'DPFU', 'BAND', 7*'POLY' /
      DATA (NAMEC(I),I=31,35) / 'SC', 'HN', 'NL', 'FD', 'LA'  /
      DATA (NAMEC(I),I=36,40) / 'PT', 'KP', 'OV', 'BR', 'MK'  /
      DATA (NAMEC(I),I=41,48) / 'COMMENT', 7*' ' /
      DATA (VALUE(I),I=1,40)  / 40*-1.D0 /
      DATA FIRST / .TRUE. /
      SAVE
C ---------------------------------------------------------------------
C     Keep G77 happy - it doesn't like to have DATA statements with
C     characters going to reals.
C
      IF( FIRST ) THEN
         CALL KPACK( '/       ', ENDMARK )
         CALL KPACK( '        ', BLANK )
         DO I = 1, NPAR
            CALL KPACK( '        ', NAME(I) )
            CALL KPACK( NAMEC(I), NAME(I) )
         END DO
      END IF
      FIRST = .FALSE.
C
C     Call to KEYIN subroutine to get next group of numbers.
C
C     First set defaults.
C
      DO I = 10, 19          !  Gains etc.
         VALUE(I) = -1.D0
      END DO
      VALUE(23) = 1.D0       !  POLY
      DO I = 1, 6
         VALUE(24+I) = 0.D0
      END DO
      DO I = 31, 40          !  Station code.
         VALUE(I) = -1.D0
      END DO
      DO I = 41, 48          !  Comment 
         VALUE(I) = BLANK
      END DO
C
C     Call Keyin to get the next record
C
      MODE = 0
      CALL KEYIN( NAME, VALUE, NPAR, ENDMARK, MODE, 37, 6 )
      IF( MODE .EQ. 1 ) THEN
         IER = 1
         GO TO 101
      ELSE
         IER = 0
      END IF
C
C     Decode data - first the comment.
C
      WRITE( COMMENT, '( 8A8 )' ) (VALUE(I), I=41,48)
C
C     Band name.
C
      WRITE( BANDNAME, '( A6 )' ) VALUE(23)
C
C     Station number - check stations array too.
C
      IS = 0
      DO I = 1, 10
            IF( VALUE(30+I) .EQ. 0.D0 ) IS = I
      END DO
      IF( IS .EQ. 0 ) THEN
         WRITE(*,*) ' GETPGAIN: A gain file record with no station'
         STOP
      END IF
      WRITE( STATION, '(A2)' ) NAME(30+IS)
C
C     Times
C
      JYR1    = VALUE(1)
      JMONTH1 = VALUE(2)
      JDAY1   = VALUE(3)
      JHR1    = VALUE(4)
      JYR2    = VALUE(5)
      JMONTH2 = VALUE(6)
      JDAY2   = VALUE(7)
      JHR2    = VALUE(8)
      YR1     = GETYR( JYR1, JMONTH1, JDAY1, JHR1 )
      YR2     = GETYR( JYR2, JMONTH2, JDAY2, JHR2 )
      TIMERANGE(1) = JYR1
      TIMERANGE(2) = JMONTH1
      TIMERANGE(3) = JDAY1
      TIMERANGE(4) = JHR1
      TIMERANGE(5) = JYR2
      TIMERANGE(6) = JMONTH2
      TIMERANGE(7) = JDAY2
      TIMERANGE(8) = JHR2
C
C     Some values
C
      FREQ = VALUE(9)
      TCALR = VALUE(10)
      TCALL = VALUE(11)
      FTR   = VALUE(12)
      FTL   = VALUE(13)
      TSYSR = VALUE(14)
      TSYSL = VALUE(15)
      TRECR = VALUE(16)
      TRECL = VALUE(17)
      DPFUR = VALUE(21)
      DPFUL = VALUE(22)
C
C     Gain curve.  Default to 1,0,0,0,0,0,0
C
      DO I = 1, 7      
         POLY(I) = VALUE(23+I)
      END DO
C
C     Jump to here when out of data
C
 101  CONTINUE
C
      RETURN
      END



