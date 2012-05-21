      SUBROUTINE GETNGAIN( IER, FREQ, YR1, YR2,
     1       TIMERANGE, TCAL, FT, TSYS, DPFU, POLY, STATION,
     2       BANDNAME, IS, IB, IP, COMMENT )
C
C     Subroutine for PLOSUM that reads the gain records file.
C     It is adapted from GETGAIN for PTANAL.
C
C     Call parameters:
C       Inputs:
          CHARACTER STATION(*)*(*) ! station code list.
          CHARACTER BANDNAME(*)*(*) ! band name list
C       Outputs:
          INTEGER  IER       !  0-ok, 1-eof.
          REAL     FREQ      !  Observing frequency (MHz).
          REAL     YR1, YR2  ! Time range for data fractional years.
          INTEGER  TIMERANGE(*) ! Time range from file (y,m,d,h,y,m,d,h)
          REAL     TCAL      !  Cal temperature.
          REAL     FT        !  Scale factor required for cal and Tsys.
          REAL     TSYS      !  Expected zenith Tsys.
          REAL     DPFU      !  Expected antenna gain.
          REAL     POLY(7)   !  Polynomial coefficients for gain curve.
          INTEGER  IS        !  Station number in list
          INTEGER  IB        !  Band number in list
          INTEGER  IP        !  1:rcp, 2:lcp
          CHARACTER COMMENT*64  ! Comment with data.
C
C     Program variables
C
      INTEGER     MAXSTA
      PARAMETER   (MAXSTA=10)
      CHARACTER   GBAND*6, TEMP*8
      INTEGER     JYR1, JMONTH1, JDAY1, JHR1, JYR2, JMONTH2, JDAY2, JHR2
      DOUBLE PRECISION  GETYR
C
C     Data input via KEYIN
C
      INTEGER    NPAR, I, MODE
      PARAMETER  (NPAR=43)
      DOUBLE PRECISION  NAME(NPAR), VALUE(NPAR), ENDMARK, BLANK
      CHARACTER         NAMEC(NPAR)
      LOGICAL           FIRST
      DATA (NAMEC(I),I=1,10)  / 8*'TIMERANG', 'FREQ', 'TCAL' /
      DATA (NAMEC(I),I=11,15) / 'RCP', 'LCP', 'FT', 'TS', 'GAIN' /
      DATA (NAMEC(I),I=16,25) / 'ALTAZ', 'DPFU', 'BAND', 7*'POLY' /
      DATA (NAMEC(I),I=26,28) / 'SC', 'HN', 'NL' /
      DATA (NAMEC(I),I=29,31) / 'FD', 'LA', 'PT' /
      DATA (NAMEC(I),I=32,34) / 'KP', 'OV', 'BR' /
      DATA (NAMEC(I),I=35,35) / 'MK' /
      DATA (NAMEC(I),I=36,43) / 'COMMENT', 7*' ' /
      DATA (VALUE(I),I=1,35)  / 35*-1.D0 /
      DATA FIRST / .TRUE. /
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
      DO I = 10, 17
         VALUE(I) = -1.D0
      END DO
      VALUE(19) = 1.D0
      DO I = 1, 6
         VALUE(19+I) = 0.D0
      END DO
      DO I = 26, 35
         VALUE(I) = -1.D0
      END DO
      DO I = 36, 43
         VALUE(I) = BLANK
      END DO
C
C     Call Keyin
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
      WRITE( COMMENT, '( 8A8 )' ) (VALUE(I), I=36, 43)
C
C     Band name.
C
      WRITE( GBAND, '( A6 )' ) VALUE(18)
      IB = 0
      DO I = 1, 12
         IF( GBAND .EQ. BANDNAME(I) ) IB = I
      END DO
      IF( IB .EQ. 0 ) THEN
         WRITE(*,*) ' GETNGAIN: Invalid band: '//GBAND
         STOP
      END IF
C
C     Station number - check stations array too.
C
      IS = 0
      DO I = 1, 10
            IF( VALUE(25+I) .EQ. 0.D0 ) IS = I
      END DO
      WRITE( TEMP, '(A8)' ) NAME(25+IS)
      IF( INDEX( TEMP, STATION(IS) ) .EQ. 0 ) THEN
         WRITE(*,*) ' GETNGAIN: Station request to getngain'//
     1         ' not valid: ', TEMP, ' req:', STATION(IS)
         STOP
      END IF
      IF( IS .EQ. 0 ) THEN
         WRITE(*,*) ' GETNGAIN: No station in gain file.'
         STOP
      END IF
C
C     Polarization number.
C
      IF( VALUE(11) .EQ. 0.D0 ) THEN
         IP = 1
      ELSE IF( VALUE(12) .EQ. 0.D0 ) THEN
         IP = 2
      ELSE
         WRITE(*,*) ' GETGAIN: No polarization specified'//
     +           ' for an entry in the gain file.'
         WRITE(*,*) '          Station: ', STATION(IS)
         STOP
      END IF
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
      TCAL = VALUE(10)
      FT   = VALUE(13)
      TSYS = VALUE(14)
      DPFU = VALUE(17)
C
C     Gain curve.  Default to 1,0,0,0,0,0,0
C
      DO I = 1, 7      
         POLY(I) = VALUE(18+I)
      END DO
C
C     Jump to here when out of data
C
 101  CONTINUE
C
      RETURN
      END
