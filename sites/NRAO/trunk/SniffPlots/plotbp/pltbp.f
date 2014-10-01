      SUBROUTINE PLTBP( ALIM )
C
C     Actually plot the bandpasses for PLOTBP
C
C     If the scale is set by a few very high points, make a second plot
C     with expanded scale.  This is for autocorrelations only.
C
      INCLUDE  'plotbp.inc'
C
      INTEGER  ICH, LEN1, IIF, ICH1, NX, NY
      REAL  XMIN, XMAX, XCHAN(MCHAN), XD(2), YD(2)
      REAL  LEFT, RIGHT, BOTTOM, ATOP, PTOP, XCH, YCH
      REAL  CHSIZE, ALIM(MCHAN)
      CHARACTER  JDATE*9, HDLINE*80, BTLINE*80, PRSCH*9
      LOGICAL    FIRSTS, DOZOOM
      DATA  LEFT, RIGHT / 0.1, 0.95 /
      DATA  BOTTOM, ATOP, PTOP / 0.15, 0.6, 0.85 /
      DATA  FIRSTS / .TRUE. /
      SAVE  FIRSTS, NX, NY
C -------------------------------------------------
C
C     Divide the plot surface into screens if it hasn't already
C     been done.
C     For screen plots, only have 2 in the vertical direction.
C     For paper, make it 5.
C     In the horizontal direction, have 2 for 8 channels or less.
C     For more than 8, make it 1 - full width.
C
      IF( FIRSTS ) THEN
         IF( NIF .LE. 8 ) THEN
            NX = 2
         ELSE
            NX = 1
         END IF
         IF( TF(2:3) .EQ. 'XW' .OR. TF(2:3) .EQ. 'XS' ) THEN
            NY = 2
            XWIN = .TRUE.
         ELSE
            NY = 5
            XWIN = .FALSE.
         END IF
         CALL PGSUBP( NX, NY )
         FIRSTS = .FALSE.
      END IF
C
C     Adjust amplitude scale.  Set phase range to +- 180.
C     Avoid scale with range of zero.
C
      DOZOOM = AMAX .GT. ALMAX
      AMAX = AMAX * 1.1
      IF( AMAX .EQ. 0.0 ) AMAX = 0.1
      PMIN = MIN( PMIN, -185.0 )
      PMAX = MAX( PMAX, 185.0 )
C
C     Page
C
      CALL PGPAGE
      IF( XWIN ) THEN
         CHSIZE = 1.5
      ELSE
         CHSIZE = 2.0
      END IF
      CALL PGSCH(CHSIZE)
C
C     Set up plot.
C
      XMIN = 0.0
      XMAX = NCHAN + 1
      DO ICH = 1, NCHAN
         XCHAN(ICH) = ICH
      END DO
C
C     Check number of channels.
C
      IF( NIF .NE. NCHAN/NCHIF ) THEN
         WRITE(*,*) 'PLTBP: Number of IFs ', NIF, 
     1      ' not consistent with total channels and chan/IF ', 
     2      NCHAN, NCHIF
      END IF
C
C     Some label information:
C
      HDLINE = ' '//SOURCE(1:LEN1(SOURCE))//'  '//
     1      EXPNAM(1:LEN1(EXPNAM))//' '//JDATE( JDAY1 )//' '//
     2      CTIME(1)//'-'//CTIME(2)
      WRITE( BTLINE, '( A, F7.3, A )' )
     1      'Channel (', BW,' MHz/BBCh. )'
C
C     Treat AC and XC data separately.
C
C     First AC.
C
      IF( TYPE .EQ. 'AC' ) THEN
         CALL PGSLS( 1 )
         CALL PGSVP( LEFT, RIGHT, BOTTOM, PTOP )
         CALL PGSWIN( XMIN, XMAX, 0.0, AMAX )
         CALL PGBOX( 'BCNTS', 0.0, 0, 'BCNTS', 0.0, 0 )
         HDLINE = NAME1(1:LEN1(NAME1))//HDLINE
         CALL PGMTXT( 'T', 0.6, 0.5, 0.5, HDLINE )
         CALL PGMTXT( 'B', 2.3, 0.5, 0.5, BTLINE )
         CALL PGLAB( ' ', 'Amp * 1000', ' ' )
         DO IIF = 1, NIF
            ICH1 = ( IIF - 1 ) * NCHIF + 1
            IF( ICH1 .GT. MCHAN ) ICH1 = MCHAN
            CALL PGSLS( 1 )
            CALL PGLINE( NCHIF, XCHAN(ICH1), AMP(ICH1) )
            CALL PGSLS( 2 )
         END DO
C
C        Draw lines between the BB channels and label them.
C        Also write the frequency, polarization, and sideband.
C
         DO IIF = 1, NIF
            ICH1 =  ( IIF - 1 ) * NCHIF
            XD(1) = ICH1 + 0.5
            XD(2) = XD(1)
            YD(1) = 0.0
            YD(2) = AMAX
            IF( IIF .NE. 1 ) CALL PGLINE( 2, XD, YD )
            IF( NEWFMT .OR. IIF .EQ. 1 ) THEN
               XCH = ICH1 + 0.05 * NCHIF
               YCH = AMAX * 0.9
               IF( NIF .GT. 8 .AND. NX .GT. 1 ) THEN
                  YCH = YCH + AMAX * ( -0.04 + 0.07 * MOD( IIF, 2 ) )
               END IF
               WRITE( PRSCH, '(F9.2)' ) FREQ(IIF)
               IF( NIF .GE. 8 ) CALL PGSCH( CHSIZE*0.6 )
               CALL PGTEXT( XCH, YCH, PRSCH )
               XCH = ICH1 + 0.15 * NCHIF
               YCH = AMAX * 0.03
               CALL PGTEXT( XCH, YCH, STOKES(IIF)//' '//SBD(IIF) )
               CALL PGSCH(CHSIZE)
            END IF
         END DO
C
C        Expanded scale version.
C
         IF( DOZOOM ) THEN
            CALL PGPAGE
            CALL PGSLS( 1 )
            CALL PGSVP( LEFT, RIGHT, BOTTOM, PTOP )
            CALL PGSWIN( XMIN, XMAX, 0.0, ALMAX )
            CALL PGBOX( 'BCNTS', 0.0, 0, 'BCNTS', 0.0, 0 )
C            HDLINE = NAME1(1:LEN1(NAME1))//
C     1          SOURCE(1:LEN1(SOURCE))//'  Last plot, expanded scale.'
            CALL PGMTXT( 'T', 0.6, 0.5, 0.5, HDLINE )
            CALL PGMTXT( 'B', 2.3, 0.5, 0.5, BTLINE )
            CALL PGLAB( ' ', 'Amp * 1000 (Expanded Scale)', ' ' )
            DO IIF = 1, NIF
               ICH1 = ( IIF - 1 ) * NCHIF + 1
               IF( ICH1 .GT. MCHAN ) ICH1 = MCHAN
               CALL PGSLS( 1 )
               CALL PGLINE( NCHIF, XCHAN(ICH1), ALIM(ICH1) )
               CALL PGSLS( 2 )
            END DO
C    
C           Draw lines between the BB channels and label them.
C           Also write the frequency, polarization, and sideband.
C    
            DO IIF = 1, NIF
               ICH1 =  ( IIF - 1 ) * NCHIF
               XD(1) = ICH1 + 0.5
               XD(2) = XD(1)
               YD(1) = 0.0
               YD(2) = ALMAX
               IF( IIF .NE. 1 ) CALL PGLINE( 2, XD, YD )
               IF( NEWFMT .OR. IIF .EQ. 1 ) THEN
                  XCH = ICH1 + 0.05 * NCHIF
                  YCH = ALMAX * 0.9
                  IF( NIF .GT. 8 .AND. NX .GT. 1 ) THEN
                     YCH = YCH + ALMAX * ( -0.04 + 0.07*MOD( IIF, 2 ) )
                  END IF
                  WRITE( PRSCH, '(F9.2)' ) FREQ(IIF)
                  IF( NIF .GE. 8 ) CALL PGSCH( CHSIZE*0.6 )
                  CALL PGTEXT( XCH, YCH, PRSCH )
                  XCH = ICH1 + 0.15 * NCHIF
                  YCH = ALMAX * 0.03
                  CALL PGTEXT( XCH, YCH, STOKES(IIF)//' '//SBD(IIF) )
                  CALL PGSCH(CHSIZE)
               END IF
            END DO
         END IF
C
C     Then XC.
C
      ELSE IF( TYPE .EQ. 'XC' ) THEN
C
C        Amplitude
C
         CALL PGSLS( 1 )
         CALL PGSVP( LEFT, RIGHT, BOTTOM, ATOP )
         CALL PGSWIN( XMIN, XMAX, 0.0, AMAX )
         CALL PGBOX( 'BCNTS', 0.0, 0, 'BCNTS', 0.0, 0 )
         CALL PGMTXT( 'B', 2.3, 0.5, 0.5, BTLINE )
         CALL PGLAB( ' ', 'Amp * 1000', ' ' )
         DO IIF = 1, NIF
            ICH1 = ( IIF - 1 ) * NCHIF + 1
            CALL PGLINE( NCHIF, XCHAN(ICH1), AMP(ICH1) )
         END DO
C
C        Draw lines between the BB channels and label them.
C        Also write the frequency, polarization, and sideband.
C
         CALL PGSLS( 2 )
         DO IIF = 1, NIF
            ICH1 =  ( IIF - 1 ) * NCHIF
            XD(1) = ICH1 + 0.5
            XD(2) = XD(1)
            YD(1) = 0.0
            YD(2) = AMAX
            IF( IIF .NE. 1 ) CALL PGLINE( 2, XD, YD )
            IF( NEWFMT .OR. IIF .EQ. 1 ) THEN
               XCH = ICH1 + 0.05 * NCHIF
               YCH = AMAX * 0.9
               IF( NIF .GT. 8 .AND. NX .GT. 1 ) THEN
                  YCH = YCH + AMAX * ( -0.04 + 0.07 * MOD( IIF, 2 ) )
               END IF
               WRITE( PRSCH, '(F9.2)' ) FREQ(IIF)
               IF( NIF .GE. 8 ) CALL PGSCH( CHSIZE*0.7 )
               CALL PGTEXT( XCH, YCH, PRSCH )
               XCH = ICH1 + 0.15 * NCHIF
               YCH = AMAX * 0.03
               CALL PGTEXT( XCH, YCH, STOKES(IIF)//' '//SBD(IIF) )
               CALL PGSCH(CHSIZE)
            END IF
         END DO
C
C        Phase
C
         CALL PGSLS( 1 )
         CALL PGSVP( LEFT, RIGHT, ATOP, PTOP )
         CALL PGSWIN( XMIN, XMAX, PMIN, PMAX )
         CALL PGBOX( 'BCTS', 0.0, 0, 'BCNTS', 120.0, 3 )
         HDLINE = NAME1(1:LEN1(NAME1))//'-'//NAME2(1:LEN1(NAME2))//
     1         HDLINE
         CALL PGMTXT( 'T', 0.6, 0.5, 0.5, HDLINE )
         CALL PGLAB( ' ', 'Phase', ' ' )
         DO IIF = 1, NIF
            ICH1 = ( IIF - 1 ) * NCHIF + 1
            CALL PGPT( NCHIF, XCHAN(ICH1), PHASE(ICH1), 20 )
         END DO
C
C        Plot lines between IF's.
C
         CALL PGSLS( 2 )
         IF( NIF .GE. 2 ) THEN
            DO IIF = 2, NIF
               ICH1 = ( IIF - 1 ) * NCHIF
               XD(1) = ICH1 + 0.5
               XD(2) = XD(1)
               YD(1) = PMIN
               YD(2) = PMAX
               CALL PGLINE( 2, XD, YD )
            END DO
         END IF
C
      END IF
C
      RETURN
      END
