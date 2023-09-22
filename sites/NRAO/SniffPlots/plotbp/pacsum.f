      SUBROUTINE PACSUM( ISTA, IFR )
C
C     Actually set up and plot the autocorrelation summary plot for one
C     station/frequency band.  This will be called twice.  The second 
C     time, if required, will zoom in on the amps of most of the data, 
C     ignoring the high points.  The code was extracted from pltsum.f 
C     on Sept. 10, 2012.
C
C     AMAX (in the include file) will be set to 0.0 for the first call.
C     It should be set here during that call, along with AMPAVG.  Those
C     two determine if there will be a second call.
C
      INCLUDE   'plotbp.inc'
C
      INTEGER    ICH, IFR, ISTA, IIF, IPT, JPT, ICH1
      INTEGER    LEN1, NAVG
      REAL       XMIN, XMAX, XCH, YCH
      REAL       XLINE(MCHAN*2), YLINE(MCHAN*2), XCHAN(MCHAN)
      REAL       LEFT, RIGHT, BOTTOM, PTOP
      REAL       CHSIZE, XD(2), YD(2)
      CHARACTER  HDLINE*80, BTLINE*80, JDATE*9, PRSCH*9
      LOGICAL    FULLSC
C
      DATA     LEFT, RIGHT / 0.1, 0.95 /
      DATA     BOTTOM, PTOP / 0.15, 0.85 /
C  ------------------------------------------------------------------------
      FULLSC = AMAX .EQ. 0.0
C
C     Get some labels.
C
      HDLINE = STA1(ISTA)(1:LEN1(STA1(ISTA)))//' '//
     1     SEXPNAM(IFR,ISTA)(1:LEN1(SEXPNAM(IFR,ISTA)))//' '//
     2     JDATE( SJDAY(IFR,ISTA) )//' '//
     3     SCTIME(1,IFR,ISTA)//'-'//
     4     SCTIME(2,IFR,ISTA)
      WRITE( BTLINE, '( A, F7.3, A )' )
     1       'Channel (', SBW(IFR),' MHz/BBCh. )'
C
C     First call - full scale:
C
      IF( FULLSC ) THEN
C
C        Normalize the data.
C
         DO ICH = 1, SNCHAN(IFR)
            SUMAC(ICH,IFR,ISTA) = SUMAC(ICH,IFR,ISTA) /
     1            NORMAC(IFR,ISTA)
         END DO
C
C        Get the amplitude plot limits.
C
         XMIN = 0.0
         XMAX = SNCHAN(IFR) + 1
         AMAX = 0.0
         ALMAX = 0.0
         NAVG = 0
         DO ICH = 1, SNCHAN(IFR)
            AMAX = MAX( AMAX, SUMAC2(ICH,IFR,ISTA) )
            AMPAVG = AMPAVG + SUMAC2(ICH,IFR,ISTA)
            NAVG = NAVG + 1
         END DO
         AMAX = AMAX * 1.1
         IF( NAVG .GT. 0 ) THEN
            AMPAVG = AMPAVG / NAVG
         ELSE
            AMPAVG = AMAX / 3.0
         END IF        
         IF( AMAX .EQ. 0.0 ) AMAX = 1.1
C
      ELSE
C
C        Use AMAX from calling routine.
C
      END IF
C
C     Get the other  plot limits.
C
      XMIN = 0.0
      XMAX = SNCHAN(IFR) + 1
C
C     Set up the plot window
C
      CALL PGPAGE
      CHSIZE = 2.0
      CALL PGSCH( CHSIZE )
      CALL PGSLW( 1 )
      CALL PGSLS( 1 )
      CALL PGSVP( LEFT, RIGHT, BOTTOM, PTOP )
      CALL PGSWIN( XMIN, XMAX, 0.0, AMAX )
      CALL PGBOX( 'BCNTS', 0, 0, 'BCNTS', 0, 0 )
      CALL PGMTXT( 'T', 0.6, 0.5, 0.5, HDLINE )
      CALL PGMTXT( 'B', 2.3, 0.5, 0.5, BTLINE )
      IF( FULLSC ) THEN
         CALL PGLAB( ' ', 'Amp', ' ' )
      ELSE
         CALL PGLAB( ' ', 'Amp (Expanded Scale)', ' ' )
      END IF
C
C     Actually draw the data - one IF at a time.
C
      DO ICH = 1, SNCHAN(IFR)
         XCHAN(ICH) = ICH
      END DO
C
      DO IIF = 1, SNIF(IFR)
         ICH1 = ( IIF - 1 ) * SNCHIF(IFR) + 1
         CALL PGSLS( 1 )
C
C        Shade the region.
C
         IPT = 0
         DO ICH = ICH1, ICH1 + SNCHIF(IFR) - 1
            IPT = IPT + 1                     
            XLINE(IPT) = ICH
            YLINE(IPT) = MIN( SUMAC1(ICH,IFR,ISTA), AMAX )
            JPT = 2 * SNCHIF(IFR) + 1 - IPT
            XLINE(JPT) = ICH
            YLINE(JPT) = MIN( SUMAC2(ICH,IFR,ISTA), AMAX )
         END DO
         CALL PGSHLS( 2, 0.0, 0.8, 0.0 )
         CALL PGSCI( 2 )
         CALL PGPOLY( 2 * SNCHIF(IFR), XLINE, YLINE )
         CALL PGSCI( 1 )
C
C        Now draw the lines, after the shading to overwrite
C        that.  Prevent going above the plot. Start with average.
C
         CALL PGSLW( 3 )
         DO IPT = 1, SNCHIF(IFR)
            JPT = ICH1 - 1 + IPT
            YLINE(IPT) = MIN( SUMAC(JPT,IFR,ISTA), AMAX )
         END DO
         CALL PGLINE( SNCHIF(IFR), XCHAN(ICH1), YLINE )
C
C        Bottom line
C
         DO IPT = 1, SNCHIF(IFR)
            JPT = ICH1 - 1 + IPT
            YLINE(IPT) = MIN( SUMAC1(JPT,IFR,ISTA), AMAX )
         END DO
         CALL PGSLW( 1 )
         CALL PGLINE( SNCHIF(IFR), XCHAN(ICH1), YLINE )
C
C        Top line.
C
         DO IPT = 1, SNCHIF(IFR)
            JPT = ICH1 - 1 + IPT
            YLINE(IPT) = MIN( SUMAC2(JPT,IFR,ISTA), AMAX )
         END DO
         CALL PGSLW( 1 )
         CALL PGLINE( SNCHIF(IFR), XCHAN(ICH1), YLINE )
         CALL PGSLW( 1 )
      END DO
C
C     Draw lines between the BB channels and label them.
C
      DO IIF = 1, SNIF(IFR)
         ICH1 =  ( IIF - 1 ) * SNCHIF(IFR)
         XD(1) = ICH1 + 0.5
         XD(2) = XD(1)
         YD(1) = 0.0
         YD(2) = AMAX
         IF( IIF .NE. 1 ) CALL PGLINE( 2, XD, YD )
         XCH = ICH1 + 0.05 * SNCHIF(IFR)
         YCH = AMAX * 0.9
         IF( NIF .GT. 8 ) THEN
            YCH = YCH + AMAX * ( -0.04 + 0.07 * MOD(IIF,2) )
         END IF
         WRITE( PRSCH, '(F9.2)' ) SFREQ(IIF,IFR)
         IF( NIF .GE. 8 ) CALL PGSCH( CHSIZE*0.75 )
         CALL PGTEXT( XCH, YCH, PRSCH )
         XCH = ICH1 + 0.15 * SNCHIF(IFR)
         YCH = AMAX * 0.03
         CALL PGTEXT( XCH, YCH, SSTOKE(IIF,IFR)//' '//
     1                SSBD(IIF,IFR) )
         CALL PGSCH(CHSIZE)
      END DO
C
      RETURN
      END
