      SUBROUTINE PLTAPD( IB )
C
C     Routine for PLOTAPD that plots a MBAS baselines of amplitude, 
C     phase, and delay.  Each baseline is plotted separately as 3
C     attached plots.  Multiple plots per page are handled by the
C     PGPLOT nx/ny mechanism specified in the main routine.
C
      INCLUDE 'plotapd.inc'
C
      INTEGER   MLABEL
      REAL      MTLAB
      PARAMETER (MLABEL=60) ! Max number of source names to write.
      PARAMETER (MTLAB=120.0) ! Relabel interval for same source(s)
C
      INTEGER   IB, JB, IT, ICH, LEN1, LTEXT, NLABEL, I, J
      REAL      LEFT, RIGHT, BOTTOM(4), TOP(4), CHT, XT, YT, XR, YR
      CHARACTER HEAD*100, JDATE*9, JPDATE*9, CH*2, CH2*7
      INTEGER   SYM(MCHAN), DSYM(MCHAN)  !  Plot symbol for each channel.
      REAL      PTHT(MCHAN), CIRHT   !  Plot symbol height.
      REAL      LTIME, CHRHTX, CHRHTY
      CHARACTER LSOURCE*12, TEXT*80
C
      DATA  LEFT, RIGHT / 0.1, 0.98 /
      DATA  BOTTOM / 0.1, 0.35, 0.5, 0.7 /
      DATA  TOP    / 0.35, 0.5, 0.7, 0.9 /
C      DATA  (DSYM(I),I=1,8) / 2, 3, 4, 5, 6, 7, 11, 12 /
      DATA  (DSYM(I),I=1,4) / 17, 4, 5, 7 /
C ----------------------------------------------------------------------
C     Encourage user:
C
C
C     Get baseline within arrays.
C
      JB = IB - IP1 + 1
C
C     Move to next plot page.
C
      CALL PGPAGE
C
C     Set up to plot using the full panel available.
C     The borders depend on character size, hence PGSCH.
C
      IF( XWIN ) THEN
         CHT = 0.8
      ELSE
         CHT = 1.0
      END IF
      CALL PGSCH( 1.0*CHT )
      CALL PGVSTD
C
C     Contrive to have the first requested channel plotted larger than
C     the others.
C
      CIRHT = 0.8 * CHT
      DO ICH = 1, NCHAN
         IF( ICH .EQ. DOCH(1) ) THEN
            PTHT(ICH) = 2.0 * CIRHT
            SYM(ICH) = DSYM(ICH)
         ELSE IF( ICH .GT. 4 ) THEN
            PTHT(ICH) = CIRHT
            SYM(ICH) = 1
         ELSE
            PTHT(ICH) = CIRHT
            SYM(ICH) = DSYM(ICH)
         END IF
      END DO
C
C     Put header on the panel.
C
      JPDATE = JDATE( JDAY1 )
      WRITE( HEAD, '( A, A, A, 3X, A, A, 3X, A9 )' )
     3          NAMEA(IB)(1:LEN1(NAMEA(IB))), '-', 
     4          NAMEB(IB)(1:LEN1(NAMEB(IB))),  
     2          'Amplitude, Phase, Delay and Rate for ', 
     1          EXPNAM(1:LEN1(EXPNAM)), 
     5          JPDATE
C
C     Rate plot.  Note that a negative or zero amplitude flags
C     a bad solution.  Don't plot such points.
C
      CALL PGSCH( 1.2*CHT )
      CALL PGSWIN( TMIN, TMAX, RMIN(JB), RMAX(JB) )
      CALL PGSVP( LEFT, RIGHT, BOTTOM(4), TOP(4) )
      CALL PGTBOX( 'BCTSZ', 0., 0, 'BCNTSV', 0.0, 0 )
      CALL PGMTXT( 'L', 4.0, 0.5, 0.5, 'Rate (mHz)' )
      CALL PGSCH( 1.5*CHT )
      CALL PGMTXT( 'T', 0.6, 0.5, 0.5, HEAD )
      CALL PGBBUF
      IF( NTIME(JB) .GT. 0 ) THEN
         DO J = 1, NDOCH
            ICH = DOCH(J)
            CALL PGSCH( PTHT(ICH) )
            DO IT = 1, NTIME(JB)
               IF( AMP(ICH,IT,JB) .GT. 0.0 ) THEN
                  CALL PGPT( 1, TIME(IT,JB), RATE(ICH,IT,JB), 
     1                       SYM(ICH) )
               END IF
            END DO
         END DO
      END IF
      CALL PGEBUF
C
C     Add source labels if there are not going to be more than MLABEL
C     of them.  First count them.  Time is in seconds.  MTLAB is
C     the minimum interval for labeling points on the same source (sec).
C    
      NLABEL = 0
      LSOURCE = ' '
      DO IT = 1, NTIME(JB)
         IF( TIME(IT,JB) - LTIME .GT. MTLAB .OR.
     1       SOURCE(IT,JB) .NE. LSOURCE ) THEN
            NLABEL = NLABEL + 1
         END IF
         LTIME = TIME(IT,JB)
         LSOURCE = SOURCE(IT,JB)
      END DO
C
C     Now write labels
C
      IF( NLABEL .LE. MLABEL) THEN
C
C        Get character height in world coordinates.
C
         CALL PGSCH( 0.7*CHT )
         CALL PGQCS( 0, CHRHTX, CHRHTY )
         CHRHTX = CHRHTX * ( TMAX - TMIN ) / ( RIGHT - LEFT )
         YR = RMAX(JB) - RMIN(JB)
         YT = RMIN(JB) + 0.05 * YR
         LSOURCE = ' '
         DO IT = 1, NTIME(JB)
            IF( TIME(IT,JB) - LTIME .GT. MTLAB .OR.
     1          SOURCE(IT,JB) .NE. LSOURCE ) THEN
               XT = TIME(IT,JB) + 0.5 * CHRHTX
               CALL PGPTXT( XT, YT, 90.0, 0.0, SOURCE(IT,JB) ) 
            END IF
            LTIME = TIME(IT,JB)
            LSOURCE = SOURCE(IT,JB)
         END DO
      END IF
C
C     Delay plot.
C
      CALL PGSCH( 1.2*CHT )
      CALL PGSWIN( TMIN, TMAX, DMIN(JB), DMAX(JB) )
      CALL PGSVP( LEFT, RIGHT, BOTTOM(3), TOP(3) )
      CALL PGTBOX( 'BCTSZ', 0., 0, 'BCNTSV', 0.0, 0 )
      CALL PGMTXT( 'L', 4.0, 0.5, 0.5, 'Delay (ns)' )
      CALL PGSCH( 1.5*CHT )
      CALL PGBBUF
      IF( NTIME(JB) .GT. 0 ) THEN
         DO J = 1, NDOCH
            ICH = DOCH(J)
            CALL PGSCH( PTHT(ICH) )
            DO IT = 1, NTIME(JB)
               IF( AMP(ICH,IT,JB) .GT. 0.0 ) THEN
                  CALL PGPT( 1, TIME(IT,JB), DELAY(ICH,IT,JB), 
     1                       SYM(ICH) )
               END IF
            END DO
         END DO
      END IF
      CALL PGEBUF
C
C     Phase plot
C
      CALL PGSCH( 1.2*CHT )
      CALL PGSWIN( TMIN, TMAX, PMIN(JB), PMAX(JB) )
      CALL PGVPORT( LEFT, RIGHT, BOTTOM(2), TOP(2) )
      CALL PGTBOX( 'BCTSZ', 0.0, 0, 'BCNTSV', 120., 3 )
      CALL PGMTXT( 'L', 4.0, 0.5, 0.5, 'Phase (deg)' )
      CALL PGBBUF
      IF( NTIME(JB) .GT. 0 ) THEN
         DO J = 1, NDOCH
            ICH = DOCH(J)
            CALL PGSCH( PTHT(ICH) )
            DO IT = 1, NTIME(JB)
               IF( AMP(ICH,IT,JB) .GT. 0.0 ) THEN
                  CALL PGPT( 1, TIME(IT,JB), PHASE(ICH,IT,JB),
     1                       SYM(ICH) )
               END IF
            END DO
         END DO
      END IF
      CALL PGEBUF
C
C     Amp plot
C
      CALL PGSCH( 1.2*CHT )
      CALL PGSWIN( TMIN, TMAX, AMIN(JB), AMAX(JB) )
      CALL PGVPORT( LEFT, RIGHT, BOTTOM(1), TOP(1) )
      CALL PGTBOX( 'BCNTSZH', 0., 0, 'BCNTSV', 0.0, 0 )
      CALL PGMTXT( 'L', 4.0, 0.5, 0.5, 'Amplitude * 10\\u5\\d' )
      CALL PGMTXT( 'B', 2.6, 0.5, 0.5, 
     1       'Time (from beginning of '//JPDATE//')' )
      CALL PGBBUF
      IF( NTIME(JB) .GT. 0 ) THEN
         DO J = 1, NDOCH
            ICH = DOCH(J)
            CALL PGSCH( PTHT(ICH) )
            DO IT = 1, NTIME(JB)
               IF( AMP(ICH,IT,JB) .GT. 0.0 ) THEN
                  CALL PGPT( 1, TIME(IT,JB), AMP(ICH,IT,JB),
     1                       SYM(ICH) )
               END IF
            END DO
         END DO
      END IF
C
C     Write symbol index at bottom of plot:
C
      CALL PGSCH( 1.1*CHT )
      XR = TMAX - TMIN
      YR = AMAX(JB) - AMIN(JB)
      XT = TMIN + XR * 0.05
      YT = AMIN(JB) + YR * 0.12
      TEXT = 'Plot symbols by baseband channel:'
      LTEXT = LEN1( TEXT )
      CALL PGTEXT( XT, YT, TEXT  )
C
C     Get how far to move over for sample symbols.
C
      CALL PGQCS( 0, CHRHTX, CHRHTY )
      CHRHTX = CHRHTX * XR / ( RIGHT - LEFT )
      XT = XT + LTEXT * CHRHTX * 0.5
C
C     Write the labeled symbols.
C     Only do the ones that are not symbol 1 plus one "Other" for
C     symbol 1 if there are any.
C
      DO J = 1, NDOCH
         ICH = DOCH(J)
         IF( SYM(ICH) .NE. 1 ) THEN
            XT = XT + CHRHTX * 2
            WRITE( CH, '(I1,A)' ) ICH, ':'
            CALL PGSCH( 1.2*CHT )
            CALL PGTEXT( XT, YT, CH )
            CALL PGSCH( PTHT(ICH) )
            XT = XT + CHRHTX * 2
            CALL PGPT( 1, XT, YT+YR*0.02, SYM(ICH) )
            CALL PGSCH( CIRHT ) 
            CALL PGPT( 1, XT, YT+YR*0.02, 26 )
         END IF
      END DO
      DO J = 1, NDOCH
         ICH = DOCH(J)
         IF( SYM(ICH) .EQ. 1 ) THEN
            XT = XT + CHRHTX * 2
            CALL PGSCH( 1.2*CHT )
            CH2 = 'Others:'
            CALL PGTEXT( XT, YT, CH2 )
            CALL PGSCH( PTHT(ICH) )
            XT = XT + CHRHTX * 5
            CALL PGPT( 1, XT, YT+YR*0.02, SYM(ICH) )
            CALL PGSCH( CIRHT ) 
            CALL PGPT( 1, XT, YT+YR*0.02, 26 )
            GO TO 99
         END IF
      END DO
   99 CONTINUE
C
      CALL PGEBUF
C
      RETURN
      END




