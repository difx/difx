      SUBROUTINE PLTSUM
C
C     Routine for PLOTBP - the sniffer bandpass plotting program - 
C     that plots the bandpass summary files.  For the autocorrelations,
C     the average and extrema seen for each station/band are plotted.
C     For the cross correlation, the highest-average-amplitude 
C     spectrum from each station/band is plotted.  The data for these
C     plots is gathered in routine ACCSUM.
C
      INCLUDE   'plotbp.inc'
C
      INTEGER    ICH, IFR, ISTA, IIF, ICH1
      INTEGER    IER, PGBEG, LEN1
      REAL       XMIN, XMAX, XCH, YCH
      REAL       XCHAN(MCHAN)
      REAL       LEFT, RIGHT, BOTTOM, ATOP, PTOP
      REAL       CHSIZE, XD(2), YD(2)
      LOGICAL    GOTAC, GOTXC
      CHARACTER  HDLINE*80, BTLINE*80, JDATE*9, PRSCH*9
C
      DATA     LEFT, RIGHT / 0.1, 0.95 /
      DATA     BOTTOM, ATOP, PTOP / 0.15, 0.6, 0.85 /
      DATA     PMIN, PMAX / -185.0, 185.0 /
C ---------------------------------------------------------------------
C     See what we've got.  Don't make these plots if we are running
C     in interactive mode - the user might not have write permission.
C
      GOTAC = .FALSE.
      GOTXC = .FALSE.
      IF( NFR .NE. 0 .AND. NSTA .NE. 0 .AND. .NOT. XWIN ) THEN
         DO IFR = 1, NFR
            DO ISTA = 1, NSTA
               IF( NORMAC(IFR,ISTA) .GT. 0 ) GOTAC = .TRUE.
               IF( LOWDP(IFR,ISTA) .LT. 360.0 ) GOTXC = .TRUE.
            END DO
         END DO
      END IF
C
C     First deal with autocorrelations if they are here.
C
      IF( GOTAC ) THEN
C
C         Open plot file.  For now, don't allow a choice of plot
C         name or type.  Can modify later, if needed, but I want
C         this to not require a change to the analysts scripts.
C
         IER = PGBEG( 0, 'acbandsum.ps/vps', 2, 5 )
         IF( IER .NE. 1 ) THEN
            WRITE(*,*) 'Error opening AC summary plot file.'
            GO TO 999
         END IF
C
C        Loop through the antenna/band plots.
C        Put the plotting in a subroutine that can be used with
C        different scales to allow expansion of scale when there
C        are strong RFI spikes.
C
         DO ISTA = 1, NSTA
            DO IFR = 1, NFR
               IF( NORMAC(IFR,ISTA) .GT. 0 ) THEN
C
                  AMAX = 0.0
                  CALL PACSUM( ISTA, IFR )
C
                  IF( AMAX .GT. 3.0 * AMPAVG ) THEN
                     AMAX = 3.0 * AMPAVG
                     CALL PACSUM( ISTA, IFR )
                  END IF
C
               END IF
            END DO
         END DO
C
         CALL PGIDEN
         CALL PGCLOS
C
      END IF
C
C     Then deal with crosscorrelations.
C
      IF( GOTXC ) THEN
C
C         Open plot file.  Same deal as AC plot.
C
         IER = PGBEG( 0, 'xcbandmax.ps/vps', 2, 5 )
         IF( IER .NE. 1 ) THEN
            WRITE(*,*) 'Error opening XC summary plot file.'
            GO TO 999
         END IF
C
C        Loop through the antenna/band plots.
C
         DO ISTA = 1, NSTA
            DO IFR = 1, NFR
               IF( LOWDP(IFR,ISTA) .LT. 360.0 ) THEN
C
C                 Get the bottom label.
C
                  WRITE( BTLINE, '( A, F7.3, A )' )
     1                'Channel (', SBW(IFR),' MHz/BBCh. )'
C
C                 Get the plot limits.
C
                  XMIN = 0.0
                  XMAX = SNCHAN(IFR) + 1
                  AMAX = 0.0
                  DO ICH = 1, SNCHAN(IFR)
                     AMAX = MAX( AMAX, HIAMP(ICH,IFR,ISTA) )
                  END DO
                  AMAX = AMAX * 1.1
                  IF( AMAX .EQ. 0.0 ) AMAX = 1.1
C
C                 Set up the amplitude plot window
C
                  CALL PGPAGE
                  CHSIZE = 2.0
                  CALL PGSCH( CHSIZE )
                  CALL PGSLW( 1 )
                  CALL PGSLS( 1 )
                  CALL PGSVP( LEFT, RIGHT, BOTTOM, ATOP )
                  CALL PGSWIN( XMIN, XMAX, 0.0, AMAX )
                  CALL PGBOX( 'BCNTS', 0, 0, 'BCNTS', 0, 0 )
                  CALL PGMTXT( 'B', 2.3, 0.5, 0.5, BTLINE )
                  CALL PGLAB( ' ', 'Amp', ' ' )
C
C                 Actually draw the data - one IF at a time.
C
                  DO ICH = 1, SNCHAN(IFR)
                     XCHAN(ICH) = ICH
                  END DO
C
                  DO IIF = 1, SNIF(IFR)
                     ICH1 = ( IIF - 1 ) * SNCHIF(IFR) + 1
                     CALL PGSLS( 1 )
                     CALL PGSLW( 3 )
                     CALL PGLINE( SNCHIF(IFR), XCHAN(ICH1), 
     1                       HIAMP(ICH1,IFR,ISTA) )
                     CALL PGSLW( 1 )
                  END DO
C
C                 Draw lines between the BB channels and label them.
C
                  DO IIF = 1, SNIF(IFR)
                     ICH1 =  ( IIF - 1 ) * SNCHIF(IFR)
                     XD(1) = ICH1 + 0.5
                     XD(2) = XD(1)
                     YD(1) = 0.0
                     YD(2) = AMAX
                     IF( IIF .NE. 1 ) CALL PGLINE( 2, XD, YD )
                     XCH = ICH1 + 0.05 * SNCHIF(IFR)
                     YCH = AMAX * 0.1
                     WRITE( PRSCH, '(F9.2)' ) SFREQ(IIF,IFR)
                     IF( NIF .GE. 8 ) CALL PGSCH( CHSIZE*0.75 )
                     CALL PGTEXT( XCH, YCH, PRSCH )
                     XCH = ICH1 + 0.15 * SNCHIF(IFR)
                     YCH = AMAX * 0.03
                     CALL PGTEXT( XCH, YCH, SSTOKE(IIF,IFR)//' '//
     1                            SSBD(IIF,IFR) )
                     CALL PGSCH(CHSIZE)
                  END DO
C
C                 Now phase.  First get the top label.
C
                  HDLINE = STA1(ISTA)(1:LEN1(STA1(ISTA)))//' '//
     1               STA2(ISTA)(1:LEN1(STA2(ISTA)))//' '//
     2               SSRC(IFR,ISTA)(1:LEN1(SSRC(IFR,ISTA)))//' '//
     3               SEXPNAM(IFR,ISTA)(1:LEN1(SEXPNAM(IFR,ISTA)))//' '//
     4               JDATE( SJDAY(IFR,ISTA) )//' '//
     5               SCTIME(1,IFR,ISTA)//'-'//
     6               SCTIME(2,IFR,ISTA)
C
C                 Set up the phase plot window
C
                  CHSIZE = 2.0
                  CALL PGSCH( CHSIZE )
                  CALL PGSLW( 1 )
                  CALL PGSLS( 1 )
                  CALL PGSVP( LEFT, RIGHT, ATOP, PTOP )
                  CALL PGSWIN( XMIN, XMAX, PMIN, PMAX )
                  CALL PGBOX( 'BCTS', 0, 0, 'BCNTS', 0, 0 )
                  CALL PGMTXT( 'T', 0.6, 0.5, 0.5, HDLINE )
                  CALL PGLAB( ' ', 'Phase', ' ' )
C
C                 Actually draw the data - one IF at a time.
C
                  CALL PGSLW( 3 )
                  CALL PGPT( SNCHAN(IFR), XCHAN, HIPH(1,IFR,ISTA), 20 )
                  CALL PGSLW( 1 )
C
C                 Draw lines between the BB channels and label them.
C
                  DO IIF = 1, SNIF(IFR)
                     ICH1 =  ( IIF - 1 ) * SNCHIF(IFR)
                     XD(1) = ICH1 + 0.5
                     XD(2) = XD(1)
                     YD(1) = PMIN
                     YD(2) = PMAX
                     IF( IIF .NE. 1 ) CALL PGLINE( 2, XD, YD )
                  END DO
C
               END IF
            END DO
         END DO
C
         CALL PGIDEN
         CALL PGCLOS
C
      END IF
C
 999  CONTINUE
      RETURN
      END
