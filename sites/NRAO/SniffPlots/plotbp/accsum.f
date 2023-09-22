      SUBROUTINE ACCSUM
C
C     Routine for PLOTBP that accumulates the summary information.
C
      INCLUDE    'plotbp.inc'
C
      INTEGER    ICH, ISTA, IIF, KSTA, IFR, KFR, INCR
      LOGICAL    NEW, SKIP, FRMATCH, GETHEAD
      REAL       DPHASE, APHASE
C ---------------------------------------------------------------------
C     Find out which station set this is.
C
      NEW = .TRUE.
      ISTA = 1
      DO WHILE( NEW .AND. ISTA .LE. NSTA )
         IF( STA1(ISTA) .EQ. NAME1 .AND. STA2(ISTA) .EQ. NAME2 ) THEN
            KSTA = ISTA
            NEW = .FALSE.
         END IF
         ISTA = ISTA + 1
      END DO
C
C     Establish a new one if needed.
C
      SKIP = .FALSE.
      IF( NEW ) THEN
         IF( ISTA .GT. MSTA ) THEN
            SKIP = .TRUE.
         ELSE
            NSTA = NSTA + 1
            KSTA = NSTA
            STA1(KSTA) = NAME1
            STA2(KSTA) = NAME2
         END IF
      END IF
C
      IF( .NOT. SKIP ) THEN
C
C        Get which frequency group is involved.
C
         NEW = .TRUE.
         IFR = 1
         DO WHILE( NEW .AND. IFR .LE. NFR )
            IF( NIF .EQ. SNIF(IFR) .AND. 
     1          NCHAN .EQ. SNCHAN(IFR) .AND.
     2          NCHIF .EQ. SNCHIF(IFR) .AND.
     3          BW .EQ. SBW(IFR) ) THEN
               FRMATCH = .TRUE.
               DO IIF = 1, NIF
                  IF( FREQ(IIF) .NE. SFREQ(IIF,IFR) .OR.
     1                STOKES(IIF) .NE. SSTOKE(IIF,IFR) .OR.
     2                SBD(IIF) .NE. SSBD(IIF,IFR) ) THEN
                     FRMATCH = .FALSE.
                  END IF
               END DO
               IF( FRMATCH ) THEN
                  NEW = .FALSE.
                  KFR = IFR
               END IF
            END IF
            IFR = IFR + 1
         END DO
C
         IF( NEW ) THEN
            IF( IFR .GT. MFR ) THEN
               WRITE(*,*) ' Too many frequency setups -skipping some'
               SKIP = .TRUE.
            ELSE
               NFR = NFR + 1
               WRITE(*,*) ' Found setup ', NFR
               KFR = NFR
               SNIF(KFR) = NIF
               SNCHAN(KFR) = NCHAN
               SNCHIF(KFR) = NCHIF
               SBW(KFR) = BW
               DO IIF = 1, NIF
                  SFREQ(IIF,KFR) = FREQ(IIF)
                  SSTOKE(IIF,KFR) = STOKES(IIF)
                  SSBD(IIF,KFR) = SBD(IIF)
               END DO
            END IF
         END IF
C
         IF( .NOT. SKIP ) THEN
C
C           Now accumulate the data.
C
            GETHEAD = .FALSE.
C
            IF( TYPE .EQ. 'AC' ) THEN
C
C              For autocorrelations, we want the extrema and the
C              average.
C
               NORMAC(KFR,KSTA) = NORMAC(KFR,KSTA) + 1
               DO ICH = 1, NCHAN
                  SUMAC(ICH,KFR,KSTA) = SUMAC(ICH,KFR,KSTA) + AMP(ICH)
                  SUMAC1(ICH,KFR,KSTA) = 
     1                  MIN( SUMAC1(ICH,KFR,KSTA), AMP(ICH) )
                  SUMAC2(ICH,KFR,KSTA) = 
     1                  MAX( SUMAC2(ICH,KFR,KSTA), AMP(ICH) )
               END DO
               GETHEAD = .TRUE.
            ELSE IF( TYPE .EQ. 'XC' ) THEN
C
C              For the cross correlations, we originally wanted the
C              spectrum with the highest amplitudes.  But this is 
C              occasionally dominated by a spectrum with low 
C              integration time.  Try using the phases to select 
C              instead.  Of course, a fringe fit has not been done,
C              so we need something insensitive to slope.  Try the
C              average adjacent point difference.
C              Without on-line averaging, adjacent channels are 
C              slightly correlated (average difference 80, rather 
C              than 90, degrees for the test case I used).  Therefore,
C              look at second channel over if using high resolution.
C
               IF( NCHAN .GE. 2 ) THEN
                  APHASE = 0.0
                  INCR = 1
                  IF( NCHAN .GT. NIF * 32 ) INCR = 2
                  DO ICH = INCR + 1, NCHAN
                     DPHASE = ABS( PHASE(ICH) - PHASE(ICH-INCR) )
                     IF( DPHASE .GT. 180.0 ) DPHASE = 360.0 - DPHASE
                     APHASE = APHASE + DPHASE
                  END DO
                  APHASE = APHASE / ( NCHAN - INCR )
               ELSE          
                  APHASE = 360.0
               END IF
C
C              If it is the lowest, keep the data and header info.
C
               IF( APHASE .LT. LOWDP(KFR,KSTA) ) THEN
                  DO ICH = 1, NCHAN
                     HIAMP(ICH,KFR,KSTA) = AMP(ICH)
                     HIPH(ICH,KFR,KSTA) = PHASE(ICH)
                  END DO
                  GETHEAD = .TRUE.
                  LOWDP(KFR,KSTA) = APHASE
               END IF
            END IF
C
C           Get the header stuff if we did an update.
C           This replaces whatever was there before, which is
C           what we want for the crosscorrelations and is
C           probably as good as anything else for the autocorrs.
C           Recall that cross and autocorrelations are done in
C           separate runs of the program.
C
            IF( GETHEAD ) THEN
               SSRC(KFR,KSTA) = SOURCE
               SJDAY(KFR,KSTA) = JDAY1
               IF( TYPE .EQ. 'XC' .OR. SCTIME(1,KFR,KSTA) .EQ. ' ' ) 
     1               SCTIME(1,KFR,KSTA) = CTIME(1)
               SCTIME(2,KFR,KSTA) = CTIME(2)
               SEXPNAM(KFR,KSTA) = EXPNAM
            END IF
C
         END IF
C
      END IF
C
      RETURN
      END
