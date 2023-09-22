      SUBROUTINE GETCMD( QUIT )
C
C     Routine that gets user requests for plot parameters for CLOCKHIST.
C
      INCLUDE 'clock.inc'
C
      INTEGER    INPLT, PGBEG, LEN1, NCMDPLT(MAXSTA), IER, WHICH
      INTEGER    NX, NY, LNX, LNY
      CHARACTER  CMDPLT(MAXSTA)*2, LPLTFILE*80
      LOGICAL    FIRST, QUIT
C
C     Keyin input parameters.
C
      INTEGER      NPARS, I, MODE, I1
      PARAMETER    (NPARS=30)
C
      INTEGER           KI(NPARS), KEYPTR
      CHARACTER         KC(NPARS)*8, KCHAR*80
      DOUBLE PRECISION  KD(NPARS*2), ENDMRK, BLANK
C
      SAVE              KI, KD, KC, ENDMRK, BLANK, LNX, LNY
      SAVE              LPLTFILE, FIRST
C
      DATA   LPLTFILE / ' ' /
      DATA   FIRST /.TRUE. /
      DATA   (KI(I),I=1,3)     / NPARS, 0, 3 /
C  ----------------------------------------------------------------------
      IF( FIRST ) THEN
C
C        Set up the arrays.  This used to use data statements, 
C        but g77 doesn't like putting character strings into 
C        double precision variables.
C
         CALL KPACK( '/       ', ENDMRK )
         CALL KPACK( '        ', BLANK )
         CALL KEYCHR( 'PLOTfile', '/xs', 80, KD, KC, KI )
         CALL KEYCHR( 'REFsta', 'LA', 2, KD, KC, KI )
         CALL KEYADD( 'PLTSTA', 0.D0, 10, KD, KC, KI )
         CALL KEYADD( 'TMIN', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'TMAX', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'YMIN', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'YMAX', 0.D0, 1, KD, KC, KI )
         CALL KEYADD( 'QUIT', -99.D9, 1, KD, KC, KI )
         CALL KEYADD( 'MINAMP', 1.D-4, 1, KD, KC, KI )
         CALL KEYADD( 'MAXRATE', 20.D0, 1, KD, KC, KI )
         CALL KEYADD( 'NX', 1.D0, 1, KD, KC, KI )
         CALL KEYADD( 'NY', 1.D0, 1, KD, KC, KI )
      END IF
C
C     Set some PLTSTA defaults.
C
      I1 = KEYPTR( 'PLTSTA', KC, KI )
       DO I = 1, 10
         CALL KPACK( '  ', KD(I1+I-1) )
      END DO
C
C
C     Get the current values to display with prompt.
C
      PLTFILE = KCHAR( 'PLOTfile', 80, .FALSE., KD, KC, KI )
      REFSTA = KCHAR( 'REFsta', 2, .TRUE., KD, KC, KI )
      I1 = KEYPTR( 'PLTSTA', KC, KI )
      DO I = 1, 10
         WRITE( CMDPLT(I), '( A2 )' ) KD(I1-1+I)
         CALL UPCASE( CMDPLT(I) )
      END DO
      UXMIN = KD( KEYPTR( 'TMIN', KC, KI ) )
      UXMAX = KD( KEYPTR( 'TMAX', KC, KI ) )
      UYMIN = KD( KEYPTR( 'YMIN', KC, KI ) )
      UYMAX = KD( KEYPTR( 'YMAX', KC, KI ) )
      UMINAMP = KD( KEYPTR( 'MINAMP', KC, KI ) )
      UMAXRATE = KD( KEYPTR( 'MAXRATE', KC, KI ) )
      NX = KD( KEYPTR( 'NX', KC, KI ) )
      NY = KD( KEYPTR( 'NY', KC, KI ) )
C
C     Some initializations, especially of PLTSTA
C
      IF( FIRST ) THEN
         NPLT = 0
         DO I = 1, 10
            IF( CMDPLT(I) .NE. '  ' ) THEN
               NPLT = NPLT + 1
               PLTSTA(I) = WHICH( CMDPLT(I), NAMES, MAXSTA )
            END IF
         END DO         
      END IF
C
C     Ok, no longer the first pass.
C
      FIRST = .FALSE.
      QUIT = .FALSE.
C
C     Display them to the user.
C
      WRITE(*,*) ' Current values of input parameters:'
      WRITE(*,*) '  PLOTfile: ', PLTFILE(1:LEN1(PLTFILE))
      WRITE(*,*) '  NX, NY: ', NX, NY     
      WRITE(*,*) '  REFsta: ', REFSTA
      WRITE(*,*) '  PLTsta: ', (NAMES(PLTSTA(I)), ' ', I=1, NPLT )
      WRITE(*,'( A, 2F10.3 )' ) 
     1           '  TMIN and TMAX: ', UXMIN, UXMAX
      WRITE(*,'( A, 2F10.3 )' ) 
     1           '  YMIN and YMAX: ', UYMIN, UYMAX
      WRITE(*,'( A, F10.5, F10.2 )' )
     1           '  MINAMP and MAXRATE: ', UMINAMP, UMAXRATE
C
C     Now get the new values.
C   
  100 CONTINUE
      MODE = 1
      CALL KEYIN( KD(NPARS+1), KD, KI(2), ENDMRK, MODE, 5, 6 )
      IF( MODE.EQ.1 .OR. KD( KEYPTR( 'QUIT', KC, KI ) ) .EQ. 0.D0 ) THEN
         QUIT = .TRUE.
         CALL PGEND
         GO TO 900
      END IF
C
C     Get the new values of the input variables.
C
      PLTFILE = KCHAR( 'PLOTfile', 80, .FALSE., KD, KC, KI )
      REFSTA = KCHAR( 'REFsta', 2, .TRUE., KD, KC, KI )
      I1 = KEYPTR( 'PLTSTA', KC, KI )
      DO I = 1, 10
         WRITE( CMDPLT(I), '( A2 )' ) KD(I1-1+I)
         CALL UPCASE( CMDPLT(I) )
      END DO
      UXMIN = KD( KEYPTR( 'TMIN', KC, KI ) )
      UXMAX = KD( KEYPTR( 'TMAX', KC, KI ) )
      UYMIN = KD( KEYPTR( 'YMIN', KC, KI ) )
      UYMAX = KD( KEYPTR( 'YMAX', KC, KI ) )
      UMINAMP = KD( KEYPTR( 'MINAMP', KC, KI ) )
      UMAXRATE = KD( KEYPTR( 'MAXRATE', KC, KI ) )
      NX = KD( KEYPTR( 'NX', KC, KI ) )
      NY = KD( KEYPTR( 'NY', KC, KI ) )
C
C     Open new plot file if needed.
C
      write(*,*) nx, lnx, ny, lny, pltfile, lpltfile
      IF( PLTFILE .NE. LPLTFILE .OR. NX .NE. LNX .OR.
     1     NY .NE. LNY ) THEN
         IF( LPLTFILE .NE. ' ' ) THEN
            CALL PGEND
         END IF
      write(*,*) 'got here', nx, ny
         IER = PGBEG( 0, PLTFILE, NX, NY )
         IF( IER .NE. 1 ) THEN
            WRITE(*,*) ' Problem with plot file: ', PLTFILE
            GO TO 100
         END IF
      END IF
      LPLTFILE = PLTFILE
      LNX = NX
      LNY = NY
C
C     Reference Antennas
C
      IREF = WHICH( REFSTA, NAMES, MAXSTA )
      IF( IREF .EQ. 0 ) THEN
         WRITE(*,*) ' Invalid reference antenna '
         GO TO 100
      END IF
C
C     Plot antennas
C
      INPLT = 0
      DO I = 1, 10
         IF( CMDPLT(I) .NE. ' ' ) THEN
            CALL UPCASE( CMDPLT(I) )
            NCMDPLT(I) = WHICH( CMDPLT(I), NAMES, MAXSTA )
            IF( NCMDPLT(I) .EQ. 0 ) THEN
               WRITE(*,*) ' Invalid plot station: ', CMDPLT(I)
            ELSE
               INPLT = INPLT + 1
            END IF
         END IF
      END DO         
      IF( INPLT .NE. 0 ) THEN
         NPLT = INPLT
         DO I = 1, MAXSTA
            PLTSTA(I) = NCMDPLT(I)
         END DO
      ELSE IF( NPLT .EQ. 0 ) THEN
         WRITE(*,*) ' Need antennas to plot '
         GO TO 100
      END IF
C
  900 RETURN
      END
