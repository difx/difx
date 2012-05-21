      PROGRAM PLOTAPD
C
C     A program to plot amplitudes, phases, and delays file 
C     produced from a VLBA export tape by John Benson's program.
C
C     An average time will be used.  The average amplitude, phase, 
C     and delay for each IF for a baseline during each avarage
C     interval will be plotted.
C
      INCLUDE    'plotapd.inc'
C
      INTEGER    NX, NY, I
      INTEGER    VLBOPE, IER, PGBEG, IB
      INTEGER    TIN(8)
      DOUBLE PRECISION  GETNUM
      CHARACTER  CBUFF*256, PTF*80, YN*1
C -------------------------------------------------------------------
C     Some initializations.
C
      NBAS = 0
C
C     Get the file names and open the data file.
C
  100 WRITE(*,*) 'Name of input file: '
      READ(*,'(A)') DATFILE
      IER = VLBOPE( INUNIT, DATFILE, 'TEXT', 'OLD', CBUFF )
      IF( IER .NE. 1 ) GO TO 100
  150 NWORDS = MWORDS
      CALL RDLINE( WORD, WLEN, NWORDS, INUNIT, 6 )
      IF( NWORDS .EQ. -1 ) THEN
         WRITE(*,*) ' No data. '
         GO TO 999
      END IF
      EXPNAM = WORD(2)
C
C     Initialize PGPLOT
C
  200 WRITE(*,*) 'Name of plot file (eg bm14_apd.ps/vps):'
      READ(*,'(A)') PLTFILE
      PTF = PLTFILE
      CALL UPCASE( PTF )
      IF( INDEX( PTF, "/X" ) .NE. 0 ) THEN
         NX = 1
         NY = 1
         XWIN = .TRUE.
      ELSE
         NX = 1
         NY = 3
         XWIN = .FALSE.
      END IF
      IER = PGBEG( 0, PLTFILE, NX, NY )
      IF( IER .NE. 1 ) GO TO 200
C
C     Get channels to plot.
C
  250 WRITE(*,*) 'Baseband channels to plot (eg: 1 3 8  or ''ALL'' or',
     1      ' ''END'' (first and last)) '
      NWORDS = MWORDS
      CALL RDLINE( WORD, WLEN, NWORDS, 5, 6 )
      NDOCH = NWORDS
      IF( NWORDS .LE. 0 ) THEN
         WRITE(*,*) '  Give SOME value'
         GO TO 250
      ELSE IF( NWORDS .EQ. 1 ) THEN
         CALL UPCASE( WORD(1) )
         IF( WORD(1)(1:3) .EQ. 'ALL' ) THEN
C
C           Will reset this NDOCH later depending on data.
C
            NDOCH = MCHAN
            DO I = 1, MCHAN
               DOCH(I) = I
            END DO
         ELSE IF( WORD(1)(1:3) .EQ. 'END' ) THEN
            NDOCH = 2
            DOCH(1) = 1
            DOCH(2) = 100    ! a flag
         ELSE
            NDOCH = 1
            DOCH(1) = GETNUM( WORD(1), 1, WLEN(1) )
         END IF
      ELSE
         NDOCH = NWORDS
         DO I = 1, NDOCH
            DOCH(I) = GETNUM( WORD(I), 1, WLEN(I) )
         END DO
      END IF
C
C     Get antennas to plot.
C
      WRITE(*,*) 'Antennas to plot (''ALL'', or up to 10 separate).'
      WRITE(*,*) 'Blank means ALL.'
      WRITE(*,*) 'Will plot any baseline with listed antennas.'
      NWORDS = MWORDS
      CALL RDLINE( WORD, WLEN, NWORDS, 5, 6 )
      IF( NWORDS .EQ. 0 ) THEN
         NANT = 1
         DOANT(1) = 'ALL'      
      ELSE
         NANT = NWORDS
         DO I = 1, NANT
            DOANT(I) = WORD(I)
            CALL UPCASE( DOANT(I) )
         END DO
      END IF
C
C
C     Get time range to plot.
C
      WRITE(*,*) 'Time range to plot (dd hh mm ss dd mm hh ss, '//
     1     'first day is day 0.  Blank for all.):'
      NWORDS = MWORDS
      CALL RDLINE( WORD, WLEN, NWORDS, 5, 6 )
      DO I = 1, 8
         IF( I .LE. NWORDS ) THEN
            TIN(I) = GETNUM( WORD(I), 1, WLEN(I) )
         ELSE
            TIN(I) = 0
         END IF
      END DO
      TMINA = TIN(1)*24*3600 + TIN(2)*3600 + TIN(3)*60 + TIN(4)
      TMAXA = TIN(5)*24*3600 + TIN(6)*3600 + TIN(7)*60 + TIN(8)
      IF( TMAXA .EQ. 0 ) TMAXA = 1.E10
C
C     Get the data.  This all has to be in a loop since the
C     arrays to hold all possible data could be too big.
C
 300  CONTINUE
         IF( NBAS .EQ. 0 ) THEN
            IP1 = 1
            IP2 = MBAS
         ELSE
            IP1 = IP2 + 1
            IP2 = MIN( IP1 + MBAS - 1, NBAS )
         END IF
C
         CALL GETAPD
C
         IF( IP1 .EQ. 1 ) IP2 = MIN( MBAS, NBAS )
C
C        Deal with setting the second channel when there was an
C        END request.
C
         IF( DOCH(2) .EQ. 100 ) DOCH(2) = NCHAN
C 
C        Now call PLTAPD once for each baseline (subpage).  If
C        interactive, allow restart.
C        Also call the routine that gets the highest weight points.
C
         DO IB = IP1, IP2
            CALL PLTAPD( IB )
            CALL HIWT( IB )
            IF( MOD( IB, NX*NY ) .EQ. 0 ) CALL PGIDEN
            IF( XWIN ) THEN
               WRITE(*,'(A,$)') 
     1            ' Do you want new parameters (Y/N - default N):'
               READ(*,*) YN
               IF( YN .EQ. 'Y' .OR. YN .EQ. 'y' ) THEN
                  CALL PGEND
                  REWIND( UNIT=INUNIT )
                  NBAS = 0
                  GO TO 150
               END IF
            END IF
         END DO
C
C        Now go back for next set of baselines.
C        First rewind the file and read past the header line.
C
         REWIND( UNIT=INUNIT )
         NWORDS = MWORDS
         CALL RDLINE( WORD, WLEN, NWORDS, INUNIT, 6 )
         IF( IP2 .LT. NBAS ) GO TO 300
C
C     Close plot if finished.
C
      CALL PGEND
C
C     Finished
C
  999 CONTINUE
      STOP
      END
