      PROGRAM PLOTWT
C
C     A program to plot weights from a file produced from a VLBA
C     export tape by John Benson's program.
C
C     An average time will be used.  The minimum, maximum, and 
C     average weight across all IF's for a station during each
C     average interval will be plotted.
C
      INCLUDE    'plotwt.inc'
C
      INTEGER    NP1, ST1, VLBOPE, IER, IP, NPAGE, NPERPAGE
      INTEGER    TIN(8), I, LEN1
      DOUBLE PRECISION  GETNUM
      CHARACTER  CBUFF*80
C -------------------------------------------------------------------
C     Get the file names.
C
      GOTSUM = .FALSE.
      IF( IARGC() .GE. 1 ) THEN
        CALL GETARG(1, WTFILE)
        IER = VLBOPE( INUNIT, WTFILE, 'TEXT', 'OLD', CBUFF )
        IF( IER .NE. 1 ) THEN
          WRITE(*,*) ' File not found'
          GO TO 999
        END IF
      ELSE
100     WRITE(*,*) 'Name of weights file (<80 char): '
        READ(*,'(A)') WTFILE
        IER = VLBOPE( INUNIT, WTFILE, 'TEXT', 'OLD', CBUFF )
        IF( IER .NE. 1 ) GO TO 100
      END IF
      NWORDS = MWORDS
      CALL RDLINE( WORD, WLEN, NWORDS, INUNIT, 6 )
      IF( NWORDS .EQ. -1 ) THEN
         WRITE(*,*) ' No data. '
         GO TO 999
      END IF
      EXPNAM = WORD(NWORDS)
C
C     Detect whether the input file is a summary or an original sniffer
C     file.
C
      GOTSUM = WORD(2)(1:WLEN(2)) .EQ. 'summary:'
C
C     Open the plot file.
C
      IF( IARGC() .GE. 2 ) THEN
        CALL GETARG(2, PLTFILE)
      ELSE
        WRITE(*,*) 'Name of plot file (eg wtsfile.ps/vps or /xs '//
     1                 '(interactive)):'
        READ(*,'(A)') PLTFILE
      END IF
C
C     Get the desired time range.
C     Note: no time selection possible via command line
      IF( IARGC() .EQ. 0 ) THEN
        WRITE(*,*) 'Time range to plot (dd hh mm ss dd mm hh ss, '//
     1       'first day is day 0.  Blank for all.):'
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
      ELSE
        TMINA = 0
        TMAXA = 1.E10
      END IF
C
C     Get the desired average time per point.
C     Note: default of 30 sec used if by command line
      IF( IARGC() .EQ. 0) THEN
        WRITE(*,*) 'Time average for each point (Sec. - Default 30)'
        NWORDS = MWORDS
        CALL RDLINE( WORD, WLEN, NWORDS, 5, 6 )
        IF( NWORDS .LE. 0 ) THEN
           TAVG = 30.0
        ELSE
           TAVG = GETNUM( WORD(1), 1, WLEN(1) )
        END IF
      ELSE
        TAVG = 30.0
      END IF
C
C     Open the summary output file.
C     Write the first line to it.
C     
      IF( .NOT. GOTSUM ) THEN
         WRTSUM = .TRUE.
         IER = VLBOPE( SUNIT, WTFILE(1:LEN1(WTFILE))//'.sum', 
     1                 'TEXT', 'NEW', CBUFF )
         IF( IER .EQ. 1 ) THEN
            WRITE( SUNIT, '( A, A )' )
     1          'PLOTWT summary: ', EXPNAM
         ELSE
            WRITE(*,*) ' ****** Warning: Unable to open summary file'
            WRITE(*,*) CBUFF
            WRTSUM = .FALSE.
         END IF
      END IF
C
C     Get the data.
C
      CALL GETWT
C
C     Plot the data 
C     Split reasonably evenly between pages with up to 12 per
C     page.
C
      NPAGE = INT( ( NSTA - 1 ) / 12 ) + 1
      NPERPAGE = INT( ( NSTA + ( NPAGE - 1 ) ) / NPAGE )
C
      DO IP = 1, NPAGE
         ST1 = ( IP - 1 ) * NPERPAGE + 1
         NP1 = MIN( ST1 + NPERPAGE, NSTA )
C
C        Now call PLTWT once for each page
C
         CALL PLTWT( ST1, NP1 )
      END DO
C
C     Finished
C
  999 CONTINUE
      STOP
      END
