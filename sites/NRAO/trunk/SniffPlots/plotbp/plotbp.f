      PROGRAM PLOTBP
C
C     Program to plot bandpasses from John Benson's distribution tape
C     reading software.
C
C     Antenna selection added.  April 2007.  Put in standard version
C     July 5, 2007  RCW.
C
C     Add baseline selection.  Now needed because data files have all
C     baselines.  Sept. 29, 2014  RCW.
C
C     Move frequency print to top of plot.  Sept. 29, 2014.  RCW.
C
      INCLUDE 'plotbp.inc'
C
      INTEGER    IER, VLBOPE, PGOPEN, ICH, ICH2, ISTA, IFR, LEN1
      CHARACTER  BPFILE*256, CBUFF*250
C ---------------------------------------------------------------------
C     Open file
C

      IF( IARGC() .GE. 1 ) THEN
        CALL GETARG(1, BPFILE) 
        IER = VLBOPE( INUNIT, BPFILE, 'TEXT', 'OLD', CBUFF )
        IF( IER .NE. 1 ) THEN
          WRITE(*,*) ' File not found'
          GO TO 999
        END IF
      ELSE
 100    WRITE(*,*) ' Bandpass file: '
        READ(*,'(A)') BPFILE
        IER = VLBOPE( INUNIT, BPFILE, 'TEXT', 'OLD', CBUFF )
        IF( IER .NE. 1 ) GO TO 100
      END IF
C
C     Get the plot file name and open the file.
C
      IF( IARGC() .GE. 2 ) THEN
        CALL GETARG(2, PLTFILE)
        TF = PLTFILE
        CALL UPCASE( TF )
        IER = PGOPEN( PLTFILE )
        IF( IER .NE. 1 ) THEN
          WRITE(*,*) ' Cannot open output file'
          GO TO 999
        END IF
      ELSE
 200    WRITE(*,*) 'Name of plot file (eg bm14_bp.ps/vps):'
        READ(*,'(A)') PLTFILE
        TF = PLTFILE
        CALL UPCASE( TF )
        IER = PGOPEN( PLTFILE )
        IF( IER .NE. 1 ) GO TO 200
      END IF
C
C     Get station to plot.
C
      IF( IARGC() .GE. 3 ) THEN
        CALL GETARG(3, DOSTA)
      ELSE
        IF( IARGC() .EQ. 0 ) THEN
          WRITE(*,*) 'Station to plot '//
     1       '(blank for all.  eg PT:LA for a baseline):'
          READ(*,'(A)') DOSTA
        ELSE
          DOSTA = ''
        END IF
      END IF


      CALL UPCASE( DOSTA )
      IF( INDEX( DOSTA, ':' ) .GT. 1 )  THEN
         ICH = INDEX( DOSTA, ':' ) + 1
         ICH2 = LEN1( DOSTA )
         DOSTA2 = DOSTA(ICH:ICH2)
         ICH2 = ICH - 2
         DOSTA = DOSTA(1:ICH2)
         WRITE(*,*) ' Will plot baseline ', DOSTA, ' ', DOSTA2
      END IF
C
C     Initialize accumulators for the summary plots.
C
      NSTA   = 0
      NFR    = 0
      DO ISTA = 1, MSTA
         DO IFR = 1, MFR
            LOWDP(IFR,ISTA) = 360.0
            SCTIME(1,IFR,ISTA) = ' '
            DO ICH = 1, MCHAN
               HIAMP(ICH,IFR,ISTA)  = 0.0
               HIPH(ICH,IFR,ISTA)   = 0.0
               SUMAC(ICH,IFR,ISTA)  = 0.0
               SUMAC1(ICH,IFR,ISTA) = 1.E10
               SUMAC2(ICH,IFR,ISTA) = 0.0
            END DO
         END DO
      END DO
C
C     Read and plot the data
C
      CALL GETBP
C
C     Plot the data.
C
      CALL PGIDEN
      CALL PGEND
C
C     Plot the summaries.
C
      CALL PLTSUM
C
C     Wrap up.
C
C
  999 STOP
      END
