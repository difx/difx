      PROGRAM PLOTBP
C
C     Program to plot bandpasses from John Benson's distribution tape
C     reading software.
C
C     Antenna selection added.  April 2007.  Put in standard version
C     July 5, 2007  RCW.
C
      INCLUDE 'plotbp.inc'
C
      INTEGER    IER, VLBOPE, PGOPEN, ICH, ISTA, IFR
      CHARACTER  BPFILE*256, CBUFF*250
C ---------------------------------------------------------------------
C     Open file
C
100   WRITE(*,*) ' Bandpass file: '
      READ(*,'(A)') BPFILE
      IER = VLBOPE( INUNIT, BPFILE, 'TEXT', 'OLD', CBUFF )
      IF( IER .NE. 1 ) GO TO 100
C
C     Get the plot file name and open the file.
C
 200  WRITE(*,*) 'Name of plot file (eg bm14_bp.ps/vps):'
      READ(*,'(A)') PLTFILE
      TF = PLTFILE
      CALL UPCASE( TF )
      IER = PGOPEN( PLTFILE )
      IF( IER .NE. 1 ) GO TO 200
C
C     Get station to plot.
C
      WRITE(*,*) 'Station to plot (blank for all):'
      READ(*,'(A)') DOSTA
      CALL UPCASE( DOSTA )
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
