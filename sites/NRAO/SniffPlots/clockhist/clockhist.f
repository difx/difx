      PROGRAM CLOCKHIST
C
C     This routine looks through a list of files of type datafile.lis
C     from the sniffer.  In each one, it finds the data point with
C     this highest amplitude for each baseline and saves the delay.
C     It then references to a common antenna, throwing out points for
C     which the experiment does not have that antenna.  It then
C     plots the results.
C
      INCLUDE 'clock.inc'
C
      INTEGER    I
      LOGICAL    QUIT
      CHARACTER  ABBR(MAXSTA)*2
      DATA ABBR / 'SC','HN','NL','FD','LA','PT','KP','OV','BR','MK', 
     1            'Y' /
C ---------------------------------------------------------------------
C     Initializations
C
      DO I = 1, MAXSTA
         NAMES(I) = ABBR(I)
      END DO
C
C     Read through the data files.
C
      CALL GETDAT
C
C     Get user input.
C
  450 CONTINUE
C
C        Get the user requests.
C
         CALL GETCMD( QUIT )
         IF( QUIT ) GO TO 900
C
C        Rereference
C
         CALL REFSET
C
C        Plot the data.
C
         CALL PLTCLK
C
C        Go back for another plot
C
         GO TO 450
C
C     Done
C
  900 CONTINUE
C
      STOP
      END
