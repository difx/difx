      SUBROUTINE PLTWT( ST1, ST2 )
C
C     Routine for PLOTWT that plots a page of weights.
C
      INTEGER   ST1, ST2
C
      INCLUDE 'plotwt.inc'
C
      INTEGER   NP, IS, IER, PGBEG, LEN1, IT
      REAL      LEFT, RIGHT, BOTTOM, TOP
      REAL      PBOTTOM, PTOP, PHEIGHT
      CHARACTER HEAD*100, XOPT*10, YOPT*10, JDATE*9, JPDATE*9
C
      DATA  LEFT, RIGHT, BOTTOM, TOP / 0.07, 0.98, 0.07, 0.92 /
C ---------------------------------------------------------------
C     Number of panels requested and height of a panel.
C
      NP = ST2 - ST1 + 1
      PHEIGHT = ( TOP - BOTTOM ) / NP
C
C     Open plot file if necessary.  Otherwise page.
C
      IF( ST1 .EQ. 1 ) THEN
         IER = PGBEG( 0, PLTFILE, 1, 1 )
         IF( IER .NE. 1 ) CALL ERROR( ' Problem with plot file' )
      END IF
      CALL PGPAGE
      CALL PGSCH( 1.0 )
      CALL PGVSTD
C
C     Put header on page:
C
      JPDATE = JDATE( JDAY1 )
C
      WRITE( HEAD, '( A,  5X, A, 5X, A )' )
     +          'WEIGHTS', EXPNAM, JPDATE
C
      CALL PGSCH( 1.2 )
      CALL PGMTXT( 'T', 2.0, 0.5, 0.5, HEAD )
      CALL PGSCH( 0.8 )
C
C
      DO IS = ST1, ST2
C
C        Window for a station plot.
C
         PBOTTOM = TOP - ( IS - ST1 + 1 ) * PHEIGHT
         PTOP = PBOTTOM + PHEIGHT
C
         CALL PGSWIN( TMIN, TMAX, 0.0, 1.25 )
         CALL PGSVP( LEFT, RIGHT, PBOTTOM, PTOP )
C
C        Specify borders and tick marks.
C
         XOPT = 'BCTSZ'
         YOPT = 'BCNTS'
         IF( IS .EQ. ST2 ) XOPT = XOPT(1:LEN1(XOPT))//'NH'
         CALL PGTBOX( XOPT, 0., 0, YOPT, 0.0, 0 )
C
C        Label axes and antennas.
C
         IF( IS .EQ. ST2) CALL PGLABEL( 
     1        'Time', ' ', ' ' )
         IF( IS .EQ. (ST1+ST2)/2 ) CALL PGLABEL(
     1        ' ', 'Weight', ' ' )
         CALL PGTEXT( TMIN + (TMAX-TMIN)/30, 1.08, STNAME(IS) )
C
C        Now plot the data points.
C
         CALL PGBBUF
         DO IT = 1, NTIME
            IF( MINWT(IT,IS) .LT. 90.0 ) THEN
               CALL PGERRY( 1, TIME(IT,IS), MAXWT(IT,IS),
     1             MINWT(IT,IS), 0.0 )
               CALL PGPT( 1, TIME(IT,IS), AVGWT(IT,IS), 20 )
            END IF
         END DO
         CALL PGEBUF
      END DO    !  Station loop
C
      CALL PGIDEN
C
C     Close plot if finished.
C
      IF( ST2 .EQ. NSTA ) THEN
         CALL PGEND
      END IF
C
      RETURN
      END




