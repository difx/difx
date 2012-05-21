      SUBROUTINE PLTCLK
C
C     Subroutine to plot clocks
C
      INCLUDE 'clock.inc'
C
      INTEGER JB, IB, IEXP, NP(MAXBAS)
      REAL    X(MAXEXP,MAXBAS), Y(MAXEXP,MAXBAS)
      REAL    XMIN, XMAX, YMIN, YMAX, XPT, YPT
C -------------------------------------------------------------
C
C     First get the data to plot and the plot limits.
C
      XMIN =  1.E10
      XMAX = -1.E10
      YMIN =  1.E10
      YMAX = -1.E10
      DO JB = 1, NPLT
         NP(JB) = 0
      END DO
      DO IEXP = 1, NEXP
         IF( NREF(IEXP) .NE. 0 ) THEN
            DO JB = 1, NPLT
               DO IB = 1, NREF(IEXP)
                  IF( PLTSTA(JB) .EQ. ISTA2(IB,IEXP) .AND. 
     1                MAXAMP(IB,IEXP) .GT. UMINAMP .AND.
     2                ABS( RATE(IB,IEXP) ) .LT. UMAXRATE  ) THEN
                     NP(JB) = NP(JB) + 1
                     X(NP(JB),JB) = TIME(IB,IEXP)
                     Y(NP(JB),JB) = DELAY(IB,IEXP)
                     XMAX = MAX( XMAX, X(NP(JB),JB) )
                     XMIN = MIN( XMIN, X(NP(JB),JB) )
                     YMAX = MAX( YMAX, Y(NP(JB),JB) )
                     YMIN = MIN( YMIN, Y(NP(JB),JB) )
                  END IF
               END DO
            END DO
         END IF
      END DO
C
C     Add some padding to derived limits or substitute fixed limits.
      IF( UXMIN .NE. 0.0 .OR. UXMAX .NE. 0.0 ) THEN
         XMIN = UXMIN
         XMAX = UXMAX
      ELSE
         CALL FIXLIM( XMIN, XMAX, 0.05, 0.05 )
      END IF
      IF( UYMIN .NE. 0.0 .OR. UYMAX .NE. 0.0 ) THEN
         YMIN = UYMIN
         YMAX = UYMAX
      ELSE
         CALL FIXLIM( YMIN, YMAX, 0.05, 0.05 )
      END IF
C
C     Do the plot
C      
      CALL PGENV( XMIN, XMAX, YMIN, YMAX, 0, 1 )
      CALL PGLAB('Julian Day - 50000', 'Clock (ns)', 
     1     'Sniffer Clocks - Referenced to '//REFSTA )
      DO JB = 1, NPLT
         IF( NP(JB) .GE. 1 ) THEN
            CALL PGPT( NP(JB), X(1,JB), Y(1,JB), JB+2 )
C
C           Labels
C
            XPT = XMIN + 0.05 * ( XMAX - XMIN )
            YPT = YMAX - 0.04 * JB * ( YMAX - YMIN )
            CALL PGPT( 1, XPT, YPT, JB+2 )
            YPT = YPT - 0.01 * ( YMAX - YMIN )
            CALL PGTEXT( XPT, YPT, ' '//NAMES(PLTSTA(JB)) )
         END IF
      END DO
C
      RETURN
      END







