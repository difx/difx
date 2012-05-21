      SUBROUTINE FIXLIM( LOW, HIGH, LOWFR, HIGHFR )
C
C     Routine to pad out plot limits to put an edge around the data.
C
      REAL   LOW, HIGH, RANGE, LOWFR, HIGHFR
C --------------------------------------------------------------------
      RANGE = HIGH - LOW
      IF( RANGE .EQ. 0.0 ) RANGE = 1.0
      LOW = LOW - RANGE * LOWFR
      HIGH = HIGH + RANGE * HIGHFR
C
      RETURN
      END

