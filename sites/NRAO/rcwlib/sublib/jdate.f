      CHARACTER*9 FUNCTION JDATE( JULDAY )
C
C     Converts Julian day to, eg, 1994oct25
C
      INTEGER  JULDAY
      DOUBLE PRECISION  JTIME, FD
      INTEGER YEAR, JM, JD, J
      CHARACTER   CMONTH(12)*3
      DATA  CMONTH / 'jan', 'feb', 'mar', 'apr', 'may', 'jun',
     1               'jul', 'aug', 'sep', 'oct', 'nov', 'dec' /
C --------------------------------------------------------------------
      JTIME = JULDAY
      CALL SLA_DJCL( JTIME, YEAR, JM, JD, FD, J )
      IF( J.NE.0 )  CALL ERROR( ' Time out of range for SLA_DJCL' )
      WRITE( JDATE, '( I4, A3, I2.2 )' ) YEAR, CMONTH(JM), JD
C
      RETURN
      END
