      DOUBLE PRECISION FUNCTION GETYR( IYR, IMONTH, IDAY, IHR )
C
      INTEGER  IYR, IMONTH, IDAY, IHR, JYR, JDAY, IER
C -----------------------------------------------------------------
      IF( IMONTH .EQ. 1 ) THEN
         IF( IYR .LT. 1000 .AND. IYR .GT. 50 ) THEN
            JYR = 1900 + IYR
         ELSE IF( IYR .LT. 1000 .AND. IYR .LT. 50 ) THEN
            JYR = 2000 + IYR
         ELSE
            JYR = IYR
         END IF
         JDAY = IDAY
      ELSE
         CALL SLA_CALYD( IYR, IMONTH, IDAY, JYR, JDAY, IER )
         IF( IER .GT. 1 ) THEN
            WRITE(*,*) ' GETYR: Bad month or day: ', IMONTH, IDAY
            STOP
         END IF
      END IF
      GETYR = JYR + ( JDAY + IHR/24.D0 ) / 365.25D0
      RETURN
      END
