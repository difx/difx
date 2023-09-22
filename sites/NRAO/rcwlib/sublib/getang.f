      REAL*8 FUNCTION GETANG( STRING, CHAR1, CHARN )
C
C     Function to decode an angle in STRING.  The format is assumed
C     to have d'", D'", hms, HMS, or :: delineating the hours (or 
C     degrees), minutes, and seconds.  The angle is returned in
C     radians.  Note that :: cannot distinguish hours and degrees
C     so degrees are assumed.
C
      CHARACTER*(*)  STRING
      INTEGER        CHAR1, CHARN,CH1, CH2
      INTEGER        CHART, ICH, CHARL, LEN1
      LOGICAL        NEG
      DOUBLE PRECISION  GETNUM, RADDEG, MULT
      PARAMETER    (RADDEG=3.1415926535897932D0/180.0D0)
C
      GETANG = 0.D0
      NEG = .FALSE.
C
C     Ignore any leading blanks.
C
      DO ICH = CHAR1, CHARN
         IF( STRING(ICH:ICH) .NE. ' ' ) GO TO 10
      END DO
   10 CH1 = ICH
C
C     Ignore any trailing blanks.
C
      CHARL = LEN1( STRING(CHAR1:CHARN) ) + CHAR1 - 1
C
C     Detect any signs.
C
      IF( STRING(CH1:CH1) .EQ. '-' ) THEN
         NEG = .TRUE.
         CH1 = CH1 + 1
      ELSE IF( STRING(CH1:CH1) .EQ. '+' ) THEN
         CH1 = CH1 + 1
      END IF
C
C     Get hours or degrees.
C
      CH2 = 0
      CHART = INDEX( STRING(CH1:CHARL), 'D' ) + CH1 - 2
      IF( CHART .GT. CH2 ) THEN            
         CH2 = CHART
         MULT = 1.D0
      END IF
      CHART = INDEX( STRING(CH1:CHARL), 'd' ) + CH1 - 2
      IF( CHART .GT. CH2 ) THEN            
         CH2 = CHART
         MULT = 1.D0
      END IF
      CHART = INDEX( STRING(CH1:CHARL), 'H' ) + CH1 - 2
      IF( CHART .GT. CH2 ) THEN            
         CH2 = CHART
         MULT = 15.D0
      END IF
      CHART = INDEX( STRING(CH1:CHARL), 'h' ) + CH1 - 2
      IF( CHART .GT. CH2 ) THEN            
         CH2 = CHART
         MULT = 15.D0
      END IF
      CHART = INDEX( STRING(CH1:CHARL), ':' ) + CH1 - 2
      IF( CHART .GT. CH2 ) THEN            
         CH2 = CHART
         MULT = 1.D0
      END IF
C
C     Parse the degrees or hours.  Get first character for min.
C
      IF( CH2 .GE. CH1 ) THEN
         GETANG = GETNUM( STRING, CH1, CH2 )
         CH1 = CH2 + 2
      ELSE
         GETANG = 0.D0
C        CH1 = CH1
      END IF
C
C     Now get the minutes.
C
      CHART = INDEX( STRING(CH1:CHARL), 'M' ) + CH1 - 2
      IF( CHART .GT. CH2 ) THEN            
         CH2 = CHART
      END IF
      CHART = INDEX( STRING(CH1:CHARL), 'm' ) + CH1 - 2
      IF( CHART .GT. CH2 ) THEN            
         CH2 = CHART
      END IF
      CHART = INDEX( STRING(CH1:CHARL), '''' ) + CH1 - 2
      IF( CHART .GT. CH2 ) THEN            
         CH2 = CHART
      END IF
      CHART = INDEX( STRING(CH1:CHARL), ':' ) + CH1 - 2
      IF( CHART .GT. CH2 ) THEN            
         CH2 = CHART
      END IF
C
C     Parse the minutes and add to degrees.  Get first character
C     for seconds.
C
      IF( CH2 .GE. CH1 ) THEN
         GETANG = GETANG + GETNUM( STRING, CH1, CH2 ) / 60.D0
         CH1 = CH2 + 2
      ELSE
C        GETANG = GETANG
C        CH1 = CH1
      END IF
C
C     Now get seconds.
C
      CH2 = CHARL
      IF( STRING(CHARL:CHARL) .EQ. 'S' .OR. 
     +    STRING(CHARL:CHARL) .EQ. 's' .OR.
     +    STRING(CHARL:CHARL) .EQ. '"' ) CH2 = CH2 - 1
      IF( CH2 .GE. CH1 ) THEN
         GETANG = GETANG + GETNUM( STRING, CH1, CH2 ) / 3600.D0
      ELSE
C        GETANG = GETANG
      END IF
C
C     Now set the sign and convert to radians.
C
      IF( NEG ) GETANG = -1.D0 * GETANG
      GETANG = MULT * GETANG * RADDEG
C
      RETURN
      END
